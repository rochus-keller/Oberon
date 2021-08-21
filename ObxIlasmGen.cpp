/*
* Copyright 2021 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon+ parser/compiler library.
*
* The following is the license that applies to this copy of the
* library. For a license to use the library under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* GNU General Public License Usage
* This file may be used under the terms of the GNU General Public
* License (GPL) versions 2.0 or 3.0 as published by the Free Software
* Foundation and appearing in the file LICENSE.GPL included in
* the packaging of this file. Please review the following information
* to ensure GNU General Public Licensing requirements will be met:
* http://www.fsf.org/licensing/licenses/info/GPLv2.html and
* http://www.gnu.org/copyleft/gpl.html.
*/

#include "ObxIlasmGen.h"
#include "ObxAst.h"
#include "ObErrors.h"
#include "ObxProject.h"
#include <QtDebug>
#include <QCoreApplication>
#include <QDateTime>
#include <QFile>
#include <QDir>
using namespace Obx;
using namespace Ob;

#ifndef OBX_AST_DECLARE_SET_METATYPE_IN_HEADER
Q_DECLARE_METATYPE( Obx::Literal::SET )
#endif

struct ObxIlasmGenCollector : public AstVisitor
{
    QList<Procedure*> allProcs;
    QList<Record*> allRecords;
    QSet<Module*> allImports;
    QList<ProcType*> allProcTypes;
    Module* thisMod;

    void collect(Type* t)
    {
        switch( t->getTag() )
        {
        case Thing::T_Array:
            collect(cast<Array*>(t)->d_type.data());
            break;
        case Thing::T_Record:
            {
                Record* r = cast<Record*>(t);
                allRecords.append( r );
                foreach( const Ref<Field>& f, r->d_fields )
                    collect(f->d_type.data());
                if( r->d_base )
                    collect(r->d_base.data());
            }
            break;
        case Thing::T_Pointer:
            collect(cast<Pointer*>(t)->d_to.data());
            break;
        case Thing::T_ProcType:
            {
                ProcType* pt = cast<ProcType*>(t);
                allProcTypes.append(pt);
                foreach( const Ref<Parameter>& p, pt->d_formals )
                    collect(p->d_type.data());
                if( pt->d_return )
                    collect(pt->d_return.data());
            }
            break;
        case Thing::T_QualiType:
            if( Record* r = t->toRecord() )
            {
                Named* n = r->findDecl();
                if( n ) // actually n cannot be 0
                    allImports.insert(n->getModule());
            }else
            {
                t = t->derefed();
                if( t && t->getTag() == Thing::T_ProcType )
                    collect(t); // even if the proc type was declared in another module, we create a local delegate here
            }
            break;
        }
    }

    void collect( Named* n )
    {
        switch( n->getTag() )
        {
        case Thing::T_Procedure:
            {
                Procedure* p = cast<Procedure*>(n);
                if( p->d_receiver.isNull() )
                    allProcs.append(cast<Procedure*>(n));
                p->accept(this);
            }
            break;
        case Thing::T_NamedType:
            collect(n->d_type.data());
            break;
        case Thing::T_Variable:
        case Thing::T_Parameter:
        case Thing::T_LocalVar:
            collect(n->d_type.data());
            break;
        }
    }

    void visit( Module* me )
    {
        thisMod = me;
        foreach( const Ref<Named>& n, me->d_order )
            collect(n.data());
    }

    void visit( Procedure* me)
    {
        foreach( const Ref<Named>& n, me->d_order )
            collect(n.data());
    }
};

struct TempPool
{
    enum { MAX_TEMP = 32 };
    std::bitset<MAX_TEMP> d_slots;
    quint16 d_start;
    qint16 d_max; // max used slot
    TempPool():d_start(0),d_max(-1){}
    void reset(quint16 start)
    {
        d_slots.reset();
        d_start = start;
        d_max = -1;
    }
    int buy()
    {
        for( int i = 0; i < MAX_TEMP; i++ )
        {
            if( !d_slots.test(i) )
            {
                d_slots.set(i);
                if( i > d_max )
                    d_max = i;
                return i + d_start;
            }
        }
        Q_ASSERT( false );
        return -1;
    }
    void sell( int i )
    {
        Q_ASSERT( i >= d_start );
        d_slots.set(i-d_start,false);
    }
};

struct ObxIlasmGenImp : public AstVisitor
{
    Errors* err;
    Module* thisMod;
    QIODevice* dev;
    QTextStream out;
    QString buffer;
    quint32 anonymousDeclNr;
    qint16 level;
    qint16 stackDepth;
    quint16 maxStackDepth;
    quint16 labelCount;
    bool ownsErr;
    RowCol last;
    TempPool temps;
    QSet<QByteArray> keywords;
    QHash<QByteArray, QPair<Type*,int> > copiers; // element type string -> element type, max dim count
    QHash<QByteArray,ProcType*> delegates;
    int exitJump; // TODO: nested LOOPs
    Procedure* scope;

    ObxIlasmGenImp():ownsErr(false),err(0),dev(0),thisMod(0),anonymousDeclNr(0),level(0),
        stackDepth(0),maxStackDepth(0),labelCount(0),exitJump(-1),scope(0)
    {
        // Source: mono-5.20.1.34/mcs/ilasm/scanner/ILTables.cs, replaced = by \t and extracted string col with excel
        keywords << "at" << "as" << "implicitcom" << "implicitres" << "noappdomain" << "noprocess"
                 << "nomachine" << "extern" << "instance" << "explicit" << "default" << "vararg" << "unmanaged"
                 << "cdecl" << "stdcall" << "thiscall" << "fastcall" << "marshal" << "in" << "out" << "opt"
                 << "static" << "public" << "private" << "family" << "initonly" << "rtspecialname" << "specialname"
                 << "assembly" << "famandassem" << "famorassem" << "privatescope" << "literal" << "notserialized"
                 << "value" << "not_in_gc_heap" << "interface" << "sealed" << "abstract" << "auto" << "sequential"
                 << "ansi" << "unicode" << "autochar" << "bestfit" << "charmaperror" << "import" << "serializable"
                 << "nested" << "lateinit" << "extends" << "implements" << "final" << "virtual" << "hidebysig"
                 << "newslot" << "unmanagedexp" << "pinvokeimpl" << "nomangle" << "ole" << "lasterr" << "winapi"
                 << "native" << "il" << "cil" << "optil" << "managed" << "forwardref" << "runtime" << "internalcall"
                 << "synchronized" << "noinlining" << "nooptimization" << "custom" << "fixed" << "sysstring" << "array"
                 << "variant" << "currency" << "syschar" << "void" << "bool" << "int8" << "int16" << "int32" << "int64"
                 << "float32" << "float64" << "error" << "unsigned" << "uint" << "uint8" << "uint16" << "uint32"
                 << "uint64" << "decimal" << "date" << "bstr" << "lpstr" << "lpwstr" << "lptstr" << "vbbyrefstr"
                 << "objectref" << "iunknown" << "idispatch" << "struct" << "safearray" << "int" << "byvalstr" << "tbstr"
                 << "lpvoid" << "any" << "float" << "lpstruct" << "null" << "vector" << "hresult" << "carray"
                 << "userdefined" << "record" << "filetime" << "blob" << "stream" << "storage" << "streamed_object"
                 << "stored_object" << "blob_object" << "cf" << "clsid" << "method" << "class" << "pinned" << "modreq"
                 << "modopt" << "typedref" << "property" << "type" << "refany" << "wchar" << "char" << "fromunmanaged"
                 << "callmostderived" << "bytearray" << "with" << "init" << "to" << "catch" << "filter" << "finally"
                 << "fault" << "handler" << "tls" << "field" << "request" << "demand" << "assert" << "deny" << "permitonly"
                 << "linkcheck" << "inheritcheck" << "reqmin" << "reqopt" << "reqrefuse" << "prejitgrant" << "prejitdeny"
                 << "noncasdemand" << "noncaslinkdemand" << "noncasinheritance" << "readonly" << "nometadata" << "algorithm"
                 << "fullorigin" << "enablejittracking" << "disablejitoptimizer" << "retargetable" << "preservesig"
                 << "beforefieldinit" << "alignment" << "nullref" << "valuetype" << "compilercontrolled" << "reqsecobj"
                 << "enum" << "object" << "string" << "true" << "false" << "is" << "on" << "off" << "strict" << "forwarder"
                 << "legacy" << "library" << "auto" << "aggressiveinlining"
               // the following are extracted from https://en.wikipedia.org/wiki/List_of_CIL_instructions
                 << "add" << "and" <<"arglist" <<"beq" <<"bge" <<"bgt" <<"ble" <<"blt" <<"bne" <<"box" <<"br" <<"break" <<"brfalse"
                 <<"brinst" <<"brnull" <<"brtrue" <<"brzero" <<"call" <<"calli" <<"callvirt" <<"castclass" <<"ceq" <<"cgt"
                 <<"ckfinite" <<"clt" <<"constrained" <<"conv" <<"cpblk" <<"cpobj" <<"div" <<"dup" <<"endfault" <<"endfilter"
                 <<"endfinally" <<"initblk" <<"initobj" <<"isinst" <<"jmp" <<"ldarg" <<"ldarga" <<"ldc" <<"ldelem" <<"ldelema"
                 <<"ldfld" <<"ldflda" <<"ldftn" <<"ldind" <<"ldlen" <<"ldloc" <<"ldloca" <<"ldloca" <<"ldnull" <<"ldobj"
                 <<"ldsfld" <<"ldsflda" <<"ldstr" <<"ldtoken" <<"ldvirtftn" <<"leave" <<"localloc" <<"mkrefany" <<"mul" <<"neg"
                 <<"newarr" <<"newobj" <<"nop" <<"not" <<"or" <<"pop" <<"readonly" <<"refanytype" <<"refanyval" <<"rem" <<"ret"
                 <<"rethrow" <<"shl" <<"shr" <<"sizeof" <<"starg" <<"stelem" <<"stfld" <<"stind" <<"stloc" <<"stobj" <<"stsfld"
                 <<"sub" <<"switch" <<"tail" <<"throw" <<"unaligned" <<"unbox" <<"volatile" <<"xor";
    }

    inline QByteArray ws() { return QByteArray(level*4,' '); }

    QByteArray inline escape( const QByteArray& name )
    {
        if( keywords.contains(name) )
            return "'" + name + "'";
        else
            return name;
    }

    QByteArray dottedName( Named* n )
    {
        // concatenate names up to but not including module
        QByteArray name = escape(n->d_name);
        if( n->d_scope && n->d_scope->getTag() != Thing::T_Module )
            return dottedName(n->d_scope) + "." + name;
        return name;
    }

    QByteArray classRef( Named* className )
    {
        Q_ASSERT( className && className->getTag() == Thing::T_NamedType );
        Module* m = className->getModule();
        return moduleRef(m) + "/" + dottedName(className);
    }

    QByteArray classRef( Record* r )
    {
        Named* n = r->findDecl();
        if( n )
            return classRef(n);
        else
        {
            Q_ASSERT( r->d_slotValid );
            return moduleRef(thisMod) + "/'#" + QByteArray::number(r->d_slot) + "'";
        }
    }

    QByteArray moduleRef( Named* modName )
    {
        Q_ASSERT( modName && modName->getTag() == Thing::T_Module );
        const QByteArray mod = escape(modName->getName());
        if( modName == thisMod )
            return "'" + mod + "'";
        else
            return "['" + mod + "']'" + mod + "'";
    }

    QByteArray variableRef( Named* varName )
    {
        Q_ASSERT( varName && varName->getTag() == Thing::T_Variable );
        return moduleRef(varName->d_scope) + "::" + escape(varName->d_name);
    }

    QByteArray fieldRef( Named* fieldName )
    {
        Q_ASSERT( fieldName && fieldName->getTag() == Thing::T_Field );
        Field* f = cast<Field*>(fieldName);
        return classRef(f->d_owner) + "::" + escape(fieldName->d_name);
    }

    QByteArray methodRef( Named* procName )
    {
        Q_ASSERT( procName && procName->getTag() == Thing::T_Procedure );
        Procedure* p = cast<Procedure*>(procName);
        if( p->d_receiverRec )
            return classRef(p->d_receiverRec) + "::" + dottedName(procName);
        else
            return moduleRef(p->getModule()) + "::" + dottedName(procName);
    }

    QByteArray delegateRef( ProcType* pt )
    {
        if( pt == 0 )
            return "?";
        Named* n = pt->findDecl(true);

        if( n )
        {
#if 0
            if( pt->d_slotValid )
                return "valuetype " + moduleRef(n->getModule()) + "/'#" + QByteArray::number(pt->d_slot) + "'";
            else
                return "valuetype " + moduleRef(n->getModule()) + "/" + dottedName(n);
#else
            Q_ASSERT( pt->d_slotValid);
            return "valuetype " + moduleRef(n->getModule()) + "/'#" + QByteArray::number(pt->d_slot) + "'";
#endif
        }else
            return "valuetype " + moduleRef(thisMod) + "/'#" + QByteArray::number(pt->d_slot) + "'";
    }

    void emitArrayCopierRef(Type* et, int dims)
    {
        et = derefed(et);
        Q_ASSERT(et);
        const QByteArray sig = formatType(et);
        QPair<Type*,int>& d = copiers[sig];
        if( d.second < dims )
            d.second = dims;
        if( d.first == 0 )
            d.first = et;
        out << "void " << moduleRef(thisMod) << "::'#copy'(";
        out << sig;
        out << "[], ";
        out << sig;
        out << "[], int32)";
    }

    void emitArrayCopier( Type* et, int dims, const RowCol& loc )
    {
        et = derefed(et);
        Q_ASSERT(et);

        // the generated procedure is used for both multi- and onedimensional arrays.
        // the multi-dim code is only generated if there is a multi-dim used in code (i.e. dims > 1)
        // the generated procedure assumes array of array if dim > 0 and array of non-array if dim == 0

        out << ws() << ".method assembly static void '#copy'(";
            // NOTE: currently each assembly has it's own copy of each required copier
        et->accept(this);
        out << "[] lhs, ";
        et->accept(this);
        out << "[] rhs, int32 dim) cil managed {" << endl;
        level++;
        beginBody();

        const int len = temps.buy();
        Q_ASSERT( len >= 0 );
        emitOpcode("ldarg.0",1,loc);
        emitOpcode("ldlen",1, loc);
        emitOpcode("ldarg.1",1,loc);
        emitOpcode("ldlen",1, loc);
        // stack: len lhs, len rhs
        const int lhsIsLen = labelCount++;
        const int storeLen = labelCount++;
        emitOpcode("ble '#"+QByteArray::number(lhsIsLen)+"'",-2,loc);
        emitOpcode("ldarg.1",1,loc);
        emitOpcode("ldlen",1, loc);
        emitOpcode("br '#"+QByteArray::number(storeLen)+"'",0,loc);
        out << "'#" << lhsIsLen << "':" << endl;
        emitOpcode("ldarg.0",1,loc);
        emitOpcode("ldlen",1, loc);
        out << "'#" << storeLen << "':" << endl;
        emitOpcode("stloc "+QByteArray::number(len),-1,loc); // len = qMin(lenLhs,lenRhs)
        // TODO: only up to and including \0 for char arrays?

        const int idx = temps.buy();
        Q_ASSERT( idx >= 0 );
        emitOpcode("ldc.i4.0",1,loc);
        emitOpcode("stloc "+QByteArray::number(idx),-1,loc);

        const int checkLenLbl = labelCount++;
        const int addLbl = labelCount++;
        const int copyLbl = labelCount++;
        out << "'#" << checkLenLbl << "':" << endl;
        emitOpcode("ldloc "+QByteArray::number(idx),1,loc);
        emitOpcode("ldloc "+QByteArray::number(len),1,loc);
        const int afterLoopLbl = labelCount++;
        emitOpcode("bge '#"+QByteArray::number(afterLoopLbl)+"'",-2,loc);

        if( dims > 1 )
        {
            emitOpcode("ldarg.2",1,loc);
            emitOpcode("brfalse '#"+QByteArray::number(copyLbl)+"'",-1,loc); // if dim == 0 goto copyLbl

            emitOpcode("ldarg.0",1,loc);
            emitOpcode("ldloc "+QByteArray::number(idx),1,loc);
            // stack: array, int
            emitOpcode2("ldelem", -2 + 1, loc);
            out << " "; et->accept(this); out << endl;

            emitOpcode("ldarg.1",1,loc);
            emitOpcode("ldloc "+QByteArray::number(idx),1,loc);
            // stack: array, array, int
            emitOpcode2("ldelem", -2 + 1, loc);
            out << " "; et->accept(this); out << endl;

            // stack: lhs array, rhs array
            emitOpcode("ldarg.2",1,loc);
            emitOpcode("ldc.i4.1",1,loc);
            emitOpcode("sub",-2+1,loc); // dim-1
            emitOpcode2("call ", -3, loc);
            emitArrayCopierRef(et,dims);
            out << endl;

            emitOpcode("br '#"+QByteArray::number(addLbl)+"'",0,loc);
        }

        out << "'#" << copyLbl << "':" << endl;
        switch( et->getTag() )
        {
        case Thing::T_Record:
            {
                emitOpcode("ldarg.0",1,loc);
                emitOpcode("ldloc "+QByteArray::number(idx),1,loc);
                // stack: array, int
                emitOpcode2("ldelem", -2 + 1, loc);
                out << " "; et->accept(this); out << endl;

                emitOpcode("ldarg.1",1,loc);
                emitOpcode("ldloc "+QByteArray::number(idx),1,loc);
                // stack: record, array, int
                emitOpcode2("ldelem", -2 + 1, loc);
                out << " "; et->accept(this); out << endl;

                // stack: lhs record, rhs record
                emitOpcode2("callvirt ", -2, loc);
                Record* r2 = cast<Record*>(et);
                out << "instance void " << classRef(r2) + "::'#copy'(";
                r2->accept(this);
                if( r2->d_byValue )
                    out << "&";
                out <<")" << endl;
            }
            break;
        case Thing::T_Array:
            Q_ASSERT(false); // et always points to the base type of the (multidim) array, which cannot be an array
            break;
        case Thing::T_BaseType:
        case Thing::T_Enumeration:
        case Thing::T_Pointer:
        case Thing::T_ProcType:
            {
                emitOpcode("ldarg.0",1,loc);
                emitOpcode("ldloc "+QByteArray::number(idx),1,loc);
                // stack: lhs array, int

                emitOpcode("ldarg.1",1,loc);
                emitOpcode("ldloc "+QByteArray::number(idx),1,loc);
                // stack: lhs array, int, rhs array, int

                emitOpcode2("ldelem", -2 + 1, loc);
                out << " "; et->accept(this); out << endl;
                // stack: lhs array, int, value

                emitOpcode2("stelem", -3, loc);
                out << " "; et->accept(this); out << endl;
            }
            break;
        }

        out << "'#" << addLbl << "':" << endl;
        emitOpcode("ldloc "+QByteArray::number(idx),1,loc);
        emitOpcode("ldc.i4.1",1,loc);
        emitOpcode("add",-1,loc);
        emitOpcode("stloc "+QByteArray::number(idx),-1,loc);
        emitOpcode("br '#"+QByteArray::number(checkLenLbl)+"'",0,loc);
        out << "'#" << afterLoopLbl << "':" << endl;
        temps.sell(idx);
        temps.sell(len);

        out << ws() << "ret" << endl;
        switchBody();
        out << ws() << ".maxstack " << maxStackDepth << endl;
        emitLocalVars();
        endBody();
        level--;
        out << ws() << "}" << endl;
    }

//#define _USE_VALUE_RECORDS_
    // no value records currently because the initialization works completely different; needs extra work

    void emitRecordDecl(Record* r)
    {
        Named* n = r->findDecl();
        out << ws() << ".class ";
        if( n == 0 || n->getTag() != Thing::T_NamedType )
        {
            r->d_slot = anonymousDeclNr++;
            r->d_slotValid = true;
            out << "nested ";
            Named* n = r->findDecl(true);
            if( n && n->d_visibility != Named::Private )
                out << "public ";
            else
                out << "assembly ";
#ifdef _USE_VALUE_RECORDS_
            out << "sealed ";
#endif
            Q_ASSERT( r->d_slotValid );
            out << "'#" << r->d_slot << "' extends ";
#ifdef _USE_VALUE_RECORDS_
            r->d_byValue = true;
            out << "[mscorlib]System.ValueType {" << endl;
#else
            r->d_byValue = false;
            out << "[mscorlib]System.Object {" << endl;
#endif
        }else
        {
            const bool isPublic = n->d_scope == thisMod && n->d_visibility == Named::ReadWrite;
#ifdef _USE_VALUE_RECORDS_
            r->d_byValue = !isPublic && r->d_baseRec == 0 && r->d_subRecs.isEmpty();
#else
            r->d_byValue = false;
#endif
            if( r->d_byValue )
                out << "sealed ";
            out << "nested ";
            if( isPublic )
                out << "public ";
            else
                out << "assembly ";
            out << "'" << dottedName(n) << "' extends ";
            if( r->d_byValue )
                out << "[mscorlib]System.ValueType ";
            else if( r->d_base.isNull() )
                out << "[mscorlib]System.Object ";
            else
                r->d_base->accept(this);
            out << "{" << endl;
        }
        level++;
        foreach( const Ref<Field>& f, r->d_fields )
            f->accept(this);
        foreach( const Ref<Procedure>& p, r->d_methods )
            p->accept(this);

        // default constructor
        out << ws() << ".method public specialname rtspecialname void .ctor() cil managed {" << endl;
        level++;
        beginBody();
        emitOpcode("ldarg.0",1,r->d_loc);
        emitOpcode2("call ", -1, r->d_loc);
        if( r->d_baseRec )
            out << "instance void " << classRef(r->d_baseRec) << "::.ctor()" << endl;
        else if( r->d_byValue )
            out << "instance void [mscorlib]System.ValueType::.ctor()" << endl;
        else
            out << "instance void [mscorlib]System.Object::.ctor()" << endl;

        // initialize fields of current record
        QList<Field*> fields = r->getOrderedFields();
        for( int i = 0; i < fields.size(); i++ )
        {
            // oberon system expects all vars to be initialized
            emitOpcode("ldarg.0",1,fields[i]->d_loc);
            if( emitInitializer(fields[i]->d_type.data(), false, fields[i]->d_loc ) )
                emitStackToVar( fields[i], fields[i]->d_loc );
            else
                emitOpcode("pop", -1, fields[i]->d_loc );
        }
        out << ws() << "ret" << endl;
        switchBody();
        out << ws() << ".maxstack " << maxStackDepth << endl;
        emitLocalVars();
        endBody();
        level--;
        out << ws() << "}" << endl;
        // end default constructor

        // copy
        out << ws() << ".method public virtual void '#copy'(";
        r->accept(this);
        if( r->d_byValue )
            out << "&";
        out << " rhs) cil managed {" << endl;
        level++;
        beginBody();
        if( r->d_baseRec )
        {
            emitOpcode("ldarg.0",1,r->d_loc);
            emitOpcode("ldarg.1",1,r->d_loc);
            emitOpcode2("call ", -2, r->d_loc);
            out << "instance void " << classRef(r->d_baseRec) << "::'#copy'(";
            r->d_baseRec->accept(this);
            if( r->d_byValue )
                out << "&";
            out <<")" << endl;
        }
        for( int i = 0; i < fields.size(); i++ )
        {
            Type* ft = derefed(fields[i]->d_type.data());
            switch( ft->getTag() )
            {
            case Thing::T_Record:
                {
                    emitOpcode("ldarg.0",1,r->d_loc);
                    emitOpcode2("ldfld ", -1+1, r->d_loc); // lhs
                    fields[i]->d_type->accept(this);
                    out << " " << fieldRef(fields[i]) << endl;
                    emitOpcode("ldarg.1",1,r->d_loc);
                    emitOpcode2("ldfld ", -1+1, r->d_loc); // rhs
                    fields[i]->d_type->accept(this);
                    out << " " << fieldRef(fields[i]) << endl;
                    emitOpcode2("callvirt ", -2, r->d_loc);
                    Record* r2 = cast<Record*>(ft);
                    out << "instance void " << classRef(r2) + "::'#copy'(";
                    r2->accept(this);
                    if( r2->d_byValue )
                        out << "&";
                    out <<")" << endl;
                }
                break;
            case Thing::T_Array:
                {
                    emitOpcode("ldarg.0",1,r->d_loc);
                    emitOpcode2("ldfld ", -1+1, r->d_loc);
                    fields[i]->d_type->accept(this);
                    out << " " << fieldRef(fields[i]) << endl;

                    emitOpcode("ldarg.1",1,r->d_loc);
                    emitOpcode2("ldfld ", -1+1, r->d_loc);
                    fields[i]->d_type->accept(this);
                    out << " " << fieldRef(fields[i]) << endl;

                    // stack: lhs array, rhs array
                    QList<Array*> dims = cast<Array*>(ft)->getDims();
                    emitOpcode("ldc.i4 "+QByteArray::number(dims.size()-1),1,r->d_loc);
                    emitOpcode2("call ", -3, r->d_loc);
                    emitArrayCopierRef(dims.last()->d_type.data(),dims.size());
                    out << endl;
                }
                break;
            case Thing::T_BaseType:
            case Thing::T_Enumeration:
            case Thing::T_Pointer:
            case Thing::T_ProcType:
                emitOpcode("ldarg.0",1,r->d_loc);
                emitOpcode("ldarg.1",1,r->d_loc);
                emitOpcode2("ldfld ", -1+1, r->d_loc);
                fields[i]->d_type->accept(this);
                out << " " << fieldRef(fields[i]) << endl;
                emitOpcode2("stfld ", -2, r->d_loc);
                fields[i]->d_type->accept(this);
                out << " " << fieldRef(fields[i]) << endl;
                break;
            }
        }
        out << ws() << "ret" << endl;
        switchBody();
        out << ws() << ".maxstack " << maxStackDepth << endl;
        emitLocalVars();
        endBody();
        level--;
        out << ws() << "}" << endl;
        // end copy

        level--;
        out << ws() << "}" << endl;
    }

    void emitDelegDecl(ProcType* pt)
    {
        const QByteArray sig = procTypeSignature(pt);
        ProcType* orig = delegates.value(sig);
        if( orig == pt )
            return;
        if( orig )
        {
            Q_ASSERT( orig->d_slotValid );
            pt->d_slot = orig->d_slot;
            pt->d_slotValid = true;
            return;
        }

        out << ws() << ".class ";
#if 0
        Named* n = pt->findDecl();
        if( n == 0 || n->getTag() != Thing::T_NamedType )
        {
            pt->d_slot = anonymousDeclNr++;
            pt->d_slotValid = true;
            out << "nested assembly sealed '#" << pt->d_slot << "' ";
        }else
        {
            const bool isPublic = n->d_scope == thisMod && n->d_visibility == Named::ReadWrite;
            out << "nested ";
            if( isPublic )
                out << "public ";
            else
                out << "assembly ";
            out << "sealed '" << dottedName(n) << "' ";
        }
#else
        // since there can be several proc types with the same signature but different names, we just use numbers
        pt->d_slot = anonymousDeclNr++;
        pt->d_slotValid = true;
        out << "nested assembly sealed '#" << pt->d_slot << "' "; // we created local deleg def even if defined in another mod
#endif
        out << "extends [mscorlib]System.MulticastDelegate {" << endl;
        level++;
        out << ws() << ".method public hidebysig instance void .ctor(object MethodsClass, "
               "native unsigned int MethodPtr) runtime managed { }" << endl;
        out << ws() << ".method public hidebysig virtual instance int32 Invoke";
        emitFormals(pt->d_formals);
        out << "runtime managed { }" << endl;
        level--;
        out << ws() << "}" << endl;
        delegates[sig] = pt;
    }

    void visit( Module* me )
    {
        ObxIlasmGenCollector co;
        me->accept(&co);

        // NOTE: module name is always set in '' and thus doesn't have to be escaped
        out << ".assembly '" << me->getName() << "' {}" << endl;
        out << ".module '" << me->getName() << ".dll'" << endl << endl;

        out << ".assembly extern mscorlib {}" << endl;
        out << ".assembly extern OBX.Runtime {}" << endl;

        foreach( Import* imp, me->d_imports )
        {
            if(imp->d_mod->d_synthetic || imp->d_mod->d_isDef ) // TODO: def
                continue; // ignore SYSTEM
            co.allImports.insert(imp->d_mod.data());
        }
        foreach( Module* m, co.allImports )
        {
            if( m && m != me )
                out << ".assembly extern '" << m->getName() << "' {}" << endl;
        }
        if( !co.allImports.isEmpty() )
            out << endl;

        out << ".class public sealed '" << me->getName() << "' extends [mscorlib]System.ValueType {" << endl; // MODULE
        level++;

        for( int i = 0; i < co.allProcTypes.size(); i++ )
            emitDelegDecl(co.allProcTypes[i]); // we use delegates for both proc types and type-bound proc types (plain function pointers are not verifiable)

        foreach( Record* r, co.allRecords )
            emitRecordDecl(r);

        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Variable )
                n->accept(this);
        }

        foreach( Procedure* p, co.allProcs )
            p->accept(this);

        out << ws() << ".method private specialname rtspecialname static void .cctor() cil managed {" << endl; // MODULE BEGIN
        level++;
        beginBody();
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Variable )
                emitInitializer(n.data());
        }
        foreach( const Ref<Statement>& s, me->d_body )
            s->accept(this);

#if 1  // TEST
        foreach( Import* imp, me->d_imports )
        {
            if( imp->d_mod->d_synthetic || imp->d_mod->d_isDef )
                continue;
            emitOpcode("call void ['" + imp->d_mod->getName() + "']'" +
                       imp->d_mod->getName() + "'::'ping#'()",0, me->d_begin );
        }
        emitOpcode( "ldstr \"this is " + me->getName() + "\"", 1, me->d_begin );
        emitOpcode( "call void [mscorlib]System.Console::WriteLine (string)", -1, me->d_begin );
#endif
        emitOpcode("ret",0,me->d_begin);

        switchBody();
        out << ws() << ".maxstack " << maxStackDepth << endl;
        emitLocalVars();
        endBody();

        level--;
        out << ws() << "}" << endl;

#if 1 // TEST
        out << ws() << ".method public static void 'ping#'() cil managed {" << endl; // NOP, just for testing
        level++;
        emitOpcode("ret", 0, me->d_end );
        level--;
        out << ws() << "}" << endl;
#endif

        QSet<QByteArray> done;
        while( !copiers.isEmpty() )
        {
            QByteArray t = copiers.begin().key();
            const int dims = copiers.begin().value().second;
            Type* tt = copiers.begin().value().first;
            copiers.remove(t);
            if( done.contains(t) )
                continue;
            emitArrayCopier(tt, dims, me->d_end );
            done.insert(t);
        }

        level--;
        out << "}" << endl;  // END MODULE
    }

    void visit( Import* me)
    {
        // NOP
    }

    QByteArray procTypeSignature(ProcType* pt)
    {
        QByteArray str;
        if( pt->d_return.isNull() )
            str = "void";
        else
            str = formatType(pt->d_return.data());
        str += "*";
        str += formatFormals(pt->d_formals);
        return str;
    }

    QByteArray formatType( Type* t )
    {
        if( t == 0 )
            return QByteArray();
        switch(t->getTag())
        {
        case Thing::T_Array:
            {
                Array* me = cast<Array*>(t);
                // we only support CLI vectors; multi dim are vectors of vectors
                // arrays are constructed types, i.e. all qualis are resolved up to their original module
                // arrays are always dynamic in CLI; the size of an array is an attribute of the instance
                if( me->d_type )
                    return formatType(me->d_type.data()) + "[]";
            }
            break;
        case Thing::T_BaseType:
            return formatBaseType(t->getBaseType());
        case Thing::T_Enumeration:
            return "uint16";
        case Thing::T_Pointer:
            {
                Pointer* me = cast<Pointer*>(t);
                // this is a CLI object reference; since all objects and arrays in CLI are dynamic,
                // a field of type object or array is always a pointer, whereas implicit;
                if( me->d_to )
                    return formatType(me->d_to.data());
            }
            break;
        case Thing::T_ProcType:
            {
                ProcType* pt = cast<ProcType*>(t);
                if( !pt->d_slotValid )
                {
                    const QByteArray sig = procTypeSignature(pt);
                    pt = delegates.value(sig);
                }
                return delegateRef(pt);
            }
            break;
        case Thing::T_QualiType:
            {
                QualiType* me = cast<QualiType*>(t);
                // all qualis are immediatedly resolved
                if( !me->d_quali->d_type.isNull() )
                {
                    if( me->d_quali->d_type->d_selfRef )
                    {
                        if( Record* r = me->d_quali->d_type->toRecord() )
                            return formatType(r);
                        else
                            return "[mscorlib]System.Object"; // avoid infinite loop
                    }else
                        return formatType(me->d_quali->d_type.data());
                }
            }
            break;
        case Thing::T_Record:
            return "class " + classRef(cast<Record*>(t));
        default:
            Q_ASSERT(false);
        }
        return "?";
    }

    void visit( ProcType* me )
    {
        out << formatType(me);
    }

    void visit( Record* me)
    {
        out << formatType(me);
    }

    void visit( Enumeration* me)
    {
        out << formatType(me);
    }

    void visit( QualiType* me)
    {
        out << formatType(me);
    }

    void visit( Array* me)
    {
        out << formatType(me);
    }

    void visit( Pointer* me)
    {
        out << formatType(me);
    }

    static inline QByteArray formatBaseType(int t)
    {
        switch( t )
        {
        case Type::BOOLEAN:
            return "bool";
        case Type::CHAR:
        case Type::WCHAR:
            return "char";
        case Type::BYTE:
            return "uint8";
        case Type::SHORTINT:
            return "int16";
        case Type::INTEGER:
            return "int32";
        case Type::LONGINT:
            return "int64";
        case Type::REAL:
            return "float32";
        case Type::LONGREAL:
            return "float64";
        case Type::SET:
            return "int32";
        default:
            return "?";
        }
    }

    void visit( BaseType* me)
    {
        out << formatBaseType(me->getBaseType());
    }

    void emitVar( Named* me, bool isStatic )
    {
        out << ws() << ".field ";
        if( me->d_visibility == Named::ReadWrite || me->d_visibility == Named::ReadOnly )
            out << "public ";
        else
            out << "assembly ";
        if( isStatic )
            out << "static ";
        if( me->d_type )
            me->d_type->accept(this);
        out << " " << escape(me->d_name) << endl;
    }

    void visit( Variable* me)
    {
        emitVar(me,true);
        // initializer is emitted in module .cctor
    }

    void visit( Field* me )
    {
        emitVar(me,false);
    }

    QByteArray formatFormals( const ProcType::Formals& formals, bool withName = true )
    {
        QByteArray res = "(";
        for( int i = 0; i < formals.size(); i++ )
        {
            if( i != 0 )
                res += ", ";
            res += formatType( formals[i]->d_type.data() );
            if( passByRef(formals[i].data()) )
                res += "&";
            if( withName );
            res += " " + escape(formals[i]->d_name);
        }
        res += ")";
        return res;
    }

    void emitFormals( const ProcType::Formals& formals )
    {
        out << formatFormals(formals);
    }

    void emitLocalVars( Procedure* me = 0 )
    {
        if( (me && me->d_varCount) || temps.d_max >= 0 )
        {
            out << ws() << ".locals init (";
            level++;
            bool first = true;
            if( me )
            {
                foreach( const Ref<Named>& n, me->d_order )
                {
                    if( n->getTag() == Thing::T_LocalVar )
                    {
                        if( !first )
                            out << ", " << endl << ws();
                        first = false;
                        n->accept(this);
                    }
                }
            }
            for( int i = 0; i <= temps.d_max; i++ )
            {
                if( !first )
                    out << ", " << endl << ws();
                first = false;
                out << "[" << i + temps.d_start << "] native int '#temp" << i << "'";
            }
            out << ")" << endl;
            level--;
        }
    }

    void visit( Procedure* me )
    {
        Q_ASSERT( scope == 0 );
        scope = me;
        out << ws() << ".method ";

        if( me->d_visibility != Named::Private )
            out << "public ";
        else
            out << "assembly ";

        if( me->d_receiver.isNull() )
            out << "static final ";
        else if( me->d_receiverRec && !me->d_receiverRec->d_byValue )
            out << "virtual ";
        else
            out << "instance ";

        ProcType* pt = me->getProcType();
        if( pt->d_return.isNull() )
            out << "void";
        else
            pt->d_return->accept(this);

        out << " " << dottedName(me);
        emitFormals(pt->d_formals);

        // allocate params and local
        int off = me->d_receiver.isNull() ? 0 : 1;
        for( int i = 0; i < pt->d_formals.size(); i++ )
        {
            pt->d_formals[i]->d_slot = i+off; // for type-bounds arg0 is self
            pt->d_formals[i]->d_slotValid = true;
        }
        off = 0;
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_LocalVar )
            {
                n->d_slot = off++;
                n->d_slotValid = true;
            }
        }

        out << " cil managed {" << endl;
        level++;
        beginBody(me->d_varCount);

        foreach( const Ref<Named>& n, me->d_order )
        {
            switch( n->getTag() )
            {
            case Thing::T_LocalVar:
            case Thing::T_Parameter:
                emitInitializer(n.data());
                break;
            }
        }
        foreach( const Ref<Statement>& s, me->d_body )
            s->accept(this);
        emitOpcode("ret", 0, me->d_end); // TODO: only if missing in d_body

        switchBody();
        out << ws() << ".maxstack " << maxStackDepth << endl;

        emitLocalVars(me);
        endBody();
        level--;
        out << ws() << "}" << endl;
        scope = 0;
    }

    void beginBody(quint16 start = 0)
    {
        Q_ASSERT( out.device() );
        out.setDevice(0);
        out.setString(&buffer);
        stackDepth = 0;
        maxStackDepth = 0;
        last = RowCol();
        temps.reset(start);
        labelCount = 0;
    }

    void switchBody()
    {
        Q_ASSERT( out.device() == 0 );
        out.flush();
        out.setDevice(dev);
    }

    void endBody()
    {
        out << buffer;
        buffer.clear();
    }

    void visit( LocalVar* me )
    {
        Q_ASSERT( me->d_slotValid );
        out << "[" << me->d_slot << "] ";
        if( !me->d_type.isNull() )
            me->d_type->accept(this);
        out << " " << escape(me->d_name);
        // initializer is emitted at begin of proc body
    }

    void emitConst(quint8 vtype, const QVariant& val, const RowCol& loc )
    {
        const int before = stackDepth;
        switch( vtype )
        {
        case Literal::Boolean:
            if( val.toBool() )
                emitOpcode("ldc.i4.1", 1, loc);
            else
                emitOpcode("ldc.i4.0", 1, loc);
            break;
        case Literal::Integer:
            emitOpcode2("ldc.i4",1,loc);
            out << " " << val.toInt() << endl; // TODO i8 vs i4
            break;
        case Literal::Enum:
            emitOpcode2("ldc.i4",1,loc);
            out << " " << val.toInt() << endl;
            break;
        case Literal::Real:
            emitOpcode2("ldc.r4",1,loc);
            out << " " << val.toFloat() << endl; // TODO r8 vs r4
            break;
        case Literal::Nil:
            emitOpcode("ldnull",1,loc);
            break;
        case Literal::String:
            {
                emitOpcode2("ldstr",1,loc);
                out << " \"" << val.toByteArray() << "\\0" << "\"" << endl;
                // String::ToCharArray doesn't append a zero, thus add one to ldstr explicitly; TODO: check
                emitOpcode("callvirt char[] [mscorlib]System.String::ToCharArray()", -1+1, loc);
            }
            break;
        case Literal::Bytes:
            {
#if 0 // not necessary
                emitOpcode2("ldstr",1,loc);
                out << " \"";
                const QByteArray ba = val.toByteArray().toHex();
                for( int i = 0; i < ba.size(); i += 2 )
                    out << "\\x" << ba[i] << ba[i+1];
                out << "\"" << endl;
#else
                const QByteArray ba = val.toByteArray();
                emitOpcode("ldc.i4 "+ QByteArray::number(ba.size()),1,loc);
                emitOpcode("newarr uint8",0, loc);

                for( int i = 0; i < ba.size(); i++ ) // TODO: this is inefficient
                {
                    emitOpcode("dup",1,loc);
                    emitOpcode("ldc.i4 "+ QByteArray::number(i),1,loc);
                    emitOpcode("ldc.i4 "+ QByteArray::number((quint8)ba[i]),1,loc);
                    emitOpcode("stelem uint8", -3, loc );
                }
#endif
            }
            break;
        case Literal::Char:
            emitOpcode2("ldc.i4",1,loc);
            out << " " << val.toUInt() << endl;
            break;
        case Literal::Set:
            {
                Literal::SET s = val.value<Literal::SET>();
                emitOpcode2("ldc.i4",1,loc);
                out << " " << s.to_ulong() << endl;
            }
            break;
        }
        Q_ASSERT( stackDepth == before+1 );
    }

    void visit( Literal* me)
    {
        emitConst( me->d_vtype, me->d_val, me->d_loc );
    }

    void visit( UnExpr* me)
    {
        Q_ASSERT( !me->d_sub.isNull() );

        me->d_sub->accept(this);
        const int before = stackDepth;

        // prev must be a pointer or a record
        Type* prevT = derefed(me->d_sub->d_type.data());
        Q_ASSERT( prevT );

        switch( me->d_op )
        {
        case UnExpr::NEG:
            if( prevT->getBaseType() == BaseType::SET )
            {
                emitOpcode("not", 0, me->d_loc);
            }else
            {
                Q_ASSERT( prevT->isNumeric() );
                emitOpcode("neg", 0, me->d_loc);
            }
            Q_ASSERT( stackDepth == before );
            return;
        case UnExpr::NOT:
            emitOpcode("ldc.i4.0", 1, me->d_loc);
            emitOpcode("ceq", -2+1, me->d_loc);
            Q_ASSERT( stackDepth == before );
            return;
        case UnExpr::DEREF:
            // NOP: both pointer deref as well as super proc calls are handled by referencing UnExpr
            Q_ASSERT( stackDepth == before );
            return;
        case UnExpr::ADDROF:
            // NOP
            Q_ASSERT( stackDepth == before );
            return;
        default:
            qDebug() << "ERR" << me->d_op << thisMod->d_name << me->d_loc.d_row << me->d_loc.d_col;
            Q_ASSERT( false );
            break;
        }
        Q_ASSERT( false );
    }

    void visit( IdentLeaf* me)
    {
        const int before = stackDepth;
        Named* id = me->getIdent();
        if( id == 0 )
            return; // already reported

        switch( id->getTag() )
        {
        case Thing::T_Const:
            {
                Const* c = cast<Const*>(id);
                emitConst( c->d_vtype, c->d_val, me->d_loc );
            }
            Q_ASSERT( stackDepth == before+1 );
            return;
        case Thing::T_Import:
            // NOP
            Q_ASSERT( before == stackDepth );
            return;
        case Thing::T_Variable:
        case Thing::T_LocalVar:
            emitVarToStack(id,me->d_loc);
            Q_ASSERT( stackDepth == before+1 );
            return;
        case Thing::T_Parameter:
            {
                Parameter* p = cast<Parameter*>(id);
                emitVarToStack(id,me->d_loc);
                if( passByRef(p) ) // the value on the stack is a &, so we need to fetch the value first
                {
                    Type* td = derefed(p->d_type.data());
                    switch( td->getTag() )
                    {
                    case Thing::T_Array:
                    case Thing::T_Record:
                    default:
                        Q_ASSERT( false ); // never happens because no passByRef for structured types
                        break;
                    case Thing::T_Pointer:
                    case Thing::T_ProcType:
                        emitOpcode("ldind.ref", -1+1, me->d_loc);
                        break;
                    case Thing::T_Enumeration:
                        emitOpcode("ldind.u4", -1+1, me->d_loc);
                        break;
                    case Thing::T_BaseType:
                        switch(td->getBaseType())
                        {
                        case Type::LONGREAL:
                            emitOpcode("ldind.r8",-2, me->d_loc);
                            break;
                        case Type::REAL:
                            emitOpcode("ldind.r4",-2, me->d_loc);
                            break;
                        case Type::LONGINT:
                            emitOpcode("ldind.i8",-2, me->d_loc);
                            break;
                        case Type::INTEGER:
                            emitOpcode("ldind.i4",-2, me->d_loc);
                            break;
                        case Type::SET:
                        case Type::BOOLEAN:
                            emitOpcode("ldind.u4",-2, me->d_loc);
                            break;
                        case Type::SHORTINT:
                            emitOpcode("ldind.i2",-2, me->d_loc);
                            break;
                        case Type::CHAR:
                        case Type::WCHAR:
                            emitOpcode("ldind.u2",-2, me->d_loc);
                            break;
                        case Type::BYTE:
                            emitOpcode("ldind.u1",-2, me->d_loc);
                            break;
                        }
                        break;
                    }
                }
                Q_ASSERT( stackDepth == before+1 );
            }
            return;
        case Thing::T_NamedType:
            // NOP
            Q_ASSERT( before == stackDepth );
            return;
        case Thing::T_BuiltIn:
        case Thing::T_Procedure:
            // NOP
            Q_ASSERT( before == stackDepth );
            return;
        default:
            qDebug() << "ERR" << id->getTag() << thisMod->d_name << me->d_loc.d_row << me->d_loc.d_col;
            Q_ASSERT( false );
            break;
        }
        Q_ASSERT( false );
    }

    void visit( IdentSel* me)
    {
        const int before = stackDepth;

        Q_ASSERT( !me->d_sub.isNull() );

        Named* subId = me->d_sub->getIdent();
        const bool derefImport = subId && subId->getTag() == Thing::T_Import;

        me->d_sub->accept(this);

        Named* id = me->getIdent();
        Q_ASSERT( id );

        switch( id->getTag() )
        {
        case Thing::T_Procedure:
            // NOP
            Q_ASSERT( before == stackDepth );
            return;
        case Thing::T_Field:
            Q_ASSERT( me->d_sub && me->d_sub->d_type->toRecord() );
            emitVarToStack(id, me->d_loc);
            Q_ASSERT( stackDepth == before+1 );
            return;
        case Thing::T_Variable:
            Q_ASSERT( derefImport );
            emitVarToStack(id, me->d_loc);
            Q_ASSERT( stackDepth == before+1 );
            return;
        case Thing::T_NamedType:
            // NOP
            Q_ASSERT( before == stackDepth );
            return;
        case Thing::T_Const:
            {
                Q_ASSERT( derefImport );
                Const* c = cast<Const*>(id);
                emitConst( c->d_vtype, c->d_val, me->d_loc );
            }
            Q_ASSERT( stackDepth == before+1 );
            return;
        case Thing::T_BuiltIn:
            // NOP
            Q_ASSERT( before == stackDepth );
            return;
        default:
            qDebug() << "ERR" << thisMod->d_name << id->getTag() << me->d_loc.d_row << me->d_loc.d_col;
            Q_ASSERT( false );
            break;
        }
        Q_ASSERT( false );
    }

    void emitIndex( ArgExpr* me )
    {
        const int before = stackDepth;
        Q_ASSERT( me->d_sub );
        me->d_sub->accept(this);
        Type* subT = derefed(me->d_sub->d_type.data());
        Q_ASSERT( subT && subT->getTag() == Thing::T_Array);

        Q_ASSERT( me->d_args.size() == 1 );
        me->d_args.first()->accept(this);

        Q_ASSERT( stackDepth == before + 2 );

        Type* et = derefed(cast<Array*>(subT)->d_type.data());
        if( et == 0 )
            return; // already reported
        emitOpcode2("ldelem", -2 + 1, me->d_loc);
        out << " "; et->accept(this); out << endl;

        Q_ASSERT( stackDepth == before + 1 );
    }

    void emitFetchDesigAddr(Expression* desig, bool omitParams = true )
    {
        const int unop = desig->getUnOp();
        const int tag = desig->getTag();
        if( unop == UnExpr::SEL )
        {
            Q_ASSERT( desig->getTag() == Thing::T_IdentSel );
            IdentSel* sel = cast<IdentSel*>(desig);
            Named* id = sel->getIdent();
            Q_ASSERT( id );
            sel->d_sub->accept(this);
            switch( id->getTag() )
            {
            case Thing::T_Variable:
                emitOpcode2("ldsflda ", 1, desig->d_loc);
                id->d_type->accept(this);
                out << " " << variableRef(id) << endl;
                break;
            case Thing::T_Field:
                emitOpcode2("ldflda ", -1+1, desig->d_loc);
                id->d_type->accept(this);
                out << " " << fieldRef(id) << endl;
                break;
            default:
                Q_ASSERT( false );
           }
        }else if( unop == UnExpr::IDX )
        {
            Q_ASSERT( desig->getTag() == Thing::T_ArgExpr );
            ArgExpr* args = cast<ArgExpr*>( desig );
            Q_ASSERT( args->d_args.size() == 1 );
            args->d_sub->accept(this); // stack: array
            args->d_args.first()->accept(this); // stack: array, index
            emitOpcode2("ldelema ",-2+1, desig->d_loc);
            desig->d_type->accept(this); out << endl;
        }else if( unop == UnExpr::CAST )
        {
            Q_ASSERT( desig->getTag() == Thing::T_ArgExpr );
            ArgExpr* args = cast<ArgExpr*>( desig );
            emitFetchDesigAddr( args->d_sub.data(), omitParams );
        }else if( unop == UnExpr::DEREF )
        {
            Q_ASSERT( desig->getTag() == Thing::T_UnExpr );
            UnExpr* ue = cast<UnExpr*>( desig );
            emitFetchDesigAddr( ue->d_sub.data(), omitParams );
        }else if( tag == Thing::T_IdentLeaf )
        {
            Named* n = desig->getIdent();
            switch( n->getTag() )
            {
            case Thing::T_Variable:
                emitOpcode2("ldsflda ", 1, desig->d_loc);
                n->d_type->accept(this);
                out << " " << variableRef(n) << endl;
                break;
            case Thing::T_Parameter:
                Q_ASSERT( n->d_slotValid );
                if( omitParams && passByRef(cast<Parameter*>(n)) )
                    emitOpcode("ldarg "+QByteArray::number(n->d_slot), 1, desig->d_loc); // we already have the address of the value
                else
                    emitOpcode("ldarga "+QByteArray::number(n->d_slot), 1, desig->d_loc);
                break;
            case Thing::T_LocalVar:
                Q_ASSERT( n->d_slotValid );
                emitOpcode("ldloca "+QByteArray::number(n->d_slot), 1, desig->d_loc);
                // NOTE: works only for local access
                break;
            }
        }else if( tag == Thing::T_Literal )
        {
            Q_ASSERT( cast<Literal*>(desig)->d_vtype == Literal::Nil );
            // this happens in BB when calling the Win32 API
            emitOpcode("ldnull",1,desig->d_loc);
        }else if( tag == Thing::T_ArgExpr )
        {
            ArgExpr* ae = cast<ArgExpr*>(desig);
            Q_ASSERT( ae->d_sub && ae->d_sub->getIdent() && ae->d_sub->getIdent()->getTag() == Thing::T_BuiltIn &&
                     ( cast<BuiltIn*>(ae->d_sub->getIdent())->d_func == BuiltIn::SYS_VAL ||
                       cast<BuiltIn*>(ae->d_sub->getIdent())->d_func == BuiltIn::VAL ) );
            Q_ASSERT( ae->d_args.size() == 2 );
            emitFetchDesigAddr(ae->d_args.last().data(), omitParams);
        }else
        {
            qDebug() << "ERR" << desig->getUnOp() << desig->getTag() << thisMod->getName() << desig->d_loc.d_row << desig->d_loc.d_col;
            Q_ASSERT( false );
        }
    }

    void emitBuiltIn( BuiltIn* bi, ArgExpr* ae )
    {
        switch( bi->d_func )
        {
        case BuiltIn::PRINTLN:
            {
                Q_ASSERT( ae->d_args.size() == 1 );
                ae->d_args.first()->accept(this);
                Type* t = derefed(ae->d_args.first()->d_type.data());
                if( t->isText() )
                {
                    if( t->isChar() )
                        emitOpcode("call void [mscorlib]System.Console::WriteLine(char)",-1,ae->d_loc);
                    else
                        emitOpcode("call void [mscorlib]System.Console::WriteLine(char[])",-1,ae->d_loc);
                }else if( t->isInteger() )
                {
                    if( t->getBaseType() <= Type::INTEGER )
                        emitOpcode("call void [mscorlib]System.Console::WriteLine(int32)",-1,ae->d_loc);
                    else
                        emitOpcode("call void [mscorlib]System.Console::WriteLine(int64)",-1,ae->d_loc);
                }else if( t->isReal() )
                    emitOpcode("call void [mscorlib]System.Console::WriteLine(float64)",-1,ae->d_loc);
                else if( t->isSet() )
                    emitOpcode("call void [mscorlib]System.Console::WriteLine(uint32)",-1,ae->d_loc);
                else
                {
                    switch(t->getTag())
                    {
                    case Thing::T_Enumeration:
                        emitOpcode("call void [mscorlib]System.Console::WriteLine(uint32)",-1,ae->d_loc);
                        break;
                    default:
                        emitOpcode("call void [mscorlib]System.Console::WriteLine(object)",-1,ae->d_loc);
                    }
                }
            }
            break;
        case BuiltIn::INC:
        case BuiltIn::DEC:
            {
                Ref<BinExpr> add = new BinExpr();
                add->d_lhs = ae->d_args.first();
                add->d_type = ae->d_args.first()->d_type;
                add->d_loc = ae->d_args.first()->d_loc;
                if( bi->d_func == BuiltIn::INC )
                    add->d_op = BinExpr::ADD;
                else
                    add->d_op = BinExpr::SUB;
                if( ae->d_args.size() == 1 )
                {
                    add->d_rhs = new Literal( Literal::Integer,add->d_loc,qlonglong(1));
                    add->d_rhs->d_type = ae->d_args.first()->d_type;
                }else
                {
                    Q_ASSERT( ae->d_args.size() == 2 );
                    add->d_rhs = ae->d_args.last();
                }
                Ref<Assign> ass = new Assign();
                ass->d_lhs = ae->d_args.first();
                ass->d_loc = ae->d_loc;
                ass->d_rhs = add.data();
                ass->accept(this);
            }
            break;
        case BuiltIn::TRAP:
            emitOpcode("break",0,ae->d_loc);
            break;
        case BuiltIn::TRAPIF:
            {
                Q_ASSERT( ae->d_args.size() == 1 );
                ae->d_args.first()->accept(this);
                const int atEnd = labelCount++;
                emitOpcode("brfalse '#"+QByteArray::number(atEnd)+"'", -1, ae->d_loc );
                emitOpcode("break",0,ae->d_loc);
                out << "'#" << atEnd << "':" << endl;
            }
            break;
        case BuiltIn::MAX:
        case BuiltIn::MIN:
            if( ae->d_args.size() == 1 )
            {
                Type* t = derefed(ae->d_args.first()->d_type.data());
                switch( t->getTag() )
                {
                case Thing::T_BaseType:
                    {
                        BaseType* bt = cast<BaseType*>(t);
                        switch( bt->getBaseType() )
                        {
                        case Type::LONGINT:
                            if( bi->d_func == BuiltIn::MAX )
                                emitOpcode("ldc.i8 " + bt->maxVal().toByteArray(),1,ae->d_args.first()->d_loc);
                            else
                                emitOpcode("ldc.i8 " + bt->minVal().toByteArray(),1,ae->d_args.first()->d_loc);
                            break;
                        case Type::LONGREAL:
                            if( bi->d_func == BuiltIn::MAX )
                                emitOpcode("ldc.r8 " + bt->maxVal().toByteArray(),1,ae->d_args.first()->d_loc);
                            else
                                emitOpcode("ldc.r8 " + bt->minVal().toByteArray(),1,ae->d_args.first()->d_loc);
                            break;
                        case Type::REAL:
                            if( bi->d_func == BuiltIn::MAX )
                                emitOpcode("ldc.r4 " + bt->maxVal().toByteArray(),1,ae->d_args.first()->d_loc);
                            else
                                emitOpcode("ldc.r4 " + bt->minVal().toByteArray(),1,ae->d_args.first()->d_loc);
                            break;
                        case Type::BOOLEAN:
                        case Type::CHAR:
                        case Type::WCHAR:
                        case Type::BYTE:
                        case Type::SHORTINT:
                        case Type::INTEGER:
                        case Type::SET:
                            if( bi->d_func == BuiltIn::MAX )
                                emitOpcode("ldc.i4 " + QByteArray::number(bt->maxVal().toInt()),1,ae->d_args.first()->d_loc);
                            else
                                emitOpcode("ldc.i4 " + QByteArray::number(bt->minVal().toInt()),1,ae->d_args.first()->d_loc);
                            break;
                        }
                    }
                    break;
                case Thing::T_Enumeration:
                    {
                        Enumeration* e = cast<Enumeration*>(t);
                        if( bi->d_func == BuiltIn::MAX )
                            emitOpcode("ldc.i4 " + e->d_items.last()->d_val.toByteArray(),1,ae->d_args.first()->d_loc);
                        else
                            emitOpcode("ldc.i4 " + e->d_items.first()->d_val.toByteArray(),1,ae->d_args.first()->d_loc);
                    }
                    break;
                default:
                    Q_ASSERT( false );
                }
            }else if( ae->d_args.size() == 2 )
            {
                ae->d_args.first()->accept(this);
                ae->d_args.last()->accept(this);
                const int posCase = labelCount++;
                if( bi->d_func == BuiltIn::MAX )
                    emitOpcode("bge '#"+QByteArray::number(posCase)+"'",-2, ae->d_loc);
                else
                    emitOpcode("ble '#"+QByteArray::number(posCase)+"'",-2, ae->d_loc);
                ae->d_args.last()->accept(this);
                const int toEnd = labelCount++;
                emitOpcode("br '#"+QByteArray::number(toEnd)+"'",0, ae->d_loc);
                out << "'#" << posCase << "':" << endl;
                ae->d_args.first()->accept(this);
                out << "'#" << toEnd << "':" << endl;
            }else
                Q_ASSERT( false );
            break;
        case BuiltIn::DEFAULT:
            {
                Q_ASSERT( !ae->d_args.isEmpty() && !ae->d_args.first()->d_type.isNull() );
                Expression* e = ae->d_args.first().data();
                emitInitializer(e->d_type.data(),false,e->d_loc);
            }
            break;
        case BuiltIn::LEN:
            {
                // TODO: len with two args
                Q_ASSERT( !ae->d_args.isEmpty() );
                Type* t = derefed(ae->d_args.first()->d_type.data() );
                if( t && t->getTag() == Thing::T_Pointer )
                    t = derefed( cast<Pointer*>(t)->d_to.data() );

                if( t->isString() )
                {
                    ae->d_args.first()->accept(this);
                    emitOpcode("call int [OBX.Runtime]OBX.Runtime::strlen(char[])", -1+1, ae->d_loc );
                }else
                {
                    Q_ASSERT( t->getTag() == Thing::T_Array );
                    Array* a = cast<Array*>(t);
                    Type* at = derefed( a->d_type.data() );
                    Q_ASSERT( at );
                    if( a->d_len > 0 )
                    {
                        emitOpcode("ldc.i4 " + QByteArray::number(a->d_len), 1, ae->d_loc );
                    }else
                    {
                        ae->d_args.first()->accept(this);
                        emitOpcode("ldlen", -1+1, ae->d_loc );
                    }
                }
            }
            break;
        case BuiltIn::NEW:
            {
                Q_ASSERT( !ae->d_args.isEmpty() );

                Type* t = ae->d_args.first()->d_type.data();
                Type* td = derefed(t);
                Q_ASSERT( td && td->getTag() == Thing::T_Pointer );
                //Pointer* ptr = cast<Pointer*>(td);

                QList<int> lengths;
                for( int i = 1; i < ae->d_args.size(); i++ )
                {
                    ae->d_args[i]->accept(this);
                    const int len = temps.buy();
                    lengths.append(len);
                    emitOpcode("stloc "+QByteArray::number(len),-1,ae->d_args[i]->d_loc);
                }

                emitFetchDesigAddr(ae->d_args.first().data(),true); // not false, because also here a var param has the address already
                // stack: address to store to

                // we must pass t here (not ptr->d_to) because the pointer could be a named type defined in another module;
                // if we deref the pointer we lose the module information
                emitInitializer(t, true, ae->d_loc, lengths );

                emitOpcode("stind.ref", -1, ae->d_loc );
            }
            break;
        case BuiltIn::INCL:
            {
                Q_ASSERT( ae->d_args.size() == 2 );

                emitFetchDesigAddr(ae->d_args.first().data(),true);
                // stack: addr of store
                emitOpcode("dup",1,ae->d_args.first()->d_loc);
                emitOpcode("ldind.u4", -1, ae->d_args.first()->d_loc);
                ae->d_args.last()->accept(this);
                emitOpcode("call int [OBX.Runtime]OBX.Runtime::addElemToSet(int,int)", -2+1, ae->d_loc );
                emitOpcode("stind.u4", -2, ae->d_args.last()->d_loc);
            }
            break;
        case BuiltIn::EXCL:
            {
                Q_ASSERT( ae->d_args.size() == 2 );

                emitFetchDesigAddr(ae->d_args.first().data(),true);
                // stack: addr of store
                emitOpcode("dup",1,ae->d_args.first()->d_loc);
                emitOpcode("ldind.u4", -1, ae->d_args.first()->d_loc);
                ae->d_args.last()->accept(this);
                emitOpcode("call int [OBX.Runtime]OBX.Runtime::removeElemFromSet(int,int)", -2+1, ae->d_loc );
                emitOpcode("stind.u4", -2, ae->d_loc);
            }
            break;
        case BuiltIn::PACK:
            {
                Q_ASSERT( ae->d_args.size() == 2 );

                emitFetchDesigAddr(ae->d_args.first().data(),true);
                // stack: addr of store
                emitOpcode("dup",1,ae->d_args.first()->d_loc);
                emitOpcode("ldind.r4", -1, ae->d_args.first()->d_loc);
                emitOpcode("conv.r8", 0, ae->d_args.first()->d_loc);
                ae->d_args.last()->accept(this);
                emitOpcode("conv.r8", 0, ae->d_args.last()->d_loc);
                emitOpcode("call float64 [mscorlib]Math::Pow(float64, float64)", -2+1, ae->d_loc );
                emitOpcode("conv.r4", 0, ae->d_loc);
                emitOpcode("stind.r4", -2, ae->d_loc);
           }
            break;
        case BuiltIn::UNPK:
            {
                Q_ASSERT( ae->d_args.size() == 2 );

                emitFetchDesigAddr(ae->d_args.first().data(),true);
                emitFetchDesigAddr(ae->d_args.last().data(),true);
                // stack: addr, addr
                emitOpcode("call void [OBX.Runtime]OBX.Runtime::UNPACK(float32&, int32&)", -2, ae->d_loc );
             }
            break;
        case BuiltIn::ORD:
            {
                Q_ASSERT( ae->d_args.size() == 1 );
                ae->d_args.first()->accept(this);
                Type* t = derefed(ae->d_args.first()->d_type.data() );
                if( t && t->isText() && t->getBaseType() == 0 )
                {
                    emitOpcode("ldc.i4.0",1,ae->d_loc);
                    emitOpcode("ldelem char", -2+1, ae->d_loc);
                }
            }
            break;
        case BuiltIn::CHR:
        case BuiltIn::FLT:
            Q_ASSERT( ae->d_args.size() == 1 );
            ae->d_args.first()->accept(this);
            break;
        case BuiltIn::ODD:
            Q_ASSERT( ae->d_args.size() == 1 );
            ae->d_args.first()->accept(this);
            emitOpcode("call bool [OBX.Runtime]OBX.Runtime::ODD(int32)", -1+1, ae->d_loc );
            break;
        case BuiltIn::ABS:
            Q_ASSERT( ae->d_args.size() == 1 );
            ae->d_args.first()->accept(this);
            {
                Type* t = derefed(ae->d_args.first()->d_type.data());
                Q_ASSERT( t );
                switch(t->getBaseType())
                {
                case Type::LONGREAL:
                    emitOpcode("call float64 [mscorlib]Math::Abs(float64)", -1+1, ae->d_loc );
                    break;
                case Type::REAL:
                    emitOpcode("call float32 [mscorlib]Math::Abs(float32)", -1+1, ae->d_loc );
                    break;
                case Type::LONGINT:
                    emitOpcode("call int64 [mscorlib]Math::Abs(int64)", -1+1, ae->d_loc );
                    break;
                case Type::INTEGER:
                    emitOpcode("call int32 [mscorlib]Math::Abs(int32)", -1+1, ae->d_loc );
                    break;
                case Type::SHORTINT:
                case Type::BYTE:
                    emitOpcode("call int16 [mscorlib]Math::Abs(int16)", -1+1, ae->d_loc );
                    break;
                default:
                    Q_ASSERT(false);
                }
            }
            break;
        case BuiltIn::FLOOR:
            Q_ASSERT( ae->d_args.size() == 1 );
            ae->d_args.first()->accept(this);
            emitOpcode("call float64 [mscorlib]Math::Floor(float64)", -1+1, ae->d_loc );
            emitOpcode("conv.i4",-1+1, ae->d_loc);
            break;
        case BuiltIn::LSL:
            Q_ASSERT( ae->d_args.size() == 2 );
            ae->d_args.first()->accept(this);
            ae->d_args.last()->accept(this);
            emitOpcode("shl", -2+1, ae->d_loc );
            break;
        case BuiltIn::ASR:
            Q_ASSERT( ae->d_args.size() == 2 );
            ae->d_args.first()->accept(this);
            ae->d_args.last()->accept(this);
            emitOpcode("shr", -2+1, ae->d_loc );
            break;
        case BuiltIn::ROR:
            Q_ASSERT( ae->d_args.size() == 2 );
            ae->d_args.first()->accept(this);
            ae->d_args.last()->accept(this);
            emitOpcode("shr.un", -2+1, ae->d_loc );
            break;
        case BuiltIn::BITAND:
            Q_ASSERT( ae->d_args.size() == 2 );
            ae->d_args.first()->accept(this);
            ae->d_args.last()->accept(this);
            emitOpcode("and", -2+1, ae->d_loc );
            break;
        case BuiltIn::BITOR:
            Q_ASSERT( ae->d_args.size() == 2 );
            ae->d_args.first()->accept(this);
            ae->d_args.last()->accept(this);
            emitOpcode("or", -2+1, ae->d_loc );
            break;
        case BuiltIn::BITXOR:
            Q_ASSERT( ae->d_args.size() == 2 );
            ae->d_args.first()->accept(this);
            ae->d_args.last()->accept(this);
            emitOpcode("xor", -2+1, ae->d_loc );
            break;
        case BuiltIn::BITNOT:
            Q_ASSERT( ae->d_args.size() == 2 );
            ae->d_args.first()->accept(this);
            ae->d_args.last()->accept(this);
            emitOpcode("not", -2+1, ae->d_loc );
            break;
        case BuiltIn::SHORT:
            Q_ASSERT( ae->d_args.size() == 1 );
            ae->d_args.first()->accept(this);
            switch(derefed(ae->d_args.first()->d_type.data())->getBaseType())
            {
            case Type::LONGINT:
                emitOpcode("conv.i4",-1+1, ae->d_loc);
                break;
            case Type::INTEGER:
                emitOpcode("conv.i2",-1+1, ae->d_loc);
                break;
            case Type::SHORTINT:
                emitOpcode("conv.u1",-1+1, ae->d_loc);
                break;
            case Type::LONGREAL:
                emitOpcode("conv.r4",-1+1, ae->d_loc);
                break;
            default:
                Q_ASSERT(false);
            }
            break;
        case BuiltIn::LONG:
            Q_ASSERT( ae->d_args.size() == 1 );
            ae->d_args.first()->accept(this);
            break;
        case BuiltIn::ADR:
            Q_ASSERT( ae->d_args.size() == 1 );
            ae->d_args.first()->accept(this);
            break;
        case BuiltIn::VAL:
            Q_ASSERT( ae->d_args.size() == 2 );
            ae->d_args.last()->accept(this);
            break;
        case BuiltIn::ASSERT:
            {
                Q_ASSERT( !ae->d_args.isEmpty() ); // TODO: by now second optional arg ignored!
                ae->d_args.first()->accept(this);

                const int after = labelCount++;
                emitOpcode("brtrue '#"+QByteArray::number(after)+"'",-1,ae->d_loc);
                emitOpcode("ldstr \"assertion failed\"",1, ae->d_loc);
                emitOpcode("newobj instance void [mscorlib]System.Exception::.ctor(string)",-1+1,ae->d_loc);
                emitOpcode("throw", -1, ae->d_loc);

                out << "'#" << after << "':" << endl;
            }
            break;
        default:
             qWarning() << "missing generator implementation of" << BuiltIn::s_typeName[bi->d_func];
             break;
        }
    }

    static inline bool passByRef( Parameter* p )
    {
        if( !p->d_var || p->d_const )
            return false;
        Type* td = derefed(p->d_type.data());
        if( td && !td->isStructured() )
            return true; // we only need to pass simple types including pointers and proc refs by &
        else
            return false; // all our structured values are already on the heap, the value is actually a object reference
    }

    void emitCall( ArgExpr* me )
    {
        const int before = stackDepth;
        Q_ASSERT( me->d_sub );
        me->d_sub->accept(this);

        Named* func = 0;
        if( me->d_sub->getUnOp() == UnExpr::DEREF )
        {
            // call to superclass method
            UnExpr* ue = cast<UnExpr*>(me->d_sub.data());
            func = ue->d_sub->getIdent();
            Q_ASSERT( func && func->getTag() == Thing::T_Procedure );
            Procedure* p = cast<Procedure*>(func);
            Q_ASSERT( p->d_super );
            func = p->d_super;
        }else
            func = me->d_sub->getIdent();

        const int funcTag = func ? func->getTag() : 0;
        if( func && funcTag == Thing::T_BuiltIn )
        {
            Q_ASSERT( before == stackDepth );
            emitBuiltIn( cast<BuiltIn*>(func), me );
            return;
        }else if( funcTag != Thing::T_Procedure )
            func = 0; // apparently a function pointer or delegate

        Type* subT = derefed( me->d_sub->d_type.data() );
        Q_ASSERT( subT && subT->getTag() == Thing::T_ProcType );
        ProcType* pt = cast<ProcType*>( subT );
        Q_ASSERT( pt->d_formals.size() <= me->d_args.size() );

        if( func == 0 || pt->d_typeBound )
            Q_ASSERT( stackDepth == before + 1 ); // self or delegate instance expected

        for( int i = 0; i < pt->d_formals.size(); i++ )
        {
            Parameter* p = pt->d_formals[i].data();
            Type* tf = derefed(p->d_type.data());
            Q_ASSERT( tf != 0 );

            if( passByRef(p) )
            {
                if( tf->getTag() == Thing::T_Array )
                {
                    Array* la = cast<Array*>(tf);
                    Type* ta = derefed(me->d_args[i]->d_type.data());
                    Q_ASSERT( ta != 0 );
                    Type* rat = ta->getTag() == Thing::T_Array ? derefed(cast<Array*>(ta)->d_type.data()) : 0;
                    if( derefed(la->d_type.data())->getBaseType() == Type::BYTE &&
                            ( rat == 0 || rat->getBaseType() != Type::BYTE ) )
                    {
                        err->error( Errors::Generator, Loc(me->d_args[i]->d_loc, thisMod->d_file),
                                      "cannot generate code for Oberon VAR ARRAY OF BYTE trick");
                        continue;
                    }
                }
                emitFetchDesigAddr(me->d_args[i].data());
            }else
            {
                // 1) a structured arg (record, array) passed by val
                // 2) or a structured arg passed to IN, i.e. just pass the reference
                // 3) or a non-structured arg passed by IN or by val, just pass the value in both cases
                // NOTE that in case of 1) the copy is done in the body of the called procedure
                me->d_args[i]->accept(this);
                prepareRhs( tf, me->d_args[i].data(), me->d_args[i]->d_loc );
            }
        }

        // TODO varargs
        const int stackDiff = -pt->d_formals.size() - (pt->d_typeBound?1:0) + (pt->d_return.isNull()?0:1);
        if( func )
        {
            if( pt->d_typeBound )
                emitOpcode2("callvirt instance", stackDiff, me->d_loc);
            else
                emitOpcode2("call ", stackDiff, me->d_loc);
            if( pt->d_return.isNull() )
                out << "void ";
            else
                pt->d_return->accept(this);
            out << methodRef(func);
            emitFormals(pt->d_formals);
            out << endl;
        }else
        {
            Q_ASSERT( pt->d_slotValid );
        }

        Q_ASSERT( before + (pt->d_return.isNull()?0:1) == stackDepth );
    }

    void inline prepareRhs(Type* tf, Expression* ea, const RowCol& loc)
    {
        Q_ASSERT(ea);
        tf = derefed(tf);
        Q_ASSERT( tf != 0 );
        Type* ta = derefed(ea->d_type.data());
        if( ta == 0 )
            return; // error already reported

        if( tf->isChar() && !ta->isChar() )
        {
            // convert len-1-string to char
            Q_ASSERT( ta->isString() || ta->isStructured() );
            emitOpcode("ldc.i4.1",1,loc);
            if( ta->isString() )
                emitOpcode("callvirt instance char string::get_Chars(int32)", -2+1, loc );
            else
                emitOpcode("ldelem char",-2+1,loc);
        }else if( ta->getTag() == Thing::T_ProcType )
        {
            Named* n = ea->getIdent();
            if( n && n->getTag() == Thing::T_Procedure )
            {
                ProcType* pt = cast<ProcType*>(ta);
                const QByteArray sig = procTypeSignature(pt);
                pt = delegates.value(sig);
                Q_ASSERT( pt && pt->d_slotValid );

                if( ta->d_typeBound )
                {
                    // we assign a type bound procedure to a type-bound proc type variable
                    // for this purpose we create a delegate instance on the stack
                    emitOpcode("dup", 1, loc); // stack: this, this
                    emitOpcode2("ldvirtftn ", -1+1, loc); // stack: this, fn
                    out << methodRef(n) << endl;
                    emitOpcode2("newobj ",-2+1, loc); // stack: -
                    out << " instance void " << delegateRef(pt) << "::.ctor()" << endl;
                }else
                {
                    // assign a normal procedure to a normal proc type variable
                    emitOpcode("ldnull",1,loc);
                    emitOpcode2("ldftn ", -1+1, loc); // stack: null, fn
                    out << methodRef(n) << endl;
                    emitOpcode2("newobj ",-2+1, loc); // stack: -
                    out << " instance void " << delegateRef(pt) << "::.ctor()" << endl;
                }
            }//else: we copy a proc type variable, i.e. delegate already exists
        }
    }

    void visit( ArgExpr* me )
    {
        switch( me->d_op )
        {
        case ArgExpr::IDX:
            emitIndex(me);
            break;
        case ArgExpr::CALL:
            emitCall(me);
            break;
        case ArgExpr::CAST:
            me->d_sub->accept(this);
            break;
        }
    }

    void visit( BinExpr* me)
    {
        const int before = stackDepth;
        Q_ASSERT( !me->d_lhs.isNull() && !me->d_rhs.isNull() &&
                  !me->d_lhs->d_type.isNull() && !me->d_rhs->d_type.isNull() );

        if( me->d_op != BinExpr::IN )
            me->d_lhs->accept(this);

        if( me->d_op != BinExpr::AND && me->d_op != BinExpr::OR )
            // AND and OR are special in that rhs might not be executed
            me->d_rhs->accept(this);

        Type* lhsT = derefed(me->d_lhs->d_type.data());
        Type* rhsT = derefed(me->d_rhs->d_type.data());
        Q_ASSERT( lhsT && rhsT );
        const int ltag = lhsT->getTag();
        const int rtag = rhsT->getTag();
        bool lwide, rwide;

        switch( me->d_op )
        {
        case BinExpr::IN:
            if( lhsT->isInteger() && rhsT->getBaseType() == Type::SET )
            {
                me->d_lhs->accept(this);
                emitOpcode("ldc.i4.1",1,me->d_lhs->d_loc);
                emitOpcode("shl",-2+1,me->d_lhs->d_loc);
                emitOpcode("and",-2+1,me->d_loc);
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::IS:
            emitOpcode2("isinst ", -2 + 1, me->d_loc);
            rhsT->accept(this); out << endl;
            break;
        case BinExpr::ADD:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) )
                emitOpcode("add", -2 + 1, me->d_loc);
            else if( lhsT->isSet() && rhsT->isSet() )
                emitOpcode("or", -2+1, me->d_loc);
            else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
                emitOpcode("call char[] [OBX.Runtime]OBX.Runtime::join(char[],char[])", -2+1, me->d_loc );
            else
                Q_ASSERT(false);
            break;
        case BinExpr::SUB:
            if( (lhsT->isNumeric() && rhsT->isNumeric()) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) )
                emitOpcode("sub", -2+1, me->d_loc );
            else if( lhsT->isSet() && rhsT->isSet() )
            {
                emitOpcode("not", -1+1, me->d_loc );
                emitOpcode("and", -2+1, me->d_loc );
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::FDIV:
            if( lhsT->isNumeric() && rhsT->isNumeric() )
                emitOpcode("div", -2+1, me->d_loc );
            else if( lhsT->isSet() && rhsT->isSet() )
            {
                const int rhs = temps.buy();
                emitOpcode("stloc "+QByteArray::number(rhs),-1, me->d_loc );
                const int lhs = temps.buy();
                emitOpcode("stloc "+QByteArray::number(lhs),-1, me->d_loc );
                emitOpcode("ldloc"+QByteArray::number(lhs),1, me->d_loc);
                emitOpcode("ldloc"+QByteArray::number(rhs),1, me->d_loc);
                emitOpcode("and", -2+1, me->d_loc );
                emitOpcode("not", -1+1, me->d_loc );
                emitOpcode("ldloc"+QByteArray::number(lhs),1, me->d_loc);
                emitOpcode("ldloc"+QByteArray::number(rhs),1, me->d_loc);
                emitOpcode("or", -2+1, me->d_loc );
                emitOpcode("and", -2+1, me->d_loc );
                temps.sell(rhs);
                temps.sell(lhs);
            }
            break;
        case BinExpr::MUL:
            if( lhsT->isNumeric() && rhsT->isNumeric() )
                emitOpcode("mul", -2+1, me->d_loc );
            else if( lhsT->isSet() && rhsT->isSet() )
                emitOpcode("and", -2+1, me->d_loc );
            else
                Q_ASSERT(false);
            break;
        case BinExpr::DIV:
            if( lhsT->isInteger() && rhsT->isInteger() )
            {
                if( lhsT->getBaseType() <= Type::INTEGER && rhsT->getBaseType() <= Type::INTEGER )
                    emitOpcode("call int32 [OBX.Runtime]OBX.Runtime::DIV(int32,int32)", -2+1, me->d_loc );
                else
                    emitOpcode("call int64 [OBX.Runtime]OBX.Runtime::DIV(int64,int64)", -2+1, me->d_loc );
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::MOD:
            if( lhsT->isInteger() && rhsT->isInteger() )
            {
                if( lhsT->getBaseType() <= Type::INTEGER && rhsT->getBaseType() <= Type::INTEGER )
                    emitOpcode("call int32 [OBX.Runtime]OBX.Runtime::MOD(int32,int32)", -2+1, me->d_loc );
                else
                    emitOpcode("call int64 [OBX.Runtime]OBX.Runtime::MOD(int64,int64)", -2+1, me->d_loc );
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::AND:
            if( lhsT->getBaseType() == Type::BOOLEAN && rhsT->getBaseType() == Type::BOOLEAN )
            {
                // lhs was run and stack has a bool result
                const int afterEnd = labelCount++;
                const int setFalse = labelCount++;
                emitOpcode("brfalse '#"+QByteArray::number(setFalse)+"'", -1, me->d_loc);
                me->d_rhs->accept(this);
                emitOpcode("br '#"+QByteArray::number(afterEnd)+"'", 0, me->d_loc);
                out << "'#" << setFalse << "':" << endl;
                emitOpcode("ldc.i4.0",1,me->d_loc);
                out << "'#" << afterEnd << "':" << endl;
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::OR:
            if( lhsT->getBaseType() == Type::BOOLEAN && rhsT->getBaseType() == Type::BOOLEAN )
            {
                // lhs was run and stack has a bool result
                const int afterEnd = labelCount++;
                const int setTrue = labelCount++;
                emitOpcode("brtrue '#"+QByteArray::number(setTrue)+"'", -1, me->d_loc);
                me->d_rhs->accept(this);
                emitOpcode("br '#"+QByteArray::number(afterEnd)+"'", 0, me->d_loc);
                out << "'#" << setTrue << "':" << endl;
                emitOpcode("ldc.i4.1",1,me->d_loc);
                out << "'#" << afterEnd << "':" << endl;
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::EQ:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( lhsT->getBaseType() == Type::BOOLEAN && rhsT->getBaseType() == Type::BOOLEAN ) ||
                    ( lhsT->getBaseType() == Type::SET && rhsT->getBaseType() == Type::SET) ||
                    ( lhsT->isChar() && rhsT->isChar() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
                    ( ( lhsT->getBaseType() == Type::NIL || ltag == Thing::T_Pointer || ltag == Thing::T_ProcType ) &&
                    ( rhsT->getBaseType() == Type::NIL || rtag == Thing::T_Pointer || rtag == Thing::T_ProcType ) ) )
            {
                emitOpcode("ceq", -2+1, me->d_loc );
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                emitOpcode("ldc.i4.1",1,me->d_loc);
                emitOpcode("call bool [OBX.Runtime]OBX.Runtime::relOp(char[],char[],int)", -3+1, me->d_loc );
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::NEQ:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( lhsT->getBaseType() == Type::BOOLEAN && rhsT->getBaseType() == Type::BOOLEAN ) ||
                    ( lhsT->getBaseType() == Type::SET && rhsT->getBaseType() == Type::SET) ||
                    ( lhsT->isChar() && rhsT->isChar() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
                    ( ( lhsT->getBaseType() == Type::NIL || ltag == Thing::T_Pointer || ltag == Thing::T_ProcType ) &&
                    ( rhsT->getBaseType() == Type::NIL || rtag == Thing::T_Pointer || rtag == Thing::T_ProcType ) ) )
            {
                emitOpcode("ceq", -2+1, me->d_loc );
                emitOpcode("ldc.i4.0", 1, me->d_loc);
                emitOpcode("ceq", -2+1, me->d_loc);
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                emitOpcode("ldc.i4.2",1,me->d_loc);
                emitOpcode("call bool [OBX.Runtime]OBX.Runtime::relOp(char[],char[],int)", -3+1, me->d_loc );
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::LT:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
                    ( lhsT->isChar() && rhsT->isChar() ) )
            {
                emitOpcode("clt", -2+1, me->d_loc );
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                emitOpcode("ldc.i4.3",1,me->d_loc);
                emitOpcode("call bool [OBX.Runtime]OBX.Runtime::relOp(char[],char[],int)", -3+1, me->d_loc );
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::LEQ:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
                    ( lhsT->isChar() && rhsT->isChar() ) )
            {
                emitOpcode("cgt", -2+1, me->d_loc );
                emitOpcode("ldc.i4.0", 1, me->d_loc);
                emitOpcode("ceq", -2+1, me->d_loc);
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                emitOpcode("ldc.i4.4",1,me->d_loc);
                emitOpcode("call bool [OBX.Runtime]OBX.Runtime::relOp(char[],char[],int)", -3+1, me->d_loc );
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::GT:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
                    ( lhsT->isChar() && rhsT->isChar() ) )
            {
                emitOpcode("cgt", -2+1, me->d_loc );
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                emitOpcode("ldc.i4.5",1,me->d_loc);
                emitOpcode("call bool [OBX.Runtime]OBX.Runtime::relOp(char[],char[],int)", -3+1, me->d_loc );
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::GEQ:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
                    ( lhsT->isChar() && rhsT->isChar() ) )
            {
                emitOpcode("clt", -2+1, me->d_loc );
                emitOpcode("ldc.i4.0", 1, me->d_loc);
                emitOpcode("ceq", -2+1, me->d_loc);
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                emitOpcode("ldc.i4.6",1,me->d_loc);
                emitOpcode("call bool [OBX.Runtime]OBX.Runtime::relOp(char[],char[],int)", -3+1, me->d_loc );
            }else
                Q_ASSERT(false);
            break;
        default:
            Q_ASSERT(false);
        }
        Q_ASSERT( before+1 == stackDepth );
    }

    void visit( SetExpr* me)
    {
        emitOpcode("ldc.i4.0", 1, me->d_loc );
        for( int i = 0; i < me->d_parts.size(); i++ )
        {
            BinExpr* bi = me->d_parts[i]->getTag() == Thing::T_BinExpr ? cast<BinExpr*>( me->d_parts[i].data() ) : 0;
            if( bi && bi->d_op == BinExpr::Range )
            {
                // set or 0 already on stack
                if( bi->d_lhs )
                    bi->d_lhs->accept(this);
                if( bi->d_rhs )
                    bi->d_rhs->accept(this);
                // set, from and to index on stack
                emitOpcode("call int [OBX.Runtime]OBX.Runtime::addRangeToSet(int, int, int)", -3+1, me->d_loc );
                // new set on stack
            }else
            {
                // set or 0 already on stack
                me->d_parts[i]->accept(this);
                // element index on stack
                emitOpcode("call int [OBX.Runtime]OBX.Runtime::addElemToSet(int, int)", -2+1, me->d_loc );
                // new set on stack
            }
        }
    }

    void visit( Call* me)
    {
        Q_ASSERT( me->d_what );
        me->d_what->accept(this);
        if( !me->d_what->d_type.isNull() &&
                (me->d_what->d_type->getTag() == Thing::T_BaseType && me->d_what->d_type->getBaseType() != Type::NONE ) )
            emitOpcode("pop", -1, me->d_loc );
    }

    void visit( ForLoop* me)
    {
        const int before = stackDepth;
        // i := from;
        // WHILE i <= to DO statements; i := i + by END
        // WHILE i >= to DO statements; i := i + by END

        Ref<Assign> a = new Assign();
        a->d_loc = me->d_loc;
        a->d_lhs = me->d_id;
        a->d_rhs = me->d_from;

        Ref<IfLoop> loop = new IfLoop();
        loop->d_loc = me->d_loc;
        loop->d_op = IfLoop::WHILE;

        Ref<BinExpr> cond = new BinExpr();
        cond->d_loc = me->d_loc;
        if( me->d_byVal.toInt() > 0 )
            cond->d_op = BinExpr::LEQ;
        else
            cond->d_op = BinExpr::GEQ;
        cond->d_lhs = me->d_id;
        cond->d_rhs = me->d_to;
        cond->d_type = me->d_id->d_type.data();
        loop->d_if.append( cond.data() );

        loop->d_then.append( me->d_do );

        Ref<BinExpr> add = new BinExpr();
        add->d_loc = me->d_loc;
        add->d_op = BinExpr::ADD;
        add->d_lhs = me->d_id;
        add->d_rhs = me->d_by;
        add->d_type = me->d_by->d_type;

        Ref<Assign> a2 = new Assign();
        a2->d_loc = me->d_loc;
        a2->d_lhs = me->d_id;
        a2->d_rhs = add.data();

        loop->d_then.back().append( a2.data() );

        a->accept(this);
        loop->accept(this);
        Q_ASSERT( before == stackDepth );
    }

    void emitIf( IfLoop* me)
    {
        me->d_if[0]->accept(this); // IF
        const int afterFirst = labelCount++;
        emitOpcode("brfalse '#"+QByteArray::number(afterFirst)+"'", -1, me->d_loc);

        for( int i = 0; i < me->d_then[0].size(); i++ )
            me->d_then[0][i]->accept(this);

        const int afterEnd = labelCount++;
        emitOpcode("br '#"+QByteArray::number(afterEnd)+"'", 0, me->d_loc);

        out << "'#" << afterFirst << "':" << endl;
        for( int i = 1; i < me->d_if.size(); i++ ) // ELSIF
        {
            me->d_if[i]->accept(this);
            const int afterNext = labelCount++;
            emitOpcode("brfalse '#"+QByteArray::number(afterNext)+"'", -1, me->d_loc);

            for( int j = 0; j < me->d_then[i].size(); j++ )
                me->d_then[i][j]->accept(this);

            emitOpcode("br '#"+QByteArray::number(afterEnd)+"'", 0, me->d_loc);

            out << "'#" << afterNext << "':" << endl;
        }

        if( !me->d_else.isEmpty() ) // ELSE
        {
            for( int j = 0; j < me->d_else.size(); j++ )
                me->d_else[j]->accept(this);
        }

        out << "'#" << afterEnd << "':" << endl;
    }

    void visit( IfLoop* me)
    {
        const int before = stackDepth;
        switch( me->d_op )
        {
        case IfLoop::IF:
            emitIf(me);
            break;
        case IfLoop::WHILE:
            {
                // substitute by primitive statements
                Ref<IfLoop> loop = new IfLoop();
                loop->d_op = IfLoop::LOOP;
                loop->d_loc = me->d_loc;

                Ref<IfLoop> conds = new IfLoop();
                conds->d_op = IfLoop::IF;
                conds->d_loc = me->d_loc;

                conds->d_if = me->d_if;
                conds->d_then = me->d_then;

                Q_ASSERT( me->d_else.isEmpty() );
                Ref<Exit> ex = new Exit();
                ex->d_loc = me->d_loc;
                conds->d_else << ex.data();

                loop->d_then << ( StatSeq() << conds.data() );

                loop->accept(this); // now render
            }
            break;
        case IfLoop::REPEAT:
            {
                const int loopStart = labelCount++;
                out << "'#" << loopStart << "':" << endl;

                for( int i = 0; i < me->d_then.first().size(); i++ )
                    me->d_then.first()[i]->accept(this);

                me->d_if[0]->accept(this); // until condition
                const int afterEnd = labelCount++;
                emitOpcode("brtrue '#"+QByteArray::number(afterEnd)+"'", -1, me->d_loc);

                emitOpcode("br '#"+QByteArray::number(loopStart)+"'", 0, me->d_loc);

                out << "'#" << afterEnd << "':" << endl;
            }
            break;
        case IfLoop::WITH:
            {
                // if guard then statseq elsif guard then statseq else statseq end
                // guard ::= lhs IS rhs
                emitIf(me);
            }
            break;
        case IfLoop::LOOP:
            {
                const int loopStart = labelCount++;
                out << "'#" << loopStart << "':" << endl;

                for( int i = 0; i < me->d_then.first().size(); i++ )
                    me->d_then.first()[i]->accept(this);

                emitOpcode("br '#"+QByteArray::number(loopStart)+"'", 0, me->d_loc);

                if( exitJump != -1 )
                {
                    out << "'#" << exitJump << "':" << endl;
                    exitJump = -1;
                }
            }
            break;
        }
        Q_ASSERT( before == stackDepth );
    }

    void visit( Assign* me )
    {
        const int before = stackDepth;
        Q_ASSERT( me->d_rhs );

        Q_ASSERT( me->d_lhs );

        Q_ASSERT( !me->d_lhs->d_type.isNull() );
        Q_ASSERT( !me->d_rhs->d_type.isNull() );

        Type* lhsT = derefed(me->d_lhs->d_type.data());
        Q_ASSERT( lhsT != 0 );

        if( lhsT->isStructured() )
        {
            me->d_lhs->accept(this);
            me->d_rhs->accept(this);
            switch(lhsT->getTag())
            {
            case Thing::T_Record:
                {
                    // stack: lhs record, rhs record
                    emitOpcode2("callvirt ", -2, me->d_loc);
                    Record* r = cast<Record*>(lhsT);
                    out << "instance void " << classRef(r) + "::'#copy'(";
                    r->accept(this);
                    if( r->d_byValue )
                        out << "&";
                    out <<")" << endl;
                }
                break;
            case Thing::T_Array:
                {
                    // stack: lhs array, lhs array, rhs array
                    QList<Array*> dims = cast<Array*>(lhsT)->getDims();
                    emitOpcode("ldc.i4 "+QByteArray::number(dims.size()-1),1,me->d_loc);
                    emitOpcode2("call ", -3, me->d_loc);
                    emitArrayCopierRef(dims.last()->d_type.data(), dims.size());
                    out << endl;
                }
                break;
            default:
                Q_ASSERT(false);
            }
        }else
        {
            emitFetchDesigAddr(me->d_lhs.data());
            me->d_rhs->accept(this);
            prepareRhs(lhsT, me->d_rhs.data(), me->d_loc );
            switch( lhsT->getTag() )
            {
            case Thing::T_Pointer:
            case Thing::T_ProcType:
                emitOpcode("stind.ref",-2, me->d_loc);
                break;
            case Thing::T_Enumeration:
                emitOpcode("stind.i4",-2, me->d_loc);
                break;
            case Thing::T_BaseType:
                switch( lhsT->getBaseType() )
                {
                case Type::LONGREAL:
                    emitOpcode("conv.r8",0,me->d_loc);
                    emitOpcode("stind.r8",-2, me->d_loc);
                    break;
                case Type::REAL:
                    emitOpcode("conv.r4",0,me->d_loc);
                    emitOpcode("stind.r4",-2, me->d_loc);
                    break;
                case Type::LONGINT:
                    emitOpcode("conv.i8",0,me->d_loc);
                    emitOpcode("stind.i8",-2, me->d_loc);
                    break;
                case Type::INTEGER:
                case Type::SET:
                case Type::BOOLEAN:
                    emitOpcode("conv.i4",0,me->d_loc);
                    emitOpcode("stind.i4",-2, me->d_loc);
                    break;
                case Type::SHORTINT:
                case Type::CHAR:
                case Type::WCHAR:
                    emitOpcode("conv.i2",0,me->d_loc);
                    emitOpcode("stind.i2",-2, me->d_loc);
                    break;
                case Type::BYTE:
                    emitOpcode("conv.u1",0,me->d_loc);
                    emitOpcode("stind.i1",-2, me->d_loc);
                    break;
                default:
                    Q_ASSERT(false);
                }
                break;
            default:
                Q_ASSERT(false);
            }
        }
        Q_ASSERT( before == stackDepth );
    }

    void visit( CaseStmt* me)
    {
        const int before = stackDepth;
        if( me->d_typeCase )
        {
            // first rewrite the AST with 'if' instead of complex 'case'

            if( me->d_cases.isEmpty() )
                return;

            Ref<IfLoop> ifl = new IfLoop();
            ifl->d_op = IfLoop::IF;
            ifl->d_loc = me->d_loc;

            for( int i = 0; i < me->d_cases.size(); i++ )
            {
                const CaseStmt::Case& c = me->d_cases[i];

                Q_ASSERT( c.d_labels.size() == 1 );

                Ref<BinExpr> eq = new BinExpr();
                eq->d_op = BinExpr::IS;
                eq->d_lhs = me->d_exp;
                eq->d_rhs = c.d_labels.first();
                eq->d_loc = me->d_exp->d_loc;
                eq->d_type = new BaseType(Type::BOOLEAN);

                ifl->d_if.append(eq.data());
                ifl->d_then.append( c.d_block );
            }

            // and now generate code for the if
            ifl->accept(this);
        }
        else
        {
            // first rewrite the AST with 'if' instead of complex 'case'

            Ref<IfLoop> ifl = new IfLoop();
            ifl->d_op = IfLoop::IF;
            ifl->d_loc = me->d_loc;

            Ref<BaseType> boolean = new BaseType(Type::BOOLEAN);

            for( int i = 0; i < me->d_cases.size(); i++ )
            {
                const CaseStmt::Case& c = me->d_cases[i];

                QList< Ref<Expression> > ors;
                for( int j = 0; j < c.d_labels.size(); j++ )
                {
                    Expression* l = c.d_labels[j].data();
                    // TODO: avoid redundant evaluations using temp vars
                    bool done = false;
                    if( l->getTag() == Thing::T_BinExpr )
                    {
                        BinExpr* bi = cast<BinExpr*>( l );
                        if( bi->d_op == BinExpr::Range )
                        {
                            Ref<BinExpr> _and = new BinExpr();
                            _and->d_op = BinExpr::AND;
                            _and->d_loc = l->d_loc;
                            _and->d_type = boolean.data();

                            Ref<BinExpr> lhs = new BinExpr();
                            lhs->d_op = BinExpr::GEQ;
                            lhs->d_lhs = me->d_exp;
                            lhs->d_rhs = bi->d_lhs;
                            lhs->d_loc = l->d_loc;
                            lhs->d_type = boolean.data();

                            Ref<BinExpr> rhs = new BinExpr();
                            rhs->d_op = BinExpr::LEQ;
                            rhs->d_lhs = me->d_exp;
                            rhs->d_rhs = bi->d_rhs;
                            rhs->d_loc = l->d_loc;
                            rhs->d_type = boolean.data();

                            _and->d_lhs = lhs.data();
                            _and->d_rhs = rhs.data();

                            ors << _and.data();
                            done = true;
                        }
                    }
                    if( !done )
                    {
                        Ref<BinExpr> eq = new BinExpr();
                        eq->d_op = BinExpr::EQ;
                        eq->d_lhs = me->d_exp;
                        eq->d_rhs = l;
                        eq->d_loc = l->d_loc;
                        eq->d_type = boolean.data();

                        ors << eq.data();
                    }
                }
                Q_ASSERT( !ors.isEmpty() );
                if( ors.size() == 1 )
                    ifl->d_if.append( ors.first() );
                else
                {
                    Q_ASSERT( ors.size() > 1 );
                    Ref<BinExpr> bi = new BinExpr();
                    bi->d_op = BinExpr::OR;
                    bi->d_lhs = ors[0];
                    bi->d_rhs = ors[1];
                    bi->d_loc = ors[1]->d_loc;
                    bi->d_type = boolean.data();
                    for( int i = 2; i < ors.size(); i++ )
                    {
                        Ref<BinExpr> tmp = new BinExpr();
                        tmp->d_op = BinExpr::OR;
                        tmp->d_lhs = bi.data();
                        tmp->d_type = boolean.data();
                        bi = tmp;
                        bi->d_rhs = ors[i];
                        bi->d_loc = ors[i]->d_loc;
                    }
                    ifl->d_if.append( bi.data() );
                }

                ifl->d_then.append( c.d_block );
            }

            // and now generate code for the if
            ifl->accept(this);
        }
        Q_ASSERT( before == stackDepth );
    }

    void visit( Exit* me)
    {
        const int before = stackDepth;
        if( exitJump < 0 )
            exitJump = labelCount++;
        emitOpcode("br '#"+QByteArray::number(exitJump)+"'",0,me->d_loc);
        Q_ASSERT( before == stackDepth );
    }

    void emitReturn(ProcType* pt, Expression* what, const RowCol& loc)
    {
        Q_ASSERT( pt );

        if( what )
        {
            Type* lt = pt->d_return.data();
            Type* ltd = derefed(lt);
            if( ltd && ltd->isStructured() )
            {
                emitInitializer(lt,false,loc); // create new record or array
                emitOpcode("dup", 1, loc);
                what->accept(this);
                switch(ltd->getTag())
                {
                case Thing::T_Record:
                    {
                        // stack: new record, new record, rhs record
                        emitOpcode2("callvirt ", -2, loc);
                        Record* r = cast<Record*>(ltd);
                        out << "instance void " << classRef(r) + "::'#copy'(";
                        r->accept(this);
                        if( r->d_byValue )
                            out << "&";
                        out <<")" << endl;
                    }
                    break;
                case Thing::T_Array:
                    {
                        // stack: new array, new array, rhs array
                        QList<Array*> dims = cast<Array*>(ltd)->getDims();
                        emitOpcode("ldc.i4 "+QByteArray::number(dims.size()-1),1,loc);
                        emitOpcode2("call ", -3, loc);
                        emitArrayCopierRef(dims.last()->d_type.data(), dims.size());
                        out << endl;
                    }
                    break;
                default:
                    Q_ASSERT(false);
                }
            }else
            {
                what->accept(this);
                prepareRhs( ltd, what, loc );
            }
            emitOpcode("ret", -1, loc);
        }else if( !pt->d_return.isNull() )
        {
            // a function with no body; return default value
            emitInitializer(pt->d_return.data(),false,loc);
            emitOpcode("ret", -1, loc);
        }else
        {
            emitOpcode("ret", 0, loc);
        }
    }

    void visit( Return* me )
    {
        const int before = stackDepth;
        Q_ASSERT( scope != 0 );
        emitReturn( scope->getProcType(), me->d_what.data(), me->d_loc );
        Q_ASSERT( before == stackDepth );
    }

    static inline Type* derefed( Type* t )
    {
        if( t )
            return t->derefed();
        else
            return 0;
    }

    void emitOpcode( const QByteArray& op, int stackDelta, const RowCol& loc, bool withEndl = true )
    {
        stackDepth += stackDelta;
        Q_ASSERT( stackDepth >= 0 );
        if( stackDepth > maxStackDepth )
            maxStackDepth = stackDepth;
        if( !(loc == last) )
        {
            out << ws() << ".line " << loc.d_row << ":" << loc.d_col << endl; // filename is given by current assembly
            last = loc;
        }
        out << ws() << op;
        if( withEndl )
            out << endl;
    }

    inline void emitOpcode2(const QByteArray& op, int stackDelta, const RowCol& loc)
    {
        emitOpcode(op, stackDelta,loc,false);
    }

    bool emitInitializer( Type* t, bool resolvePtr, const RowCol& loc, const QList<int>& lengths = QList<int>() )
    {
        const int before = stackDepth;
        // note that this proc is also called if t is a pointer

        // expects non-derefed t!
        Type* td = derefed(t);
        Q_ASSERT( td );

        if( resolvePtr && td->getTag() == Thing::T_Pointer )
            td = derefed(cast<Pointer*>(td)->d_to.data());

        switch( td->getTag() )
        {
        case Thing::T_BaseType:
            // at least the oberon system assumes initialized module variables
            switch( td->getBaseType() )
            {
            case Type::BOOLEAN:
            case Type::CHAR:
            case Type::WCHAR:
            case Type::BYTE:
            case Type::SHORTINT:
            case Type::INTEGER:
            case Type::SET:
                emitOpcode("ldc.i4 0",1,loc);
                break;
            case Type::LONGINT:
                emitOpcode("ldc.i8 0",1,loc);
                break;
            case Type::REAL:
                emitOpcode("ldc.r4 0.0",1,loc);
                break;
            case Type::LONGREAL:
                emitOpcode("ldc.r8 0.0",1,loc);
                break;
            default:
                Q_ASSERT( false );
                break;
            }
            Q_ASSERT( stackDepth == before+1 );
            return true;
        case Thing::T_Enumeration:
            emitOpcode("ldc.i4 0",1,loc);
            Q_ASSERT( stackDepth == before+1 );
            return true;
#if 0 // not needed with CLI
        case Thing::T_Pointer:
            emitOpcode("ldnull",1,loc);
            Q_ASSERT( stackDepth == before+1 );
            return true;
#endif
        case Thing::T_Record:
            {
                Record* r = cast<Record*>(td);
                Q_ASSERT( !r->d_byValue );

                emitOpcode2("newobj",1,loc);
                out << " instance void " << classRef(r) << "::.ctor()" << endl; // initializes fields incl. superclasses
            }
            Q_ASSERT( stackDepth == before+1 );
            return true;
        case Thing::T_Array:
            {
                Array* a = cast<Array*>(td);
                Type* td = derefed(a->d_type.data());

                int len = -1;
                if( !lengths.isEmpty() )
                {
                    Q_ASSERT( a->d_lenExpr.isNull() );
                    len = lengths.first();
                    emitOpcode("ldloc "+QByteArray::number(len),1,loc);
                }else
                {
                    Q_ASSERT( !a->d_lenExpr.isNull() );
                    emitOpcode2("ldc.i4",1,loc);
                    out << " " << a->d_len << endl;
                    if( td->isStructured() )
                    {
                        len = temps.buy();
                        emitOpcode("dup",1,loc);
                        emitOpcode("stloc "+QByteArray::number(len),-1,loc);
                    }
                }
                // here the len is on the stack, either from constant or
                emitOpcode2("newarr ", 0, loc );
                td->accept(this);
                out << endl;

                if( td->isStructured() )
                {
                    const int i = temps.buy();
                    Q_ASSERT( i >= 0 );
                    emitOpcode("ldc.i4.0",1,loc);
                    emitOpcode("stloc "+QByteArray::number(i),-1,loc);
#if 0 // works with .Net 4.0 Windows, but neither with Mono 3 nor 5 (runtime exception because of dup (the one at loopStartLbl)
                    const int checkLenLbl = labelCount++;
                    emitOpcode("br '#"+QByteArray::number(checkLenLbl)+"'",0,loc);

                    const int loopStartLbl = labelCount++;
                    out << "'#" << loopStartLbl << "':" << endl;
                    emitOpcode("dup",1,loc);
                    // new array on top
                    emitOpcode("ldloc "+QByteArray::number(i),1,loc);
                    // index on top

                    if( lengths.size() > 1 )
                        emitInitializer(a->d_type.data(), false, false, loc, lengths.mid(1) );
                    else
                        emitInitializer(a->d_type.data(), false, false, loc );
                    // now the array value is on top of the stack
                    emitOpcode2("stelem ", -3, loc );
                    a->d_type->accept(this);
                    out << endl;

                    emitOpcode("ldloc "+QByteArray::number(i),1,loc);
                    emitOpcode("ldc.i4.1",1,loc);
                    emitOpcode("add",-1,loc);
                    emitOpcode("stloc "+QByteArray::number(i),-1,loc);

                    out << "'#" << checkLenLbl << "':" << endl;
                    emitOpcode("ldloc "+QByteArray::number(i),1,loc);
                    emitOpcode("ldloc "+QByteArray::number(len),1,loc);
                    emitOpcode("blt '#"+QByteArray::number(loopStartLbl)+"'",-2,loc);
#else // works with Mono 3 and .Net 4.0 Windows
                    // apparently Mono doesn't like dup after br; looks like a verifier issue
                    const int checkLenLbl = labelCount++;
                    out << "'#" << checkLenLbl << "':" << endl;
                    emitOpcode("ldloc "+QByteArray::number(i),1,loc);
                    emitOpcode("ldloc "+QByteArray::number(len),1,loc);
                    const int afterLoopLbl = labelCount++;
                    emitOpcode("bge '#"+QByteArray::number(afterLoopLbl)+"'",-2,loc);

                    emitOpcode("dup",1,loc);
                    // new array on top
                    emitOpcode("ldloc "+QByteArray::number(i),1,loc);
                    // index on top

                    if( lengths.size() > 1 )
                        emitInitializer(a->d_type.data(), false, loc, lengths.mid(1) );
                    else
                        emitInitializer(a->d_type.data(), false, loc );
                    // now the array value is on top of the stack
                    emitOpcode2("stelem ", -3, loc );
                    a->d_type->accept(this);
                    out << endl;

                    emitOpcode("ldloc "+QByteArray::number(i),1,loc);
                    emitOpcode("ldc.i4.1",1,loc);
                    emitOpcode("add",-1,loc);
                    emitOpcode("stloc "+QByteArray::number(i),-1,loc);
                    emitOpcode("br '#"+QByteArray::number(checkLenLbl)+"'",0,loc);
                    out << "'#" << afterLoopLbl << "':" << endl;
#endif
                    temps.sell(i);
                }
                if( len >= 0 )
                    temps.sell(len);
                // leaves new array on top of stack
            }
            Q_ASSERT( stackDepth == before+1 );
            return true;
        }
        Q_ASSERT( stackDepth == before );
        return false;
    }

    void emitStackToVar( Named* me, const RowCol& loc )
    {
        switch( me->getTag() )
        {
        case Thing::T_Field:
            emitOpcode2("stfld", -2, loc);
            out << " ";
            me->d_type->accept(this);
            out << " " << fieldRef(me) << endl;
            break;
        case Thing::T_Variable:
            emitOpcode2("stsfld", -1, loc);
            out << " ";
            me->d_type->accept(this);
            out << " " << variableRef(me) << endl;
            break;
        case Thing::T_LocalVar:
            Q_ASSERT( me->d_slotValid );
            if( me->d_slot <= 0xff )
                emitOpcode2("stloc.s", -1,loc);
            else
                emitOpcode2("stloc", -1, loc);
            out << " " << QByteArray::number(me->d_slot) << endl;
            break;
        case Thing::T_Parameter:
            Q_ASSERT( me->d_slotValid );
            if( me->d_slot <= 0xff )
                emitOpcode2("starg.s", -1, loc);
            else
                emitOpcode2("starg", -1, loc);
            out << " " << QByteArray::number(me->d_slot) << endl;
            break;
        }
    }

    void emitVarToStack( Named* me, const RowCol& loc )
    {
        switch( me->getTag() )
        {
        case Thing::T_Field:
            emitOpcode2("ldfld", 0, loc);
            out << " ";
            me->d_type->accept(this);
            out << " " << fieldRef(me) << endl;
            break;
        case Thing::T_Variable:
            emitOpcode2("ldsfld", 1, loc);
            out << " ";
            me->d_type->accept(this);
            out << " " << variableRef(me) << endl;
            break;
        case Thing::T_LocalVar:
            if( me->d_slot <= 0xff )
                emitOpcode2("ldloc.s", 1,loc);
            else
                emitOpcode2("ldloc", 1, loc);
            out << " " << QByteArray::number(me->d_slot) << endl;
            break;
        case Thing::T_Parameter:
            if( me->d_slot <= 0xff )
                emitOpcode2("ldarg.s", 1, loc);
            else
                emitOpcode2("ldarg", 1, loc);
            out << " " << QByteArray::number(me->d_slot) << endl;
            break;
        }
    }

    void emitCalcLengths( Type* t, QList<int>& lengths, const RowCol& loc )
    {
        Q_ASSERT( t );
        Array* a = 0;
        while( t->getTag() == Thing::T_Array && ( a = cast<Array*>(t) ) && a->d_lenExpr.isNull() )
        {
            // array is on the stack
            emitOpcode("dup", 1, loc);
            emitOpcode("ldlen", -1+1, loc );
            const int len = temps.buy();
            lengths.append(len);
            emitOpcode("stloc "+QByteArray::number(len),-1,loc);
            emitOpcode("ldc.i4.0",1,loc);
            emitOpcode2("ldelem", -2 + 1, loc);
            out << " "; a->d_type->accept(this); out << endl;
            t = derefed(a->d_type.data() );
        }
        emitOpcode("pop", -1, loc);
    }

    void emitInitializer( Named* me )
    {
        const int before = stackDepth;
        switch( me->getTag() )
        {
        case Thing::T_Variable:
        case Thing::T_LocalVar:
            if( emitInitializer( me->d_type.data(), false, me->d_loc ) )
                emitStackToVar( me, me->d_loc );
            break;
        case Thing::T_Parameter:
            {
                Parameter* p = cast<Parameter*>(me);
                Type* t = derefed(p->d_type.data());
                if( !p->d_var && t && t->isStructured() )
                {
                    // make a copy if a structured value is not passed by VAR or IN
                    const int tag = t->getTag();
                    QList<int> lengths;
                    if( tag == Thing::T_Array && cast<Array*>(t)->d_lenExpr.isNull() )
                    {
                        // if formal par is an open array get the length from the passed actual array
                        emitVarToStack(me, me->d_loc);
                        emitCalcLengths( t, lengths, me->d_loc );
                    }
                    emitInitializer(me->d_type.data(),false, me->d_loc, lengths );
                    // stack: array or record
                    emitOpcode("dup", 1, me->d_loc);
                    emitVarToStack(me, me->d_loc);
                    switch(t->getTag())
                    {
                    case Thing::T_Record:
                        {
                            // stack: lhs record, lhs record, rhs record
                            emitOpcode2("callvirt ", -2, me->d_loc);
                            Record* r = cast<Record*>(t);
                            out << "instance void " << classRef(r) + "::'#copy'(";
                            r->accept(this);
                            if( r->d_byValue )
                                out << "&";
                            out <<")" << endl;
                            // stack: lhs record
                        }
                        break;
                    case Thing::T_Array:
                        {
                            // stack: lhs array, lhs array, rhs array
                            QList<Array*> dims = cast<Array*>(t)->getDims();
                            emitOpcode("ldc.i4 "+QByteArray::number(dims.size()-1),1,me->d_loc);
                            emitOpcode2("call ", -3, me->d_loc);
                            emitArrayCopierRef(dims.last()->d_type.data(), dims.size());
                            out << endl;
                            // stack: lhs array
                        }
                        break;
                    default:
                        Q_ASSERT(false);
                    }
                    // store the new struct in param
                    emitStackToVar( me, me->d_loc );
                }
            }
            break;
        default:
            Q_ASSERT(false);
        }
        Q_ASSERT( before == stackDepth );
    }

    // NOP
    void visit( NamedType* ) {Q_ASSERT( false );}
    void visit( Const* ) {Q_ASSERT( false );}
    void visit( GenericName* ) {Q_ASSERT( false );}
    void visit( BuiltIn* ) {Q_ASSERT( false );}
    void visit( Parameter* ) { Q_ASSERT( false ); }
};

bool IlasmGen::translate(Module* m, QIODevice* out, Ob::Errors* errs)
{
    Q_ASSERT( m != 0 && out != 0 );

    if( m->d_hasErrors || !m->d_isValidated ) //  not validated can happen if imports cannot be resolved
        return false;

    if( m->d_isDef )
        return true;

    ObxIlasmGenImp imp;
    imp.thisMod = m;
    imp.dev = out;
    imp.out.setDevice(out);

    imp.out << "// Generated by " << qApp->applicationName() << " " << qApp->applicationVersion() << " on "
           << QDateTime::currentDateTime().toString(Qt::ISODate) << endl << endl;

    if( errs == 0 )
    {
        imp.err = new Errors();
        imp.err->setReportToConsole(true);
        imp.ownsErr = true;
    }else
    {
        imp.err = errs;
        imp.ownsErr = false;
    }
    const quint32 errCount = imp.err->getErrCount();


    m->accept(&imp);

    const bool hasErrs = ( imp.err->getErrCount() - errCount ) != 0;

    if( imp.ownsErr )
        delete imp.err;
    return hasErrs;
}

bool IlasmGen::generateMain(QIODevice* d, const QByteArray& name, const QByteArray& module, const QByteArray& function)
{
    Q_ASSERT( d );
    if( module.isEmpty() )
        return false;
    QTextStream out(d); // default is UTF-8, so no need to setCodec

    out << ".assembly '" << name << "' {}" << endl;
    out << ".module '"<< name << ".exe'" << endl;
    out << ".assembly extern mscorlib {}" << endl;
    out << ".assembly extern '" << module << "' {}" << endl;

    out << ".method static void main() cil managed {" << endl;
    out << "    .maxstack 1" << endl;
    out << "    .entrypoint" << endl;
    if( !function.isEmpty() )
        out << "    call void ['" << module << "']'" << module << "'::" << function << "()" << endl;
    else
        out << "    call void ['" << module << "']'" << module << "'::'ping#'()" << endl;
    out << "    ldstr \"this is " << name << ".main\"" << endl;
    out << "    call void [mscorlib]System.Console::WriteLine (string)" << endl;
    out << "    ret" << endl;
    out << "}" << endl;

    return true;
}

bool IlasmGen::generateMain(QIODevice* d, const QByteArray& name, const QByteArrayList& modules)
{
    Q_ASSERT( d );
    if( modules.isEmpty() )
        return false;
    QTextStream out(d); // default is UTF-8, so no need to setCodec

    out << ".assembly '" << name << "' {}" << endl;
    out << ".module '"<< name << ".exe'" << endl;
    out << ".assembly extern mscorlib {}" << endl;
    foreach( const QByteArray& mod, modules )
        out << ".assembly extern '" << mod << "' {}" << endl;

    out << ".method static void main() cil managed {" << endl;
    out << "    .maxstack 1" << endl;
    out << "    .entrypoint" << endl;
    foreach( const QByteArray& mod, modules )
        out << "    call void ['" << mod << "']'" << mod << "'::'ping#'()" << endl;
    out << "    ldstr \"this is " << name << ".main\"" << endl;
    out << "    call void [mscorlib]System.Console::WriteLine (string)" << endl;
    out << "    ret" << endl;
    out << "}" << endl;

    return true;
}

bool IlasmGen::translateAll(Project* pro, const QString& where)
{
    Q_ASSERT( pro );
    QDir outDir(where);
    if( where.isEmpty() )
    {
        QDir dir( qApp->applicationDirPath() );
        dir.mkpath("cli"); // TEST
        dir.cd("cli");
        outDir = dir;
    }

    QFile make( outDir.absoluteFilePath("build.sh") );
    if( !make.open(QIODevice::WriteOnly) )
    {
        qCritical() << "could not open for writing" << make.fileName();
        return false;
    }

    QTextStream out(&make);

    QList<Module*> mods = pro->getModulesToGenerate();
    const quint32 errCount = pro->getErrs()->getErrCount();
    QSet<Module*> generated;
    foreach( Module* m, mods )
    {
        if( m->d_synthetic )
            ; // NOP
        else if( m->d_hasErrors )
        {
            qDebug() << "terminating because of errors in" << m->d_name;
            return false;
        }else if( m->d_isDef )
        {
            // NOP, TODO
        }else
        {
            if( m->d_metaParams.isEmpty() )
            {
                QList<Module*> result;
                m->findAllInstances(result);
                result.append(m);
                foreach( Module* inst, result )
                {
                    // instances must be generated after the modules using them, otherwise we get !slotValid assertions
                    if( !generated.contains(inst) )
                    {
                        generated.insert(inst);
                        QFile f(outDir.absoluteFilePath(inst->getName() + ".il"));
                        if( f.open(QIODevice::WriteOnly) )
                        {
                            //qDebug() << "generating IL for" << m->getName() << "to" << f.fileName();
                            IlasmGen::translate(inst,&f,pro->getErrs());
                            out << "./ilasm /dll \"" << inst->getName() << ".il\"" << endl;
                        }else
                            qCritical() << "could not open for writing" << f.fileName();
                    }
                }
            }
        }
    }
    if( !mods.isEmpty() )
    {
        const QByteArray name = "Main#";
        QFile f(outDir.absoluteFilePath(name + ".il"));
        if( f.open(QIODevice::WriteOnly) )
        {
            const Project::ModProc& mp = pro->getMain();
            if( mp.first.isEmpty() )
            {
                QByteArrayList roots;
                for(int i = mods.size() - 1; i >= 0; i-- )
                {
                    if( mods[i]->d_usedBy.isEmpty() )
                        roots.append(mods[i]->getName());
                    else
                        break;
                }
                if( roots.isEmpty() )
                    roots.append(mods.last()->getName()); // shouldn't actually happenk
                IlasmGen::generateMain(&f,name,roots);
            }else
                IlasmGen::generateMain(&f, name,mp.first, mp.second);
            out << "./ilasm /exe \"" << name << ".il\"" << endl;
        }else
            qCritical() << "could not open for writing" << f.fileName();
    }

    QFile run( outDir.absoluteFilePath("run.sh") );
    if( !run.open(QIODevice::WriteOnly) )
    {
        qCritical() << "could not open for writing" << make.fileName();
        return false;
    }
    out.setDevice(&run);
    out << "export MONO_PATH=." << endl;
    out << "./mono Main#.exe" << endl;

    return pro->getErrs()->getErrCount() != errCount;
}

