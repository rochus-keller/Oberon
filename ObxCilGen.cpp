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

#include "ObxCilGen.h"
#include "ObxAst.h"
#include "ObErrors.h"
#include "ObxProject.h"
#include "ObxIlEmitter.h"
#include "ObxPelibGen.h"
#include <MonoTools/MonoMdbGen.h>
#include <QtDebug>
#include <QFile>
#include <QDir>
#include <QCryptographicHash>
using namespace Obx;
using namespace Ob;

#ifndef OBX_AST_DECLARE_SET_METATYPE_IN_HEADER
Q_DECLARE_METATYPE( Obx::Literal::SET )
#endif

#define _MY_GENERICS_ // using my own generics implementation instead of the dotnet generics;
                      // there is an architectural value type initialization issue with dotnet generics!
                      // NOTE that disabling this define most likely leads to errors, since no longer maintained

// #define _CLI_USE_PTR_TO_MEMBER_ // access struct members by native int + offset instead of memberref (no advantage so far)

// #define _CLI_DYN_STRUCT_VARIABLES_ // unsafe structs in module variables are dynamically allocated on heap
                                   // TODO: this doesnt seem to resolve the issue; still random crashes in SDL test;
                                   // the issue vanishes if either a) we use Mono 5, or b) the SDL function using the
                                   // address of the struct is called in a method (not on top level of the .cctor), regardless
                                   // whether the struct is a module or local variable, or c) we compile with ILASM instead
                                   // of Pelib ; this might be the urgent reason to switch to Mono5 also on Linux and
                                   // just accept the 30% speed-down; but it's likely a pelib issue

#define _CLI_VARARG_SUBST_PROCS_ // generated local substitution methods with the required signature instead of relying on
                                 // CLI pinvoke vararg implementation (which apparently doesn't work on all platforms/architectures).

// #define _CLI_PASS_RAW_FUNCTION_POINTER // TODO

// NOTE: even though CoreCLR replaced mscorlib by System.Private.CoreLib the generated code still runs with "dotnet Main.exe",
// but the directory with the OBX assemblies requires a Main.runtimeconfig.json file as generated below
// "dotnet.exe run" apparently creates an non-managed exe which loads coreclr.dll and the app assembly dll; mono5 (in contrast to 3)
// is able to disasm and even run the app assembly dll created by dotnet.exe CoreCLR 3.1.

struct ObxCilGenCollector : public AstVisitor
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
                {
                    collect(f->d_type.data());
                }
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
                if( !( pt->d_formals.isEmpty() && pt->d_return.isNull() ) ) // proc types with no params are mapped to OBX.Command
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
            }
#if 0
            // no, we only create delegates for proc types declared here
            else
            {
                t = t->derefed();
                if( t && t->getTag() == Thing::T_ProcType )
                    collect(t); // even if the proc type was declared in another module, we create a local delegate here
            }
#endif
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

struct CilGenTempPool
{
    enum { MAX_TEMP = 250 };
    std::bitset<MAX_TEMP> d_slots;
    QList<QByteArray> d_types;
    quint16 d_start;
    CilGenTempPool():d_start(0){}
    void reset(quint16 start)
    {
        d_slots.reset();
        d_start = start;
        d_types.clear();
    }
    int buy(const QByteArray& type)
    {
        int i = 0;
        while( i < d_types.size() && i < MAX_TEMP )
        {
            if( d_types[i] == type && !d_slots.test(i) )
            {
                d_slots.set(i);
                return i + d_start;
            }
            i++;
        }
        if( i < MAX_TEMP )
        {
            d_types.append(type);
            Q_ASSERT( !d_slots.test(i) );
            d_slots.set(i);
            return i + d_start;
        }
        Q_ASSERT( false );
        return -1;
    }
    void sell( int i )
    {
        Q_ASSERT( i >= d_start );
        d_slots.set(i-d_start,false);
    }
    void sellAll()
    {
        d_slots.reset();
    }
};

struct ObxCilGenImp : public AstVisitor
{
    Errors* err;
    Module* thisMod;
    IlEmitter* emitter;
    QString buffer;
    quint32 anonymousDeclNr; // starts with one, zero is an invalid slot
    qint16 level;
    bool ownsErr;
    bool forceAssemblyPrefix;
    bool forceFormalIndex;
    bool debug;
    bool arrayAsElementType;
    bool structAsPointer;
    bool checkPtrSize;
    RowCol last;
    CilGenTempPool temps;
    QHash<QByteArray, QPair<Array*,int> > copiers; // type string -> array, max dim count
    QHash<QByteArray,ProcType*> delegates; // signature hash -> signature
#ifdef _CLI_VARARG_SUBST_PROCS_
    QHash<Module*,QHash<Procedure*,QHash<QByteArray, QList<Type*> > > > substitutes; // replace vararg by overloads
#endif
    QList<QPair<int,int> > pinnedTemps; // pinned temp var -> write back var or -1
    int exitJump; // TODO: nested LOOPs
    Procedure* scope;
    int suppressLine;

    ObxCilGenImp():ownsErr(false),err(0),thisMod(0),anonymousDeclNr(1),level(0),
        exitJump(-1),scope(0),forceAssemblyPrefix(false),forceFormalIndex(false),
        suppressLine(0),debug(false),checkPtrSize(false),
        arrayAsElementType(false),structAsPointer(false)
    {
    }

    static QByteArray inline escape( const QByteArray& name )
    {
        return "'" + name + "'";
    }

    QByteArray dottedName( Named* n )
    {
        // concatenate names up to but not including module
        QByteArray name = n->d_name;
        Named* scope = n->d_scope;
        if( scope )
        {
            const int tag = scope->getTag();
            if( tag != Thing::T_Module )
            {
                if( tag == Thing::T_Procedure )
                {
                    Procedure* proc = cast<Procedure*>(scope);
                    if( proc->d_receiverRec )
                    {
                        // if the scope is a bound proc follow its receiver, but first use the proc name.
                        // this is necessary because procs bound to different recs can have the same name and
                        // even have the same name as ordinary procs, so there is a risk of duplicate names when
                        // just following the normal scope
                        name = scope->d_name + "#" + name;
                        scope = proc->d_receiverRec->findDecl();
                        Q_ASSERT( scope );
                    }
                }
                return dottedName(scope) + "#" + name;
            }
        }
        return name;
    }

    QByteArray nestedPath(Named* n)
    {
        return escape(dottedName(n));
    }

    QByteArray formatMetaActuals(Module* m)
    {
#ifndef _MY_GENERICS_
        if( m == thisMod && !m->d_metaParams.isEmpty() )
        {
            Q_ASSERT( m->d_metaActuals.isEmpty() );
            QByteArray res = "<";
            for( int i = 0; i < m->d_metaParams.size(); i++ )
            {
                if( i != 0 )
                    res += ",";
                Q_ASSERT( m->d_metaParams[i]->d_slotValid );
                res += "!" + QByteArray::number(m->d_metaParams[i]->d_slot);
            }
            res += ">";
            return res;
        }else if( !m->d_metaActuals.isEmpty() )
        {
            QByteArray res = "<";
            for( int i = 0; i < m->d_metaActuals.size(); i++ )
            {
                if( i != 0 )
                    res += ",";
                res += formatType(m->d_metaActuals[i].data());
            }
            res += ">";
            return res;
        }
#endif
        return QByteArray();
    }

    QByteArray formatMetaActuals(Type* t)
    {
        // t==0 -> module
        Module* m = 0;
        t = derefed(t);
        if( t == 0 )
            m = thisMod;
        else
            m = t->declaredIn();

        return formatMetaActuals(m);
    }

    inline QByteArray getName( Named* n )
    {
        Q_ASSERT(n);
#ifdef _MY_GENERICS_
        return n->getName();
#else
        if( n->getTag() == Thing::T_Module )
            return cast<Module*>(n)->d_fullName.join('.');
        else
            return n->d_name;
#endif
    }

    QByteArray moduleRef( Named* modName )
    {
        if( modName == 0 )
            return "???";
        Q_ASSERT( modName->getTag() == Thing::T_Module );
        const QByteArray mod = escape(cast<Module*>(modName)->d_name);
        if( !forceAssemblyPrefix && modName == thisMod )
            return mod;
        else
        {
            const QByteArray ass = escape(getName(modName));
            return "[" + ass + "]" + mod;
        }
    }

    QByteArray classRef( Named* className )
    {
        Q_ASSERT( className && className->getTag() == Thing::T_NamedType );
        Module* m = className->getModule();
#if 0
        if( m == 0 && className->d_type && className->d_type->derefed()->d_anyRec )
            return "object";
        else
#endif
            return moduleRef(m) + "/" + nestedPath(className); // dotted because also records nested in procs are lifted to module level
    }

    QByteArray classRef( Record* r )
    {
        Named* n = r->findDecl();
        if( n && n->getTag() == Thing::T_NamedType )
            return classRef(n);
        else
        {
#ifdef _DEBUG
            Q_ASSERT( r->d_slotValid );
#endif
            if( n == 0 )
                n = r->findDecl(true);
            Module* m = n ? n->getModule() : 0;
            if( m == 0 )
                m = thisMod;
            return moduleRef(m) + "/'#" + QByteArray::number(r->d_slot) + "'";
        }
    }

    QByteArray memberRef( Named* member, const QList<Type*>& varargs = QList<Type*>())
    {
        QByteArray res;
        Record* record = 0;
        ProcType* pt = 0;
        switch( member->getTag() )
        {
        case Thing::T_Field:
            {
                Field* f = cast<Field*>(member);
                record = f->d_owner;
            }
            break;
        case Thing::T_Variable:
            break;
        case Thing::T_Procedure:
            {
                Procedure* p = cast<Procedure*>(member);
                if( p->d_receiverRec )
                    record = p->d_receiverRec;
                pt = p->getProcType();
#ifdef _CLI_VARARG_SUBST_PROCS_
                if( !varargs.isEmpty() && pt->d_varargs )
                {
                    // substitute vararg function with local non-vararg function
                    const QByteArray sig = formatFormals(pt,false,varargs,false);
                    substitutes[member->getModule()][p][sig] = varargs;
                    return formatType(pt->d_return.data(),pt->d_unsafe) + " " +
                            moduleRef(thisMod) + "::" + escape( p->getModule()->getName() + "#" + p->d_name ) + sig;
                }
#endif
            }
            break;
        default:
            Q_ASSERT(false);
        }
        const QByteArray ma = formatMetaActuals(record);
        forceFormalIndex = !ma.isEmpty();
        if( pt )
        {
            if( !varargs.isEmpty() )
                res = "vararg ";
            res += formatType(pt->d_return.data(),pt->d_unsafe);
        }else
            res = formatType(member->d_type.data(),member->d_unsafe);
        res += " ";
        if( !ma.isEmpty() )
            res += "class "; // only if not my generics
        if( record == 0 ) // if module level
            res += moduleRef(member->getModule());
        else
            res += classRef(record);
        res += ma;
        res += "::";
        if( record == 0 ) // if module level
            res += nestedPath(member); // because of nested procedures which are lifted to module level
        else
            res += escape(member->d_name);
        if( pt )
            res += formatFormals(pt,varargs.isEmpty(),varargs);
        forceFormalIndex = false;
        return res;
    }

    QByteArray inline delegateName( const QByteArray& sig )
    {
        QCryptographicHash hash(QCryptographicHash::Md5);
        hash.addData(sig);
        return hash.result().toHex(); // issues because of '/': toBase64(QByteArray::OmitTrailingEquals);
    }

    QByteArray delegateRef( ProcType* pt )
    {
        if( pt == 0 )
            return "?";

        if( pt->d_formals.isEmpty() && pt->d_return.isNull() )
            return "[OBX.Runtime]OBX.Command";

        forceAssemblyPrefix = true;


#ifndef _MY_GENERICS_
        const bool old = forceFormalIndex;
        forceFormalIndex = true;
        const QByteArray sig = procTypeSignature(pt);
        forceFormalIndex = old;
#else
        const QByteArray sig = procTypeSignature(pt);
#endif
        const QByteArray name = "Ð" + delegateName(sig); // using Ð (U+00D0) instead of @ for C# compatibility
        if( pt->declaredIn() == thisMod )
            delegates.insert(name,pt);

        Module* m = pt->declaredIn();
        if( m == 0 )
            m = thisMod;
        const QByteArray res = moduleRef(m) + "/'" + name + "'" + formatMetaActuals(pt);
        forceAssemblyPrefix = false;
        return res;
    }

    QByteArray formatArrayCopierRef(Array* a)
    {
        Q_ASSERT(a);
        const QByteArray sig = formatType(a);
        QPair<Array*,int>& d = copiers[sig];
        if( d.first == 0 )
            d.first = a;
        QByteArray res = "void " + moduleRef(thisMod) + "::'#copy'(";
        res += sig;
        res += ", ";
        res += sig;
        res += ")";
        return res;
    }

    void emitArrayCopier( Array* a, const RowCol& loc )
    {
        // this is no longer for array of char

        Q_ASSERT(a);
        Type* et = derefed(a->d_type.data());
        Q_ASSERT(et);

        emitter->beginMethod("'#copy'", true, IlEmitter::Static );
        const QByteArray type = formatType(a);
        emitter->addArgument(type,"lhs");
        emitter->addArgument(type,"rhs");
        beginBody();

        line(loc); // the same line for the whole method
        const int len = temps.buy("int32");
        Q_ASSERT( len >= 0 );
        emitter->ldarg_(0);
        emitter->ldlen_();
        emitter->ldarg_(1);
        emitter->ldlen_();
        // stack: len lhs, len rhs
        const int lhsIsLen = emitter->newLabel();
        const int storeLen = emitter->newLabel();
        emitter->ble_(lhsIsLen);
        emitter->ldarg_(1);
        emitter->ldlen_();
        emitter->br_(storeLen);
        emitter->label_(lhsIsLen);
        emitter->ldarg_(0);
        emitter->ldlen_();
        emitter->label_(storeLen);
        emitter->stloc_(len); // len = qMin(lenLhs,lenRhs)

        const int idx = temps.buy("int32");
        Q_ASSERT( idx >= 0 );
        emitter->ldc_i4(0);
        emitter->stloc_(idx);

        const int checkLenLbl = emitter->newLabel();
        const int addLbl = emitter->newLabel();
        emitter->label_(checkLenLbl);
        emitter->ldloc_(idx);
        emitter->ldloc_(len);
        const int afterLoopLbl = emitter->newLabel();
        emitter->bge_(afterLoopLbl);

        if( et->getTag() == Thing::T_Array )
        {
            emitter->ldarg_(0);
            emitter->ldloc_(idx);
            // stack: array, int
            emitter->ldelem_(formatType(et));

            emitter->ldarg_(1);
            emitter->ldloc_(idx);
            // stack: array, array, int
            emitter->ldelem_(formatType(et));

            // stack: lhs array, rhs array
            emitCopyArray(et,et,loc);
            // emitter->call_(formatArrayCopierRef(cast<Array*>(et)),2);

            emitter->br_(addLbl);
        }else
        {
            switch( et->getTag() )
            {
            case Thing::T_Record:
                {
                    emitter->ldarg_(0);
                    emitter->ldloc_(idx);
                    // stack: array, int
                    emitter->ldelem_(formatType(et));

                    emitter->ldarg_(1);
                    emitter->ldloc_(idx);
                    // stack: record, array, int
                    emitter->ldelem_(formatType(et));

                    // stack: lhs record, rhs record
                    Record* r2 = cast<Record*>(et);
                    QByteArray type = formatType(r2);
                    if( r2->d_byValue )
                        type += "&";
                    emitter->callvirt_("void " + classRef(r2) + formatMetaActuals(r2) +
                                        "::'#copy'(" + type + ")", 1 );
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
                    emitter->ldarg_(0);
                    emitter->ldloc_(idx);
                    // stack: lhs array, int

                    emitter->ldarg_(1);
                    emitter->ldloc_(idx);
                    // stack: lhs array, int, rhs array, int

                    emitter->ldelem_(formatType(et));
                    // stack: lhs array, int, value

                    emitter->stelem_(formatType(et));
                }
                break;
            }
        }

        emitter->label_(addLbl);
        emitter->ldloc_(idx);
        emitter->ldc_i4(1);
        emitter->add_();
        emitter->stloc_(idx);
        emitter->br_(checkLenLbl);
        emitter->label_(afterLoopLbl);
        temps.sell(idx);
        temps.sell(len);

        emitter->ret_();
        emitLocalVars();
        emitter->endMethod();
    }

//#define _USE_VALUE_RECORDS_
    // no value records currently because the initialization works completely different; needs extra work

    void allocRecordDecl(Record* r)
    {
        if( r->d_slotValid )
            return; // can happen e.g. with VAR foo, bar: RECORD ch: CHAR; i: INTEGER END;
        Named* n = r->findDecl();
        if( n == 0 || n->getTag() != Thing::T_NamedType )
        {
            r->d_slot = anonymousDeclNr++;
            r->d_slotValid = true;
        }
    }

    void emitRecordDecl(Record* r)
    {
        if( r->d_slotAllocated )
            return;
        r->d_slotAllocated = true;
        Named* n = r->findDecl();

        QByteArray className, superClassName;
        bool isPublic = false;
        if( n == 0 || n->getTag() != Thing::T_NamedType )
        {
            Q_ASSERT(r->d_slotValid);
            className = "'#" + QByteArray::number(r->d_slot) + "'";
        }else
        {
            isPublic = n->d_scope == thisMod && n->d_visibility == Named::ReadWrite;
#ifdef _USE_VALUE_RECORDS_
            r->d_byValue = !isPublic && r->d_baseRec == 0 && r->d_subRecs.isEmpty();
#else
            r->d_byValue = false;
#endif
            className = nestedPath(n); // because of records declared in procedures are lifted to module level
            if( !r->d_base.isNull() )
                superClassName = formatType(r->d_base.data()); // unsafe rec has no basetype
        }
        emitter->beginClass(className, isPublic, r->d_unsafe ? IlEmitter::Value : IlEmitter::Object,
                            superClassName, r->d_unsafe ? r->getByteSize() : -1 );

        foreach( const Ref<Field>& f, r->d_fields )
            f->accept(this);
        foreach( const Ref<Procedure>& p, r->d_methods )
            p->accept(this);

        // NOTE: I verified that in case of unsafe structs there is really no marshalling; the struct address in
        // the dll is the same as in the Mono engine.

        QList<Field*> fields = r->getOrderedFields();
        // default constructor
        if( !r->d_unsafe ) // unsafe records use initobj; no constructor is called for unsafe records
        {
            emitter->beginMethod(".ctor",true);
            beginBody();
            line(r->d_loc).ldarg_(0);
            QByteArray what;
            if( r->d_baseRec )
            {
                Q_ASSERT( !r->d_unsafe );
                what = "void class " + classRef(r->d_baseRec) + formatMetaActuals(r->d_baseRec) + "::.ctor()";
            }else if( r->d_byValue )
                what = "void [mscorlib]System.ValueType::.ctor()";
            else
                what = "void [mscorlib]System.Object::.ctor()";
            line(r->d_loc).call_(what,1,false,true);

            // initialize fields of current record
            // NOTE safe records cannot have fields of unsafe structured types by value; thus no destructor required
            for( int i = 0; i < fields.size(); i++ )
            {
                // oberon system expects all vars to be initialized
                line(fields[i]->d_loc).ldarg_(0);
                if( emitInitializer(fields[i]->d_type.data(), false, fields[i]->d_loc ) )
                    emitStackToVar( fields[i], fields[i]->d_loc );
                else
                    line(fields[i]->d_loc).pop_();
            }
            line(r->d_loc).ret_();
            emitLocalVars();
            emitter->endMethod();
            // end default constructor
        }

        // copy
        if( !r->d_unsafe )
        {
            emitter->beginMethod("'#copy'",true, IlEmitter::Virtual);
            QByteArray type = formatType(r);
            if( r->d_byValue )
                type += "&";
            emitter->addArgument(type, "rhs");
            beginBody();
            if( r->d_baseRec )
            {
                line(r->d_loc).ldarg_(0);
                line(r->d_loc).ldarg_(1);
                QByteArray what = "void class " + classRef(r->d_baseRec) + formatMetaActuals(r->d_baseRec) + "::'#copy'(";
                type = formatType(r->d_baseRec);
                if( r->d_byValue )
                    type += "&";
                what += type + ")";
                line(r->d_loc).call_(what,1,false,true);
            }
            for( int i = 0; i < fields.size(); i++ )
            {
                Type* ft = derefed(fields[i]->d_type.data());
                switch( ft->getTag() )
                {
                case Thing::T_Record:
                    {
                        line(r->d_loc).ldarg_(0);
                        line(r->d_loc).ldfld_(memberRef(fields[i]));
                        line(r->d_loc).ldarg_(1);
                        line(r->d_loc).ldfld_(memberRef(fields[i]));
                        Record* r2 = cast<Record*>(ft);
                        QByteArray what = "void " + classRef(r2) + formatMetaActuals(r2) + "::'#copy'(";
                        type = formatType(r2);
                        if( r2->d_byValue )
                            type += "&";
                        what += type + ")";
                        line(r->d_loc).callvirt_(what,1);
                    }
                    break;
                case Thing::T_Array:
                    {
                        line(r->d_loc).ldarg_(0);
                        line(r->d_loc).ldfld_(memberRef(fields[i]));

                        line(r->d_loc).ldarg_(1);
                        line(r->d_loc).ldfld_(memberRef(fields[i]));

                        // stack: lhs array, rhs array
                        emitCopyArray(ft,ft,r->d_loc);
                        //line(r->d_loc).call_(formatArrayCopierRef(cast<Array*>(ft)),2);
                    }
                    break;
                case Thing::T_BaseType:
                case Thing::T_Enumeration:
                case Thing::T_Pointer:
                case Thing::T_ProcType:
                    line(r->d_loc).ldarg_(0);
                    line(r->d_loc).ldarg_(1);
                    line(r->d_loc).ldfld_(memberRef(fields[i]));
                    line(r->d_loc).stfld_(memberRef(fields[i]));
                    break;
                }
            }
            line(r->d_loc).ret_();
            emitLocalVars();
            emitter->endMethod();
            // end copy
        }

        emitter->endClass();
    }

    void emitDelegDecl(ProcType* sig, const QByteArray& name)
    {
        // NOTE: if the name deviates from the one used for referencing the delegate mono3 crashes with this message:
        // TypeRef ResolutionScope not yet handled (3) for .48b15Qezth5ae11+xOqLVw in image GenericTest6.dll
        // * Assertion at class.c:5695, condition `!mono_loader_get_last_error ()' not met

        emitter->beginClass(escape(name),true,IlEmitter::Delegate,"[mscorlib]System.MulticastDelegate");
        // formatMetaParams(thisMod)
        emitter->beginMethod(".ctor",true,IlEmitter::Instance,true);
        emitter->addArgument("object","MethodsClass");
        emitter->addArgument("native int", "MethodPtr");
        emitter->endMethod();
        emitter->beginMethod("Invoke",true,IlEmitter::Instance,true);
        if( !sig->d_return.isNull() || sig->d_unsafe )
        {
            QByteArray ret = formatType(sig->d_return.data(),sig->d_unsafe);
            if( sig->d_unsafe )
                ret += " modopt([mscorlib]System.Runtime.CompilerServices.CallConvCdecl)";
            emitter->setReturnType(ret);
        }
        for( int i = 0; i < sig->d_formals.size(); i++ )
        {
            QByteArray type = formatType(sig->d_formals[i]->d_type.data(),sig->d_unsafe);
            if( passByRef(sig->d_formals[i].data()) )
                type += "&";

            emitter->addArgument(type,escape(sig->d_formals[i]->d_name));
        }
        emitter->endMethod();
        emitter->endClass();
    }

    QByteArray formatMetaParams(Module* m)
    {
#ifdef _MY_GENERICS_
        return QByteArray();
#else
        if( m->d_metaParams.isEmpty() )
            return QByteArray();
        QByteArray res = "<";
        for( int i = 0; i < m->d_metaParams.size(); i++ )
        {
            if( i != 0 )
                res += ", ";
            res += escape(m->d_metaParams[i]->d_name);
        }
        res += ">";
        return res;
#endif
    }

    void visit( Module* me )
    {
        ObxCilGenCollector co;
        me->accept(&co);

        foreach( Import* imp, me->d_imports )
        {
            if(imp->d_mod->d_synthetic || imp->d_mod->d_isDef ) // TODO: def
                continue; // ignore SYSTEM
            co.allImports.insert(imp->d_mod.data());
            if( !imp->d_mod.isNull() && !imp->d_mod->d_metaActuals.isEmpty() )
            {
                for( int i = 0; i < imp->d_mod->d_metaActuals.size(); i++ )
                {
                    Type* at = imp->d_mod->d_metaActuals[i].data();
                    //Q_ASSERT( !at->d_slotValid );
                    at->d_slot = i;
                    at->d_slotValid = true;
                    at->d_metaActual = true;
                }
            }
        }
        QByteArrayList imports;
        imports.append( escape("mscorlib") );
        imports.append( escape("OBX.Runtime") );
        foreach( Module* m, co.allImports )
        {
            if( m && m != me )
                imports.append( escape(getName(m)) );
        }

        // NOTE: module name is always set in '' and thus doesn't have to be escaped
        emitter->beginModule(escape(me->getName()), escape(me->d_name), imports, thisMod->d_file);

        for( int i = 0; i < co.allProcTypes.size(); i++ )
            delegateRef(co.allProcTypes[i]);

        foreach( Record* r, co.allRecords )
            allocRecordDecl(r);

        foreach( Record* r, co.allRecords )
            emitRecordDecl(r);

        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Variable )
                n->accept(this);
        }
#ifndef _MY_GENERICS_
        if( !me->d_metaParams.isEmpty() && me->d_metaActuals.isEmpty() )
        {
            foreach( const Ref<GenericName>& n, me->d_metaParams )
            {
                Q_ASSERT( n->d_slotValid );
                out << ws() << ".field assembly static !" << n->d_slot << " '##" << n->d_slot << "'" << endl;
            }
        }
#endif

        foreach( Procedure* p, co.allProcs )
            p->accept(this);

        if( !me->d_externC )
        {
            // instead of .cctor we now have an ordinary begïn method which is explicitly called via import
            // dependency chain; this was necessary because during .cctor apparently not all relevant parts of
            // Mono are ready, e.g. Thread.Join doesn't work an blocks all running threads instead.
            // "begïn" (note the ï, U+00EF) is a compatible ident with C# (in contrast to e.g. begin#)
            emitter->addField(escape("beginCalled#"),"bool",false,true);

            emitter->beginMethod(".cctor", false, IlEmitter::Static );
            line(me->d_end).ldc_i4(0);
            line(me->d_end).stsfld_("bool " + moduleRef(thisMod)+"::'beginCalled#'");
            line(me->d_end).ret_(false);
            emitter->endMethod();

            emitter->beginMethod(escape("begïn"), false, IlEmitter::Static ); // MODULE BEGIN
            beginBody();
            line(me->d_end).ldsfld_("bool " + moduleRef(thisMod)+"::'beginCalled#'");
            const int callPending = emitter->newLabel();
            line(me->d_end).brfalse_(callPending);
            line(me->d_end).ret_(callPending);
            line(me->d_end).label_(callPending);
            line(me->d_end).ldc_i4(1);
            line(me->d_end).stsfld_("bool " + moduleRef(thisMod)+"::'beginCalled#'");

            foreach( Import* imp, me->d_imports )
            {
                if(imp->d_mod->d_synthetic )
                    continue; // ignore SYSTEM
                const QByteArray mod = moduleRef(imp->d_mod.data());
                line(me->d_end).call_("void " + mod + "::'begïn'()");
            }

#ifndef _MY_GENERICS_
            if( !me->d_metaParams.isEmpty() && me->d_metaActuals.isEmpty() )
            {
                foreach( const Ref<GenericName>& n, me->d_metaParams ) // generate default values
                {
                    // NOTE: this doesn't initialize OBX value types; e.g. in GenericTest3 l1.value is initialized to null instead
                    // of an empty array 20 of char; to get around a default constructor for all possible types, especially
                    // fixed size arrays, would be needed.
                    emitOpcode2("ldsflda ",  1, me->d_begin );
                    Q_ASSERT(n->d_slotValid);
                    out << "!" << QByteArray::number(n->d_slot) << " class " << moduleRef(me) << formatMetaActuals(me)
                        << "::'##" << n->d_slot << "'" << endl;
                    emitOpcode("initobj !"+escape(n->d_name),-1, me->d_begin);
                }
            }
#endif

            suppressLine++;
            foreach( const Ref<Named>& n, me->d_order )
            {
                if( n->getTag() == Thing::T_Variable )
                    emitInitializer(n.data());
            }
            suppressLine--;

            emitCheckPtrSize(me->d_begin); // after declarations

            foreach( const Ref<Statement>& s, me->d_body )
            {
                temps.sellAll();
                s->accept(this);
            }

            line(me->d_end).ret_(false);

            emitLocalVars();

            emitter->endMethod();
        }else
        {
            emitter->beginMethod(escape("begïn"), false, IlEmitter::Static ); // MODULE BEGIN
            if( checkPtrSize )
            {
                beginBody();
                emitCheckPtrSize(me->d_begin);
            }
            line(me->d_end).ret_(false);
            emitter->endMethod();
        }

        // ping# is no longer required because we explicitly call begïn of each module

        QSet<QByteArray> done;
        while( !copiers.isEmpty() )
        {
            QByteArray t = copiers.begin().key();
            //const int dims = copiers.begin().value().second;
            Array* a = copiers.begin().value().first;
            copiers.remove(t);
            if( done.contains(t) )
                continue;
            emitArrayCopier(a, me->d_end );
            done.insert(t);
        }

        QHash<QByteArray,ProcType*>::const_iterator i;
        for( i = delegates.begin(); i != delegates.end(); ++i )
            emitDelegDecl( i.value(), i.key() );


#ifdef _CLI_VARARG_SUBST_PROCS_
        QHash<Module*,QHash<Procedure*,QHash<QByteArray,QList<Type*> > > >::const_iterator s;
        for( s = substitutes.begin(); s != substitutes.end(); ++s )
        {
            QHash<Procedure*,QHash<QByteArray,QList<Type*> > >::const_iterator p;
            for( p = s.value().begin(); p != s.value().end(); ++p )
            {
                QHash<QByteArray,QList<Type*> >::const_iterator t;
                for( t = p.value().begin(); t != p.value().end(); ++t )
                    emitProcedure(p.key(), s.key()->getName() + "#", t.value() );
                // for each combination of parameters a local version of the imported external lib function
                // is created; this for one part avoids the issue that varargs in Mono (and CoreCLR?) are not
                // supported on all patforms/architectures and for the other part the unknows with Pelib
            }
        }
#endif

        emitter->endModule();
    }

    void emitCheckPtrSize(const RowCol& loc)
    {
        if( checkPtrSize )
        {
            line(loc).ldc_i4(Pointer::s_pointerByteSize);
            line(loc).call_("void [OBX.Runtime]OBX.Runtime::checkPtrSize(int32)",1);
        }
    }

    QByteArray procTypeSignature(ProcType* pt)
    {
        QByteArray str;
        if( pt->d_return.isNull() )
            str = "void";
        else
            str = formatType(pt->d_return.data(),pt->d_unsafe);
        str += "*";
        str += formatFormals(pt,false);
        return str;
    }

    // special rules
    // - an array in an unsafe struct declaration is the element type
    // - an unsafe array is always a "*" to the element type
    // - a cpointer to an unsafe array is still a "*" to the element type (not "**")
    // - in an unsafe array we have to differ char and wchar as uint8 and uint16
    // - a cstruct is a valuetype, not a class)
    // - in a signature of an unsafe proc a proc type is a native int, not a delegate
    QByteArray formatType( Type* t, bool unsafe = false )
    {
        if( t == 0 )
            return "void";
        else if( forceFormalIndex && t->d_metaActual )
            return "!"+QByteArray::number(t->d_slot);
        switch(t->getTag())
        {
        case Thing::T_Array:
            {
                Array* a = cast<Array*>(t);
                if( a->d_type.isNull() )
                    return QByteArray(); // already reported
                // we only support CLI vectors; multi dim are vectors of vectors
                // arrays are constructed types, i.e. all qualis are resolved up to their original module
                // arrays are always dynamic in CLI; the size of an array is an attribute of the instance
                if( a->d_unsafe )
                {
                    Type* et = derefed(a->d_type.data());
                    if( et && et->isPointer() && !a->d_lenExpr.isNull() )
                        checkPtrSize = true; // pointer size in unsafe array elements is platform dependent
                }
                // in unsafe records the array is implicit by explicit position and size, and the field
                // represents the first element; otherwise unsafe arrays are just a pointer to the base type.
                return formatType(a->d_type.data(), a->d_unsafe) +
                        ( arrayAsElementType ? "" :
                                               ( a->d_unsafe ? "*" : "[]" ) );
            }
            break;
        case Thing::T_BaseType:
            return formatBaseType(t->getBaseType(), unsafe );
        case Thing::T_Enumeration:
            return "int32";
        case Thing::T_Pointer:
            {
                Pointer* me = cast<Pointer*>(t);
                // this is a CLI object reference; since all objects and arrays in CLI are dynamic,
                // a field of type object or array is always a pointer, whereas implicit;
                if( me->d_to )
                {
                    QByteArray type = formatType(me->d_to.data(),me->d_unsafe);
                    Type* td = derefed(me->d_to.data());
                    if( td && td->getBaseType() == Type::CVOID )
                        return type;
                    Array* a = td->getTag() == Thing::T_Array ? cast<Array*>(td) : 0;
                    if( me->d_unsafe && td && a == 0 )
                        type += "*";
                    return type;
                }
            }
            break;
        case Thing::T_ProcType:
            {
                ProcType* pt = cast<ProcType*>(t);
                if( unsafe )
                    return "native int";
                else
                    return "class " + delegateRef(pt);
            }
            break;
        case Thing::T_QualiType:
            {
                QualiType* me = cast<QualiType*>(t);
                Type* td = derefed(me->d_quali->d_type.data());
                if( td == 0 )
                    break; // error already reported
                // all qualis are immediatedly resolved

                if( me->d_selfRef )
                {
                    if( Record* r = td->toRecord() )
                        return formatType(r, unsafe);
                    else
                        return "[mscorlib]System.Object"; // avoid infinite loop
                }
#ifndef _MY_GENERICS_
                else if( td->getBaseType() == Type::ANY )
                {
                    Named* n = me->d_quali->getIdent();
                    Q_ASSERT( n && n->getTag() == Thing::T_GenericName );
                    Q_ASSERT( n->d_slotValid );
                    return "!"+QByteArray::number(n->d_slot);
                }
#endif
                else
                    return formatType(me->d_quali->d_type.data(),unsafe);
            }
            break;
        case Thing::T_Record:
            return ( t->d_unsafe ? "valuetype " : "class " ) + classRef(cast<Record*>(t))
                    + formatMetaActuals(t)
#ifdef _CLI_DYN_STRUCT_VARIABLES_
                    + ( t->d_unsafe && structAsPointer ? "*" : "" )
#endif
                    ;
        default:
            Q_ASSERT(false);
        }
        return "?";
    }

    void visit( ProcType* me )
    {
        Q_ASSERT(false);
    }

    void visit( Record* me)
    {
        Q_ASSERT(false);
    }

    void visit( Enumeration* me)
    {
        Q_ASSERT(false);
    }

    void visit( QualiType* me)
    {
        Q_ASSERT(false);
    }

    void visit( Array* me)
    {
        Q_ASSERT(false);
    }

    void visit( Pointer* me)
    {
        Q_ASSERT(false);
    }

    inline static QByteArray formatBaseType(int t, bool unsafe)
    {
        switch( t )
        {
        case Type::BOOLEAN:
            if( unsafe )
                return "uint8";
            else
                return "bool";
        case Type::CHAR:
            if( unsafe )
                return "uint8";
            else
                return "char";
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
        case Type::CVOID:
            return "native int";
        case Type::STRING:
            return "uint8";
        case Type::WSTRING:
            return "uint16";
        default:
            return "?";
        }
    }

    void visit( BaseType* me)
    {
        Q_ASSERT(false);
    }

    void visit( Variable* me)
    {
#ifdef  _CLI_DYN_STRUCT_VARIABLES_
        // NOTE: we need GCHandle.Alloc() for vars used for pinvoke.
        // Mono 3 (not 5) produces random crashes with struct module variables passed to SDL.WaitEventTimeout otherwise
        Q_ASSERT(!structAsPointer);
        Type* td = derefed(me->d_type.data());
        if( td && td->getTag() == Thing::T_Record && td->d_unsafe )
            structAsPointer = true;
#endif
        emitter->addField(escape(me->d_name),formatType(me->d_type.data()), // there are no unsafe variables
                         me->d_visibility == Named::ReadWrite || me->d_visibility == Named::ReadOnly, true );
        // initializer is emitted in begïn
        structAsPointer = false;
    }

    void visit( Field* me )
    {
        Q_ASSERT(!arrayAsElementType);
        arrayAsElementType = me->d_unsafe;
        Type* td = derefed(me->d_type.data());
        if( me->d_unsafe && td && td->isPointer() )
            checkPtrSize = true;
        emitter->addField(escape(me->d_name),formatType(me->d_type.data(), me->d_unsafe),
                         me->d_visibility == Named::ReadWrite || me->d_visibility == Named::ReadOnly,
                          false, me->d_unsafe ? me->d_slot : -1 );
        if( me->d_unsafe && td->getTag() == Thing::T_Array && debug )
        {
            // this is a work-around for the debugger (otherwise only the first element is shown)
            Array* a = cast<Array*>(td);
            const int size = a->d_type->getByteSize();
            for( int i = 1; i < a->d_len; i++ )
                emitter->addField(escape(me->d_name+"#"+QByteArray::number(i)),formatType(me->d_type.data(), me->d_unsafe),
                                  true,false,me->d_slot+i*size);
#if 0
            // padding is not required
            Field* next = me->d_owner->nextField(me);
            const int nextOff = next ? next->d_slot : me->d_owner->d_byteSize;
            const int padding = ( ( nextOff - me->d_slot ) - a->d_len * size ) / size;
            for( int i = 0; i < padding; i++ )
                emitter->addField(escape("("+me->d_name+"#"+QByteArray::number(i+a->d_len)+")"),formatType(me->d_type.data()),
                                  true,false,me->d_slot+i*size);
#endif
        }
        arrayAsElementType = false;
    }

    QByteArray formatFormals( ProcType* pt, bool withName = true, const QList<Type*>& varargs = QList<Type*>(), bool withSentinel = true )
    {
        QByteArray res = "(";
        for( int i = 0; i < pt->d_formals.size(); i++ )
        {
            if( i != 0 )
                res += ", ";
            res += formatType( pt->d_formals[i]->d_type.data(), pt->d_unsafe );
            if( passByRef(pt->d_formals[i].data()) )
                res += "&";
            if( withName )
                res += " " + escape(pt->d_formals[i]->d_name);
        }
        if( !varargs.isEmpty() )
        {
            if( !pt->d_formals.isEmpty() )
                res += ", ";
            if( withSentinel )
                res += "..., ";
            for( int i = 0; i < varargs.size(); i++ )
            {
                if( i != 0 )
                    res += ", ";
                res += formatType( varargs[i], pt->d_unsafe );
            }
        }
        res += ")";
        return res;
    }

    void emitLocalVars()
    {
        for( int i = 0; i < temps.d_types.size(); i++ )
            emitter->addLocal( temps.d_types[i], escape("#temp" + QByteArray::number(i) ) );
    }

    void emitProcedure( Procedure* me, const QByteArray& prefix = QByteArray(),
                        const QList<Type*>& extraArgs = QList<Type*>() )
    {
        QByteArray name;
        if( !prefix.isEmpty() )
            name = escape( prefix + me->d_name ); // for local vararg substitute proc
        else if( me->d_receiverRec )
            name = escape(me->d_name); // the method is directly in the receiver class
        else
            name = nestedPath(me); // the method is lifted to module level

        Module* mod = me->getModule();
        Q_ASSERT(mod);
        IlEmitter::MethodKind k;
        if( mod->d_externC )
            k = IlEmitter::Pinvoke;
        else if( me->d_receiver.isNull() )
            k = IlEmitter::Static;
        else if( me->d_receiverRec && !me->d_receiverRec->d_byValue )
            k = IlEmitter::Virtual;
        else
            k = IlEmitter::Instance;

        emitter->beginMethod(name,me->d_visibility != Named::Private,k);

        bool isVararg = false;
        if( mod->d_externC )
        {
            QByteArray dll, prefix, alias;
            SysAttr* a = mod->d_sysAttrs.value("dll").data();
            if( a && a->d_values.size() == 1 )
                dll = a->d_values.first().toByteArray() + ".dll";
            a = mod->d_sysAttrs.value("prefix").data(); // module attr
            if( a && a->d_values.size() == 1 )
                prefix = a->d_values.first().toByteArray();
            a = me->d_sysAttrs.value("dll").data(); // proc attr
            if( a && a->d_values.size() == 1 )
                dll = a->d_values.first().toByteArray();
            a = me->d_sysAttrs.value("prefix").data(); // proc attr
            if( a && a->d_values.size() == 1 )
                prefix = a->d_values.first().toByteArray();
            a = me->d_sysAttrs.value("alias").data(); // proc attr
            if( a && a->d_values.size() == 1 )
                alias = a->d_values.first().toByteArray();
            isVararg = !me->d_sysAttrs.value("varargs").isNull(); // proc attr
            QByteArray useName;
            if( !extraArgs.isEmpty() )
                useName = me->d_name;
            if( !alias.isEmpty() )
                useName = alias;
            else if( !prefix.isEmpty() )
                useName = prefix + me->d_name;
            emitter->setPinvoke(dll,useName);
            //qDebug() << "*EXPORT*" << ( useName.isEmpty() ? me->d_name : useName );
        }

        if( isVararg && extraArgs.isEmpty() ) // extraArgs is to substitute vararg, so we dont need the flag in this case
            emitter->setVararg();

        ProcType* pt = me->getProcType();
        if( !pt->d_return.isNull() )
            emitter->setReturnType(formatType(pt->d_return.data(),pt->d_unsafe));

        // allocate params and local
        int off = me->d_receiver.isNull() ? 0 : 1;
        for( int i = 0; i < pt->d_formals.size(); i++ )
        {
            pt->d_formals[i]->d_slot = i+off; // for type-bounds arg0 is self
            pt->d_formals[i]->d_slotValid = true;

            QByteArray type = formatType(pt->d_formals[i]->d_type.data(), pt->d_unsafe);
            if( passByRef(pt->d_formals[i].data()) )
                type += "&";
            emitter->addArgument(type,escape(pt->d_formals[i]->d_name));
        }
        if( !extraArgs.isEmpty() )
        {
            for( int i = 0; i < extraArgs.size(); i++ )
            {
                QByteArray type = formatType(extraArgs[i], pt->d_unsafe);
                emitter->addArgument(type,QString("'extra%1'").arg(i).toUtf8());
            }
        }
        off = 0;
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_LocalVar )
            {
                n->d_slot = off++;
                n->d_slotValid = true;
                emitter->addLocal( formatType(n->d_type.data()), escape(n->d_name) ); // unsafe procs have no local vars
            }
        }

        beginBody(me->d_varCount);

        foreach( const Ref<Named>& n, me->d_order )
        {
            switch( n->getTag() )
            {
            case Thing::T_LocalVar:
            case Thing::T_Parameter:
                suppressLine++;
                emitInitializer(n.data());
                suppressLine--;
                break;
            }
        }
        foreach( const Ref<Statement>& s, me->d_body )
        {
            temps.sellAll(); // no temp var kept from one statement to next
            s->accept(this);
        }
        if( me->d_body.isEmpty() || me->d_body.last()->getTag() != Thing::T_Return )
        {
            temps.sellAll();
            emitReturn( pt, 0, me->d_end );
        }

        emitLocalVars();

        emitter->endMethod();
    }

    void visit( Procedure* me )
    {
        Q_ASSERT( scope == 0 );
        scope = me;

        emitProcedure(me);

        scope = 0;
    }

    void beginBody(quint16 start = 0)
    {
        last = RowCol();
        temps.reset(start);
    }

    void visit( LocalVar* me )
    {
        Q_ASSERT(false);
    }

    void emitConst(quint8 basetype, const QVariant& val, const RowCol& loc )
    {
        switch( basetype )
        {
        case Type::BOOLEAN:
            if( val.toBool() )
                line(loc).ldc_i4(1);
            else
                line(loc).ldc_i4(0);
            break;
        case Type::SHORTINT:
            line(loc).ldc_i4(val.toInt());
            break;
        case Type::INTEGER:
            line(loc).ldc_i4(val.toInt());
            break;
        case Type::LONGINT:
            line(loc).ldc_i8(val.toInt());
            break;
        case Type::BYTE:
            line(loc).ldc_i4(val.toInt());
            break;
        case Type::ENUMINT:
            line(loc).ldc_i4(val.toInt());
            break;
        case Type::REAL:
            //emitOpcode2("ldc.r8",1,loc); // NOTE: before r4, but this causes round-off errors when e.g. 365.24 is later converted to r8
                                         // CLR anyway has F on the stack, even when pushing r4
            //out << " " << QByteArray::number(val.toDouble(),'e',9) << endl;
            line(loc).ldc_r8(val.toDouble());
            break;
        case Type::LONGREAL:
            //emitOpcode2("ldc.r8",1,loc);
            //out << " " << QByteArray::number(val.toDouble(),'e',17) << endl;
            line(loc).ldc_r8(val.toDouble());
            break;
        case Type::NIL:
            line(loc).ldnull_();
            break;
        case Type::STRING:
        case Type::WSTRING:
            {
                QByteArray str = val.toByteArray();
                str.replace('\\', "\\\\");
                str.replace("\"","\\\"");
                line(loc).ldstr_("\"" + str + "\\0" + "\""); // without explicit \0 the resulting char[] has no trailing zero!
                line(loc).callvirt_("char[] [mscorlib]System.String::ToCharArray()",0,true);
            }
            break;
        case Type::BYTEARRAY:
            {
                const QByteArray ba = val.toByteArray();
                line(loc).ldc_i4(ba.size());
                line(loc).newarr_("uint8");

                for( int i = 0; i < ba.size(); i++ ) // TODO: this is inefficient
                {
                    line(loc).dup_();
                    line(loc).ldc_i4(i);
                    line(loc).ldc_i4((quint8)ba[i]);
                    line(loc).stelem_("uint8");
                }
            }
            break;
        case Type::CHAR:
        case Type::WCHAR:
            line(loc).ldc_i4(val.toUInt());
            break;
        case Type::SET:
            {
                Literal::SET s = val.value<Literal::SET>();
                line(loc).ldc_i4(s.to_ulong());
            }
            break;
        default:
            Q_ASSERT(false);
        }
    }

    void visit( Literal* me)
    {
        Type* td = derefed(me->d_type.data());
        Q_ASSERT( td && ( td->getTag() == Thing::T_BaseType || td->getTag() == Thing::T_Enumeration) );
        // Enumeration has basetype ENUMINT
        emitConst( td->getBaseType(), me->d_val, me->d_loc );
    }

    void visit( UnExpr* me)
    {
        Q_ASSERT( !me->d_sub.isNull() );

        me->d_sub->accept(this);

        // prev must be a pointer or a record
        Type* prevT = derefed(me->d_sub->d_type.data());
        Q_ASSERT( prevT );

        switch( me->d_op )
        {
        case UnExpr::NEG:
            if( prevT->getBaseType() == BaseType::SET )
            {
                line(me->d_loc).not_();
            }else
            {
                Q_ASSERT( prevT->isNumeric() );
                line(me->d_loc).neg_();
            }
            return;
        case UnExpr::NOT:
            line(me->d_loc).ldc_i4(0);
            line(me->d_loc).ceq_();
            return;
        case UnExpr::DEREF:
            // NOP: both pointer deref as well as super proc calls are handled by referencing UnExpr
            return;
        case UnExpr::ADDROF:
            // NOP
            return;
        default:
            qDebug() << "ERR" << me->d_op << thisMod->d_name << me->d_loc.d_row << me->d_loc.d_col;
            Q_ASSERT( false );
            break;
        }
        Q_ASSERT( false );
    }

    void emitValueFromAdrToStack(Type* t, bool unsafe, const RowCol& loc )
    {
#ifdef _USE_LDSTOBJ
        line(me->d_loc).ldobj_(formatType(t));
#else
        // NOTE: _USE_LDSTOBJ on or off has practically no influence on performance, geomean 0.40 vs 0.39
        Type* td = derefed(t);
        switch( td->getTag() )
        {
        case Thing::T_Array:
        default:
            Q_ASSERT(false);
            break;
        case Thing::T_Record:
            // No, dont do that: line(loc).ldobj_(formatType(t));
            // we arrive here e.g. indexing an array; the "field value" in case of a record field is
            // usually the address of the record; so when accessing a cstruct we don't copy the struct
            // here to the stack, but just leave the address of the field, which is on the stack
            // when we arrive here, where it is
            break;
        case Thing::T_Pointer:
        case Thing::T_ProcType:
            if( unsafe )
                line(loc).ldind_(IlEmitter::IntPtr);
            else
                line(loc).ldind_(IlEmitter::Ref);
            break;
        case Thing::T_Enumeration:
            line(loc).ldind_(IlEmitter::I4);
            break;
        case Thing::T_BaseType:
            switch(td->getBaseType())
            {
            case Type::LONGREAL:
                line(loc).ldind_(IlEmitter::R8);
                break;
            case Type::REAL:
                line(loc).ldind_(IlEmitter::R4);
                break;
            case Type::LONGINT:
                line(loc).ldind_(IlEmitter::I8);
                break;
            case Type::INTEGER:
                line(loc).ldind_(IlEmitter::I4);
                break;
            case Type::SET:
                line(loc).ldind_(IlEmitter::U4);
                break;
            case Type::SHORTINT:
                line(loc).ldind_(IlEmitter::I2);
                break;
            case Type::CHAR:
                if( unsafe )
                    line(loc).ldind_(IlEmitter::U1);
                else
                    line(loc).ldind_(IlEmitter::U2);
                break;
            case Type::WCHAR:
                line(loc).ldind_(IlEmitter::U2);
                break;
            case Type::BYTE:
            case Type::BOOLEAN: // bool is 1 byte in RAM but 4 bytes on stack
                line(loc).ldind_(IlEmitter::U1);
                break;
            }
            break;
        }
#endif
    }

    void visit( IdentLeaf* me)
    {
        Named* id = me->getIdent();
        if( id == 0 )
            return; // already reported

        switch( id->getTag() )
        {
        case Thing::T_Const:
            {
                Type* td = derefed(me->d_type.data() );
                Q_ASSERT( td && ( td->getTag() == Thing::T_BaseType || td->getTag() == Thing::T_Enumeration ) );
                emitConst( td->getBaseType(), cast<Const*>(id)->d_val, me->d_loc );
            }
            return;
        case Thing::T_Import:
            // NOP
            return;
        case Thing::T_Variable:
        case Thing::T_LocalVar:
            emitVarToStack(id,me->d_loc);
            return;
        case Thing::T_Parameter:
            {
                Parameter* p = cast<Parameter*>(id);
                emitVarToStack(id,me->d_loc);
                if( passByRef(p) ) // the value on the stack is a &, so we need to fetch the value first
                    emitValueFromAdrToStack(p->d_type.data(),false,me->d_loc);
            }
            return;
        case Thing::T_NamedType:
            // NOP
            return;
        case Thing::T_BuiltIn:
        case Thing::T_Procedure:
            // NOP
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
            return;
        case Thing::T_Field:
            Q_ASSERT( me->d_sub && me->d_sub->d_type->toRecord() );
            emitVarToStack(id, me->d_loc);
            return;
        case Thing::T_Variable:
            Q_ASSERT( derefImport );
            emitVarToStack(id, me->d_loc);
            return;
        case Thing::T_NamedType:
            // NOP
            return;
        case Thing::T_Const:
            {
                Q_ASSERT( derefImport );
                Type* td = derefed(id->d_type.data() );
                Q_ASSERT( td && ( td->getTag() == Thing::T_BaseType || td->getTag() == Thing::T_Enumeration ) );
                emitConst( td->getBaseType(), cast<Const*>(id)->d_val, me->d_loc );
            }
            return;
        case Thing::T_BuiltIn:
            // NOP
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
        Q_ASSERT( me->d_sub );
        me->d_sub->accept(this);
        Type* subT = derefed(me->d_sub->d_type.data());
        Q_ASSERT( subT && subT->getTag() == Thing::T_Array);

        Q_ASSERT( me->d_args.size() == 1 );
        me->d_args.first()->accept(this);

        Type* et = derefed(cast<Array*>(subT)->d_type.data());
        if( et == 0 )
            return; // already reported
        if( subT->d_unsafe )
        {
            const int len = et->getByteSize();
            if( len > 1 )
            {
                line(me->d_loc).ldc_i4(len);
                line(me->d_loc).mul_();
            }
            line(me->d_loc).add_();
            emitValueFromAdrToStack(et,true,me->d_loc);
        }else
            line(me->d_loc).ldelem_(formatType(et));
    }

    bool emitFetchDesigAddr(Expression* desig, bool omitParams = true )
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
            Type* td = derefed(id->d_type.data());
            switch( id->getTag() )
            {
            case Thing::T_Variable:
#ifdef _CLI_DYN_STRUCT_VARIABLES_
                if( td && td->d_unsafe && td->getTag() == Thing::T_Record )
                {
                    Q_ASSERT(!inUnsafeRecord);
                    inUnsafeRecord = true; // trick to get the "*" behind the type
                    line(desig->d_loc).ldsfld_(memberRef(id)); // it's already a pointer
                    inUnsafeRecord = false;
                }else
#endif
                line(desig->d_loc).ldsflda_(memberRef(id));
                break;
            case Thing::T_Field:
#ifdef _CLI_USE_PTR_TO_MEMBER_
                if( id->d_unsafe )
                {
                    // explicit pointer arithmentic instead of memberref access
                    Q_ASSERT(id->d_slotValid);
                    line(desig->d_loc).ldc_i4(id->d_slot);
                    line(desig->d_loc).add_();
                }else
#endif
                    line(desig->d_loc).ldflda_(memberRef(id));
                break;
            default:
                Q_ASSERT( false );
           }
        }else if( unop == UnExpr::IDX )
        {
            Q_ASSERT( desig->getTag() == Thing::T_ArgExpr );
            ArgExpr* args = cast<ArgExpr*>( desig );
            Q_ASSERT( args->d_args.size() == 1 );
            args->d_sub->accept(this);
            Type* array = derefed(args->d_sub->d_type.data());
            if( array && array->d_unsafe )
            {
                // stack: pointer to array
                args->d_args.first()->accept(this);
                // stack: pointer to array, index
                const int len = desig->d_type->getByteSize();
                if( len > 1 )
                {
                    line(desig->d_loc).ldc_i4(len);
                    line(desig->d_loc).mul_();
                }
                line(desig->d_loc).add_();
                return true;
            }else
            {
                // stack: array
                args->d_args.first()->accept(this); // stack: array, index
                line(desig->d_loc).ldelema_(formatType(desig->d_type.data()));
            }
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
            Type* td = derefed(n->d_type.data());
            switch( n->getTag() )
            {
            case Thing::T_Variable:
#ifdef _CLI_DYN_STRUCT_VARIABLES_
                if( td && td->d_unsafe && td->getTag() == Thing::T_Record )
                {
                    Q_ASSERT(!inUnsafeRecord);
                    inUnsafeRecord = true;
                    line(desig->d_loc).ldsfld_(memberRef(n)); // it's already a pointer
                    inUnsafeRecord = false;
                }else
#endif
                line(desig->d_loc).ldsflda_(memberRef(n));
                break;
            case Thing::T_Parameter:
                Q_ASSERT( n->d_slotValid );
                if( omitParams && passByRef(cast<Parameter*>(n)) )
                    line(desig->d_loc).ldarg_(n->d_slot); // we already have the address of the value
                else
                    line(desig->d_loc).ldarga_(n->d_slot);
                break;
            case Thing::T_LocalVar:
                Q_ASSERT( n->d_slotValid );
                line(desig->d_loc).ldloca_(n->d_slot);
                // NOTE: works only for local access
                break;
            }
        }else if( tag == Thing::T_Literal )
        {
            Q_ASSERT( cast<Literal*>(desig)->d_vtype == Literal::Nil );
            // this happens in BB when calling the Win32 API
            line(desig->d_loc).ldnull_();
        }else if( tag == Thing::T_ArgExpr )
        {
            ArgExpr* ae = cast<ArgExpr*>(desig);
            Q_ASSERT( ae->d_sub && ae->d_sub->getIdent() && ae->d_sub->getIdent()->getTag() == Thing::T_BuiltIn &&
                     ( cast<BuiltIn*>(ae->d_sub->getIdent())->d_func == BuiltIn::SYS_VAL ||
                       cast<BuiltIn*>(ae->d_sub->getIdent())->d_func == BuiltIn::CAST ) );
            Q_ASSERT( ae->d_args.size() == 2 );
            emitFetchDesigAddr(ae->d_args.last().data(), omitParams);
        }else
        {
            qDebug() << "ERR" << desig->getUnOp() << desig->getTag() << thisMod->getName() << desig->d_loc.d_row << desig->d_loc.d_col;
            Q_ASSERT( false );
        }
        return false;
    }

    void assureInteger(Type* t, const RowCol& loc )
    {
        t = derefed(t);
        if( t == 0 )
            return;
        switch( t->getBaseType() )
        {
        case Type::LONGREAL:
            convertTo(Type::LONGINT,t,loc);
            break;
        case Type::REAL:
            convertTo(Type::INTEGER,t,loc);
            break;
        }
    }

    void emitBuiltIn( BuiltIn* bi, ArgExpr* ae )
    {
        switch( bi->d_func )
        {
        case BuiltIn::LDMOD:
            {
                Q_ASSERT( ae->d_args.size() == 1 );
                ae->d_args.first()->accept(this);
                line(ae->d_loc).call_("bool [OBX.Runtime]OBX.Runtime::loadModule(char[])",1,true);
            }
            break;
        case BuiltIn::LDCMD:
            {
                Q_ASSERT( ae->d_args.size() == 2 );
                ae->d_args.first()->accept(this);
                ae->d_args.last()->accept(this);
                line(ae->d_loc).call_("class [OBX.Runtime]OBX.Command [OBX.Runtime]OBX.Runtime::getCommand(char[],char[])",2,true);
            }
            break;
        case BuiltIn::PRINTLN:
            {
                Q_ASSERT( ae->d_args.size() == 1 );
                ae->d_args.first()->accept(this);
                Type* td = derefed(ae->d_args.first()->d_type.data());
                bool wide = false;
                if( td->isText(&wide) )
                {
                    if( td->isChar() )
                        line(ae->d_loc).call_("void [mscorlib]System.Console::WriteLine(char)",1);
                    else if( td->d_unsafe )
                    {
                        if( wide )
                            line(ae->d_loc).call_("string [mscorlib]System.Runtime.InteropServices.Marshal::PtrToStringUni(native int)",1,true);
                        else
                            line(ae->d_loc).call_("string [mscorlib]System.Runtime.InteropServices.Marshal::PtrToStringAnsi(native int)",1,true);
                        line(ae->d_loc).call_("void [mscorlib]System.Console::WriteLine(string)",1);
                    }else
                    {
                        line(ae->d_loc).call_("string [OBX.Runtime]OBX.Runtime::toString(char[])", 1, true );
                        line(ae->d_loc).call_("void [mscorlib]System.Console::WriteLine(string)",1); // WriteLine(char[]) doesn't seem to respect 0
                    }
                }else if( td->isInteger() )
                {
                    if( td->getBaseType() <= Type::INTEGER )
                        line(ae->d_loc).call_("void [mscorlib]System.Console::WriteLine(int32)",1);
                    else
                        line(ae->d_loc).call_("void [mscorlib]System.Console::WriteLine(int64)",1);
                }else if( td->isReal() )
                    line(ae->d_loc).call_("void [mscorlib]System.Console::WriteLine(float64)",1);
                else if( td->isSet() )
                    line(ae->d_loc).call_("void [mscorlib]System.Console::WriteLine(uint32)",1);
                else if( td->getBaseType() == Type::BOOLEAN )
                    line(ae->d_loc).call_("void [mscorlib]System.Console::WriteLine(bool)",1);
                else if( td->isStructured(true) )
                {
                    if( td->d_unsafe )
                    {
                        checkPtrSize = true;
                        if( Pointer::s_pointerByteSize == 8 )
                        {
                            line(ae->d_loc).conv_(IlEmitter::ToU8);
                            line(ae->d_loc).call_("string [OBX.Runtime]OBX.Runtime::toHex(uint64)",1,true);
                        }else
                        {
                            line(ae->d_loc).conv_(IlEmitter::ToU4);
                            line(ae->d_loc).call_("string [OBX.Runtime]OBX.Runtime::toHex(uint32)",1,true);
                        }
                        line(ae->d_loc).call_("void [mscorlib]System.Console::WriteLine(string)",1);
                    }else
                        line(ae->d_loc).call_("void [mscorlib]System.Console::WriteLine(object)",1);
                }else
                {
                    switch(td->getTag())
                    {
                    case Thing::T_Enumeration:
                        line(ae->d_loc).call_("void [mscorlib]System.Console::WriteLine(int32)",1);
                        break;
                    default:
                        line(ae->d_loc).call_("void [mscorlib]System.Console::WriteLine(object)",1);
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
            // doesn't work:
            //emitOpcode("ldstr \"trap hit\"",1,ae->d_loc); // TEST
            //emitOpcode("call void [mscorlib]System.Console::WriteLine(string)",-1,ae->d_loc);
            // doesn't work either: line(ae->d_loc).break_(); // in this case when in debug agent mono goes to 150% cpu and no longer reacts

            line(ae->d_loc).call_("void [mscorlib]System.Diagnostics.Debugger::Break()");
            break;
        case BuiltIn::TRAPIF:
            {
                Q_ASSERT( ae->d_args.size() == 1 );
                ae->d_args.first()->accept(this);
                const int atEnd = emitter->newLabel();
                line(ae->d_loc).brfalse_(atEnd);
                // doesn't work: line(ae->d_loc).break_();
                line(ae->d_loc).call_("void [mscorlib]System.Diagnostics.Debugger::Break()");
                line(ae->d_loc).label_(atEnd);
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
                                line(ae->d_loc).ldc_i8(bt->maxVal().toLongLong());
                            else
                                line(ae->d_loc).ldc_i8(bt->minVal().toLongLong());
                            break;
                        case Type::LONGREAL:
                            if( bi->d_func == BuiltIn::MAX )
                                line(ae->d_loc).ldc_r8(bt->maxVal().toDouble());
                            else
                                line(ae->d_loc).ldc_r8(bt->minVal().toDouble());
                            break;
                        case Type::REAL:
                            if( bi->d_func == BuiltIn::MAX ) // NOTE: used r4 before, but see above
                                line(ae->d_loc).ldc_r8(bt->maxVal().toDouble());
                            else
                                line(ae->d_loc).ldc_r8(bt->minVal().toDouble());
                            break;
                        case Type::BOOLEAN:
                        case Type::CHAR:
                        case Type::WCHAR:
                        case Type::BYTE:
                        case Type::SHORTINT:
                        case Type::INTEGER:
                        case Type::SET:
                            if( bi->d_func == BuiltIn::MAX )
                                line(ae->d_loc).ldc_i4(bt->maxVal().toInt());
                            else
                                line(ae->d_loc).ldc_i4(bt->minVal().toInt());
                            break;
                        }
                    }
                    break;
                case Thing::T_Enumeration:
                    {
                        Enumeration* e = cast<Enumeration*>(t);
                        if( bi->d_func == BuiltIn::MAX )
                            line(ae->d_loc).ldc_i4(e->d_items.last()->d_val.toInt());
                        else
                            line(ae->d_loc).ldc_i4(e->d_items.first()->d_val.toInt());
                    }
                    break;
                default:
                    Q_ASSERT( false );
                }
            }else if( ae->d_args.size() == 2 )
            {
                ae->d_args.first()->accept(this);
                ae->d_args.last()->accept(this);
                const int posCase = emitter->newLabel();
                if( bi->d_func == BuiltIn::MAX )
                    line(ae->d_loc).bge_(posCase);
                else
                    line(ae->d_loc).ble_(posCase); // if
                ae->d_args.last()->accept(this); // then // TODO: use temps to avoid multiple evaluation

                const int toEnd = emitter->newLabel();
                line(ae->d_loc).br_(toEnd);
                line(ae->d_loc).label_(posCase);
                ae->d_args.first()->accept(this); // else
                line(ae->d_loc).label_(toEnd);
                // TODO stackDepth--; // correct for alternative
            }else
                Q_ASSERT( false );
            break;
        case BuiltIn::DEFAULT:
            {
                Q_ASSERT( !ae->d_args.isEmpty() && !ae->d_args.first()->d_type.isNull() );
                Expression* e = ae->d_args.first().data();
                if( !emitInitializer(e->d_type.data(),false,e->d_loc) )
                    line(ae->d_loc).ldnull_();
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
                    line(ae->d_loc).call_("int32 [OBX.Runtime]OBX.Runtime::strlen(char[])",1,true); // TODO: likely wrong!
                }else
                {
                    Q_ASSERT( t->getTag() == Thing::T_Array );
                    Array* a = cast<Array*>(t);
                    Type* at = derefed( a->d_type.data() );
                    Q_ASSERT( at );
                    if( a->d_len > 0 )
                    {
                        line(ae->d_loc).ldc_i4(a->d_len);
                    }else
                    {
                        ae->d_args.first()->accept(this);
                        line(ae->d_loc).ldlen_();
                    }
                }
            }
            break;
        case BuiltIn::STRLEN:
            {
                Q_ASSERT( !ae->d_args.isEmpty() );
                Type* t = derefed(ae->d_args.first()->d_type.data() );
                bool wide;
                if( t->isText(&wide) )
                {
                    ae->d_args.first()->accept(this);
                    line(ae->d_loc).call_("int32 [OBX.Runtime]OBX.Runtime::strlen(char[])",1,true);
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
                    const int len = temps.buy("int32");
                    lengths.append(len);
                    line(ae->d_loc).stloc_(len);
                }
                // TODO: Pelib issue when 2d array local variable; apparently
                // newarr class 'T3VariableDeclarations'/'Vector'[] is written
                // as newarr class 'T3VariableDeclarations'/'Vector' by Pelib!
                // see T3VariableDeclarations. Same if module variable instead of local,
                // same if RectangleDesc instead of Vector; but it works if CHAR or INTEGER,
                // or if compiled with ILASM instead of Pelib.

                emitFetchDesigAddr(ae->d_args.first().data(),true); // not false, because also here a var param has the address already
                // stack: address to store to

                // we must pass t here (not ptr->d_to) because the pointer could be a named type defined in another module;
                // if we deref the pointer we lose the module information
                //suppressLine++;
                emitInitializer(t, true, ae->d_loc, lengths );
                //suppressLine--;

                line(ae->d_loc).stind_(IlEmitter::Ref);
            }
            break;
        case BuiltIn::INCL:
            {
                Q_ASSERT( ae->d_args.size() == 2 );

                emitFetchDesigAddr(ae->d_args.first().data(),true);
                // stack: addr of store
                line(ae->d_loc).dup_();
                line(ae->d_loc).ldind_(IlEmitter::U4);
                ae->d_args.last()->accept(this);
                line(ae->d_loc).call_("int32 [OBX.Runtime]OBX.Runtime::addElemToSet(int32,int32)",2,true );
                line(ae->d_loc).stind_(IlEmitter::I4);
            }
            break;
        case BuiltIn::EXCL:
            {
                Q_ASSERT( ae->d_args.size() == 2 );

                emitFetchDesigAddr(ae->d_args.first().data(),true);
                // stack: addr of store
                line(ae->d_loc).dup_();
                line(ae->d_loc).ldind_(IlEmitter::U4);
                ae->d_args.last()->accept(this);
                line(ae->d_loc).call_("int32 [OBX.Runtime]OBX.Runtime::removeElemFromSet(int32,int32)", 2, true );
                line(ae->d_loc).stind_(IlEmitter::I4);
            }
            break;
        case BuiltIn::PACK:
            {
                Q_ASSERT( ae->d_args.size() == 2 );

                emitFetchDesigAddr(ae->d_args.first().data(),true);
                ae->d_args.last()->accept(this);
                line(ae->d_loc).call_("void [OBX.Runtime]OBX.Runtime::PACK(float32&, int32)", 2 );
           }
            break;
        case BuiltIn::UNPK:
            {
                Q_ASSERT( ae->d_args.size() == 2 );

                emitFetchDesigAddr(ae->d_args.first().data(),true);
                emitFetchDesigAddr(ae->d_args.last().data(),true);
                // stack: addr, addr
                line(ae->d_loc).call_("void [OBX.Runtime]OBX.Runtime::UNPACK(float32&, int32&)", 2 );
             }
            break;
        case BuiltIn::ORD:
            {
                Q_ASSERT( ae->d_args.size() == 1 );
                ae->d_args.first()->accept(this);
                Type* t = derefed(ae->d_args.first()->d_type.data() );
                bool wide;
                if( t && ( t->isText(&wide) && !t->isChar() ) )
                {
                    if( t->d_unsafe && wide )
                        line(ae->d_loc).ldind_(IlEmitter::U2);
                    else if( t->d_unsafe && !wide )
                        line(ae->d_loc).ldind_(IlEmitter::U1);
                    else
                    {
                        line(ae->d_loc).ldc_i4(0);
                        line(ae->d_loc).ldelem_("char");
                    }
                }
                else if( t && ( t->getBaseType() == Type::REAL || t->getBaseType() == Type::LONGREAL ) )
                    assureInteger(t,ae->d_loc);
                else if( t && t->getTag() == Thing::T_Pointer )
                {
                    line(ae->d_loc).pop_();
                    line(ae->d_loc).ldc_i4(0);
                }
            }
            break;
        case BuiltIn::CHR:
            Q_ASSERT( ae->d_args.size() == 1 );
            ae->d_args.first()->accept(this);
            break;
        case BuiltIn::FLT:
            {
                Q_ASSERT( ae->d_args.size() == 1 );
                ae->d_args.first()->accept(this);
                const int bt = ae->d_args.first()->d_type.isNull() ? 0 : ae->d_args.first()->d_type->getBaseType();
                if( bt != Type::REAL && bt != Type::LONGREAL )
                {
                    // always double on stack; Mono 5 and 6 sometimes result in NaN if conv.r4
                    //if( bt == Type::LONGINT )
                        line(ae->d_loc).conv_(IlEmitter::ToR8);
                    //else
                    //    line(ae->d_loc).conv_(IlEmitter::ToR4);
                }
            }
            break;
        case BuiltIn::ODD:
            Q_ASSERT( ae->d_args.size() == 1 );
            ae->d_args.first()->accept(this);
            line(ae->d_loc).call_("bool [OBX.Runtime]OBX.Runtime::ODD(int32)", 1, true );
            break;
        case BuiltIn::ABS:
            {
                Q_ASSERT( ae->d_args.size() == 1 );
                ae->d_args.first()->accept(this);
                Type* t = derefed(ae->d_args.first()->d_type.data());
                Q_ASSERT( t );
                switch(t->getBaseType())
                {
                case Type::LONGREAL:
                    line(ae->d_loc).call_("float64 [mscorlib]System.Math::Abs(float64)", 1, true );
                    break;
                case Type::REAL:
                    line(ae->d_loc).call_("float32 [mscorlib]System.Math::Abs(float32)", 1, true );
                    break;
                case Type::LONGINT:
                    line(ae->d_loc).call_("int64 [mscorlib]System.Math::Abs(int64)", 1, true );
                    break;
                case Type::INTEGER:
                    line(ae->d_loc).call_("int32 [mscorlib]System.Math::Abs(int32)", 1, true );
                    break;
                case Type::SHORTINT:
                case Type::BYTE:
                    line(ae->d_loc).call_("int16 [mscorlib]System.Math::Abs(int16)", 1, true );
                    break;
                default:
                    Q_ASSERT(false);
                }
            }
            break;
        case BuiltIn::FLOOR:
            Q_ASSERT( ae->d_args.size() == 1 );
            ae->d_args.first()->accept(this);
            convertTo(Type::LONGREAL,ae->d_args.first()->d_type.data(), ae->d_args.first()->d_loc);
            line(ae->d_loc).call_("float64 [mscorlib]System.Math::Floor(float64)", 1, true );
            if( ae->d_args.first()->d_type->derefed()->getBaseType() == Type::LONGREAL )
                line(ae->d_loc).conv_(IlEmitter::ToI8);
            else
                line(ae->d_loc).conv_(IlEmitter::ToI4);
            break;
        case BuiltIn::ENTIER: // obsolete
            Q_ASSERT( ae->d_args.size() == 1 );
            ae->d_args.first()->accept(this);
            line(ae->d_loc).call_("float64 [mscorlib]System.Math::Floor(float64)", 1, true );
            line(ae->d_loc).conv_(IlEmitter::ToI8);
            break;
#if 0
        case BuiltIn::LSL:
            Q_ASSERT( ae->d_args.size() == 2 );
            ae->d_args.first()->accept(this);
            assureInteger(ae->d_args.first()->d_type.data(),ae->d_loc);
            ae->d_args.last()->accept(this);
            convertTo( Type::INTEGER,ae->d_args.last()->d_type.data(), ae->d_args.last()->d_loc );
#if 1
            line(ae->d_loc).shl_();
#else
            // TODO
            line(ae->d_loc).neg_(); // make rhs negative
            line(ae->d_loc).ldc_i4(0);
            if( ae->d_args.first()->d_type->derefed()->getBaseType() == Type::LONGINT )
                line(ae->d_loc).call_("int64 [OBX.Runtime]OBX.Runtime::Shr64(int64,int32,bool)", 2, true );
            else
                line(ae->d_loc).call_("int32 [OBX.Runtime]OBX.Runtime::Shr32(int32,int32,bool)", 2, true );
#endif
            break;
#endif
        case BuiltIn::ASH:
            Q_ASSERT( ae->d_args.size() == 2 );
            ae->d_args.first()->accept(this);
            assureInteger(ae->d_args.first()->d_type.data(),ae->d_loc);
            ae->d_args.last()->accept(this);
            convertTo( Type::INTEGER,ae->d_args.last()->d_type.data(), ae->d_args.last()->d_loc );
            line(ae->d_loc).ldc_i4(1);
            if( ae->d_args.first()->d_type->derefed()->getBaseType() == Type::LONGINT )
                line(ae->d_loc).call_("int64 [OBX.Runtime]OBX.Runtime::Ash64(int64,int32,bool)", 2, true );
            else
                line(ae->d_loc).call_("int32 [OBX.Runtime]OBX.Runtime::Ash32(int32,int32,bool)", 2, true );
            break;
        case BuiltIn::ASR:
        case BuiltIn::BITASR:
            Q_ASSERT( ae->d_args.size() == 2 );
            ae->d_args.first()->accept(this);
            assureInteger(ae->d_args.first()->d_type.data(),ae->d_loc);
            ae->d_args.last()->accept(this);
            convertTo( Type::INTEGER,ae->d_args.last()->d_type.data(), ae->d_args.last()->d_loc );
            line(ae->d_loc).shr_();
            break;
#if 0
        case BuiltIn::ROR: // obsolete
            Q_ASSERT( ae->d_args.size() == 2 );
            ae->d_args.first()->accept(this);
            assureInteger(ae->d_args.first()->d_type.data(),ae->d_loc);
            ae->d_args.last()->accept(this);
            convertTo( Type::INTEGER,ae->d_args.last()->d_type.data(), ae->d_args.last()->d_loc );
            line(ae->d_loc).shr_(true);
            break;
#endif
        case BuiltIn::BITAND:
            Q_ASSERT( ae->d_args.size() == 2 );
            ae->d_args.first()->accept(this);
            adjustTypes(ae->d_args.first()->d_type.data(), ae->d_args.last()->d_type.data(), ae->d_args.first()->d_loc);
            ae->d_args.last()->accept(this);
            adjustTypes(ae->d_args.last()->d_type.data(), ae->d_args.first()->d_type.data(), ae->d_args.last()->d_loc);
            line(ae->d_loc).and_();
            break;
        case BuiltIn::BITOR:
            Q_ASSERT( ae->d_args.size() == 2 );
            ae->d_args.first()->accept(this);
            adjustTypes(ae->d_args.first()->d_type.data(), ae->d_args.last()->d_type.data(), ae->d_args.first()->d_loc);
            ae->d_args.last()->accept(this);
            adjustTypes(ae->d_args.last()->d_type.data(), ae->d_args.first()->d_type.data(), ae->d_args.last()->d_loc);
            line(ae->d_loc).or_();
            break;
        case BuiltIn::BITXOR:
            Q_ASSERT( ae->d_args.size() == 2 );
            ae->d_args.first()->accept(this);
            adjustTypes(ae->d_args.first()->d_type.data(), ae->d_args.last()->d_type.data(), ae->d_args.first()->d_loc);
            ae->d_args.last()->accept(this);
            adjustTypes(ae->d_args.last()->d_type.data(), ae->d_args.first()->d_type.data(), ae->d_args.last()->d_loc);
            line(ae->d_loc).xor_();
            break;
        case BuiltIn::BITNOT:
            Q_ASSERT( ae->d_args.size() == 1 );
            ae->d_args.first()->accept(this);
            line(ae->d_loc).not_();
            break;
        case BuiltIn::LSL:
        case BuiltIn::BITSHL:
            Q_ASSERT( ae->d_args.size() == 2 );
            ae->d_args.first()->accept(this);
            assureInteger(ae->d_args.first()->d_type.data(),ae->d_loc);
            ae->d_args.last()->accept(this);
            convertTo( Type::INTEGER,ae->d_args.last()->d_type.data(), ae->d_args.last()->d_loc );
            line(ae->d_loc).shl_();
            break;
        case BuiltIn::ROR:
        case BuiltIn::BITSHR:
            Q_ASSERT( ae->d_args.size() == 2 );
            ae->d_args.first()->accept(this);
            assureInteger(ae->d_args.first()->d_type.data(),ae->d_loc);
            ae->d_args.last()->accept(this);
            convertTo( Type::INTEGER,ae->d_args.last()->d_type.data(), ae->d_args.last()->d_loc );
            line(ae->d_loc).shr_(true);
            break;
        case BuiltIn::WCHR:
            Q_ASSERT( ae->d_args.size() == 1 );
            ae->d_args.first()->accept(this);
            break;
        case BuiltIn::SHORT:
            {
                Q_ASSERT( ae->d_args.size() == 1 );
                ae->d_args.first()->accept(this);
                Type* td = derefed(ae->d_args.first()->d_type.data());
                if( td->isText() )
                    ; // NOP
                else
                {
                    switch(td->getBaseType())
                    {
                    case Type::LONGINT:
                        line(ae->d_loc).conv_(IlEmitter::ToI4);
                        break;
                    case Type::INTEGER:
                        line(ae->d_loc).conv_(IlEmitter::ToI2);
                        break;
                    case Type::SHORTINT:
                        line(ae->d_loc).conv_(IlEmitter::ToU1);
                        break;
                    case Type::LONGREAL:
                        // No, we always have r8 on stack to avoid mono x64 issues: line(ae->d_loc).conv_(IlEmitter::ToR4);
                        break;
                    default:
                        Q_ASSERT(false);
                    }
                }
            }
            break;
        case BuiltIn::LONG:
            Q_ASSERT( ae->d_args.size() == 1 );
            ae->d_args.first()->accept(this);
            switch(derefed(ae->d_args.first()->d_type.data())->getBaseType())
            {
            case Type::INTEGER:
                line(ae->d_loc).conv_(IlEmitter::ToI8);
                break;
            case Type::SHORTINT:
                line(ae->d_loc).conv_(IlEmitter::ToI4);
                break;
            case Type::BYTE:
                line(ae->d_loc).conv_(IlEmitter::ToI2);
                break;
            case Type::REAL:
                line(ae->d_loc).conv_(IlEmitter::ToR8);
                break;
            default:
                Q_ASSERT(false);
            }
            break;
        case BuiltIn::ADR: // TODO obsolete
            Q_ASSERT( ae->d_args.size() == 1 );
            ae->d_args.first()->accept(this);
            break;
        case BuiltIn::BITS:
            Q_ASSERT( ae->d_args.size() == 1 );
            ae->d_args.first()->accept(this);
            break;
        case BuiltIn::CAST:
            {
                Q_ASSERT( ae->d_args.size() == 2 );
                ae->d_args.last()->accept(this);
                Type* td = derefed(ae->d_args.first()->d_type.data());
                if( td && td->isInteger() )
                    convertTo( td->getBaseType(), ae->d_args.last()->d_type.data(), ae->d_loc );
            }
            break;
        case BuiltIn::ASSERT:
            {
                Q_ASSERT( !ae->d_args.isEmpty() ); // TODO: by now second optional arg ignored!
                ae->d_args.first()->accept(this);

                const int after = emitter->newLabel();
                line(ae->d_loc).brtrue_(after);
                line(ae->d_loc).ldstr_("\"assertion failed at line "+QByteArray::number(ae->d_loc.d_row)+"\"");
                line(ae->d_loc).newobj_("void [mscorlib]System.Exception::.ctor(string)",1);
                line(ae->d_loc).throw_();

                line(ae->d_loc).label_(after);
            }
            break;
        case BuiltIn::BYTESIZE:
            {
                Q_ASSERT( !ae->d_args.isEmpty() && !ae->d_args.first()->d_type.isNull() );
                Expression* e = ae->d_args.first().data();
                Type* t = derefed(e->d_type.data());
                switch( t->getBaseType() )
                {
                case Type::BOOLEAN:
                case Type::CHAR:
                case Type::BYTE:
                    line(ae->d_loc).ldc_i4(1);
                    break;
                case Type::WCHAR:
                case Type::SHORTINT:
                    line(ae->d_loc).ldc_i4(2);
                    break;
                case Type::INTEGER:
                case Type::REAL:
                case Type::SET:
                    line(ae->d_loc).ldc_i4(4);
                    break;
                case Type::LONGINT:
                case Type::LONGREAL:
                    line(ae->d_loc).ldc_i4(8);
                    break;
                default:
                    switch( t->getTag() )
                    {
                    case Thing::T_Pointer:
                        line(ae->d_loc).ldc_i4(4);// TODO
                        break;
                    case Thing::T_Record:
                    case Thing::T_Array:
                        line(ae->d_loc).ldc_i4(1);// TODO
                        break;
                    default:
                        Q_ASSERT( false ); // TODO
                        break;
                    }
                    break;
                }
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

    void preparePinnedArray( Type* t, const RowCol& loc )
    {
        // https://docs.microsoft.com/en-us/dotnet/framework/interop/copying-and-pinning
        // https://codereview.stackexchange.com/questions/158035/pin-an-array-in-memory-and-construct-a-bitmap-using-that-buffer

        Type* td = derefed(t);
        if( td->getTag() == Thing::T_Pointer )
        {
            Pointer* p = cast<Pointer*>(td);
            Type* tp = derefed(p->d_to.data());
            if( tp == 0 || ( tp->getTag() != Thing::T_Array && !tp->isString() ) || tp->d_unsafe )
                return;
            t = p->d_to.data();
            td = tp;
        }else
            return;

        // here remain string literals or safe arrays

        bool wide;
        if( td->isText(&wide) && !wide )
        {
            int slot1 = -1;
            if( !td->isString() )
            {
                slot1 = temps.buy("char[]");
                line(loc).dup_();
                line(loc).stloc_(slot1);
            }
            line(loc).call_("uint8[] [OBX.Runtime]OBX.Runtime::toAnsi(char[])", 1, true );
            line(loc).dup_();
            const int slot2 = temps.buy("uint8[] pinned");
            Q_ASSERT( slot2 >= 0 );
            pinnedTemps << qMakePair(slot2,slot1);
            line(loc).stloc_(slot2);
            line(loc).ldc_i4(0);
#if 1
            line(loc).ldelema_("uint8");
#else
            line(loc).call_("native int [mscorlib]System.Runtime.InteropServices.Marshal::UnsafeAddrOfPinnedArrayElement([mscorlib]System.Array,int32)", 2, true );
#endif
        }else if( td->getTag() == Thing::T_Array )
        {
            line(loc).dup_();
            const int slot = temps.buy(formatType(t) + " pinned");
            Q_ASSERT( slot >= 0 );
            pinnedTemps << qMakePair(slot,-1);
            line(loc).stloc_(slot);
            line(loc).ldc_i4(0);
            line(loc).ldelema_(formatType(cast<Array*>(td)->d_type.data()));
        }
    }

    void releasePinnedArrays(const RowCol& loc)
    {
        for( int i = 0; i < pinnedTemps.size(); i++ )
        {
            if( pinnedTemps[i].second >= 0 )
            {
                line(loc).ldloc_(pinnedTemps[i].second);
                line(loc).ldloc_(pinnedTemps[i].first);
                line(loc).call_("void [OBX.Runtime]OBX.Runtime::writeBack(char[],uint8[])", 2 );
                line(loc).ldnull_();
                line(loc).stloc_(pinnedTemps[i].second);
                temps.sell(pinnedTemps[i].second);
            }
            line(loc).ldnull_();
            line(loc).stloc_(pinnedTemps[i].first);
            temps.sell(pinnedTemps[i].first);
        }
        pinnedTemps.clear();
    }

    void emitCall( ArgExpr* me )
    {
        Q_ASSERT( me->d_sub );
        me->d_sub->accept(this);

        Named* func = 0;
        bool superCall = false;
        if( me->d_sub->getUnOp() == UnExpr::DEREF )
        {
            // call to superclass method
            UnExpr* ue = cast<UnExpr*>(me->d_sub.data());
            func = ue->d_sub->getIdent();
            Q_ASSERT( func && func->getTag() == Thing::T_Procedure );
            Procedure* p = cast<Procedure*>(func);
            Q_ASSERT( p->d_super );
            func = p->d_super;
            superCall = true;
        }else
            func = me->d_sub->getIdent();

        const int funcTag = func ? func->getTag() : 0;
        if( func && funcTag == Thing::T_BuiltIn )
        {
            emitBuiltIn( cast<BuiltIn*>(func), me );
            return;
        }else if( funcTag != Thing::T_Procedure )
            func = 0; // apparently a function pointer or delegate

        Type* subT = derefed( me->d_sub->d_type.data() );
        Q_ASSERT( subT && subT->getTag() == Thing::T_ProcType );
        ProcType* pt = cast<ProcType*>( subT );
        Q_ASSERT( pt->d_formals.size() <= me->d_args.size() );

#if 0
        if( func == 0 || pt->d_typeBound )
            Q_ASSERT( stackDepth == before + 1 ); // self or delegate instance expected
#endif

        for( int i = 0; i < pt->d_formals.size(); i++ )
        {
            Parameter* p = pt->d_formals[i].data();
            Type* tf = derefed(p->d_type.data());
            Q_ASSERT( tf != 0 );

            const int tag = tf->getTag();
            if( passByRef(p) )
            {
                if( tag == Thing::T_Array )
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
                if( !( tag == Thing::T_Record && tf->d_unsafe ) ) // check if we already have an address
                    emitFetchDesigAddr(me->d_args[i].data());
            }else
            {
                // 1) a structured arg (record, array) passed by val
                // 2) or a structured arg passed to IN, i.e. just pass the reference
                // 3) or a non-structured arg passed by IN or by val, just pass the value in both cases
                // NOTE that in case of 1) the copy is done in the body of the called procedure
                me->d_args[i]->accept(this);
                prepareRhs( tf, me->d_args[i].data(), me->d_args[i]->d_loc );

                if( pt->d_unsafe && tag == Thing::T_Pointer && tf->d_unsafe )
                    preparePinnedArray(me->d_args[i]->d_type.data(),me->d_args[i]->d_loc);
            }
        }

        // pass varargs if present
        QList<Type*> varargs;
        for( int i = pt->d_formals.size(); i < me->d_args.size(); i++ )
        {
            me->d_args[i]->accept(this);
            Type* ta = derefed(me->d_args[i]->d_type.data());
            prepareRhs( ta, me->d_args[i].data(), me->d_args[i]->d_loc );
            varargs.append(me->d_args[i]->d_type.data());
            Q_ASSERT( pt->d_unsafe );
            preparePinnedArray(me->d_args[i]->d_type.data(),me->d_args[i]->d_loc);
        }

        if( !varargs.isEmpty() && ( func == 0 || pt->d_typeBound ) )
            err->warning(Errors::Generator, Loc(me->d_loc,thisMod->d_file), "varargs not supported here" );

        if( func )
        {
            if( pt->d_typeBound && !superCall )
                line(me->d_loc).callvirt_(memberRef(func),pt->d_formals.size(),!pt->d_return.isNull()); // we dont support virtual funcs with varargs
            else
                line(me->d_loc).call_(memberRef(func,varargs),pt->d_formals.size(),!pt->d_return.isNull());
        }else
        {
            QByteArray ret = formatType(pt->d_return.data(), pt->d_unsafe);
            if( pt->d_unsafe )
                ret += " modopt([mscorlib]System.Runtime.CompilerServices.CallConvCdecl)";
            const QByteArray what = ret + " " + delegateRef(pt) + "::Invoke"
                + formatFormals(pt,false); // we don't support callbacks with varargs

            line(me->d_loc).callvirt_(what, pt->d_formals.size(),!pt->d_return.isNull());
        }

        releasePinnedArrays(me->d_loc);

        Type* rt = derefed(pt->d_return.data());
        if( rt && rt->d_unsafe && rt->getTag() == Thing::T_Record )
        {
            // we need the address of the value so we need to store it in a temp
            const int temp = temps.buy(formatType(pt->d_return.data(),pt->d_unsafe));
            line(me->d_loc).stloc_(temp);
            line(me->d_loc).ldloca_(temp);
        }
    }

    void inline prepareRhs(Type* tf, Expression* ea, const RowCol& loc)
    {
        Q_ASSERT(ea);
        Type* tfd = derefed(tf);
        Type* ta = derefed(ea->d_type.data());
        if( ta == 0 || tfd == 0 )
            return; // error already reported

        const int tagf = tfd->getTag();
        if( tfd->isChar() && !ta->isChar() )
        {
            // convert len-1-string to char
            Q_ASSERT( ta->isString() || ta->isStructured() );
            line(loc).ldc_i4(0);
            line(loc).ldelem_("char");
        }else if( tfd->isText() && !tfd->isChar() && ta->isChar() )
        {
            line(loc).call_("char[] [OBX.Runtime]OBX.Runtime::toString(char)", 1, true ); // works for both char and wchar
        }else if( tagf == Thing::T_ProcType )
        {
            Named* n = ea->getIdent();
            const bool rhsIsProc = n && n->getTag() == Thing::T_Procedure;
            if( rhsIsProc )
            {
                ProcType* pt = cast<ProcType*>(tfd);

                if( ta->d_typeBound )
                {
                    // we assign a type bound procedure to a type-bound proc type variable
                    // for this purpose we create a delegate instance on the stack
                    line(loc).dup_(); // stack: this, this
                    line(loc).ldvirtftn_(memberRef(n)); // stack: this, fn
                    line(loc).newobj_("void class " + delegateRef(pt) + "::.ctor(object, native int)", 2 );
#ifndef _CLI_PASS_RAW_FUNCTION_POINTER
                }else
                {
                    // assign a normal procedure to a normal proc type variable
                    line(loc).ldnull_();
                    line(loc).ldftn_(memberRef(n));
                    line(loc).newobj_("void class " + delegateRef(pt) + "::.ctor(object, native int)",2);
                }
#else
                }else if( tfd == 0 || !tfd->d_unsafe )
                {
                    // assign a normal procedure to a normal proc type variable
                    line(loc).ldnull_();
                    line(loc).ldftn_(memberRef(n));
                    line(loc).newobj_("void class " + delegateRef(pt) + "::.ctor(object, native int)",2);
                }else
                    line(loc).ldftn_(memberRef(n));
#endif
            }//else: we copy a proc type variable, i.e. delegate already exists

#ifndef _CLI_PASS_RAW_FUNCTION_POINTER
            if( tfd->d_unsafe && ( rhsIsProc && ( !ta->d_unsafe || tfd == ta ) ) )
            {
                // we assign a newly created or existing deleg to an unsafe proc pointer
                // add a reference to it so the GC doesn't collect it while in callback
                // TODO: more selective and consider to remove if no longer needed
                line(loc).dup_();
                line(loc).call_("void [OBX.Runtime]OBX.Runtime::addRef(object)",1); // TODO: consider GC.KeepAlive()
            }

            if( tfd->d_unsafe && ( !ta->d_unsafe || rhsIsProc ) && ta->getBaseType() != Type::NIL )
                line(loc).call_("native int [mscorlib]System.Runtime.InteropServices.Marshal::GetFunctionPointerForDelegate(class [mscorlib]System.Delegate)",1,true);
#endif
            if( !tfd->d_unsafe && ta->d_unsafe && !rhsIsProc )
                // TODO consider Marshal.GetDelegateForFunctionPointer(IntPtr, Type) if rhsIsProc and unsafe
                err->error(Errors::Generator, Loc(loc,thisMod->d_file), "assignment of unsafe to a safe procedure pointer is not supported");

        }else if( tfd->d_unsafe && tagf == Thing::T_Record )
            line(loc).ldobj_(formatType(tf)); // the formal requires a cstruct by value, so we fetch it
        else if( tagf == Thing::T_BaseType )
            convertTo(tfd->getBaseType(), ta, ea->d_loc);
        else if( tagf == Thing::T_Pointer && tfd->d_unsafe && ta->isInteger() )
            line(ea->d_loc).conv_(IlEmitter::ToI);
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

    void stringOp( Type* lhs, Type* rhs, bool lwide, bool rwide, int op, const RowCol& loc )
    {
        const bool lhsChar = lhs->isChar();
        const bool rhsChar = rhs->isChar();
        if( !lhs->d_unsafe && !rhs->d_unsafe )
        {
            line(loc).ldc_i4(op);
            if( lhsChar && rhsChar )
                line(loc).call_("bool [OBX.Runtime]OBX.Runtime::relOp(char,char,int32)",3,true);
            else if( lhsChar && !rhsChar )
                line(loc).call_("bool [OBX.Runtime]OBX.Runtime::relOp(char,char[],int32)",3,true);
            else if( !lhsChar && rhsChar )
                line(loc).call_("bool [OBX.Runtime]OBX.Runtime::relOp(char[],char,int32)",3,true);
            else
                line(loc).call_("bool [OBX.Runtime]OBX.Runtime::relOp(char[],char[],int32)",3,true);
        }else
        {
            line(loc).ldc_i4(op);
            line(loc).ldc_i4(lwide);
            line(loc).ldc_i4(rwide);
            if( lhs->d_unsafe && rhs->d_unsafe )
                line(loc).call_("bool [OBX.Runtime]OBX.Runtime::relOp(native int,native int,int32,bool,bool)",5,true);
            else if( lhs->d_unsafe )
            {
                if( rhsChar )
                    line(loc).call_("bool [OBX.Runtime]OBX.Runtime::relOp(native int,char,int32,bool,bool)",5,true);
                else
                    line(loc).call_("bool [OBX.Runtime]OBX.Runtime::relOp(native int,char[],int32,bool,bool)",5,true);
            }else
            {
                if( rhsChar )
                    line(loc).call_("bool [OBX.Runtime]OBX.Runtime::relOp(char,native int,int32,bool,bool)",5,true);
                else
                    line(loc).call_("bool [OBX.Runtime]OBX.Runtime::relOp(char[],native int,int32,bool,bool)",5,true);
            }
        }
    }

    void convertTo( quint8 toBaseType, Type* from, const RowCol& loc )
    {
        from = derefed(from);
        if( from == 0 )
            return;
        const int fromBaseType = from->getBaseType();
        if( toBaseType == fromBaseType )
            return;
        switch( toBaseType )
        {
        case Type::LONGREAL:
        case Type::REAL:
            if( fromBaseType != Type::LONGREAL )
                line(loc).conv_(IlEmitter::ToR8); // always r8 on stack
            break;
        case Type::LONGINT:
            line(loc).conv_(IlEmitter::ToI8);
            break;
        case Type::INTEGER:
        case Type::SET:
        case Type::ENUMINT:
            line(loc).conv_(IlEmitter::ToI4);
            break;
        case Type::SHORTINT:
        case Type::CHAR:
        case Type::WCHAR:
            line(loc).conv_(IlEmitter::ToI2);
            break;
        case Type::BYTE:
        case Type::BOOLEAN:
            line(loc).conv_(IlEmitter::ToU1);
            break;
        }
    }

    int widenType(int baseType )
    {
        switch(baseType)
        {
        case Type::BOOLEAN:
        case Type::CHAR:
        case Type::WCHAR:
        case Type::BYTE:
        case Type::SHORTINT:
            return Type::INTEGER;
        default:
            return baseType;
        }
    }

    void adjustTypes( Type* cur, Type* other, const RowCol& loc )
    {
        cur = derefed(cur);
        other = derefed(other);
        Q_ASSERT( cur && other );
        const int c = widenType(cur->getBaseType());
        const int o = widenType(other->getBaseType());
        if( c == o )
            return;
        if( o == Type::LONGINT && c != Type::LONGINT )
            convertTo(Type::LONGINT, cur, loc);
        else if( o == Type::LONGREAL && c != Type::LONGREAL )
            convertTo(Type::LONGREAL, cur, loc);
        else if( o == Type::REAL && c == Type::INTEGER )
            convertTo(Type::REAL, cur, loc);
    }

    void visit( BinExpr* me)
    {
        Q_ASSERT( !me->d_lhs.isNull() && !me->d_rhs.isNull() &&
                  !me->d_lhs->d_type.isNull() && !me->d_rhs->d_type.isNull() );

        Type* lhsT = derefed(me->d_lhs->d_type.data());
        Type* rhsT = derefed(me->d_rhs->d_type.data());
        Q_ASSERT( lhsT && rhsT );

        me->d_lhs->accept(this);
        if( me->isRelation() )
            convertTo(me->d_inclType, me->d_lhs->d_type.data(), me->d_lhs->d_loc);
        else if( me->isArithOp() )
            adjustTypes( lhsT, rhsT, me->d_lhs->d_loc );

        if( me->d_op != BinExpr::AND && me->d_op != BinExpr::OR )
        {
            // AND and OR are special in that rhs might not be executed
            me->d_rhs->accept(this);
            if( me->isRelation() )
                convertTo(me->d_inclType, me->d_rhs->d_type.data(), me->d_rhs->d_loc );
            else if( me->isArithOp() )
                adjustTypes( rhsT, lhsT, me->d_rhs->d_loc );
        }

        const int ltag = lhsT->getTag();
        const int rtag = rhsT->getTag();
        bool lwide, rwide;

        switch( me->d_op )
        {
        case BinExpr::IN:
            if( lhsT->isInteger() && rhsT->getBaseType() == Type::SET )
            {
                line(me->d_loc).call_("bool [OBX.Runtime]OBX.Runtime::IN(int32, int32)", 2, true );
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::IS:
            line(me->d_loc).isinst_(formatType(rhsT));// returns object or null
            line(me->d_loc).ldnull_();
            line(me->d_loc).ceq_(); // true if null
            line(me->d_loc).ldc_i4(0);
            line(me->d_loc).ceq_(); // not
            break;
        case BinExpr::ADD:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) )
                line(me->d_loc).add_();
            else if( lhsT->isSet() && rhsT->isSet() )
                line(me->d_loc).or_();
            else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) && !lhsT->d_unsafe && !rhsT->d_unsafe )
            {
                if( lhsT->isChar() && rhsT->isChar() )
                    line(me->d_loc).call_("char[] [OBX.Runtime]OBX.Runtime::join(char,char)", 2, true );
                else if( lhsT->isChar() && !rhsT->isChar() )
                    line(me->d_loc).call_("char[] [OBX.Runtime]OBX.Runtime::join(char,char[])",2,true);
                else if( !lhsT->isChar() && rhsT->isChar() )
                    line(me->d_loc).call_("char[] [OBX.Runtime]OBX.Runtime::join(char[],char)",2,true);
                else
                    line(me->d_loc).call_("char[] [OBX.Runtime]OBX.Runtime::join(char[],char[])",2,true);
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::SUB:
            if( (lhsT->isNumeric() && rhsT->isNumeric()) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) )
                line(me->d_loc).sub_();
            else if( lhsT->isSet() && rhsT->isSet() )
            {
                line(me->d_loc).not_();
                line(me->d_loc).and_();
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::FDIV:
            if( lhsT->isNumeric() && rhsT->isNumeric() )
                line(me->d_loc).div_();
            else if( lhsT->isSet() && rhsT->isSet() )
            {
                const int rhs = temps.buy("int32");
                line(me->d_loc).stloc_(rhs);
                const int lhs = temps.buy("int32");
                line(me->d_loc).stloc_(lhs);
                line(me->d_loc).ldloc_(lhs);
                line(me->d_loc).ldloc_(rhs);
                line(me->d_loc).and_();
                line(me->d_loc).not_();
                line(me->d_loc).ldloc_(lhs);
                line(me->d_loc).ldloc_(rhs);
                line(me->d_loc).or_();
                line(me->d_loc).and_();
                temps.sell(rhs);
                temps.sell(lhs);
            }
            break;
        case BinExpr::MUL:
            if( lhsT->isNumeric() && rhsT->isNumeric() )
                line(me->d_loc).mul_();
            else if( lhsT->isSet() && rhsT->isSet() )
                line(me->d_loc).and_();
            else
                Q_ASSERT(false);
            break;
        case BinExpr::DIV:
            if( lhsT->isInteger() && rhsT->isInteger() )
            {
#if 1
                if( lhsT->getBaseType() <= Type::INTEGER && rhsT->getBaseType() <= Type::INTEGER )
                    line(me->d_loc).call_("int32 [OBX.Runtime]OBX.Runtime::DIV(int32,int32)",2,true );
                else
                    line(me->d_loc).call_("int64 [OBX.Runtime]OBX.Runtime::DIV(int64,int64)",2,true );
#else
                line(me->d_loc).div_(); // TEST, nearly no performance impact on Mono and CoreCLR
#endif
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::MOD:
            if( lhsT->isInteger() && rhsT->isInteger() )
            {
#if 1
                if( lhsT->getBaseType() <= Type::INTEGER && rhsT->getBaseType() <= Type::INTEGER )
                    line(me->d_loc).call_("int32 [OBX.Runtime]OBX.Runtime::MOD(int32,int32)",2,true);
                else
                    line(me->d_loc).call_("int64 [OBX.Runtime]OBX.Runtime::MOD(int64,int64)",2,true);
#else
                line(me->d_loc).rem_(); // TEST
#endif
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::AND:
            if( lhsT->getBaseType() == Type::BOOLEAN && rhsT->getBaseType() == Type::BOOLEAN )
            {
                // lhs was run and stack has a bool result
                const int afterEnd = emitter->newLabel();
                const int setFalse = emitter->newLabel();
                line(me->d_loc).brfalse_(setFalse);
                me->d_rhs->accept(this);
                line(me->d_loc).br_(afterEnd);
                line(me->d_loc).label_(setFalse);
                line(me->d_loc).ldc_i4(0);
                line(me->d_loc).label_(afterEnd);
                // TODO stackDepth--; // correct for alternative rhs or ldc
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::OR:
            if( lhsT->getBaseType() == Type::BOOLEAN && rhsT->getBaseType() == Type::BOOLEAN )
            {
                // lhs was run and stack has a bool result
                const int afterEnd = emitter->newLabel();
                const int setTrue = emitter->newLabel();
                line(me->d_loc).brtrue_(setTrue);
                me->d_rhs->accept(this);
                line(me->d_loc).br_(afterEnd);
                line(me->d_loc).label_(setTrue);
                line(me->d_loc).ldc_i4(1);
                line(me->d_loc).label_(afterEnd);
                // TODO stackDepth--; // correct for alternative rhs or ldc
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
                line(me->d_loc).ceq_();
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                stringOp(lhsT, rhsT, lwide, rwide, 1, me->d_loc );
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
                line(me->d_loc).ceq_();
                line(me->d_loc).ldc_i4(0);
                line(me->d_loc).ceq_();
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                stringOp(lhsT, rhsT, lwide, rwide, 2, me->d_loc );
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::LT:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
                    ( lhsT->isChar() && rhsT->isChar() ) )
            {
                line(me->d_loc).clt_();
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                stringOp(lhsT, rhsT, lwide, rwide, 3, me->d_loc );
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::LEQ:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
                    ( lhsT->isChar() && rhsT->isChar() ) )
            {
                line(me->d_loc).cgt_();
                line(me->d_loc).ldc_i4(0);
                line(me->d_loc).ceq_();
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                stringOp(lhsT, rhsT, lwide, rwide, 4, me->d_loc );
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::GT:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
                    ( lhsT->isChar() && rhsT->isChar() ) )
            {
                line(me->d_loc).cgt_();
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                stringOp(lhsT, rhsT, lwide, rwide, 5, me->d_loc );
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::GEQ:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
                    ( lhsT->isChar() && rhsT->isChar() ) )
            {
                line(me->d_loc).clt_();
                line(me->d_loc).ldc_i4(0);
                line(me->d_loc).ceq_();
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                stringOp(lhsT, rhsT, lwide, rwide, 6, me->d_loc );
            }else
            {
                qDebug() << me->d_loc.d_row << me->d_loc.d_col << thisMod->d_file;
                Q_ASSERT(false);
            }
            break;
        default:
            Q_ASSERT(false);
        }
    }

    void visit( SetExpr* me)
    {
        line(me->d_loc).ldc_i4(0);
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
                line(me->d_loc).call_("int32 [OBX.Runtime]OBX.Runtime::addRangeToSet(int32, int32, int32)",3,true);
                // new set on stack
            }else
            {
                // set or 0 already on stack
                me->d_parts[i]->accept(this);
                // element index on stack
                line(me->d_loc).call_("int32 [OBX.Runtime]OBX.Runtime::addElemToSet(int32, int32)",2,true);
                // new set on stack
            }
        }
    }

    void visit( Call* me)
    {
        Q_ASSERT( me->d_what );
        me->d_what->accept(this);
        if( !me->d_what->d_type.isNull() )
        {
            Type* td = derefed(me->d_what->d_type.data());
            if(td && td->getTag() == Thing::T_BaseType && td->getBaseType() != Type::NONE )
                line(me->d_loc).pop_();
        }
    }

    void visit( ForLoop* me)
    {
        //const int before = stackDepth;
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
        // TODO Q_ASSERT( before == stackDepth );
    }

    void emitIf( IfLoop* me)
    {
        me->d_if[0]->accept(this); // IF
        const int afterFirst = emitter->newLabel();
        line(me->d_loc).brfalse_(afterFirst);

        for( int i = 0; i < me->d_then[0].size(); i++ )
            me->d_then[0][i]->accept(this);

        const int afterEnd = emitter->newLabel();
        line(me->d_loc).br_(afterEnd);

        line(me->d_loc).label_(afterFirst);
        for( int i = 1; i < me->d_if.size(); i++ ) // ELSIF
        {
            me->d_if[i]->accept(this);
            const int afterNext = emitter->newLabel();
            line(me->d_loc).brfalse_(afterNext);

            for( int j = 0; j < me->d_then[i].size(); j++ )
                me->d_then[i][j]->accept(this);

            line(me->d_loc).br_(afterEnd);

            line(me->d_loc).label_(afterNext);
        }

        if( !me->d_else.isEmpty() ) // ELSE
        {
            for( int j = 0; j < me->d_else.size(); j++ )
                me->d_else[j]->accept(this);
        }

        line(me->d_loc).label_(afterEnd);
    }

    void visit( IfLoop* me)
    {
        //const int before = stackDepth;
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
                const int loopStart = emitter->newLabel();
                line(me->d_loc).label_(loopStart);

                for( int i = 0; i < me->d_then.first().size(); i++ )
                    me->d_then.first()[i]->accept(this);

                me->d_if[0]->accept(this); // until condition
                const int afterEnd = emitter->newLabel();
                line(me->d_loc).brtrue_(afterEnd);

                line(me->d_loc).br_(loopStart);

                line(me->d_loc).label_(afterEnd);
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
                Q_ASSERT( exitJump == -1 );
                const int loopStart = emitter->newLabel();
                line(me->d_loc).label_(loopStart);

                for( int i = 0; i < me->d_then.first().size(); i++ )
                    me->d_then.first()[i]->accept(this);

                line(me->d_loc).br_(loopStart);

                if( exitJump != -1 )
                    line(me->d_loc).label_(exitJump);
                exitJump = -1;
            }
            break;
        }
        // no, it is legal and not known here how many values are pushed in the body: Q_ASSERT( before == stackDepth );
    }

    void emitCopyArray( Type* lhs, Type* rhs, const RowCol& loc )
    {
        Type* tl = derefed(lhs);
        Type* tr = derefed(rhs);
        Q_ASSERT( tl && tr );
        bool lwide, rwide;
        if( tl->isText(&lwide) && tr->isText(&rwide) )
        {
            if( !tl->d_unsafe && !tr->d_unsafe )
                line(loc).call_("void [OBX.Runtime]OBX.Runtime::copy(char[],char[])",2);
            else if( tl->d_unsafe && !tr->d_unsafe )
            {
                line(loc).ldc_i4(lwide);
                line(loc).call_("void [OBX.Runtime]OBX.Runtime::copy(native int,char[],bool)",3);
            }
            else if( !tl->d_unsafe && tr->d_unsafe )
            {
                line(loc).ldc_i4(rwide);
                line(loc).call_("void [OBX.Runtime]OBX.Runtime::copy(char[],native int,bool)",3);
            }else
            {
                line(loc).ldc_i4(lwide);
                line(loc).ldc_i4(rwide);
                line(loc).call_("void [OBX.Runtime]OBX.Runtime::copy(native int,native int,bool,bool)",4);
            }
        }else if( tl->d_unsafe && tr->d_unsafe )
        {
            Q_ASSERT( tl->getTag() == Thing::T_Array && tr->getTag() == Thing::T_Array );
            Array* la = cast<Array*>(tl);
            Array* ra = cast<Array*>(tr);
            if( la->d_lenExpr.isNull() || ra->d_lenExpr.isNull() )
                err->error(Errors::Generator, Loc(loc,thisMod->d_file),"copying of unsafe open arrays not supported");
            else
            {
                const quint32 len = qMin(la->d_len,ra->d_len) * la->d_type->getByteSize();
                line(loc).ldc_i4(len);
                line(loc).cpblk_();
            }
        }else if( !tl->d_unsafe && !tr->d_unsafe )
            line(loc).call_(formatArrayCopierRef(cast<Array*>(tl)),2);
        else
            err->error(Errors::Generator, Loc(loc,thisMod->d_file),"operation not supported");
    }

    void visit( Assign* me )
    {
        Q_ASSERT( me->d_rhs );

        Q_ASSERT( me->d_lhs );

        Q_ASSERT( !me->d_lhs->d_type.isNull() );
        Q_ASSERT( !me->d_rhs->d_type.isNull() );

        Type* lhsT = derefed(me->d_lhs->d_type.data());
        Q_ASSERT( lhsT != 0 );

        if( lhsT->isStructured() )
        {
            switch(lhsT->getTag())
            {
            case Thing::T_Record:
                {
                    // stack: lhs record, rhs record
                    Record* r = cast<Record*>(lhsT);
                    if( r->d_unsafe )
                    {
#if 1
                        emitFetchDesigAddr(me->d_lhs.data());
                        me->d_rhs->accept(this);
                        line(me->d_loc).ldc_i4(lhsT->getByteSize());
                        line(me->d_loc).cpblk_();
#else
                        me->d_rhs->accept(this); // TODO rhs kann funktion sein die value zurückgibt
                        line(me->d_loc).ldobj_( formatType(me->d_rhs->d_type.data()) );
                        // TODO emitStackToVar();
#endif
                    }else
                    {
                        me->d_lhs->accept(this);
                        me->d_rhs->accept(this);
                        prepareRhs(lhsT, me->d_rhs.data(), me->d_loc );
                        QByteArray what = "void " + classRef(r) + formatMetaActuals(r) + "::'#copy'(";
                        what += formatType(r);
                        if( r->d_byValue )
                            what += "&";
                        what += ")";
                        line(me->d_loc).callvirt_(what,1);
                    }
                }
                break;
            case Thing::T_Array:
                {
                    me->d_lhs->accept(this);
                    me->d_rhs->accept(this);
                    prepareRhs(lhsT, me->d_rhs.data(), me->d_loc );
                    // stack: lhs array, lhs array, rhs array
                    emitCopyArray(me->d_lhs->d_type.data(),me->d_rhs->d_type.data(), me->d_loc);
                }
                break;
            default:
                Q_ASSERT(false);
            }
        }else
        {
            const bool unsafe = emitFetchDesigAddr(me->d_lhs.data());
            me->d_rhs->accept(this);
            prepareRhs(lhsT, me->d_rhs.data(), me->d_loc );
            // no longer used, instead in prepareRhs:
            // convertTo(lhsT->getBaseType(),me->d_rhs->d_type.data(), me->d_loc);
#if _USE_LDSTOBJ
            line(me->d_loc).stobj_(formatType(me->d_lhs->d_type.data()));
#else
            switch( lhsT->getTag() )
            {
            case Thing::T_Pointer:
            case Thing::T_ProcType:
                if( lhsT->d_unsafe )
                    line(me->d_loc).stind_(IlEmitter::IntPtr);
                else
                    line(me->d_loc).stind_(IlEmitter::Ref);
                break;
            case Thing::T_Enumeration:
                //line(me->d_loc).stobj_(formatType(me->d_lhs->d_type.data()));
                line(me->d_loc).stind_(IlEmitter::I4);
                break;
            case Thing::T_BaseType:
                switch( lhsT->getBaseType() )
                {
                case Type::LONGREAL:
                    line(me->d_loc).stind_(IlEmitter::R8);
                    break;
                case Type::REAL:
                    line(me->d_loc).stind_(IlEmitter::R4);
                    break;
                case Type::LONGINT:
                    line(me->d_loc).stind_(IlEmitter::I8);
                    break;
                case Type::INTEGER:
                case Type::SET:
                    line(me->d_loc).stind_(IlEmitter::I4);
                    break;
                case Type::CHAR:
                    if( unsafe )
                        line(me->d_loc).stind_(IlEmitter::I1);
                    else
                        line(me->d_loc).stind_(IlEmitter::I2);
                    break;
                case Type::SHORTINT:
                case Type::WCHAR:
                    line(me->d_loc).stind_(IlEmitter::I2);
                    break;
                case Type::BYTE:
                case Type::BOOLEAN:
                    line(me->d_loc).stind_(IlEmitter::I1);
                    break;
                default:
                    Q_ASSERT(false);
                }
                break;
            default:
                Q_ASSERT(false);
            }
#endif
        }
    }

    void visit( CaseStmt* me)
    {
        // TODO: if else missing then abort if no case hit
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

            ifl->d_else = me->d_else;

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
                            // TODO: consider lhs > rhs
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

            ifl->d_else = me->d_else;

            // and now generate code for the if
            ifl->accept(this);
        }
    }

    void visit( Exit* me)
    {
        if( exitJump < 0 )
            exitJump = emitter->newLabel();
        else
            Q_ASSERT( false );
        line(me->d_loc).br_(exitJump);
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
                switch(ltd->getTag())
                {
                case Thing::T_Record:
                    {
                        Record* r = cast<Record*>(ltd);
                        if( r->d_unsafe )
                        {
                            what->accept(this);
                            line(loc).ldobj_(formatType(r)); // return type requires a cstruct by value
                        }else
                        {
                            emitInitializer(lt,false,loc); // create new record or array
                            line(loc).dup_();
                            what->accept(this);
                            // stack: new record, new record, rhs record
                            QByteArray what = "void " + classRef(r) + formatMetaActuals(r) + "::'#copy'(";
                            what += formatType(r);
                            if( r->d_byValue )
                                what += "&";
                            what += ")";
                            line(loc).callvirt_(what,1);
                        }
                    }
                    break;
                case Thing::T_Array:
                    {
                        if( ltd && ltd->d_unsafe )
                        {
                            err->error(Errors::Generator, Loc(loc,thisMod->d_file), "returning unsafe arrays by value not supported");
                            break;
                        }
                        emitInitializer(lt,false,loc); // create new record or array
                        line(loc).dup_();
                        what->accept(this);
                        // stack: new array, new array, rhs array
                        line(loc).call_(formatArrayCopierRef(cast<Array*>(ltd)),2);
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
            line(loc).ret_(true);
        }else if( !pt->d_return.isNull() )
        {
            // a function with no body; return default value
            //suppressLine++;
            if( !emitInitializer(pt->d_return.data(),false,loc) )
                line(loc).ldnull_(); // only happens for pointer and proctype
            //suppressLine--;
            line(loc).ret_(true);
        }else
        {
            line(loc).ret_();
        }
    }

    void visit( Return* me )
    {
        Q_ASSERT( scope != 0 );
        emitReturn( scope->getProcType(), me->d_what.data(), me->d_loc );
    }

    static inline Type* derefed( Type* t )
    {
        if( t )
            return t->derefed();
        else
            return 0;
    }

    IlEmitter& line(const RowCol& loc)
    {
        if( debug ) // TODO has unwanted effects (some nested procs no longer have symbols): suppressLine <= 0 )
        {
            if( !(loc == last) )
            {
                emitter->line_(loc);
                last = loc;
            }
        }
        return *emitter;
    }

    bool emitInitializer( Type* t, bool resolvePtr, const RowCol& loc, const QList<int>& lengths = QList<int>() )
    {
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
                line(loc).ldc_i4(0);
                break;
            case Type::LONGINT:
                line(loc).ldc_i8(0);
                break;
            case Type::REAL:
                line(loc).ldc_r4(0.0);
                break;
            case Type::LONGREAL:
                line(loc).ldc_r8(0.0);
                break;
#ifndef _MY_GENERICS_
            case Type::ANY:
                {
                    QByteArray name = formatType(t);
                    emitOpcode2("ldsfld ",  1, loc );
                    out << name;
                    if( name.startsWith('\'') )
                        name = name.mid(1, name.size() - 2 );
                    name = name.mid(1); // get rid of !
                    out << " class " << moduleRef(thisMod) << formatMetaActuals(thisMod) << "::'##" << name << "'" << endl;
                }
                break;
#endif
            default:
                Q_ASSERT( false );
                break;
            }
            return true;
        case Thing::T_Enumeration:
            line(loc).ldc_i4(0);
            return true;
        case Thing::T_ProcType:
        case Thing::T_Pointer:
#if 0 // not needed with CLI
            emitOpcode("ldnull",1,loc);
            Q_ASSERT( stackDepth == before+1 );
            return true;
#else
            break;
#endif
        case Thing::T_Record:
            if( !td->d_unsafe )
            {
                Record* r = cast<Record*>(td);
                Q_ASSERT( !r->d_byValue );

                line(loc).newobj_("void class " + classRef(r) + formatMetaActuals(r)
                    + "::.ctor()"); // initializes fields incl. superclasses
                return true;
            }
            break;
        case Thing::T_Array:
            if( td->d_unsafe )
            {
                Array* a = cast<Array*>(td);
                Type* td = derefed(a->d_type.data());

                Q_ASSERT( !a->d_lenExpr.isNull() );
                const quint32 len = a->d_len * td->getByteSize();
                line(loc).ldc_i4( len );
                if( scope )
                    line(loc).localloc_(); // we're in a procedure
                else
                    // we're on module level TODO: someone to call Marshal.FreeHGlobal()
                    line(loc).call_("native int [mscorlib]System.Runtime.InteropServices.Marshal::AllocHGlobal(int32)",1,true);
                line(loc).dup_();
                line(loc).ldc_i4(0);
                line(loc).ldc_i4(len);
                line(loc).initblk_();
            }else
            {
                Array* a = cast<Array*>(td);
                Type* td = derefed(a->d_type.data());

                int len = -1;
                if( !lengths.isEmpty() )
                {
                    Q_ASSERT( a->d_lenExpr.isNull() );
                    len = lengths.first();
                    line(loc).ldloc_(len);
                }else
                {
                    Q_ASSERT( !a->d_lenExpr.isNull() );
                    line(loc).ldc_i4(a->d_len);
                    if( td->isStructured() )
                    {
                        len = temps.buy("int32");
                        line(loc).dup_();
                        line(loc).stloc_(len);
                    }
                }
                // here the len is on the stack, either from constant or
                line(loc).newarr_(formatType(a->d_type.data())); // must be a->d_type, not td!

                if( td->isStructured() )
                {
                    const int i = temps.buy("int32");
                    Q_ASSERT( i >= 0 );
                    line(loc).ldc_i4(0);
                    line(loc).stloc_(i);
#if 0 // works with .Net 4.0 Windows, but neither with Mono 3 nor 5 (runtime exception because of dup (the one at loopStartLbl)
                    const int checkLenLbl = emitter->newLabel();
                    emitOpcode("br '#"+QByteArray::number(checkLenLbl)+"'",0,loc);

                    const int loopStartLbl = emitter->newLabel();
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
                    const int checkLenLbl = emitter->newLabel();
                    line(loc).label_(checkLenLbl);
                    line(loc).ldloc_(i);
                    line(loc).ldloc_(len);
                    const int afterLoopLbl = emitter->newLabel();
                    line(loc).bge_(afterLoopLbl);

                    line(loc).dup_();
                    // new array on top
                    line(loc).ldloc_(i);
                    // index on top

                    if( lengths.size() > 1 )
                        emitInitializer(a->d_type.data(), false, loc, lengths.mid(1) );
                    else
                        emitInitializer(a->d_type.data(), false, loc );
                    // now the array value is on top of the stack
                    line(loc).stelem_(formatType(a->d_type.data()));

                    line(loc).ldloc_(i);
                    line(loc).ldc_i4(1);
                    line(loc).add_();
                    line(loc).stloc_(i);
                    line(loc).br_(checkLenLbl);
                    line(loc).label_(afterLoopLbl);
#endif
                    temps.sell(i);
                }
                if( len >= 0 )
                    temps.sell(len);
                // leaves new array on top of stack
            }
            return true;
        default:
            Q_ASSERT(false);
        }
        return false;
    }

    void emitStackToVar( Named* me, const RowCol& loc )
    {
        switch( me->getTag() )
        {
        case Thing::T_Field:
            Q_ASSERT( !me->d_unsafe );
            line(loc).stfld_(memberRef(me));
            break;
        case Thing::T_Variable:
            line(loc).stsfld_(memberRef(me));
            break;
        case Thing::T_LocalVar:
            Q_ASSERT( me->d_slotValid );
            line(loc).stloc_(me->d_slot);
            break;
        case Thing::T_Parameter:
            Q_ASSERT( me->d_slotValid );
            line(loc).starg_(me->d_slot);
            break;
        }
    }

    void emitVarToStack( Named* me, const RowCol& loc )
    {
        Type* td = derefed(me->d_type.data());
        const bool getAddr = td && td->getTag() == Thing::T_Record && td->d_unsafe;
        switch( me->getTag() )
        {
        case Thing::T_Field:
#ifdef _CLI_USE_PTR_TO_MEMBER_
            if( me->d_unsafe )
            {
                // pointer arithmetic instead of ldfld/ldflda
                Q_ASSERT(me->d_slotValid);
                line(loc).ldc_i4(me->d_slot);
                line(loc).add_();
                if( !getAddr && !td->isStructured() )
                    emitValueFromAdrToStack(td,true,loc);
            }
#else
            if( me->d_unsafe && td && td->getTag() == Thing::T_Array )
            {
                // an array member of an unsafe record is just the first element by value with reserved space afterwards
                // so we take the address of the first element
                Q_ASSERT(!arrayAsElementType);
                arrayAsElementType = true;
                line(loc).ldflda_(memberRef(me));
                arrayAsElementType = false;
            }
#endif
            else if( getAddr )
                line(loc).ldflda_(memberRef(me));
            else
                line(loc).ldfld_(memberRef(me));
            break;
        case Thing::T_Variable:
#ifndef _CLI_DYN_STRUCT_VARIABLES_
            if( getAddr )
                line(loc).ldsflda_(memberRef(me));
            else
                line(loc).ldsfld_(memberRef(me));
#else
            Q_ASSERT(!inUnsafeRecord);
            if( getAddr )
                inUnsafeRecord = true; // trick to get the "*" behind the type
            line(me->d_loc).ldsfld_(memberRef(me)); // it's already a pointer
            inUnsafeRecord = false;
#endif
            break;
        case Thing::T_LocalVar:
            if( getAddr )
                line(loc).ldloca_(me->d_slot);
            else
                line(loc).ldloc_(me->d_slot);
            break;
        case Thing::T_Parameter:
            if( getAddr )
                line(loc).ldarga_(me->d_slot);
            else
                line(loc).ldarg_(me->d_slot);
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
            line(loc).dup_();
            line(loc).ldlen_();
            const int len = temps.buy("int32");
            lengths.append(len);
            line(loc).stloc_(len);
            line(loc).ldc_i4(0);
            line(loc).ldelem_(formatType(a->d_type.data()));
            t = derefed(a->d_type.data() );
        }
        line(loc).pop_();
    }

    void emitInitializer( Named* me )
    {
        const int tag = me->getTag();
        switch( tag )
        {
        case Thing::T_Variable:
#ifdef _CLI_DYN_STRUCT_VARIABLES_
            {
                Type* t = derefed(me->d_type.data());
                if( t && t->d_unsafe && t->getTag() == Thing::T_Record )
                {
                    const quint32 len = t->getByteSize();
                    line(me->d_loc).ldc_i4( len );
                    line(me->d_loc).call_("native int [mscorlib]System.Runtime.InteropServices.Marshal::AllocHGlobal(int32)",1,true);
                    line(me->d_loc).dup_();
                    line(me->d_loc).initobj_(formatType(me->d_type.data())); // here we dont want the star
                    Q_ASSERT( !inUnsafeRecord );
                    inUnsafeRecord = true;
                    line(me->d_loc).stsfld_(memberRef(me)); // here we want the star
                    inUnsafeRecord = false;
                }else if( emitInitializer( me->d_type.data(), false, me->d_loc ) )
                    line(me->d_loc).stsfld_(memberRef(me));
            }
            break;
#endif
        case Thing::T_LocalVar:
            {
                Type* t = derefed(me->d_type.data());
                if( t && t->d_unsafe && t->getTag() == Thing::T_Record )
                {
                    if( tag == Thing::T_LocalVar )
                    {
                        Q_ASSERT( me->d_slotValid );
                        line(me->d_loc).ldloca_(me->d_slot);
                    }else
                        line(me->d_loc).ldsflda_(memberRef(me));
                    line(me->d_loc).initobj_(formatType(me->d_type.data()));
                }else if( emitInitializer( me->d_type.data(), false, me->d_loc ) )
                    emitStackToVar( me, me->d_loc );
            }
            break;
        case Thing::T_Parameter:
            {
                Parameter* p = cast<Parameter*>(me);
                Type* t = derefed(p->d_type.data());
                if( !p->d_var && t && t->isStructured() && !t->d_unsafe ) // TODO: unsafe arrays
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
                    line(me->d_loc).dup_();
                    emitVarToStack(me, me->d_loc);
                    switch(t->getTag())
                    {
                    case Thing::T_Record:
                        {
                            // stack: lhs record, lhs record, rhs record
                            Record* r = cast<Record*>(t);
                            QByteArray what = "void " + classRef(r) + formatMetaActuals(r) + "::'#copy'(";
                            what += formatType(r);
                            if( r->d_byValue )
                                what += "&";
                            what += ")";
                            line(me->d_loc).callvirt_(what,1);
                            // stack: lhs record
                        }
                        break;
                    case Thing::T_Array:
                        {
                            // stack: lhs array, lhs array, rhs array
                            line(me->d_loc).call_(formatArrayCopierRef(cast<Array*>(t)),2);
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
    }

    // NOP
    void visit( NamedType* ) {Q_ASSERT( false );}
    void visit( Const* ) {Q_ASSERT( false );}
    void visit( GenericName* ) {Q_ASSERT( false );}
    void visit( BuiltIn* ) {Q_ASSERT( false );}
    void visit( Parameter* ) { Q_ASSERT( false ); }
    void visit( Import* ) { Q_ASSERT( false ); }
};

bool CilGen::translate(Module* m, IlEmitter* e, bool debug, Ob::Errors* errs)
{
    Q_ASSERT( m != 0 && e != 0 );

    if( m->d_hasErrors || !m->d_isValidated ) //  not validated can happen if imports cannot be resolved
        return false;

    if( m->d_isDef && !m->d_externC )
        return true;

    ObxCilGenImp imp;
    imp.thisMod = m;
    imp.emitter = e;
    imp.debug = debug;

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


    bool ok = true;
    try
    {
        m->accept(&imp);
        ok = imp.err->getErrCount() == errCount;
    }catch(...)
    {
        ok = false;
    }

    if( imp.ownsErr )
        delete imp.err;
    return ok;
}

bool CilGen::generateMain(IlEmitter* e, const QByteArray& thisMod, const QByteArray& callMod, const QByteArray& callFunc)
{
    if( callMod.isEmpty() )
        return false;

    QByteArray mod = ObxCilGenImp::escape(thisMod);
    QByteArrayList imports;
    imports << ObxCilGenImp::escape(callMod);
    imports.append( "mscorlib" );
    imports.append( "OBX.Runtime" );
    e->beginModule(mod, mod, imports,QString(),IlEmitter::ConsoleApp);
    e->beginMethod("main",false,IlEmitter::Primary);

    // call indirectly so that every exception is caught (Mono 3 & 5 debugger doesn't handle uncaught exceptions)
    e->ldnull_();
    QByteArray func;
    func = "void ['" + callMod + "']'" + callMod + "'::'begïn'()";
    e->ldftn_(func);
    e->newobj_("void class [OBX.Runtime]OBX.Command::'.ctor'(object, native int)");
    e->ldc_i4(1);
    e->call_("bool [OBX.Runtime]OBX.Runtime::pcall(class [OBX.Runtime]OBX.Command,bool)",2,true);
    // NOTE: this is delicate code an mishaps happen quickly leading to Mono runtime crashes!
    e->pop_();

    if( !callFunc.isEmpty() )
    {
        e->ldnull_();
        func = "void ['" + callMod + "']'" + callMod + "'::'" + callFunc + "'()";
        e->ldftn_(func);
        e->newobj_("void class [OBX.Runtime]OBX.Command::'.ctor'(object, native int)");
        //e->callvirt_("void [OBX.Runtime]OBX.Command::Invoke()",0);
        e->ldc_i4(1);
        e->call_("bool [OBX.Runtime]OBX.Runtime::pcall(class [OBX.Runtime]OBX.Command,bool)",2,true);
        // NOTE: this is delicate code an mishaps happen quickly leading to Mono runtime crashes!
        e->pop_();
    }

    e->ret_();
    e->endMethod();
    e->endModule();
    return true;
}

bool CilGen::generateMain(IlEmitter* e, const QByteArray& thisMod, const QByteArrayList& callMods)
{
    Q_ASSERT( e );
    if( callMods.isEmpty() )
        return false;

    QByteArrayList imports;
    foreach( const QByteArray& mod, callMods )
        imports << ObxCilGenImp::escape(mod);
    imports.append( "'mscorlib'" );
    imports.append( "'OBX.Runtime'" );

    e->beginModule(ObxCilGenImp::escape(thisMod),ObxCilGenImp::escape(thisMod), imports,QString(),IlEmitter::ConsoleApp);

    e->beginMethod("main",false,IlEmitter::Primary);

    foreach( const QByteArray& mod, callMods )
    {
        e->ldnull_();
        const QByteArray func = "void ['" + mod + "']'" + mod + "'::'begïn'()";
        e->ldftn_(func);
        e->newobj_("void class [OBX.Runtime]OBX.Command::'.ctor'(object, native int)");
        e->ldc_i4(1);
        e->call_("bool [OBX.Runtime]OBX.Runtime::pcall(class [OBX.Runtime]OBX.Command,bool)",2,true);
        e->pop_();
    }

    e->ret_();
    e->endMethod();
    e->endModule();
    return true;
}

static bool copyLib( const QDir& outDir, const QByteArray& name, QTextStream* cout )
{
    QFile f( QString(":/runtime/Dll/%1.dll" ).arg(name.constData() ) );
    if( !f.open(QIODevice::ReadOnly) )
    {
        qCritical() << "unknown lib" << name;
        return false;
    }
    QFile out( outDir.absoluteFilePath(name + ".dll") );
    if( !out.open(QIODevice::WriteOnly) )
    {
        qCritical() << "cannot open for writing" << out.fileName();
        return false;
    }
    out.write( f.readAll() );
    if( cout )
        *cout << "rm \"" << name << ".dll\"" << endl;
    return true;
}

bool CilGen::translateAll(Project* pro, How how, bool debug, const QString& where)
{
    Q_ASSERT( pro );
    if( where.isEmpty() )
    {
        qCritical() << "translateAll requires a path";
        return false;
    }
#if 0
    if( how == Pelib && debug )
        qWarning() << "cannot generate debug files with the Pelib option";
#endif

    QDir outDir(where);

    QByteArray buildStr;
    QTextStream bout(&buildStr);
    QByteArray clearStr;
    QTextStream cout(&clearStr);

#ifdef _MY_GENERICS_
    QList<Module*> mods = pro->getModulesToGenerate();
#else
    QList<Module*> mods = pro->getModulesToGenerate(true);
#endif
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
        }else if( m->d_isDef
#ifdef _OBX_USE_NEW_FFI_
                  && !m->d_externC
#endif
                  )
        {
            // NOP
        }else
        {
#ifdef _MY_GENERICS_
            if( m->d_metaParams.isEmpty() )
            {
                QList<Module*> result;
                m->findAllInstances(result);
                result.append(m);
                foreach( Module* inst, result )
                {
                    if( !generated.contains(inst) )
                    {
                        generated.insert(inst);
                        if( how == Ilasm || how == Fastasm || how == IlOnly )
                        {
                            QFile f(outDir.absoluteFilePath(inst->getName() + ".il"));
                            if( f.open(QIODevice::WriteOnly) )
                            {
                                //qDebug() << "generating IL for" << m->getName() << "to" << f.fileName();
                                IlAsmRenderer r(&f);
                                IlEmitter e(&r);
                                if( !CilGen::translate(inst,&e, debug, pro->getErrs()) )
                                {
                                    qCritical() << "error generating IL for" << inst->getName();
                                    return false;
                                }
                                if( how == Ilasm )
                                    bout << "./ilasm /dll " << ( debug ? "/debug ": "" ) << "\"" << inst->getName() << ".il\"" << endl;
                                else if( how == Fastasm )
                                    bout << "/dll " << ( debug ? "/debug ": "" ) << inst->getName() << ".il" << endl;
                                if( how == Ilasm )
                                {
                                    cout << "rm \"" << inst->getName() << ".il\"" << endl;
                                    cout << "rm \"" << inst->getName() << ".dll\"" << endl;
                                }else if( how == Fastasm )
                                    cout << inst->getName() << endl;
                            }else
                                qCritical() << "could not open for writing" << f.fileName();
                        }else
                        {
                            PelibGen r;
                            IlEmitter e(&r);
                            if( !CilGen::translate(inst,&e,debug,pro->getErrs()) )
                            {
                                qCritical() << "error generating assembly for" << inst->getName();
                                return false;
                            }
#if 0
                            // no longer used
                            r.writeAssembler(outDir.absoluteFilePath(inst->getName() + ".il").toUtf8());
                            cout << "rm \"" << inst->getName() << ".il\"" << endl;
#endif
                            r.writeByteCode(outDir.absoluteFilePath(inst->getName() + ".dll").toUtf8());
                            // cout << "rm \"" << inst->getName() << ".dll\"" << endl;
                            if( debug )
                            {
                                Mono::MdbGen mdb;
                                mdb.write( outDir.absoluteFilePath(inst->getName() + ".dll.mdb" ), r.getPelib(),
                                           QByteArrayList() << ".ctor" << "#copy" );
                            }
                        }
                    }
                }
            }
#else
            if( m->d_metaParams.isEmpty() || m->d_metaActuals.isEmpty() )
            {
                QFile f(outDir.absoluteFilePath(m->getName() + ".il"));
                if( f.open(QIODevice::WriteOnly) )
                {
                    qDebug() << "generating IL for" << m->getName() << "to" << f.fileName();
                    IlasmGen::translate(m,&f,pro->getErrs());
                    bout << "./ilasm /dll \"" << m->getName() << ".il\"" << endl;
                    cout << "rm \"" << m->getName() << ".il\"" << endl;
                    cout << "rm \"" << m->getName() << ".dll\"" << endl;
                }else
                    qCritical() << "could not open for writing" << f.fileName();
            }
#endif
        }
    }
    if( !mods.isEmpty() )
    {
        const QByteArray name = "Main#";
        QByteArrayList roots;
        for(int i = mods.size() - 1; i >= 0; i-- )
        {
            if( mods[i]->d_usedBy.isEmpty() )
                roots.append(mods[i]->getName());
        }
        if( roots.isEmpty() )
            roots.append(mods.last()->getName()); // shouldn't actually happenk
        if( how == Ilasm || how == Fastasm || how == IlOnly )
        {
            QFile f(outDir.absoluteFilePath(name + ".il"));
            if( f.open(QIODevice::WriteOnly) )
            {
                IlAsmRenderer r(&f);
                IlEmitter e(&r);
                const Project::ModProc& mp = pro->getMain();
                if( mp.first.isEmpty() )
                    CilGen::generateMain(&e,name,roots);
                else
                    CilGen::generateMain(&e, name,mp.first, mp.second);
                if( how == Ilasm )
                    bout << "./ilasm /exe " << ( debug ? "/debug ": "" ) << "\"" << name << ".il\"" << endl;
                else if( how == Fastasm )
                    bout << "/exe " << ( debug ? "/debug ": "" ) << name << ".il" << endl;
                if( how == Ilasm )
                {
                    cout << "rm \"" << name << ".il\"" << endl;
                    cout << "rm \"" << name << ".exe\"" << endl;
                }else if( how == Fastasm )
                    cout << name << endl;
            }else
                qCritical() << "could not open for writing" << f.fileName();
        }else
        {
            PelibGen r;
            IlEmitter e(&r);
            const Project::ModProc& mp = pro->getMain();
            if( mp.first.isEmpty() )
                CilGen::generateMain(&e,name,roots);
            else
                CilGen::generateMain(&e, name,mp.first, mp.second);

            //r.writeAssembler(outDir.absoluteFilePath(name + ".il").toUtf8());
            //cout << "rm \"" << name << ".il\"" << endl;
            r.writeByteCode(outDir.absoluteFilePath(name + ".exe").toUtf8());
            cout << "rm \"" << name << ".exe\"" << endl;
#if 0 // no source file
            if( debug )
            {
                MdbGen mdb;
                mdb.write( outDir.absoluteFilePath(name + ".exe.mdb" ), r.getPelib() );
            }
#endif
        }
        if( how != IlOnly )
        {
            QFile json(outDir.absoluteFilePath(name + ".runtimeconfig.json"));
            if( json.open(QIODevice::WriteOnly) )
            {
                cout << "rm \"" << name << ".runtimeconfig.json\"" << endl;
                json.write("{\n\"runtimeOptions\": {\n"
                           "\"framework\": {\n"
                           "\"name\": \"Microsoft.NETCore.App\",\n"
                           "\"version\": \"3.1.0\"\n" // TODO: replace version number depending on the used CoreCLR runtime version
                           "}}}");
            }else
                qCritical() << "could not open for writing" << json.fileName();
        }
    }

    if( how == Ilasm )
    {
        QFile run( outDir.absoluteFilePath("run.sh") );
        if( !run.open(QIODevice::WriteOnly) )
        {
            qCritical() << "could not open for writing" << run.fileName();
            return false;
        }else
        {
            run.write("export MONO_PATH=.\n");
            run.write("./mono Main#.exe\n");
        }
    }

    if( how != IlOnly )
    {
        const bool log = how == Ilasm;
        if( pro->useBuiltInOakwood() )
        {
            copyLib(outDir,"In",log?&cout:0);
            copyLib(outDir,"Out",log?&cout:0);
            copyLib(outDir,"Files",log?&cout:0);
            copyLib(outDir,"Input",log?&cout:0);
            copyLib(outDir,"Math",log?&cout:0);
            copyLib(outDir,"MathL",log?&cout:0);
            copyLib(outDir,"Strings",log?&cout:0);
            copyLib(outDir,"Coroutines",log?&cout:0);
            copyLib(outDir,"XYplane",log?&cout:0);
        }
        copyLib(outDir,"OBX.Runtime",log ? &cout : 0);
    }

    bout.flush();
    cout.flush();

    if( how == Ilasm || how == Fastasm )
    {
        QFile build( outDir.absoluteFilePath( how == Ilasm ? "build.sh" : "batch" ) );
        if( !build.open(QIODevice::WriteOnly) )
        {
            qCritical() << "could not open for writing" << build.fileName();
            return false;
        }else
            build.write(buildStr);
    }
    if( how == Ilasm || how == Fastasm )
    {
        QFile clear( outDir.absoluteFilePath( how == Ilasm ? "clean.sh" : "modules" ) );
        if( !clear.open(QIODevice::WriteOnly) )
        {
            qCritical() << "could not open for writing" << clear.fileName();
            return false;
        }else
            clear.write(clearStr);
    }
    const bool ok = pro->getErrs()->getErrCount() == errCount;
    return ok;
}

