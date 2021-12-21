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

#include "ObxCGen2.h"
#include "ObxAst.h"
#include "ObErrors.h"
#include "ObxProject.h"
#include <QtDebug>
#include <QFile>
#include <QDir>
#include <QCryptographicHash>
#include <QCoreApplication>
#include <QDateTime>
#include <QBuffer>
using namespace Obx;
using namespace Ob;

#ifndef OBX_AST_DECLARE_SET_METATYPE_IN_HEADER
Q_DECLARE_METATYPE( Obx::Literal::SET )
#endif

#define _OBX_FUNC_SEQ_POINT_
    /* The concept ($t1 = object)->func($t1) correctly works on all compilers and platforms tested so far,
     * also interleaved applications of the concept like ($t1 = object)->func($t1, ($t2 = object2)->func2($t2) )
     * properly work. I neither came across a situation since I wrote my first C program in the eighties where
     * this was suspected not to work. Unfortunately the unspecified evaluation order in the C99 standard includes the
     * function designator (not only the order of arguments). So theoretically the function argument $t1 could
     * be evaluated before the assignment $t1 = object. This define enables a version with an additional indirection
     * and comma expression like ($t2 = ($t1 = object)->func, $t2($t1)) so that there is an explicit sequence point
     * between the evaluation of the function designator and the evaluation of the arguments. The additional performance
     * cost is neglible, so we can just leave it enabled.
     */

struct ObxCGenCollector : public AstVisitor
{
    QList<Procedure*> allProcs;
    QList<Record*> allRecords;
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
                foreach( const Ref<Parameter>& p, pt->d_formals )
                    collect(p->d_type.data());
                if( pt->d_return )
                    collect(pt->d_return.data());
            }
            break;
        case Thing::T_QualiType:
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

struct ObxCGenImp : public AstVisitor
{
    Errors* err;
    Module* thisMod;
    int level;
    QTextStream h,b;
    QIODevice* park;
    QString body;
    bool ownsErr;
    bool debug; // generate line pragmas
    quint32 anonymousDeclNr; // starts with one, zero is an invalid slot
    Procedure* curProc;
    Named* curVarDecl;
    QSet<Record*> declToInline;
    Named* retypedRecIdent;

#ifdef _OBX_FUNC_SEQ_POINT_
    struct Temp
    {
        QByteArray d_type;
        ProcType* d_pt;
        bool d_inUse;
        Temp(const QByteArray& name = QByteArray(), ProcType* pt = 0, bool inUse = false ):d_type(name),d_pt(pt),d_inUse(inUse){}
    };
    QList<Temp> temps;
#else
    QList<QPair<QByteArray,bool> > temps;
#endif


    ObxCGenImp():err(0),thisMod(0),ownsErr(false),level(0),debug(false),anonymousDeclNr(1),
        curProc(0),curVarDecl(0),retypedRecIdent(0),park(0){}

    inline QByteArray ws() { return QByteArray(level*4,' '); }

#ifdef _OBX_FUNC_SEQ_POINT_
    int buyTemp( const QByteArray& type, ProcType* pt = 0 )
    {
        for( int i = 0; i < temps.size(); i++ )
        {
            if( temps[i].d_type == type && !temps[i].d_inUse )
            {
                temps[i].d_inUse = true;
                return i;
            }
        }
        temps.append(Temp(type,pt,true));
        return temps.size() - 1;
    }

    void sellTemp(int i)
    {
        Q_ASSERT( i >= 0 && i < temps.size() );
        temps[i].d_inUse = false;
    }
#else
    int buyTemp( const QByteArray& type )
    {
        for( int i = 0; i < temps.size(); i++ )
        {
            if( temps[i].first == type && !temps[i].second )
            {
                temps[i].second = true;
                return i;
            }
        }
        temps.append(qMakePair(type,true));
        return temps.size() - 1;
    }

    void sellTemp(int i)
    {
        Q_ASSERT( i >= 0 && i < temps.size() );
        temps[i].second = false;
    }
#endif

    void beginBody()
    {
        Q_ASSERT( b.device() );
        park = b.device();
        b.setDevice(0);
        body.clear();
        b.setString(&body);
        temps.clear();
    }

    void endBody()
    {
        Q_ASSERT( park );
        b.flush();
        for(int i = 0; i < temps.size(); i++ )
        {
#ifdef _OBX_FUNC_SEQ_POINT_
            if( temps[i].d_pt )
                park->write(ws() + formatProcType(temps[i].d_pt," $t" + QByteArray::number(i)) + ";\n" );
            else
                park->write(ws() + temps[i].d_type + " $t" + QByteArray::number(i) + ";\n" );
#else
            park->write(ws() + temps[i].first + " $t" + QByteArray::number(i) + ";\n" );
#endif
        }
        park->write(body.toUtf8());
        b.setDevice(park);
    }

    static QByteArray escape(const QByteArray& str)
    {
        static QSet<QByteArray> keywords;
        if( keywords.isEmpty() )
        {
            keywords << "auto" << "break" << "case" << "char" << "const" << "continue" << "default"
                     << "do" << "double" << "else" << "enum" << "extern" << "float" << "for" << "goto"
                     << "if" << "inline" << "int" << "long" << "register" << "restrict" << "return" << "short"
                     << "signed" << "sizeof" << "static" << "struct" << "switch" << "typedef" << "union"
                     << "unsigned" << "void" << "volatile" << "while" << "_Bool" << "_Complex" << "_Imaginary"

                     << "assert" << "main" << "fabs" << "fabsf" << "llabs" << "abs" << "floor" << "floorf"
                     << "exit";
        }
        if( keywords.contains(str) )
            return str + "_"; // avoid collision with C keywords
        else
            return str;
    }

    static void dedication(QTextStream& out)
    {
        out << "// Generated by " << qApp->applicationName() << " " << qApp->applicationVersion() << " on "
               << QDateTime::currentDateTime().toString(Qt::ISODate) << endl << endl;
    }

    static inline Type* derefed( Type* t )
    {
        if( t )
            return t->derefed();
        else
            return 0;
    }

    QByteArray dottedName( Named* n, bool withModule = true )
    {
        // concatenate names up to but not including module
        QByteArray name = n->d_name; // TODO: check escape is done by callers
        Named* scope = n->d_scope;
        if( n->getTag() == Thing::T_Procedure )
        {
            Procedure* proc = cast<Procedure*>(n);
            if( proc->d_receiverRec )
            {
                // if the scope is a bound proc follow its receiver, but first use the proc name.
                // this is necessary because procs bound to different recs can have the same name and
                // even have the same name as ordinary procs, so there is a risk of duplicate names when
                // just following the normal scope
                scope = proc->d_receiverRec->findDecl();
                Q_ASSERT( scope );
            }
        }
        if( scope )
        {
            const int tag = scope->getTag();
            if( tag != Thing::T_Module )
            {
                return dottedName(scope) + "$" + name;
            }else if( withModule )
                return moduleRef(cast<Module*>(scope)) + "$" + name;
        }
        return name;
    }

    static QByteArray moduleRef(Module* m, char separator = '$')
    {
        QByteArray name = m->d_fullName.join(separator);
        if( name.isEmpty() )
            name = m->d_name; // happens with built-in modules like Out
        if( !m->d_metaActuals.isEmpty() )
        {
            QCryptographicHash hash(QCryptographicHash::Md5);
            hash.addData(m->formatMetaActuals());
            name += separator + hash.result().toHex().left(10); // TODO
        }
        return escape(name);
    }

    static QByteArray fileName(Module* m)
    {
        return moduleRef(m,'.');
    }

    QByteArray classRef( Named* className )
    {
        Q_ASSERT( className && className->getTag() == Thing::T_NamedType );
        Module* m = className->getModule();
        return dottedName(className); // dotted because also records nested in procs are lifted to module level
    }

    QByteArray classRef( Type* rec )
    {
        return classRef(rec->toRecord());
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
            return moduleRef(m) + "$" + QByteArray::number(r->d_slot);
        }
    }

    static inline bool passByRef( Parameter* p )
    {
        return p->d_var;
    }

    QByteArray formatFormals( ProcType* pt, bool withName = true, Parameter* receiver = 0 )
    {
        QByteArray res = "(";
        if( pt->d_typeBound )
        {
            res += "void*" + ( withName && receiver ? " " + escape(receiver->d_name) : "" );
        }
        for( int i = 0; i < pt->d_formals.size(); i++ )
        {
            if( i != 0 || pt->d_typeBound )
                res += ", ";
            Type* t = pt->d_formals[i]->d_type.data();
            Type* td = derefed(t);
            Pointer p;
            if( passByRef(pt->d_formals[i].data()) || td->getTag() == Thing::T_Array )
            {
                // TODO: avoid passing by ref when IN with non-structured types
                t = &p;
                p.d_to = pt->d_formals[i]->d_type.data();
                p.d_decl = pt->d_formals[i].data();
                p.d_loc = pt->d_formals[i]->d_loc;
            }
            res += formatType( t, withName ? escape(pt->d_formals[i]->d_name) : "" );
        }
        if( pt->d_varargs )
        {
            if( !pt->d_formals.isEmpty() )
                res += ", ";
            res += "...";
        }
        res += ")";
        return res;
    }

    QByteArray formatReturn( ProcType* pt, const QByteArray& residue )
    {
        QByteArray res;
        if( pt->d_return.isNull() )
            res = formatType(0,residue);
        else
        {
            Type* t = pt->d_return.data();
            Type* td = derefed(t);
            Pointer p;
            if( td->getTag() == Thing::T_Array )
            {
                t = &p;
                p.d_to = pt->d_return.data();
                p.d_decl = pt->d_decl;
                p.d_loc = pt->d_return->d_loc;
            }
            res += formatType( t, residue );
        }
        return res;
    }

    inline static QByteArray formatBaseType(int t)
    {
        switch( t )
        {
        case Type::BYTE:
        case Type::BOOLEAN:
            return "uint8_t";
        case Type::CHAR:
            return "char";
        case Type::WCHAR:
            return "wchar_t";
        case Type::SHORTINT:
            return "int16_t";
        case Type::INTEGER:
            return "int32_t";
        case Type::LONGINT:
            return "int64_t";
        case Type::REAL:
            return "float";
        case Type::LONGREAL:
            return "double";
        case Type::SET:
            return "uint32_t";
        case Type::CVOID:
            return "void";
        default:
            return "?basetype?";
        }
    }

    QByteArray arrayType(int dims, const RowCol& loc)
    {
        switch( dims )
        {
        case 1:
            return "struct OBX$Array$1";
        case 2:
            return "struct OBX$Array$2";
        case 3:
            return "struct OBX$Array$3";
        case 4:
            return "struct OBX$Array$4";
        case 5:
            return "struct OBX$Array$5";
        default:
            err->error(Errors::Generator, Loc(loc,thisMod->d_file),
                       "C code generator cannot handle array dimensions > 5");
            return "???";
        }
    }

    QByteArray formatProcType( ProcType* pt, const QByteArray& name = QByteArray() )
    {
        return formatReturn(pt, "(*" + name + ")" + formatFormals(pt, false));
    }

    QByteArray formatType( Type* t, const QByteArray& name = QByteArray() )
    {
        if( t == 0 )
            return "void" + ( !name.isEmpty() ? " " + name : "" );
        switch(t->getTag())
        {
        case Thing::T_BaseType:
            return formatBaseType(t->getBaseType()) + ( !name.isEmpty() ? " " + name : "" );
        case Thing::T_Enumeration:
            return "int" + ( !name.isEmpty() ? " " + name : "" );
        case Thing::T_Array:
            /*
                arrays or pointer to arrays can be the types of fields, variables, locals and parameters.
                in general taking the address of an array is not supported, but with VAR/IN params an implicit
                address is handed into the procedure; it still cannot be converted to a pointer though.
                in contrast arrays can be dynamically created, even with a length not known at compile time.
                we need a way to transport the dim/size of an array with the pointer. it's inefficient to explicitly
                store these data with value arrays where it is known at compile time and invariant. with open
                arrays (or VAR/IN parameter) it is unavoidable though. If we add this information to the array object
                we obviously need two types of array objects, one with and one without dim/size information.
                A possible solution is to add dim/size information to the pointer/var param value. the dim number
                and type doens't change and is always known at compile time; so the pointer/var param could look
                e.g. like struct { uint32_t $0; void* $a; } or struct { uint32_t $0,$1; void* $a; } etc.
                if this information is in the array object instead we get a problem when passing either a value array
                (which has no size info) or a dereferenced array pointer (whose object has size info) is passed
                to a var parameter (the body of the proc has no clue which kind was passed).

                NOTE this approach has some disadvantages: a) a lot of anonymous objects on stack which might be
                difficult to follow by a M&S GC; b) parallel existence of structured (safe) array pointers and
                plain unsafe array pointers, c) complexity because of size transfer etc. requiring e.g. the $ trick.
            */
            {
                Array* a = cast<Array*>(t);
                if( a->d_type.isNull() )
                    return QByteArray(); // already reported

                QByteArray res;
                if( a->d_lenExpr.isNull() || name.isEmpty() )
                    res = "*" + ( !name.isEmpty() ? " " + name : "" );
                else
                {
                    res = name;
                    res += "[";
                    res += QByteArray::number(a->d_len);
                    res += "]";
                }
                return formatType( a->d_type.data(), res );
            }
            break;
        case Thing::T_Pointer:
            {
                Pointer* me = cast<Pointer*>(t);
                if( me->d_to.isNull() )
                    break;

                QByteArray res = "*" + ( !name.isEmpty() ? " " + name : "" );
                Type* td = derefed(me->d_to.data());
                if( td && td->getTag() == Thing::T_Array && !td->d_unsafe )
                {
                    Array* a = cast<Array*>(td);
                    int dims = 0;
                    a->getTypeDim(dims);
                    res = arrayType(dims, t->d_loc);
                    res += ( !name.isEmpty() ? " " + name : "" );
                    return res;
                }
                // else
                return formatType( me->d_to.data(), res );
            }
            break;
        case Thing::T_ProcType:
            {
                ProcType* pt = cast<ProcType*>(t);
                if( pt->d_typeBound )
                    return "struct OBX$Deleg" + ( !name.isEmpty() ? " " + name : "" );

                return formatProcType(pt,name);
                //return formatReturn(pt, "(*" + name + ")" + formatFormals(pt)); // fix for proc type returns
                //const QByteArray returnType = formatType(pt->d_return.data());
                //return returnType + " (*" + name + ")" + formatFormals(pt); // name can legally be empty
            }
            break;
        case Thing::T_QualiType:
            {
                QualiType* me = cast<QualiType*>(t);
#if 0
                Named* n = me->d_quali->getIdent();
                if( n )
                {
                    Module* m = n->getModule();
                    if( m )
                        return moduleRef(m) + "$" + dottedName(n);
                    else
                        return formatType(n->d_type.data());
                }else
                    return "???";
#else
                return formatType(me->d_quali->d_type.data(),name);
#endif
            }
            break;
        case Thing::T_Record:
            {
                // create a local dummy struct with the same memory layout as the original struct, but avoiding
                // the declaration order deadlock caused by mutual dependency of generic modules with the ones
                // where the instantiated types are declared
                Record* r = cast<Record*>(t);
                if( !thisMod->d_metaActuals.isEmpty() && curVarDecl && declToInline.contains(r) )
                {
                    QByteArray res = "struct /*" + classRef(r) + "*/ { ";
                    if( !r->d_unsafe )
                        res += "void* class$; ";
                    QList<Field*> fields = r->getOrderedFields();
                    foreach( Field* f, fields )
                        res += formatType(f->d_type.data(), escape(f->d_name) ) + "; ";
                    res += "}";
                    return res + ( !name.isEmpty() ? " " + name : "" );
                }else
                    return ( t->d_union ? "union " : "struct " ) +
                        classRef(r) + ( !name.isEmpty() ? " " + name : "" );
            }
            break;
        default:
            Q_ASSERT(false);
        }
        return "?type?" + ( !name.isEmpty() ? " " + name : "" );
    }

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

    void emitArrayInit( Array* a, const QByteArray& variable )
    {
        QList<Array*> dims = cast<Array*>(a)->getDims();
        Type* td = derefed(dims.last()->d_type.data());
        if( td->getTag() == Thing::T_Record && !td->d_unsafe )
        {
            // array of records by value; call init on each
            b << ws() << "for(int $i = 0; $i < (";
            for( int i = 0; i < dims.size(); i++ )
            {
                if( i != 0 )
                    b << "*";
                Q_ASSERT(!dims[i]->d_lenExpr.isNull() );
                b << dims[i]->d_len;
            }
            b << "); $i++) ";
            b << classRef(td) << "$init$(&((" << formatType(td,"*") << ")"
              << variable << ")[$i]);" << endl;
        }
    }

    void emitRecordDecl(Record* r)
    {
        if( r->d_slotAllocated )
            return;
        r->d_slotAllocated = true;

        const QByteArray className = classRef(r);
        h << ws() << "struct " << className << " {" << endl;
        level++;

        if( !r->d_unsafe )
            h << ws() << "struct " << className << "$Class$* class$;" << endl;

        QList<Field*> fields = r->getOrderedFields();

        foreach( Field* f, fields )
            f->accept(this);

        level--;
        h << "};" << endl << endl;
        if( !r->d_unsafe )
        {
            h << ws() << "extern void " << className << "$init$(struct " << className << "*);" << endl;
            b << ws() << "void " << className << "$init$(struct " << className << "* inst){" << endl;
            level++;
            b << ws() << "inst->class$ = &" << className << "$class$;" << endl;
            foreach( Field* f, fields )
            {
                Type* td = derefed(f->d_type.data());
                switch( td->getTag() )
                {
                case Thing::T_Record:
                    if( !td->d_unsafe )
                        b << ws() << classRef(td) << "$init$(&inst->" << escape(f->d_name) << ");" << endl;
                    break;
                case Thing::T_Array:
                    emitArrayInit( cast<Array*>(td), "inst->" + escape(f->d_name));
                    break;
                }
            }
            level--;
            b << ws() << "}" << endl;
        }
    }

    void emitClassDecl(Record* r)
    {
        if( r->d_unsafe )
            return;

        const QByteArray className = classRef(r);

        h << ws() << "struct " << className << "$Class$ {" << endl;
        b << ws() << "struct " << className << "$Class$ " << className << "$class$ = { " << endl;
        level++;

        // we need a valid type in any case, thus use this type if no base rec
        h << ws() << "struct " << classRef(r->d_baseRec ? r->d_baseRec : r ) << "$Class$* super$;" << endl;

        if( r->d_baseRec )
            b << ws() << "&" << classRef(r->d_baseRec) << "$class$," << endl;
        else
            b << ws() << "0," << endl;

        QList<Procedure*> mm = r->getOrderedMethods();
        foreach( Procedure* m, mm )
        {
            ProcType* pt = m->getProcType();

            QByteArray name = escape(m->d_name);
            name = "(*" + name + ")";
            name += formatFormals(pt,true,m->d_receiver.data());
            name = formatReturn(pt, name);
            h << ws() << name << ";" << endl;

            name = dottedName(m);
            b << ws() << name << "," << endl;
        }

        level--;
        b << "};" << endl << endl;
        h << "};" << endl;
        h << "struct " << className << "$Class$ " << className << "$class$;" << endl << endl;
    }

    void visit( Module* me)
    {
        const QByteArray moduleName = moduleRef(me);
        h << "#ifndef _" << moduleName.toUpper() << "_" << endl;
        h << "#define _" << moduleName.toUpper() << "_" << endl << endl;

        dedication(h);

        h << "#include \"OBX.Runtime.h\"" << endl;

        dedication(b);

        b << "#include \"" << fileName(thisMod) << ".h\"" << endl;

        ObxCGenCollector co;
        me->accept(&co);

        foreach( Import* imp, me->d_imports )
        {
            if(imp->d_mod->d_synthetic )
                continue; // ignore SYSTEM
            h << "#include \"" << fileName(imp->d_mod.data()) << ".h\"" << endl;
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
        h << endl;
        h << "// Declaration of module " << me->getName() << endl << endl;

        if( !me->d_metaActuals.isEmpty() )
        {
            QSet<Module*> imports;
            for( int i = 0; i < me->d_metaActuals.size(); i++ )
            {
                Q_ASSERT( me->d_metaActuals[i]->getTag() == Thing::T_QualiType );
                QualiType* t = cast<QualiType*>(me->d_metaActuals[i].data());
                Named* n = t->d_quali->getIdent();
                Q_ASSERT(n);
                Module* m = n->getModule();
                if( m ) // 0 in case of e.g. INTEGER
                    imports.insert(m);
                Type* td = t->derefed();
                if( td->getTag() == Thing::T_Record )
                {
                    h << formatType(t) << ";" << endl;
                    declToInline.insert(cast<Record*>(td));
                }
            }
            foreach(Module* m, imports)
                b << "#include \"" << fileName(m) << ".h\"" << endl;
        }
        b << endl;

        if( !Record::calcDependencyOrder(co.allRecords).isEmpty() )
            err->error(Errors::Generator, thisMod->d_file, 1,1, // shouldn't acutally happen since caught by validator
                         "circular record by value dependencies are not supported by C");

        foreach( Record* r, co.allRecords )
            allocRecordDecl(r);

        for( int i = 0; i < me->d_metaActuals.size(); i++ )
        {
            Type* td = derefed(me->d_metaActuals[i].data());
            Q_ASSERT(td);
            if( Record* r = td->toRecord() )
                h << ws() << "struct " << classRef(r) << "; // meta actual" << endl;
        }

        foreach( Record* r, co.allRecords )
        {
            // forward decls
            const QByteArray className = classRef(r);
            h << ws() << "struct " << className << "$Class$;" << endl;
            h << ws() << "struct " << className << ";" << endl;
        }

        foreach( Record* r, co.allRecords )
            emitRecordDecl(r);

        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Variable )
                n->accept(this);
        }

        foreach( Procedure* p, co.allProcs )
            p->accept(this);

        foreach( Record* r, co.allRecords )
            emitClassDecl(r);

        h << "void " << moduleName << "$init$(void);" << endl;
        b << "static int initDone$ = 0;" << endl;
        b << "void " << moduleName << "$init$(void) {" << endl;

        level++;
        beginBody();
        b << ws() << "if(initDone$) return; else initDone$ = 1;" << endl;
        foreach( Import* imp, me->d_imports )
        {
            if(imp->d_mod->d_synthetic )
                continue; // ignore SYSTEM
            b << ws() << moduleRef(imp->d_mod.data())  + "$init$();" << endl;
        }
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Variable )
            {
                int f = isStructuredOrArrayPointer(n->d_type.data() );
                if( f )
                {
                    const QByteArray name = moduleRef(thisMod)+"$"+n->d_name;
                    b << ws() << "memset(&" << name << ",0,sizeof(" << name << "));" << endl;
                    if( f == IsRecord )
                        b << ws() << classRef(n->d_type.data()) << "$init$(&" << name << ");" << endl;
                    else if( f == IsArray )
                        emitArrayInit( cast<Array*>(n->d_type->derefed()), name );
                }
            }
        }
        //b << ws() << "void* $;" << endl;

        foreach( const Ref<Statement>& s, me->d_body )
        {
            emitStatement(s.data());
        }

        endBody();
        level--;

        b << "}" << endl;

        h << "OBX$Cmd " << moduleName << "$cmd$(const char* name);" << endl;
        b << "OBX$Cmd " << moduleName << "$cmd$(const char* name) {" << endl;
        level++;
        b << ws() << "if( name == 0 ) return " << moduleName << "$init$;" << endl;
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Procedure )
            {
                Procedure* p = cast<Procedure*>(n.data());
                ProcType* pt = p->getProcType();
                if( p->d_receiver.isNull() && pt->d_return.isNull() && pt->d_formals.isEmpty() )
                    b << ws() << "if( strcmp(name,\"" << n->d_name <<
                         "\") == 0 ) return " << moduleName << "$" << escape(n->d_name) << ";" << endl;
            }
        }
        b << ws() << "return 0;" << endl;
        level--;
        b << "}" << endl;

        h << "#endif" << endl;
    }

    void visit( Field* me)
    {
        curVarDecl = me;
        h << ws() << formatType(me->d_type.data(), escape(me->d_name) ) << ";" << endl;
        curVarDecl = 0;
    }

    enum { NotStructured = 0, IsArrayPointer, IsRecord, IsArray, IsDeleg };
    int isStructuredOrArrayPointer(Type* t)
    {
        Type* td = derefed(t);
        if( td == 0 )
            return 0;
        const int tag = td->getTag();
        if( tag == Thing::T_ProcType && td->d_typeBound )
            return IsDeleg;
        if( td->isStructured() )
        {
            if( tag == Thing::T_Record )
                return IsRecord;
            else
                return IsArray;
        }
        if( tag == Thing::T_Pointer )
        {
            Pointer* p = cast<Pointer*>(td);
            td = derefed(p->d_to.data());
            if( td && td->getTag() == Thing::T_Array )
                return IsArrayPointer;
        }
        //else
        return 0;
    }

    void visit( Variable* me )
    {
        curVarDecl = me;
        h << ws() << "extern " << formatType(me->d_type.data(), moduleRef(thisMod)+"$"+me->d_name ) << ";" << endl;
        b << ws() << formatType(me->d_type.data(), moduleRef(thisMod)+"$"+me->d_name );
        if( !isStructuredOrArrayPointer(me->d_type.data()) )
            b << " = 0";
        b << ";" << endl;
        curVarDecl = 0;
    }

    void emitStatement(Statement* s)
    {
        s->accept(this);
    }

    void visit( Procedure* me)
    {
        curProc = me;

        ProcType* pt = me->getProcType();
        QByteArray name = dottedName(me);
        name += formatFormals(pt,true,me->d_receiver.data());
        name = formatReturn(pt, name);

        h << name << ";" << endl;

        b << name << " {" << endl;
        level++;

        // declaration
        foreach( const Ref<Named>& n, me->d_order )
        {
            switch( n->getTag() )
            {
            case Thing::T_LocalVar:
                {
                    curVarDecl = n.data();
                    b << ws() << formatType( n->d_type.data(), escape(n->d_name) ) << ";" << endl;
                    curVarDecl = 0;
                }
                break;
            case Thing::T_Parameter:
                {
                    Parameter* p = cast<Parameter*>(n.data());
                    if( p->d_receiver )
                        b << ws() << formatType(me->d_receiverRec) << "* this$ = " <<
                             escape(p->d_name) << ";" << endl;
                }
                break;
            }
        }

        beginBody();

        // initializer
        foreach( const Ref<Named>& n, me->d_order )
        {
            switch( n->getTag() )
            {
            case Thing::T_LocalVar:
                {
                    const int f = isStructuredOrArrayPointer(n->d_type.data() );
                    if( !f )
                        b << ws() << escape(n->d_name) << " = 0;" << endl;
                    if( f )
                        b << ws() << "memset(&" << escape(n->d_name) << ",0,sizeof(" << escape(n->d_name) << "));" << endl;
                    if( f == IsRecord )
                        b << ws() << classRef(n->d_type.data()) << "$init$(&" << escape(n->d_name) << ");" << endl;
                    else if( f == IsArray )
                        emitArrayInit( cast<Array*>(n->d_type->derefed()), escape(n->d_name) );
                    }
                break;
            case Thing::T_Parameter:
                {
                    Parameter* p = cast<Parameter*>(n.data());
                    Type* td = derefed(p->d_type.data());
                    if( !p->d_var && td->getTag() == Thing::T_Array )
                    {
                        // array passed by value; make a copy of it
                        Array* a = cast<Array*>(td);
                        QList<Array*> dims = a->getDims();
                        b << ws() << escape(p->d_name) << ".$a = OBX$Copy(" <<  escape(p->d_name) << ".$a,";
                        for(int i = 0; i < dims.size(); i++ )
                        {
                            if( i != 0 )
                                b << " * ";
                            b << escape(p->d_name) << ".$" << (i+1);
                        }
                        b << " * sizeof(" << formatType(dims.last()->d_type.data()) << "));" << endl;
                        b << ws() << escape(p->d_name) << ".$s = 0;" << endl;
                    }
                }
                break;
            }
        }

        //b << ws() << "void* $;" << endl;

        foreach( const Ref<Statement>& s, me->d_body )
        {
            emitStatement(s.data());
        }
        if( !pt->d_return.isNull() && ( me->d_body.isEmpty() || me->d_body.last()->getTag() != Thing::T_Return ) )
        {
            b << ws() << "return ";
            emitDefault(pt->d_return.data(),me->d_end);
            b << ";" << endl;
        }

        endBody();

        level--;
        b << "}" << endl << endl;
        curProc = 0;
    }

#if 0
    QVector<uint> toNumbers(const QString& in)
    {
        QVector<uint> res(in.size());
        for( int i = 0; i < in.size(); i++ )
            res[i] = in[i].unicode();
        return res;
    }
#endif

    void emitConst(quint8 basetype, const QVariant& val, const RowCol& loc )
    {
        switch( basetype )
        {
        case Type::BOOLEAN:
        case Type::SHORTINT:
        case Type::INTEGER:
        case Type::BYTE:
            b << val.toInt();
            break;
        case Type::LONGINT:
            b << val.toLongLong();
            break;
        case Type::ENUMINT:
            b << val.toUInt();
            break;
        case Type::REAL:
            b << QByteArray::number(val.toDouble(),'e',7);
            break;
        case Type::LONGREAL:
            b << QByteArray::number(val.toDouble(),'e',16); // empirically optimized, 17 is too much
            break;
        case Type::NIL:
            b << "0";
            break;
        case Type::STRING:
        case Type::WSTRING:
            {
                QString str = val.toString();
                const int len = str.length();
                str.replace('\\', "\\\\");
                str.replace("\"","\\\"");
                // TEST qDebug() << "Literal:" << str << toNumbers(str);
                // no L prefix, we want UTF-8 in any case
                b << "(const struct OBX$Array$1){";
                b << len+1 << ",0,";
                b << "OBX$FromUtf(\"" << str << "\"," << len+1 << ","
                  << int(basetype==Type::WSTRING) << ")";
                b << "}";
            }
            break;
        case Type::BYTEARRAY:
            {
                const QByteArray ba = val.toByteArray();
                b << "(const struct OBX$Array$1){";
                b << ba.length() << ",1,&(const uint8_t[]){";
                for( int i = 0; i < ba.size(); i++ )
                    b << "0x" << QByteArray::number(quint8(ba[i]),16) << ", ";
                b << "}}";
            }
            break;
        case Type::CHAR:
        case Type::WCHAR:
            b << "0x" << QByteArray::number(val.toUInt(),16);
            break;
        case Type::SET:
            {
                Literal::SET s = val.value<Literal::SET>();
                b << "0x" << QByteArray::number((quint32)s.to_ulong(),16);
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

    void visit( IdentLeaf* me)
    {
        Named* id = me->getIdent();
        if( id == 0 )
            return; // already reported


        const bool doCast = hasGenericRecType(id) || id == retypedRecIdent;
        if( doCast )
        {
            Type* td = derefed(me->d_type.data() );
            Q_ASSERT(td);
            const int tag = td->getTag();
            Q_ASSERT( tag != Thing::T_Array && tag != Thing::T_ProcType );
            b << "(";
            if( true ) //tag != Thing::T_Pointer )
                b << "*";
            b << "(" << formatType(me->d_type.data(),true /* tag != Thing::T_Pointer */ ? "*" : "" ) << ")";
            if( true ) //tag != Thing::T_Pointer )
                b << "&";
            // NOTE: just do brute force *& regardless whether pointer or not, because C has a strange feature
            // (*(struct T5Statements$RectangleDesc * *)&sp) = (*(struct T5Statements$RectangleDesc * *)&sp); // works
            // ((struct T5Statements$RectangleDesc * )sp) = ((struct T5Statements$RectangleDesc  *)sp); // error: lvalue required as left operand of assignment
        }

        switch( id->getTag() )
        {
        case Thing::T_Const:
            {
                Type* td = derefed(me->d_type.data() );
                Q_ASSERT(td);
                Q_ASSERT( td->getTag() == Thing::T_BaseType || td->getTag() == Thing::T_Enumeration );
                emitConst( td->getBaseType(), cast<Const*>(id)->d_val, me->d_loc );
            }
            break;
        case Thing::T_Import:
            // NOP
            break;
        case Thing::T_Variable:
            b << dottedName(id);
            break;
        case Thing::T_LocalVar:
            b << escape(id->d_name);
            break;
        case Thing::T_Parameter:
            {
                // a VAR/IN parameter is implemented as a pointer.
                // an array passed by value is also represented by a pointer
                // record pointers are plain C pointers
                // array pointers are OBX$Array$x structs

                // NOTE: an array is always represented by a OBX$Array$x struct in memory unless it is
                // a variable, local var or field by value; an array parameter is always a OBX$Array$x struct
                // or a pointer to an OBX$Array$x struct (if param is a var to a pointer);
                // so when the type is an array value it is an OBX$Array$x struct if the ident is a parameter
                // or a dereferenced pointer, and a true value otherwise.

                Parameter* p = cast<Parameter*>(id);
                Type* td = derefed(me->d_type.data() );

                // we dereference here in case of VAR/IN so the succeeding desigs can assume a value based on d_type
                // without checking the prefix desig. But we don't do it for array values.
                const bool doDeref = p->d_var
                        && td->getTag() != Thing::T_Array;
                if( doDeref )
                    b << "(*";
                if( p->d_receiver )
                    b << "this$";
                else
                    b << escape(id->d_name);
                if( doDeref )
                    b << ")";
            }
            break;
        case Thing::T_NamedType:
            // NOP
            break;
        case Thing::T_Procedure:
            b << dottedName(id);
            break;
        case Thing::T_BuiltIn:
            // NOP
            break;
        default:
            qDebug() << "ERR" << id->getTag() << thisMod->d_name << me->d_loc.d_row << me->d_loc.d_col;
            Q_ASSERT( false );
            break;
        }
        if( doCast )
            b << ")";
    }

    static inline bool hasGenericRecType(Named* id)
    {
        Q_ASSERT(id);
        Type* t = id->d_type.data(); // id can be anything, including import
        if( t && t->getTag() == Thing::T_QualiType )
        {
            QualiType* qt = cast<QualiType*>(t);
            Named* n = qt->d_quali->getIdent();
            Type* td = derefed(t);
            return n && n->getTag() == Thing::T_GenericName && td->getTag() == Thing::T_Record;
        }
        return false;
    }

    void visit( IdentSel* me)
    {
        Q_ASSERT( !me->d_sub.isNull() );

        Named* subId = me->d_sub->getIdent();
        const bool derefImport = subId && subId->getTag() == Thing::T_Import;

        Named* id = me->getIdent();
        Q_ASSERT( id );

        const bool doCast = hasGenericRecType(id) || id == retypedRecIdent;
        if( doCast )
        {
            Type* td = derefed(me->d_type.data() );
            Q_ASSERT(td);
            const int tag = td->getTag();
            Q_ASSERT( tag != Thing::T_Array && tag != Thing::T_ProcType );
            b << "(";
            b << "*"; // see the doCast section in visit(IdenLeaf) for more information
            b << "(" << formatType(me->d_type.data(),"*" ) << ")";
            b << "&";
        }

        me->d_sub->accept(this);

        switch( id->getTag() )
        {
        case Thing::T_BuiltIn:
            // NOP
            break;
        case Thing::T_Procedure:
            {
                Procedure* p = cast<Procedure*>(id);
                if( !p->d_receiver.isNull() )
                {
                    // a bound proc must be prefixed with a designator
                    b << "."; // no "->" because in case of a pointer there is always a DEREF
                    b << "class$->" << escape(p->d_name);
                }else
                {
                    Q_ASSERT( derefImport );
                    b << dottedName(id);
                }
            }
            break;
        case Thing::T_Variable:
            Q_ASSERT( derefImport );
            // no dot here because the module where the variable lives is part of the dotted name
            b << dottedName(id);
            break;
        case Thing::T_Field:
            b << "."; // no "->" because in case of a pointer there is always a DEREF
            b << escape(id->d_name);
            break;
        case Thing::T_NamedType:
            // NOP
            break;
        case Thing::T_Const:
            {
                Type* td = derefed(id->d_type.data() );
                Q_ASSERT(td);
                Q_ASSERT( derefImport );
                Q_ASSERT( td->getTag() == Thing::T_BaseType || td->getTag() == Thing::T_Enumeration );
                emitConst( td->getBaseType(), cast<Const*>(id)->d_val, me->d_loc );
            }
            break;
        default:
            qDebug() << "ERR" << thisMod->d_name << id->getTag() << me->d_loc.d_row << me->d_loc.d_col;
            Q_ASSERT( false );
            break;
        }
        if( doCast )
            b << ")";
    }

    void visit( UnExpr* me)
    {
        Q_ASSERT( !me->d_sub.isNull() );

        Type* prevT = derefed(me->d_sub->d_type.data());
        Q_ASSERT( prevT );
        Type* thisT = derefed(me->d_type.data());
        Q_ASSERT( thisT );

        switch( me->d_op )
        {
        case UnExpr::NEG:
            if( prevT->getBaseType() == BaseType::SET )
                b << "(~";
            else
                b << "(-";
            me->d_sub->accept(this);
            b << ")";
            break;
        case UnExpr::NOT:
            b << "(!";
            me->d_sub->accept(this);
            b << ")";
            break;
        case UnExpr::DEREF:
            {
                const bool doDeref = prevT->getTag() == Thing::T_Pointer && // only deref pointers, not e.g. supercalls
                        thisT->getTag() != Thing::T_Array; // don't deref arrays
                if( doDeref )
                    b << "(*";
                me->d_sub->accept(this);
                if( doDeref )
                    b << ")";
            }
            break;
        case UnExpr::ADDROF:
            b << "(&";
            me->d_sub->accept(this);
            b << ")";
            break;
        default:
            qDebug() << "ERR" << me->d_op << thisMod->d_name << me->d_loc.d_row << me->d_loc.d_col;
            Q_ASSERT( false );
            break;
        }
    }

    void emitDefault( Type* t, const RowCol& loc )
    {
        Type* td = derefed(t);
        Q_ASSERT( td );
        switch( td->getTag() )
        {
        case Thing::T_Record:
            qDebug() << "DEFAULT(record)" << thisMod->getName();
            if( td->d_unsafe )
                b << "(" << formatType(td) << "){}"; // partial init sets all members to zero
            else
                b << "(" << formatType(td) << "){ &" << classRef(td) << "$class$ }";
            break;
        case Thing::T_Array:
            {
                Array* a = cast<Array*>(td);
                QList<Array*> dims = a->getDims();
                td = derefed(dims.last()->d_type.data());
                if( dims.size() == 1 && td->isChar() )
                {
                    if( td->getBaseType() == Type::CHAR )
                        b << "(struct OBX$Array$1){1,1,&(char[1]){0}}";
                    else
                        b << "(struct OBX$Array$1){1,1,&(wchar_t[1]){0}}";
                }else
                {
                    b << "(" << arrayType(dims.size(), loc) << "){";
                    for( int i = 0; i < dims.size(); i++ )
                    {
                        Q_ASSERT(!dims[i]->d_lenExpr.isNull());
                        b << dims[i]->d_len;
                        b << ",";
                    }
                    b << "1,&(";
                    b << formatType(dims.last()->d_type.data());
                    for( int i = 0; i < dims.size(); i++ )
                    {
                        b << "[" << dims[i]->d_len << "]";
                    }
                    b << "){}}";
                }
            }
            break;
        case Thing::T_Pointer:
            {
                td = derefed(cast<Pointer*>(td)->d_to.data());
                Q_ASSERT(td);
                if( td->getTag() == Thing::T_Array )
                {
                    int dims;
                    cast<Array*>(td)->getTypeDim(dims);
                    b << "(" << arrayType(dims, loc) << "){}";
                }else
                    b << "0";
            }
            break;
        case Thing::T_BaseType:
        case Thing::T_Enumeration:
        case Thing::T_ProcType:
            b << "0";
            break;
        default:
            Q_ASSERT(false);
        }
    }

    void emitBuiltIn( BuiltIn* bi, ArgExpr* ae )
    {
        switch( bi->d_func )
        {
        case BuiltIn::LDMOD:
            {
                Q_ASSERT( ae->d_args.size() == 1 );
                b << "OBX$LoadModule((const char*)";
                ae->d_args.first()->accept(this);
                b << ".$a)";
            }
            break;
        case BuiltIn::LDCMD:
            {
                Q_ASSERT( ae->d_args.size() == 2 );
                b << "OBX$LoadCmd((const char*)";
                ae->d_args.first()->accept(this);
                b << ".$a,(const char*)";
                ae->d_args.last()->accept(this);
                b << ".$a)";
            }
            break;
        case BuiltIn::ROR:
            Q_ASSERT( ae->d_args.size() == 2 );
            b << "(((";
            switch( ae->d_args.first()->d_type->derefed()->getBaseType() )
            {
            case Type::LONGINT:
                b << "uint64_t";
                break;
            case Type::INTEGER:
            case Type::SET:
            case Type::ENUMINT:
            default:
                b << "uint32_t";
                break;
            case Type::SHORTINT:
                b << "uint16_t";
                break;
            case Type::BYTE:
                b << "uint8_t";
                break;
            }
            b << ")";
            ae->d_args.first()->accept(this);
            b << ") >> ";
            ae->d_args.last()->accept(this);
            b << ")";
            break;
        case BuiltIn::ASR:
            Q_ASSERT( ae->d_args.size() == 2 );
            switch( ae->d_args.first()->d_type->derefed()->getBaseType() )
            {
            case Type::LONGINT:
                b << "OBX$Asr64(";
                break;
            case Type::SET:
            case Type::ENUMINT:
            case Type::INTEGER:
            default:
                b << "OBX$Asr32(";
                break;
            case Type::SHORTINT:
                b << "OBX$Asr16(";
                break;
            }
            ae->d_args.first()->accept(this);
            b << ", ";
            ae->d_args.last()->accept(this);
            b << ")";
            break;
        case BuiltIn::LSL:
            Q_ASSERT( ae->d_args.size() == 2 );
            b << "(";
            ae->d_args.first()->accept(this);
            b << " << ";
            ae->d_args.last()->accept(this);
            b << ")";
            break;
        case BuiltIn::ORD:
            {
                Q_ASSERT( ae->d_args.size() == 1 );
                Type* td = derefed(ae->d_args.first()->d_type.data());
                if( td->isString() )
                {
                    if( td->getBaseType() == Type::STRING )
                        b << "((uint8_t)";
                    else
                        b << "((wchar_t)";
                    BaseType bt(td->getBaseType() == Type::STRING ? Type::CHAR : Type::WCHAR );
                    renderArg(&bt, ae->d_args.first().data(), false);
                    b << ")";
                }else if( td->getBaseType() == Type::CHAR )
                {
                    b << "((uint8_t)";
                    ae->d_args.first()->accept(this);
                    b << ")";
                }else
                    ae->d_args.first()->accept(this);
            }
            break;
        case BuiltIn::FLOOR:
            Q_ASSERT( ae->d_args.size() == 1 );
            b << "floorf(";
            ae->d_args.first()->accept(this);
            b << ")";
            break;
        case BuiltIn::ENTIER:
            Q_ASSERT( ae->d_args.size() == 1 );
            b << "floor(";
            ae->d_args.first()->accept(this);
            b << ")";
            break;
        case BuiltIn::ABS:
            {
                Q_ASSERT( ae->d_args.size() == 1 );
                Type* t = derefed(ae->d_args.first()->d_type.data());
                Q_ASSERT( t );
                switch(t->getBaseType())
                {
                case Type::LONGREAL:
                    b << "fabs(";
                    break;
                case Type::REAL:
                    b << "fabsf(";
                    break;
                case Type::LONGINT:
                    b << "llabs(";
                    break;
                case Type::SHORTINT:
                case Type::INTEGER:
                case Type::BYTE:
                    b << "abs(";
                    break;
                default:
                    Q_ASSERT(false);
                }
                ae->d_args.first()->accept(this);
                b << ")";
            }
            break;
        case BuiltIn::DEFAULT:
            Q_ASSERT( !ae->d_args.isEmpty() );
            emitDefault(ae->d_args.first()->d_type.data(),ae->d_args.first()->d_loc);
            break;
        case BuiltIn::BITAND:
            Q_ASSERT( ae->d_args.size() == 2 );
            b << "(";
            ae->d_args.first()->accept(this);
            b << " & ";
            ae->d_args.last()->accept(this);
            b << ")";
            break;
        case BuiltIn::BITOR:
            Q_ASSERT( ae->d_args.size() == 2 );
            b << "(";
            ae->d_args.first()->accept(this);
            b << " | ";
            ae->d_args.last()->accept(this);
            b << ")";
            break;
        case BuiltIn::BITXOR:
            Q_ASSERT( ae->d_args.size() == 2 );
            b << "(";
            ae->d_args.first()->accept(this);
            b << " ^ ";
            ae->d_args.last()->accept(this);
            b << ")";
            break;
        case BuiltIn::BITNOT:
            Q_ASSERT( ae->d_args.size() == 1 );
            b << "~";
            ae->d_args.first()->accept(this);
            break;
        case BuiltIn::HALT:
            Q_ASSERT( ae->d_args.size() == 1 );
            b << "exit(";
            ae->d_args.first()->accept(this);
            b << ")";
            break;
        case BuiltIn::FLT:
            {
                Q_ASSERT( ae->d_args.size() == 1 );
                ae->d_args.first()->accept(this);
            }
            break;
        case BuiltIn::PACK:
            {
                Q_ASSERT( ae->d_args.size() == 2 );

                b << "OBX$Pack32(";
                renderArg2(ae->d_args.first()->d_type.data(), ae->d_args.first().data(),true);
                b << ",";
                ae->d_args.last()->accept(this);
                b << ")";
           }
            break;
        case BuiltIn::UNPK:
            {
                Q_ASSERT( ae->d_args.size() == 2 );

                b << "OBX$Unpack32(";
                renderArg2(ae->d_args.first()->d_type.data(), ae->d_args.first().data(),true);
                b << ",";
                renderArg2(ae->d_args.last()->d_type.data(), ae->d_args.last().data(),true);
                b << ")";
             }
            break;
        case BuiltIn::ASSERT:
            {
                Q_ASSERT( !ae->d_args.isEmpty() ); // TODO: by now second optional arg ignored!
                b << "assert(";
                ae->d_args.first()->accept(this);
                b << ")";
            }
            break;
        case BuiltIn::PRINTLN:
            {
                Q_ASSERT( ae->d_args.size() == 1 );
                Type* td = derefed(ae->d_args.first()->d_type.data());
                QByteArray format;
                bool wide = false;
                bool deref = false;
                bool addr = false;
                bool strcast = false;
                if( td->isText(&wide) )
                {
                    if( td->isChar() )
                    {
                        if( wide )
                            format = "\"%lc\\n\"";
                        else
                            format = "\"%c\\n\"";
                    }else
                    {
                        if( wide )
                            format = "\"%ls\\n\"";
                        else
                            format = "\"%s\\n\"";
                        deref = true;
                        strcast = true;
                    }
                }else if( td->isInteger() )
                {
                    if( td->getBaseType() <= Type::INTEGER )
                        format = "\"%d\\n\"";
                    else
                        format = "\"%lld\\n\"";
                }else if( td->isReal() )
                {
                    if( td->getBaseType() == Type::LONGREAL )
                        format = "\"%1.16e\\n\"";
                    else
                        format = "\"%1.7e\\n\"";
                }else if( td->isSet() )
                    format = "\"%x\\n\"";
                else if( td->getBaseType() == Type::BOOLEAN )
                    format = "\"%d\\n\"";
                else
                {
                    switch(td->getTag())
                    {
                    case Thing::T_Enumeration:
                        format = "\"%u\\n\"";
                        break;
                    case Thing::T_Pointer:
                        if( cast<Pointer*>(td)->d_to->derefed()->getTag() == Thing::T_Array )
                            deref = true;
                        format = "\"%p\\n\"";
                        break;
                    case Thing::T_Array:
                        deref = true;
                        format = "\"%p\\n\"";
                        break;
                    case Thing::T_Record:
                        addr = true;
                        format = "\"%p\\n\"";
                        break;
                    default:
                        format = "\"%p\\n\"";
                        break;
                    }
                }
                if( strcast && !wide )
                    b << "OBX$PrintA(1";
                else
                {
                    b << "printf(";
                    b << format;
                }
                b << ",";
                if( addr )
                    b << "&";
                if( strcast )
                {
                    if( wide )
                        b << "(const wchar_t*)";
                    else
                        b << "(const char*)";
                }
                if( deref )
                    b << "(";
                renderArg(td, ae->d_args.first().data(),false);
                if( deref )
                    b << ").$a";
                b << ")";
            }
            break;
        case BuiltIn::INC:
        case BuiltIn::DEC:
            if( ae->d_args.size() == 1 )
            {
                ae->d_args.first()->accept(this);
                if( bi->d_func == BuiltIn::INC )
                    b << "++";
                else
                    b << "--";

            }else
            {
                Q_ASSERT( ae->d_args.size() == 2 );
                ae->d_args.first()->accept(this);
                if( bi->d_func == BuiltIn::INC )
                    b << "+=";
                else
                    b << "-=";
                ae->d_args.last()->accept(this);
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
                    Q_ASSERT(ae->d_args.first()->getTag() == Thing::T_Literal);
                    Literal* l = cast<Literal*>(ae->d_args.first().data());
                    b << "( " << l->d_strLen << " + 1 )";
                }else
                {
                    Q_ASSERT( t->getTag() == Thing::T_Array );
                    Array* a = cast<Array*>(t);
                    if( !a->d_lenExpr.isNull() )
                        b << a->d_len;
                    else
                    {
                        b << "(";
                        renderArg(0, ae->d_args.first().data(),false);
                        b << ").$1";
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
                td = derefed(cast<Pointer*>(td)->d_to.data());
                if( td->getTag() == Thing::T_Array )
                {
                    QList<Array*> dims = cast<Array*>(td)->getDims();
                    td = derefed(dims.last()->d_type.data());
                    int d = 0;
                    b << "{int ";
                    for( int i = 0; i < dims.size(); i++ )
                    {
                        if( dims[i]->d_lenExpr.isNull() )
                            break;
                        b << "$" << d << " = " << dims[i]->d_len << ", ";
                        d++;
                    }
                    for( int i = 1; i < ae->d_args.size(); i++ )
                    {
                        b << "$" << d << " = ";
                        ae->d_args[i]->accept(this);
                        b << ", ";
                        d++;
                    }
                    b << "$n = ";
                    for( int i = 0; i < d; i++ )
                    {
                        if( i != 0 )
                            b << " * ";
                        b << "$" << i;
                    }
                    b << ", $s=" << "sizeof(" << formatType(td) << "); ";
                    const int temp = buyTemp(formatType(td,"*"));
                    b << "$t" << temp << " = OBX$Alloc($s*$n); ";
                    b << "memset($t" << temp << ",0,$s*$n); ";
                    renderArg(0,ae->d_args.first().data(),false);
                    b << " = ";
                    b << "(" << arrayType(dims.size(), ae->d_loc) << "){";
                    for( int i = 0; i < d; i++ )
                    {
                        b << "$" << i << ", ";
                    }
                    b << "1, $t" << temp << "};";
                    if( td->getTag() == Thing::T_Record && !td->d_unsafe )
                    {
                        // like emitArrayInit
                        b << "for(int $i = 0; $i < $n; $i++) ";
                        b << classRef(td) << "$init$(&$t" << temp << "[$i]); ";
                    }
                    sellTemp(temp);
                    b << "}";
                }else
                {
                    Q_ASSERT( td->getTag() == Thing::T_Record );
                    Q_ASSERT( ae->d_args.size() == 1 );
                    const int temp = buyTemp(formatType(td,"*"));
                    b << "$t" << temp << " = OBX$Alloc(sizeof(" << formatType(td) << "));" << endl;
                    b << ws() << "memset($t" << temp << ",0,sizeof(" << formatType(td) << "));" << endl;
                    b << ws();
                    renderArg(0,ae->d_args.first().data(),false);
                    b << " = ";
                    b << "$t" << temp << ";" << endl;
                    b << ws() << classRef(td) << "$init$(" << "$t" << temp << ")";
                    sellTemp(temp);
                }
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
                                b << bt->maxVal().toLongLong();
                            else
                                b << bt->minVal().toLongLong();
                            break;
                        case Type::LONGREAL:
                        case Type::REAL:
                            if( bi->d_func == BuiltIn::MAX )
                                b << bt->maxVal().toDouble();
                            else
                                b << bt->minVal().toDouble();
                            break;
                        case Type::SHORTINT:
                        case Type::INTEGER:
                            if( bi->d_func == BuiltIn::MAX )
                                b << bt->maxVal().toInt();
                            else
                                b << bt->minVal().toInt();
                            break;
                        case Type::BOOLEAN:
                        case Type::CHAR:
                        case Type::WCHAR:
                        case Type::BYTE:
                        case Type::SET:
                            if( bi->d_func == BuiltIn::MAX )
                                b << bt->maxVal().toUInt();
                            else
                                b << bt->minVal().toUInt();
                            break;
                        }
                    }
                    break;
                case Thing::T_Enumeration:
                    {
                        Enumeration* e = cast<Enumeration*>(t);
                        if( bi->d_func == BuiltIn::MAX )
                            b << e->d_items.last()->d_val.toInt();
                        else
                            b << e->d_items.first()->d_val.toInt();
                    }
                    break;
                default:
                    Q_ASSERT( false );
                }
            }else if( ae->d_args.size() == 2 )
            {
                b << "(";
                ae->d_args.first()->accept(this);
                b << ( bi->d_func == BuiltIn::MAX ? " > " : " < " );
                ae->d_args.last()->accept(this);
                b << " ? ";
                ae->d_args.first()->accept(this); // TODO: use runtime calls to avoid multiple evaluation
                b << " : ";
                ae->d_args.last()->accept(this);
                b << ")";
            }else
                Q_ASSERT( false );
            break;
        case BuiltIn::SHORT:
        case BuiltIn::LONG:
            Q_ASSERT( ae->d_args.size() == 1 );
            ae->d_args.first()->accept(this);
            // TODO: text
            break;
        case BuiltIn::CHR:
            Q_ASSERT( ae->d_args.size() == 1 );
            b << "((char)(uint8_t)";
            ae->d_args.first()->accept(this);
            b << ")";
            break;
        case BuiltIn::WCHR:
            Q_ASSERT( ae->d_args.size() == 1 );
            b << "((wchar_t)";
            ae->d_args.first()->accept(this);
            b << ")";
            break;
        case BuiltIn::ODD:
            Q_ASSERT( ae->d_args.size() == 1 );
            b << "(";
            ae->d_args.first()->accept(this);
            b << " % 2 != 0 )";
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
                    b << 1;
                    break;
                case Type::WCHAR:
                case Type::SHORTINT:
                    b << 2;
                    break;
                case Type::INTEGER:
                case Type::REAL:
                case Type::SET:
                    b << 4;
                    break;
                case Type::LONGINT:
                case Type::LONGREAL:
                    b << 8;
                    break;
                default:
                    switch( t->getTag() )
                    {
                    case Thing::T_Pointer:
                        b << "sizeof(void*)";
                        break;
                    case Thing::T_Record:
                    case Thing::T_Array:
                        b << "sizeof(";
                        ae->d_args.first()->accept(this);
                        b << ")";
                        break;
                    default:
                        Q_ASSERT( false ); // TODO
                        break;
                    }
                    break;
                }
            }
            break;
        case BuiltIn::INCL:
            {
                Q_ASSERT( ae->d_args.size() == 2 );

                b << "(";
                ae->d_args.first()->accept(this);
                b << " |= 1 << ";
                ae->d_args.last()->accept(this);
                b << ")";
            }
            break;
        case BuiltIn::EXCL:
            {
                Q_ASSERT( ae->d_args.size() == 2 );

                b << "(";
                ae->d_args.first()->accept(this);
                b << " &= ~( 1 << ";
                ae->d_args.last()->accept(this);
                b << "))";
            }
            break;
        default:
             qCritical() << "missing generator implementation of" << BuiltIn::s_typeName[bi->d_func];
             break;
        }
    }

    inline static bool hasDynLen(const QList<Array*>& dims )
    {
        for( int i = 0; i < dims.size(); i++ )
        {
            if( dims[i]->d_lenExpr.isNull() )
                return true;
        }
        return false;
    }

    void renderDeref(Expression* arrDesig, const QList<ArgExpr*>& idxDims, bool addrOf )
    {
        // Partly index an array so the result is a subset array, not a scalar

        Type* td = derefed(arrDesig->d_type.data());
        Q_ASSERT( td->getTag() == Thing::T_Array );
        Array* a = cast<Array*>(td);
        QList<Array*> allDims = a->getDims();
        const bool dynLen = hasDynLen(allDims);

        const int temp = buyTemp(arrayType(allDims.size(),arrDesig->d_loc)+"*");
        if( dynLen )
        {
            // take the address of the base OBX$Array$x struct so it's fields can be transferred to the slice
            // this results in a sequential evaluation expression in the kind of
            // ($=&OBX$Array$x, &(OBX$Array$y){ $->$1, ... })
            b << "($t" << temp << " = (";
            renderArg(0,arrDesig,true);
            b << "),";
        }
        if( addrOf )
            b << "&";
        b << "(" << arrayType(allDims.size()-idxDims.size(), arrDesig->d_loc) << "){";
        for(int i = 0; i < (allDims.size()-idxDims.size()); i++ )
        {
            if( allDims[i+idxDims.size()]->d_lenExpr )
                b << allDims[i+idxDims.size()]->d_len << ",";
            else
            {
                Q_ASSERT(dynLen);
                b << "$t" << temp << "->$" << i+idxDims.size()+1 << ",";
            }
        }
        b << "1,"; // 1 because we point in a slice here, not the start of the array
        b << "&((" << formatType(allDims.last()->d_type.data(),"*") << ")"; // cast to a 1d array because we only index one dim
             // addrof result required because we need address of array, i.e. &array[index]
        if( dynLen )
            b  << "$t" << temp << "->$a)[";
        else
        {
            b << "(";
            renderArg(0,arrDesig,false);
            b << ").$a)[";
        }
        for(int dim = 0; dim < idxDims.size(); dim++ )
        {
            // NOTE: similar to visit(ArgExpr*), but here we don't need the +d0 part, only the ( D0 * d1 )... parts
            // (d1 * D2 * D3 ... * Dn) + (d2 * D3 * D4 ... * Dn) + (d3 * D4 ... * Dn) ... + (dn-1 * Dn) + dn
            if( dim != 0 )
                b << "+";
            for( int i = dim; i < allDims.size(); i++ )
            {
                if( i != dim )
                    b << "*";
                if( i == dim )
                {
                    Q_ASSERT( idxDims[i]->d_args.size() == 1 );
                    idxDims[i]->d_args.first()->accept(this);
                }else
                {
                    if( allDims[i]->d_lenExpr )
                        b << allDims[i]->d_len;
                    else
                    {
                        Q_ASSERT(dynLen);
                        b << "$t" << temp << "->$" << i+1;
                    }
                }
            }
        }
        b << "]}";
        sellTemp(temp);
        if( dynLen )
            b << ")";
    }

    void renderArg( Type* lhs, Expression* rhs, bool addrOf )
    {
        // this method takes care that all arrays are rendered as OBX$Array
        Type* tf = derefed(lhs);
        Type* ta = derefed(rhs->d_type.data());
        Q_ASSERT(ta);
        const int atag = ta->getTag();
        if( ta->isString() || ta->getBaseType() == Type::BYTEARRAY )
        {
            if( addrOf )
                b << "&";
            if( tf && tf->isChar() )
            {
                if( ta->getBaseType() == Type::STRING )
                {
                    b << "*(const char*)(";
                    rhs->accept(this);
                    b << ").$a";
                }else
                {
                    b << "*(const wchar_t*)(";
                    rhs->accept(this);
                    b << ").$a";
                }
            }else
                rhs->accept(this);
        }else if( atag == Thing::T_Array )
        {
            // NOTE: copy if not passByRef is done in procedure
            const int tag = rhs->getTag();
            if( rhs->getUnOp() == UnExpr::DEREF )
            {
                UnExpr* e = cast<UnExpr*>(rhs);
                if( addrOf )
                    b << "&"; // TODO: is this true?
                e->accept(this); // the thing before DEREF is a pointer
            }else if( tag == Thing::T_BinExpr // happens if strings are added
                      || tag == Thing::T_Literal // a string or bytearray literal is always OBX$Array
                      || ( rhs->getIdent() && rhs->getIdent()->getTag() == Thing::T_Parameter ) // a param is always OBX$Array
                      )
            {
                if( addrOf )
                    b << "&";
                rhs->accept(this);
            }else if( rhs->getUnOp() == UnExpr::CALL ) // a call always returns an OBX$Array
            {
                if( addrOf )
                    b << "&";
                rhs->accept(this);
            }else if( rhs->getUnOp() == UnExpr::IDX )
            {
                // a n-dim array is partly indexed, i.e. not to the element, but an m-dim array of elements
                ArgExpr* e = cast<ArgExpr*>(rhs);
                QList<ArgExpr*> idx;
                idx.prepend(e);
                while( e->d_sub->getUnOp() == UnExpr::IDX )
                {
                    e = cast<ArgExpr*>(e->d_sub.data());
                    idx.prepend(e);
                }
                renderDeref(e->d_sub.data(),idx,addrOf);
            }else
            {
                Q_ASSERT(tag == Thing::T_IdentLeaf || Thing::T_IdentSel );
                // It's a array value, convert it to an array pointer
                Array* a = cast<Array*>(ta);
                QList<Array*> dims = a->getDims();
                if( addrOf )
                    b << "&";
                b << "(" << arrayType(dims.size(), rhs->d_loc) << "){";
                for(int i = 0; i < dims.size(); i++ )
                {
                    Q_ASSERT( dims[i]->d_lenExpr ); // array values always have static lens
                    b << dims[i]->d_len << ",";
                }
                b << "1,";
                rhs->accept(this);
                b << "}";
            }
        }else if( atag == Thing::T_ProcType && ta->d_typeBound &&
                  rhs->getIdent() && rhs->getIdent()->getTag() == Thing::T_Procedure )
        {
            Q_ASSERT( rhs->getTag() == Thing::T_IdentSel );
            IdentSel* sel = cast<IdentSel*>(rhs);
            if( addrOf )
                b << "&";
            b << "(struct OBX$Deleg){";
            Type* td = derefed(sel->d_sub->d_type.data());
            Q_ASSERT( td );
            if( td->getTag() != Thing::T_Pointer )
                b << "&";
            sel->d_sub->accept(this);
            b << ",";
            b << "(OBX$NullMeth)";
            sel->accept(this); // TODO: do we need the $ trick here?
            b << "}";
        }else if( atag == Thing::T_Record )
        {
            // ta is record or pointer to record
            if( tf && ta != tf ) // tf may be 0 here
            {
                // we need a cast
                // ta is record
                if( addrOf )
                    b << "((" << formatType(tf,"*") << ")&"; // ((T*)&rec)
                else
                    b << "*((" << formatType(tf,"*") << ")&"; // *((T*)&rec)
                rhs->accept(this);
                b << ")";
            }else
            {
                if( addrOf )
                    b << "&";
                    //b << "&(" << formatType(tf) << "[1]){";
                rhs->accept(this);
                //if( addrOf )
                //   b << "}[0]";
            }
        }else if( ta->toRecord() )
        {
            Q_ASSERT( ta->getTag() == Thing::T_Pointer );
            if( tf && ta != tf ) // tf may be 0 here
            {
                // we need a cast
                Q_ASSERT( tf->getTag() == Thing::T_Pointer );
                // ta is pointer to record
                if( addrOf )
                    b << "((" << formatType(tf,"*") << ")&"; // (T**)&ptr
                else
                    b << "((" << formatType(tf) << ")"; // (T*)ptr
                rhs->accept(this);
                b << ")";
            }else
            {
                if( addrOf )
                    b << "&";
                rhs->accept(this);
            }
        }else
        {
            // if rhs is a skalar or constant taking the address of it requires a temporary storage
            const bool isObject = isLvalue(rhs) || rhs->getUnOp() == UnExpr::IDX || rhs->getUnOp() == UnExpr::CAST;
            if( addrOf && !isObject )
                b << "&(" << formatType(rhs->d_type.data()) << "[1]){";
            else if( addrOf )
                b << "&";
            rhs->accept(this);
            if( addrOf && !isObject )
                b << "}[0]";
        }
    }

    bool isLvalue( Expression* lhs )
    {
        if( lhs == 0 )
            return false;
        Named* id = lhs->getIdent();
        if( id )
        {
            switch( id->getTag() )
            {
            case Thing::T_BuiltIn:
            case Thing::T_Import:
            case Thing::T_GenericName:
            case Thing::T_Procedure:
            case Thing::T_NamedType:
            case Thing::T_Module:
            case Thing::T_Const:
                return false;
            default:
                return true;
            }
        }
        return false;
    }

    void emitActuals( ProcType* pt, ArgExpr* me )
    {
        Q_ASSERT( pt->d_formals.size() <= me->d_args.size() );
        for( int i = 0; i < pt->d_formals.size(); i++ )
        {
            if( i != 0 )
                b << ", ";
            Parameter* p = pt->d_formals[i].data();
            Type* ta = derefed(me->d_args[i]->d_type.data());
            if( p->d_const && !ta->isStructured(true) && !ta->isString() && !isLvalue(me->d_args[i].data()) )
            {
                // if IN and not an lvalue and not structured use a compound literal
                b << "&(" << formatType(ta) << "){";
                me->d_args[i]->accept(this);
                b << "}";
            }else
            {
                const bool addrOf = passByRef(p) && ( ta->getTag() != Thing::T_Array && !ta->isString() );
                renderArg2(p->d_type.data(), me->d_args[i].data(), addrOf );
            }
        }
    }

    void renderArg2( Type* lhs, Expression* rhs, bool addrOf )
    {
        Q_ASSERT( lhs );

        // if rhs is a call, we need a compound literal for addrOf, otherwise the
        // C compiler complains that it is not an lvalue
        const bool tempStore = addrOf &&
                ( rhs->getUnOp() == UnExpr::CALL ||
                                           ( rhs->getUnOp() == UnExpr::DEREF
                                             && cast<UnExpr*>(rhs)->d_sub->getUnOp() == UnExpr::CALL ) );
        if( tempStore )
        {
            b << "&(";
            Type* tf = derefed(lhs);
            if( tf->getTag() == Thing::T_Pointer )
                tf = derefed(cast<Pointer*>(tf)->d_to.data());
            Q_ASSERT(tf);
            if( tf->getTag() == Thing::T_Array )
            {
                int dims;
                cast<Array*>(tf)->getTypeDim(dims);
                b << arrayType(dims,rhs->d_loc);
            }else
                b << formatType(lhs);
            b << "[1]){";
        }
        renderArg( lhs, rhs, addrOf && !tempStore );
        if( tempStore )
            b << "}[0]";
    }

    void emitCall( ArgExpr* me )
    {
        Q_ASSERT( me->d_sub );

        Named* func = 0;
        bool superCall = false;
        if( me->d_sub->getUnOp() == UnExpr::DEREF )
        {
            // call to superclass method; this is not a vtbl dispatch
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

        if( pt->d_typeBound && !superCall )
        {
            if( func )
            {
                //  ( pointer ^ | record ) .method (args)
                Q_ASSERT(me->d_sub->getTag() == Thing::T_IdentSel );
                IdentSel* method = cast<IdentSel*>(me->d_sub.data());
                Type* td = derefed(method->d_sub->d_type.data());
                Q_ASSERT(td && td->getTag() == Thing::T_Record);
                Procedure* p = cast<Procedure*>(func);
                const int self = buyTemp("void*");

#ifdef _OBX_FUNC_SEQ_POINT_
                const int fptr = buyTemp(formatProcType(pt),pt);
                b << "($t" << fptr << " = ";
#endif
                b << "((" << formatType(p->d_receiverRec,"*") << ")($t" << self << " = ";
                b << "&(";
                method->d_sub->accept(this);
                b << ")";
                b << "))->class$->" << escape( method->getIdent()->d_name);
#ifdef _OBX_FUNC_SEQ_POINT_
                b << ", " << "$t" << fptr;
#endif
                b << "($t" << self;
                if( !pt->d_formals.isEmpty() )
                {
                    b << ", ";
                    emitActuals(pt,me);
                }
                b << ")";
#ifdef _OBX_FUNC_SEQ_POINT_
                b << ")";
                sellTemp(fptr);
#endif
                sellTemp(self);
            }else
            {
                const int deleg = buyTemp("struct OBX$Deleg*");
                b << "($t" << deleg << " = &(struct OBX$Deleg[1]){";
                me->d_sub->accept(this); // use compound because d_sub could be function call resulting in lvalue error
                b << "}[0],";
                b << "((" << formatReturn(pt,"") << "(*)"
                  << formatFormals(pt,false) << ")"; // cast to method
                b << "$t" << deleg << "->func)";
                b << "($t" << deleg << "->inst"; // call
                if( !pt->d_formals.isEmpty() )
                {
                    b << ", ";
                    emitActuals(pt,me);
                }
                b << ")"; // call
                b << ")"; // ($t=
                sellTemp(deleg);
            }
        }else if( pt->d_typeBound && superCall )
        {
            Q_ASSERT( func );

            Q_ASSERT(me->d_sub->getUnOp() == UnExpr::DEREF );
            UnExpr* sub = cast<UnExpr*>(me->d_sub.data());
            Q_ASSERT(sub->d_sub->getTag() == Thing::T_IdentSel );
            IdentSel* method = cast<IdentSel*>(sub->d_sub.data());
            Type* td = derefed(method->d_sub->d_type.data());
            Q_ASSERT(td && td->getTag() == Thing::T_Record);
            Procedure* p = cast<Procedure*>(func);
            const int self = buyTemp("void*");

#ifdef _OBX_FUNC_SEQ_POINT_
                const int fptr = buyTemp(formatProcType(pt),pt);
                b << "($t" << fptr << " = ";
#endif
            b << "((" << formatType(p->d_receiverRec,"*") << ")($t" << self << " = ";
            b << "&(";
            method->d_sub->accept(this);
            b << ")";
            b << "))->class$->super$->" << escape( method->getIdent()->d_name);
#ifdef _OBX_FUNC_SEQ_POINT_
                b << ", " << "$t" << fptr;
#endif
            b << "($t" << self;
            if( !pt->d_formals.isEmpty() )
            {
                b << ", ";
                emitActuals(pt,me);
            }
            b << ")";
#ifdef _OBX_FUNC_SEQ_POINT_
            b << ")";
            sellTemp(fptr);
#endif
            sellTemp(self);
        }else
        {
            me->d_sub->accept(this);
            b << "(";
            emitActuals(pt,me);
            b << ")";
        }
    }

    void visit( ArgExpr* me )
    {
        switch( me->d_op )
        {
        case ArgExpr::IDX:
            {
                Q_ASSERT( me->d_sub );
                Q_ASSERT( me->d_args.size() == 1 );
                ArgExpr* e = me;
                QList<ArgExpr*> idx;
                idx.prepend(e);
                while( e->d_sub->getUnOp() == UnExpr::IDX )
                {
                    e = cast<ArgExpr*>(e->d_sub.data());
                    idx.prepend(e);
                }
                Type* td = derefed(e->d_sub->d_type.data());
                Q_ASSERT( td->getTag() == Thing::T_Array );
                QList<Array*> dims = cast<Array*>(td)->getDims();
                const bool dynLen = hasDynLen(dims);
                b << "(";
                const int temp = buyTemp(arrayType(dims.size(),e->d_sub->d_loc)+"*");
                if( dynLen )
                {
                    // take the address of the base OBX$Array$x struct so it's fields can be used later
                    b << "*($t" << temp << " = (";
                    renderArg(0,e->d_sub.data(),true);
                    b << "),&";
                }
                b << "((" << formatType(me->d_type.data(),"*") << ")"; // cast to a 1d array because we only index one dim
                if( dynLen )
                    b << "$t" << temp << "->$a)[";
                else
                {
                    b << "(";
                    renderArg(0,e->d_sub.data(),false);
                    b << ").$a)[";
                }
                // C uses row-col order, i.e. A[row][col]
                for(int dim = 0; dim < idx.size(); dim++ )
                {
                    // sample for 3D (Dx dim width, dx index):
                    // ( d0 * D1 * D2 ) + ( d1 * D2 ) + d2
                    // (d1 * D2 * D3 ... * Dn) + (d2 * D3 * D4 ... * Dn) + (d3 * D4 ... * Dn) ... + (dn-1 * Dn) + dn
                    if( dim != 0 )
                        b << "+";
                    for( int i = dim; i < idx.size(); i++ )
                    {
                        if( i != dim )
                            b << "*";
                        if( i == dim )
                        {
                            Q_ASSERT( idx[i]->d_args.size() == 1 );
                            idx[i]->d_args.first()->accept(this);
                        }else
                        {
                            if( dims[i]->d_lenExpr )
                                b << dims[i]->d_len;
                            else
                            {
                                Q_ASSERT(dynLen);
                                b << "$t" << temp << "->$" << i+1;
                            }
                        }
                    }
                }
                b << "]";
                if( dynLen )
                    b << ")";
                b << ")";
                sellTemp(temp);
            }
            break;
        case ArgExpr::CALL:
            emitCall(me);
            break;
        case ArgExpr::CAST:
            {
                Type* tsub = derefed(me->d_sub->d_type.data());
                Q_ASSERT( tsub );
                if( tsub->getTag() == Thing::T_Pointer )
                {
                    b << "((struct " << classRef(me->d_type->toRecord()) << "*)";
                    me->d_sub->accept(this);
                    b << ")";
                }else
                {
                    b << "(*(struct " << classRef(me->d_type->toRecord()) << "*)&";
                    me->d_sub->accept(this);
                    b << ")";
                }
            }
            break;
        }
    }

    void emitBinOp(BinExpr* me, const char* op )
    {
        b << "(";
        me->d_lhs->accept(this);
        b << " " << op << " ";
        me->d_rhs->accept(this);
        b << ")";
    }

    void emitPointerBinOp(BinExpr* me, const char* op )
    {
        const int lhs = isStructuredOrArrayPointer(me->d_lhs->d_type.data());
        const int rhs = isStructuredOrArrayPointer(me->d_rhs->d_type.data());
        b << "(";
        if( lhs != IsArrayPointer && lhs != IsDeleg )
            b << "(void*)";
        me->d_lhs->accept(this);
        if( lhs == IsArrayPointer )
            b << ".$a";
        else if( lhs == IsDeleg )
            b << ".func"; // TODO: also include inst in comparison!
        b << " " << op << " ";
        if( rhs != IsArrayPointer && rhs != IsDeleg )
            b << "(void*)";
        me->d_rhs->accept(this);
        if( rhs == IsArrayPointer )
            b << ".$a";
        else if( rhs == IsDeleg )
            b << ".func";
        b << ")";
    }

    void emitStringOp( Type* lhs, Type* rhs, bool lwide, bool rwide, int op, BinExpr* e )
    {
        const bool lhsChar = lhs->isChar();
        const bool rhsChar = rhs->isChar();
        if( lhsChar || rhsChar )
        {
            b << "(";
            if( !lwide )
                b << "(uint8_t)";
            renderArg(lhsChar ? lhs : rhs,e->d_lhs.data(),false);
            switch(op)
            {
            case 1: // ==
                b << "==";
                break;
            case 2: // !=
                b << "!=";
                break;
            case 3: // <
                b << "<";
                break;
            case 4: // <=
                b << "<=";
                break;
            case 5: // >
                b << ">";
                break;
            case 6: // >=
                b << ">=";
                break;
            }
            if( !rwide )
                b << "(uint8_t)";
            renderArg(lhsChar ? lhs : rhs,e->d_rhs.data(),false);
            b << ")";
        }else
        {
            b << "OBX$StrOp(";
            renderArg2(lhs,e->d_lhs.data(),true);
            b << "," << int(lwide) << ",";
            renderArg2(rhs,e->d_rhs.data(),true);
            b << "," << int(rwide) << ",";
            b << op << ")";
        }
    }

    void visit( BinExpr* me )
    {
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
                b << "((1<<";
                me->d_lhs->accept(this);
                b << ")&";
                me->d_rhs->accept(this);
                b << ")";
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::IS:
            b << "OBX$IsSubclass(";
            b << "&" << classRef(me->d_rhs->d_type.data()) << "$class$, OBX$ClassOf(";
            if( rtag == Thing::T_Record )
                b << "&";
            me->d_lhs->accept(this);
            b << "))";
            break;
        case BinExpr::ADD:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) )
                emitBinOp(me,"+");
            else if( lhsT->isSet() && rhsT->isSet() )
                emitBinOp(me,"|");
            else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) && !lhsT->d_unsafe && !rhsT->d_unsafe )
            {
                b << "(struct OBX$Array$1 [1]){OBX$StrJoin(";
                renderArg2(lhsT,me->d_lhs.data(),true);
                b << "," << int(lwide) << ",";
                renderArg2(rhsT,me->d_rhs.data(),true);
                b << "," << int(rwide) << ")}[0]";
                // NOTE: if we don't use the dirty compound array literal element zero trick nested OBX$StrJoin
                // would issue an lvalue error
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::SUB:
            if( (lhsT->isNumeric() && rhsT->isNumeric()) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) )
                emitBinOp(me,"-");
            else if( lhsT->isSet() && rhsT->isSet() )
            {
                b << "(";
                me->d_lhs->accept(this);
                b << " & ~";
                me->d_rhs->accept(this);
                b << ")";
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::FDIV:
            if( lhsT->isNumeric() && rhsT->isNumeric() )
                emitBinOp(me,"/");
            else if( lhsT->isSet() && rhsT->isSet() )
            {
                b << "OBX$SetDiv(";
                me->d_lhs->accept(this);
                b << ",";
                me->d_rhs->accept(this);
                b << ")";
            }
            break;
        case BinExpr::MUL:
            if( lhsT->isNumeric() && rhsT->isNumeric() )
                emitBinOp(me,"*");
            else if( lhsT->isSet() && rhsT->isSet() )
                emitBinOp(me,"&");
            else
                Q_ASSERT(false);
            break;
        case BinExpr::DIV:
            if( lhsT->isInteger() && rhsT->isInteger() )
            {
                if( lhsT->getBaseType() <= Type::INTEGER && rhsT->getBaseType() <= Type::INTEGER )
                    b << "OBX$Div32(";
                else
                    b << "OBX$Div64(";
                me->d_lhs->accept(this);
                b << ",";
                me->d_rhs->accept(this);
                b << ")";
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::MOD:
            if( lhsT->isInteger() && rhsT->isInteger() )
            {
                if( lhsT->getBaseType() <= Type::INTEGER && rhsT->getBaseType() <= Type::INTEGER )
                    b << "OBX$Mod32(";
                else
                    b << "OBX$Mod64(";
                me->d_lhs->accept(this);
                b << ",";
                me->d_rhs->accept(this);
                b << ")";
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::AND:
            if( lhsT->getBaseType() == Type::BOOLEAN && rhsT->getBaseType() == Type::BOOLEAN )
                emitBinOp(me,"&&");
            else
                Q_ASSERT(false);
            break;
        case BinExpr::OR:
            if( lhsT->getBaseType() == Type::BOOLEAN && rhsT->getBaseType() == Type::BOOLEAN )
                emitBinOp(me,"||");
            else
                Q_ASSERT(false);
            break;
        case BinExpr::EQ:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( lhsT->getBaseType() == Type::BOOLEAN && rhsT->getBaseType() == Type::BOOLEAN ) ||
                    ( lhsT->getBaseType() == Type::SET && rhsT->getBaseType() == Type::SET) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) )
            {
                emitBinOp(me,"==");
            }else if( ( ( lhsT->getBaseType() == Type::NIL || ltag == Thing::T_Pointer || ltag == Thing::T_ProcType ) &&
                        ( rhsT->getBaseType() == Type::NIL || rtag == Thing::T_Pointer || rtag == Thing::T_ProcType ) ) )
            {
                emitPointerBinOp(me,"==");
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                emitStringOp(lhsT, rhsT, lwide, rwide, 1, me );
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::NEQ:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( lhsT->getBaseType() == Type::BOOLEAN && rhsT->getBaseType() == Type::BOOLEAN ) ||
                    ( lhsT->getBaseType() == Type::SET && rhsT->getBaseType() == Type::SET) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) )
            {
                emitBinOp(me,"!=");
            }else if( ( ( lhsT->getBaseType() == Type::NIL || ltag == Thing::T_Pointer || ltag == Thing::T_ProcType ) &&
                        ( rhsT->getBaseType() == Type::NIL || rtag == Thing::T_Pointer || rtag == Thing::T_ProcType ) ) )
            {
                emitPointerBinOp(me,"!=");
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                emitStringOp(lhsT, rhsT, lwide, rwide, 2, me );
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::LT:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) )
            {
                emitBinOp(me,"<");
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                emitStringOp(lhsT, rhsT, lwide, rwide, 3, me );
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::LEQ:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) )
            {
                emitBinOp(me,"<=");
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                emitStringOp(lhsT, rhsT, lwide, rwide, 4, me );
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::GT:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) )
            {
                emitBinOp(me,">");
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                emitStringOp(lhsT, rhsT, lwide, rwide, 5, me );
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::GEQ:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) )
            {
                emitBinOp(me,">=");
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                emitStringOp(lhsT, rhsT, lwide, rwide, 6, me );
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
        b << "OBX$MakeSet(" << me->d_parts.size() * 2;
        for( int i = 0; i < me->d_parts.size(); i++ )
        {
            b << ",";
            BinExpr* bi = me->d_parts[i]->getTag() == Thing::T_BinExpr ? cast<BinExpr*>( me->d_parts[i].data() ) : 0;
            if( bi && bi->d_op == BinExpr::Range )
            {
                Q_ASSERT( bi->d_lhs );
                bi->d_lhs->accept(this);
                b << ",";
                Q_ASSERT( bi->d_rhs );
                bi->d_rhs->accept(this);
            }else
            {
                me->d_parts[i]->accept(this);
                b << ",-1";
            }
        }
        b << ")";
    }

    void visit( Assign* me)
    {
        b << ws();
        Type* tl = derefed(me->d_lhs->d_type.data());
        if( tl->getTag() == Thing::T_Array )
        {
            Type* tr = derefed(me->d_rhs->d_type.data());
            bool lwide, rwide;
            if( tl->isText(&lwide) && tr->isText(&rwide) )
            {
                Q_ASSERT( !tl->isChar() );
                b << "OBX$StrCopy(";
                renderArg2(tl, me->d_lhs.data(),true);
                b << "," << int(lwide) << ",";
                if( tr->isChar() )
                {
                    b << "&(struct OBX$Array$1){2,1,&(";
                    if( rwide )
                        b << "wchar_t";
                    else
                        b << "char";
                    b << "[2]){";
                    me->d_rhs->accept(this);
                    b << ",0}}";
                }else
                    renderArg2(tl, me->d_rhs.data(),true);
                b << "," << int(rwide) << ")";
            }else
            {
                b << "OBX$ArrCopy(";
                renderArg2(tl, me->d_lhs.data(),true);
                b << ",";
                renderArg2(tl, me->d_rhs.data(),true);
                b << ",";
                int dims;
                Type* at = cast<Array*>(tl)->getTypeDim(dims);
                b << dims << ",";
                b << "sizeof(" << formatType(at) << ")";
                b << ")";
            }
        }else
        {
            me->d_lhs->accept(this);
            b << " = ";
            renderArg(tl, me->d_rhs.data(),false);
        }
        b << ";" << endl;
    }

    void visit( Call* me )
    {
        Q_ASSERT( me->d_what );
        b << ws();
        me->d_what->accept(this);
        b << ";" << endl;
    }

    void visit( Exit*)
    {
        b << ws() << "break;" << endl; // since we don't use switch this doesn't colide with switch/break.
    }

    void visit( Return* me)
    {
        Q_ASSERT( curProc );
        b << ws() << "return ";
        if( me->d_what )
            renderArg( curProc->getProcType()->d_return.data(), me->d_what.data(), false );
        b << ";" << endl;
    }

    void visit( CaseStmt* me)
    {
        // NOTE: identical with CilGen!

        // TODO: else not yet handled; if else missing then abort if no case hit
        if( !me->d_else.isEmpty() )
            qCritical() << "CASE ELSE not yet implemented" << me->d_loc.d_row << me->d_loc.d_col << thisMod->d_file;
        if( me->d_typeCase )
        {
            // first rewrite the AST with 'if' instead of complex 'case'

            if( me->d_cases.isEmpty() )
                return;

            Named* old = retypedRecIdent;
            retypedRecIdent = me->d_exp->getIdent();

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
            retypedRecIdent = old;
        }
        else
        {
            // first rewrite the AST with 'if' instead of complex 'case'

            Ref<IfLoop> ifl = new IfLoop();
            ifl->d_op = IfLoop::IF;
            ifl->d_loc = me->d_loc;

            Ref<BaseType> boolean = new BaseType(Type::BOOLEAN);

            // TODO: consider switch statement
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

            // and now generate code for the if
            ifl->accept(this);
        }
    }

    Named* getGuard(Expression* e)
    {
        Q_ASSERT( e->getTag() == Thing::T_BinExpr );
        BinExpr* guard = cast<BinExpr*>(e);
        Q_ASSERT( guard->d_op == BinExpr::IS );

        return guard->d_lhs->getIdent();
    }

    void emitIf( IfLoop* me, bool withStatement )
    {
        b << ws() << "if( ";
        me->d_if[0]->accept(this);
        b << " ) {" << endl;
        level++;
        Named* old = retypedRecIdent;
        if( withStatement )
            retypedRecIdent = getGuard(me->d_if[0].data());
        for( int i = 0; i < me->d_then[0].size(); i++ )
            emitStatement(me->d_then[0][i].data());
        retypedRecIdent = old;
        level--;
        b << ws() << "} ";
        for( int i = 1; i < me->d_if.size(); i++ ) // ELSIF
        {
            b << "else if( ";
            me->d_if[i]->accept(this);
            b << " ) {" << endl;
            level++;
            if( withStatement )
                retypedRecIdent = getGuard(me->d_if[i].data());
            for( int j = 0; j < me->d_then[i].size(); j++ )
                emitStatement(me->d_then[i][j].data());
            retypedRecIdent = old;
            level--;
            b << ws() << "} ";
        }
        if( !me->d_else.isEmpty() ) // ELSE
        {
            b << "else {" << endl;
            level++;
            for( int j = 0; j < me->d_else.size(); j++ )
                emitStatement(me->d_else[j].data());
            level--;
            b << ws() << "}";
        }
        b << endl;
    }

    void visit( IfLoop* me)
    {
        switch( me->d_op )
        {
        case IfLoop::IF:
            emitIf(me,false);
            break;
        case IfLoop::WHILE:
            {
                // NOTE: identical with CilGen!
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
                b << ws() << "do {" << endl;
                level++;
                for( int i = 0; i < me->d_then.first().size(); i++ )
                    emitStatement(me->d_then.first()[i].data());
                level--;
                b <<  ws() << "}while(!(";
                me->d_if[0]->accept(this);
                b << "));" << endl;
            }
            break;
        case IfLoop::WITH:
            {
                // if guard then statseq elsif guard then statseq else statseq end
                // guard ::= lhs IS rhs
                emitIf(me,true);
            }
            break;
        case IfLoop::LOOP:
            {
                b << ws() << "while(1) {" << endl;
                level++;
                for( int i = 0; i < me->d_then.first().size(); i++ )
                    emitStatement(me->d_then.first()[i].data());
                level--;
                b << ws() << "}" << endl;
            }
            break;
        }
    }

    void visit( ForLoop* me)
    {
        // NOTE: identical with CilGen!

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
    }

    void visit( LocalVar* ) { Q_ASSERT(false); }
    void visit( Const* ) { Q_ASSERT(false); }
    void visit( Array* ) { Q_ASSERT(false); }
    void visit( GenericName* )  { Q_ASSERT(false); }
    void visit( NamedType* ) { Q_ASSERT(false); }
    void visit( Import* ) { Q_ASSERT(false); }
    void visit( BuiltIn* ) { Q_ASSERT(false); }
    void visit( Pointer* ) { Q_ASSERT(false); }
    void visit( Record* ) { Q_ASSERT(false); }
    void visit( ProcType* ) { Q_ASSERT(false); }
    void visit( QualiType* ) { Q_ASSERT(false); }
    void visit( Enumeration* ) { Q_ASSERT(false); }
    void visit( Parameter* ) { Q_ASSERT(false); }
    void visit( BaseType* ) { Q_ASSERT(false); }
};

static bool copyFile( const QDir& outDir, const QByteArray& name, QTextStream& list )
{
    QFile f( QString(":/scripts/%1" ).arg(name.constData() ) );
    if( !f.open(QIODevice::ReadOnly) )
    {
        qCritical() << "unknown lib" << name;
        return false;
    }
    QFile out( outDir.absoluteFilePath(name) );
    if( !out.open(QIODevice::WriteOnly) )
    {
        qCritical() << "cannot open for writing" << out.fileName();
        return false;
    }
    out.write( f.readAll() );
    list << name << endl;
    return true;
}

bool Obx::CGen2::translateAll(Obx::Project* pro, bool debug, const QString& where)
{
    // NOTE: can be built using cc -O2 --std=c99 *.c -lm resulting in a.out

    Q_ASSERT( pro );
    if( where.isEmpty() )
    {
        qCritical() << "translateAll requires a path";
        return false;
    }

    QDir outDir(where);

    QByteArray buildStr;
    QTextStream bout(&buildStr);
    QByteArray clearStr;
    QTextStream fout(&clearStr);

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
        }else if( m->d_isDef
#ifdef _OBX_USE_NEW_FFI_
                  && !m->d_externC
#endif
                  )
        {
            // NOP
        }else
        {
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
                        QFile b(outDir.absoluteFilePath(ObxCGenImp::fileName(inst) + ".c"));
                        if( b.open(QIODevice::WriteOnly) )
                        {
                            QFile h(outDir.absoluteFilePath(ObxCGenImp::fileName(inst) + ".h"));
                            if( h.open(QIODevice::WriteOnly) )
                            {
                                //qDebug() << "generating C for" << m->getName() << "to" << f.fileName();
                                if( !CGen2::translate(&h, &b, inst,debug,pro->getErrs()) )
                                {
                                    qCritical() << "error generating C for" << inst->getName();
                                    return false;
                                }
                                fout << ObxCGenImp::fileName(inst) << ".c" << endl;
                                fout << ObxCGenImp::fileName(inst) << ".h" << endl;
                            }else
                                qCritical() << "could not open for writing" << h.fileName();
                        }else
                            qCritical() << "could not open for writing" << b.fileName();
                    }
                }
            }
        }
    }
    if( !mods.isEmpty() )
    {
        const QByteArray name = "OBX.Main";
        QByteArrayList roots, all;
        for(int i = mods.size() - 1; i >= 0; i-- )
        {
            all.append(ObxCGenImp::moduleRef(mods[i]));
            if( mods[i]->d_usedBy.isEmpty() )
                roots.append(all.back());

        }
        if( roots.isEmpty() )
            roots.append(ObxCGenImp::moduleRef(mods.last())); // shouldn't actually happenk

        QFile f(outDir.absoluteFilePath(name + ".c"));
        if( f.open(QIODevice::WriteOnly) )
        {
            const Project::ModProc& mp = pro->getMain();
            if( mp.first.isEmpty() )
                CGen2::generateMain(&f,roots, all);
            else
                CGen2::generateMain(&f,mp.first, mp.second, all);
            fout << name << ".c" << endl;
        }else
            qCritical() << "could not open for writing" << f.fileName();
    }

    if( pro->useBuiltInOakwood() )
    {
        copyFile(outDir,"Input.c",fout);
        copyFile(outDir,"Input.h",fout);
        copyFile(outDir,"Out.c",fout);
        copyFile(outDir,"Out.h",fout);
        copyFile(outDir,"Math.h",fout);
        copyFile(outDir,"Math.c",fout);
        copyFile(outDir,"MathL.h",fout);
        copyFile(outDir,"MathL.c",fout);
#if 0 // TODO
        copyFile(outDir,"Files",fout);
        copyFile(outDir,"In",fout);
        copyFile(outDir,"Strings",fout);
        copyFile(outDir,"Coroutines",fout);
        copyFile(outDir,"XYplane",fout);
#endif
    }
    copyFile(outDir,"OBX.Runtime.h",fout);
    copyFile(outDir,"OBX.Runtime.c",fout);

    bout << "cc -O2 --std=c99 *.c -lm" << endl;
    bout << "or with Boehm-Demers-Weiser GC" << endl;
    bout << "cc -O2 --std=c99 *.c -lm -DOBX_USE_BOEHM_GC -lgc" << endl;
    bout << "if on Unix/Linux/macOS dynamic libraries should be loaded add -DOBX_USE_DYN_LOAD -ldl" << endl;
    bout.flush();
    fout.flush();

    QFile build( outDir.absoluteFilePath( "build.txt" ) );
    if( !build.open(QIODevice::WriteOnly) )
    {
        qCritical() << "could not open for writing" << build.fileName();
        return false;
    }else
        build.write(buildStr);
    QFile fileList( outDir.absoluteFilePath( "files.txt" ) );
    if( !fileList.open(QIODevice::WriteOnly) )
    {
        qCritical() << "could not open for writing" << fileList.fileName();
        return false;
    }else
        fileList.write(clearStr);
    const bool ok = pro->getErrs()->getErrCount() == errCount;
    return ok;
}

bool Obx::CGen2::translate(QIODevice* header, QIODevice* body, Obx::Module* m, bool debug, Ob::Errors* errs)
{
    Q_ASSERT( m != 0 && header != 0 && body != 0 );

    if( m->d_hasErrors || !m->d_isValidated ) //  not validated can happen if imports cannot be resolved
        return false;

    if( m->d_isDef && !m->d_externC )
        return true;

    ObxCGenImp imp;
    imp.thisMod = m;
    //imp.emitter = e;
    imp.debug = debug;
    imp.h.setDevice(header);
    imp.b.setDevice(body);

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

static QByteArray escapeFileName(QByteArray name)
{
    name.replace('$','.');
    return name;
}

bool CGen2::generateMain(QIODevice* to, const QByteArray& callMod, const QByteArray& callFunc, const QByteArrayList& allMods)
{
    if( callMod.isEmpty() )
        return false;

    QTextStream out(to);
    ObxCGenImp::dedication(out);

    out << "#include <locale.h>" << endl; // https://stackoverflow.com/questions/40590207/displaying-wide-chars-with-printf
    out << "#include \"" << escapeFileName(callMod) << ".h\"" << endl << endl;

    out << "int main(int argc, char **argv) {" << endl;
    out << "    setlocale(LC_ALL, \"C.UTF-8\");" << endl;
    // NOTE: default locale is "C"; in it a string literal with umlaute is coded in UTF-8; printf prints it correctly
    //       in contrast a L"" string literal is decoded in memory as plain Unicode (not UTF-8) in memory
    // we have latin-1 char strings in memory; didn't find a way yet to properly print these, that's why we
    // make a temp wchar_t* copy of the char* and work with C.UTF-8
    out << "    OBX$InitApp(argc,argv);" << endl;
    foreach( const QByteArray& m, allMods )
        out << "    OBX$RegisterModule(\"" << escapeFileName(m) << "\"," << m << "$cmd$ );" << endl;
    out << "    " << callMod + "init$();" << endl;
    if( !callFunc.isEmpty() )
        out << "    " << callMod + "$" + callFunc + "();" << endl;
    out << "    return 0;" << endl;
    out << "}" << endl;
    return true;
}

bool CGen2::generateMain(QIODevice* to, const QByteArrayList& callMods, const QByteArrayList& allMods)
{
    Q_ASSERT( to );
    if( callMods.isEmpty() )
        return false;

    QTextStream out(to);
    ObxCGenImp::dedication(out);

    out << "#include <locale.h>" << endl;
    foreach( const QByteArray& mod, callMods )
        out << "#include \"" << escapeFileName(mod) << ".h\"" << endl;
    out << endl;

    out << "int main(int argc, char **argv) {" << endl;
    out << "    setlocale(LC_ALL, \"C.UTF-8\");" << endl;
    out << "    OBX$InitApp(argc,argv);" << endl;
    foreach( const QByteArray& m, allMods )
        out << "    OBX$RegisterModule(\"" << escapeFileName(m) << "\"," << m << "$cmd$ );" << endl;
    foreach( const QByteArray& mod, callMods )
        out << "    " << mod + "$init$();" << endl;
    out << "    return 0;" << endl;
    out << "}" << endl;
    return true;
}
