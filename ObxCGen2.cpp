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
using namespace Obx;
using namespace Ob;

#ifndef OBX_AST_DECLARE_SET_METATYPE_IN_HEADER
Q_DECLARE_METATYPE( Obx::Literal::SET )
#endif

#define _MY_GENERICS_

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

struct ObxCGenCollector2 : public AstVisitor
{
    QSet<ArgExpr*> boundCalls;
    void visit( Call* me)
    {
       if( me->d_what )
           me->d_what->accept(this);
    }
    void visit( Return* me )
    {
        if( me->d_what )
            me->d_what->accept(this);
    }
    void visit( Assign* me)
    {
        me->d_lhs->accept(this);
        me->d_rhs->accept(this);
    }
    void visit( IfLoop* me)
    {
        foreach( const Ref<Expression>& e, me->d_if )
            e->accept(this);
    }
    void visit( ForLoop* me)
    {
        me->d_from->accept(this);
        me->d_to->accept(this);
        if( me->d_by )
            me->d_by->accept(this);
    }
    void visit( CaseStmt* me)
    {
        me->d_exp->accept(this);
        foreach( const CaseStmt::Case& c, me->d_cases )
            foreach( const Ref<Expression>& e, c.d_labels )
                e->accept(this);
    }

    void visit( SetExpr* me)
    {
        foreach( const Ref<Expression>& e, me->d_parts )
            e->accept(this);
    }
    void visit( UnExpr* me)
    {
        me->d_sub->accept(this);
    }
    void visit( IdentLeaf* ) {} // NOP
    void visit( IdentSel* me)
    {
        me->d_sub->accept(this);
    }
    void visit( BinExpr* me)
    {
        me->d_lhs->accept(this);
        me->d_rhs->accept(this);
    }
    void visit( ArgExpr* me )
    {
        me->d_sub->accept(this);
        if( me->getUnOp() != UnExpr::CALL || me->d_sub->getUnOp() == UnExpr::DEREF )
            return; // supercall
        Type* subT = me->d_sub->d_type->derefed();
        Q_ASSERT( subT && subT->getTag() == Thing::T_ProcType );
        ProcType* pt = cast<ProcType*>( subT );
        if( pt->d_typeBound )
            boundCalls.insert(me); // applies to both named methods and delegates (fp-object aggregate)
    }
};


struct ObxCGenImp : public AstVisitor
{
    Errors* err;
    Module* thisMod;
    int level;
    QTextStream h,b;
    bool ownsErr;
    bool debug; // generate line pragmas
    quint32 anonymousDeclNr; // starts with one, zero is an invalid slot
    QHash<ArgExpr*,int> temps;
    Procedure* curProc;

    ObxCGenImp():err(0),thisMod(0),ownsErr(false),level(0),debug(false),anonymousDeclNr(1),curProc(0){}

    inline QByteArray ws() { return QByteArray(level*4,' '); }

    static QByteArray escape(const QByteArray& str)
    {
        return str; // TODO avoid collision with C keywords
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

    static QByteArray moduleRef(Module* m)
    {
        QByteArray name = m->d_fullName.join('$');
        if( !m->d_metaActuals.isEmpty() )
        {
            QCryptographicHash hash(QCryptographicHash::Md5);
            hash.addData(m->formatMetaActuals());
            name += "$" + hash.result().toHex();
        }
        return escape(name);
    }

    QByteArray classRef( Named* className )
    {
        Q_ASSERT( className && className->getTag() == Thing::T_NamedType );
        Module* m = className->getModule();
        return dottedName(className); // dotted because also records nested in procs are lifted to module level
    }

    QByteArray classRef( Type* rec )
    {
        rec = derefed(rec);
        return classRef(cast<Record*>(rec));
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
        if( receiver )
        {
#if 0
            Type* t = receiver->d_type.data();
            Pointer p;
            if( passByRef(receiver) )
            {
                t = &p;
                p.d_to = receiver->d_type.data();
                p.d_decl = receiver;
                p.d_loc = receiver->d_loc;
            }
            res += formatType( t, withName ? escape(receiver->d_name) : "" );
#else
            res += "void*" + ( withName ? " " + escape(receiver->d_name) : "" );
#endif
        }
        for( int i = 0; i < pt->d_formals.size(); i++ )
        {
            if( i != 0 || receiver )
                res += ", ";
            Type* t = pt->d_formals[i]->d_type.data();
            Type* td = derefed(t);
            Pointer p;
            if( passByRef(pt->d_formals[i].data()) || td->getTag() == Thing::T_Array )
            {
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

                TODO: passing arrays by value to a function needs special treatment; the formal param is still a pointer
                even if declared as an open or fixed size array, so the caller has to make a copy and possibly remove it
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
                const QByteArray returnType = formatType(pt->d_return.data());
                return returnType + " (*" + name + ")" + formatFormals(pt); // name can legally be empty
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
            return ( t->d_union ? "union " : "struct " ) +
                    classRef(cast<Record*>(t)) + ( !name.isEmpty() ? " " + name : "" );
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

    void emitRecordDecl(Record* r)
    {
        if( r->d_slotAllocated )
            return;
        r->d_slotAllocated = true;

        const QByteArray className = classRef(r);
        h << ws() << "struct " << className << "$Class$;" << endl;
        h << ws() << "struct " << className << " {" << endl;
        level++;

        if( !r->d_unsafe )
            h << ws() << "struct " << className << "$Class$* class$;" << endl;

        QList<Field*> fields = r->getOrderedFields();

        foreach( Field* f, fields )
            f->accept(this);

        level--;
        h << "};" << endl << endl;
    }

    void emitClassDecl(Record* r)
    {
        if( r->d_unsafe )
            return;

        const QByteArray className = classRef(r);

        h << ws() << "struct " << className << "$Class$ {" << endl;
        b << ws() << "struct " << className << "$Class$ " << className << "$class$ = { " << endl;
        level++;

        if( r->d_baseRec )
            h << ws() << "struct " << classRef(r->d_baseRec) << "$Class$* super$;" << endl;
        else
            h << ws() << "void* super$;" << endl;

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
            name = formatType(pt->d_return.data(), name);
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
        const QByteArray name = moduleRef(me);
        h << "#ifndef _" << name.toUpper() << "_" << endl;
        h << "#define _" << name.toUpper() << "_" << endl << endl;

        dedication(h);

        h << "#include \"OBX.Runtime.h\"" << endl;

        dedication(b);
        b << "#include \"" << thisMod->getName() << ".h\"" << endl << endl;

        ObxCGenCollector co;
        me->accept(&co);

        foreach( Import* imp, me->d_imports )
        {
            if(imp->d_mod->d_synthetic )
                continue; // ignore SYSTEM
            h << "#include \"" << imp->d_mod->getName() << ".h\"" << endl;
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

        if( !Record::calcDependencyOrder(co.allRecords).isEmpty() )
            err->error(Errors::Generator, thisMod->d_file, 1,1, // shouldn't acutally happen since caught by validator
                         "circular record by value dependencies are not supported by C");

        foreach( Record* r, co.allRecords )
            allocRecordDecl(r);

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

        h << "void " << name << "$init$(void);" << endl;
        b << "static int initDone$ = 0;" << endl;
        b << "void " << name << "$init$(void) {" << endl;

        temps.clear();
        level++;
        b << ws() << "if(initDone$) return; else initDone$ = 1;" << endl;
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
                        b << ws() << moduleRef(thisMod)+"$"+n->d_name << ".class$ = &" << classRef(n->d_type.data()) << "$class$;" << endl;
                }
            }
        }

        foreach( const Ref<Statement>& s, me->d_body )
        {
            emitStatement(s.data());
        }
        level--;

        b << "}" << endl;

        h << "#endif" << endl;
    }

    void visit( Field* me)
    {
        h << ws() << formatType(me->d_type.data(), escape(me->d_name) ) << ";" << endl;
    }

    enum { NotStructured = 0, IsArrayPointer, IsRecord, IsArray };
    int isStructuredOrArrayPointer(Type* t)
    {
        Type* td = derefed(t);
        if( td == 0 )
            return 0;
        if( td->isStructured() )
        {
            if( td->getTag() == Thing::T_Record )
                return IsRecord;
            else
                return IsArray;
        }
        if( td->getTag() == Thing::T_Pointer )
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
        h << ws() << "extern " << formatType(me->d_type.data(), moduleRef(thisMod)+"$"+me->d_name ) << ";" << endl;
        b << ws() << formatType(me->d_type.data(), moduleRef(thisMod)+"$"+me->d_name );
        if( !isStructuredOrArrayPointer(me->d_type.data()) )
            b << " = 0";
        b << ";" << endl;
    }

    void emitStatement(Statement* s)
    {
        ObxCGenCollector2 v;
        s->accept(&v);
        foreach( ArgExpr* a, v.boundCalls )
        {
            const int temp = temps.size()+1;
            temps[ a ] = temp; // start with 1, 0 is invalid
            Named* id = a->d_sub->getIdent();
            if( id && id->getTag() == Thing::T_Procedure )
            {
                Procedure* p = cast<Procedure*>(id);
                Q_ASSERT( p->d_receiverRec );
                // we later use this variable to store the this pointer which is used to dispatch the
                // method and to pass this to the method
                b << ws() << "struct " << classRef(p->d_receiverRec) << "* $" << temp << ";" << endl;
            }
        }
        s->accept(this);
    }

    void visit( Procedure* me)
    {
        curProc = me;
        temps.clear();

        ProcType* pt = me->getProcType();
        QByteArray name = dottedName(me);
        name += formatFormals(pt,true,me->d_receiver.data());
        name = formatType(pt->d_return.data(), name);

        h << name << ";" << endl;

        b << name << " {" << endl;
        level++;
        foreach( const Ref<Named>& n, me->d_order )
        {
            switch( n->getTag() )
            {
            case Thing::T_LocalVar:
                {
                    b << ws() << formatType( n->d_type.data(), escape(n->d_name) );
                    const int f = isStructuredOrArrayPointer(n->d_type.data() );
                    if( !f )
                        b << " = 0";
                    b << ";" << endl;
                    if( f )
                        b << ws() << "memset(&" << escape(n->d_name) << ",0,sizeof(" << escape(n->d_name) << "));" << endl;
                    if( f == IsRecord )
                        b << ws() << escape(n->d_name) << ".class$ = &" << classRef(n->d_type.data()) << "$class$;" << endl;
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

        foreach( const Ref<Statement>& s, me->d_body )
        {
            emitStatement(s.data());
        }

        level--;
        b << "}" << endl << endl;
        curProc = 0;
    }

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
        case Type::LONGREAL:
            b << val.toDouble();
            break;
        case Type::NIL:
            b << "0";
            break;
        case Type::STRING:
        case Type::WSTRING:
            {
                QByteArray str = val.toByteArray();
                str.replace('\\', "\\\\");
                if( basetype == Type::STRING )
                {
                    b << "(const struct OBX$Array$1){";
                    b << str.length()+1 << ",1,";
                    b << "\"" << str << "\"";
                    b << "}";
                }else
                {
                    b << "(const struct OBX$Array$1){";
                    b << QString::fromUtf8(str).length()+1 << ",1,";
                    b << "L\"" << str << "\"";
                    b << "}";
                }
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
            b << dottedName(id);
            return;
        case Thing::T_LocalVar:
        case Thing::T_Parameter:
            b << escape(id->d_name);
            return;
        case Thing::T_NamedType:
            // NOP
            return;
        case Thing::T_Procedure:
            b << dottedName(id);
            return;
        case Thing::T_BuiltIn:
            // NOP
            return;
        default:
            qDebug() << "ERR" << id->getTag() << thisMod->d_name << me->d_loc.d_row << me->d_loc.d_col;
            Q_ASSERT( false );
            break;
        }
        Q_ASSERT( false );
    }

    static inline bool derefOrVarParam(Expression* sub)
    {
        return sub->getUnOp() == UnExpr::DEREF ||
                            ( sub->getIdent() && sub->getIdent()->getTag() == Thing::T_Parameter &&
                              cast<Parameter*>(sub->getIdent())->d_var );
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
        case Thing::T_BuiltIn:
            // NOP
            return;
        case Thing::T_Procedure:
            {
                Procedure* p = cast<Procedure*>(id);
                if( !p->d_receiver.isNull() )
                {
                    // TODO: desig to bound procedures need special treatment
                    if( derefOrVarParam(me->d_sub.data()) )
                        b << "->";
                    else
                        b << ".";
                    b << "class$->" << escape(p->d_name);
                }else
                {
                    Q_ASSERT( derefImport );
                    b << dottedName(id);
                }
            }
            return;
        case Thing::T_Variable:
            Q_ASSERT( derefImport );
            b << dottedName(id);
            return;
        case Thing::T_Field:
            if( derefOrVarParam(me->d_sub.data()) )
                b << "->";
            else
                b << ".";
            b << escape(id->d_name);
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
        default:
            qDebug() << "ERR" << thisMod->d_name << id->getTag() << me->d_loc.d_row << me->d_loc.d_col;
            Q_ASSERT( false );
            break;
        }
        Q_ASSERT( false );
    }

    void visit( UnExpr* me)
    {
        Q_ASSERT( !me->d_sub.isNull() );

        Type* prevT = derefed(me->d_sub->d_type.data());
        Q_ASSERT( prevT );

        switch( me->d_op )
        {
        case UnExpr::NEG:
            if( prevT->getBaseType() == BaseType::SET )
                b << "^(";
            else
                b << "-(";
            me->d_sub->accept(this);
            b << ")";
            return;
        case UnExpr::NOT:
            b << "!(";
            me->d_sub->accept(this);
            b << ")";
            return;
        case UnExpr::DEREF:
            me->d_sub->accept(this);
            // is handled by owner
            return;
        case UnExpr::ADDROF:
            b << "&(";
            me->d_sub->accept(this);
            b << ")";
            return;
        default:
            qDebug() << "ERR" << me->d_op << thisMod->d_name << me->d_loc.d_row << me->d_loc.d_col;
            Q_ASSERT( false );
            break;
        }
        Q_ASSERT( false );
    }

    void emitBuiltIn( BuiltIn* bi, ArgExpr* ae )
    {
        switch( bi->d_func )
        {
        case BuiltIn::PRINTLN:
            {
                Q_ASSERT( ae->d_args.size() == 1 );
                Type* td = derefed(ae->d_args.first()->d_type.data());
                b << "printf(";
                bool wide = false;
                bool deref = false;
                bool addr = false;
                if( td->isText(&wide) )
                {
                    if( td->isChar() )
                    {
                        if( wide )
                            b << "\"%lc\\n\"";
                        else
                            b << "\"%c\\n\"";
                    }else
                    {
                        if( wide )
                            b << "\"%ls\\n\"";
                        else
                            b << "\"%s\\n\"";
                        deref = true;
                    }
                }else if( td->isInteger() )
                {
                    if( td->getBaseType() <= Type::INTEGER )
                        b << "\"%d\\n\"";
                    else
                        b << "\"%ld\\n\"";
                }else if( td->isReal() )
                    b << "\"%f\\n\"";
                else if( td->isSet() )
                    b << "\"%x\\n\"";
                else if( td->getBaseType() == Type::BOOLEAN )
                    b << "\"%d\\n\"";
                else
                {
                    switch(td->getTag())
                    {
                    case Thing::T_Enumeration:
                        b << "\"%u\\n\"";
                        break;
                    case Thing::T_Pointer:
                        if( cast<Pointer*>(td)->d_to->derefed()->getTag() == Thing::T_Array )
                            deref = true;
                        b << "\"%p\\n\"";
                        break;
                    case Thing::T_Array:
                        deref = true;
                        b << "\"%p\\n\"";
                        break;
                    case Thing::T_Record:
                        addr = true;
                        b << "\"%p\\n\"";
                        break;
                    default:
                        b << "\"%p\\n\"";
                        break;
                    }
                }
                b << ",";
                if( addr )
                    b << "&";
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
                    Array* a = cast<Array*>(td);
                    QList<Array*> dims = a->getDims();
                    b << "OBX$NewArr(&"; // does also memset
                    renderArg(0,ae->d_args.first().data(),false);
                    b << ", " << dims.size() << ", sizeof(" << formatType(dims.last()->d_type.data()) << ")";
                    for( int i = 0; i < dims.size(); i++ )
                    {
                        if( dims[i]->d_lenExpr.isNull() )
                            break;
                        b << "," << dims[i]->d_len;

                    }
                    for( int i = 1; i < ae->d_args.size(); i++ )
                    {
                        b << ",";
                        ae->d_args[i]->accept(this);
                    }
                    b << ")";
                }else
                {
                    Q_ASSERT( td->getTag() == Thing::T_Record );
                    Q_ASSERT( ae->d_args.size() == 1 );

                    renderArg(0,ae->d_args.first().data(),false);
                    b << " = OBX$NewRec(sizeof(" << formatType(td) << "),";
                    b << ws() << classRef(t) << "$class$";
                    b << ");";
                }
            }
            break;
        default:
             qWarning() << "missing generator implementation of" << BuiltIn::s_typeName[bi->d_func];
             break;
        }
    }

    void renderArg( Type* lhs, Expression* rhs, bool byRef )
    {
        // this method takes care that all arrays are rendered as OBX$Array
        Type* tf = derefed(lhs);
        Type* ta = derefed(rhs->d_type.data());
        if( ta->isString() )
        {
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
        }else if( ta->getTag() == Thing::T_Array )
        {
            // NOTE: a copy if not passByRef done in procedure
            const int tag = rhs->getTag();
            if( rhs->getUnOp() == UnExpr::DEREF )
            {
                UnExpr* e = cast<UnExpr*>(rhs);
                e->d_sub->accept(this); // the thing before DEREF is a pointer
            }else if( tag == Thing::T_BinExpr // happens if strings are added
                      || tag == Thing::T_Literal // a string or bytearray literal is always OBX$Array
                      || rhs->getUnOp() == UnExpr::CALL  // a call always returns an OBX$Array
                      || ( rhs->getIdent() && rhs->getIdent()->getTag() == Thing::T_Parameter ) // a param is always OBX$Array
                      )
            {
                rhs->accept(this);
            }else if( rhs->getUnOp() == UnExpr::IDX )
            {
                // a n-dim array is partly indexed, i.e. not to the element, but an m-dim array of elements
                UnExpr* e = cast<UnExpr*>(rhs);
                int dim = 1;
                while( e->d_sub->getUnOp() == UnExpr::IDX )
                {
                    e = cast<UnExpr*>(e->d_sub.data());
                    dim++;
                }
                Type* td = derefed(e->d_sub->d_type.data());
                Q_ASSERT( td->getTag() == Thing::T_Array );
                Array* a = cast<Array*>(td);
                QList<Array*> dims = a->getDims();
                b << "(" << arrayType(dims.size()-dim, rhs->d_loc) << "){";
                for(int i = 0; i < (dims.size()-dim); i++ )
                    b << dims[i+dim]->d_len << ","; // TODO: dynamic len
                b << "1,&"; // 1 because we point in a slice here, not the start of the array
                rhs->accept(this); // rhs because we want all IDX up to here rendered
                b << "}";
            }else
            {
                Q_ASSERT(tag == Thing::T_IdentLeaf || Thing::T_IdentSel );
                // It's a array value, convert it to an array pointer
                Array* a = cast<Array*>(ta);
                QList<Array*> dims = a->getDims();
                b << "(" << arrayType(dims.size(), rhs->d_loc) << "){";
                for(int i = 0; i < dims.size(); i++ )
                    b << dims[i]->d_len << ","; // TODO: dynamic len
                b << "1,&";
                rhs->accept(this);
                b << "}";
            }
        }else
        {
            if( byRef )
                b << "&";
            rhs->accept(this);
        }
    }

    void emitActuals( ProcType* pt, ArgExpr* me )
    {
        Q_ASSERT( pt->d_formals.size() <= me->d_args.size() );
        for( int i = 0; i < pt->d_formals.size(); i++ )
        {
            if( i != 0 )
                b << ", ";
            Parameter* p = pt->d_formals[i].data();
            renderArg( p->d_type.data(), me->d_args[i].data(), passByRef(p) );
        }
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

        int temp = 0;
        if( pt->d_typeBound && func && !superCall ) // TODO: delegates
        {
            //  ( pointer ^ | record ) .method (args)
            Q_ASSERT(me->d_sub->getTag() == Thing::T_IdentSel );
            IdentSel* method = cast<IdentSel*>(me->d_sub.data());
            Type* td = derefed(method->d_sub->d_type.data());
            Q_ASSERT(td && td->getTag() == Thing::T_Record);
            temp = temps[me];
            Q_ASSERT(temp);
            b << "($" << temp << " = ";
            if( method->d_sub->getUnOp() != UnExpr::DEREF )
                b << "&";
            b << "(";
            method->d_sub->accept(this);
            b << "))->class$->" << escape( method->getIdent()->d_name) << "($" << temp << ", ";
            emitActuals(pt,me);
            b << ")";
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
#if 0
                Type* subT = derefed(me->d_sub->d_type.data());
                Q_ASSERT( subT && subT->getTag() == Thing::T_Array);
                b << "(";
                renderArg(0,me->d_sub.data(),false);
                b << ").$a"; // TODO: cast? shortcut if array value?
                b << "[";
                me->d_args.first()->accept(this);
                b << "]";
#else
                ArgExpr* e = me;
                QList<ArgExpr*> dims;
                dims.prepend(e);
                while( e->d_sub->getUnOp() == UnExpr::IDX )
                {
                    e = cast<ArgExpr*>(e->d_sub.data());
                    dims.prepend(e);
                }
                Type* td = derefed(e->d_sub->d_type.data());
                Q_ASSERT( td->getTag() == Thing::T_Array );
                b << "((" << formatType(me->d_type.data()) << QByteArray(dims.size(),'*') << ")(";
                renderArg(0,e->d_sub.data(),false);
                b << ").$a)";
                for(int i = 0; i < dims.size(); i++ )
                {
                    b << "[";
                    Q_ASSERT( dims[i]->d_args.size() == 1 );
                    dims[i]->d_args.first()->accept(this);
                    b << "]";
                }
#endif
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
                    b << "(struct " << classRef(me->d_type->toRecord()) << "*)(";
                    me->d_sub->accept(this);
                    b << ")";
                }else
                {
                    b << "*(struct " << classRef(me->d_type->toRecord()) << "*)&(";
                    me->d_sub->accept(this);
                    b << ")";
                }
            }
            break;
        }
    }

    void emitBinOp(BinExpr* me, const char* op)
    {
        b << "(";
        me->d_lhs->accept(this);
        b << " " << op << " ";
        me->d_rhs->accept(this);
        b << ")";
    }

    void emitStringOp( Type* lhs, Type* rhs, bool lwide, bool rwide, int op, BinExpr* e )
    {
        b << "OBX$StrOp(&(";
        renderArg(lhs,e->d_lhs.data(),false);
        b << ")," << int(lwide) << ",&(";
        renderArg(rhs,e->d_rhs.data(),false);
        b << ")," << int(rwide) << ",";
        b << op << ")";
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
            me->d_rhs->accept(this);
            b << ( rtag == Thing::T_Record ? "." : "->" );
            b << "class$,";
            me->d_lhs->accept(this);
            b << ( ltag == Thing::T_Record ? "." : "->" );
            b << "class$)";
            break;
        case BinExpr::ADD:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) )
                emitBinOp(me,"+");
            else if( lhsT->isSet() && rhsT->isSet() )
                emitBinOp(me,"|");
            else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) && !lhsT->d_unsafe && !rhsT->d_unsafe )
            {
                b << "OBX$StrJoin(&(";
                renderArg(lhsT,me->d_lhs.data(),false);
                b << ")," << int(lwide) << ",&(";
                renderArg(rhsT,me->d_rhs.data(),false);
                b << ")," << int(rwide) << ")";
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
                    ( lhsT->isChar() && rhsT->isChar() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
                    ( ( lhsT->getBaseType() == Type::NIL || ltag == Thing::T_Pointer || ltag == Thing::T_ProcType ) &&
                    ( rhsT->getBaseType() == Type::NIL || rtag == Thing::T_Pointer || rtag == Thing::T_ProcType ) ) )
            {
                emitBinOp(me,"==");
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
                    ( lhsT->isChar() && rhsT->isChar() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
                    ( ( lhsT->getBaseType() == Type::NIL || ltag == Thing::T_Pointer || ltag == Thing::T_ProcType ) &&
                    ( rhsT->getBaseType() == Type::NIL || rtag == Thing::T_Pointer || rtag == Thing::T_ProcType ) ) )
            {
                emitBinOp(me,"!=");
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                emitStringOp(lhsT, rhsT, lwide, rwide, 2, me );
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::LT:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
                    ( lhsT->isChar() && rhsT->isChar() ) )
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
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
                    ( lhsT->isChar() && rhsT->isChar() ) )
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
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
                    ( lhsT->isChar() && rhsT->isChar() ) )
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
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
                    ( lhsT->isChar() && rhsT->isChar() ) )
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

    // TODO
    void visit( SetExpr* ) {}

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
                b << "OBX$StrCopy(&";
                renderArg(tl, me->d_lhs.data(),false);
                b << "," << int(lwide) << ",&";
                renderArg(tl, me->d_rhs.data(),false);
                b << "," << int(rwide) << ")";
            }else
            {
                b << "OBX$ArrCopy(&";
                renderArg(tl, me->d_lhs.data(),false);
                b << ",&";
                renderArg(tl, me->d_rhs.data(),false);
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
            qWarning() << "CASE ELSE not yet implemented" << me->d_loc.d_row << me->d_loc.d_col << thisMod->d_file;
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
    }

    void emitIf( IfLoop* me)
    {
        b << ws() << "if( ";
        me->d_if[0]->accept(this);
        b << " ) {" << endl;
        level++;
        for( int i = 0; i < me->d_then[0].size(); i++ )
            emitStatement(me->d_then[0][i].data());
        level--;
        b << ws() << "} ";
        for( int i = 1; i < me->d_if.size(); i++ ) // ELSIF
        {
            b << "else if( ";
            me->d_if[i]->accept(this);
            b << " ) {" << endl;
            level++;
            for( int j = 0; j < me->d_then[i].size(); j++ )
                emitStatement(me->d_then[i][j].data());
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
            emitIf(me);
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
                emitIf(me);
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

static bool copyFile( const QDir& outDir, const QByteArray& name )
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
    return true;
}

bool Obx::CGen2::translateAll(Obx::Project* pro, bool debug, const QString& where)
{
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
    QTextStream cout(&clearStr);

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
                        QFile b(outDir.absoluteFilePath(inst->getName() + ".c"));
                        if( b.open(QIODevice::WriteOnly) )
                        {
                            QFile h(outDir.absoluteFilePath(inst->getName() + ".h"));
                            if( h.open(QIODevice::WriteOnly) )
                            {
                                //qDebug() << "generating C for" << m->getName() << "to" << f.fileName();
                                if( !CGen2::translate(&h, &b, inst,debug,pro->getErrs()) )
                                {
                                    qCritical() << "error generating C for" << inst->getName();
                                    return false;
                                }
                                bout << "./ilasm /dll " << "\"" << inst->getName() << ".il\"" << endl;
                                cout << "rm \"" << inst->getName() << ".il\"" << endl;
                                cout << "rm \"" << inst->getName() << ".dll\"" << endl;
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
        const QByteArray name = "main$";
        QByteArrayList roots;
        for(int i = mods.size() - 1; i >= 0; i-- )
        {
            if( mods[i]->d_usedBy.isEmpty() )
                roots.append(mods[i]->getName());
        }
        if( roots.isEmpty() )
            roots.append(mods.last()->getName()); // shouldn't actually happenk

        QFile f(outDir.absoluteFilePath(name + ".c"));
        if( f.open(QIODevice::WriteOnly) )
        {
            const Project::ModProc& mp = pro->getMain();
            if( mp.first.isEmpty() )
                CGen2::generateMain(&f,roots);
            else
                CGen2::generateMain(&f,mp.first, mp.second);
            bout << "cc /exe " << "\"" << name << ".c\"" << endl;
            cout << "rm \"" << name << ".c\"" << endl;
            cout << "rm \"" << name << ".o\"" << endl;
        }else
            qCritical() << "could not open for writing" << f.fileName();
    }

#if 0
    if( pro->useBuiltInOakwood() ) // TODO
    {
        copyFile(outDir,"In");
        copyFile(outDir,"Out");
        copyFile(outDir,"Files");
        copyFile(outDir,"Input");
        copyFile(outDir,"Math");
        copyFile(outDir,"MathL");
        copyFile(outDir,"Strings");
        copyFile(outDir,"Coroutines");
        copyFile(outDir,"XYplane");
    }
    copyFile(outDir,"OBX.Runtime.h");
    copyFile(outDir,"OBX.Runtime.c");
#endif

    bout.flush();
    cout.flush();

    QFile build( outDir.absoluteFilePath( "build.sh" ) );
    if( !build.open(QIODevice::WriteOnly) )
    {
        qCritical() << "could not open for writing" << build.fileName();
        return false;
    }else
        build.write(buildStr);
    QFile clear( outDir.absoluteFilePath( "clean.sh" ) );
    if( !clear.open(QIODevice::WriteOnly) )
    {
        qCritical() << "could not open for writing" << clear.fileName();
        return false;
    }else
        clear.write(clearStr);
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

bool CGen2::generateMain(QIODevice* to, const QByteArray& callMod, const QByteArray& callFunc)
{
    if( callMod.isEmpty() )
        return false;

    QTextStream out(to);
    ObxCGenImp::dedication(out);

    //out << "#include \"OBX$Runtime.h\"" << endl;
    out << "#include \"" << callMod << ".h\"" << endl << endl;

    out << "int main(void) {" << endl;
    out << "    " << callMod + "init$();" << endl;
    if( !callFunc.isEmpty() )
        out << "    " << callMod + "$" + callFunc + "();" << endl;
    out << "    return 0;" << endl;
    out << "}" << endl;
    return true;
}

bool CGen2::generateMain(QIODevice* to, const QByteArrayList& callMods)
{
    Q_ASSERT( to );
    if( callMods.isEmpty() )
        return false;

    QTextStream out(to);
    ObxCGenImp::dedication(out);

    //out << "#include \"OBX$Runtime.h\"" << endl;
    foreach( const QByteArray& mod, callMods )
        out << "#include \"" << mod << ".h\"" << endl;
    out << endl;

    out << "int main(void) {" << endl;

    foreach( const QByteArray& mod, callMods )
    {
        out << "    " << mod + "$init$();" << endl;
    }
    out << "    return 0;" << endl;
    out << "}" << endl;
    return true;
    return true;
}
