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

#include "ObxPelibGen.h"
#include "ObRowCol.h"
#include "ObErrors.h"
#include "ObxModel.h"
#include <PeLib/SimpleApi.h>
#include <QDir>
#include <QtDebug>
using namespace Obx;
using namespace Ob;

struct ObxPelibGenImp : public AstVisitor
{
    struct Collector : public AstVisitor
    {
        QList<Procedure*> allProcs; // only static procs, i.e. not bound to a record
        QList<Record*> allRecords;
        QList<ProcType*> allProcTypes;
        QList<QualiType*> allAliasses;

        void collect(Procedure* p, Type* t, bool isTypedef )
        {
            Q_ASSERT( p != 0 || t != 0 );
            if( p )
            {
                if( p->d_receiver.isNull() )
                    allProcs.append(p);
                p->accept(this);
                return;
            }

            const int tag = t->getTag();
            if( tag == Thing::T_Array )
            {
                collect(0,cast<Array*>(t)->d_type.data(), false);
                return;
            }
            if( isTypedef && tag == Thing::T_QualiType )
            {
                // this is B in "type A = B"
                // QualiTypes pointing to a record or pointer to record get their own slot in the module table
                if( t->toRecord() )
                    allAliasses.append( cast<QualiType*>(t) );
                return;
            }
            if( tag == Thing::T_ProcType )
            {
                ProcType* pt = cast<ProcType*>(t);
                Q_ASSERT( pt->d_decl == 0 || pt->d_decl->getTag() != Thing::T_Procedure ); // we only want proc pointer types
                allProcTypes.append(pt);
                foreach( const Ref<Parameter>& p, pt->d_formals )
                    collect(0,p->d_type.data(),false);
                // return type is quali; we don't follow qualies here
                return;
            }
            if( tag == Thing::T_Pointer )
                t = cast<Pointer*>(t)->d_to.data(); // no deref, intentionally
            if( t && t->getTag() == Thing::T_Record )
            {
                Record* r = cast<Record*>(t);
                allRecords.append( r );
                foreach( const Ref<Field>& f, r->d_fields )
                    collect(0,f->d_type.data(), false);
            }
        }

        void visit( Module* me )
        {
            foreach( const Ref<Named>& n, me->d_order )
            {
                const int tag = n->getTag();
                if( tag == Thing::T_Procedure )
                    collect(cast<Procedure*>(n.data()),0,false);
                else if( tag == Thing::T_NamedType )
                    collect(0,n->d_type.data(), true);
                else if( tag == Thing::T_Variable )
                    collect(0,n->d_type.data(), false);
            }
        }

        void visit( Procedure* me)
        {
            foreach( const Ref<Named>& n, me->d_order )
            {
                const int tag = n->getTag();
                if( tag == Thing::T_Procedure )
                    collect(cast<Procedure*>(n.data()),0,false);
                else if( tag == Thing::T_NamedType )
                    collect(0,n->d_type.data(), true);
                else if( tag == Thing::T_LocalVar )
                    collect(0,n->d_type.data(), false);
                else if( tag == Thing::T_Parameter )
                {
                    Parameter* p = cast<Parameter*>(n.data());
                    Q_ASSERT( !p->d_receiver );
                    collect(0, p->d_type.data(), false);
                }
            }
            // return type is quali; we don't follow qualies here
        }
    };

    DotNetPELib::SimpleApi out;
    quint32 anonRecord;
    Ob::Errors* err;
    Module* curMod;
    QSet<Module*> imports;
    bool ownsErr;

    ObxPelibGenImp():err(0),ownsErr(false),anonRecord(0),curMod(0) {}

    // NOTE: arbitrary order of declarations in OBX collides with dependencies in PeLib; we thus apply the concept
    // described e.g. in Lidin's "Inside Microsoft .Net Il Assembler (2002)" section "Forward Declaration of Classes"
    void visit( Module* me)
    {
        curMod = me;

        // A Module is a top-level class with all record types as nested classes; otherwise we could not merge more
        // than one OBX module in an assembly.
        for( int i = 0; i < me->d_fullName.size() - 1; i++ )
            out.beginNamespace(me->d_fullName[i]);
        out.beginClass( me->d_name, true ); // this is the module

        Collector coll;
        coll.visit(me);

        // go through all record types and generate .net classes without members first (doing class augmentation later)
        for( int i = 0; i < coll.allRecords.size(); i++ )
        {
            // for this version each OBX record inherits from .net object; later versions will sort out the ones suitable as values
            out.beginClass( className(coll.allRecords[i] ) );
            out.endClass();
        }

        // go through all procedure types (not procedures!) and generate corresponding delegate headers
        for( int i = 0; i < coll.allProcTypes.size(); i++ )
        {
            out.beginClass( className(coll.allProcTypes[i] ), true, "System.MulticastDelegate" );
            out.beginMethod( ".ctor", true, DotNetPELib::SimpleApi::Instance, true );
            out.addArgument("object");
            out.addArgument("uint");
            out.endMethod();
            out.endClass();
        }
        // now go through all procedure types and add Invoke method with args (whose types are all known now)
        for( int i = 0; i < coll.allProcTypes.size(); i++ )
        {
            ProcType* pt = coll.allProcTypes[i];
            out.openClass( className( pt ) );
            out.beginMethod( "Invoke", true, DotNetPELib::SimpleApi::Virtual, true );
            for( int j = 0; j < pt->d_formals.size(); j++ )
                out.addArgument( typeQuali(pt->d_formals[j]->d_type.data()));
            out.endMethod();
            out.endClass();
        }

        // go through all record types and generate fields and method headers
        for( int i = 0; i < coll.allRecords.size(); i++ )
        {
            Record* r = coll.allRecords[i];
            out.openClass( className(r) );
            if( r->d_baseRec )
                out.setSuperClass( className( r->d_baseRec ) );
            for( int j = 0; j < r->d_fields.size(); j++ )
                r->d_fields[j]->accept(this);
            for( int j = 0; j < r->d_methods.size(); j++ )
            {
                Procedure* p = r->d_methods[j].data();
                while( p->d_super )
                    p = p->d_super; // use the original signature since dotnet doesn't directly support covariance
                out.beginMethod( p->d_name, true, DotNetPELib::SimpleApi::Virtual );
                ProcType* pt = p->getProcType();
                if( pt->d_return )
                    out.setReturnType( typeQuali(pt->d_return.data()) );
                for( int k = 0; k < pt->d_formals.size(); k++ )
                    out.addArgument( typeQuali( pt->d_formals[k]->d_type.data() ) +
                                     ( pt->d_formals[k]->isVarParam() ? "&" : "" ),
                                     pt->d_formals[k]->d_name );
                out.endMethod();
            }
            out.endClass();
        }

        // declare module variables
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Variable )
                n->accept(this);
        }

        // go through all static procedures and generate headers
        for( int i = 0; i < coll.allProcs.size(); i++ )
        {
            Procedure* p = coll.allProcs[i];
            out.beginMethod( p->d_name, true, DotNetPELib::SimpleApi::Static );
            ProcType* pt = p->getProcType();
            if( pt->d_return )
                out.setReturnType( typeQuali(pt->d_return.data()) );
            for( int k = 0; k < pt->d_formals.size(); k++ )
            {
                QByteArray type = typeQuali( pt->d_formals[k]->d_type.data() );
                out.addArgument( type + ( pt->d_formals[k]->isVarParam() ? "&" : "" ),
                                 pt->d_formals[k]->d_name );
            }
            out.endMethod();
        }

        // emit code for all procedures (headers were already created)
        foreach( Record* r, coll.allRecords )
        {
            out.openClass( className(r) );
            for( int j = 0; j < r->d_methods.size(); j++ )
                r->d_methods[j]->accept(this);
            out.endClass();
        }
        foreach( Procedure* p, coll.allProcs )
            p->accept(this);

        // generate module level initializer (for variables and the begin part)
        out.beginMethod( "..ctor", true, DotNetPELib::SimpleApi::Static );
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Variable )
                ; // TODO emitInitializer(n.data());
        }
        foreach( const Ref<Statement>& s, me->d_body )
            s->accept(this);
        out.RET();
        out.endMethod();

        out.endClass(); // this ends the module
        for( int i = 0; i < me->d_fullName.size() - 1; i++ )
            out.endNamespace();
    }

    void visit( Procedure* me )
    {
        // set param slots
        ProcType* pt = me->getProcType();
        int off = 0;
        if( !me->d_receiver.isNull() )
            off = 1; // this is slot 0
        for( int i = 0; i < pt->d_formals.size(); i++ )
            pt->d_formals[i]->d_slot = i + off;

        out.openMethod( me->d_name );
        // emit local vars
        int count = 0;
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_LocalVar )
            {
                n->d_slot = count++;
                out.addLocal( typeQuali(n->d_type.data()), n->d_name );
                ; // TODO emitInitializer(n.data());
            }
        }

        // emit body
        foreach( const Ref<Statement>& s, me->d_body )
            s->accept(this);
        out.RET();
        out.endMethod();
    }

    void visit( Field* me)
    {
        out.addField( me->d_name, typeQuali( me->d_type.data() ) );
    }

    void visit( Variable* me)
    {
        out.addField( me->d_name, typeQuali( me->d_type.data() ), true, true );
    }

    void visit( LocalVar* ) {}
    void visit( NamedType* ) {}
    void visit( Const* ) {}
    void visit( Import* ) {}
    void visit( Call* ) {}
    void visit( Return* ) {}
    void visit( Assign* ) {}
    void visit( IfLoop* ) {}
    void visit( ForLoop* ) {}
    void visit( CaseStmt* ) {}
    void visit( Literal* ) {}
    void visit( SetExpr* ) {}
    void visit( IdentLeaf* ) {}
    void visit( UnExpr* ) {}
    void visit( IdentSel* ) {}
    void visit( ArgExpr* ) {}
    void visit( BinExpr* ) {}
    void visit( Enumeration* ) {}
    void visit( Exit* ) {}

    // Unused
    void visit( Parameter* ) {}
    void visit( ProcType* ) {}
    void visit( BaseType* ) {}
    void visit( BuiltIn* ) {}
    void visit( Pointer* ) {}
    void visit( Array* ) {}
    void visit( Record* ) {}
    void visit( QualiType* ) {}

    // pending
    void visit( GenericName* ) {}

    ///// Utilities

    QByteArray className( Type* r, bool asRef = false )
    {
        Named* n = r->findDecl(false);
        if( n && n->getTag() == Thing::T_NamedType )
        {
            QByteArray name = n->d_name;
            Scope* s = n->d_scope;
            while( s && s->getTag() == Thing::T_Procedure )
            {
                name = s->d_name + "$" + name;
                s = s->d_scope;
            }
            Q_ASSERT( s->getTag() == Thing::T_Module );
            if( asRef )
            {
                if( s != curMod  )
                    imports.insert( cast<Module*>(s) );
                name = s->getName() + "." + name;
            }
            return name;
        }else
        {
            if( r->d_slot == 0 )
                r->d_slot = ++anonRecord;
            return "$" + QByteArray::number(r->d_slot);
        }
    }

    QByteArray typeQuali( Type* t )
    {
        t = derefed(t);
        switch( t->getTag() )
        {
        case Thing::T_Record:
        case Thing::T_ProcType:
            return className(t, true );
        case Thing::T_Pointer:
            return typeQuali( cast<Pointer*>(t)->d_to.data() );
        case Thing::T_Array:
            {
                Array* a = cast<Array*>(t);
                const QList<Array*> dims = a->getDims();
                return typeQuali(dims.last()->d_type.data()) + "[" + QByteArray( dims.size() - 1, ',' ) + "]";
            }
        case Thing::T_BaseType:
            switch( t->d_baseType )
            {
            case Type::BOOLEAN:
                return "bool";
            case Type::CHAR:
                return "uint8";
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
                return "uint32";
            default:
                Q_ASSERT( false );
            }
            break;
        case Thing::T_Enumeration:
            // TODO
            qWarning() << "typeQuali Enum not implemented";
            break;
        default:
            Q_ASSERT( false );
        }
    }

    void emitCreateRecord( Type* t, const RowCol& loc )
    {
        Record* r = t->toRecord();
        Q_ASSERT( r );

        out.NEWOBJ( typeQuali(t) + "::.ctor" );
    }

    void emitCreateArray( Array* a, int dynDims, const RowCol& loc )
    {
        QList<Array*> dims = a->getDims();
        Type* baseType = dims.last()->d_type.data();
        baseType = derefed(baseType);
        Q_ASSERT( baseType );

        if( dims.size() == 1 )
        {
            if( dims.first()->d_len )
                out.LDC((qint32)dims.first()->d_len);
            else
                Q_ASSERT( dynDims == 1 ); // len is on stack
            out.NEWARR( typeQuali(baseType) );
        }else
        {
            out.LDTOKEN( typeQuali(baseType) );
            out.CALL("System.Type::GetTypeFromHandle(System.RuntimeTypeHandle)");

            // NOTE: apparently DotNet doesn't support duplicating stack entries which are not top; if the variable number
            // of lenths were pushed on the stack to call this method I would have to fetch them to temporary locals
            // before to achieve the correct order of arguments for CreateInstance; this is unpractical.

            // LDC x
            // LDC y

            // CALL("System.Array::CreateInstance(System.Type, int32, int32 )");
        }

#if 0
        if( baseType->getBaseType() > 0 )
        {
            if( constLen > 0 )
                bc.KSET(tmp+1, constLen, loc.packed() );
            else
                bc.MOV(tmp+1, lenSlot, loc.packed() );
            bc.CALL(tmp,1,1,loc.packed());
            bc.MOV(to,tmp,loc.packed());
            ctx.back().sellSlots(tmp,2);
        }else
        {
            bc.TNEW( to, constLen, 0, loc.packed() );
            if( constLen > 0 )
            {
                const int tmp = ctx.back().buySlots(1);
                bc.KSET( tmp, constLen, loc.packed() );
                bc.TSET(tmp,to,"count",loc.packed());
                ctx.back().sellSlots(tmp);
            }else
            {
                bc.TSET(lenSlot,to,"count",loc.packed());
            }
        }
#endif
    }

    bool emitInitializer( Type* t, const RowCol& loc, const QList<Expression*>& lengths = QList<Expression*>() )
    {
        // generate an initializer if necessary; the generated object will be pushed to the stack;
        // returns true if an initializer is generated, otherwise false.


        // expects non-derefed t!
        Type* td = derefed(t);
        Q_ASSERT( td );
#if 0
        switch( td->getTag() )
        {
        case Thing::T_Record:
            {
                Record* r = cast<Record*>(td);
                emitCreateRecord( to, t, loc );

                // initialize structured fields
                int tmp = ctx.back().buySlots(1);
                QList<Field*> fields = r->getOrderedFields();
                for( int i = 0; i < fields.size(); i++ )
                {
                    Type* t2 = fields[i]->d_type.data();
                    Type* t2d = derefed(t2);
                    Q_ASSERT( t2d );
                    const int tag = t2d->getTag();
                    if( tag == Thing::T_Record || tag == Thing::T_Array )
                    {
                        emitInitializer(tmp, t2, loc );
                        emitSetTableByIndex(tmp,to,fields[i]->d_slot,loc);
                    }
                }
                ctx.back().sellSlots(tmp);
            }
            break;
        case Thing::T_Array:
            {
                Array* a = cast<Array*>(td);
                Type* td = derefed(a->d_type.data());

                int lenSlot = -1;
                if( !lengths.isEmpty() )
                {
                    // len comes from NEW( ptr, expression ) instead of compile time constant
                    Q_ASSERT( a->d_lenExpr.isNull() );
                    lengths.first()->accept(this);
                    Q_ASSERT( !slotStack.isEmpty() );
                    lenSlot = slotStack.back();
                    emitCreateArray(to, td, 0, lenSlot, loc );
                    slotStack.pop_back(); // keep lenSlot for now
                }else
                {
                    Q_ASSERT( !a->d_lenExpr.isNull() );
                    emitCreateArray(to, td, a->d_len, 0, loc );
                }

                if( td->isStructured() )
                {
                    quint8 base = ctx.back().buySlots(4);
                    const int tmp = ctx.back().buySlots(1);
                    bc.KSET(base,0,loc.packed());
                    if( !lengths.isEmpty() )
                    {
                        bc.SUB(lenSlot, lenSlot, QVariant(1), loc.packed() );
                        bc.MOV(base+1, lenSlot, loc.packed() );
                    }else
                        bc.KSET(base+1, a->d_len-1,loc.packed());

                    if( lenSlot >= 0 )
                    {
                        ctx.back().sellSlots(lenSlot); // free as soon as possible, i.e. before recursion
                        lenSlot = -1;
                    }

                    bc.KSET(base+2,1,loc.packed());
                    bc.FORI(base,0,loc.packed());
                    const quint32 pc = bc.getCurPc();

                    if( lengths.size() > 1 )
                        emitInitializer(tmp, a->d_type.data(), loc, lengths.mid(1) );
                    else
                        emitInitializer(tmp, a->d_type.data(), loc );
                    bc.TSET(tmp,to,base+3, loc.packed() );

                    bc.FORL(base, pc - bc.getCurPc() - 1,loc.packed());
                    bc.patch(pc);

                    ctx.back().sellSlots(tmp);
                    ctx.back().sellSlots(base,4);
                }
                if( lenSlot >= 0 )
                    ctx.back().sellSlots(lenSlot);
            }
            break;
        }
#endif
    }

    static inline Type* derefed( Type* t )
    {
        if( t )
            return t->derefed();
        else
            return 0;
    }
};

bool PelibGen::translate(Model* mdl, const QString& outdir, bool strip, Ob::Errors* err)
{
    Q_ASSERT( mdl );
    //Q_ASSERT( RowCol::COL_BIT_LEN == JitComposer::COL_BIT_LEN && RowCol::ROW_BIT_LEN == JitComposer::ROW_BIT_LEN );

    int errs = 0;
    foreach( Module* m, mdl->getDepOrder() )
    {
        if( m->d_isDef )
            ; // NOP
        else if( !translate(m, outdir, false, strip, err) ) // TODO isPrimary
            errs++;
    }
    return errs == 0;
}

bool PelibGen::translate(Module* m, const QString& outdir, bool isPrimary, bool strip, Ob::Errors* errs)
{
    Q_ASSERT( m != 0 && !outdir.isEmpty() );

    if( m->d_hasErrors )
        return false;

    if( m->d_isDef )
        return true;

    ObxPelibGenImp imp;
    /*
    imp.bc.setUseRowColFormat(true);
    imp.bc.setStripped(strip);
    imp.thisMod = m;
    imp.stripped = strip;
    */

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


    const QByteArray modName = m->getName();
    const QString fileName( QDir(outdir).absoluteFilePath( modName + ( isPrimary ? ".exe" : ".dll" ) ) );

    bool hasErrs = false;
    try
    {
        imp.out.beginModule( m->d_name );
        imp.out.addModuleReference("mscorlib");

        m->accept(&imp);

        hasErrs = ( imp.err->getErrCount() - errCount ) != 0;

        //imp.bc.setStripped(true);
        if( !hasErrs )
        {
            imp.out.writeByteCode( fileName.toUtf8() );
            //imp.out.writeAssembler( fileName.toUtf8() + ".il" ); // TEST
            qDebug() << "wrote bytecode to" << fileName;
        }

        imp.out.endModule();
#if 1
    }catch( const std::runtime_error& e )
    {
        imp.err->error(Errors::Generator, m->d_file, 1, 1, e.what() );
        hasErrs = true;
    }
#endif

    if( imp.ownsErr )
        delete imp.err;
    return hasErrs;
}
