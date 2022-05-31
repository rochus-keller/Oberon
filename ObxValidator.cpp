/*
* Copyright 2020 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the OBX parser/code model library.
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

#include "ObxEvaluator.h"
#include "ObxValidator.h"
#include <QtDebug>
using namespace Obx;
using namespace Ob;

struct ValidatorImp : public AstVisitor
{
    struct VlaChecker : public AstVisitor
    {
        Scope* scope;
        Errors* err;
        VlaChecker(Scope* s, Errors* e):scope(s),err(e) {}
        void visit( SetExpr* me)
        {
            foreach( const Ref<Expression>& e, me->d_parts )
                e->accept(this);
        }
        void visit( IdentLeaf* me)
        {
            Named* id = me->getIdent();
            if( id && id->d_scope == scope && id->getTag() == Thing::T_LocalVar )
                err->error(Errors::Semantics,Loc(me->d_loc,scope->getModule()->d_file),
                           Validator::tr("variable array length cannot depend on local variable"));
        }
        void visit( UnExpr* me)
        {
            me->d_sub->accept(this);
        }
        void visit( IdentSel* me)
        {
            me->d_sub->accept(this);
        }
        void visit( ArgExpr* me)
        {
            me->d_sub->accept(this);
            foreach( const Ref<Expression>& e, me->d_args )
                e->accept(this);
        }
        void visit( BinExpr* me)
        {
            me->d_lhs->accept(this);
            me->d_rhs->accept(this);
        }
    };

    Errors* err;
    Module* mod;
    Instantiator* insts;
    Validator::BaseTypes bt;
    struct Level
    {
        Scope* scope;
        QList<IfLoop*> loops;
        Level(Scope* s):scope(s){}
    };

    QList<Level> levels;
    Type* curTypeDecl;
    Statement* prevStat;
    QSet<Const*> constTrace;
    QList<Expression*> deferProcCheck;
    bool returnValueFound;

    ValidatorImp():err(0),mod(0),curTypeDecl(0),prevStat(0),returnValueFound(false) {}

    //////// Scopes

    void visitScope( Scope* me )
    {
        QList<Record*> recs;
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Const )
                n->accept(this);
        }
#if 0
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_NamedType )
                n->accept(this);
        }
#else
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_NamedType && n->d_type && !n->d_type->isStructured(true) )
                n->accept(this);
        }
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_NamedType && n->d_type && n->d_type->isStructured(true) )
            {
                n->accept(this);
                Type* td = n->d_type.data(); // no, don't look at qualitypes: n->d_type.isNull() ? 0 : n->d_type->derefed();
                if( td && td->getTag() == Thing::T_Record )
                    recs.append(cast<Record*>(td));
            }
        }
        QSet<Record*> circular = Record::calcDependencyOrder(recs);
        if( !circular.isEmpty() )
        {
            foreach( Record* r, circular )
                error(r->d_loc, Validator::tr("RECORD used as field value type has circular dependency on itself"));
        }
#endif
        foreach( const Ref<Named>& n, me->d_order )
        {
            const int tag = n->getTag();
            if( tag == Thing::T_Variable || tag == Thing::T_LocalVar )
                n->accept(this);
        }
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Procedure )
                visitHeader( cast<Procedure*>(n.data()) ); // body can call procs defined later
        }
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Procedure )
                visitBody( cast<Procedure*>(n.data()) );
        }
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Import )
                n->accept(this); // make sure import is validated even if not used in module (TODO: can we avoid this?)
        }
    }

    void collectNonLocals( Procedure* caller, QSet<Procedure*>& visited, int level = 0 )
    {
        //qDebug() << QByteArray(level*4,' ').constData() << caller->d_name << visited.contains(caller);
        if( visited.contains(caller) )
            return;
        visited.insert(caller);
        ProcType* callerPt = caller->getProcType();
        foreach( Procedure* called, caller->d_calling )
        {
#if 0
            // this is wrong, since a procedure might not yet be identified as intermediate, even if so;
            // Example: GameHunter msysTextOut.Set.WriteInt
            if( !called->d_upvalIntermediate && !called->d_upvalSink )
                continue;
#endif
            collectNonLocals(called,visited, level+1);
            ProcType* calledPt = called->getProcType();
            foreach( Named* nl, calledPt->d_nonLocals )
            {
                if( nl->d_scope != caller )
                {
                    caller->d_upvalIntermediate = true;
                    callerPt->addNonLocal(nl);
                }
            }
        }
        if( callerPt->d_typeBound && caller->d_upvalIntermediate )
            error( callerPt->d_loc, Validator::tr("type-bound procedures cannot be non-local access intermediates") );
#if 0
        qDebug() << "****" << caller->d_name << caller->getProcType()->d_nonLocals.size() <<
                    caller->d_upvalSource << caller->d_upvalIntermediate << caller->d_upvalSink;
        foreach(Named* nl, caller->getProcType()->d_nonLocals )
            qDebug() << "    " << nl->d_name;
#endif
    }

    void visit( Module* me)
    {
        if( me->d_visited )
            return;
        me->d_visited = true;
        if( !me->d_metaParams.isEmpty() )
        {
            if( !me->d_metaActuals.isEmpty() )
            {
                // this is an instantiated generic module
                Q_ASSERT( me->d_metaActuals.size() == me->d_metaParams.size() );
                for( int i = 0; i < me->d_metaActuals.size(); i++ )
                    me->d_metaParams[i]->d_type = me->d_metaActuals[i];
            }else
            {
                // this is a generic module
                for( int i = 0; i < me->d_metaParams.size(); i++ )
                    me->d_metaParams[i]->d_type = bt.d_anyType;
            }
        }

        SysAttrs::const_iterator i;
        for( i = me->d_sysAttrs.begin(); i != me->d_sysAttrs.end(); ++i )
        {
            for( int j = 0; j < i.value()->d_valExpr.size(); j++ )
            {
                i.value()->d_valExpr[j]->accept(this);
                Evaluator::Result res = Evaluator::eval(i.value()->d_valExpr[j].data(), mod, false, err);
                i.value()->d_values.append(res.d_value);
            }
        }


        levels.push_back(me);
        // imports are supposed to be already resolved at this place
        visitScope(me);
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Procedure && n->d_upvalSource )
            {
                QSet<Procedure*> visited;
                collectNonLocals( cast<Procedure*>(n.data()), visited );
            }
        }
        visitStats( me->d_body );

        foreach( Expression* e, deferProcCheck )
        {
            Named* p = e->getIdent();
            Q_ASSERT( p && p->getTag() == Thing::T_Procedure );
            if( p->d_upvalIntermediate || p->d_upvalSink )
                error( e->d_loc, Validator::tr("this procedure depends on the environment and cannot be assigned"));
        }
        deferProcCheck.clear();

        levels.pop_back();
    }

    void visitBoundProc( Procedure* me )
    {
        // Helper of visitHeader
        Q_ASSERT( !me->d_receiver.isNull() );
        me->d_receiver->accept(this);

        Type* t = derefed(me->d_receiver->d_type.data());
        if( t == 0 )
        {
            error( me->d_receiver->d_loc, Validator::tr("cannot resolve receiver type") );
            return;
        }
        switch( t->getTag() )
        {
        case Thing::T_Pointer:
            if( me->d_receiver->d_var )
                error( me->d_receiver->d_loc, Validator::tr("receiver to pointer types must be value parameters") );
            t = derefed( cast<Pointer*>(t)->d_to.data() );
            break;
        case Thing::T_Record:
            if( !me->d_receiver->d_var )
                error( me->d_receiver->d_loc, Validator::tr("receiver to record types must be variable parameters") );
            break;
        }
        if( t == 0 || t->getTag() != Thing::T_Record )
        {
            error( me->d_receiver->d_loc, Validator::tr("the receiver must be of record or pointer to record type") );
            return;
        }
#ifndef _OBX_USE_NEW_FFI_
        // yes, they can, but there is no virtual method table and no type pointer for such in Oberon+ (but it can be in C)
        // but not yet
        if( t->d_unsafe )
        {
            error( me->d_receiver->d_loc, Validator::tr("CSTRUCT and CUNION cannot have type-bound procedures") );
            return;
        }
#endif
        Record* r = cast<Record*>(t);
        if( r->find( me->d_name, false ) )
            error( me->d_loc, Validator::tr("name is not unique in record"));
        else
        {
            r->d_methods << me;
            r->d_names[ me->d_name.constData() ] = me;
            me->d_receiverRec = r;
            Named* decl = r->findDecl();
            Q_ASSERT( decl );
            if( me->d_scope != decl->d_scope )
                error( me->d_loc, Validator::tr("type bound procedures must be declared in the same scope as the record they bind to"));
        }
        if( r->d_baseRec )
        {
            // check wheter base has a method with this name and the signature is compatible
            Named* n = r->d_baseRec->find( me->d_name, true );
            if( n == 0 )
                return; // this is no override
            if( n->getTag() != Thing::T_Procedure )
            {
                error( me->d_loc, Validator::tr("bound procedure name collides with a field name in the receiver base record"));
                return;
            }
            me->d_super = cast<Procedure*>(n);
            me->d_super->d_subs.append(me);
            if( !matchingFormalParamLists( me->d_super->getProcType(), me->getProcType()
                               #ifdef OBX_BBOX
                                           , true
                               #endif
                                           ) )
                error( me->d_loc, Validator::tr("formal paramater list doesn't match the overridden procedure in the receiver base record"));
        }
    }

    void visitHeader( Procedure* me )
    {
        // don't push/pop levels here because qualis have to be resolved in environment of me
        // (otherwise collision with params of same name as type)
        ProcType* pt = me->getProcType();
        pt->accept(this);

        if( me->d_visibility == Named::ReadOnly )
            warning( me->d_loc, Validator::tr("export mark '-' not supported for procedures; using '*' instead") );
        else if( me->d_visibility == Named::ReadWrite )
            me->d_visibility = Named::ReadOnly; // make public procs readonly; they cannot be assigned to!

        if( !me->d_sysAttrs.isEmpty() && !mod->d_externC )
            error( me->d_sysAttrs.begin().value()->d_loc, Validator::tr("procedure attributes only supported in external library modules"));
        else if( me->d_sysAttrs.contains("varargs") )
            pt->d_varargs = true;

        if( !me->d_receiver.isNull() )
        {
            // receiver was already accepted in visitScope
            visitBoundProc(me);
        }

        SysAttrs::const_iterator i;
        for( i = me->d_sysAttrs.begin(); i != me->d_sysAttrs.end(); ++i )
        {
            for( int j = 0; j < i.value()->d_valExpr.size(); j++ )
            {
                i.value()->d_valExpr[j]->accept(this);
                Evaluator::Result res = Evaluator::eval(i.value()->d_valExpr[j].data(), levels.back().scope, false, err);
                i.value()->d_values.append(res.d_value);
            }
        }
    }

    void visitBody( Procedure* me )
    {
        levels.push_back(me);
        visitScope(me); // also handles formal parameters
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Procedure && n->d_upvalSource )
            {
                QSet<Procedure*> visited;
                collectNonLocals( cast<Procedure*>(n.data()), visited );
            }
        }
        returnValueFound = false;
        visitStats( me->d_body );
        if( !mod->d_isDef &&
                !( me->d_noBody && me->d_order.size() == me->d_parCount ) // empty or only one return statement, and no declarations
                )
        {
            ProcType* pt = me->getProcType();
            if( pt->d_return.isNull() && returnValueFound )
                error( me->d_loc, Validator::tr("procedure is not expected to return a value"));
            else if( !pt->d_return.isNull() && !returnValueFound )
                error( me->d_loc, Validator::tr("procedure is expected to return a value"));
        }

        levels.pop_back();
    }

    void visit( Procedure* me )
    {
        // instead split to visitHeader and visitBody so signatures are all known before a body is validated
        Q_ASSERT( false );
    }

    ///////// Expressions

    void checkNonLocalAccess( Named* n, Expression* e )
    {
        const int tag = n->getTag();
        if( tag == Thing::T_Parameter || tag == Thing::T_LocalVar )
        {
            if( n->d_scope != levels.back().scope )
            {
                Type* td = derefed(n->d_type.data());
                if( td->d_unsafe )
                    error( e->d_loc, Validator::tr("non-local access to unsafe types not supported") );
                ProcType* pt;
                n->d_scope->d_upvalSource = true;
                int i = levels.size() - 2;
                while( i > 0 && levels[i].scope != n->d_scope )
                {
                    levels[i].scope->d_upvalIntermediate = true;
                    pt = cast<ProcType*>(levels[i].scope->d_type.data());
                    pt->addNonLocal(n);
                    if( pt->d_typeBound )
                        error( e->d_loc, Validator::tr("type-bound procedures cannot be non-local access intermediates") );
                    i--;
                }
                levels.back().scope->d_upvalSink = true;
                pt = cast<ProcType*>(levels.back().scope->d_type.data());
                pt->addNonLocal(n);
                if( pt->d_typeBound )
                    error( e->d_loc, Validator::tr("type-bound procedures cannot access non-local variables and parameters") );
                n->d_upvalSource = true;
                // no longer true:
                //error( e->d_loc, Validator::tr("cannot access local variables and parameters of outer procedures") );
                //warning( e->d_loc, Validator::tr("non-local access") );
            }
        }
    }

    void visit( IdentLeaf* me )
    {
        Q_ASSERT( !levels.isEmpty() );
        me->d_ident = levels.back().scope->find( me->d_name );
        // me->d_mod = mod;
        if( me->d_ident.isNull() )
        {
            error( me->d_loc, Validator::tr("cannot resolve identifier '%1'").arg(me->d_name.constData()) );
        }else
        {
            switch( me->d_ident->getTag() )
            {
            case Thing::T_Import:
                {
                    Import* imp = cast<Import*>(me->d_ident.data());
                    if( !imp->d_metaActuals.isEmpty() && !imp->d_visited )
                        imp->accept(this);
                }
                break;
            case Thing::T_Const:
                {
                    Const* c = cast<Const*>(me->d_ident.data());
                    if( !c->d_visited )
                        c->accept(this);
                }
            }
            checkNonLocalAccess(me->d_ident.data(),me);
            //if( me->d_ident->getTag() == Thing::T_Import )
            //    me->d_mod = static_cast<Import*>( me->d_ident.data() )->d_mod.data();
            me->d_type = me->d_ident->d_type.data();
#if 0
            if( me->d_ident->getTag() == Thing::T_Procedure && me->d_ident.data() == levels.back().scope )
                qDebug() << "recursive call" << mod->d_name << me->d_loc.d_row;
#endif
        }
    }

    void visit( IdentSel* me )
    {
        if( me->d_sub.isNull() )
            return;

        me->d_sub->accept(this);
        Named* subId = me->d_sub->getIdent();
        if( subId && subId->getTag() == Thing::T_Import )
        {
            // prev is an import
            Import* imp = cast<Import*>( subId );
            if( imp->d_mod.isNull() )
            {
                error( imp->d_loc, Validator::tr("cannot resolve identifier '%1'").arg(me->d_name.constData()));
                return;
            }
            Named* modVar = imp->d_mod->find( me->d_name, false );
            if( modVar == 0 )
            {
                error( me->d_loc,Validator::tr("cannot resolve identifier '%1' in imported module '%2'")
                      .arg(me->d_name.constData()).arg( imp->d_name.constData() ) );
                return;
            }
            if( !imp->d_mod->d_isDef && !modVar->isPublic() )
            {
                error( me->d_loc,Validator::tr("cannot access private identifier '%1' in imported module '%2'")
                      .arg(me->d_name.constData()).arg( imp->d_path.join('/').constData() ) );
                return;
            }
#if 0 // unnecessary, and since delegates counterproductive
            if( modVar->getTag() == Thing::T_Procedure )
            {
                Procedure* p = cast<Procedure*>(modVar);
                if( !p->d_receiver.isNull() )
                {
                    error( me->d_loc,Validator::tr("cannot reference bound procedure '%1' from another module")
                          .arg(me->d_name.constData()) );
                }
            }
#endif
            me->d_ident = modVar;
            me->d_type = modVar->d_type.data();
        }else
        {
            // prev must be a pointer or a record
            Type* prevT = derefed( me->d_sub->d_type.data() );

            if( prevT && prevT->getTag() == Thing::T_Pointer )
            {
                // The designator p^.f may be abbreviated as p.f, i.e. record selectors imply dereferencing.
                // So add a deref to the AST.
                Ref<UnExpr> deref = new UnExpr();
                deref->d_op = UnExpr::DEREF;
                deref->d_sub = me->d_sub;
                deref->d_loc = me->d_loc;
                me->d_sub = deref.data();
                Pointer* p = cast<Pointer*>(prevT);
                prevT = derefed(p->d_to.data());
                deref->d_type = prevT;
                if( prevT == 0 )
                    return;
            }

            // TODO: do we allow a selector from an unsafe pointer to a safe record?
            if( prevT && prevT->getTag() == Thing::T_Record )
            {
                Record* r = cast<Record*>(prevT);
                Named* field = r->find(me->d_name, true);
                if( field == 0 )
                {
                    error( me->d_loc, Validator::tr("there is no member named '%1'").arg(me->d_name.constData()) );
                    return;
                }
                Module* sourceMod = field->getModule();
                Q_ASSERT( sourceMod );
                if( sourceMod != mod && !sourceMod->d_isDef && !field->isPublic() )
                    error( me->d_loc, Validator::tr("element is not public") );

                me->d_ident = field;
                me->d_type = field->d_type.data();
            }else
                error(me->d_loc, Validator::tr("the designated object is not a record") );
        }
    }

    inline bool isBasicOrSet( Type* lhs ) const
    {
        return lhs == bt.d_boolType || lhs == bt.d_charType || lhs == bt.d_wcharType ||
                lhs == bt.d_byteType || lhs == bt.d_intType || lhs == bt.d_shortType || lhs == bt.d_longType ||
                lhs == bt.d_realType || lhs == bt.d_longrealType || lhs == bt.d_setType;
    }

    bool isInParam( Expression* e )
    {
        Named* n = e->getIdent();
        if( n != 0 && n->getTag() == Thing::T_Parameter )
            return cast<Parameter*>(n)->d_const;
        else
            return false;
    }

    bool isMemoryLocation( Expression* e )
    {
        Named* n = e->getIdent();
        if( n == 0 )
        {
            // + a pointer points to a memory location; from this memory location an address can be taken (which
            //   trivially corresponds to the pointer
            // + a string or byte literal or constant is a memory location (wheras read only)
            // + an index is an offset from the start of the memory location of the array
            // + a guard is just another type for the same memory location
            // + a function returning a pointer to something corresponds to the first case (even if there is no direct ident)
            // - the value result (basic or structured) of a function is only transient (unless assigned to something)
            while( e )
            {
                if( !e->d_type.isNull() && ( e->d_type->getBaseType() == Type::STRING ||
                                   e->d_type->getBaseType() == Type::WSTRING ||
                                             e->d_type->getBaseType() == Type::BYTEARRAY ) )
                    return true; // a string literal (either direct literal or designator to literal)
                switch( e->getTag() )
                {
                case Thing::T_IdentLeaf:
                    n = e->getIdent();
                    e = 0;
                    continue;
                }
                switch( e->getUnOp() )
                {
                case UnExpr::SEL:
                    n = e->getIdent();
                    e = 0;
                    break;
                case UnExpr::CAST:
                case UnExpr::DEREF:
                case UnExpr::IDX:
                    e = cast<UnExpr*>(e)->d_sub.data();
                    break;
                case UnExpr::CALL:
                    if( derefed(e->d_type.data())->getTag() == Thing::T_Pointer )
                        return true; // only NAppGUI/HelloGui/Popups.obx so far requires true; see above for concept
                    else
                    {
                        ArgExpr* args = cast<ArgExpr*>(e);
                        if( args->d_sub && args->d_sub->getIdent() )
                        {
                            if( args->d_sub->getIdent()->getTag() == Thing::T_BuiltIn )
                            {
                                BuiltIn* bi = cast<BuiltIn*>(args->d_sub->getIdent());
                                if( bi->d_func == BuiltIn::SYS_VAL && args->d_args.size() == 2 )
                                {
                                    // VAL does not actually return something but is just a cast for the second argument
                                    e = args->d_args.last().data();
                                    break;
                                }
                            }
                        }
                        return false;
                    }
                    break;
                default:
                    e = 0;
                    break;
                }
            }
        }
        if( n != 0 )
        {
            switch( n->getTag() )
            {
            case Thing::T_Field:
            case Thing::T_LocalVar:
            case Thing::T_Parameter:
            case Thing::T_Variable:
                return true;
            case Thing::T_Const:
                {
                    Const* c = cast<Const*>(n);
                    if( c->d_vtype == Literal::String || c->d_vtype == Literal::Bytes )
                        return true;
                }
                break;
            default:
                return false;
            }
        }
        return false;
    }

    bool checkBuiltInArgs( ProcType* p, ArgExpr* args )
    {
        Q_ASSERT( p->d_decl && p->d_decl->getTag() == Thing::T_BuiltIn );
        BuiltIn* bi = cast<BuiltIn*>(p->d_decl);

        switch( bi->d_func )
        {
        case BuiltIn::CHR:
        case BuiltIn::INCL:
        case BuiltIn::EXCL:
        case BuiltIn::PACK:
        case BuiltIn::UNPK:
        case BuiltIn::LED:
        case BuiltIn::TRAP:
        case BuiltIn::TRAPIF:
        case BuiltIn::WCHR:
        case BuiltIn::BITS:
        case BuiltIn::HALT:
        case BuiltIn::ROR:
            return false; // these can be handled by ordinary arg checker

        case BuiltIn::PCALL:
            if( args->d_args.size() >= 2 )
            {
                // NOTE: PCALL must be a procedure, not a function; if a function it could be called in any expression;
                // instead a PCALL shall be a call statement, in correspondence with the rule that PCALL can only call
                // true procedures, which also makes the implementation of the CIL backend easier
                Type* t0 = derefed(args->d_args[0]->d_type.data() );
                Type* t1 = derefed(args->d_args[1]->d_type.data() );
                Named* n1 = args->d_args[1]->getIdent();
                if( !checkValidLhs(args->d_args[0].data()) || t0 == 0 ||
                        t0->getTag() != Thing::T_Pointer || derefed(cast<Pointer*>(t0)->d_to.data()) != bt.d_anyRec )
                    error( args->d_args[0]->d_loc, Validator::tr("first argument must be pointer to anyrec"));
                else if( n1 && n1->getTag() == Thing::T_BuiltIn )
                    error( args->d_args[1]->d_loc, Validator::tr("predeclared procedures cannot be used with PCALL"));
                else if( t1 && t1->getTag() == Thing::T_ProcType )
                {
                    ProcType* pt = cast<ProcType*>(t1);
                    if( pt->d_return.isNull() )
                    {
                        // checkValidRhs(args->d_args[1].data()); // forbids non-local access
                        ArgExpr tmp = *args;
                        tmp.d_sub = args->d_args[1].data();
                        tmp.d_args.pop_front();
                        tmp.d_args.pop_front();
                        checkCallArgs(pt,&tmp); // PCALL is transparent; this is the actual call
                    }else
                        error( args->d_args[1]->d_loc, Validator::tr("function procedures are not supported by PCALL"));
                }else // td can be null, e.g. when calling new(ptr) as second argument
                    error( args->d_args[1]->d_loc, Validator::tr("expecting procedure as first argument"));
            }else
                error( args->d_loc, Validator::tr("expecting at least two arguments"));
            break;
        case BuiltIn::RAISE:
            if( args->d_args.size() == 0 )
            {
                // generates an instance of ANYREC
            }else if( args->d_args.size() == 1 )
            {
                Type* td = derefed(args->d_args.first()->d_type.data() );
                if( td && ( !isPointerToRecord(td) || td->d_unsafe ) ) // CUNION or CSTRUCT not supported
                    error( args->d_args.first()->d_loc, Validator::tr("expecting a non-nil pointer to a record"));
            }else
                error( args->d_loc, Validator::tr("expecting zero or one arguments"));
            break;

        case BuiltIn::BITNOT:
            if( args->d_args.size() == 1 )
            {
                Type* td = derefed(args->d_args.first()->d_type.data());
                if( !td->isInteger() )
                    error( args->d_args[0]->d_loc, Validator::tr("expecting integer argument"));
            }else
                error( args->d_loc, Validator::tr("expecting one argument"));
            break;
        case BuiltIn::BITAND:
        case BuiltIn::BITOR:
        case BuiltIn::BITXOR:
        case BuiltIn::BITSHL:
        case BuiltIn::BITSHR:
        case BuiltIn::BITASR:
        case BuiltIn::LSL:
        case BuiltIn::ASR:
        case BuiltIn::ASH:
            if( args->d_args.size() == 2 )
            {
                Type* td = derefed(args->d_args.first()->d_type.data());
                if( td == 0 )
                    break; // already reported
                if( !td->isInteger() )
                    error( args->d_args[0]->d_loc, Validator::tr("expecting integer argument"));
                td = derefed(args->d_args.last()->d_type.data());
                if( !td->isInteger() )
                    error( args->d_args[1]->d_loc, Validator::tr("expecting integer argument"));
            }else
                error( args->d_loc, Validator::tr("expecting two arguments"));
            break;
        case BuiltIn::FLOOR:
        case BuiltIn::ENTIER:
            if( args->d_args.size() == 1 )
            {
                Type* td = derefed(args->d_args.first()->d_type.data());
                if( !td->isReal() )
                    error( args->d_args[0]->d_loc, Validator::tr("expecting real argument"));
            }else
                error( args->d_loc, Validator::tr("expecting one argument"));
           break;
        case BuiltIn::FLT:
        case BuiltIn::ODD:
            if( args->d_args.size() == 1 )
            {
                Type* td = derefed(args->d_args.first()->d_type.data());
                if( td == 0 || !td->isInteger() )
                    error( args->d_args[0]->d_loc, Validator::tr("expecting integer argument"));
            }else
                error( args->d_loc, Validator::tr("expecting one argument"));
           break;
        case BuiltIn::ADR:
            if( args->d_args.size() == 1 )
            {
                // TODO: we no longer need ADR
                warning( args->d_loc, Validator::tr("the ADR() built-in function is deprecated") );
                if( !isMemoryLocation(args->d_args.first().data()) )
                {
                    error( args->d_args.first()->d_loc, Validator::tr("cannot take address of expression"));
                    break;
                }
                if( args->d_args.first()->visibilityFor(mod) == Named::Private )
                {
                    error( args->d_args.first()->d_loc, Validator::tr("cannot take address of private designator"));
                    break;
                }

                Type* rhsT = derefed(args->d_args.first()->d_type.data());
#ifdef _OBX_USE_NEW_FFI_
                if( rhsT && rhsT->d_unsafe )
#else
                if( rhsT && ( rhsT->d_unsafe || !rhsT->isStructured(true) || isArrayOfUnstructuredType(rhsT) ) )
#endif
                {
                    Ref<Pointer> ptr = new Pointer();
                    ptr->d_loc = args->d_loc;
                    ptr->d_unsafe = true;
                    ptr->d_to = args->d_args.first()->d_type.data();
                    args->d_type = ptr.data();
                    mod->d_helper2.append(ptr.data()); // otherwise ptr gets deleted when leaving this scope
                }else
                    error( args->d_loc, Validator::tr("taking the address of objects of given type is not supported"));
            }else
                error( args->d_loc, Validator::tr("expecting one argument"));
           break;

        case BuiltIn::INC:
        case BuiltIn::DEC:
            if( args->d_args.size() == 1 || args->d_args.size() == 2 )
            {
                Parameter lhs;
                lhs.d_var = true;
                lhs.d_type = derefed(args->d_args.first()->d_type.data());
                if( lhs.d_type.isNull() )
                    break; // already reported
                if( !isInteger( lhs.d_type.data() ) && lhs.d_type->getTag() != Thing::T_Enumeration )
                    error( args->d_args.first()->d_loc, Validator::tr("expecting integer or enumeration argument"));
                checkCallArg( p, &lhs, args->d_args[0] );
                if( args->d_args.size() == 2 )
                {
                    Parameter rhs;
                    rhs.d_type = derefed(args->d_args[1]->d_type.data());
                    if( lhs.d_type && lhs.d_type->getTag() == Thing::T_Enumeration )
                        error( args->d_args[1]->d_loc, Validator::tr("cannot use second argument for enumeration types"));
                    else if( !isInteger( rhs.d_type.data() ) )
                        error( args->d_args[1]->d_loc, Validator::tr("expecting integer argument"));
                    checkCallArg( p, &rhs, args->d_args[1] );
                }
            }else
                error( args->d_loc, Validator::tr("expecting one or two arguments"));
            break;
        case BuiltIn::ASSERT: // optional arg
            if( args->d_args.size() == 1 || args->d_args.size() == 2 )
            {
                Parameter lhs;
                lhs.d_type = bt.d_boolType;
                checkCallArg( p, &lhs, args->d_args[0] );
                if( args->d_args.size() == 2 )
                {
                    Parameter rhs;
                    rhs.d_type = derefed(args->d_args[1]->d_type.data());
                    if( !isInteger( rhs.d_type.data() ) )
                        error( args->d_args[1]->d_loc, Validator::tr("expecting integer argument"));
                    checkCallArg( p, &rhs, args->d_args[1] );
                }
            }else
                error( args->d_loc, Validator::tr("expecting one or two arguments"));
            break;
        case BuiltIn::CAP:
            if( args->d_args.size() == 1 )
            {
                Parameter a,b;
                a.d_type = bt.d_charType;
                b.d_type = bt.d_wcharType;
                if( !paramCompatible( &a, args->d_args[0].data()) && !paramCompatible( &b, args->d_args[0].data()) )
                    error( args->d_args[0]->d_loc, Validator::tr("expecting char or wchar argument"));
            }else
                error( args->d_loc, Validator::tr("expecting one argument"));
            break;
        case BuiltIn::ORD:
            if( args->d_args.size() == 1 )
            {
                Parameter a,b,c,d,e;
                a.d_type = bt.d_charType;
                b.d_type = bt.d_wcharType;
                c.d_type = bt.d_setType;
                d.d_type = derefed(args->d_args[0]->d_type.data());
                e.d_type = bt.d_boolType;

                if( d.d_type.isNull() )
                    break; // already reported

                if( isCharConst(args->d_args[0].data()) )
                    break;

                if( d.d_type->getTag() == Thing::T_Pointer ||
                        d.d_type->getBaseType() == Type::REAL || d.d_type->getBaseType() == Type::LONGREAL )
                    break; // undocumented oberon feature

                if( !paramCompatible( &a, args->d_args[0].data()) && !paramCompatible( &b, args->d_args[0].data())
                        && !paramCompatible( &e, args->d_args[0].data())
                        && !paramCompatible( &c, args->d_args[0].data()) && d.d_type && d.d_type->getTag() != Thing::T_Enumeration )
                    error( args->d_args[0]->d_loc, Validator::tr("expecting char, wchar, boolean, set or enumeration argument"));
            }else
                error( args->d_loc, Validator::tr("expecting one argument"));
            break;
        case BuiltIn::ABS:
            if( args->d_args.size() == 1 )
            {
                Parameter lhs;
                lhs.d_type = derefed(args->d_args.first()->d_type.data());
                if( !isNumeric( lhs.d_type.data() ) )
                    error( args->d_args.first()->d_loc, Validator::tr("expecting numeric argument"));
                checkCallArg( p, &lhs, args->d_args[0] );
            }else
                error( args->d_loc, Validator::tr("expecting one argument"));
            break;
        case BuiltIn::LEN:
            if( args->d_args.size() == 1 || args->d_args.size() == 2 )
            {
                Type* lhs = derefed(args->d_args.first()->d_type.data());
                if( lhs && lhs->getTag() == Thing::T_Pointer )
                    lhs = derefed(cast<Pointer*>(lhs)->d_to.data());
                if( lhs == 0 )
                    return false; // already reported
                const int ltag = lhs->getTag();
                if( ltag != Thing::T_Array && lhs != bt.d_stringType && lhs != bt.d_wstringType
                        && lhs != bt.d_byteArrayType )
                    error( args->d_args.first()->d_loc, Validator::tr("expecting array, string or bytearray argument"));
                if( ltag == Thing::T_Array )
                {
                    Array* a = cast<Array*>(lhs);
                    if( a->d_unsafe && a->d_lenExpr.isNull() )
                        error( args->d_args.first()->d_loc, Validator::tr("not supported for open carrays"));
                }
                if( args->d_args.size() == 2 )
                {
                    if( ltag != Thing::T_Array )
                        error( args->d_args.first()->d_loc, Validator::tr("expecting array argument"));
                    Type* rhs = derefed(args->d_args[1]->d_type.data());
                    if( !isInteger( rhs ) )
                        error( args->d_args[1]->d_loc, Validator::tr("expecting integer argument"));
                }
            }else
                error( args->d_loc, Validator::tr("expecting one or two arguments"));
            break;
        case BuiltIn::MAX:
        case BuiltIn::MIN:
            if( args->d_args.size() == 1 )
            {
                Named* n = args->d_args.first()->getIdent();
                if( n == 0 || n->getTag() != Thing::T_NamedType )
                {
                    error( args->d_args.first()->d_loc, Validator::tr("expecting named type"));
                    break;
                }
                Type* lhs = derefed(args->d_args.first()->d_type.data());
                if( !isBasicOrSet(lhs) && lhs->getTag() != Thing::T_Enumeration )
                    error( args->d_args.first()->d_loc, Validator::tr("expecting basic, set or enumeration type"));
            }else if( args->d_args.size() == 2 )
            {
                Type* lhs = derefed(args->d_args.first()->d_type.data());
                Type* rhs = derefed(args->d_args.last()->d_type.data());
                const bool ok = ( lhs && lhs->isChar() && rhs && rhs->isChar() ) ||
                        ( isNumeric(lhs) && isNumeric(rhs) );
                if( !ok )
                    error( args->d_loc, Validator::tr("expecting both arguments of numeric or character type"));
            }else
                error( args->d_loc, Validator::tr("expecting one or two arguments"));
            break;
        case BuiltIn::DEFAULT:
            if( args->d_args.size() == 1 )
            {
                Named* n = args->d_args.first()->getIdent();
                if( n == 0 || ( n->getTag() != Thing::T_NamedType && n->getTag() != Thing::T_GenericName ) )
                    error( args->d_args.first()->d_loc, Validator::tr("expecting a type name") );
            }else
                error( args->d_loc, Validator::tr("expecting one argument"));
            break;
        case BuiltIn::SHORT:
            if( args->d_args.size() == 1 )
            {
                Type* lhs = derefed(args->d_args.first()->d_type.data());
                const bool ok = lhs == bt.d_wcharType ||
                        lhs == bt.d_intType || lhs == bt.d_shortType || lhs == bt.d_longType ||
                        lhs == bt.d_longrealType;
                if( !ok && lhs != bt.d_wstringType && charArrayType(lhs,false) != bt.d_wcharType )
                    error( args->d_args.first()->d_loc, Validator::tr("incompatible argument"));
            }else
                error( args->d_loc, Validator::tr("expecting one argument"));
            break;
        case BuiltIn::LONG:
            if( args->d_args.size() == 1 )
            {
                Type* lhs = derefed(args->d_args.first()->d_type.data());
                const bool ok = lhs == bt.d_charType ||
                        lhs == bt.d_byteType || lhs == bt.d_intType || lhs == bt.d_shortType ||
                        lhs == bt.d_realType;
                if( !ok && lhs != bt.d_stringType && charArrayType(lhs,false) != bt.d_charType )
                    error( args->d_args.first()->d_loc, Validator::tr("incompatible argument"));
            }else
                error( args->d_loc, Validator::tr("expecting one argument"));
            break;
        case BuiltIn::NEW:
            if( args->d_args.size() >= 1 )
            {
                Type* lhs = derefed(args->d_args.first()->d_type.data());
                if( lhs == 0 )
                    return false; // already reported
                if( lhs->getTag() != Thing::T_Pointer )
                {
                    error( args->d_args.first()->d_loc, Validator::tr("expecting a pointer"));
                    break;
                } // else
                Pointer* ptr = cast<Pointer*>(lhs);
                if( ptr->d_unsafe )
                {
                    error( args->d_args.first()->d_loc, Validator::tr("not supported for CPOINTER"));
                    break;
                }
                Type* to = derefed(ptr->d_to.data());
                const int tag = to->getTag();
                if( tag == Thing::T_Array )
                {
                    QList<Array*> dims = cast<Array*>(to)->getDims();
                    int dim = 0;
                    for( int i = 0; i < dims.size(); i++ )
                    {
                        if( dims[i]->d_lenExpr.isNull() )
                            dim++;
                        else
                            break;
                    }
                    if( args->d_args.size() - 1 != dim )
                        error( args->d_loc, Validator::tr("the number of arguments is not compatible with the given array"));
                }else if( tag == Thing::T_Record )
                {
                    if( args->d_args.size() > 1 )
                        error( args->d_args[1]->d_loc, Validator::tr("too many arguments"));
                }else if( tag == Thing::T_QualiType )
                    return false; // already reported
                else
                    error( args->d_loc, Validator::tr("invalid argument type for NEW"));
            }else
                error( args->d_loc, Validator::tr("expecting at least one argument"));
            break;
        case BuiltIn::BYTESIZE:
            if( args->d_args.size() != 1 )
                error( args->d_loc, Validator::tr("expecting one argument"));
            break; // accepts any type
        case BuiltIn::COPY:
            if( args->d_args.size() != 2 )
                error( args->d_loc, Validator::tr("expecting two arguments"));
            else
            {
                if( toCharArray( args->d_args.last()->d_type.data(), false ) == 0 ||
                    !assignmentCompatible( args->d_args.last()->d_type.data(), args->d_args.first().data() ) )
                    error( args->d_loc, Validator::tr("incompatible arguments"));
            }
            break;
        case BuiltIn::CAST:
            if( args->d_args.size() == 2 )
            {
                Named* n = args->d_args.first()->getIdent();
                if( n == 0 || ( n->getTag() != Thing::T_NamedType && n->getTag() != Thing::T_GenericName ) )
                {
                    error( args->d_args.first()->d_loc, Validator::tr("expecting a type name") );
                    break;
                }
                Type* lhs = derefed(args->d_args.first()->d_type.data());
                Type* rhs = derefed(args->d_args.last()->d_type.data());
                if( lhs == 0 || rhs == 0 )
                    break; // already reported
                const int ltag = lhs->getTag();
                const int rtag = rhs->getTag();

                if( ltag == Thing::T_Pointer && lhs->d_unsafe &&
                        rtag == Thing::T_Pointer && rhs->d_unsafe )
                {
                    Type* l = derefed(cast<Pointer*>(lhs)->d_to.data());
                    Type* r = derefed(cast<Pointer*>(rhs)->d_to.data());
                    if( l == 0 || r == 0 )
                        break; // already reported
                    if( ( ( l->getTag() == Thing::T_Record && !l->d_union ) || l->getBaseType() == Type::CVOID ) &&
                            ( ( r->getTag() == Thing::T_Record && !l->d_union ) || r->getBaseType() == Type::CVOID) )
                        return true; // we allow like C to cast void* to struct* and struct* to struct*
                }

                if( lhs->isInteger() && rtag == Thing::T_Pointer && rhs->d_unsafe )
                {
                    Type* r = derefed(cast<Pointer*>(rhs)->d_to.data());
                    if( r && r->getBaseType() == Type::CVOID )
                        return true; // we allow to cast void* to integer
                }

                if( rhs->isInteger() && ltag == Thing::T_Pointer && lhs->d_unsafe )
                {
                    Type* l = derefed(cast<Pointer*>(lhs)->d_to.data());
                    if( l && l->getBaseType() == Type::CVOID )
                        return true; // we allow to cast integer to void*
                }

                if( ( ltag == Thing::T_Enumeration && isInteger(rhs) ) ||
                        ( lhs == bt.d_intType && rhs == bt.d_setType ) ||
                        ( lhs == bt.d_setType && isInteger(rhs) ) ||
                        ( lhs == bt.d_intType && rhs == bt.d_intType ) ) // shortcut for short()/long() sequences
                    ; // ok
                else
                    error( args->d_loc, Validator::tr("cannot cast type %1 to type %2").
                           arg(rhs->pretty()).arg(lhs->pretty()));
            }else
                error( args->d_loc, Validator::tr("expecting two arguments"));
            break;
        case BuiltIn::STRLEN:
            if( args->d_args.size() == 1 )
            {
                Type* lhs = derefed(args->d_args.first()->d_type.data());
                if( lhs->getTag() == Thing::T_Pointer )
                    lhs = derefed(cast<Pointer*>(lhs)->d_to.data());
                const int ltag = lhs->getTag();
                if( ltag != Thing::T_Array && lhs != bt.d_stringType && lhs != bt.d_wstringType )
                    error( args->d_args.first()->d_loc, Validator::tr("expecting array or string argument"));
                if( !lhs->isText() )
                    error( args->d_args.first()->d_loc, Validator::tr("expecting array to char or wchar"));
                // TODO: can we support carray of char/wchar here?
            }else
                error( args->d_loc, Validator::tr("expecting one argument"));
            break;
        case BuiltIn::LDMOD:
            if( args->d_args.size() == 1 )
            {
                Type* lhs = derefed(args->d_args.first()->d_type.data());
                bool wide = false;
                if( !lhs->isText(&wide,true) || wide )
                    error( args->d_args.first()->d_loc, Validator::tr("expecting string or char array"));
            }else
                error( args->d_loc, Validator::tr("expecting one argument"));
            break;
        case BuiltIn::LDCMD:
            if( args->d_args.size() == 2 )
            {
                Type* t = derefed(args->d_args.first()->d_type.data());
                bool wide = false;
                if( !t->isText(&wide,true) || wide )
                    error( args->d_args.first()->d_loc, Validator::tr("expecting string or char array"));
                t = derefed(args->d_args.last()->d_type.data());
                if( !t->isText(&wide,true) || wide )
                    error( args->d_args.last()->d_loc, Validator::tr("expecting string or char array"));
            }else
                error( args->d_loc, Validator::tr("expecting two arguments"));
            break;
        case BuiltIn::PRINTLN:
            if( args->d_args.size() == 1 )
            {
                // NOP
            }else
                error( args->d_loc, Validator::tr("expecting one argument"));
            break;
        case BuiltIn::SYS_ADR:
        case BuiltIn::SYS_BIT:
        case BuiltIn::SYS_H:
        case BuiltIn::SYS_LDREG:
        case BuiltIn::SYS_REG:
        case BuiltIn::SYS_COPY:
        case BuiltIn::SYS_GET:
        case BuiltIn::SYS_PUT:
        case BuiltIn::SYS_VAL:
        case BuiltIn::SYS_MOVE:
        case BuiltIn::SYS_NEW:
        case BuiltIn::SYS_ROT:
        case BuiltIn::SYS_LSH:
        case BuiltIn::SYS_GETREG:
        case BuiltIn::SYS_PUTREG:
        case BuiltIn::SYS_TYP:
            warning( args->d_loc, Validator::tr("Oberon+ doesn't have a built-in SYSTEM module") );
            break; // TODO ignored for now
        }
        return true;
    }

    bool isArrayOfUnstructuredType( Type* t )
    {
        t = derefed(t);
        if( t && t->getTag() == Thing::T_Array )
        {
            Array* a = cast<Array*>(t);
            t = derefed(a->d_type.data());
            if( t && ( !t->isStructured() || ( t->d_unsafe && t->getTag() == Thing::T_Pointer ) ) )
                return true;
        }
        return false;
    }

    void checkRecordUse( Type* t, bool isVarParam = false )
    {
        Type* td = derefed(t);
        bool isPointer = false;
        if( td && td->getTag() == Thing::T_Pointer )
        {
            td = derefed( cast<Pointer*>(td)->d_to.data() );
            isPointer = true;
        }

        if( td && td->getTag() == Thing::T_Record )
        {
            Record* r = cast<Record*>(td);
            bool changed = false;
            if( ( isPointer && !isVarParam ) || ( !isPointer && isVarParam ) )
            {
                changed = !r->d_usedByRef;
                r->d_usedByRef = true;
            }else
            {
                changed = !r->d_usedByVal;
                r->d_usedByVal = true;
            }
#if 0
            // SDL has no record which is used both by val and by ref
            if( changed && mod->d_externC )
            {
                Named* n = r->findDecl();
                if( n && r->d_usedByRef && r->d_usedByVal )
                    qDebug() << n->d_name << "used by ref and by val" << n->d_loc.d_row << mod->getName();
                else if( n && r->d_usedByRef )
                    qDebug() << n->d_name << "used by ref" << n->d_loc.d_row << mod->getName();
                else if( n && r->d_usedByVal )
                    qDebug() << n->d_name << "used by val" << n->d_loc.d_row << mod->getName();
            }
#endif
        }
    }

    Type* addAddrOf(Ref<Expression>& actual)
    {
        Ref<UnExpr> ue = new UnExpr();
        ue->d_loc = actual->d_loc;
        ue->d_op = UnExpr::ADDROF;
        ue->d_sub = actual;
        Ref<Pointer> ptr = new Pointer();
        ptr->d_loc = actual->d_loc;
        ptr->d_unsafe = true;
        ptr->d_to = actual->d_type.data();
        ue->d_type = ptr.data();
        actual = ue.data();
        mod->d_helper2.append(ptr.data()); // otherwise ptr gets deleted when leaving this scope
        return ptr.data();
    }

    void checkCallArg( ProcType* pt, Parameter* formal, Ref<Expression>& actual )
    {
        Type* tf = derefed(formal->d_type.data());
        Type* ta = derefed(actual->d_type.data());
        if( tf == 0 )
            return; // error already handled

        if( ta == 0 )
        {
            // happens when e.g. a procedure with no return type is used as an argument
            error( actual->d_loc, Validator::tr("this expression cannot be used as actual parameter"));
            return;
        }

        const int tftag = tf->getTag();
        Array* af = tftag == Thing::T_Array ? cast<Array*>(tf) : 0;
        const int tatag = ta->getTag();

        // TODO: do we check readability of actual param idents?

#ifdef OBX_BBOX
        if( ( tftag == Thing::T_Record || tftag == Thing::T_Array ) && tatag == Thing::T_Pointer )
        {
            // BBOX does implicit deref of actual pointer when passing to a formal record or array parameter
            // applies also if ta or tf is unsafe
            Ref<UnExpr> arg = new UnExpr(UnExpr::DEREF, actual.data() );
            arg->d_loc = actual->d_loc;
            Pointer* p = cast<Pointer*>(ta);
            ta = derefed(p->d_to.data());
            arg->d_type = p->d_to.data();
            actual = arg.data();
        }

#if 1
        // BBOX supports passing RECORD and ARRAY to var/in or value UNSAFE POINTER parameters,
        // implicit address of supported in Oberon+ in calls to external library module procedures
        if( tftag == Thing::T_Pointer && ( tatag == Thing::T_Record || tatag == Thing::T_Array
                                           || ta == bt.d_stringType || ta == bt.d_wstringType ||
                                           ta == bt.d_byteArrayType ) )
        {
            const bool memloc = isMemoryLocation(actual.data());
            const bool notpriv = actual->visibilityFor(mod) != Named::Private;
            const bool scalarr = isArrayOfUnstructuredType(ta);
            const bool strlit = ta == bt.d_stringType || ta == bt.d_wstringType || ta == bt.d_byteArrayType;
            if( /* pt->d_unsafe && */ // no, allow it for all params; doesnt make sense to allow it
                    // on assignment everywhere, but at the same time only for passing to unsafe procs
                    tf->d_unsafe &&
                memloc && notpriv &&
                ( ta->d_unsafe || ( pt->d_unsafe && (strlit || scalarr ) ) ) )
                // ok, if actual is unsafe,
                // or proc is unsafe and actual is string literal
                // or proc is unsafe and actual is safe array to unstructured type
                ta = addAddrOf(actual);
        }
#endif
#endif

#if 0
        // too strong
        if( tftag == Thing::T_Pointer && tatag == Thing::T_Pointer && isInParam(actual.data())
                && !formal->d_var && !formal->d_const )
        {
            error( actual->d_loc, Validator::tr("IN pointer cannot be passed by value"));
            return;
        }
#endif

#ifdef OBX_SUPPORT_CB_DELEG_
        if( tftag == Thing::T_ProcType && tf->d_unsafe && !tf->d_typeBound
                && tatag == Thing::T_ProcType && ta->d_typeBound )
        {
            ProcType* pf = cast<ProcType*>(tf);
            ProcType* pa = cast<ProcType*>(ta);
            if( matchingFormalParamLists(pf,pa) )
                return; // works with CilGen out of the box; not feasible in plain C because we need a
                        // unique function pointer per callback (i.e. the function pointer must
                        // uniquely represent the receiver id too)
        }
#endif
#ifdef _OBX_USE_NEW_FFI_
        if( pt->d_unsafe && tf->d_unsafe && tftag == Thing::T_Pointer &&
                ta->isInteger() && ta->getBaseType() != Type::LONGINT )
        {
            Type* fpt = derefed(cast<Pointer*>(tf)->d_to.data());
            if( fpt->getBaseType() == Type::CVOID )
            {
                warning(actual->d_loc,Validator::tr("passing %1 to *VOID").
                        arg(BaseType::s_typeName[ta->getBaseType()]));
                return; // allow passing up to 32 bit integers to *void params of unsafe procedures
                        // if we allow it for all procedures this could be misused for pointer arithmetic
            }
        }
#endif

        if( formal->d_var && !formal->d_const )
        {
            // check if VAR really gets a physical location
            bool ok = false;
            ok = isMemoryLocation(actual.data());
            if( ta->isString() )
                ok = false; // string literals cannot be passed to a VAR param (but to IN)
#ifdef OBX_BBOX
            if( ta == bt.d_nilType )
            {
                Q_ASSERT( tftag == Thing::T_Pointer );
                ok = true;
            }
#endif
            if( !ok )
            {
                error( actual->d_loc, Validator::tr("cannot pass this expression to a VAR parameter") );
                return;
            }else
                checkValidLhs(actual.data());
        }
        if( formal->d_var && !formal->d_unsafe && ta->d_unsafe && ta->isStructured() )
        {
            error( actual->d_loc, Validator::tr("cannot pass this expression to a VAR or IN parameter") );
            return;
        }else if( !formal->d_var && af && !formal->d_unsafe && ta->d_unsafe )
        {
            error( actual->d_loc, Validator::tr("cannot pass this expression to an ARRAY parameter") );
            return;
        }
        const QString var = formal->d_var ? formal->d_const ? "IN " : "VAR " : "";

        checkValidRhs(actual.data());

        if( af && ( af->d_lenExpr.isNull() || af->d_vla || ta->d_vla ) )
        {
            // If Tf is an open array, then a must be array compatible with f
            if( !arrayCompatible( af, actual->d_type.data(), actual->d_loc ) )
            {
                error( actual->d_loc,
                       Validator::tr("actual open array parameter type %1 not compatible with formal type of %2%3")
                       .arg(actual->d_type->pretty()).arg(var).arg(formal->d_type->pretty()));
                return;
            }
        }
        else
        {
            // Otherwise Ta must be parameter compatible to f
            if( !paramCompatible( formal, actual.data() ) )
            {
                error( actual->d_loc,
                   Validator::tr("actual parameter type %1 not compatible with formal type %2%3")
                   .arg(actual->d_type->pretty()).arg(var).arg(formal->d_type->pretty()));
            }
        }
    }

    void checkCallArgs( ProcType* p, ArgExpr* me )
    {
        if( p->d_varargs && me->d_args.size() < p->d_formals.size() )
        {
            error( me->d_loc, Validator::tr("at least %1 actual parameters required").arg(p->d_formals.size()));
            return;
        }else if( !p->d_varargs && p->d_formals.size() != me->d_args.size() )
        {
            error( me->d_loc, Validator::tr("number of actual and formal parameters doesn't match"));
            return;
        }

        for( int i = 0; i < p->d_formals.size(); i++ )
        {
            checkCallArg( p, p->d_formals[i].data(), me->d_args[i] ); // varargs are not considered here
        }
        for( int i = p->d_formals.size(); i < me->d_args.size(); i++ )
        {
            Expression* a = me->d_args[i].data();
            Type* t = derefed(a->d_type.data());
            if( t == 0 )
                continue; // error reported
            if( isArrayOfUnstructuredType(t) || t->isString() )
            {
                Q_ASSERT( p->d_unsafe ); // only unsafe procs can be variadic
                addAddrOf(me->d_args[i]);
            }else if( !( t->d_unsafe || !t->isStructured(true) ) )
                error( a->d_loc, Validator::tr("actual parameter type not supported in variadic procedure call"));
        }
        Named* n = me->d_sub->getIdent();
        if( n && n->getTag() == Thing::T_Procedure && levels.back().scope->getTag() == Thing::T_Procedure )
        {
#if 0
            // we cannot do it here because not all procedures have been visited yet
            ProcType* curProc = cast<ProcType*>(levels.back().scope->d_type.data());
            foreach( Named* nl, p->d_nonLocals )
            {
                if( false ) // TODO nl->d_scope != levels.back().scope )
                {
                    levels.back().scope->d_upvalIntermediate = true;
                    curProc->addNonLocal(nl);
                }
            }
#else
            Procedure* curProc = cast<Procedure*>(levels.back().scope);
            Procedure* calledProc = cast<Procedure*>(n);
            curProc->d_calling.insert(calledProc);
#endif
        }else
        {
            Q_ASSERT(p->d_nonLocals.isEmpty());
        }
    }

    Type* calcBuiltInReturnType( BuiltIn* bi, const ExpList& args )
    {
        switch( bi->d_func )
        {
        case BuiltIn::BITNOT:
        case BuiltIn::BITSHL:
        case BuiltIn::BITSHR:
        case BuiltIn::BITASR:
        case BuiltIn::LSL:
        case BuiltIn::ASR:
        case BuiltIn::ASH:
            if( !args.isEmpty() )
            {
                Type* td = derefed(args.first()->d_type.data());
                if( td && td->getBaseType() == Type::LONGINT )
                    return bt.d_longType;
                else
                    return bt.d_intType;
            }
            break;
        case BuiltIn::FLOOR:
        case BuiltIn::ENTIER:
            if( !args.isEmpty() )
            {
                Type* td = derefed(args.first()->d_type.data());
                if( td && td->getBaseType() == Type::LONGREAL )
                    return bt.d_longType;
                else
                    return bt.d_intType;
            }
            break;
        case BuiltIn::FLT:
            if( !args.isEmpty() )
            {
                Type* td = derefed(args.first()->d_type.data());
                if( td && td->getBaseType() == Type::LONGINT )
                    return bt.d_longrealType;
                else
                    return bt.d_realType;
            }
            break;
        case BuiltIn::BITAND:
        case BuiltIn::BITOR:
        case BuiltIn::BITXOR:
            if( !args.isEmpty() )
            {
                Type* lhs = derefed(args.first()->d_type.data());
                Type* rhs = derefed(args.last()->d_type.data());
                if( (lhs && lhs->getBaseType() == Type::LONGINT) ||
                    (rhs && rhs->getBaseType() == Type::LONGINT) )
                    return bt.d_longType;
                else
                    return bt.d_intType;
            }
            break;
        case BuiltIn::ADR:
        case BuiltIn::SYS_VAL:
        case BuiltIn::SYS_ROT:
        case BuiltIn::SYS_LSH:
        case BuiltIn::CAST:
        case BuiltIn::ABS:
        case BuiltIn::CAP:
            if( !args.isEmpty() )
                return args.first()->d_type.data();
            break;
        case BuiltIn::SHORT:
            if( !args.isEmpty() )
            {
                Type* t = derefed(args.first()->d_type.data());
                if( t == bt.d_longType )
                    return bt.d_shortType;
                else if( t == bt.d_shortType )
                    return bt.d_byteType;
                else if( t == bt.d_intType )
                    return bt.d_shortType;
                else if( t == bt.d_longrealType )
                    return bt.d_realType;
#ifdef OBX_BBOX
                else if( t == bt.d_wcharType || charArrayType(t) == bt.d_wcharType )
                    return bt.d_charType;
#endif
                else
                    error( args.first()->d_loc, Validator::tr("SHORT not applicable to given argument"));
            }
            break;
        case BuiltIn::LONG:
            if( !args.isEmpty() )
            {
                Type* t = derefed(args.first()->d_type.data());
                if( t == bt.d_charType || t == bt.d_byteType )
                    return bt.d_shortType;
                else if( t == bt.d_shortType )
                    return bt.d_intType;
                else if( t == bt.d_intType )
                    return bt.d_longType;
                else if( t == bt.d_realType )
                    return bt.d_longrealType;
#ifdef OBX_BBOX
                else if( t == bt.d_charType || charArrayType(t) == bt.d_charType )
                    return bt.d_wcharType;
#endif
                else
                    error( args.first()->d_loc, Validator::tr("LONG not applicable to given argument"));
            }
            break;
        case BuiltIn::MIN:
        case BuiltIn::MAX:
            if( args.size() == 1 )
            {
                if( derefed(args.first()->d_type.data()) == bt.d_setType )
                    return bt.d_intType;
                else
                    return args.first()->d_type.data();
            }else if( args.size() == 2 )
                return inclusiveType1(derefed(args.first()->d_type.data()),derefed(args.last()->d_type.data())).first;
            break;
        case BuiltIn::DEFAULT:
            if( args.size() == 1 )
            {
                Type* td = derefed(args.first()->d_type.data());
                switch( td->getTag() )
                {
                case Thing::T_Pointer:
                case Thing::T_ProcType:
                    return bt.d_nilType;
                default:
                    return args.first()->d_type.data();
                }
            }
            break;
        case BuiltIn::ORD:
            if( !args.isEmpty() )
            {
                bool wide;
                if( isCharConst(args[0].data(), &wide) )
                    return wide ? bt.d_shortType: bt.d_byteType;

                Type* t = derefed(args.first()->d_type.data());
                if( t == bt.d_charType || t == bt.d_boolType )
                    return bt.d_byteType;
                if( t == bt.d_wcharType )
                    return bt.d_shortType;
                else
                    return bt.d_intType;
            }
            break;
        default:
            {
                Q_ASSERT( bi->d_type && bi->d_type->getTag() == Thing::T_ProcType );
                Type* t = cast<ProcType*>(bi->d_type.data())->d_return.data();
                if( t == 0 )
                    t = bt.d_noType;
                return t;
            }
            break;
        }
        return 0;
    }

    void visit( ArgExpr* me )
    {
        // NOTE: a function call has always an ArgExpr even if it is empty.

        Q_ASSERT( me->d_op == UnExpr::CALL || me->d_op == UnExpr::IDX ); // defaults to CALL

        if( me->d_sub.isNull() )
            return;
        me->d_sub->accept(this);

        foreach( const Ref<Expression>& e, me->d_args )
            e->accept(this);

        Type* subType = derefed( me->d_sub->d_type.data() );
        if( me->d_op == UnExpr::CALL )
        {
            // check whether this might be a cast and if a call whether there is an appropriate procedure type
            Named* decl = me->d_args.size() == 1 ? me->d_args.first()->getIdent() : 0;
            if( subType && subType->getTag() == Thing::T_ProcType )
            {
                // this is a call
                ProcType* p = cast<ProcType*>( subType );
                const bool isBuiltIn = p->d_decl && p->d_decl->getTag() == Thing::T_BuiltIn;
                if( !isBuiltIn || !checkBuiltInArgs( p, me ) )
                    checkCallArgs( p, me );
                if( me->d_sub->getIdent() )
                {
                    // me->d_sub->getIdent() not always equal to p->d_ident because of type aliasing
                    switch( me->d_sub->getTag() )
                    {
                    case Thing::T_IdentLeaf:
                        cast<IdentLeaf*>(me->d_sub.data())->d_role = CallRole;
                        break;
                    case Thing::T_IdentSel:
                        cast<IdentSel*>(me->d_sub.data())->d_role = CallRole;
                        break;
                    default:
                        Q_ASSERT( false );
                    }
                }
                // TODO: avoid calling a not-in bound proc from an in this
                if( isBuiltIn )
                {
                    // for some built-in procs the return type is dependent on proc arguments
                    if( me->d_type.isNull() )
                        me->d_type = calcBuiltInReturnType( cast<BuiltIn*>(p->d_decl), me->d_args );
                }else
                {
                    if( p->d_decl && p->d_decl->getTag() == Thing::T_Procedure )
                        p->d_decl->d_used = true;
                    me->d_type = p->d_return.data();
                    if( me->d_type.isNull() )
                        me->d_type = bt.d_noType;
                        // even procs return a type with BaseType::NONE so that the generator knows if to emit a pop
                }
            }else if( decl && decl->getTag() == Thing::T_NamedType )
            {
                // this is a type guard
                me->d_op = UnExpr::CAST;
                me->d_type = decl->d_type.data();
                bool fromPtr, toPtr;
                Record* rfrom = subType ? subType->toRecord(&fromPtr) : 0;
                Record* rto = decl->d_type->toRecord(&toPtr);
                Named* subId = me->d_sub->getIdent();
                if( rfrom && !fromPtr && rto && !toPtr &&
                        ( subId == 0
                               || subId->getTag() != Thing::T_Parameter
                               || !cast<Parameter*>(subId)->d_var ) )
                    error(me->d_loc, Validator::tr("type guard not supported on record type unless it's a VAR/IN parameter"));
                else if( rfrom == 0 || rto == 0 || fromPtr != toPtr )
                    error( me->d_loc, Validator::tr("type guard requires both operand and type to be record or pointer to record") );
                else if( rfrom->d_unsafe || rto->d_unsafe )
                    error(me->d_loc, Validator::tr("type guard not supported on unsafe types"));
            }else
                error( me->d_loc, Validator::tr("this expression cannot be called") );
        }else if( me->d_op == UnExpr::IDX )
        {
            Q_ASSERT( !me->d_args.isEmpty() );
            // sub must be a pointer to an array or an array
            if( subType && subType->getTag() == Thing::T_Pointer )
            {
                Pointer* p = cast<Pointer*>(subType);
                if( p->d_to.isNull() )
                    return; // error already reported

                // The designator p^[e] may be abbreviated p[e], i.e. array selectors imply dereferencing.
                // So add a deref to the AST.
                Ref<UnExpr> deref = new UnExpr();
                deref->d_op = UnExpr::DEREF;
                deref->d_sub = me->d_sub;
                me->d_sub = deref.data();
                deref->d_loc = me->d_loc;
                subType = derefed(p->d_to.data());
                deref->d_type = subType;
                if( deref->d_type.isNull() )
                    return; // error already reported
            }
            if( subType == 0 || subType->getTag() != Thing::T_Array )
            {
                error( me->d_loc, Validator::tr("no array dimension corresponds to this index selector") );
                return;
            }

            // subType before points to array
            subType = cast<Array*>(subType)->d_type.data();
            if( subType == 0 )
                return; // already reported
            // subType after points to array base type (which might be yet another array)

            for( int i = 0; i < me->d_args.size(); i++ )
            {
                Type* td = derefed(me->d_args[i]->d_type.data());
                if( td && !td->isInteger() )
                    error( me->d_args[i]->d_loc, Validator::tr("expecting integer index") );
            }

            if( me->d_args.size() > 1 )
            {
                Q_ASSERT(false); // no longer used since Parser::selector already makes a chain of ArgExpr
                // Modify AST so that each IDX only has one dimension and me is the last element in the chain
                Ref<ArgExpr> prev;
                for( int i = 0; i < me->d_args.size() - 1; i++ )
                {
                    Ref<ArgExpr> dim = new ArgExpr();
                    if( i == 0 )
                        dim->d_sub = me->d_sub;
                    else
                        dim->d_sub = prev.data();
                    prev = dim;
                    dim->d_op = ArgExpr::IDX;
                    dim->d_args << me->d_args[i];
                    dim->d_loc = me->d_args[i]->d_loc;
                    dim->d_type = subType;
                    subType = derefed(subType);
                    if( subType && subType->getTag() == Thing::T_Array )
                    {
                        subType = cast<Array*>(subType)->d_type.data();
                        if( subType == 0 )
                            return;
                    }else
                    {
                        error( me->d_loc, Validator::tr("index has more dimensions than array") );
                        return;
                    }
                }
                me->d_sub = prev.data();
                Ref<Expression> idx = me->d_args.last();
                me->d_args.clear();
                me->d_args << idx;
                me->d_loc = idx->d_loc;
                me->d_type = subType;
            }else
                me->d_type = subType;
        }else
            Q_ASSERT(false);
    }

    void visit( UnExpr* me )
    {
        Q_ASSERT( me->d_op == UnExpr::NEG || me->d_op == UnExpr::NOT || me->d_op == UnExpr::DEREF );

        if( me->d_sub.isNull() )
            return;
        me->d_sub->accept(this);

        // prev must be a pointer or a record
        Type* prevT = derefed( me->d_sub->d_type.data() );
        if( prevT == 0 )
            return;

        switch( me->d_op )
        {
        case UnExpr::DEREF:
            switch( prevT->getTag() )
            {
            case Thing::T_Pointer:
                {
                    Pointer* p = cast<Pointer*>(prevT);
                    me->d_type = p->d_to.data();
                    if( derefed(me->d_type.data()) == bt.d_voidType )
                        error( me->d_loc, Validator::tr("*void cannot be dereferenced") );
                }
                break;
            case Thing::T_ProcType:
                {
                    QList<Expression*> desig = me->d_sub->getSubList();
                    Named* id1 = 0;
                    if( desig.size() == 2 || ( desig.size() == 3 && desig[1]->getUnOp() == UnExpr::DEREF ) )
                    {
                        id1 = desig.first()->getIdent();
                        if( id1 && id1->getTag() == Thing::T_Parameter )
                        {
                            Named* id2 = desig.last()->getIdent();
                            if( id2 && id2->getTag() == Thing::T_Procedure )
                            {
                                Procedure* p = cast<Procedure*>(id2);
                                if( p->d_receiver.data() == id1 )
                                {
                                    Named* super = p->d_receiverRec && p->d_receiverRec->d_baseRec ?
                                                p->d_receiverRec->d_baseRec->find(p->d_name, true) : 0;
                                    if( super && super->getTag() == Thing::T_Procedure )
                                        me->d_type = p->d_type.data();
                                    else
                                        error( me->d_loc, Validator::tr("invalid super call (identifier '%1' is not "
                                                                        "the receiver parameter of proc '%2')" )
                                               .arg(id1->d_name.constData()).arg(p->d_name.constData()));
                                }else
                                    error( me->d_loc, Validator::tr("invalid super call (other procedure than the one called from)" ) );
                            }else
                                error( me->d_loc, Validator::tr("invalid super call (not designating a procedure)" ) );
                        }else
                            error( me->d_loc, Validator::tr("invalid super call (identifier not a receiver parameter)") );
                    }else
                        error( me->d_loc, Validator::tr("invalid super call (expecting identifier referencing bound procedure") );
                }
                break;
            default:
                error( me->d_loc, Validator::tr("only a pointer can be dereferenced") );
                break;
            }
            break;
        case UnExpr::NEG:
            if( isNumeric(prevT) || prevT == bt.d_setType )
            {
                if( prevT->getBaseType() == Type::BYTE )
                    me->d_type = bt.d_shortType;
                else
                    me->d_type = prevT;
            }else
                error( me->d_loc, Validator::tr("sign inversion only applicable to numeric or set types") );
            break;
        case UnExpr::NOT:
            if( prevT == bt.d_boolType )
                me->d_type = prevT;
            else
                error( me->d_loc, Validator::tr("negation only applicable to boolean types") );
            break;
        }
    }

    void visit( BinExpr* me )
    {
        if( me->d_lhs.isNull() || me->d_rhs.isNull() )
            return;

        me->d_lhs->accept(this);
        me->d_rhs->accept(this);

        Type* lhsT = derefed( me->d_lhs->d_type.data() );
        Type* rhsT = derefed( me->d_rhs->d_type.data() );
        if( lhsT == 0 || rhsT == 0 )
            return;

        const int ltag = lhsT->getTag();
        const int rtag = rhsT->getTag();

        bool lwchar, rwchar;
        switch( me->d_op )
        {
        case BinExpr::Range: // int
            if( isInteger(lhsT) && isInteger(rhsT) )
                me->d_type = rhsT;
            else if( isCharConst(me->d_lhs.data(),&lwchar) && isCharConst(me->d_rhs.data(),&rwchar) )
                me->d_type = lwchar || rwchar ? bt.d_wcharType : bt.d_charType;
            else if(lhsT->getBaseType() == Type::ENUMINT && rhsT->getBaseType() == Type::ENUMINT )
                me->d_type = rhsT;
            else
                error( me->d_loc, Validator::tr("range operator expects operands to be of integer, enumeration or character type") );
            break;

        case BinExpr::EQ:
        case BinExpr::NEQ:
            // TODO: shouldn't records and arrays of the same type be comparable?
            if( ( isNumeric(lhsT) && isNumeric(rhsT) ) ||
                    ( isTextual(lhsT) && isTextual(rhsT) ) || // cannot compare pointer to array with string
                    ( lhsT == bt.d_boolType && rhsT == bt.d_boolType ) ||
                    ( lhsT == bt.d_setType && rhsT == bt.d_setType ) ||
                    ( ltag == Thing::T_Enumeration && lhsT == rhsT ) ||
                    ( ( lhsT == bt.d_nilType || ltag == Thing::T_Pointer ) &&
                      ( rtag == Thing::T_Pointer || rhsT == bt.d_nilType ) ) ||
                    ( lhsT == bt.d_nilType && rtag == Thing::T_ProcType ) ||
                    ( ltag == Thing::T_ProcType && rhsT == bt.d_nilType ) ||
                    ( ltag == Thing::T_ProcType && rtag == Thing::T_ProcType &&
                      lhsT->d_typeBound == rhsT->d_typeBound ) ||
                    ( lhsT == bt.d_anyType && rhsT == bt.d_anyType ) ) // because of generics
            {
                me->d_type = bt.d_boolType;
                if( isNumeric(lhsT) && isNumeric(rhsT) )
                {
                    QPair<Type*,bool> t = inclusiveType1(lhsT,rhsT);
                    if( t.first )
                    {
                        me->d_inclType = t.first->d_baseType;
                        if( !t.second )
                            conversionWarning(lhsT,rhsT,me->d_loc);
                    }
                }
            }else
            {
                // qDebug() << "lhsT" << lhsT->getTagName() << "rhsT" << rhsT->getTagName();
                error( me->d_loc, Validator::tr("operands of the given type cannot be compared") );
            }
            break;

        case BinExpr::LT:
        case BinExpr::LEQ:
        case BinExpr::GT:
        case BinExpr::GEQ:
            if( ( isNumeric(lhsT) && isNumeric(rhsT) ) ||
                    ( ltag == Thing::T_Enumeration && lhsT == rhsT ) ||
                    ( isTextual(lhsT) && isTextual(rhsT) ) )
            {
                me->d_type = bt.d_boolType;
                if( isNumeric(lhsT) && isNumeric(rhsT) )
                {
                    QPair<Type*,bool> t = inclusiveType1(lhsT,rhsT);
                    if( t.first )
                    {
                        me->d_inclType = t.first->d_baseType;
                        if( !t.second )
                            conversionWarning(lhsT,rhsT,me->d_loc);
                    }
                }
            }else
            {
                // qDebug() << "lhsT" << lhsT->getTagName() << "rhsT" << rhsT->getTagName();
                error( me->d_loc, Validator::tr("operands of the given type cannot be compared") );
            }
            break;

        case BinExpr::IN:
            if( isInteger(lhsT) && rhsT == bt.d_setType )
                me->d_type = bt.d_boolType;
            else
                error( me->d_loc, Validator::tr("operator 'IN' expects left operand in 0..MAX(SET) and right operand of SET") );
            break;

        case BinExpr::IS:
            {
                // v IS T -> lhs IS rhs

                // v IS T stands for the dynamic type of v is T (or an extension of T ) and is called a type test.
                //   It is applicable if:
                // 1) v is a variable parameter of record type or v is a pointer, and if
                // 2) T is an extension of the static type of v.
                Named* n = me->d_lhs->getIdent();
                const int ltag = lhsT->getTag();
                const bool isVarParamOfRecordType = ltag == Thing::T_Record && n
                        && n->getTag() == Thing::T_Parameter && cast<Parameter*>(n)->d_var;
                const bool isPointerToRecord = ltag == Thing::T_Pointer && lhsT->toRecord();
                if( me->d_rhs->getIdent() == 0 || me->d_rhs->getIdent()->getTag() != Thing::T_NamedType )
                    error( me->d_rhs->d_loc, Validator::tr("right side of IS operator must be a named type") );
                else if( !isVarParamOfRecordType && !isPointerToRecord )
                    error( me->d_lhs->d_loc, Validator::tr("left side is neither a VAR parameter nor a pointer to record") );
                else if( !typeExtension( lhsT , rhsT ) )
                    error( me->d_loc, Validator::tr("the type on the right side is not an extension of the static type of the left side") );
                else
                    me->d_type = bt.d_boolType;
            }
            break;

        case BinExpr::ADD:
        case BinExpr::SUB:
        case BinExpr::MUL:
            if( isNumeric(lhsT) && isNumeric(rhsT) )
            {
                QPair<Type*,bool> t = inclusiveType1(lhsT,rhsT);
                me->d_type = t.first;
                if( t.first && !t.second )
                    conversionWarning(lhsT,rhsT,me->d_loc);
            }else if( lhsT == bt.d_setType && rhsT == bt.d_setType )
                me->d_type = bt.d_setType;
#ifdef OBX_BBOX
            else if( me->d_op == BinExpr::ADD && lhsT->isText(&lwchar) && rhsT->isText(&rwchar)
                     && !lhsT->d_unsafe && !rhsT->d_unsafe )
            {
                // NOTE: because of Blackbox we had isTextual(str,true) so far, but there is actually only one place
                // in BB minimal where a pointer is added; added a deref, case closed.

                if( lwchar || rwchar )
                    me->d_type = bt.d_wstringType;
                else
                    me->d_type = bt.d_stringType;
            }
#endif
            else
                error( me->d_loc, Validator::tr("operator '%1' expects both operands to "
                                                "be either of numeric, SET or text type").arg( BinExpr::s_opName[me->d_op]) );
            break;

        case BinExpr::FDIV:
            if( isNumeric(lhsT) && isNumeric(rhsT) )
            {
                me->d_type = inclusiveType2(lhsT,rhsT);
            }else if( lhsT == bt.d_setType || rhsT == bt.d_setType )
                me->d_type = bt.d_setType;
            else
                error( me->d_loc, Validator::tr("operator '/' expects both operands to be either of numeric or SET type") );
            break;

        case BinExpr::DIV:  // int
        case BinExpr::MOD:  // int
            {
                if( !isInteger(lhsT ) )
                    error( me->d_lhs->d_loc, Validator::tr("integer type expected for left side of MOD or DIV operator") );
                if( !isInteger(rhsT ) )
                    error( me->d_rhs->d_loc, Validator::tr("integer type expected for right side of MOD or DIV operator") );
                QPair<Type*,bool> t = inclusiveType1(lhsT,rhsT);
                me->d_type = t.first;
                if( t.first && !t.second )
                    conversionWarning(lhsT,rhsT,me->d_loc);
            }
            break;

        case BinExpr::OR:  // bool
        case BinExpr::AND:  // bool
            if( lhsT != bt.d_boolType )
                error( me->d_lhs->d_loc, Validator::tr("boolean type expected for left side of logical operator") );
            if( rhsT != bt.d_boolType )
                error( me->d_rhs->d_loc, Validator::tr("boolean type expected for right side of logical operator") );
            me->d_type = bt.d_boolType;
            break;

        default:
            Q_ASSERT( false );
            break;
        }
    }

    void visit( SetExpr* me )
    {
        foreach( const Ref<Expression>& e, me->d_parts )
        {
            e->accept(this);
            Type* t = derefed(e->d_type.data());
            if( t == 0 )
                continue; // error already handled
            if( !isInteger( t ) && t != bt.d_setType )
                error( e->d_loc, Validator::tr("set constructor expects operands to be integers in 0..MAX(SET)") );
        }
        me->d_type = bt.d_setType;
    }

    void visit( Literal* me )
    {
        const qint64 i = me->d_val.toLongLong();
        switch( me->d_vtype )
        {
        case Literal::Enum:
            break; // keep the Enumeration type
        case Literal::Integer:
            if( i >= 0 && i <= bt.d_byteType->maxVal().toInt()
                    && !me->d_wide && !me->d_minInt )
                me->d_type = bt.d_byteType;
            else if( i >= bt.d_shortType->minVal().toInt() && i <= bt.d_shortType->maxVal().toInt()
                     && !me->d_wide && !me->d_minInt )
                me->d_type = bt.d_shortType;
            else if( i >= bt.d_intType->minVal().toInt() && i <= bt.d_intType->maxVal().toInt()
                     && !me->d_wide )
                me->d_type = bt.d_intType;
            else
                me->d_type = bt.d_longType;
            break;
        case Literal::Real:
            if( me->d_wide )
                me->d_type = bt.d_longrealType;
            else
                me->d_type = bt.d_realType;
            break;
        case Literal::Boolean:
            me->d_type = bt.d_boolType;
            break;
        case Literal::String:
            if( me->d_wide )
                me->d_type = bt.d_wstringType;
            else
                me->d_type = bt.d_stringType;
            break;
        case Literal::Bytes:
            me->d_type = bt.d_byteArrayType;
            break;
        case Literal::Char:
            me->d_strLen = 1;
            if( i > 255 )
                me->d_type = bt.d_wcharType;
            else
                me->d_type = bt.d_charType;
            if( i > 0xffff || ( i >= 0xd800 && i < 0xe000) )
                warning( me->d_loc, Validator::tr("character is not in the Unicode Basic Multilingual Plane (BMP)") );
            break;
        case Literal::Nil:
            me->d_type = bt.d_nilType;
            break;
        case Literal::Set:
            me->d_type = bt.d_setType;
            break;
        default:
            Q_ASSERT( false );
            break;
        }
        Q_ASSERT( !me->d_type.isNull() &&
                  ( me->d_type->getTag() == Thing::T_BaseType || me->d_type->getTag() == Thing::T_Enumeration ));
    }

    ///////// Types

    void visit( Pointer* me )
    {
        if( me->d_visited )
            return;
        me->d_visited = true;

        if( mod->d_externC && !me->d_unsafe )
            error(me->d_loc, Validator::tr("POINTER not supported in external library modules; use CPOINTER or *") );
#if 0   // maybe too strong, but we still can import named types
        // it's definitely too strong; would force us to declare all pointer types in external library module
        if( !mod->d_externC && me->d_unsafe )
            error(me->d_loc, Validator::tr("CPOINTER only supported in external library modules; use POINTER or ^") );
#endif

        if( !me->d_to.isNull() )
        {
            me->d_to->accept(this);
            Type* t = derefed(me->d_to.data());
            checkNoVla(t, me->d_loc);
            if( me->d_unsafe )
            {
                if( t && !( ( t->isStructured() && t->d_unsafe ) || t->getBaseType() == Type::CVOID ) )
                    error( me->d_loc, Validator::tr("CPOINTER must point to a CARRAY, CSTRUCT, CUNION or VOID") );
            }else
            {
                if( t && ( !t->isStructured() || t->d_unsafe ) )
                    error( me->d_loc, Validator::tr("POINTER must point to a RECORD or ARRAY") );
            }
        }
    }

    void visit( Array* me )
    {
        if( me->d_visited )
            return;
        me->d_visited = true;

        if( !me->d_lenExpr.isNull() )
        {
            me->d_lenExpr->accept(this);
            if( !isInteger(derefed(me->d_lenExpr->d_type.data()) ) )
            {
                error( me->d_lenExpr->d_loc, Validator::tr("expression doesn't evaluate to an integer") );
            }else if( me->d_vla )
            {
                // if me->d_lenExpr references a local var in this scope because it is unset at this time
                if( !levels.isEmpty() )
                {
                    VlaChecker checker(levels.back().scope,err);
                    me->d_lenExpr->accept(&checker);
                }else
                    error(me->d_lenExpr->d_loc,Validator::tr("variable length array types can only be declared on procedure level") );
                me->d_len = 0;
            }else
            {
                Scope* scope = levels.back().scope;
                Evaluator::Result res = Evaluator::eval(me->d_lenExpr.data(), scope, false,err);
                if( res.d_dyn )
                {
                    me->d_len = 0;
                }else
                {
                    bool ok;
                    const qint64 len = res.d_value.toLongLong(&ok);
                    if( res.d_vtype != Literal::Integer || !ok || len <= 0 )
                    {
                        me->d_len = -1;
                        error( me->d_lenExpr->d_loc, Validator::tr("expecting positive non-zero integer for array length") );
                    }else if( len > std::numeric_limits<qint32>::max() )
                    {
                        me->d_len = -1;
                        error( me->d_lenExpr->d_loc, Validator::tr("maximum supported array length is MAX(INTEGER)") );
                    }else
                        me->d_len = len;
                }
            }
        }
        if( me->d_type )
            me->d_type->accept(this);
        if( !me->d_vla )
            checkNoVla(me->d_type.data(), me->d_type->d_loc);
        checkNoAnyRecType(me->d_type.data());
        checkNoBooleanTypeInUnsafe(me->d_type.data(),me->d_unsafe, me->d_loc);
        checkSelfRef(me);
        if( me->d_type && me->d_type->getTag() == Thing::T_Array && !me->d_lenExpr.isNull() )
        {
            Array* a = cast<Array*>(me->d_type.data());
            if( a->d_lenExpr.isNull() )
                error( me->d_type->d_loc, Validator::tr("only open arrays can have open array element types") );
        }

#ifdef OBX_BBOX
        if( me->d_unsafe )
        {
#if 0
            // too strong; we need to be able to allocate carray e.g. on the stack; we still can do it using imported named types
            // but actually it doesn't harm; otherwise we would have to declare carray in library module for all required lengths
            // this is mostly about anonymous local type definitions in variable declarations, not in named type declarations
            if( !mod->d_externC )
                error(me->d_loc, Validator::tr("CARRAY only supported in external library modules; use ARRAY or [] instead") );
#endif
            Type* t = derefed(me->d_type.data());
            if( t && t->isStructured(true) && !t->d_unsafe )
                error( me->d_loc, Validator::tr("CARRAY cannot have safe non-basic element types") );
#if 1
            if( t && t->getTag() == Thing::T_Array )
                error( me->d_loc, Validator::tr("CARRAY can only have one dimension") );
#endif

        }else
        {
            Type* t = derefed(me->d_type.data());
            if( t->d_unsafe && t->isStructured() )
                error( me->d_type->d_loc, Validator::tr("ARRAY cannot have unsafe structured element types") );

            if( mod->d_externC )
                error(me->d_loc, Validator::tr("ARRAY not supported in external library modules; use CARRAY instead") );
        }
#endif
    }

    void visit( Enumeration* me )
    {
        if( me->d_visited )
            return;
        me->d_visited = true;

        foreach( const Ref<Const>& c, me->d_items )
        {
            c->accept(this);
            c->d_type->d_baseType = Type::ENUMINT;
        }
    }

    void visit( QualiType* me )
    {
        if( me->d_visited )
            return;
        me->d_visited = true;

        if( !me->d_quali.isNull() )
            me->d_quali->accept(this);
    }

    void visit( Record* me )
    {
        if( me->d_visited )
            return;
        me->d_visited = true;

#if 0
        if( !me->d_flag.isNull() )
            me->d_flag->accept(this);
#endif

#ifdef _OBX_USE_NEW_FFI_
#if 0 // no, we need this feature because the debugger cannot display unsafe arrays, so we can at least use local records
      // with embedded arrays
        // maybe too strong; we need to be able to allocate cstruct e.g. on the stack; we still can import named types
        // but it doesn't harm; we can only use them by value, and we can take their address by passing/assigning to a *type
        // but we should not support declaring cstruct types throughout the app; using the types in var decls is ok though.
        if( !mod->d_externC && me->d_unsafe )
        {
            if( me->d_union )
                error(me->d_loc, Validator::tr("CUNION only supported in external library modules") );
            else
                error(me->d_loc, Validator::tr("CSTRUCT only supported in external library modules; use RECORD instead") );
        }else
#endif
#endif
        if( mod->d_externC && !me->d_unsafe )
            error(me->d_loc, Validator::tr("RECORD not supported in external library modules; use CSTRUCT instead") );


#ifdef _OBX_USE_NEW_FFI_
        if( !me->d_base.isNull() && me->d_unsafe )
            error( me->d_base->d_loc, Validator::tr("A CSTRUCT cannot have a base type") );
            // some cstruct in BBOX inherit from COM.IUnknown, fixed
        else
#endif
        if( !me->d_base.isNull() )
        {
            me->d_base->accept(this);

            Type* base = derefed( me->d_base->d_quali->d_type.data() );
            if( base && base->getTag() == Thing::T_Pointer )
                base = derefed(cast<Pointer*>(base)->d_to.data());
            if( base && base->getTag() == Thing::T_Record)
            {
                me->d_baseRec = cast<Record*>(base);
                me->d_baseRec->d_subRecs.append(me);
            }else
                error( me->d_base->d_loc, Validator::tr("base type must be a record") );

            if( me->d_baseRec )
            {
                if( me->d_unsafe && me->d_union )
                    error( me->d_base->d_loc, Validator::tr("A CUNION cannot have a base type") );
                else if( me->d_baseRec->d_unsafe && !me->d_unsafe )
                    error( me->d_base->d_loc, Validator::tr("CSTRUCT or CUNION cannot be the base type of a RECORD") );
                else if( !me->d_baseRec->d_unsafe && me->d_unsafe )
                    error( me->d_base->d_loc, Validator::tr("RECORD cannot be the base type of a CSTRUCT") );
            }

            Record* baseRec = me->d_baseRec;
            while(baseRec)
            {
                if( baseRec->d_baseRec == me )
                {
                    error( me->d_base->d_loc, Validator::tr("record cannot be its own base type") );
                    baseRec->d_baseRec->d_subRecs.removeAll(baseRec);
                    baseRec->d_baseRec = 0; // to avoid infinite loop in code using the AST
                    break;
                }
                baseRec = baseRec->d_baseRec;
            }

#if 0
            // cstruct cannot have base type so we don't need this
            if( me->d_baseRec && me->d_baseRec->d_unsafe != me->d_unsafe )
                error( me->d_base->d_loc, Validator::tr("cstruct cannot inherit from record and vice versa") );
                // in BBOX regular records inherit from COM cstructs
#endif
            if( me->d_baseRec && me->d_unsafe && !me->d_baseRec->d_unsafe  )
                error( me->d_base->d_loc, Validator::tr("CSTRUCT cannot inherit from record") ); // at least this is true
        }
        foreach( const Ref<Field>& f, me->d_fields )
        {
            f->accept(this);

            checkSelfRef(f->d_type.data());
            checkNoVla(f->d_type.data(),f->d_loc);
#ifdef OBX_BBOX
            if( me->d_unsafe )
            {
                Type* t = derefed(f->d_type.data());
                if( t && t->isStructured(true) && !t->d_unsafe )
                    error( f->d_loc, Validator::tr("CSTRUCT or CUNION cannot have safe non-basic field types") );
            }else
            {
                Type* t = derefed(f->d_type.data());
                if( t->d_unsafe && t->isStructured() )
                    error( f->d_loc, Validator::tr("RECORD cannot have unsafe structured field types") );
            }
#endif

            Named* found = me->d_baseRec ? me->d_baseRec->find( f->d_name, true ) : 0;
            if( found  )
            {
#ifdef OBX_BBOX
                bool ok = false;
                if( found->getTag() == Thing::T_Field )
                {
                    // BBOX supports covariance also for record fields; a field with a pointer type of a superclass can be
                    // redefined in a  subclass if the field type of the super class is an extension of the field type of
                    // the subclass; this is an undocumented BBOX feature.
                    Field* ff = cast<Field*>(found);
                    Type* super = derefed(ff->d_type.data());
                    Type* sub = derefed(f->d_type.data());
                    if( sub && sub->getTag() == Thing::T_Pointer && super && super->getTag() == Thing::T_Pointer &&
                            typeExtension( super, sub ) )
                    {
                        f->d_super = ff;
                        ok = true;
                    }
                }
                if( !ok )
#endif
                error( f->d_loc, Validator::tr("field name collides with a name in the base record") );
            }
        }
        // note that bound procedures are handled in the procedure visitor
    }

    void checkNoArrayByVal(Type* t, const RowCol& loc )
    {
        t = derefed(t);
        if( t && t->getTag() == Thing::T_Array )
            error(loc, Validator::tr("arrays cannot be passed by value in external procedures") );
    }

    void visit( ProcType* me )
    {
        if( me->d_visited )
            return;
        me->d_visited = true;

        if( !me->d_return.isNull() )
        {
            me->d_return->accept(this);
            if( !me->d_return->hasByteSize() )
                error(me->d_return->d_loc, Validator::tr("this type cannot be used here") );
            checkNoVla(me->d_return.data(),me->d_return->d_loc);
            checkNoBooleanTypeInUnsafe(me->d_return.data(), me->d_unsafe, me->d_loc);

            if( me->d_unsafe )
                checkNoArrayByVal(me->d_return.data(), me->d_return->d_loc);

#if 0 // TEST
            checkUnsafePointer(me->d_return.data(), true, me->d_loc);
#endif
            checkRecordUse(me->d_return.data());

        }

        if( me->d_unsafe && me->d_typeBound )
            error(me->d_loc, Validator::tr("type-bound procedure types are not supported in external library modules") );

        checkNoAnyRecType(me->d_return.data());
        foreach( const Ref<Parameter>& p, me->d_formals )
        {
            p->accept(this);
            checkRecordUse(p->d_type.data(), p->d_var);
            checkNoVla(p->d_type.data(),p->d_loc);
        }
    }

    //////// Others

    void visit( NamedType* me )
    {
        curTypeDecl = me->d_type.data();
        if( me->d_type )
            me->d_type->accept(this);
        checkNoVla(me->d_type.data(), me->d_loc);
        checkNoAnyRecType(me->d_type.data());
        checkNoBooleanTypeInUnsafe(me->d_type.data(), me->d_unsafe, me->d_loc);
        curTypeDecl = 0;
    }

    void visit( Const* me )
    {
        if( me->d_constExpr.isNull() || me->d_visited )
            return;
        if( constTrace.contains(me) )
        {
            error(me->d_loc, Validator::tr("const depends on itself"));
            return;
        }
        constTrace.insert(me);
        me->d_constExpr->accept(this);
        me->d_type = me->d_constExpr->d_type.data();
        Evaluator::Result res = Evaluator::eval(me->d_constExpr.data(), mod, false, err);
        me->d_val = res.d_value;
        me->d_vtype = res.d_vtype;
        me->d_wide = res.d_wide;
        me->d_strLen = res.d_strLen;
        me->d_visited = true;
        constTrace.remove(me);
        Type* td = derefed(me->d_type.data());
        if( td && td->isInteger() )
        {
#if 0
            bool overflow = false;
            qint64 v = me->d_val.toLongLong();
            switch(td->getBaseType())
            {
            case Type::BYTE:
                if( v > 255 || v < 0 )
                    overflow = true;
                break;
            case Type::SHORTINT:
                if( v > std::numeric_limits<qint16>::max() || v < std::numeric_limits<qint16>::min() )
                    overflow = true;
                break;
            case Type::INTEGER:
            case Type::SET:
            case Type::ENUMINT:
                if( v > std::numeric_limits<qint32>::max() || v < std::numeric_limits<qint32>::min() )
                    overflow = true;
                break;
            }
            if( overflow )
                warning(me->d_constExpr->d_loc, Validator::tr("value is out of range") );
#else
            // if the type according to expression rules is not wide enough to accomodate the number, widen it
            qint64 v = me->d_val.toLongLong();
            Type* newType = td;
            if( newType->getBaseType() == Type::BYTE &&
                    ( v < 0 || v > bt.d_byteType->maxVal().toInt() ) )
                newType = bt.d_shortType;
            if( newType->getBaseType() == Type::SHORTINT &&
                    ( v < bt.d_shortType->minVal().toInt() || v > bt.d_shortType->maxVal().toInt() ) )
                newType = bt.d_intType;
            if( newType->getBaseType() == Type::INTEGER &&
                    ( v < bt.d_intType->minVal().toInt() || v > bt.d_intType->maxVal().toInt() ) )
                newType = bt.d_longType;
            if( newType != td )
            {
                warning(me->d_constExpr->d_loc, Validator::tr("widened type from %1 to %2")
                        .arg(BaseType::s_typeName[td->getBaseType()])
                        .arg(BaseType::s_typeName[newType->getBaseType()]));
                me->d_type = newType;
            }
#endif
        }
    }

    void visit( Field* me )
    {
        checkVarType(me);
        checkNoVla(me->d_type.data(),me->d_loc);
    }

    void visit( Variable* me )
    {
        checkVarType(me);
        if( mod->d_externC )
            error(me->d_loc, Validator::tr("variables not supported in external library modules") );
        else
            checkNoVla(me->d_type.data(),me->d_loc);
    }

    void visit( LocalVar* me )
    {
        checkVarType(me);
    }

    void visit( Parameter* me )
    {
        if( me->d_type )
            me->d_type->accept(this);
        if( !me->d_var )
            checkNoAnyRecType(me->d_type.data());

        checkNoBooleanTypeInUnsafe(me->d_type.data(), me->d_unsafe, me->d_loc);
        Type* td = derefed(me->d_type.data());
        if( td == 0 )
            return; // already reported
        if( me->d_unsafe && me->d_var )
            error(me->d_loc, Validator::tr("VAR not supported in external library modules") );
        else if( td->d_unsafe && me->d_var && td->isStructured() )
            error(me->d_loc, Validator::tr("VAR not supported with unsafe structured types") );
        if( me->d_unsafe )
            checkNoArrayByVal(me->d_type.data(),me->d_loc);
        const int tag = td->getTag();
#if 0
        // not good; too much code dependend on IN pointer, e.g. in Awfy
        // instead we could forbid assignment/actual arg of IN pointer to another pointer
        if( me->d_var && me->d_const && !td->d_unsafe && tag != Thing::T_Record && tag != Thing::T_Array )
            error(me->d_loc, Validator::tr("IN can only be used for array and record parameters") );
#endif
        checkNoVla(me->d_type.data(),me->d_loc);

#if 0 // TEST
        checkUnsafePointer(me->d_type.data(),false,me->d_loc);
#endif

        // open array value parameter are supported in all old Oberon/-2, Oberon-07 and BBOX
        if( !me->d_unsafe && tag == Thing::T_Array )
        {
            Array* a = cast<Array*>( td );
            if( a->d_lenExpr.isNull() && !me->d_var )
                warning( me->d_loc, Validator::tr("passing an array by value might be inefficient") );
                // TODO: check body if the array is lhs at all
        }
    }

    //////// Statements

    void visit( IfLoop* me )
    {
        Q_ASSERT( !levels.isEmpty() );

        if( me->d_op == IfLoop::LOOP )
            levels.back().loops.push_back(me);

        foreach( const Ref<Expression>& e, me->d_if )
        {
            if( !e.isNull() )
            {
                e->accept(this);
                if( !e->d_type.isNull() && derefed( e->d_type.data() ) != bt.d_boolType )
                    error( e->d_loc, Validator::tr("expecting boolean expression"));
            }
        }

        for( int ifThenNr = 0; ifThenNr < me->d_then.size(); ifThenNr++ )
        {
            Ref<Type> orig;
            Named* caseId = 0;
            if( me->d_op == IfLoop::WITH && ifThenNr < me->d_if.size() )
            {
                Q_ASSERT( me->d_if[ifThenNr]->getTag() == Thing::T_BinExpr );
                BinExpr* guard = cast<BinExpr*>(me->d_if[ifThenNr].data());
                Q_ASSERT( guard->d_op == BinExpr::IS );

                caseId = guard->d_lhs->getIdent();
                Type* lhsT = derefed(guard->d_lhs->d_type.data());
                Type* rhsT = derefed(guard->d_rhs->d_type.data());
                if( caseId != 0 && lhsT != 0 && rhsT != 0 )
                {
                    orig = caseId->d_type;
                    caseId->d_type = rhsT;

                    const bool isRec = lhsT->getTag() == Thing::T_Record;
                    const bool isRecPtr = isPointerToRecord(lhsT);

                    const int tag = caseId->getTag();
                    // caseId must be Variable, LocalVar, Parameter or Field
                    if( ( !isRecPtr && !isRec ) ||
                            ( isRec && !caseId->isVarParam() ) ||
                            !( tag == Thing::T_Variable || tag == Thing::T_LocalVar ||
                               tag == Thing::T_Parameter || tag == Thing::T_Field ) )
                        error( me->d_if.first()->d_loc,
                               Validator::tr("guard must be a VAR parameter of record type or a pointer variable") );
                } // else error already reported
            }

            visitStats( me->d_then[ifThenNr] );

            if( caseId != 0 && !orig.isNull() )
                caseId->d_type = orig;

        }

        visitStats( me->d_else );

        if( me->d_op == IfLoop::LOOP )
            levels.back().loops.pop_back();
    }

    void visit( Return* me )
    {
        Q_ASSERT( !levels.isEmpty() );

        Procedure* p = levels.back().scope->getTag() == Thing::T_Procedure ? cast<Procedure*>(levels.back().scope) : 0;
        if( p == 0 )
        {
            error( me->d_loc, Validator::tr("return statement only supported in procedure bodies"));
            return;
        }
        ProcType* pt = p->getProcType();
        Q_ASSERT( pt != 0 );
        if( pt->d_return.isNull() && !me->d_what.isNull() )
            error( me->d_loc, Validator::tr("cannot return expression in a proper procedure"));
        else if( !pt->d_return.isNull() && me->d_what.isNull() )
            error( me->d_loc, Validator::tr("expecting return expression in a function procedure"));

        if( !me->d_what.isNull() )
        {
            returnValueFound = true;
            me->d_what->accept(this);
            if( me->d_what->d_type.isNull() )
                return;
        }

        if( !pt->d_return.isNull() && !me->d_what.isNull() )
        {
            checkValidRhs(me->d_what.data());
#if 0
            // too strong
            Type* ld = derefed(pt->d_return.data());
            Type* rd = derefed(me->d_what->d_type.data());
            if( ld == 0 || rd == 0 )
                return; // already reported
            if( ld->getTag() == Thing::T_Pointer && rd->getTag() == Thing::T_Pointer && isInParam(me->d_what.data()) )
                error( me->d_what->d_loc, Validator::tr("IN pointer cannot be returned"));
#endif
            if( !assignmentCompatible(pt->d_return.data(), me->d_what.data() ) )
                error( me->d_what->d_loc, Validator::tr("return expression is not assignment compatible with function return type"));
        }

        // TODO: check in a function whether all paths return a value
    }

    void visit( Exit* me )
    {
        Q_ASSERT( !levels.isEmpty() );

        if( levels.back().loops.isEmpty() )
            error( me->d_loc, Validator::tr("exit statement requires an enclosing loop statement") );
    }

    static inline void markIdent( bool lhs, Expression* e, int level = 0 )
    {
        if( level > 1 )
            return;
        switch( e->getTag() )
        {
        case Thing::T_IdentLeaf:
            cast<IdentLeaf*>(e)->d_role = lhs ? LhsRole : RhsRole;
            break;
        case Thing::T_IdentSel:
            cast<IdentSel*>(e)->d_role = lhs ? LhsRole : RhsRole;
            break;
        case Thing::T_UnExpr:
            if( e->getUnOp() == UnExpr::DEREF )
                markIdent( lhs, cast<UnExpr*>(e)->d_sub.data(), level + 1 );
            break;
        case Thing::T_ArgExpr:
            if( e->getUnOp() == UnExpr::IDX )
                markIdent( lhs, cast<ArgExpr*>(e)->d_sub.data(), level + 1 );
            break;
        }
    }

    void visit( Assign* me )
    {
        if( me->d_lhs.isNull() || me->d_rhs.isNull() )
            return; // error already reported
        me->d_lhs->accept(this);
        markIdent( true, me->d_lhs.data() );
        me->d_rhs->accept(this);
        markIdent( false, me->d_rhs.data() );
        if( !checkValidLhs(me->d_lhs.data()) )
            return;

        Type* lhsT = derefed(me->d_lhs->d_type.data());
        Type* rhsT = derefed(me->d_rhs->d_type.data());

        if( lhsT == 0 || rhsT == 0 )
            return; // already reported

        const int rhsTag = rhsT->getTag();

        if( rhsTag == Thing::T_BaseType && rhsT->getBaseType() == Type::NONE )
        {
            error(me->d_rhs->d_loc,Validator::tr("procedure doesn't return a value") );
            return;
        }
#ifdef OBX_BBOX
        const int lhsTag = lhsT->getTag();
        if( ( lhsTag == Thing::T_Record || lhsTag == Thing::T_Array ) && rhsTag == Thing::T_Pointer )
        {
            // BBOX does implicit deref of rhs pointer in assignment to lhs record or array
            Ref<UnExpr> ue = new UnExpr(UnExpr::DEREF, me->d_rhs.data() );
            ue->d_loc = me->d_rhs->d_loc;
            me->d_rhs = ue.data();
            Pointer* p = cast<Pointer*>(rhsT);
            rhsT = derefed(p->d_to.data());
            me->d_rhs->d_type = p->d_to.data();
        }

#if 1
        if( lhsTag == Thing::T_Pointer && ( rhsTag == Thing::T_Record || rhsTag == Thing::T_Array ) )
        {
            // in BBOX the assignment of a structured value to an unsafe pointer is an "address of" operation
            // in Oberon+ this is only allowed for unsafe types
            if( lhsT->d_unsafe && rhsT->d_unsafe )
                // don't allow taking the address of safe types
            {
#if 1
                rhsT = addAddrOf(me->d_rhs);
#else
                Ref<UnExpr> ue = new UnExpr();
                ue->d_loc = me->d_rhs->d_loc;
                ue->d_op = UnExpr::ADDROF;
                ue->d_sub = me->d_rhs.data();
                Ref<Pointer> ptr = new Pointer();
                ptr->d_loc = me->d_rhs->d_loc;
                ptr->d_unsafe = true;
                ptr->d_to = me->d_rhs->d_type.data();
                rhsT = ptr.data();
                ue->d_type = ptr.data();
                me->d_rhs = ue.data();
                mod->d_helper2.append(ptr.data()); // otherwise ptr gets deleted when leaving this scope
#endif
            }
        }
#endif
#endif

#if 0
        // too strong
        if( lhsTag == Thing::T_Pointer && rhsTag == Thing::T_Pointer && isInParam(me->d_rhs.data()) )
            error( me->d_rhs->d_loc, Validator::tr("IN pointer cannot be assigned"));
#endif

        if( lhsT->getTag() == Thing::T_Array && me->d_rhs->getTag() == Thing::T_Literal )
        {
            Array* a = cast<Array*>(lhsT);
            Type* at = derefed(a->d_type.data());
            Literal* lit = cast<Literal*>( me->d_rhs.data() );
            if( at && at->isChar() )
            {
                // Both Oberon-2 and 07 support this
                // TODO: check wchar vs char compat
                if( !a->d_lenExpr.isNull() && !a->d_vla && lit->d_vtype == Literal::String && lit->d_strLen > a->d_len )
                    error( me->d_rhs->d_loc, Validator::tr("string is too long to assign to given character array"));
                if( !a->d_lenExpr.isNull() && !a->d_vla && lit->d_vtype == Literal::Char && 1 > a->d_len )
                    error( me->d_rhs->d_loc, Validator::tr("the character array is too small for the character"));
            }else if( at && at == bt.d_byteType )
            {
                if( !a->d_lenExpr.isNull() && !a->d_vla && lit->d_vtype == Literal::Bytes && lit->d_strLen > a->d_len )
                    error( me->d_rhs->d_loc, Validator::tr("literal is too long to assign to given array"));
            }
        }

        checkValidRhs(me->d_rhs.data());

        // lhs and rhs might have no type which might be an already reported error or the attempt to assign no value
        if( !assignmentCompatible( me->d_lhs->d_type.data(), me->d_rhs.data() ) )
        {
            const QString lhs = !me->d_lhs->d_type.isNull() ? me->d_lhs->d_type->pretty() : QString("");
            const QString rhs = !me->d_rhs->d_type.isNull() ? me->d_rhs->d_type->pretty() : QString("");

            error( me->d_rhs->d_loc, Validator::tr("right side %1 of assignment is not compatible with left side %2")
                   .arg(rhs).arg(lhs) );
            // assignmentCompatible( me->d_lhs->d_type.data(), me->d_rhs.data() ); // TEST
        }
    }

    void checkValidRhs( Expression* rhs )
    {
        // TODO: do we check readability of rhs idents?
        if( rhs == 0 || rhs->d_type.isNull() )
            return; // reported elsewhere
        if( rhs->d_type->getTag() == Thing::T_ProcType )
        {
            Named* n = rhs->getIdent();
            const int tag = n ? n->getTag() : 0;
            if( tag == Thing::T_Procedure )
            {
                if( rhs->d_type->d_typeBound )
                {
                    // rhs is a type bound procedure
                    const int etag = rhs->getTag();
                    if( etag == Thing::T_IdentLeaf )
                        error( rhs->d_loc, Validator::tr("a designator is required to assign a type-bound procedure"));
                    else
                    {
                        Q_ASSERT( etag == Thing::T_IdentSel );
                        IdentSel* sel = cast<IdentSel*>(rhs);
                        if( sel->d_sub.isNull() )
                            return; // already reported
                        if( sel->d_sub->getUnOp() != UnExpr::DEREF )
                            error( rhs->d_loc, Validator::tr("only a type-bound procedure designated by a pointer can be assigned"));
                    }
                }else
                {
                    deferProcCheck.append(rhs);
                    n->d_used = true;
                }

            }else if( tag == Thing::T_BuiltIn )
                error( rhs->d_loc, Validator::tr("a predeclared procedure cannot be assigned"));
        }
    }

    void visit( Call* me )
    {
        // TODO: check that bound proc is not directly (ie without self designator) called
        if( !me->d_what.isNull() )
        {
            me->d_what->accept(this);
            Expression* proc = me->d_what.data();
            if( proc->getTag() != Thing::T_ArgExpr )
            {
                Type* t = derefed(proc->d_type.data());
                if( t == 0 || t->getTag() != Thing::T_ProcType )
                {
                    if( prevStat && prevStat->getTag() == Thing::T_Return && !levels.isEmpty()
                            && levels.back().scope->getTag() == Thing::T_Procedure &&
                            cast<Procedure*>( levels.back().scope )->getProcType()->d_return.isNull() )
                        error( me->d_loc, Validator::tr("qualifier following return statement; is the function return type missing?") );
                    else
                        error( me->d_loc, Validator::tr("cannot call this expression") );
                    return;
                }
                ProcType* pt = cast<ProcType*>(t);
                if( !pt->d_formals.isEmpty() )
                {
                    error( me->d_loc, Validator::tr("this procedure requires actual arguments") );
                    return;
                }
                Ref<ArgExpr> ae = new ArgExpr();
                ae->d_op = UnExpr::CALL;
                ae->d_loc = me->d_what->d_loc;
                ae->d_type = pt->d_return.data();
                ae->d_sub = proc;
                me->d_what = ae.data();
                proc = ae.data();
            }
            Q_ASSERT( proc->getTag() == Thing::T_ArgExpr );
            ArgExpr* ae = cast<ArgExpr*>(proc);
#if 0
            // no, we allow that to avoid having to declar a lot of dummy vars when not using the result
            if( !ae->d_type.isNull() )
            {
                error( me->d_loc, Validator::tr("cannot use a function procedure in a call statement") ); // TODO: why not?
                return;
            }
#endif
            proc = ae->d_sub.data();
            Type* t = derefed(proc->d_type.data());
            if( ae->d_op != UnExpr::CALL || t == 0 || t->getTag() != Thing::T_ProcType )
            {
                error( me->d_loc, Validator::tr("cannot call this expression") );
                return;
            }
#if 0 // called twice?? already called in me->d_what->accept(this); above
            ProcType* pt = cast<ProcType*>(t);
            const bool isBuiltIn = pt->d_decl && pt->d_decl->getTag() == Thing::T_BuiltIn;
            if( !isBuiltIn || !checkBuiltInArgs( pt, ae ) )
                checkCallArgs( pt, ae );
#endif
            Named* id = proc->getIdent();
            if( id )
            {
                switch( proc->getTag() )
                {
                case Thing::T_IdentLeaf:
                    cast<IdentLeaf*>(proc)->d_role = CallRole;
                    break;
                case Thing::T_IdentSel:
                    cast<IdentSel*>(proc)->d_role = CallRole;
                    break;
                default:
                    Q_ASSERT( false );
                }
            }
        }// else error already reported
    }

    void visit( ForLoop* me )
    {
        me->d_id->accept(this);
        Type* enumType = 0;
        if( !me->d_from.isNull() )
        {
            me->d_from->accept(this);
            Type* td = derefed(me->d_from->d_type.data());
            if( td && td->getTag() == Thing::T_Enumeration )
                enumType = td;
            if( td == 0 || !( isInteger(td) || enumType ) )
                error( me->d_from->d_loc, Validator::tr("expecting an integer or enumeration as start value of the for loop"));
        }
        if( !me->d_to.isNull() )
        {
            me->d_to->accept(this);
            Type* td = derefed(me->d_to->d_type.data());
            if( td && td->getTag() == Thing::T_Enumeration && ( td != enumType || enumType == 0 ) )
                error( me->d_to->d_loc, Validator::tr("must be of the same enumeration type as the start value"));
            else if( td == 0 || !( isInteger(td) || enumType ) )
                error( me->d_to->d_loc, Validator::tr("expecting an integer or enumeration as end value of the for loop"));
        }
        if( !me->d_by.isNull() )
        {
            me->d_by->accept(this);
            if( enumType )
                error( me->d_by->d_loc, Validator::tr("BY not allowed when using enumerated start and end values"));
            else if( !me->d_by->d_type.isNull() && !isInteger(derefed(me->d_by->d_type.data())) )
                error( me->d_by->d_loc, Validator::tr("expecting an integer as the step value of the for loop"));
            else
            {
                Evaluator::Result res = Evaluator::eval(me->d_by.data(), mod, false, err);
                if( res.d_vtype != Literal::Integer )
                    error( me->d_by->d_loc, Validator::tr("expecting an integer as the step value of the for loop"));
                me->d_byVal = res.d_value;
            }
        }else if( enumType )
        {
            Evaluator::Result from = Evaluator::eval(me->d_from.data(), mod, false, err);
            Evaluator::Result to = Evaluator::eval(me->d_to.data(), mod, false, err);
            const int val = from.d_value <= to.d_value ? 1 : -1;
            me->d_by = new Literal( Literal::Integer, me->d_loc, val, enumType);
            me->d_byVal = val;
        }else
        {
            me->d_by = new Literal( Literal::Integer, me->d_loc, 1, bt.d_intType);
            me->d_byVal = 1;
        }
        visitStats( me->d_do );
    }

    static qint64 toInt(const Evaluator::Result& res )
    {
        if( res.d_vtype == Literal::Integer || res.d_vtype == Literal::Enum || res.d_vtype == Literal::Char )
            return res.d_value.toLongLong();
        if( res.d_vtype == Literal::String )
        {
            const QByteArray str = res.d_value.toByteArray();
            if( !str.isEmpty() )
                return (quint8)str[0];
        }
        return 0;
    }

    void visit( CaseStmt* me )
    {
        if( me->d_exp.isNull() )
            return;

        me->d_exp->accept(this);
        Type* te = derefed(me->d_exp->d_type.data());
        if( te == 0 )
            return; // already reported

        if( !te->isChar() && !te->isInteger() && te->toRecord() == 0 && te->getTag() != Thing::T_Enumeration )
            error( me->d_exp->d_loc, Validator::tr("case expression must be a character, integer, enumeration or record") );

        Ref<Type> orig;

        Named* caseId = me->d_exp->getIdent();
        if( caseId != 0 && !caseId->d_type.isNull() )
        {
            Type* caseType = derefed(caseId->d_type.data());
            orig = caseId->d_type;

            const bool isRec = caseType->getTag() == Thing::T_Record;
            const bool isRecPtr = isPointerToRecord(caseType);

            if( isRecPtr || isRec )
            {
                const int tag = caseId->getTag();
                // caseId must be Variable, LocalVar, Parameter or Field
                if( ( isRec && !caseId->isVarParam() ) ||
                        !( tag == Thing::T_Variable || tag == Thing::T_LocalVar ||
                           tag == Thing::T_Parameter || tag == Thing::T_Field ) )
                    error( me->d_exp->d_loc,
                           Validator::tr("type case variable must be a VAR parameter of record type or a pointer variable") );
                else
                    me->d_typeCase = true;
            }
        }
        QList< QPair<qint64,qint64> > ranges;
        foreach( const CaseStmt::Case& c, me->d_cases )
        {
            foreach( const Ref<Expression>& e, c.d_labels )
            {
                if( !e.isNull() )
                {
                    e->accept(this);
                    if( !me->d_typeCase )
                    {
                        bool lwide,rwide;
                        Type* tl = derefed(e->d_type.data() );
                        if( ( te->isInteger() && !includes(te,tl) ) ||
                            ( te->isChar(&lwide) && ( !isCharConst(e.data(),&rwide) || (!lwide && rwide) ) ) ||
                                ( te->getTag() == Thing::T_Enumeration && te != tl ))
                            error( e->d_loc, Validator::tr("label expression type not compatible with case expression type"));
                        if( e->getBinOp() == BinExpr::Range )
                        {
                            BinExpr* be = cast<BinExpr*>(e.data());
                            const qint64 a = toInt(Evaluator::eval(be->d_lhs.data(), levels.back().scope,err));
                            const qint64 b = toInt(Evaluator::eval(be->d_rhs.data(), levels.back().scope,err));
                            if( a <= b )
                                ranges.append(qMakePair(a,b));
                            else
                                ranges.append(qMakePair(b,a));
                        }else
                        {
                            const qint64 a = toInt(Evaluator::eval(e.data(), levels.back().scope,err));
                            ranges.append(qMakePair(a,a));
                        }
                    }
                }
            }

            if( me->d_typeCase )
            {
                Type* to = derefed(orig.data());
                if( c.d_labels.size() == 1 && derefed(c.d_labels.first()->d_type.data()) == bt.d_nilType )
                {
                    if( to && isPointerToRecord(to) )
                        ; // no, just leafe it as is; caseId->d_type = bt.d_nilType;
                    else
                    {
                        error( c.d_labels.first()->d_loc, Validator::tr("nil only acceptable if case variable is a pointer type"));
                        continue;
                    }
                }else if( c.d_labels.size() != 1 || c.d_labels.first()->getIdent() == 0 )
                {
                    Q_ASSERT(!c.d_labels.isEmpty());
                    error( c.d_labels.first()->d_loc, Validator::tr("expecting a qualident case label in a type case statement"));
                    continue;
                }else if( !typeExtension( to, derefed(c.d_labels.first()->getIdent()->d_type.data()) ) )
                {
                    Q_ASSERT(!c.d_labels.isEmpty());
                    error( c.d_labels.first()->d_loc, Validator::tr("case label must be a subtype of the case variable in a type case statement"));
                    continue;
                }else
                    caseId->d_type = c.d_labels.first()->getIdent()->d_type;
            }else
            {
                qSort( ranges );
                for( int i = 1; i < ranges.size(); i++ )
                {
                    if( ranges[i].first <= ranges[i-1].second )
                    {
                        error( me->d_loc, Validator::tr("no case label must occur more than once"));
                        break;
                    }
                }
            }

            visitStats( c.d_block );
        }

        if( me->d_typeCase )
            caseId->d_type = orig;

        visitStats( me->d_else );
    }

    void visit( Import* me)
    {
        if( me->d_visited )
            return;
        me->d_visited = true;
        if( mod->d_externC && me->d_mod && !me->d_mod->d_externC )
            error( me->d_loc, Validator::tr("regular modules cannot be imported by external library modules") );

        //qDebug() << "check imports of" << mod->getName();
        if( !me->d_metaActuals.isEmpty() )
        {
            if( insts == 0 )
            {
                error( me->d_loc, Validator::tr("this import requires generic module support"));
                return;
            }
            foreach( const Ref<Type>& t, me->d_metaActuals )
            {
                t->accept(this);
                Type* td = t->derefed();
                Q_ASSERT( td != 0 );
                if( !td->hasByteSize() )
                {
                    error( t->d_loc, Validator::tr("this type cannot be used as actual generic type parameter") );
                    return;
                }
                if( td->getTag() == Thing::T_BaseType && td->d_baseType == Type::ANY )
                    continue; // type is yet another generic type; ok, ANY has no d_decl
                Named* n = td->findDecl();
                if( n == 0 || n->getTag() != Thing::T_NamedType )
                {
                    error( t->d_loc, Validator::tr("only named types allowed as actual generic type parameters") );
                    return;
                }
            }
            if( me->d_mod )
                me->d_mod = insts->instantiate( me->d_mod.data(), me->d_metaActuals );
            if( me->d_mod.isNull() )
                return; // already reported
            if( !me->d_mod->d_isValidated )
            {
                qDebug() << "analyzing" << me->d_mod->getName();
                Errors err2;
                err2.setShowWarnings(true);
                err2.setReportToConsole(false);
                err2.setRecord(true);
                Validator::check(me->d_mod.data(),bt, &err2, insts );
                if( err2.getErrCount() || err2.getWrnCount() )
                {
                    error( me->d_loc, Validator::tr("errors when instantiating generic module"));
                    Errors::EntryList::const_iterator i;
                    for( i = err2.getErrors().begin(); i != err2.getErrors().end(); ++i )
                    {
                        const QString msg = (*i).d_msg + QString(", see %1:%2:%3").arg( mod->getName().constData() )
                                .arg( me->d_loc.d_row ).arg(me->d_loc.d_col);
                        if( (*i).d_isErr )
                            err->error((Errors::Source)(*i).d_source, (*i).d_file, (*i).d_line, (*i).d_col, msg);
                        else
                            err->warning((Errors::Source)(*i).d_source, (*i).d_file, (*i).d_line, (*i).d_col, msg);
                    }
                }
                //me->d_mod->dump(); // TEST
            }
        }
    }


    ///////// NOP

    void visit( BaseType* ) { }
    void visit( BuiltIn* ) { }
    void visit( GenericName* ) {}

    ////////// Utility

    inline void checkVarType( Named* me )
    {
        if( me->d_type )
        {
            me->d_type->accept(this);
            me->d_visited = true;
            if( !me->d_type->hasByteSize() )
                error(me->d_type->d_loc, Validator::tr("this type cannot be used here") );
            checkNoAnyRecType(me->d_type.data());
            // checkRecordUse(me->d_type.data());
            checkNoBooleanTypeInUnsafe(me->d_type.data(),me->d_unsafe, me->d_loc);
        }
    }

    void checkNoAnyRecType( Type* t )
    {
        Type* td = derefed(t);
        if( td == bt.d_anyRec || td == bt.d_voidType )
            error(t->d_loc, Validator::tr("this type cannot be used here") );
    }

    inline void checkNoBooleanTypeInUnsafe( Type* t, bool unsafe, const RowCol& loc )
    {
#if 0
        // no, we support boolean as uint8 in cstruct/cunion/carray and int32 on the stack with 0 (false) or 1 (true)
#ifdef _OBX_USE_NEW_FFI_
        if( !mod->d_externC && !unsafe )
            return;
        Type* td = derefed(t);
        if( td == bt.d_boolType )
        {
            if( unsafe )
                error(loc, Validator::tr("BOOLEAN type not supported in CARRAY, CSTRUCT or CUNION") );
            else
                error(loc, Validator::tr("BOOLEAN type not supported in external library modules") );
        }
#endif
#endif
    }

    inline void checkNoVla(Type* t, const RowCol& loc )
    {
        Type* td = derefed(t);
        if( td && td->d_vla )
            error(loc, Validator::tr("cannot use variable length arrays here") );
    }

    void visitStats( const StatSeq& ss )
    {
        Statement* ps = prevStat;
        foreach( const Ref<Statement>& s, ss )
        {
            if( !s.isNull() )
            {
                s->accept(this);
                prevStat = s.data();
            }
        }
        prevStat = ps;
    }

    void error(const RowCol& r, const QString& msg) const
    {
        mod->d_hasErrors = true;
        // d_errCount++;
        err->error( Errors::Semantics, Loc(r,mod->d_file), msg );
    }

    void warning(const RowCol& r, const QString& msg) const
    {
        // d_errCount++;
        err->warning( Errors::Semantics, Loc(r,mod->d_file), msg );
    }

    Type* derefed( Type* t ) const
    {
        if( t )
            return t->derefed();
        else
            return 0;
    }

    inline bool isNumeric(Type* t) const
    {
        // return isInteger(t) || isReal(t);
        if( t == 0 )
            return false;
        return t->isNumeric();
    }

    inline bool isInteger(Type* t) const
    {
        if( t == 0 )
            return false;
        return t->isInteger();
        // return t == bt.d_byteType || t == bt.d_intType || t == bt.d_shortType || t == bt.d_longType;
    }

    inline bool isReal(Type* t) const
    {
        if( t == 0 )
            return false;
        return t->isReal();
        // return t == bt.d_realType || t == bt.d_longrealType;
    }

    inline bool isPointerToRecord(Type* t) const
    {
        if( t->getTag() == Thing::T_Pointer )
        {
            Type* to = derefed(cast<Pointer*>(t)->d_to.data());
            return to && to->getTag() == Thing::T_Record;
        }
        return false;
    }

    bool includes( Type* lhs, Type* rhs ) const
    {
        // expects derefed types
        if( lhs == rhs )
            return true;
        if( lhs == 0 || rhs == 0 )
            return false;
        return Validator::includesType( lhs->getBaseType(), rhs->getBaseType() );
    }

    Type* baseToType( quint8 baseType ) const
    {
        switch( baseType )
        {
        case Type::BYTE:
            return bt.d_byteType;
        case Type::SHORTINT:
            return bt.d_shortType;
        case Type::INTEGER:
            return bt.d_intType;
        case Type::LONGINT:
            return bt.d_longType;
        case Type::REAL:
            return bt.d_realType;
        case Type::LONGREAL:
            return bt.d_longrealType;
        case Type::BOOLEAN:
            return bt.d_boolType;
        case Type::CHAR:
            return bt.d_charType;
        case Type::WCHAR:
            return bt.d_wcharType;
        case Type::SET:
            return bt.d_setType;
        case Type::STRING:
            return bt.d_stringType;
        case Type::WSTRING:
            return bt.d_wstringType;
        case Type::BYTEARRAY:
            return bt.d_byteArrayType;
        case Type::NIL:
            return bt.d_nilType;
        }
        Q_ASSERT(false);
        return 0;
    }

    QPair<Type*,bool> inclusiveType1(Type* lhs, Type* rhs) const
    {
        // expects derefed types
        if( lhs == 0 || rhs == 0 )
            return qMakePair((Type*)0,false);
        QPair<quint8, bool> t = Validator::inclusiveType(lhs->getBaseType(), rhs->getBaseType() );
        return qMakePair(baseToType(t.first),t.second);
    }

    Type* inclusiveType2(Type* lhs, Type* rhs) const
    {
        if( includes( bt.d_realType, lhs ) && includes( bt.d_realType, rhs ) )
            return bt.d_realType;
        else
            return bt.d_longrealType;
    }

    Type* charArrayType( Type* t, bool resolvePointer = true ) const
    {
        Array* a = toCharArray(t, resolvePointer);
        if( a )
            return derefed(a->d_type.data());
        else
            return 0;
    }

    Array* toCharArray( Type* t, bool resolvePointer = true ) const
    {
        if( t == 0 )
            return 0;

        if( t->getTag() == Thing::T_Pointer )
        {
            if( !resolvePointer )
                return 0;
            Pointer* p = cast<Pointer*>( t );
            t = derefed(p->d_to.data());
        }
        Array* a = 0;
        if( t->getTag() == Thing::T_Array )
        {
            a = cast<Array*>( t );
            t = derefed(a->d_type.data());
        }
        if( t && t->isChar() )
            return a;
        else
            return 0;
    }

    inline Type* isTextual( Type* t, bool resolvePointer = false ) const
    {
        if( t && ( t->isChar() || t->isString() ) )
            return t;
        return charArrayType(t, resolvePointer);
    }

    inline bool isCharConst( Expression* e, bool* wide = 0 ) const
    {
        Type* t = derefed(e->d_type.data());
        if( t == 0 )
            return false;
        if( t->isChar() )
        {
            if( wide )
                *wide = t->getBaseType() == Type::WCHAR;
            return true;
        }
        if( t->isString() )
        {
            if( e->getTag() == Thing::T_Literal )
            {
                Literal* l = cast<Literal*>(e);
                if( l->d_strLen == 1 )
                {
                    if( wide )
                        *wide = l->d_wide;
                    return true;
                }
            }
            Named* id = e->getIdent();
            if( id && id->getTag() == Thing::T_Const )
            {
                Const* c = cast<Const*>(id);
                if( c->d_strLen == 1 )
                {
                    if( wide )
                        *wide = c->d_wide;
                    return true;
                }
            }
        }
        return false;
    }

    bool isOpenArray( Type* t ) const
    {
        t = derefed(t);
        if( t && t->getTag() == Thing::T_Array )
        {
            Array* a = cast<Array*>(t);
            if( a->d_lenExpr.isNull() )
                return true;
        }
        return false;
    }

    bool sameType( Type* lhs, Type* rhs ) const
    {
        lhs = derefed(lhs);
        rhs = derefed(rhs);
        if( lhs == 0 || rhs == 0 )
            return false;
        if( lhs->d_vla || rhs->d_vla )
            return false;
        if( lhs == rhs && !isOpenArray(lhs) )
            return true;
        return false;
    }

    bool equalType( Type* lhs, Type* rhs ) const
    {
        if( sameType(lhs,rhs) )
            return true;
        if( lhs == 0 || rhs == 0 )
            return false;
        lhs = derefed(lhs);
        rhs = derefed(rhs);

#ifdef _OBX_USE_NEW_FFI_
        if( lhs->d_unsafe != rhs->d_unsafe )
            return false;
#endif
        const int lhstag = lhs->getTag();
        const int rhstag = rhs->getTag();
        if( lhstag == Thing::T_Array )
        {
            Array* l = cast<Array*>(lhs);
            Type* lt = derefed( l->d_type.data() );
            if( rhstag == Thing::T_Array )
            {
                Array* r = cast<Array*>(rhs);
                // Ta and Tb are open array types with equal element types
                if( l->d_lenExpr.isNull() && r->d_lenExpr.isNull() && !l->d_unsafe && !r->d_unsafe &&
                        equalType( l->d_type.data(), r->d_type.data() ) )
                    return true;

                if( l->d_unsafe && r->d_unsafe && equalType( l->d_type.data(), r->d_type.data() ) )
                    return true;
            }
#if 0
            else if( l->d_lenExpr.isNull() &&
                      ( ( lt == bt.d_charType && rhs == bt.d_stringType ) ||
                        ( lt == bt.d_wcharType && ( rhs == bt.d_stringType || rhs == bt.d_wstringType ) ) ) )
                return true; // because of ADDROF with unsafe pointers
#endif
        }
        if( lhstag == Thing::T_ProcType && rhstag == Thing::T_ProcType )
            return matchingFormalParamLists( cast<ProcType*>(lhs), cast<ProcType*>(rhs) ) &&
                    lhs->d_typeBound == rhs->d_typeBound;
        if( lhstag == Thing::T_Pointer && rhstag == Thing::T_Pointer )
        {
            Pointer* lhsP = cast<Pointer*>(lhs);
            Pointer* rhsP = cast<Pointer*>(rhs);
            return equalType( lhsP->d_to.data(), rhsP->d_to.data() );
        }
        return false;
    }

    bool typeExtension( Type* super, Type* sub, bool checkKind = false ) const
    {
        if( super == 0 || sub == 0 )
            return false;
        if( sameType(super,sub))
            return true;
        bool superIsPointer = false;
        if( super->getTag() == Thing::T_Pointer )
        {
            superIsPointer = true;
            super = derefed( cast<Pointer*>(super)->d_to.data() );
        }
        bool subIsPointer = false;
        if( sub->getTag() == Thing::T_Pointer )
        {
            subIsPointer = true;
            sub = derefed( cast<Pointer*>(sub)->d_to.data() );
        }
        if( checkKind && subIsPointer != superIsPointer )
            return false;
        if( sameType(super,sub))
            return true;
        if( super->getTag() == Thing::T_Record && sub->getTag() == Thing::T_Record )
        {
#ifdef OBX_BBOX
            if( super == bt.d_anyRec )
                return true;
#endif
            Record* superRec = cast<Record*>(super);
            Record* subRec = cast<Record*>(sub);
            if( sameType(superRec,subRec) )
                return true;
            while( subRec && subRec->d_baseRec )
            {
                if( sameType(superRec,subRec->d_baseRec) )
                    return true;
                subRec = subRec->d_baseRec;
            }
            return false;
        }
        return false;
    }

    void conversionWarning(Type* lhs, Type* rhs, const RowCol& loc ) const
    {
        warning( loc, Validator::tr("possible loss of information when converting %1 to %2")
                 .arg(rhs->pretty()).arg(lhs->pretty()));

    }

    bool assignmentCompatible( Type* lhsT, Expression* rhs ) const
    {
        if( lhsT == 0 || rhs == 0 || rhs->d_type.isNull() )
            return false;
        Type* rhsT = rhs->d_type.data();

        if( sameType(lhsT,rhsT) ) // T~e~ and T~v~ are the _same type_
            return true;
        lhsT = derefed(lhsT);
        rhsT = derefed(rhsT);
        if( rhsT == 0 )
            return false;
        const int rtag = rhsT->getTag();

        // T~v~ is a BYTE type and T~e~ is a Latin-1 character type
        // Oberon 90: The type BYTE is compatible with CHAR (shortint is 16 bit here)
        if( lhsT == bt.d_byteType && rhsT == bt.d_charType )
            return true;
#if 0
        // not necessary
        if( lhsT == bt.d_byteType &&
                ( rhsT == bt.d_charType || charArrayType( rhsT ) == bt.d_charType ) ) )
            return true;
#endif

        // T~e~ and T~v~ are numeric or character types and T~v~ _includes_ T~e~
        if( isNumeric(lhsT) && ( isNumeric(rhsT) || rtag == Thing::T_Enumeration ) )
        {
            if( isInteger(lhsT) && rtag == Thing::T_Enumeration )
            {
                // automatically convert enumeration to number
                Enumeration* en = cast<Enumeration*>(rhsT);
                if( en->d_items.size() < 256 )
                    rhsT = bt.d_byteType;
                else if( en->d_items.size() < ( 0xffff >> 1 ) )
                    rhsT = bt.d_shortType;
                else
                    rhsT = bt.d_intType;
            }
            if( !includes(lhsT,rhsT) )
                conversionWarning(lhsT,rhsT,rhs->d_loc);
            return true;
        }else if( lhsT == bt.d_wcharType && rhsT == bt.d_charType )
            return true;

        // T~v~ is a SET type and T~e~ is of INTEGER or smaller type
        if( lhsT == bt.d_setType && ( rhsT == bt.d_intType || rhsT == bt.d_shortType || rhsT == bt.d_byteType ) )
            return true;

        // TODO: can we assign records with private fields? if yes, doesn't this undermine class integrity?

        // T~e~ and T~v~ are record types and T~e~ is a _type extension_ of T~v~ and the dynamic type of v is T~v~
        if( typeExtension(lhsT,rhsT,true) )
            return true;

        const int ltag = lhsT->getTag();

        // T~e~ and T~v~ are pointer types and T~e~ is a _type extension_ of T~v~
        // or the pointers have _equal_ base types
        if( ltag == Thing::T_Pointer )
        {
            if( rtag == Thing::T_Pointer && equalType( lhsT, rhsT ) )
                return true;
#if 0
            // TODO: why do we need this rule? There are actually two cases in BB which need it,
            // but the original rule would allow assignment of safe to unsafe text pointer und vice versa
            // which is not good
            bool lwide, rwide;
            if( rtag == Thing::T_Pointer &&
                    ( lhsT->d_unsafe == rhsT->d_unsafe ) && // TODO: was || instead of == before, but makes no sense
                    lhsT->isText(&lwide,true) && rhsT->isText(&rwide,true) && lwide == rwide )
                return true;
#endif
        }

        // T~v~ is a pointer or a procedure type and `e` is NIL
        if( ( ltag == Thing::T_Pointer || ltag == Thing::T_ProcType ) && rhsT == bt.d_nilType )
            return true;

        // T~v~ is a procedure type and `e` is the name of a procedure whose formal parameters _match_ those of T~v~
        if( ltag == Thing::T_ProcType && rtag == Thing::T_ProcType )
        {
            ProcType* lp = cast<ProcType*>(lhsT);
            ProcType* rp = cast<ProcType*>(rhsT);
            return matchingFormalParamLists( lp, rp ) && lp->d_typeBound == rp->d_typeBound;
        }

#if 1
        if( ( lhsT == bt.d_charType || lhsT == bt.d_wcharType ) && isCharConst(rhs) )
            return true;
#else
        if( ( lhsT == bt.d_charType && rhsT == bt.d_stringType ) ||
            ( lhsT == bt.d_wcharType && rhsT->isString() ) )
        {
            Named* id = rhs->getIdent();
            if( id && id->getTag() == Thing::T_Const )
                return cast<Const*>(id)->d_strLen == 1;
            if( rhs->getTag() == Thing::T_Literal )
                return cast<Literal*>(rhs)->d_strLen == 1;
        }
#endif

        if( ltag == Thing::T_Array )
        {
            // Note:
            // a variable cannot be an open array; therefore if lhs and rhs are same type, they cannot be open arrays
            // lhs can be an open array iff it has a char/wchar base type or it is a value parameter or reference by a pointer
            // in case of a value parameter it is not a real assignment, but the creation of a copy
            // lhs and rhs can be pointer to open arrays, i.e. both sizes have to be dynamically determined
            Array* l = cast<Array*>(lhsT);
            Type* lt = derefed(l->d_type.data());
            if( rtag == Thing::T_Array )
            {
                // Array := Array
                Array* r = cast<Array*>(rhsT);
                Type* rt = derefed(r->d_type.data());
                if( equalType( lt, rt ) && !l->d_unsafe && !r->d_unsafe )
                {
                    if( r->d_lenExpr.isNull() )
                        return true; // T~e~ is an open array and T~v~ is an array of _equal_ base type
                    if( r->d_vla || l->d_vla )
                        return true;
                }
                if( lt == bt.d_charType && rt == bt.d_charType )
                    return true;
                if( lt == bt.d_wcharType && rt->isChar() )
                    // TODO: does assig automatic long(char array)?
                    return true;
                if( lt == bt.d_byteType && rt == bt.d_byteType )
                    return true;

#if 0
                if( l->d_unsafe && r->d_unsafe && !l->d_lenExpr.isNull() && !r->d_lenExpr.isNull()
                        && l->d_len == r->d_len && equalType(lt,rt) )
                    return true;
#endif

                // NOTE: open carrays cannot be rhs because len is unknown at runtime
            }else if( lt == bt.d_charType && ( rhsT == bt.d_stringType || rhsT == bt.d_charType ) )
                // Array := string
                return true;
            else if( lt == bt.d_byteType && rhsT == bt.d_byteArrayType )
                // Array := bytearray
                return true;
            else if( lt == bt.d_wcharType && ( rhsT->isString() || rhsT->isChar() ) )
                return true;
        }

        return false;
    }

    bool paramCompatible( Parameter* lhs, Expression* rhs )
    {
        // T~f~ and T~a~ are _equal_ types
        if( equalType( lhs->d_type.data(), rhs->d_type.data() ) )
            return true;

        Type* tf = derefed(lhs->d_type.data());
        Type* ta = derefed(rhs->d_type.data());
        if( tf == 0 || ta == 0 )
            return false; // error already handled


        const int tftag = tf->getTag();

#ifdef OBX_BBOX
        if( tftag == Thing::T_Pointer )
        {
            Pointer* p = cast<Pointer*>(tf);
            if( p->d_to.data() == bt.d_anyType ) // BBOX supports value and var params of SYSTEM.PTR
                return true;
        }
#endif

        if( lhs->d_var || lhs->d_const )
        {
            // `f` is an IN or VAR parameter

            // Oberon-2: Ta must be the same type as Tf, or Tf must be a record type and Ta an extension of Tf.
            // T~a~ must be the _same type_ as T~f~,
            // or T~f~ must be a record type and T~a~ an _extension_ of T~f~.
            if( sameType( tf, ta ) || typeExtension(tf,ta,true) )
                return true;

            // Oberon 90: If a formal variable parameter is of type ARRAY OF BYTE, then the corresponding
            //   actual parameter may be of any type.
            Type* lat = tf->getTag() == Thing::T_Array ? derefed(cast<Array*>(tf)->d_type.data()) : 0;
            Type* rat = ta->getTag() == Thing::T_Array ? derefed(cast<Array*>(ta)->d_type.data()) : 0;
            if( lat == bt.d_byteType && rat != bt.d_byteType )
            {
                Q_ASSERT( !equalType(tf,ta) );
                warning( rhs->d_loc, Validator::tr("Oberon VAR ARRAY OF BYTE trick not officially supported") );
                return true;
            }

            // Oberon 90: The type BYTE is compatible with CHAR and SHORTINT (shortint is 16 bit here)
            if( tf == bt.d_byteType && ta == bt.d_charType )
                return true;

#ifdef OBX_BBOX
            if( lhs->d_const && rhs->getTag() == Thing::T_Literal &&
                    ( ( lat == bt.d_charType && ta == bt.d_stringType ) ||
                      ( lat == bt.d_wcharType && ta->isString() )
                      ) )
                return true; // BBOX supports passing string literals to IN ARRAY TO CHAR/WCHAR

#if 0
            // no longer necessary since library modules may no longer have VAR/IN parameters; use *type instead
            Record* rf = tf->toRecord();
            if( ta == bt.d_nilType && rf && rf->d_unsafe )
                return true; // BBOX supports passing nil to VAR CSTRUCT, and actually also to VAR INTEGER, but OBX
                             // supports the latter by allowing UNSAFE POINTER TO INTEGER
                             // All these calls go to WinApi and WinNet; the original Win32 signatures are pointers,
                             // not var. So we could well remove this rule and fix the BBOX code, but too many places.
#endif
#endif
            if( lhs->d_const && rhs->getTag() == Thing::T_Literal &&
                    lat == bt.d_byteType && ta == bt.d_byteArrayType )
                return true; // support passing bytearray literals to IN ARRAY TO BYTE

            return false;
        }else
        {

#ifdef _OBX_USE_NEW_FFI_
            if( tf->d_unsafe && tftag == Thing::T_Pointer && ta->d_unsafe && ta->getTag() == Thing::T_Pointer )
            {
                // ta is an unsafe pointer here because of earlier call to addAddrOf()
                Type* fpt = derefed(cast<Pointer*>(tf)->d_to.data());
                Type* apt = derefed(cast<Pointer*>(ta)->d_to.data());
                if( fpt->isText() && apt->isText() ) // covers literals and c/array of char
                    return true;
                if( fpt->isByteArray() && apt->isByteArray() ) // covers bytearray literals and c/array of byte
                    return true;
                if( fpt->getTag() == Thing::T_Array && apt->getTag() == Thing::T_Array && // covers all other arrays
                        equalType( cast<Array*>(fpt)->d_type.data(), cast<Array*>(apt)->d_type.data() ) )
                    return true; // equaltype on array level would fail for safe/unsafe combination generated by ADDROF
                if( fpt->getBaseType() == Type::CVOID )
                    return true; // *void
            }
#endif

            // `f` is a value parameter and T~a~ is _assignment compatible_ with T~f~
            const bool res = assignmentCompatible( lhs->d_type.data(), rhs );
            if( res && tftag != Thing::T_Pointer && typeExtension(tf, ta ) &&
                    tf->toRecord()->d_fields.size() < ta->toRecord()->d_fields.size() )
            {
                // rhs is a subrecord of lhs (if it's the same record type we don't come here) and we try to pass by value
                warning(rhs->d_loc,Validator::tr("passing record by value to base type loses fields"));
            }
            return res;
        }
    }

    bool arrayCompatible( Type* lhsT, Type* rhsT, const RowCol& loc ) const
    {
        if( lhsT == 0 || rhsT == 0 )
            return false;

        // T~f~ and T~a~ are the _equal type_
        if( equalType( lhsT, rhsT ) )
            return true;

        lhsT = derefed(lhsT);
        rhsT = derefed(rhsT);

#ifdef _OBX_USE_NEW_FFI_
        if( lhsT && rhsT && lhsT->d_unsafe != rhsT->d_unsafe && !lhsT->isText() && !rhsT->isText() )
            return false;
#endif

        const int ltag = lhsT->getTag();
        Array* la = ltag == Thing::T_Array ? cast<Array*>(lhsT) : 0 ;
        const int rtag = rhsT->getTag();
        Array* ra = rtag == Thing::T_Array ? cast<Array*>(rhsT) : 0 ;

        if( la == 0 || ( !la->d_lenExpr.isNull() && !la->d_vla) )
            return false; // Tf is not an open array

        // T~f~ is an open array, T~a~ is any array, and their element types are array compatible
        if( ra && arrayCompatible( la->d_type.data(), ra->d_type.data(), loc ) )
            return true;

        Type* laT = la ? derefed(la->d_type.data()) : 0 ;
        Type* raT = ra ? derefed(ra->d_type.data()) : 0 ;

        // T~f~ is an open array of CHAR and T~a~ is a Latin-1 string
        if( la && laT == bt.d_charType && ( rhsT == bt.d_stringType || rhsT == bt.d_charType ) )
            return true;
        // T~f~ is an open array of WCHAR and T~a~ is a Unicode BMP or Latin-1 string
        if( la && laT == bt.d_wcharType &&  ( rhsT->isString() || rhsT->isChar() ) )
            return true;

        // Oberon 90: If a formal parameter is of type ARRAY OF BYTE, then the corresponding
        //   actual parameter may be of any type.
        // Oberon-2: If a formal **variable** parameter is of type ARRAY OF BYTE then the corresponding
        //   actual parameter may be of any type.
        if( laT == bt.d_byteType && raT != bt.d_byteType )
        {
            warning( loc, Validator::tr("Oberon VAR ARRAY OF BYTE trick not officially supported") );
            return true;
        }


        return false;
    }

    bool matchingFormalParamLists( ProcType* lhs, ProcType* rhs, bool allowRhsCovariance = false ) const
    {
        if( lhs == 0 || rhs == 0 )
            return false;
        if( lhs->d_formals.size() != rhs->d_formals.size() || lhs->d_varargs != rhs->d_varargs )
            return false;
        if( !lhs->d_return.isNull() && !rhs->d_return.isNull() )
        {
            if( !allowRhsCovariance && !sameType( lhs->d_return.data(), rhs->d_return.data() ) )
                return false;
            if( allowRhsCovariance && !sameType( lhs->d_return.data(), rhs->d_return.data() ) )
            {
                Q_ASSERT( !lhs->d_return.isNull() && !rhs->d_return.isNull() );
                Type* super = derefed(lhs->d_return.data());
                Type* sub = derefed(rhs->d_return.data());
                if( super && super->getTag() == Thing::T_Pointer && sub && sub->getTag() == Thing::T_Pointer )
                {
                    if( !typeExtension(super, sub) )
                        return false;
                }else
                    return false;
            }
        }else if( ( lhs->d_return.isNull() && !rhs->d_return.isNull() ) ||
                          ( !lhs->d_return.isNull() && rhs->d_return.isNull() ) )
                              return false;

        for( int i = 0; i < lhs->d_formals.size(); i++ )
        {
            if( ( lhs->d_formals[i]->d_var != rhs->d_formals[i]->d_var ) ||
                ( lhs->d_formals[i]->d_const != rhs->d_formals[i]->d_const ) )
                return false;
            if( !equalType( lhs->d_formals[i]->d_type.data(), rhs->d_formals[i]->d_type.data() ) )
                return false;
        }

        return true;
    }

    bool checkValidLhs( Expression* lhs )
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
                error( lhs->d_loc, Validator::tr("cannot assign to '%1'").arg(id->d_name.constData()));
                return false;
            }
        }
        const quint8 v = lhs->visibilityFor(mod); // also handles IN param, const, buitin etc.
        switch( v )
        {
        case Named::ReadOnly:
            error( lhs->d_loc, Validator::tr("cannot modify read-only designator"));
            return false;
        case Named::Private:
            error( lhs->d_loc, Validator::tr("cannot modify private designator"));
            return false;
        }
        return true;
    }

#if 0
    void checkUnsafePointer( Type* t, bool isReturn, const RowCol& r )
    {
        // TEST code
        t = derefed(t);
        if( t && t->getTag() == Thing::T_Pointer )
        {
            Pointer* p = cast<Pointer*>(t);
            t = derefed(p->d_to.data());
            if( p->d_unsafe && t && ( t->getBaseType() >= Type::BOOLEAN  ||
                                      t->getTag() == Thing::T_Pointer
                                      || t->getTag() == Thing::T_Enumeration || t->getTag() == Thing::T_ProcType ) )
                qDebug() << (isReturn ? "return" : "param" ) << "pointer to"
                         << t->getTagName() << "in" << mod->d_name << r.d_row << r.d_col;
        }
    }
#endif

    void markSelfRef( Type* t )
    {
        Type* ctdd = derefed(curTypeDecl);
        Type* td = derefed(t);
        if( td == ctdd && ctdd->getTag() != Thing::T_Pointer )
            error(t->d_loc, Validator::tr("a structured type cannot contain itself"));
        else if( td == ctdd )
            t->d_selfRef = true;
        else if( td == t ) // dont follow qualitypes
            checkSelfRef(td);
    }

    void checkSelfRef( Type* t )
    {
        Type* td = derefed(t);
        if( td == 0 )
            return; // error already reported
        if( td != t )
            return; // don't check type aliasses, only original types

#if 0 // we do this now in markSelfRef
        Type* ctdd = derefed(curTypeDecl);
        if( ctdd && ctdd->getTag() == Thing::T_Pointer )
            return; // legal in any case
            // and a pointer cannot point to a pointer anyway
#endif

        switch( td->getTag() )
        {
        case Thing::T_Record:
            {
                Record* r = cast<Record*>(td);
                foreach( const Ref<Field>& f, r->d_fields )
                    markSelfRef(f->d_type.data());
            }
            break;
        case Thing::T_Array:
            {
                Array* a = cast<Array*>(td);
                markSelfRef(a->d_type.data());
            }
            break;
        case Thing::T_Pointer:
            {
                Pointer* p = cast<Pointer*>(td);
                Type* d = derefed(p->d_to.data());
                if( d == p->d_to.data() )
                    checkSelfRef(d);
            }
            break;
        }
    }
};

bool Validator::check(Module* m, const BaseTypes& bt, Ob::Errors* err, Instantiator* insts)
{
    Q_ASSERT( m != 0 && err != 0 );

    if( m->d_hasErrors )
        return false;

    const quint32 errCount = err->getErrCount();

    ValidatorImp imp;
    imp.err = err;
    imp.bt = bt;
    imp.bt.check();
    imp.mod = m;
    imp.insts = insts;
    m->accept(&imp);

    m->d_isValidated = true;

    m->d_hasErrors = ( err->getErrCount() - errCount ) != 0;

    return !m->d_hasErrors;
}

bool Validator::includesType(quint8 lhs, quint8 rhs)
{
    if( lhs == rhs )
        return true;

    /*
        LONGINT >= INTEGER >= SHORTINT >= BYTE
        LONGREAL >= REAL >= SHORTINT >= BYTE
        LONGREAL >= INTEGER >= SHORTINT >= BYTE
        WCHAR >= CHAR

        non-inclusive pairs: REAL/INTEGER, REAL/LONGINT, LONGREAL/LONGINT
    */

    switch( lhs )
    {
    case Type::LONGINT:
        return rhs == Type::BYTE || rhs == Type::INTEGER || rhs == Type::SHORTINT;
    case Type::INTEGER:
        return rhs == Type::BYTE || rhs == Type::SHORTINT;
    case Type::SHORTINT:
        return rhs == Type::BYTE;
    case Type::BYTE:
        break;
    case Type::LONGREAL:
        return rhs == Type::BYTE || rhs == Type::INTEGER || rhs == Type::SHORTINT ||
                rhs == Type::REAL;
    case Type::REAL:
        return rhs == Type::BYTE || rhs == Type::SHORTINT;
    case Type::WCHAR:
        return rhs == Type::CHAR;
    case Type::WSTRING:
        return rhs == Type::STRING || rhs == Type::CHAR;
    }
    return false;
}

QPair<quint8, bool> Validator::inclusiveType(quint8 lhs, quint8 rhs)
{
    if( includesType(lhs,rhs) )
        return qMakePair(lhs,true);
    else if( includesType(rhs,lhs) )
        return qMakePair(rhs,true);

    // and now for the non-inclusive pairs
    if( ( lhs == Type::REAL && rhs == Type::INTEGER ) ||
        ( lhs == Type::INTEGER && rhs == Type::REAL ) )
        return qMakePair(Type::REAL,false);
    if( ( lhs == Type::REAL && rhs == Type::LONGINT ) ||
        ( lhs == Type::LONGINT && rhs == Type::REAL ) )
        return qMakePair(Type::LONGREAL,false);
    if( ( lhs == Type::LONGREAL && rhs == Type::LONGINT ) ||
        ( lhs == Type::LONGINT && rhs == Type::LONGREAL ) )
        return qMakePair(Type::LONGREAL,false);
    Q_ASSERT(false);
    return qMakePair(0,false);
}

Validator::BaseTypes::BaseTypes()
{
    ::memset(this,0,sizeof(BaseTypes));
}

void Validator::BaseTypes::check() const
{
    Q_ASSERT( d_boolType && d_charType && d_byteType && d_intType && d_realType && d_setType &&
              d_stringType && d_nilType && d_anyType && d_shortType && d_longType && d_longrealType &&
              d_anyRec && d_wcharType && d_wstringType && d_voidType && d_byteArrayType && d_noType );
}
