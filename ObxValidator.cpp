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
    bool returnValueFound;

    ValidatorImp():err(0),mod(0),curTypeDecl(0),prevStat(0),returnValueFound(false) {}

    //////// Scopes

    void visitScope( Scope* me )
    {
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Const )
                n->accept(this);
        }
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_NamedType )
                n->accept(this);
        }
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

        levels.push_back(me);
        // imports are supposed to be already resolved at this place
        visitScope(me);
        visitStats( me->d_body );
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
        levels.push_back(me);

        ProcType* pt = me->getProcType();
        pt->accept(this);

        if( me->d_visibility == Named::ReadOnly )
            warning( me->d_loc, Validator::tr("export mark '-' not supported for procedures; using '*' instead") );
        else if( me->d_visibility == Named::ReadWrite )
            me->d_visibility = Named::ReadOnly; // make public procs readonly; they cannot be assigned to!

        if( !me->d_receiver.isNull() )
        {
            // receiver was already accepted in visitScope
            visitBoundProc(me);
        }

        levels.pop_back();
    }

    void visitBody( Procedure* me )
    {
        levels.push_back(me);
        visitScope(me); // also handles formal parameters
        returnValueFound = false;
        visitStats( me->d_body );
        if( !mod->d_isDef && !( me->d_bodyKind == Scope::EmptyBody && me->d_order.size() == me->d_parCount ) )
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
                n->d_scope->d_upvalSource = true;
                int i = levels.size() - 2;
                while( i > 0 && levels[i].scope != n->d_scope )
                    levels[i--].scope->d_upvalIntermediate = true;
                levels.back().scope->d_upvalSink = true;
                n->d_upvalSource = true;
                error( e->d_loc, Validator::tr("cannot access local variables and parameters of outer procedures") );
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
            if( me->d_ident->getTag() == Thing::T_Import )
            {
                Import* imp = cast<Import*>(me->d_ident.data());
                if( !imp->d_metaActuals.isEmpty() && !imp->d_visited )
                    imp->accept(this);
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
            if( modVar->getTag() == Thing::T_Procedure )
            {
                Procedure* p = cast<Procedure*>(modVar);
                if( !p->d_receiver.isNull() )
                {
                    error( me->d_loc,Validator::tr("cannot reference bound procedure '%1' from another module")
                          .arg(me->d_name.constData()) );
                }
            }
            me->d_ident = modVar;
            me->d_type = modVar->d_type.data();
        }else
        {
            // prev must be a pointer or a record
            Type* prevT = derefed( me->d_sub->d_type.data() );
            if( prevT == 0 )
                return;

            if( prevT->getTag() == Thing::T_Pointer )
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

            if( prevT->getTag() == Thing::T_Record )
            {
                Record* r = cast<Record*>(prevT);
                Named* field = r->find(me->d_name, true);
                if( field == 0 )
                {
                    field = r->find(me->d_name, true); // TEST
                    error( me->d_loc, Validator::tr("record has no field or bound procedure named '%1'").arg(me->d_name.constData()) );
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

    bool checkBuiltInArgs( ProcType* p, ArgExpr* args )
    {
        Q_ASSERT( p->d_decl && p->d_decl->getTag() == Thing::T_BuiltIn );
        BuiltIn* bi = cast<BuiltIn*>(p->d_decl);

        switch( bi->d_func )
        {
        case BuiltIn::SYS_BIT:
        case BuiltIn::SYS_H:
        case BuiltIn::SYS_LDREG:
        case BuiltIn::SYS_REG:
        case BuiltIn::SYS_COPY:
        case BuiltIn::ODD:
        case BuiltIn::LSL:
        case BuiltIn::ASR:
        case BuiltIn::FLOOR:
        case BuiltIn::FLT:
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
        case BuiltIn::ASH:
        case BuiltIn::ROR:
        case BuiltIn::ENTIER:
        case BuiltIn::BITAND:
        case BuiltIn::BITNOT:
        case BuiltIn::BITOR:
        case BuiltIn::BITXOR:
            return false; // these can be handled by ordinary arg checker


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
                checkCallArg( &lhs, args->d_args[0] );
                if( args->d_args.size() == 2 )
                {
                    Parameter rhs;
                    rhs.d_type = derefed(args->d_args[1]->d_type.data());
                    if( lhs.d_type && lhs.d_type->getTag() == Thing::T_Enumeration )
                        error( args->d_args[1]->d_loc, Validator::tr("cannot use second argument for enumeration types"));
                    else if( !isInteger( rhs.d_type.data() ) )
                        error( args->d_args[1]->d_loc, Validator::tr("expecting integer argument"));
                    checkCallArg( &rhs, args->d_args[1] );
                }
            }else
                error( args->d_loc, Validator::tr("expecting one or two arguments"));
            break;
        case BuiltIn::ASSERT: // optional arg
            if( args->d_args.size() == 1 || args->d_args.size() == 2 )
            {
                Parameter lhs;
                lhs.d_type = bt.d_boolType;
                checkCallArg( &lhs, args->d_args[0] );
                if( args->d_args.size() == 2 )
                {
                    Parameter rhs;
                    rhs.d_type = derefed(args->d_args[1]->d_type.data());
                    if( !isInteger( rhs.d_type.data() ) )
                        error( args->d_args[1]->d_loc, Validator::tr("expecting integer argument"));
                    checkCallArg( &rhs, args->d_args[1] );
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
                if( d.d_type.isNull() )
                    break; // already reported
                e.d_type = bt.d_boolType;
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
                checkCallArg( &lhs, args->d_args[0] );
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
                if( ltag != Thing::T_Array && lhs != bt.d_stringType && lhs != bt.d_wstringType )
                    error( args->d_args.first()->d_loc, Validator::tr("expecting array or string argument"));
                else if( args->d_args.size() == 2 )
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
                Type* lhs = derefed(args->d_args.first()->d_type.data());
                // TODO: check for quali to type
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
                Type* to = derefed(cast<Pointer*>(lhs)->d_to.data());
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
                    assignmentCompatible( args->d_args.last()->d_type.data(), args->d_args.first().data() ) )
                    error( args->d_loc, Validator::tr("incompatible arguments"));
            }
            break;
        case BuiltIn::VAL:
            if( args->d_args.size() == 2 )
            {
                Type* lhs = derefed(args->d_args.first()->d_type.data());
                Type* rhs = derefed(args->d_args.last()->d_type.data());
                if( lhs->getTag() != Thing::T_Enumeration && !isInteger(rhs) )
                    error( args->d_loc, Validator::tr("incompatible arguments"));
            }else
                error( args->d_loc, Validator::tr("expecting one or two arguments"));
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
            }else
                error( args->d_loc, Validator::tr("expecting one argument"));
            break;
        case BuiltIn::PRINTLN:
            if( args->d_args.size() == 1 )
            {
                // NOP
            }else
                error( args->d_loc, Validator::tr("expecting one argument"));
            break;
        case BuiltIn::SYS_ADR:
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
            break; // TODO ignored for now
        }
        return true;
    }

    void checkCallArg( Parameter* formal, Ref<Expression>& actual )
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
            Ref<UnExpr> arg = new UnExpr(UnExpr::DEREF, actual.data() );
            arg->d_loc = actual->d_loc;
            Pointer* p = cast<Pointer*>(ta);
            ta = derefed(p->d_to.data());
            arg->d_type = p->d_to.data();
            actual = arg.data();
        }

        // BBOX supports passing RECORD and ARRAY to variant or value UNSAFE POINTER parameters, implicit address of operation
        if( tftag == Thing::T_Pointer && ( tatag == Thing::T_Record || tatag == Thing::T_Array
                                           || ta == bt.d_stringType || ta == bt.d_wstringType  ) )
        {
            Pointer* p = cast<Pointer*>(tf);
            if( p->d_unsafe )
            {
                Ref<UnExpr> ue = new UnExpr();
                ue->d_loc = actual->d_loc;
                ue->d_op = UnExpr::ADDROF;
                ue->d_sub = actual;
                Ref<Pointer> ptr = new Pointer();
                ptr->d_loc = actual->d_loc;
                ptr->d_unsafe = true;
                ptr->d_to = actual->d_type.data();
                ta = ptr.data();
                ue->d_type = ptr.data();
                actual = ue.data();
                mod->d_helper2.append(ptr.data()); // otherwise ptr gets deleted when leaving this scope
            }
        }
#endif

        if( formal->d_var && !formal->d_const )
        {
            // check if VAR really gets a physical location
            bool ok = false;
            switch( actual->getTag() )
            {
            case Thing::T_UnExpr:
                ok = actual->getUnOp() == UnExpr::DEREF;
                break;
            case Thing::T_IdentLeaf:
                cast<IdentLeaf*>(actual.data())->d_role = VarRole;
                ok = true;
                break;
            case Thing::T_IdentSel:
                cast<IdentSel*>(actual.data())->d_role = VarRole;
                ok = true;
                break;
            case Thing::T_ArgExpr:
                if( actual->getUnOp() == UnExpr::CALL )
                {
                    ArgExpr* ae = cast<ArgExpr*>(actual.data());
                    if( ae->d_sub && ae->d_sub->getIdent() )
                    {
                        if( ae->d_sub->getIdent()->getTag() == Thing::T_BuiltIn )
                        {
                            // VAL does not actually return something but is just a cast
                            BuiltIn* bi = cast<BuiltIn*>(ae->d_sub->getIdent());
                            ok = bi->d_func == BuiltIn::SYS_VAL;
                        }
                    }
                }else
                    ok = true; // TODO: is readoly checked for pointers derefs or array elements?
                break;
            default:
                break;
            }
#ifdef OBX_BBOX
            if( ta == bt.d_nilType )
                ok = true;
#endif
            if( !ok )
                error( actual->d_loc, Validator::tr("cannot pass this expression to a VAR parameter") );
            else
                checkValidLhs(actual.data()); // TODO: does this replace all the above VAR rules?
        }

        const QString var = formal->d_var ? formal->d_const ? "IN " : "VAR " : "";

        if( af && af->d_lenExpr.isNull() )
        {
            // If Tf is an open array, then a must be array compatible with f
            if( !arrayCompatible( af, actual->d_type.data() ) )
                error( actual->d_loc,
                       Validator::tr("actual parameter type %1 not compatible with formal type of %2%3 of '%4'")
                       .arg(actual->d_type->pretty()).arg(var).arg(formal->d_type->pretty())
                       .arg(formal->d_name.constData()));
        }
        else
        {
            if( tatag == Thing::T_ProcType )
            {
                Named* n = actual->getIdent();
                const int tag = n ? n->getTag() : 0;
                if( tag == Thing::T_Procedure )
                {
                    Procedure* p = cast<Procedure*>(n);
                    if( p->d_receiverRec )
                        error( actual->d_loc, Validator::tr("a type-bound procedure cannot be passed to a procedure type parameter"));
                    if( p->d_upvalIntermediate || p->d_upvalSink )
                        error( actual->d_loc, Validator::tr("this procedure cannot be passed to a procedure type parameter"));
                }else if( tag == Thing::T_BuiltIn )
                    error( actual->d_loc, Validator::tr("a predeclared procedure cannot be passed to a procedure type parameter"));
            }

            // Otherwise Ta must be parameter compatible to f
            if( !paramCompatible( formal, actual.data() ) )
                error( actual->d_loc,
                   Validator::tr("actual parameter type %1 not compatible with formal type %2%3 of '%4'")
                   .arg(actual->d_type->pretty()).arg(var).arg(formal->d_type->pretty())
                   .arg(formal->d_name.constData()));
        }
    }

    void checkCallArgs( ProcType* p, ArgExpr* me )
    {
        if( p->d_formals.size() != me->d_args.size() )
        {
            error( me->d_loc, Validator::tr("number of actual and formal parameters doesn't match"));
            return;
        }

        for( int i = 0; i < p->d_formals.size(); i++ )
        {
            checkCallArg( p->d_formals[i].data(), me->d_args[i] );
        }
    }

    Type* calcBuiltInReturnType( BuiltIn* bi, const ExpList& args )
    {
        switch( bi->d_func )
        {
        case BuiltIn::SYS_VAL:
        case BuiltIn::SYS_ROT:
        case BuiltIn::SYS_LSH:
        case BuiltIn::VAL:
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
                return inclusiveType1(derefed(args.first()->d_type.data()),derefed(args.last()->d_type.data()));
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
            Q_ASSERT( bi->d_type && bi->d_type->getTag() == Thing::T_ProcType );
            return cast<ProcType*>(bi->d_type.data())->d_return.data();
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
                    me->d_type = calcBuiltInReturnType( cast<BuiltIn*>(p->d_decl), me->d_args );
                }else
                    me->d_type = p->d_return.data();
            }else if( decl && decl->getTag() == Thing::T_NamedType )
            {
                // this is a type guard
                me->d_op = UnExpr::CAST;
                me->d_type = decl->d_type.data();
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
                error( me->d_loc, Validator::tr("index selector only available for arrays") );
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
                me->d_type = prevT;
            else
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

        switch( me->d_op )
        {
        case BinExpr::Range: // int
            if( isInteger(lhsT) && isInteger(rhsT) )
                me->d_type = rhsT;
            else if( isCharConst(me->d_lhs.data()) && isCharConst(me->d_rhs.data()) )
                me->d_type = bt.d_charType;
            else
                error( me->d_loc, Validator::tr("range operator expects operands to be either integers or characters") );
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
                    ( ( lhsT == bt.d_nilType || ltag == Thing::T_ProcType ) &&
                      ( rtag == Thing::T_ProcType || rhsT == bt.d_nilType ) ) ||
                    ( lhsT == bt.d_anyType && rhsT == bt.d_anyType ) ) // because of generics
                me->d_type = bt.d_boolType;
            else
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
                me->d_type = bt.d_boolType;
            else
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
            if( typeExtension( lhsT, rhsT ) )
                me->d_type = bt.d_boolType;
            else
                error( me->d_loc, Validator::tr("operator 'IS' expects operands of record type") );
            break;

        case BinExpr::ADD: // set num
        case BinExpr::SUB: // set num
        case BinExpr::MUL:  // set num
            if( isNumeric(lhsT) && isNumeric(rhsT) )
                me->d_type = inclusiveType1(lhsT,rhsT);
            else if( lhsT == bt.d_setType && rhsT == bt.d_setType )
                me->d_type = bt.d_setType;
#ifdef OBX_BBOX
            else if( me->d_op == BinExpr::ADD && (lhsT = isTextual(lhsT,true)) && (rhsT = isTextual(rhsT,true)) )
            {
                // TODO: add deref op in case of pointer to array
                if( includes(lhsT,rhsT) ) // allow concat of mixed latin/unicode strings
                    me->d_type = me->d_lhs->d_type;
                else
                    me->d_type = me->d_rhs->d_type;
            }
#endif
            else
                error( me->d_loc, Validator::tr("operator '%1' expects both operands to "
                                                "be either of numeric or SET type").arg( BinExpr::s_opName[me->d_op]) );
            break;

        case BinExpr::FDIV: // set num
            if( isNumeric(lhsT) && isNumeric(rhsT) )
                me->d_type = inclusiveType2(lhsT,rhsT);
            else if( lhsT == bt.d_setType || rhsT == bt.d_setType )
                me->d_type = bt.d_setType;
            else
                error( me->d_loc, Validator::tr("operator '/' expects both operands to be either of numeric or SET type") );
            break;

        case BinExpr::DIV:  // int
        case BinExpr::MOD:  // int
            if( !isInteger(lhsT ) )
                error( me->d_lhs->d_loc, Validator::tr("integer type expected for left side of MOD or DIV operator") );
            if( !isInteger(rhsT ) )
                error( me->d_rhs->d_loc, Validator::tr("integer type expected for right side of MOD or DIV operator") );
            me->d_type = inclusiveType1(lhsT,rhsT);
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
        const double d = me->d_val.toDouble();
        switch( me->d_vtype )
        {
        case Literal::Enum:
            break; // keep the Enumeration type
        case Literal::Integer:
            if( i >= 0 && i <= bt.d_byteType->maxVal().toInt() )
                me->d_type = bt.d_byteType;
            else if( i >= bt.d_shortType->minVal().toInt() && i <= bt.d_shortType->maxVal().toInt() )
                me->d_type = bt.d_shortType;
            else if( i >= bt.d_intType->minVal().toInt() && i <= bt.d_intType->maxVal().toInt() )
                me->d_type = bt.d_intType;
            else
                me->d_type = bt.d_longType;
            break;
        case Literal::Real:
            me->d_type = bt.d_realType; // TODO: adjust precision
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
            me->d_type = bt.d_stringType;
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
    }

    ///////// Types

    void visit( Pointer* me )
    {
        if( me->d_visited )
            return;
        me->d_visited = true;

        if( !me->d_flag.isNull() )
        {
            me->d_flag->accept(this);
            // only one in Kernel line 268
            // qDebug() << "flagged pointer" << me->d_flag->getIdent()->d_name << "in module" << mod->d_name << me->d_loc.d_row;
        }

        if( !me->d_to.isNull() )
        {
            me->d_to->accept(this);
            switch( derefed(me->d_to.data())->getTag() )
            {
            case Thing::T_Record:
            case Thing::T_Array:
                // NOP
                break;
            default:
                if( !me->d_unsafe )
                    error( me->d_loc, Validator::tr("pointer must point to a RECORD or an ARRAY") );
                break;
            }
        }
    }

    void visit( Array* me )
    {
        if( me->d_visited )
            return;
        me->d_visited = true;

        if( !me->d_flag.isNull() )
            me->d_flag->accept(this);

        if( !me->d_lenExpr.isNull() )
        {
            me->d_lenExpr->accept(this);
            if( !isInteger(me->d_lenExpr->d_type.data() ) )
                error( me->d_lenExpr->d_loc, Validator::tr("expression doesn't evaluate to an integer") );
            else
            {
                bool ok;
                Evaluator::Result res = Evaluator::eval(me->d_lenExpr.data(), mod,err);
                const int len = res.d_value.toInt(&ok);
                if( res.d_vtype != Literal::Integer || !ok || len <= 0 )
                    error( me->d_lenExpr->d_loc, Validator::tr("expecting positive non-zero integer for array length") );
                me->d_len = len;
            }
        }
        if( me->d_type )
            me->d_type->accept(this);
        checkNoAnyRecType(me->d_type.data());
        checkSelfRef(me);

#ifdef OBX_BBOX
        if( me->d_unsafe )
        {
            Type* t = derefed(me->d_type.data());
            const int tag = t ? t->getTag() : 0;
            switch( tag )
            {
            case Thing::T_Pointer:
                {
                    Pointer* p = cast<Pointer*>(t);
                    if( !p->d_unsafe )
                        error( me->d_loc, Validator::tr("carray cannot have safe pointers as element types") );
                }
                break;
            case Thing::T_Record:
                {
                    Record* r = cast<Record*>(t);
                    if( !r->d_unsafe )
                        error( me->d_loc, Validator::tr("carray cannot have records as element types") );
                        // this was already the case in BBOX 1.7.2
                }
                break;
            case Thing::T_Array:
                {
                    Array* a = cast<Array*>(t);
                    if( !a->d_unsafe )
                        error( me->d_loc, Validator::tr("carray cannot have arrays as element types") );
                        // this was already the case in BBOX 1.7.2
                }
                break;
            }
        }
#endif
    }

    void visit( Enumeration* me )
    {
        if( me->d_visited )
            return;
        me->d_visited = true;

        foreach( const Ref<Const>& c, me->d_items )
            c->accept(this);
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

        if( !me->d_flag.isNull() )
            me->d_flag->accept(this);

#if 0
        if( !me->d_base.isNull() && me->d_unsafe )
            error( me->d_base->d_loc, Validator::tr("A cstruct cannot have a base type") );
            // not true; many cstruct in BBOX inherit from COM.IUnknown etc.
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
            if( me->d_baseRec && me->d_baseRec->d_unsafe != me->d_unsafe )
                error( me->d_base->d_loc, Validator::tr("cstruct cannot inherit from record and vice versa") );
                // in BBOX regular records inherit from COM cstructs
#endif
            if( me->d_baseRec && me->d_unsafe && !me->d_baseRec->d_unsafe  )
                error( me->d_base->d_loc, Validator::tr("cstruct cannot inherit from record") ); // at least this is true
        }
        foreach( const Ref<Field>& f, me->d_fields )
        {
            f->accept(this);

            checkSelfRef(f->d_type.data());
#ifdef OBX_BBOX
            if( me->d_unsafe )
            {
                Type* t = derefed(f->d_type.data());
                const int tag = t ? t->getTag() : 0;
                switch( tag )
                {
                case Thing::T_Pointer:
                    {
                        Pointer* p = cast<Pointer*>(t);
                        if( !p->d_unsafe )
                            error( me->d_loc, Validator::tr("cstruct cannot have safe pointers as field types") );
                    }
                    break;
                case Thing::T_Record:
                    {
                        Record* r = cast<Record*>(t);
                        if( !r->d_unsafe )
                            error( me->d_loc, Validator::tr("cstruct cannot have records as field types") );
                            // this was already the case in BBOX 1.7.2
                    }
                    break;
                case Thing::T_Array:
                    {
                        Array* a = cast<Array*>(t);
                        if( !a->d_unsafe )
                            error( me->d_loc, Validator::tr("cstruct cannot have arrays as field types") );
                            // this needed a dozen of fixes in BBOX 1.7.2
                    }
                    break;
                }
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

    void visit( ProcType* me )
    {
        if( me->d_visited )
            return;
        me->d_visited = true;

        if( !me->d_return.isNull() )
            me->d_return->accept(this); // OBX has no restrictions on return types
        checkNoAnyRecType(me->d_return.data());
        foreach( const Ref<Parameter>& p, me->d_formals )
            p->accept(this);
    }

    //////// Others

    void visit( NamedType* me )
    {
        curTypeDecl = me->d_type.data();
        if( me->d_type )
            me->d_type->accept(this);
        checkNoAnyRecType(me->d_type.data());
        curTypeDecl = 0;
    }

    void visit( Const* me )
    {
        if( me->d_constExpr.isNull() )
            return;
        me->d_constExpr->accept(this);
        me->d_type = me->d_constExpr->d_type.data();
        Evaluator::Result res = Evaluator::eval(me->d_constExpr.data(), mod, err);
        me->d_val = res.d_value;
        me->d_vtype = res.d_vtype;
        me->d_wide = res.d_wide;
        me->d_strLen = res.d_strLen;
    }

    void visit( Field* me )
    {
        checkVarType(me);
    }

    void visit( Variable* me )
    {
        checkVarType(me);
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
#if 0
        // not true; open array value parameter are supported as well, in all old Oberon/-2, Oberon-07 and BBOX
        Type* t = derefed(me->d_type.data());
        if( t && t->getTag() == Thing::T_Array )
        {
            Array* a = cast<Array*>( t );
            if( a->d_lenExpr.isNull() && !me->d_var )
                error( me->d_loc, Validator::tr("only variable parameters allowed with open array type") );
        }
#endif
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
            if( !assignmentCompatible(pt->d_return.data(), me->d_what.data() ) )
                error( me->d_loc, Validator::tr("return expression is not assignment compatible with function return type"));
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
        me->d_rhs->accept(this);            // TODO: do we check readability of rhs idents?
        markIdent( false, me->d_rhs.data() );
        if( !checkValidLhs(me->d_lhs.data()) )
            return;
        Type* lhsT = derefed(me->d_lhs->d_type.data());
        Type* rhsT = derefed(me->d_rhs->d_type.data());

#ifdef OBX_BBOX
        const int lhsTag = lhsT ? lhsT->getTag() : 0;
        const int rhsTag = rhsT ? rhsT->getTag() : 0;
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

        if( lhsTag == Thing::T_Pointer && ( rhsTag == Thing::T_Record || rhsTag == Thing::T_Array
                                            || rhsT == bt.d_stringType || rhsT == bt.d_wstringType ) )
        {
            // in BBOX the assignment of a structured value to an unsafe pointer is an "address of" operation
            Pointer* p = cast<Pointer*>(lhsT);
            if( p->d_unsafe )
            {
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
            }
        }
#endif
        Array* lstr = toCharArray(lhsT,false);
        if( lstr && me->d_rhs->getTag() == Thing::T_Literal )
        {
            // TODO: check wchar vs char compat
            Literal* lit = cast<Literal*>( me->d_rhs.data() );
            if( lstr->d_len && lit->d_vtype == Literal::String && lit->d_strLen > lstr->d_len )
                error( me->d_rhs->d_loc, Validator::tr("string is too long to assign to given character array"));
            if( lstr->d_len && lit->d_vtype == Literal::Char && 1 > lstr->d_len )
                error( me->d_rhs->d_loc, Validator::tr("the character array is too small for the character"));
            // TODO: runtime checks for var length arrays

        }

        if( rhsTag == Thing::T_ProcType )
        {
            Named* n = me->d_rhs->getIdent();
            const int tag = n ? n->getTag() : 0;
            if( tag == Thing::T_Procedure )
            {
                Procedure* p = cast<Procedure*>(n);
                if( p->d_receiverRec )
                    error( me->d_rhs->d_loc, Validator::tr("a type-bound procedure cannot be assigned to a procedure variable"));
                if( p->d_upvalIntermediate || p->d_upvalSink )
                    error( me->d_rhs->d_loc, Validator::tr("this procedure cannot be assigned to a procedure variable"));
            }else if( tag == Thing::T_BuiltIn )
                error( me->d_rhs->d_loc, Validator::tr("a predeclared procedure cannot be assigned to a procedure variable"));
        }

        // lhs and rhs might have no type which might be an already reported error or the attempt to assign no value
        if( !assignmentCompatible( me->d_lhs->d_type.data(), me->d_rhs.data() ) )
        {
            const QString lhs = !me->d_lhs->d_type.isNull() ? me->d_lhs->d_type->pretty() : QString("");
            const QString rhs = !me->d_rhs->d_type.isNull() ? me->d_rhs->d_type->pretty() : QString("");

            error( me->d_rhs->d_loc, Validator::tr("right side %1 of assignment is not compatible with left side %2")
                   .arg(rhs).arg(lhs) );
            //assignmentCompatible( me->d_lhs->d_type.data(), me->d_rhs.data() ); // TEST
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
            if( !ae->d_type.isNull() )
            {
                error( me->d_loc, Validator::tr("cannot use a function procedure in a call statement") ); // TODO: why not?
                return;
            }
            proc = ae->d_sub.data();
            Type* t = derefed(proc->d_type.data());
            if( ae->d_op != UnExpr::CALL || t == 0 || t->getTag() != Thing::T_ProcType )
            {
                error( me->d_loc, Validator::tr("cannot call this expression") );
                return;
            }
            ProcType* pt = cast<ProcType*>(t);
            const bool isBuiltIn = pt->d_decl && pt->d_decl->getTag() == Thing::T_BuiltIn;
            if( !isBuiltIn || !checkBuiltInArgs( pt, ae ) )
                checkCallArgs( pt, ae );
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
                Evaluator::Result res = Evaluator::eval(me->d_by.data(), mod, err);
                if( res.d_vtype != Literal::Integer )
                    error( me->d_by->d_loc, Validator::tr("expecting an integer as the step value of the for loop"));
                me->d_byVal = res.d_value;
            }
        }else if( enumType )
        {
            Evaluator::Result from = Evaluator::eval(me->d_from.data(), mod, err);
            Evaluator::Result to = Evaluator::eval(me->d_to.data(), mod, err);
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

    void visit( CaseStmt* me )
    {
        if( me->d_exp.isNull() )
            return;

        me->d_exp->accept(this);

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

        foreach( const CaseStmt::Case& c, me->d_cases )
        {
            foreach( const Ref<Expression>& e, c.d_labels )
            {
                if( !e.isNull() )
                        e->accept(this);
            }

            if( me->d_typeCase )
            {
                if( c.d_labels.size() != 1 || c.d_labels.first()->getIdent() == 0 )
                {
                    Q_ASSERT(!c.d_labels.isEmpty());
                    error( c.d_labels.first()->d_loc, Validator::tr("expecting a qualident case label in a type case statement"));
                    continue;
                }else if( !typeExtension( derefed(orig.data()), derefed(c.d_labels.first()->getIdent()->d_type.data()) ) )
                {
                    Q_ASSERT(!c.d_labels.isEmpty());
                    error( c.d_labels.first()->d_loc, Validator::tr("case label must be a subtype of the case variable in a type case statement"));
                    continue;
                }else
                    caseId->d_type = c.d_labels.first()->getIdent()->d_type;
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
                if( false )// !td->hasByteSize() )
                {
                    // not necessary here because such errors are discovered when the instance is validated in context
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
            if( !me->d_type->hasByteSize() )
                error(me->d_type->d_loc, Validator::tr("this type cannot be used here") );
            checkNoAnyRecType(me->d_type.data());
        }
    }

    inline void checkNoAnyRecType( Type* t )
    {
        Type* td = derefed(t);
        if( td == bt.d_anyRec )
            error(t->d_loc, Validator::tr("this type cannot be used here") );
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
        if( lhs == rhs )
            return true;
        if( lhs == bt.d_longType )
            return rhs == bt.d_byteType || rhs == bt.d_intType || rhs == bt.d_shortType;
        if( lhs == bt.d_intType )
            return rhs == bt.d_byteType || rhs == bt.d_shortType;
        if( lhs == bt.d_shortType )
            return rhs == bt.d_byteType;
        if( lhs == bt.d_byteType )
            return false;
        if( lhs == bt.d_realType )
            return rhs == bt.d_byteType || rhs == bt.d_shortType ||
                     rhs == bt.d_intType; // RISK: possible loss of precision
        if( lhs == bt.d_longrealType )
            return rhs == bt.d_byteType || rhs == bt.d_intType || rhs == bt.d_shortType || rhs == bt.d_longType ||
                    rhs == bt.d_realType;
        if( lhs == bt.d_wcharType )
            return rhs == bt.d_charType;
        if( lhs == bt.d_wstringType )
            return rhs == bt.d_stringType || rhs == bt.d_charType;
        // can happen if QualiType not relovable Q_ASSERT( false );
        return false;
    }

    Type* inclusiveType1(Type* lhs, Type* rhs) const
    {
        if( includes( lhs, rhs ) )
            return lhs;
        else
            return rhs;
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

    inline bool isCharConst( Expression* e ) const
    {
        Type* t = derefed(e->d_type.data());
        if( t == 0 )
            return false;
        if( t->isChar() )
            return true;
        if( t->isString() )
        {
            if( e->getTag() == Thing::T_Literal )
            {
                if( cast<Literal*>(e)->d_strLen == 1 )
                    return true;
            }
        }
        return false;
    }

    bool sameType( Type* lhs, Type* rhs ) const
    {
        if( lhs == 0 && rhs == 0 )
            return true;
        if( lhs == 0 || rhs == 0 )
            return false;
        if( lhs == rhs )
            return true;
        if( derefed(lhs) == derefed(rhs) )
            return true;
        return false;
    }

    bool equalType( Type* lhs, Type* rhs, bool ptrref = false ) const
    {
        // ptrref requred because of ADDROF
        if( sameType(lhs,rhs) )
            return true;
        if( lhs == 0 || rhs == 0 )
            return false;
        lhs = derefed(lhs);
        rhs = derefed(rhs);
        const int lhstag = lhs->getTag();
        const int rhstag = rhs->getTag();
        if( lhstag == Thing::T_Array )
        {
            Array* l = cast<Array*>(lhs);
            Type* lt = derefed( l->d_type.data() );
            if( rhstag == Thing::T_Array )
            {
                Array* r = cast<Array*>(rhs);
                if( l->d_lenExpr.isNull() && ( r->d_lenExpr.isNull() || ptrref ) &&
                        equalType( l->d_type.data(), r->d_type.data() ) )
                    return true;
            }else if( l->d_lenExpr.isNull() &&
                      ( ( lt == bt.d_charType && rhs == bt.d_stringType ) ||
                        ( lt == bt.d_wcharType && ( rhs == bt.d_stringType || rhs == bt.d_wstringType ) ) ) )
                return true; // because of ADDROF with unsafe pointers
        }
        if( lhstag == Thing::T_ProcType && rhstag == Thing::T_ProcType )
            return matchingFormalParamLists( cast<ProcType*>(lhs), cast<ProcType*>(rhs) );
        if( lhstag == Thing::T_Pointer && rhstag == Thing::T_Pointer )
        {
            Pointer* lhsP = cast<Pointer*>(lhs);
            Pointer* rhsP = cast<Pointer*>(rhs);
            return equalType( lhsP->d_to.data(), rhsP->d_to.data(), lhsP->d_unsafe && rhsP->d_unsafe );
        }
        return false;
    }

    bool typeExtension( Type* super, Type* sub ) const
    {
        if( super == 0 || sub == 0 )
            return false;
        if( super == sub )
            return true; // same type
        if( super->getTag() == Thing::T_Pointer )
            super = derefed( cast<Pointer*>(super)->d_to.data() );
        if( sub->getTag() == Thing::T_Pointer )
            sub = derefed( cast<Pointer*>(sub)->d_to.data() );
        if( super == sub )
            return true; // same type
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
                if( subRec->d_baseRec == superRec )
                    return true;
                subRec = subRec->d_baseRec;
            }
        }
        return false;
    }

    bool assignmentCompatible( Type* lhsT, Expression* rhs ) const
    {
        if( lhsT == 0 || rhs == 0 || rhs->d_type.isNull() )
            return false;
        Type* rhsT = rhs->d_type.data();

        //if( sameType(lhs,rhs) )
        //    return true;
        lhsT = derefed(lhsT);
        rhsT = derefed(rhsT);
        if( lhsT == rhsT )
            return true; // T~e~ and T~v~ are the _same type_

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
        if( isNumeric(lhsT) && isNumeric(rhsT) )
        {
            if( includes(lhsT,rhsT) )
                return true;
            else
            {
                warning( rhs->d_loc, Validator::tr("possible loss of information") );
                return true;
            }
        }else if( lhsT == bt.d_wcharType && rhsT == bt.d_charType )
            return true;

        // T~v~ is a SET type and T~e~ is of INTEGER or smaller type
        if( lhsT == bt.d_setType && ( rhsT == bt.d_intType || rhsT == bt.d_shortType || rhsT == bt.d_byteType ) )
            return true;

        // TODO: can we assign records with private fields? if yes, doesn't this undermine class integrity?

        // T~e~ and T~v~ are record types and T~e~ is a _type extension_ of T~v~ and the dynamic type of v is T~v~
        if( typeExtension(lhsT,rhsT) )
            return true;

        const int ltag = lhsT->getTag();
        const int rtag = rhsT->getTag();

        // T~e~ and T~v~ are pointer types and T~e~ is a _type extension_ of T~v~ or the pointers have _equal_ base types
        if( ltag == Thing::T_Pointer && rtag == Thing::T_Pointer && equalType( lhsT, rhsT ) )
            return true;

        // T~v~ is a pointer or a procedure type and `e` is NIL
        if( ( ltag == Thing::T_Pointer || ltag == Thing::T_ProcType ) && rhsT == bt.d_nilType )
            return true;

        // T~v~ is a procedure type and `e` is the name of a procedure whose formal parameters _match_ those of T~v~
        if( ltag == Thing::T_ProcType && rtag == Thing::T_ProcType &&
                matchingFormalParamLists( cast<ProcType*>(lhsT), cast<ProcType*>(rhsT) ))
            return true;

        if( ( lhsT == bt.d_charType && rhsT == bt.d_stringType ) ||
            ( lhsT == bt.d_wcharType && rhsT->isString() ) )
        {
            Named* id = rhs->getIdent();
            if( id && id->getTag() == Thing::T_Const )
                return cast<Const*>(id)->d_strLen == 1;
            if( rhs->getTag() == Thing::T_Literal )
                return cast<Literal*>(rhs)->d_strLen == 1;
        }

        if( ltag == Thing::T_Array )
        {
            Array* l = cast<Array*>(lhsT);
            Type* lt = derefed(l->d_type.data());
            if( rtag == Thing::T_Array )
            {
                // Array := Array
                Array* r = cast<Array*>(rhsT);
                Type* rt = derefed(r->d_type.data());
                if( r->d_lenExpr.isNull() && equalType( lt, rt ) )
                    return true; // T~e~ is an open array and T~v~ is an array of _equal_ base type
                if( lt == bt.d_charType && rt == bt.d_charType )
                    return true;
                if( lt == bt.d_wcharType && rt->isChar() )
                    // TODO: does assig automatic long(char array)?
                    return true;
            }else if( lt == bt.d_charType && ( rhsT == bt.d_stringType || rhsT == bt.d_charType ) )
                // Array := string
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
            Record* rf = tftag == Thing::T_Record ? cast<Record*>(tf) : 0;
            Record* ra = ta->getTag() == Thing::T_Record ? cast<Record*>(ta) : 0;

            // Oberon-2: Ta must be the same type as Tf, or Tf must be a record type and Ta an extension of Tf.
            // T~a~ must be the _same type_ as T~f~,
            // or T~f~ must be a record type and T~a~ an _extension_ of T~f~.
            if( sameType( tf, ta ) || typeExtension(rf,ra) )
                return true;

            // Oberon 90: If a formal variable parameter is of type ARRAY OF BYTE, then the corresponding
            //   actual parameter may be of any type.
            Array* af = tftag == Thing::T_Array ? cast<Array*>(tf) : 0;
            Type* afT = af ? derefed( af->d_type.data() ) : 0;
            if( afT != 0 && afT == bt.d_byteType )
                return true;

            // Oberon 90: The type BYTE is compatible with CHAR and SHORTINT (shortint is 16 bit here)
            if( tf == bt.d_byteType && ta == bt.d_charType )
                return true;

#ifdef OBX_BBOX
            if( lhs->d_const && rhs->getTag() == Thing::T_Literal &&
                    ( ( afT == bt.d_charType && ta == bt.d_stringType ) ||
                      ( afT == bt.d_wcharType && ta->isString() )
                      ) )
                return true; // BBOX supports passing string literals to IN ARRAY TO CHAR/WCHAR

            if( ta == bt.d_nilType && rf && rf->d_unsafe )
                return true; // BBOX supports passing nil to VAR CSTRUCT, and actually also to VAR INTEGER, but OBX
                             // supports the latter by allowing UNSAFE POINTER TO INTEGER
                             // All these calls go to WinApi and WinNet; the original Win32 signatures are pointers,
                             // not var. So we could well remove this rule and fix the BBOX code, but too many places.
#endif

            return false;
        }else
        {
#if 0 // ifdef OBX_BBOX, not needed
            Type* tf = derefed(lhs->d_type.data());
            Type* ta = derefed(rhs->d_type.data());
            if( tf == 0 || ta == 0 )
                return false; // error already handled
            if( toCharArray( tf, true ) && toCharArray(ta, false) )
                return true;
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

    bool arrayCompatible( Type* lhsT, Type* rhsT ) const
    {
        if( lhsT == 0 || rhsT == 0 )
            return false;

        // T~f~ and T~a~ are the _equal type_
        if( equalType( lhsT, rhsT ) )
            return true;

        lhsT = derefed(lhsT);
        rhsT = derefed(rhsT);

        const int ltag = lhsT->getTag();
        Array* la = ltag == Thing::T_Array ? cast<Array*>(lhsT) : 0 ;
        const int rtag = rhsT->getTag();
        Array* ra = rtag == Thing::T_Array ? cast<Array*>(rhsT) : 0 ;

        if( la == 0 || !la->d_lenExpr.isNull() )
            return false; // Tf is not an open array

        // T~f~ is an open array, T~a~ is any array, and their element types are array compatible
        if( ra && arrayCompatible( la->d_type.data(), ra->d_type.data() ) )
            return true;

        Type* laT = la ? derefed(la->d_type.data()) : 0 ;

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
        if( la && laT == bt.d_byteType )
            return true;

        return false;
    }

    bool matchingFormalParamLists( ProcType* lhs, ProcType* rhs, bool allowRhsCovariance = false ) const
    {
        if( lhs == 0 || rhs == 0 )
            return false;
        if( lhs->d_formals.size() != rhs->d_formals.size() )
            return false;
        if( ( lhs->d_return.isNull() && !rhs->d_return.isNull() ) ||
            ( !lhs->d_return.isNull() && rhs->d_return.isNull() ) )
                return false;
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
            error( lhs->d_loc, Validator::tr("cannot assign to read-only designator"));
            return false;
        case Named::Private:
            error( lhs->d_loc, Validator::tr("cannot assign to private designator"));
            return false;
        }
        return true;
    }

    void checkSelfRef( Type* t )
    {
        Type* td = derefed(t);
        if( td == 0 )
            return; // error already reported
        if( td != t )
            return; // don't check type aliasses, only original types

        Type* ctdd = derefed(curTypeDecl);

        if( ctdd && ctdd->getTag() == Thing::T_Pointer )
            return; // legal in any case
            // and a pointer cannot point to a pointer anyway

        switch( td->getTag() )
        {
        case Thing::T_Record:
            {
                Record* r = cast<Record*>(td);
                foreach( const Ref<Field>& f, r->d_fields )
                {
                    Type* ftd = derefed(f->d_type.data());
                    if( ftd == ctdd )
                        error(f->d_type->d_loc, Validator::tr("cannot use structured type to define itself"));
                    else if( ftd == f->d_type.data() ) // dont follow qualitypes
                        checkSelfRef(ftd);
                }
            }
            break;
        case Thing::T_Array:
            {
                Array* a = cast<Array*>(td);
                Type* atd = derefed(a->d_type.data());
                if( atd == ctdd )
                    error(a->d_type->d_loc, Validator::tr("cannot use structured type to define itself"));
                else if( atd == a->d_type.data() ) // dont follow qualitypes
                    checkSelfRef(atd);
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
    imp.bt.assert();
    imp.mod = m;
    imp.insts = insts;
    m->accept(&imp);

    m->d_isValidated = true;

    m->d_hasErrors = ( err->getErrCount() - errCount ) != 0;

    return !m->d_hasErrors;
}

Validator::BaseTypes::BaseTypes()
{
    ::memset(this,0,sizeof(BaseTypes));
}

void Validator::BaseTypes::assert() const
{
    Q_ASSERT( d_boolType && d_charType && d_byteType && d_intType && d_realType && d_setType &&
              d_stringType && d_nilType && d_anyType && d_shortType && d_longType && d_longrealType &&
              d_anyRec && d_wcharType && d_wstringType );
}
