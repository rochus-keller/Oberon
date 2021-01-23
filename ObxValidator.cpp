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
    Ref<Type> resType;
    Validator::BaseTypes bt;
    struct Level
    {
        Scope* scope;
        QList<IfLoop*> loops;
        Level(Scope* s):scope(s){}
    };

    QList<Level> levels;

    ValidatorImp():err(0),mod(0) {}

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
            if( n->getTag() == Thing::T_Variable )
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
    }

    void visit( Module* me)
    {
        levels.push_back(me);
        // imports are supposed to be already resolved at this place
        visitScope(me);
        foreach( const Ref<Statement>& s, me->d_body )
        {
            if( !s.isNull() )
                s->accept(this);
        }
        levels.pop_back();
    }

    void visitBoundProc( Procedure* me )
    {
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
            me->d_receiverRec = r;
            r->d_names[ me->d_name.constData() ] = me;
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
            if( !matchingFormalParamLists( cast<Procedure*>(n)->getProcType(), me->getProcType()
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

        if( !me->d_receiver.isNull() )
        {
            // receiver was already accepted in visitScope
            visitBoundProc(me);
        }
        // no need to visit d_metaParams

        levels.pop_back();
    }

    void visitBody( Procedure* me )
    {
        levels.push_back(me);
        visitScope(me); // also handles formal parameters

        foreach( const Ref<Statement>& s, me->d_body )
        {
            if( !s.isNull() )
                s->accept(this);
        }

        levels.pop_back();
    }

    void visit( Procedure* me )
    {
        Q_ASSERT( false );
    }

    ///////// Expressions

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
            //if( me->d_ident->getTag() == Thing::T_Import )
            //    me->d_mod = static_cast<Import*>( me->d_ident.data() )->d_mod.data();
            me->d_type = me->d_ident->d_type.data();
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
            me->d_ident = modVar;
            me->d_type = modVar->d_type.data();
#if 0
            if( d_fillXref )
                d_xref[modVar].append(id.data());
#endif
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
                me->d_sub = deref.data();
                deref->d_loc = me->d_loc;
                Pointer* p = cast<Pointer*>(prevT);
                prevT = derefed(p->d_to.data());
                deref->d_type = prevT;
                if( prevT == 0 )
                    return;
            }

            if( prevT->getTag() == Thing::T_Record )
            {
                Record* r = cast<Record*>(prevT);
                Named* f = r->find(me->d_name, true);
                if( f == 0 )
                {
                    error( me->d_loc, Validator::tr("record has no field or bound procedure named '%1'").arg(me->d_name.constData()) );
                    return;
                }
                Module* sourceMod = f->getModule();
                Q_ASSERT( sourceMod );
                if( sourceMod != mod && !sourceMod->d_isDef && !f->isPublic() )
                    error( me->d_loc, Validator::tr("element is not public") );

                me->d_ident = f;
                me->d_type = f->d_type.data();
#if 0
                if( d_fillXref )
                    d_xref[f].append(id.data());
#endif
            }else
                error(me->d_loc, Validator::tr("the designated object is not a record") );
        }
    }

    void checkBuiltInArgs( ProcType* p, ArgExpr* args )
    {
        // TODO
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
            Parameter* formal = p->d_formals[i].data();
            Expression* actual = me->d_args[i].data();
            Type* tf = derefed(formal->d_type.data());
            Type* ta = derefed(actual->d_type.data());
            if( tf == 0 || ta == 0 )
                continue; // error already handled

            const int tftag = tf->getTag();
            Array* af = tftag == Thing::T_Array ? cast<Array*>(tf) : 0;

#ifdef OBX_BBOX
            if( ( tftag == Thing::T_Record || tftag == Thing::T_Array ) && ta->getTag() == Thing::T_Pointer )
            {
                me->d_args[i] = new UnExpr(UnExpr::DEREF, actual ); // implicit deref if actual is ptr and formal is arr or rec
                Pointer* p = cast<Pointer*>(ta);
                ta = derefed(p->d_to.data());
                me->d_args[i]->d_type = ta;
                actual = me->d_args[i].data();
            }
#endif

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
#ifdef OBX_BBOX
            else if( toCharArray(tf) && ta == bt.d_stringType )
                ; // NOP
#endif
            else if( !paramCompatible( formal, actual ) )
            {
                error( actual->d_loc,
                       Validator::tr("actual parameter type %1 not compatible with formal type %2%3 of '%4'")
                       .arg(actual->d_type->pretty()).arg(var).arg(formal->d_type->pretty())
                       .arg(formal->d_name.constData()));
            }
        }
    }

    void visit( ArgExpr* me )
    {
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
                const bool isBuiltIn = p->d_ident && p->d_ident->getTag() == Thing::T_BuiltIn;
                if( isBuiltIn )
                    checkBuiltInArgs( p, me );
                else
                    checkCallArgs( p, me );

                if( isBuiltIn )
                {
                    // for some built-in procs the return type is dependent on proc arguments
                    BuiltIn* bi = cast<BuiltIn*>(p->d_ident);
                    switch( bi->d_func )
                    {
                    case BuiltIn::SYS_VAL:
                    case BuiltIn::SYS_ROT:
                    case BuiltIn::SYS_LSH:
                    case BuiltIn::VAL:
                    case BuiltIn::ABS:
                        if( !me->d_args.isEmpty() )
                            me->d_type = me->d_args.first()->d_type;
                        break;
                    case BuiltIn::SHORT:
                        if( !me->d_args.isEmpty() )
                        {
                            Type* t = derefed(me->d_args.first()->d_type.data());
                            if( t == bt.d_longType )
                                me->d_type = bt.d_shortType;
                            else if( t == bt.d_shortType )
                                me->d_type = bt.d_byteType;
                            else if( t == bt.d_intType )
                                me->d_type = bt.d_shortType;
                            else if( t == bt.d_doubleType )
                                me->d_type = bt.d_realType;
#ifdef OBX_BBOX
                            else if( t == bt.d_charType || toCharArray(t) )
                                me->d_type = t;
#endif
                            else
                                error( me->d_loc, Validator::tr("SHORT not applicable to given argument"));
                        }
                        break;
                    case BuiltIn::LONG:
                        if( !me->d_args.isEmpty() )
                        {
                            Type* t = derefed(me->d_args.first()->d_type.data());
                            if( t == bt.d_charType || t == bt.d_byteType )
                                me->d_type = bt.d_shortType;
                            else if( t == bt.d_shortType )
                                me->d_type = bt.d_intType;
                            else if( t == bt.d_intType )
                                me->d_type = bt.d_longType;
                            else if( t == bt.d_realType )
                                me->d_type = bt.d_doubleType;
#ifdef OBX_BBOX
                            else if( t == bt.d_charType || toCharArray(t) )
                                me->d_type = t;
#endif
                            else
                                error( me->d_loc, Validator::tr("LONG not applicable to given argument"));
                        }
                        break;
                    case BuiltIn::MIN:
                    case BuiltIn::MAX:
                        if( !me->d_args.isEmpty() )
                        {
                            if( derefed(me->d_args.first()->d_type.data()) == bt.d_setType )
                                me->d_type = bt.d_intType;
                            else
                                me->d_type = me->d_args.first()->d_type;
                        }
                        break;
                    default:
                        me->d_type = p->d_return.data();
                        break;
                    }
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
            Array* a = cast<Array*>(subType);
            subType = derefed(a->d_type.data());
            if( subType == 0 )
                return; // already reported

            // check if we are really indexing an array and it has the appropriate dimension
            for( int j = 1; j < me->d_args.size() ; j++ )
            {
                if( subType->getTag() == Thing::T_Array )
                {
                    a = cast<Array*>(subType);
                    subType = derefed(a->d_type.data());
                    if( subType == 0 )
                        break;
                }else
                {
                    error( me->d_loc, Validator::tr("index has more dimensions than array") );
                    break;
                }
            }
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
            if( ( isNumeric(lhsT) && isNumeric(rhsT) ) ||
                    ( isTextual(lhsT) && isTextual(rhsT) ) ||
                    ( lhsT == bt.d_boolType && rhsT == bt.d_boolType ) ||
                    ( lhsT == bt.d_setType && rhsT == bt.d_setType ) ||
                    ( ( lhsT == bt.d_nilType || lhsT->getTag() == Thing::T_Pointer ) &&
                      ( rhsT->getTag() == Thing::T_Pointer || rhsT == bt.d_nilType ) ) ||
                    ( ( lhsT == bt.d_nilType || lhsT->getTag() == Thing::T_ProcType ) &&
                      ( rhsT->getTag() == Thing::T_ProcType || rhsT == bt.d_nilType ) ) )
                me->d_type = bt.d_boolType;
            else
            {
                qDebug() << "lhsT" << lhsT->getTagName() << "rhsT" << rhsT->getTagName();
                error( me->d_loc, Validator::tr("operands of the given type cannot be compared") );
            }
            break;

        case BinExpr::LT:
        case BinExpr::LEQ:
        case BinExpr::GT:
        case BinExpr::GEQ:
            if( ( isNumeric(lhsT) && isNumeric(rhsT) ) ||
                    ( isTextual(lhsT) && isTextual(rhsT) ) )
                me->d_type = bt.d_boolType;
            else
            {
                qDebug() << "lhsT" << lhsT->getTagName() << "rhsT" << rhsT->getTagName();
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
            else if( lhsT == bt.d_setType || rhsT == bt.d_setType )
                me->d_type = bt.d_setType;
#ifdef OBX_BBOX
            else if( me->d_op == BinExpr::ADD && isTextual(lhsT) && isTextual(rhsT) )
                me->d_type = bt.d_stringType;
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
        switch( me->d_kind )
        {
        case Literal::Integer:
            if( i >= 0 && i <= 255)
                me->d_type = bt.d_byteType;
            else if( i >= -32768 && i <= 32767 )
                me->d_type = bt.d_shortType;
            else if( i >= -2147483648 && i <= 2147483647 )
                me->d_type = bt.d_intType;
            else
                me->d_type = bt.d_intType;
            break;
        case Literal::Real:
            me->d_type = bt.d_realType; // TODO: adjust precision
            break;
        case Literal::Boolean:
            me->d_type = bt.d_boolType;
            break;
        case Literal::String:
            me->d_type = bt.d_stringType;
            break;
        case Literal::Char:
            me->d_type = bt.d_charType;
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
                Evaluator e;
                const int len = e.eval(me->d_lenExpr.data(), mod,err).toInt(&ok);
                if( ok && len <= 0 )
                    error( me->d_lenExpr->d_loc, Validator::tr("expecting positive non-zero integer for array length") );
                me->d_len = len;
            }
        }
        if( me->d_type )
            me->d_type->accept(this);
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

        foreach( const Ref<Thing>& t, me->d_metaActuals )
            t->accept(this);
        // TODO selfRef
    }

    void visit( Record* me )
    {
        if( me->d_visited )
            return;
        me->d_visited = true;

        if( !me->d_flag.isNull() )
            me->d_flag->accept(this);

        if( !me->d_base.isNull() )
        {
            me->d_base->accept(this);
            QualiType::ModItem mi = me->d_base->getQuali();
            if( mi.second && mi.second->d_type )
            {
                Type* base = derefed( mi.second->d_type.data() );
                if( base->getTag() == Thing::T_Pointer )
                    base = derefed(cast<Pointer*>(base)->d_to.data());
                if( base  && base->getTag() == Thing::T_Record)
                    me->d_baseRec = cast<Record*>(base);
                else
                    error( me->d_base->d_loc, Validator::tr("base type must be a record") );
            }else
                error( me->d_base->d_loc, Validator::tr("cannot resolve base record") );
        }
        foreach( const Ref<Field>& f, me->d_fields )
        {
            f->accept(this);
            Named* found = me->d_baseRec ? me->d_baseRec->find( f->d_name, true ) : 0;
            if( found  )
            {
#ifdef OBX_BBOX
                bool ok = false;
                if( found->getTag() == Thing::T_Field )
                {
                    Field* ff = cast<Field*>(found);
                    Type* super = derefed(ff->d_type.data());
                    Type* sub = derefed(f->d_type.data());
                    if( sub && sub->getTag() == Thing::T_Pointer && super && super->getTag() == Thing::T_Pointer &&
                            typeExtension( super, sub ) )
                    {
                        f->d_specialization = true;
                        ok = true;
                        // there are some cases where fields with same type are overriden; does this make sense? TODO
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
        foreach( const Ref<Parameter>& p, me->d_formals )
            p->accept(this);
    }

    //////// Others

    void visit( NamedType* me )
    {
        // meta params don't have to be visited
        levels.push_back(me);
        if( me->d_type )
            me->d_type->accept(this);
        levels.pop_back();
    }

    void visit( Const* me )
    {
        if( me->d_constExpr.isNull() )
            return;
        me->d_constExpr->accept(this);
        me->d_type = me->d_constExpr->d_type.data();
        Evaluator e;
        me->d_val = e.eval( me->d_constExpr.data(), mod, err );
    }

    void visit( Field* me )
    {
        if( me->d_type )
            me->d_type->accept(this);
    }

    void visit( Variable* me )
    {
        if( me->d_type )
            me->d_type->accept(this);
    }

    void visit( LocalVar* me )
    {
        if( me->d_type )
            me->d_type->accept(this);
    }

    void visit( Parameter* me )
    {
        if( me->d_type )
            me->d_type->accept(this);
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

            const StatSeq& ss = me->d_then[ifThenNr];
            foreach( const Ref<Statement>& s, ss )
            {
                if( !s.isNull() )
                    s->accept(this);
            }

            if( caseId != 0 && !orig.isNull() )
                caseId->d_type = orig;

        }

        foreach( const Ref<Statement>& s, me->d_else )
        {
            if( !s.isNull() )
                s->accept(this);
        }

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

    void visit( Assign* me )
    {
        if( me->d_lhs.isNull() || me->d_rhs.isNull() )
            return; // error already reported
        me->d_lhs->accept(this);
        me->d_rhs->accept(this);
        const quint8 v = me->d_lhs->visibilityFor(mod);
        if( v == Named::ReadOnly )
        {
            error( me->d_lhs->d_loc, Validator::tr("cannot assign to read-only designator"));
            return;
        }
        Named* lhs = me->d_lhs->getIdent();
        if( lhs )
        {
            switch( lhs->getTag() )
            {
            case Thing::T_Field:
            case Thing::T_LocalVar:
            case Thing::T_Variable:
                break;
            case Thing::T_Parameter:
                {
                    Parameter* p = cast<Parameter*>(lhs);
                    // OBX allows assignment to structured value params and imported variables (unless read-only)
                    if( p->d_var && p->d_const )
                    {
                        error( me->d_lhs->d_loc, Validator::tr("cannot assign to IN parameter") );
                        return;
                    }
                }
                break;
            default:
                // BuiltIn, Const, GenericName, Import, Module, NamedType, Procedure
                error( me->d_lhs->d_loc, Validator::tr("cannot assign to '%1'").arg(lhs->d_name.constData()));
                return;
            }
        }
        Type* lhsT = derefed(me->d_lhs->d_type.data());
        Type* rhsT = derefed(me->d_rhs->d_type.data());

#ifdef OBX_BBOX
        const int lhsTag = lhsT ? lhsT->getTag() : 0;
        const int rhsTag = rhsT ? rhsT->getTag() : 0;
        if( ( lhsTag == Thing::T_Record || lhsTag == Thing::T_Array ) && rhsTag == Thing::T_Pointer )
        {
            me->d_rhs = new UnExpr(UnExpr::DEREF, me->d_rhs.data() ); // implicit deref if rhs is ptr and lhs is arr or rec
            Pointer* p = cast<Pointer*>(rhsT);
            rhsT = derefed(p->d_to.data());
            me->d_rhs->d_type = rhsT;
        }

        if( lhsTag == Thing::T_Pointer && ( rhsTag == Thing::T_Record || rhsTag == Thing::T_Array || rhsT == bt.d_stringType ) )
        {
            Pointer* p = cast<Pointer*>(lhsT);
            if( p->d_unsafe )
            {
                Type* lhsT = derefed( p->d_to.data() );
                const int lhsTag = lhsT ? lhsT->getTag() : 0;
                Q_ASSERT( lhsTag == Thing::T_Record || lhsTag == Thing::T_Array );
                if( lhsTag == Thing::T_Record && typeExtension( lhsT, rhsT ) )
                    return; // assign the address of record to unsafe pointer to record
                if( lhsTag == Thing::T_Array && arrayCompatible( lhsT, rhsT ) )
                    return; // assign the address of array to unsafe pointer to array
            }
        }
#endif
        Array* str = toCharArray(lhsT);
        if( str && me->d_rhs->getTag() == Thing::T_Literal )
        {
            Literal* lit = cast<Literal*>( me->d_rhs.data() );
            if( str->d_len && lit->d_kind == Literal::String && lit->d_val.toByteArray().size() > str->d_len )
                error( me->d_rhs->d_loc, Validator::tr("string is too long to assign to given character array"));
            if( str->d_len && lit->d_kind == Literal::Char && 1 > str->d_len )
                error( me->d_rhs->d_loc, Validator::tr("the character array is too small for the character"));
            // TODO: runtime checks for var length arrays

        }

        // lhs and rhs might have no type which might be an already reported error or the attempt to assign no value
        if( !assignmentCompatible( me->d_lhs->d_type.data(), me->d_rhs.data() ) )
        {
            const QString lhs = !me->d_lhs->d_type.isNull() ? me->d_lhs->d_type->pretty() : QString("");
            const QString rhs = !me->d_rhs->d_type.isNull() ? me->d_rhs->d_type->pretty() : QString("");

            error( me->d_rhs->d_loc, Validator::tr("right side %1 of assignment is not compatible with left side %2")
                   .arg(rhs).arg(lhs) );
        }
    }

    void visit( Call* me )
    {
        if( !me->d_what.isNull() )
            me->d_what->accept(this);
    }

    void visit( ForLoop* me )
    {
        me->d_id->accept(this);
        if( !me->d_from.isNull() )
        {
            me->d_from->accept(this);
            if( !me->d_from->d_type.isNull() && !isInteger(derefed(me->d_from->d_type.data())) )
                error( me->d_from->d_loc, Validator::tr("expecting an integer as start value of the for loop"));
        }
        if( !me->d_to.isNull() )
        {
            me->d_to->accept(this);
            if( !me->d_to->d_type.isNull() && !isInteger(derefed(me->d_to->d_type.data())) )
                error( me->d_to->d_loc, Validator::tr("expecting an integer as end value of the for loop"));
        }
        if( !me->d_by.isNull() )
        {
            me->d_by->accept(this);
            if( !me->d_by->d_type.isNull() && !isInteger(derefed(me->d_by->d_type.data())) )
                error( me->d_by->d_loc, Validator::tr("expecting an integer as the step value of the for loop"));
            else
            {
                Evaluator e;
                me->d_byVal = e.eval( me->d_by.data(), mod, err );
            }
        }
        foreach( const Ref<Statement>& s, me->d_do )
        {
            if( !s.isNull() )
                s->accept(this);
        }
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

            foreach( const Ref<Statement>& s, c.d_block )
            {
                if( !s.isNull() )
                    s->accept(this);
            }
        }

        if( me->d_typeCase )
            caseId->d_type = orig;

        foreach( const Ref<Statement>& s, me->d_else )
        {
            if( !s.isNull() )
                s->accept(this);
        }
    }

    ///////// NOP

    void visit( BaseType* ) { }
    void visit( BuiltIn* ) { }
    void visit( GenericName* ) { }
    void visit( Import* ) {}

    ////////// Utility

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
        return isInteger(t) || isReal(t);
    }

    inline bool isInteger(Type* t) const
    {
        if( t == 0 )
            return 0;
        return t == bt.d_byteType || t == bt.d_intType || t == bt.d_shortType || t == bt.d_longType;
    }

    inline bool isReal(Type* t) const
    {
        if( t == 0 )
            return 0;
        return t == bt.d_realType || t == bt.d_doubleType;
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
        if( lhs == bt.d_doubleType )
            return rhs == bt.d_byteType || rhs == bt.d_intType || rhs == bt.d_shortType || rhs == bt.d_longType ||
                    rhs == bt.d_realType;
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
            return bt.d_doubleType;
    }

    inline Array* toCharArray( Type* t, bool resolvePointer = true ) const
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
        if( t && t == bt.d_charType )
            return a;
        else
            return 0;
    }

    inline bool isTextual( Type* t ) const
    {
        return t == bt.d_charType || t == bt.d_stringType || toCharArray(t);
    }

    inline bool isCharConst( Expression* e ) const
    {
        Type* t = derefed(e->d_type.data());
        if( t == bt.d_charType )
            return true;
        if( t == bt.d_stringType )
        {
            if( e->getTag() == Thing::T_Literal )
            {
                Literal* str = cast<Literal*>(e);
                if( str->d_val.toByteArray().size() == 1 )
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

    bool equalType( Type* lhs, Type* rhs ) const
    {
        if( sameType(lhs,rhs) )
            return true;
        if( lhs == 0 || rhs == 0 )
            return false;
        lhs = derefed(lhs);
        rhs = derefed(rhs);
        const int lhstag = lhs->getTag();
        const int rhstag = rhs->getTag();
        if( lhstag == Thing::T_Array && rhstag == Thing::T_Array )
        {
            Array* l = cast<Array*>(lhs);
            Array* r = cast<Array*>(rhs);
            if( l->d_lenExpr.isNull() && r->d_lenExpr.isNull() &&
                    equalType( l->d_type.data(), r->d_type.data() ) )
                return true;
        }
        if( lhstag == Thing::T_ProcType && rhstag == Thing::T_ProcType )
            return matchingFormalParamLists( cast<ProcType*>(lhs), cast<ProcType*>(rhs) );
        if( lhstag == Thing::T_Pointer && rhstag == Thing::T_Pointer )
            return equalType( cast<Pointer*>(lhs)->d_to.data(), cast<Pointer*>(rhs)->d_to.data() );
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
            return true; // sameType implementation

        // Oberon 90: The type BYTE is compatible with CHAR and SHORTINT (shortint is 16 bit here)
        if( lhsT == bt.d_byteType && rhsT == bt.d_charType )
            return true;

        if( isNumeric(lhsT) && isNumeric(rhsT) )
        {
            if( includes(lhsT,rhsT) )
                return true;
            else
            {
                warning( rhs->d_loc, Validator::tr("possible loss of information") );
                return true;
            }
        }

        if( lhsT == bt.d_setType && ( rhsT == bt.d_intType || rhsT == bt.d_shortType || rhsT == bt.d_byteType ) )
            return true;

        if( typeExtension(lhsT,rhsT) )
            return true;

        const int ltag = lhsT->getTag();

        if( ( ltag == Thing::T_Pointer || ltag == Thing::T_ProcType ) && rhsT == bt.d_nilType )
            return true;

        const int rtag = rhsT->getTag();

        if( ltag == Thing::T_ProcType && rtag == Thing::T_ProcType &&
                matchingFormalParamLists( cast<ProcType*>(lhsT), cast<ProcType*>(rhsT) ))
            return true;

        if( ltag == Thing::T_Pointer && rtag == Thing::T_Pointer && equalType( lhsT, rhsT ) )
            return true;

        if( lhsT == bt.d_charType && ( rhs->getTag() == Thing::T_Literal && rhsT == bt.d_stringType ) )
        {
            Literal* l = cast<Literal*>(rhs);
            return l->d_val.toByteArray().size() == 1;
        }

        if( lhsT == bt.d_byteType &&
                ( rhsT == bt.d_charType ||
                ( rtag == Thing::T_Array && derefed( cast<Array*>( rhsT )->d_type.data() ) == bt.d_charType ) ) )
            return true;

        if( ltag == Thing::T_Array )
        {
            // Array Array
            Array* l = cast<Array*>(lhsT);
            Type* lt = derefed(l->d_type.data());
            if( rtag == Thing::T_Array )
            {
                Array* r = cast<Array*>(rhsT);
                Type* rt = derefed(r->d_type.data());
                if( lt == bt.d_charType && rt == bt.d_charType )
                    return true;
            }else if( rhsT == bt.d_stringType || rhsT == bt.d_charType )
                return true;
        }

        return false;
    }

    bool paramCompatible( Parameter* lhs, Expression* rhs )
    {
        if( equalType( lhs->d_type.data(), rhs->d_type.data() ) )
            return true;
        if( lhs->d_var || lhs->d_const )
        {
            Type* tf = derefed(lhs->d_type.data());
            Type* ta = derefed(rhs->d_type.data());
            if( tf == 0 || ta == 0 )
                return false; // error already handled
#ifdef OBX_BBOX
            if( ta == bt.d_nilType )
                return true;
#endif
            const int tftag = tf->getTag();
            Record* rf = tftag == Thing::T_Record ? cast<Record*>(tf) : 0;
            Record* ra = ta->getTag() == Thing::T_Record ? cast<Record*>(ta) : 0;

            // Oberon-2: Ta must be the same type as Tf, or Tf must be a record type and Ta an extension of Tf.
            const bool isTypeExt = typeExtension(rf,ra);
            const bool isSameT = sameType( tf, ta );
            // Oberon 90: If a formal parameter is of type ARRAY OF BYTE, then the corresponding
            //   actual parameter may be of any type.
            Array* af = tftag == Thing::T_Array ? cast<Array*>(tf) : 0;
            const bool isFormalBa = ( af != 0 && derefed(af->d_type.data()) == bt.d_byteType );
            // Oberon 90: The type BYTE is compatible with CHAR and SHORTINT (shortint is 16 bit here)
            const bool isByteChar = tf == bt.d_byteType && ta == bt.d_charType;
            if( !isTypeExt && !isSameT && !isFormalBa && !isByteChar )
                return false;
            else
                return true;
        }else
        {
#ifdef OBX_BBOX
            Type* tf = derefed(lhs->d_type.data());
            Type* ta = derefed(rhs->d_type.data());
            if( tf == 0 || ta == 0 )
                return false; // error already handled
            if( toCharArray( tf, true ) && toCharArray(ta, false) )
                return true;
#endif
            return assignmentCompatible( lhs->d_type.data(), rhs );
        }
    }

    bool arrayCompatible( Type* lhsT, Type* rhsT ) const
    {
        if( lhsT == 0 || rhsT == 0 )
            return false;

        if( equalType( lhsT, rhsT ) )
            return true;

        lhsT = derefed(lhsT);
        rhsT = derefed(rhsT);

        const int ltag = lhsT->getTag();
        Array* la = ltag == Thing::T_Array ? cast<Array*>(lhsT) : 0 ;
        const int rtag = rhsT->getTag();
        Array* ra = rtag == Thing::T_Array ? cast<Array*>(rhsT) : 0 ;

        // Tf is an open array, Ta is any array, and their element types are array compatible, or
        if( la && la->d_lenExpr.isNull() && ra && arrayCompatible( la->d_type.data(), ra->d_type.data() ) )
            return true;

        Type* type = la ? derefed(la->d_type.data()) : 0 ;

        if( la && type == bt.d_charType && rhsT == bt.d_stringType )
            return true;

        if( la && type == bt.d_charType && rhsT == bt.d_charType )
            return true;

        // Oberon 90: If a formal parameter is of type ARRAY OF BYTE, then the corresponding
        //   actual parameter may be of any type.
        // Oberon-2: If a formal **variable** parameter is of type ARRAY OF BYTE then the corresponding
        //   actual parameter may be of any type.
        if( la && type == bt.d_byteType )
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
};

bool Validator::check(Module* m, const BaseTypes& bt, Ob::Errors* err)
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
    m->accept(&imp);

    m->d_hasErrors = ( err->getErrCount() - errCount ) != 0;

    return !m->d_hasErrors;
}

Validator::BaseTypes::BaseTypes()
{
    ::memset(this,sizeof(BaseType),0);
}

void Validator::BaseTypes::assert() const
{
    Q_ASSERT( d_boolType && d_charType && d_byteType && d_intType && d_realType && d_setType &&
              d_stringType && d_nilType && d_anyType && d_anyNum && d_shortType && d_longType && d_doubleType &&
              d_anyRec );
}
