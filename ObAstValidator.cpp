/*
* Copyright 2019, 2020 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon parser/code model library.
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

#include "ObAstValidator.h"
#include "ObAst.h"
#include "ObErrors.h"
#include <QtDebug>
using namespace Ob;
using namespace Ob::Ast;

static const bool s_strict = false; // TODO

struct ValidatorImp : public AstVisitor
{
    Errors* err;
    Model::BaseTypes bt;
    Module* mod;

    void visit( BaseType* ) {} // NOP

    void visit( Pointer* p)
    {
        Q_ASSERT( !p->d_to.isNull() && p->d_to->derefed()->getTag() == Thing::T_Record );
        removeSelfRef(p->d_to);

        if( p->d_to->d_ident == 0 )
            p->d_to->accept(this);
    }

    void visit( Array* a)
    {
        Q_ASSERT( !a->d_type.isNull() );
        if( !a->d_lenExpr.isNull() ) // may be null when ARRAY OF params
            a->d_lenExpr->accept(this);
        removeSelfRef(a->d_type);
        if( a->d_type->d_ident == 0 )
            a->d_type->accept(this);
    }

    void visit( Record* r )
    {
        if( !r->d_base.isNull() )
        {
            Q_ASSERT( !r->d_base->d_quali->d_type.isNull() && r->d_base->d_quali->getIdent() != 0 );

            Type* base = r->d_base->d_quali->d_type->derefed();
            Q_ASSERT( base != 0 );
            const int tag = base->getTag();
            if( tag != Thing::T_Record && tag != Thing::T_Pointer )
                error(r->d_base->d_quali->d_loc,tr("expecting record or pointer to record"));
            Record* br = r->getBaseRecord();
            while( br )
            {
                // check for base self reference
                if( br == r )
                {
                    error(r->d_base->d_quali->d_loc,tr("base of record references itself"));
                    break;
                }
                br = br->getBaseRecord();
            }
        }
        for( int i = 0; i < r->d_fields.size(); i++ )
        {
            Q_ASSERT( !r->d_fields[i].isNull() );
            r->d_fields[i]->accept(this);
        }
    }

    void visit( ProcType* p )
    {
        removeSelfRef(p->d_return);
        if( !p->d_return.isNull() )
        {
            p->d_return->accept(this);

            Q_ASSERT( p->d_return->getTag() == Thing::T_QualiType );
            Type* rt = p->d_return->derefed();
            if( rt->getTag() == Thing::T_Record || rt->getTag() == Thing::T_Array )
            {
                QualiType* q = Ast::thing_cast<QualiType*>(p->d_return.data());
                error( q->d_quali->getIdent()->d_loc,
                       tr("The result type of a procedure can be neither a record nor an array"));
            }
        }
        for( int i = 0; i < p->d_formals.size(); i++ )
            p->d_formals[i]->accept(this);
    }

    void visit( QualiType* q )
    {
        // References already validated idents, thus not again here
        // q->d_quali->accept(this);
    }

    void visit( Field* f)
    {
        if( f->d_type.isNull() )
            return;
        removeSelfRef(f->d_type);
        if( f->d_type->d_ident == 0 )
            f->d_type->accept(this);
    }

    void visit( Variable* v)
    {
        if( v->d_type.isNull() )
            return;
        removeSelfRef(v->d_type);
        if( v->d_type->d_ident == 0 )
            v->d_type->accept(this);
    }

    void visit( LocalVar* v )
    {
        if( v->d_type.isNull() )
            return;
        removeSelfRef(v->d_type);
        if( v->d_type->d_ident == 0 )
            v->d_type->accept(this);
    }

    void visit( Parameter* p)
    {
        if( p->d_type.isNull() )
            return;
        removeSelfRef(p->d_type);
        if( p->d_type->d_ident == 0 )
            p->d_type->accept(this);
    }

    void visit( NamedType* t)
    {
        if( t->d_type.isNull() )
            return;
        Q_ASSERT( !t->d_type->isSelfRef() );
        if( t->d_type->d_ident == t )
            t->d_type->accept(this);
    }
    void visit( Const* c)
    {
        Q_ASSERT( !c->d_constExpr.isNull() );
        c->d_constExpr->accept(this);
    }

    void visit( Import* )
    { // NOP
    }

    void visit( Procedure* m )
    {
        Q_ASSERT( !m->d_type.isNull() && m->d_type->derefed()->getTag() == Thing::T_ProcType );
        removeSelfRef(m->d_type);

        if( m->d_type->d_ident == 0 )
            m->d_type->accept(this);

        for( int i = 0; i < m->d_order.size(); i++ )
            m->d_order[i]->accept(this);
        for( int i = 0; i < m->d_body.size(); i++ )
            m->d_body[i]->accept(this);

        ProcType* pt = Ast::thing_cast<ProcType*>(m->d_type->derefed() );

        // check again d_return for not Array or Record (after removing SelfRefs)
        if( !pt->d_return.isNull() &&
                ( pt->d_return->derefed()->getTag() == Thing::T_Record ||
                  pt->d_return->derefed()->getTag() == Thing::T_Array ) )
            error( m->d_loc, tr("The result type of a procedure can be neither a record nor an array"));

        if( !mod->d_isDef && !pt->d_return.isNull() )
        {
            Q_ASSERT( !m->d_body.isEmpty() && m->d_body.last()->getTag() == Thing::T_Return );
            Return* r = Ast::thing_cast<Return*>( m->d_body.last().data() );
            checkAssignableToType( pt->d_return.data(), r->d_what.data(), false );
        }else if( !m->d_body.isEmpty() )
        {
            if( m->d_body.last()->getTag() == Thing::T_Return )
                error( m->d_body.last()->d_loc, tr("RETURN not allowed in proper procedure") );
        }
    }

    void visit( BuiltIn* )
    { // NOP
    }

    void visit( Module* m )
    {
        for( int i = 0; i < m->d_order.size(); i++ )
            m->d_order[i]->accept(this);
        for( int i = 0; i < m->d_body.size(); i++ )
            m->d_body[i]->accept(this);
    }

    void visit( Call* c )
    {
        Q_ASSERT( !c->d_what.isNull() );
        c->d_what->accept(this);
        Q_ASSERT( c->d_what->getTag() == Thing::T_CallExpr );
    }

    void visit( Return* r )
    {
        Q_ASSERT( !r->d_what.isNull() );
        r->d_what->accept(this);
    }

    void visit( Assign* a )
    {
        Q_ASSERT( !a->d_lhs.isNull() && !a->d_rhs.isNull() );
        a->d_lhs->accept(this);
        a->d_rhs->accept(this);

        Expression* leaf = getTail(a->d_lhs.data());
        Q_ASSERT( leaf && leaf->getIdent() );
        if( leaf->getIdent()->getTag() == Thing::T_Parameter )
        {
            // only the first lhs of the desig can be a parameter
            Parameter* p = Ast::thing_cast<Parameter*>( leaf->getIdent() );
            if( !p->d_var && ( p->d_type->derefed()->getTag() == Thing::T_Array ||
                               p->d_type->derefed()->getTag() == Thing::T_Record ) )
                error( leaf->d_loc, tr("cannot assign to structured value parameter") );
        }
        if( leaf->getIdent()->getTag() == Thing::T_Import )
            error( leaf->d_loc, tr("cannot assign to imported values") );
        else if( leaf->getIdent()->getTag() == Thing::T_Const )
            error( leaf->d_loc, tr("cannot assign to a constant") );

        checkAssignableToType( a->d_lhs->d_type.data(), a->d_rhs.data(), false );
    }

    void visit( IfLoop* l )
    {
        for( int i = 0; i < l->d_if.size(); i++ )
        {
            l->d_if[i]->accept(this);
            if( l->d_if[i]->d_type->derefed() != bt.d_boolType )
                error( l->d_if[i]->d_loc, tr("expecting boolean expression") );
        }
        for( int i = 0; i < l->d_then.size(); i++ )
        {
            const StatSeq& then = l->d_then[i];
            for( int j = 0; j < then.size(); j++ )
                then[j]->accept(this);
        }
        for( int i = 0; i < l->d_else.size(); i++ )
            l->d_else[i]->accept(this);
    }

    void visit( ForLoop* l )
    {
        Q_ASSERT( !l->d_id.isNull() && !l->d_id->d_type.isNull() );
        if( l->d_id->d_type->derefed() != bt.d_intType )
            error( l->d_loc, tr("control variable must be of type INTEGER") );

        Q_ASSERT( !l->d_from.isNull() && !l->d_to.isNull() && !l->d_by.isNull() );
        l->d_from->accept(this);
        if( l->d_from->d_type->derefed() != bt.d_intType )
            error( l->d_loc, tr("begin expression must be of type INTEGER") );
        l->d_to->accept(this);
        if( l->d_to->d_type->derefed() != bt.d_intType )
            error( l->d_loc, tr("end expression must be of type INTEGER") );
        l->d_by->accept(this);
        if( l->d_by->d_type->derefed() != bt.d_intType )
            error( l->d_loc, tr("increment expression must be of type INTEGER") );
        for( int i = 0; i < l->d_do.size(); i++ )
            l->d_do[i]->accept(this);
    }

    void visit( CaseStmt* c )
    {
        Q_ASSERT( !c->d_exp.isNull() );
        c->d_exp->accept(this);
        Type* caseType = c->d_exp->d_type->derefed();

        if( c->d_typeCase &&
                ( c->d_exp->d_type->derefed()->getTag() != Thing::T_Record ||
                    c->d_exp->getIdent() == 0 || !c->d_exp->getIdent()->isVarParam() )
                && c->d_exp->d_type->derefed()->getTag() != Thing::T_Pointer )
            error( c->d_exp->d_loc,tr("type case variable must be a record VAR parameter or pointer") );

        for( int i = 0; i < c->d_cases.size(); i++ )
        {
            for( int j = 0; j < c->d_cases[i].d_labels.size(); j++ )
            {
                c->d_cases[i].d_labels[j]->accept(this);
                if( c->d_typeCase )
                {
                    if( !Model::isSubType( c->d_cases[i].d_labels[j]->d_type.data(), caseType ) )
                        error( c->d_cases[i].d_labels[j]->d_loc, tr("case labels must be extensions of case designator") );
                }else
                {
                    if( !isComparableType( caseType, c->d_cases[i].d_labels[j]->d_type.data() ) )
                        error( c->d_cases[i].d_labels[j]->d_loc,
                               tr("case labels must have a type compatibly with the case expression") );
                }
            }
            for( int j = 0; j < c->d_cases[i].d_block.size(); j++ )
                c->d_cases[i].d_block[j]->accept(this);
        }
    }

    void visit( Literal* )
    { // NOP
    }

    void visit( SetExpr* s)
    {
        for( int i = 0; i < s->d_parts.size(); i++ )
        {
            s->d_parts[i]->accept(this);
            Type* t = s->d_parts[i]->d_type.isNull() ? 0 : s->d_parts[i]->d_type->derefed();
            if( t == 0 ||
                    ( t != bt.d_intType
                      && t != bt.d_byteType ) )
                error(s->d_parts[i]->d_loc, tr("expecting INTEGER for SET elements") );
        }
    }

    void visit( IdentLeaf* )
    { // NOP
    }

    void visit( UnExpr* e )
    {
        Q_ASSERT( !e->d_sub.isNull() && !e->d_sub->d_type.isNull() );
        e->d_sub->accept(this);
        Type* td = e->d_type->derefed();
        Type* subtd = e->d_sub->d_type->derefed();
        switch( e->d_op )
        {
        case UnExpr::NOT:
            if( subtd != bt.d_boolType )
                error(e->d_loc, tr("NOT requires boolean operand") );
            break;
        case UnExpr::NEG:
            if( !isNumeric(subtd) && subtd != bt.d_setType )
                error(e->d_loc, tr("unary minus requires numeric or set operand") );
            break;
        case UnExpr::DEREF:
            // NOP
            break;
        case UnExpr::CAST:
            // The guard is applicable, if
            // T0 is an extension of the declared type T of v, and if
            Q_ASSERT( !e->d_type.isNull() );
            if( td->getTag() != Thing::T_Record && td->getTag() != Thing::T_Pointer )
                error(e->d_loc,tr("the type guard must be a record or pointer") );

            // if( !isSubType( type, tg->d_sub->d_type.data() ) ) // doesn't work, still 8 errors in Oberon System
            //      error( sel, tr("the designated type is not a subtype of the actual type") );

            // v is a variable parameter of record type, or v is a pointer.
            if( ( subtd->getTag() != Thing::T_Record ||
                  e->d_sub->getIdent() == 0 || !e->d_sub->getIdent()->isVarParam() )
                    && subtd->getTag() != Thing::T_Pointer )
                error( e->d_sub->d_loc,tr("the type guarded variable must be a record VAR parameter or pointer") );
            break;
        }
    }

    void visit( IdentSel* e )
    {
        Q_ASSERT( !e->d_sub.isNull() );
        e->d_sub->accept(this);
    }

    void visit( CallExpr* c )
    {
        Q_ASSERT( !c->d_sub.isNull() && !c->d_sub->d_type.isNull() );
        c->d_sub->accept(this);
        for( int i = 0; i < c->d_actuals.size(); i++ )
            c->d_actuals[i]->accept(this);
        Type* td = c->d_sub->d_type->derefed();
        Q_ASSERT( td->getTag() == Thing::T_ProcType );
        ProcType* p = Ast::thing_cast<ProcType*>(td);
        checkActuals(p,c->d_actuals,c);
    }

    void visit( BinExpr* e )
    {
        Q_ASSERT( !e->d_lhs.isNull() && !e->d_rhs.isNull() );
        Q_ASSERT( !e->d_lhs->d_type.isNull() && !e->d_rhs->d_type.isNull() );
        e->d_lhs->accept(this);
        e->d_rhs->accept(this);

        Type* lhs = e->d_lhs->d_type->derefed();
        Type* rhs = e->d_rhs->d_type->derefed();

        switch( e->d_op )
        {
        case BinExpr::Range:
        // arithmetic
        case BinExpr::ADD: case BinExpr::SUB:
        case BinExpr::MUL: case BinExpr::FDIV: case BinExpr::DIV: case BinExpr::MOD:
        // logical
        case BinExpr::AND: case BinExpr::OR:
        // relation
        case BinExpr::EQ: case BinExpr::NEQ: case BinExpr::LT: case BinExpr::LEQ:
        case BinExpr::GT: case BinExpr::GEQ:
            if( !isComparableType(lhs,rhs) )
                error( e->d_loc,tr("operands must be of the same type"));
            break;
        default:
            break;
        }

        switch( e->d_op )
        {
        case BinExpr::AND:
        case BinExpr::OR:
            if( lhs != bt.d_boolType )
               error(e->d_loc,tr("operator requires boolean operands"));
            break;
        case BinExpr::EQ:
        case BinExpr::NEQ:
            if( !isNumeric(lhs)
                    && lhs != bt.d_charType
                    && lhs != bt.d_stringType
                    && !isString( lhs )
                    && lhs != bt.d_boolType
                    && lhs != bt.d_setType
                    && lhs->getTag() != Thing::T_Pointer
                    && lhs->getTag() != Thing::T_ProcType )
                error(e->d_loc,tr("operator requires numeric, string or char operands"));
           break;
        case BinExpr::GT:
        case BinExpr::GEQ:
        case BinExpr::LT:
        case BinExpr::LEQ:
            if( !isNumeric(lhs)
                    && lhs != bt.d_charType
                    && lhs != bt.d_stringType
                    && !isString( lhs ) )
                error(e->d_loc,tr("operator requires numeric, string or char operands"));
            break;
        case BinExpr::IN:
            if( lhs != bt.d_intType && lhs != bt.d_byteType )
                error(e->d_loc,tr("left side of IN must be integer"));
            else if( rhs != bt.d_setType )
                error(e->d_loc,tr("right side of IN must be set"));
            break;
        case BinExpr::Range:
            if( lhs != rhs )
                error(e->d_loc,tr("lower and upper bound of range must be of same type"));
            if( lhs != bt.d_intType
                    && lhs != bt.d_byteType
                    && lhs != bt.d_charType
                    && lhs != bt.d_stringType )
                error(e->d_loc,tr("range with invalid types"));
            break;
        case BinExpr::Index:
            if( rhs != bt.d_intType &&
                    rhs != bt.d_byteType )
                error(e->d_loc,tr("index must be of type integer or byte"));
            break;
        case BinExpr::IS:
            {
                // v is a variable parameter of record type, or v is a pointer.
                if( ( lhs->getTag() != Thing::T_Record ||
                      e->d_lhs->getIdent() == 0 || !e->d_lhs->getIdent()->isVarParam() )
                        && lhs->getTag() != Thing::T_Pointer )
                    error( e->d_lhs->d_loc,tr("left operand of IS must be a record VAR parameter or pointer") );
                if( Model::toRecord(rhs) == 0 )
                    error(e->d_loc,tr("right operand of IS must be of RECORD or POINTER type"));
            }
            break;
        case BinExpr::DIV:
        case BinExpr::MOD:
            if( lhs != bt.d_intType && lhs != bt.d_byteType )
                error(e->d_loc,tr("DIV and MOD require operands of type INTEGER"));
            break;
        case BinExpr::ADD:
        case BinExpr::SUB:
        case BinExpr::MUL:
        case BinExpr::FDIV:
            if( !isNumeric(lhs) && lhs != bt.d_setType )
                error(e->d_loc,tr("operator requires numeric or set operands"));
            break;
        }
    }

    void removeSelfRef( Ref<Type>& t )
    {
        if( !t.isNull() && t->isSelfRef() && t->getTag() == Thing::T_QualiType )
        {
            QualiType* q = Ast::thing_cast<QualiType*>( t.data() );
            q->d_quali->d_type = q->d_quali->getIdent()->d_type.data();
            q->d_selfRef = false;
        }
    }

    Expression* getTail( Expression* e )
    {
        if( e == 0 )
            return 0;
        switch( e->getTag() )
        {
        case Thing::T_UnExpr:
        case Thing::T_IdentSel:
        case Thing::T_CallExpr:
            return getTail( Ast::thing_cast<UnExpr*>(e)->d_sub.data() );
        case Thing::T_BinExpr:
            return getTail( Ast::thing_cast<BinExpr*>(e)->d_lhs.data() );
        case Thing::T_IdentLeaf:
            return e;
        default:
            return 0;
        }
    }

    static bool isEqualType( Type* lhs, Type* rhs, bool withProcType )
    {
        lhs = lhs ? lhs->derefed() : 0;
        rhs = rhs ? rhs->derefed() : 0;

        if( lhs == rhs )
            return true;

        else if( lhs == 0 || rhs == 0 )
            return false;

        else if( lhs->getTag() == Thing::T_Pointer && rhs->getTag() == Thing::T_Pointer )
            return isEqualType( Ast::thing_cast<Pointer*>(lhs)->d_to.data(),
                                Ast::thing_cast<Pointer*>(rhs)->d_to.data(), withProcType );

        else if( lhs->getTag() == Thing::T_Array && rhs->getTag() == Thing::T_Array )
        {
            Array* al = Ast::thing_cast<Array*>(lhs);
            Array* ar = Ast::thing_cast<Array*>(rhs);
            return ( al->d_len == 0 || al->d_len == ar->d_len ) &&
                    isEqualType(al->d_type.data(), ar->d_type.data(), withProcType );
        }

        else if( withProcType && lhs->getTag() == Thing::T_ProcType && rhs->getTag() == Thing::T_ProcType )
        {
            ProcType* pl = Ast::thing_cast<ProcType*>(lhs);
            ProcType* pr = Ast::thing_cast<ProcType*>(rhs);
            if( !isEqualType( pl->d_return.data(), pr->d_return.data(), withProcType ) )
                return false;
            if( pl->d_formals.size() != pr->d_formals.size() )
                return false;
            for( int i = 0; i < pl->d_formals.size(); i++ )
            {
                if( pl->d_formals[i]->d_var != pr->d_formals[i]->d_var ||
                        !isEqualType( pl->d_formals[i]->d_type.data(), pr->d_formals[i]->d_type.data(), withProcType ) )
                    return false;
            }
            return true;
        }

        return false;
    }

    bool isComparableType(Type* lhs, Type* rhs) const
    {
        if( isEqualType(lhs,rhs, true ) )
            return true;

        if( lhs == 0 || rhs == 0 )
            return false;
        lhs = lhs->derefed();
        rhs = rhs->derefed();

#if 0
        // no longer used
        if( lhs == bt.d_anyType || rhs == bt.d_anyType )
            return true;
#else
        Q_ASSERT( lhs != bt.d_anyType && rhs != bt.d_anyType );
        // we replace anytype returns in Model::designator by true types
#endif
        if( lhs->getTag() == Thing::T_Pointer && rhs->getTag() == Thing::T_Pointer )
            return Model::isSubType(lhs,rhs) || Model::isSubType(rhs,lhs);
#if 0
        if( lhs->getTag() == Thing::T_ProcType && rhs->getTag() == Thing::T_ProcType )
            return true;
#endif
        if( lhs == bt.d_nilType )
            return rhs->getTag() == Thing::T_Pointer || rhs->getTag() == Thing::T_ProcType;
        if( rhs == bt.d_nilType )
            return lhs->getTag() == Thing::T_Pointer || lhs->getTag() == Thing::T_ProcType;
#if 0
        if( lhs == bt.d_anyNum )
            return rhs == bt.d_intType || rhs == bt.d_realType || rhs == bt.d_byteType;
        if( rhs == bt.d_anyNum )
            return lhs == bt.d_intType || lhs == bt.d_realType || lhs == bt.d_byteType;
#else
        Q_ASSERT( lhs != bt.d_anyNum && rhs != bt.d_anyNum ); // we replace it in Model::designator
#endif
        if( ( lhs == bt.d_intType && rhs == bt.d_byteType ) ||
                ( lhs == bt.d_byteType && rhs == bt.d_intType ) )
            return true;
        if( isString(rhs) && isString(lhs) ) // needed to compare arrays of differen lenght
            return true;
        if( ( lhs == bt.d_charType && rhs == bt.d_stringType ) ||
                ( lhs == bt.d_stringType && rhs == bt.d_charType ) )
            return true;
        if( ( lhs == bt.d_charType && isString(rhs) ) || ( isString(lhs) && rhs == bt.d_charType ) )
            return true;
        if( ( lhs == bt.d_stringType && isString(rhs) ) || ( isString(lhs) && rhs == bt.d_stringType ) )
            return true;
        return false;
    }

    bool inline error( const RowCol& l, const QString& msg )
    {
        err->error(Errors::Semantics, mod->d_file, l.d_row, l.d_col, msg );
        return false;
    }

    bool inline warning( const RowCol& l, const QString& msg )
    {
        err->warning(Errors::Semantics, mod->d_file, l.d_row, l.d_col, msg );
        return true;
    }

    static inline QString tr(const char* msg )
    {
        return Validator::tr(msg);
    }

    inline bool isString( Type* t ) const
    {
        return t && t->getTag() == Thing::T_Array && Ast::thing_cast<Array*>(t)->d_type->derefed() == bt.d_charType;
    }

    inline bool isNumeric( Type* t ) const
    {
        const bool res = t && ( t == bt.d_byteType || t == bt.d_intType || t == bt.d_realType );
        return res;
    }

    bool checkActuals(const ProcType* p, const Ast::CallExpr::Actuals& a, Expression* e)
    {
        if( p->d_ident && p->d_ident->getTag() == Thing::T_BuiltIn )
        {
            BuiltIn* bi = Ast::thing_cast<BuiltIn*>(p->d_ident);
            switch( bi->d_func )
            {
            case BuiltIn::INC:
            case BuiltIn::DEC:
                if( a.size() == 1 ) // version with two params is declared as such
                {
                    checkAssignableToType( bt.d_intType, a.first().data(), true );
                    if( a.first()->d_type->derefed() != bt.d_intType )
                        return error(e->d_loc, tr("INTEGER parameter expected") );
                    else
                        return true;
                }
                break;
            case BuiltIn::ORD:
                if( a.size() == 1 )
                {
                    Type* t = a.first()->d_type.isNull() ? 0 : a.first()->d_type->derefed();
                    if( t == 0 ||
                            ( t != bt.d_charType &&
                              t != bt.d_stringType && // happens in Net.Mod
                              t != bt.d_boolType &&
                              t != bt.d_setType &&
                              // In Oberon System ORD is also called with REAL and POINTER
                              t != bt.d_realType &&
                              t->getTag() != Thing::T_Pointer ) )
                        return error(e->d_loc, tr("SET, CHAR or BOOLEAN parameter expected") );
                    else
                        return true;
                }
                break;
            case BuiltIn::LEN:
                if( a.size() == 1 )
                {
                    if( !a.first()->d_type.isNull() && a.first()->d_type->derefed()->getTag() != Thing::T_Array )
                        return error(e->d_loc, tr("array parameter expected") );
                    else
                        return true;
                }
                break;
            case BuiltIn::NEW:
                if( a.size() == 1 )
                {
                    checkAssignableToVar( a.first().data() );
                    if( !a.first()->d_type.isNull() && a.first()->d_type->derefed()->getTag() != Thing::T_Pointer )
                        return error(e->d_loc, tr("pointer parameter expected") );
                    else
                        return true;
                }
                break;
            case BuiltIn::ABS:
                if( a.size() == 1 )
                {
                    Type* t = a.first()->d_type.isNull() ? 0 : a.first()->d_type->derefed();
                    if( t == 0 ||
                            ( t != bt.d_intType &&
                              t != bt.d_realType ) )
                        return error(e->d_loc, tr("numeric parameter expected") );
                    else
                        return true;
                }
                break;
            case BuiltIn::ROR:
                if( a.size() == 2 )
                {
                    Type* t = a.first()->d_type.isNull() ? 0 : a.first()->d_type->derefed();
                    if( t == 0 ||
                            ( t != bt.d_intType &&
                              t != bt.d_byteType &&
                              t != bt.d_setType ) )
                        return error(e->d_loc, tr("INTEGER or SET first parameter expected") );
                    t = a.last()->d_type.isNull() ? 0 : a.last()->d_type->derefed();
                    if( t == 0 ||
                            ( t != bt.d_intType &&
                              t != bt.d_byteType ) )
                        return error(e->d_loc, tr("INTEGER second parameter expected") );
                    else
                        return true;
                }
                break;
            case BuiltIn::ADR:
                if( a.size() == 1 )
                {
                    Expression* leaf = getTail(a.first().data());
                    if( leaf == 0 && a.first()->getTag() != Thing::T_Literal &&
                            a.first()->d_type->derefed() != bt.d_stringType )
                         return error( a.first()->d_loc,
                                       tr("designator or string literal required as an actual parameter of ADR") );
                    else
                        return true;
                }
                break;
            case BuiltIn::VAL:
                if( a.size() == 2 )
                {
                    if( a.first()->getIdent() == 0 || a.first()->getIdent()->getTag() != Thing::T_NamedType )
                        return error(e->d_loc, tr("named type expected as first parameter of VAL") );
                    else
                        return true;
                }
                break;
            case BuiltIn::GET:
            case BuiltIn::PUT:
                if( a.size() == 2 )
                {
                    if( bi->d_func == BuiltIn::GET )
                        checkAssignableToVar( a.last().data() );
                    Type* t = a.first()->d_type.isNull() ? 0 : a.first()->d_type->derefed();
                    if( t == 0 ||
                            ( t != bt.d_intType &&
                              t != bt.d_byteType ) )
                        return error(e->d_loc, tr("INTEGER first parameter expected") );
                    t = a.last()->d_type.isNull() ? 0 : a.last()->d_type->derefed();
                    if( t == 0 ||
                            ( t->getTag() != Thing::T_BaseType
                              && t->getTag() != Thing::T_Pointer ) )
                        return error(e->d_loc, tr("basic type or pointer second parameter expected") );
                    else
                        return true;
                }
                break;
            default:
                break;
            }
        }
        if( p->d_formals.size() != a.size() )
            return error(e->d_loc,tr("number of actual parameters doesn't correspond to number of formal parameters") );
        for( int i = 0; i < p->d_formals.size(); i++ )
        {
            checkAssignableToType(p->d_formals[i]->d_type.data(), a[i].data(), p->d_formals[i]->d_var );
        }
        return true;
    }

    bool checkAssignableToVar( Expression* rightEx, Type** rightTPtr = 0)
    {
        Expression* leaf = getTail(rightEx);
        if( leaf == 0 )
            return error( rightEx->d_loc, tr("designator required as an actual VAR parameter") );
        Q_ASSERT( leaf->getIdent() );
        if( rightEx->getTag() == Thing::T_CallExpr  )
        {
            CallExpr* c = Ast::thing_cast<CallExpr*>(rightEx);
            if( c->d_sub->getIdent() && c->d_sub->getIdent()->getTag() == Thing::T_BuiltIn &&
                    Ast::thing_cast<BuiltIn*>( c->d_sub->getIdent() )->d_func == BuiltIn::VAL )
            {
                // NOTE: SYSTEM module is not supported by the code generators of this project and it's use is reported there
                if( c->d_actuals.size() != 2 || c->d_actuals.first()->d_type.isNull() )
                    return error( c->d_loc, tr("invalid use of SYSTEM.VAL") );
                if( rightTPtr )
                    *rightTPtr = c->d_actuals.first()->d_type->derefed();
            }else
                return error( rightEx->d_loc, tr("cannot use result of a function call as an actual VAR parameter") );
        }else if( leaf->getIdent()->getTag() == Thing::T_Import )
            error( leaf->d_loc, tr("cannot assign imported values to VAR parameters") );
        else if( leaf->getIdent()->getTag() == Thing::T_Const )
            error( leaf->d_loc, tr("cannot assign constants to VAR parameters") );
        else if( leaf->getIdent()->getTag() == Thing::T_Parameter )
        {
            Parameter* p = Ast::thing_cast<Parameter*>( leaf->getIdent() );
            Type* td = p->d_type->derefed();
            if( !p->d_var && ( td->getTag() == Thing::T_Array || td->getTag() == Thing::T_Record ) )
                error( leaf->d_loc, tr("cannot assign structured value to VAR parameter") );
        }
        return true;
    }

    bool checkAssignableToType( Type* leftT, Expression* rightEx, bool toVar )
    {
        if( leftT == 0 || rightEx == 0 ) // previous errors
            return false;

        static const char* s_msg1 = QT_TR_NOOP("incompatible types");
        static const char* s_msg2 = QT_TR_NOOP("number of characters in string must be less than the array");

        Q_ASSERT( leftT != 0 );
        leftT = leftT->derefed();

        Type* rightT = rightEx->d_type->derefed();
        if( rightT == 0 )
            return error( rightEx->d_loc, tr(s_msg1) );

        if( toVar )
            checkAssignableToVar(rightEx, &rightT);

        Q_ASSERT( rightT != 0 );

        if( isEqualType( leftT, rightT, false ) )
            return true;
        else if( leftT->getTag() == Thing::T_Record )
        {
            if( rightT->getTag() == Thing::T_Record && Model::isSubType( rightT, leftT ) )
                return true;
            else
                return false;
        }else if( leftT->getTag() == Thing::T_Pointer )
        {
            if( rightT == bt.d_nilType )
                return true;
            else if( rightT->getTag() == Thing::T_Pointer
                      && !toVar && Model::isSubType( rightT, leftT ) )
                return true;
            else if( rightT->getTag() == Thing::T_Pointer && toVar)
            {
                if( rightT == leftT )
                    return true;
                else
                    return error( rightEx->d_loc, tr("variable pointer parameter cannot be an extended type") );
            }
            else
                return error( rightEx->d_loc, tr(s_msg1) );
        }else if( leftT->getTag() == Thing::T_ProcType )
        {
            if( rightT == bt.d_nilType )
                return true;
            else if( rightT->getTag() == Thing::T_ProcType )
            {
                ProcType* lhs = Ast::thing_cast<ProcType*>(leftT );
                ProcType* rhs = Ast::thing_cast<ProcType*>(rightT );

                // If a formal parameter specifies a procedure type, then the corresponding actual parameter must be
                // either a procedure declared globally, or a variable (or parameter) of that procedure type. It cannot
                // be a predefined procedure.

                if( rightEx->getIdent() )
                {
                    if( rightEx->getIdent()->getTag() == Thing::T_Procedure &&
                            rightEx->getIdent()->d_scope->getTag() != Thing::T_Module )
                        return error( rightEx->d_loc, tr("assigning local procedure") );
                    if( rightEx->getIdent()->getTag() == Thing::T_BuiltIn )
                        return error( rightEx->d_loc, tr("assigning predefined procedure") );
                }

                if( !isEqualType( lhs->d_return.data(), rhs->d_return.data(), true ) )
                    return error( rightEx->d_loc, tr("incompatible return type") );
                if( lhs->d_formals.size() != rhs->d_formals.size() )
                    return error( rightEx->d_loc, tr("different number of formal parameter") );
                else
                {
                    for( int i = 0; i < lhs->d_formals.size(); i++ )
                    {
                        if( lhs->d_formals[i]->d_var != rhs->d_formals[i]->d_var ||
                                !isEqualType( lhs->d_formals[i]->d_type.data(), rhs->d_formals[i]->d_type.data(), true ) )
                            error( rightEx->d_loc, tr("formal parameter with incompatible type %1").arg(i+1) );
                    }
                }
            }else
                error( rightEx->d_loc, tr(s_msg1) );
        }else if( leftT->getTag() == Thing::T_Array )
        {
            Array* lhs = Ast::thing_cast<Array*>( leftT );
            Type* lt = lhs->d_type->derefed();
            if( lt == bt.d_byteType && toVar && !s_strict )
                return true; // Old Oberon rule, see https://inf.ethz.ch/personal/wirth/Oberon/Oberon.Report.pdf chap. 12
            else if( rightT->getTag() == Thing::T_Array )
            {
                Array* rhs = Ast::thing_cast<Array*>( rightT );
                if( isEqualType( lt, rhs->d_type.data(), true ) )
                {
                    if( lhs->d_len == 0 )
                    {
                        // Ok; the parameter is said to be an open array, and the
                        // corresponding actual parameter may be of arbitrary length.
                    }else if( lhs->d_len == rhs->d_len )
                    {
                        // OK
                        Q_ASSERT( false ); // already handeled above
                    }else if( rhs->d_len == 0 )
                    {
                        // OK, runtime check!
                    }else
                        error( rightEx->d_loc, tr("arrays with different lengths") );
                }else
                    error( rightEx->d_loc, tr(s_msg1) );
            }else if( lt == bt.d_charType )
            {
                // LHS is string
                if( rightT == bt.d_stringType )
                {
                    if( lhs->d_len == 0 )
                        // OK, as it is done many times in Oberon System
                        return true;
                    else if( rightEx->getTag() == Thing::T_Literal )
                    {
                        if( Ast::thing_cast<Literal*>(rightEx)->d_val.toByteArray().size() >= lhs->d_len )
                            error( rightEx->d_loc, tr(s_msg2) );
                    }else if( rightEx->getTag() == Thing::T_IdentLeaf )
                    {
                        Q_ASSERT( rightEx->getIdent() && rightEx->getIdent()->getTag() == Thing::T_Const );
                        if( Ast::thing_cast<Const*>(rightEx->getIdent())->d_val.toByteArray().size() >= lhs->d_len )
                            error( rightEx->d_loc, tr(s_msg2) );
                    }else
                        Q_ASSERT( false );
                    // else ok
                }else if( rightT == bt.d_charType )
                {
                    if( lhs->d_len == 1 )
                        return error( rightEx->d_loc, tr(s_msg2) );
                    // else ok
                }else
                    return error( rightEx->d_loc, tr(s_msg1) );
            }else
                return error( rightEx->d_loc, tr(s_msg1) );
        }else if( leftT == bt.d_charType && rightT == bt.d_stringType )
        {
            if( rightEx->getTag() == Thing::T_IdentLeaf )
            {
                Q_ASSERT( rightEx->getIdent() && rightEx->getIdent()->getTag() == Thing::T_Const );
                if( Ast::thing_cast<Const*>(rightEx->getIdent())->d_val.toByteArray().size() > 1 )
                    return error( rightEx->d_loc, tr("only single-character strings can be assigned to CHAR") );
            }else
            {
                Q_ASSERT( rightEx->getTag() == Thing::T_Literal );
                if( Ast::thing_cast<Literal*>(rightEx)->d_val.toByteArray().size() > 1 )
                    return error( rightEx->d_loc, tr("only single-character strings can be assigned to CHAR") );
            }
            // else ok
        }else if( leftT == bt.d_intType && rightT == bt.d_byteType )
            return true;
        else if( leftT == bt.d_byteType && rightT == bt.d_intType )
            return true; // ok, runtime check!
        else if( !s_strict && leftT == bt.d_setType && rightT == bt.d_intType )
            return true;
        else if( !s_strict && leftT == bt.d_realType && rightT == bt.d_intType )
            return true;
        else
            return error( rightEx->d_loc, tr(s_msg1));
        return true;
    }
};

bool Validator::validate(Model* mdl, Module* m, Errors* err)
{
    Q_ASSERT( mdl != 0 && m != 0 && err != 0 );

    if( m->d_hasErrors )
        return false;

    const quint32 errCount = err->getErrCount();

    ValidatorImp imp;
    imp.err = err;
    imp.bt = mdl->getBaseTypes();
    imp.mod = m;
    m->accept(&imp);

    m->d_hasErrors = ( err->getErrCount() - errCount ) != 0;

    return !m->d_hasErrors;
}
