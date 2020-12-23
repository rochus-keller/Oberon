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

#include "ObxParser.h"
#include "ObErrors.h"
#include <QtDebug>
using namespace Obx;
using namespace Ob;

#define MATCH( tok, msg ) \
    if( d_la == tok ){ \
        next(); \
    }else { \
        syntaxError( msg ); \
        d_cur = Token(); \
    }

struct MaximumErrorCountExceeded {};
struct PrematureEndFound {};

Parser::Parser(Ob::Lexer* l, Ob::Errors* e, QObject *parent) : QObject(parent),d_lex(l),d_errs(e),
    d_errCount(0), d_sync(false)
{

}

Ref<Module> Parser::parse()
{
    d_errCount = 0;
    next();
    try
    {
        switch( d_la )
        {
        case Tok_MODULE:
            next();
            module(false);
            break;
        case Tok_DEFINITION:
            next();
            module(true);
            break;
        default:
            syntaxError( tr("expecting MODULE or DEFINITION keyword"));
            break;
        }
        if( peek(1) != Tok_Eof )
            d_errs->warning( Errors::Syntax, d_next.toLoc(), tr("ignoring text after module") );
    }catch( const MaximumErrorCountExceeded& )
    {
        if( !d_mod.isNull() )
            d_mod->d_hasErrors = true;
        d_errs->error( Errors::Syntax, d_cur.toLoc(), tr("maximum number of errors exceeded, stop parsing") );
    }catch( const PrematureEndFound& )
    {
        if( !d_mod.isNull() )
            d_mod->d_hasErrors = true;
        d_errs->error( Errors::Syntax, d_next.toLoc(), tr("unexpected end of file found") );
    }

    return d_mod;
}

bool Parser::module(bool definition )
{
    Ref<Module> m = new Module();
    d_mod = m;
    m->d_isDef = definition;
    MATCH( Tok_ident, tr("expecting module name") );
    m->d_hasErrors = !d_cur.isValid();
    m->d_name = d_cur.d_val;
    m->d_loc = d_cur.toRowCol();
    m->d_file = d_cur.d_sourcePath;
    if( d_la == Tok_Semi )
        next();
    if( d_la == Tok_IMPORT )
        importList();
    declarationSequence(false, m.data() );
    if( d_la == Tok_BEGIN || d_la == Tok_DO )
    {
        next();
        if( definition )
            semanticError( d_cur.toLoc(), tr("There is no statement sequence in a DEFINITION module") );
        m->d_body = statementSequence(m.data());
    }
    MATCH( Tok_END, tr("expecting END keyword at the end of the module") );
    MATCH( Tok_ident, tr("expecting module name after END keyword") );
    if( d_cur.isValid() && d_cur.d_val.constData() != m->d_name.constData() )
        semanticError( d_next.toLoc(), tr("the ident '%1' after the END keyword must be equal "
                          "to the module name").arg(d_next.d_val.constData()));
    else
        m->d_end = d_cur.toRowCol();
    if( d_la == Tok_Dot )
    {
        next();
        return true;
    }else
        return false;
}

Ref<Literal> Parser::number()
{
    QVariant val;
    switch( d_la )
    {
    case Tok_integer:
        next();
        if( d_cur.d_val.endsWith('H') )
            val = d_cur.d_val.left(d_cur.d_val.size()-1).toLongLong(0,16);
        else
            val = d_cur.d_val.toLongLong();
        return new Literal( Literal::Integer, d_cur.toRowCol(),val);
    case Tok_real:
        next();
        val = d_cur.d_val.toDouble();
        return new Literal( Literal::Real, d_cur.toRowCol(),val);
        break;
    default:
        syntaxError( tr("expecting a number") );
        return 0;
    }
}

Ref<Expression> Parser::qualident()
{
    // Just build the AST and record the names to be resolved; they will be resolved in the second pass
    Ref<Expression> cur;
    if( d_la == Tok_ident && peek(2) == Tok_Dot )
    {
        next(); // ident
        Ref<IdentLeaf> id = new IdentLeaf();
        id->d_name = d_cur.d_val;
        id->d_loc = d_cur.toRowCol();
        cur = id.data();
        next(); // dot
    }
    MATCH( Tok_ident, tr("expecting an identifier") );
    if( cur.isNull() )
    {
        Ref<IdentLeaf> id = new IdentLeaf();
        id->d_name = d_cur.d_val;
        id->d_loc = d_cur.toRowCol();
        cur = id.data();
    }else
    {
        Ref<IdentSel> id = new IdentSel();
        id->d_sub = cur.data();
        id->d_name = d_cur.d_val;
        id->d_loc = d_cur.toRowCol();
        cur = id.data();
    }
    return cur;
}

void Parser::identdef(Named* n, Scope* scope)
{
    MATCH( Tok_ident, tr("expecting an identifier") );
    n->d_loc = d_cur.toRowCol();
    n->d_name = d_cur.d_val;
    if( d_la == Tok_Star )
    {
        next();
        if( scope->getTag() != Thing::T_Procedure )
            n->d_visibility = Named::ReadWrite;
        else
            semanticError(d_cur.toLoc(), tr("export mark only allowed on module level"));
    }else if( d_la == Tok_Minus )
    {
        next();
        if( scope->getTag() == Thing::T_Procedure )
            n->d_visibility = Named::ReadOnly;
        else
            semanticError(d_cur.toLoc(), tr("export mark only allowed on module level"));
    }else if( scope->getTag() == Thing::T_Procedure )
        n->d_visibility = Named::NotApplicable;
}

void Parser::constDeclaration(Scope* scope)
{
    Ref<Const> res = new Const();
    identdef(res.data(), scope );
    MATCH( Tok_Eq, tr("expecting '=' after ident in constant declaration") );
    res->d_constExpr = constExpression();
    // this is done in Obx::Validator:
    // if( !res->d_constExpr.isNull() ) // prev errors
    //   res->d_type = res->d_constExpr->d_type.data();
    // res->d_val = eval( res->d_constExpr.data(), true );
    if( !scope->add( res.data() ) )
        semanticError( res->d_loc, tr("constant name is not unique") );
}

Ref<Expression> Parser::constExpression()
{
    return expression();
}

static inline quint8 relation( quint16 t )
{
    switch( t )
    {
    case Tok_Eq:
        return BinExpr::EQ;
    case Tok_Hash:
        return BinExpr::NEQ;
    case Tok_Lt:
        return BinExpr::LT;
    case Tok_Leq:
        return BinExpr::LEQ;
    case Tok_Gt:
        return BinExpr::GT;
    case Tok_Geq:
        return BinExpr::GEQ;
    case Tok_IN:
        return BinExpr::IN;
    case Tok_IS:
        return BinExpr::IS;
    }
    return BinExpr::Invalid;
}

Ref<Expression> Parser::expression()
{
    Ref<Expression> lhs = simpleExpression();
    if( relation(d_la) )
    {
        Ref<BinExpr> res = new BinExpr();
        next();
        res->d_op = relation(d_cur.d_type);
        Q_ASSERT( res->d_op );
        res->d_loc = d_cur.toRowCol();
        //res->d_type = boolType();
        res->d_lhs = lhs;
        res->d_rhs = simpleExpression();
        lhs = res.data();
    }
    return lhs;
}

Ref<NamedType> Parser::typeDeclaration(Scope* scope)
{
    Ref<NamedType> nt = new NamedType();

    identdef(nt.data(),scope);
    if( d_la == Tok_Lt )
    {
        nt->d_metaParams = typeParams();
        for( int i = 0; i < nt->d_metaParams.size(); i++ )
        {
            if( !nt->add( nt->d_metaParams[i].data() ) )
                semanticError(nt->d_metaParams[i]->d_loc,tr("name of type parameter must be unique"));
        }
    }
    MATCH( Tok_Eq, tr("expecting '=' after ident in type declaration") );

    if( !scope->add(nt.data()) )
        semanticError(nt->d_loc,tr("name of type declaration must be unique"));

    nt->d_type = type(scope, nt.data());

    if( !nt->d_type.isNull() )
    {
        if( nt->d_type->isSelfRef() )
            semanticError(nt->d_loc,tr("recursive type definition"));

        // NOTE: check SelfRefs when type is set. If type is pointer or procedure then there is only
        // an issue when X = POINTER TO X, but not X = POINTER TO RECORD x: X; END.
        // If type is not a pointer, then X = RECORD x: X; END is an error.
        // The following cases are already checked: X = X or X = RECORD(X)
    }
    return nt;
}

MetaParams Parser::typeParams()
{
    MATCH( Tok_Lt, tr("expecting '<' to start type parameters") );
    MATCH( Tok_ident, tr("at least one identifier required as type parameter") );
    MetaParams res;
    Ref<GenericName> t = new GenericName();
    t->d_name = d_cur.d_val;
    t->d_loc = d_cur.toRowCol();
    res << t;
    while( d_la == Tok_Comma )
    {
        next();
        MATCH( Tok_ident, tr("identifier expected in type parameter list") );
        Ref<GenericName> t = new GenericName();
        t->d_name = d_cur.d_val;
        t->d_loc = d_cur.toRowCol();
        res << t;
    }
    MATCH( Tok_Gt, tr("expecting '>' to end type parameters") );
    return res;
}

Ref<Type> Parser::type(Scope* scope, Named* id, Pointer* binding)
{
    static const TokSet toks = TokSet() << Tok_Lpar << Tok_Lbrack << Tok_Hat << Tok_ARRAY
                                  << Tok_POINTER << Tok_PROC << Tok_PROCEDURE << Tok_RECORD << Tok_ident;
    if( d_sync )
        sync(toks);

    switch( d_la )
    {
    case Tok_ident:
        return namedType(id).data();
    case Tok_Lpar:
        return enumeration(scope,id);
    case Tok_ARRAY:
    case Tok_Lbrack:
        return arrayType(scope, id);
    case Tok_RECORD:
        return recordType(scope,id, binding);
    case Tok_POINTER:
    case Tok_Hat:
        return pointerType(scope, id);
    case Tok_PROCEDURE:
    case Tok_PROC:
        return procedureType(scope, id);
    default:
        syntaxError( tr("expecting type"));
        break;
    }
    return 0;
}

MetaActuals Parser::typeActuals()
{
    MATCH( Tok_Lt, tr("expecting '<' to start type actuals") );
    MetaActuals res;
    Ref<Thing> t = typeActual();
    if( !t.isNull() )
        res << t;
    while( d_la == Tok_Comma )
    {
        next();
        t = typeActual();
        if( !t.isNull() )
            res << t;
    }
    MATCH( Tok_Gt, tr("expecting '>' to end type actuals") );
    return res;
}

void Parser::addEnum( Scope* scope, Enumeration* e, const Token& t )
{
    if( !t.isValid() )
        return;
    if( scope->find( t.d_val ) )
    {
        semanticError( t.toRowCol(), tr("name of enumeration symbol must be unique in scope") );
    }else
    {
        Ref<Const> c = new Const();
        c->d_val = e->d_items.count();
        c->d_name = t.d_val;
        c->d_loc = t.toRowCol();
        c->d_type = e;
        e->d_items.append(c);
    }
}

Ref<Type> Parser::enumeration(Scope* scope,Named* id)
{
    Ref<Enumeration> e = new Enumeration();
    e->d_ident = id;
    MATCH( Tok_Lpar, tr("expecting '(' to start enumeration") );
    e->d_loc = d_cur.toRowCol();
    MATCH( Tok_ident, tr("at least one identifier required in enumeration") );
    addEnum( scope, e.data(), d_cur );
    while( d_la == Tok_Comma )
    {
        next();
        MATCH( Tok_ident, tr("identifier expected after ',' in enumeration") );
        addEnum( scope, e.data(), d_cur );
    }
    MATCH( Tok_Rpar, tr("expecting ')' to end enumeration") );
    return e.data();
}

Ref<QualiType> Parser::namedType(Named* id)
{
    Ref<Expression> e = qualident();
    if( e.isNull() )
        return 0; // error was already reported

    Ref<QualiType> q = new QualiType();
    q->d_quali = e;
    q->d_ident = id;
    q->d_loc = e->d_loc;
    if( d_la == Tok_Lt )
    {
        q->d_metaActuals = typeActuals();
    }

    return q.data();
}

Ref<Type> Parser::arrayType(Scope* scope, Named* id)
{
    Ref<Array> res = new Array();
    res->d_ident = id;
    QList< Ref<Expression> > dims;
    if( d_la == Tok_ARRAY )
    {
        MATCH( Tok_ARRAY, tr("expecting the ARRAY keyword") );
        res->d_loc = d_cur.toRowCol();
        ofrontTag();
        if( d_la != Tok_OF )
        {
            dims = lengthList();
        }
        MATCH( Tok_OF, tr("expecting OF after ARRAY keyword or length list") );
    }else
    {
        MATCH( Tok_Lbrack, tr("expecting '['") );
        res->d_loc = d_cur.toRowCol();
        if( d_la != Tok_Rbrack )
        {
            dims = lengthList();
        }
        MATCH( Tok_Rbrack, tr("expecting ']'") );
    }

    Ref<Type> t = type(scope,0);
    res->d_type = t;

    if( !dims.isEmpty() )
    {
        res->d_lenExpr = dims[0];
        Ref<Array> last = res;
        for( int i = 1; i < dims.size(); i++ )
        {
            Ref<Array> cur = new Array();
            cur->d_type = t;
            cur->d_lenExpr = dims[i];
            last->d_type = cur.data();
            last = cur;
        }
    }

    return res.data();
}

Ref<Type> Parser::recordType(Scope* scope, Named* id, Pointer* binding)
{
    Ref<Record> res = new Record();
    res->d_ident = id;
    MATCH( Tok_RECORD, tr("expecting the RECORD keyword") );
    res->d_loc = d_cur.toRowCol();
    ofrontTag();
    if( d_la == Tok_Lpar )
    {
        next();
        res->d_base = baseType();
        MATCH( Tok_Rpar, tr("expecting ')' after record base type") );
    }
    if( d_la != Tok_END )
        fieldListSequence( scope, res.data() );
    MATCH( Tok_END, tr("expecting END in record type") );
    return res.data();
}

Ref<Type> Parser::pointerType(Scope* scope, Named* id)
{
    Ref<Pointer> p = new Pointer();
    p->d_ident = id;
    if( d_la == Tok_Hat )
    {
        next();
        p->d_loc = d_cur.toRowCol();
    }else
    {
        MATCH( Tok_POINTER, tr("expecting the POINTER keyword") );
        p->d_loc = d_cur.toRowCol();
        ofrontTag();
        MATCH( Tok_TO, tr("expecting the TO keyword after the POINTER keyword") );
    }
    p->d_to = type(scope, 0, p.data() );
    return p.data();
}

Ref<Type> Parser::procedureType(Scope* scope, Named* id)
{
    Ref<ProcType> p = new ProcType();
    p->d_ident = id;
    if( d_la == Tok_PROC )
    {
        MATCH( Tok_PROC, tr("expecting the PROCEDURE keyword") );
    }else
    {
        MATCH( Tok_PROCEDURE, tr("expecting the PROCEDURE keyword") );
    }
    p->d_loc = d_cur.toRowCol();
    if( d_la == Tok_Lpar )
    {
        formalParameters(scope, p.data());
    }
    return p.data();
}

Ref<Thing> Parser::typeActual()
{
    switch( d_la )
    {
    case Tok_ident:
        return namedType(0).data();

    case Tok_integer:
    case Tok_real:
    case Tok_string:
    case Tok_hexstring:
    case Tok_hexchar:
    case Tok_NIL:
    case Tok_TRUE:
    case Tok_FALSE:
    case Tok_Lbrace:
        return literal().data();

    default:
        syntaxError(tr("expecting type or literal as actual type parameters"));
        break;
    }
    return 0;
}

Ref<Expression> Parser::literal()
{
    switch( d_la )
    {
    case Tok_integer:
    case Tok_real:
        return number().data();
    case Tok_string:
        next();
        return new Literal(Literal::String, d_cur.toRowCol(),d_cur.d_val.mid(1,d_cur.d_val.size()-2));
    case Tok_hexstring:
        next();
        return new Literal(Literal::String, d_cur.toRowCol(), QByteArray::fromHex( d_cur.d_val.mid(1, d_cur.d_val.size() - 2)));
    case Tok_hexchar:
        next();
        return new Literal( Literal::Char, d_cur.toRowCol(), QByteArray::fromHex( d_cur.d_val.left( d_cur.d_val.size() - 1 ) ));
    case Tok_NIL:
        next();
        return new Literal( Literal::Nil, d_cur.toRowCol());
    case Tok_TRUE:
        next();
        return new Literal( Literal::Boolean, d_cur.toRowCol(), true);
    case Tok_FALSE:
        next();
        return new Literal( Literal::Boolean, d_cur.toRowCol(), false);
    case Tok_Lbrace:
        return set();
    default:
        syntaxError(tr("invalid literal"));
        break;
    }
    return 0;
}

QList<Ref<Expression> > Parser::lengthList()
{
    QList<Ref<Expression> > res;
    Ref<Expression> e = length();
    if( !e.isNull() )
        res << e;
    while( d_la == Tok_Comma )
    {
        next();
        Ref<Expression> e = length();
        if( !e.isNull() )
            res << e;
    }
    return res;
}

Ref<Expression> Parser::length()
{
    return constExpression();
}

Ref<QualiType> Parser::baseType()
{
    return namedType(0);
}

void Parser::fieldListSequence(Scope* scope, Record* r)
{
    while( d_la == Tok_Semi )
    {
        next();
    }
#ifdef OBN07
    fieldList(scope, r);
    if( d_la == Tok_Semi )
    {
        next();
    }
#endif
    while( d_la == Tok_ident )
    {
        fieldList(scope, r);
        if( d_la == Tok_Semi )
        {
            next();
        }
    }
}

void Parser::fieldList(Scope* scope, Record* r)
{
    QList<Ref<Field> > fields;
    Field* f = new Field();
    f->d_scope = scope;
    fields << f;
    identdef(f,scope);
    while( d_la == Tok_Comma )
    {
        next();
        f = new Field();
        f->d_scope = scope;
        fields << f;
        identdef(fields.back().data(),scope);
    }
    MATCH( Tok_Colon, tr("expecting ':' between identifier list and type in field list") );
    Ref<Type> t = type(scope,0);
    for( int i = 0; i < fields.size(); i++ )
    {
        fields[i]->d_type = t;
        if( r->find( fields[i]->d_name, false ) )
            semanticError( fields[i]->d_loc, tr("field name is not unique in record"));
        else
        {
            r->d_fields << fields[i];
            r->d_names[ fields[i]->d_name.constData() ] = fields[i].data();
        }
    }
}

void Parser::formalParameters(Scope* scope, ProcType* p)
{
    MATCH( Tok_Lpar, tr("expecting '(' to start formal parameter list") );
    if( d_la != Tok_Rpar )
    {
        bool ok = fPSection(scope, p);
        while( ok && d_la != Tok_Rpar )
        {
            if( d_la == Tok_Semi )
                next();
            ok = fPSection(scope, p);
        }
    }
    MATCH( Tok_Rpar, tr("expecting ')' at the end of the formal parameter list") );
    if( d_la == Tok_Colon )
    {
        next();
        p->d_return = namedType(0).data();
    }
}

void Parser::variableDeclaration(Scope* scope)
{
    QList<Ref<Variable> > vars;
    vars << new Variable();
    identdef(vars.back().data(),scope);
    while( d_la == Tok_Comma )
    {
        next();
        vars << new Variable();
        identdef(vars.back().data(),scope);
    }
    MATCH( Tok_Colon, tr("expecting ':' between identifier list and type in field list") );
    Ref<Type> t = type(scope,0);
    for( int i = 0; i < vars.size(); i++ )
    {
        vars[i]->d_type = t;
        if( !scope->add( vars[i].data() ) )
            semanticError( vars[i]->d_loc, tr("duplicate variable name"));
    }
}

Ref<Expression> Parser::designator()
{
    // a.b.c.d is represented as
    // exp(d)->exp(c)->exp(b)->exp(a)
    Ref<Expression> e = qualident();
    while( d_la == Tok_Dot || d_la == Tok_Lbrack || d_la == Tok_Hat || d_la == Tok_Lpar )
    {
        Ref<UnExpr> u = selector();
        if( !u.isNull() )
        {
            u->d_sub = e;
            e = u.data();
        }
    }
    return e;
}

Ref<UnExpr> Parser::selector()
{
    switch( d_la )
    {
    case Tok_Dot:
        next();
        MATCH( Tok_ident, tr("expecting ident after '.' in selector") );
        if( d_cur.isValid() )
        {
            Ref<IdentSel> id = new IdentSel();
            id->d_name = d_cur.d_val;
            id->d_loc = d_cur.toRowCol();
            return id.data();
        }
        break;
    case Tok_Lbrack:
        {
            next();
            Ref<ArgExpr> a = new ArgExpr();
            a->d_loc = d_cur.toRowCol();
            a->d_args = expList();
            a->d_op = UnExpr::IDX;
            MATCH( Tok_Rbrack, tr("expecting ']' to terminate expression list in index") );
            return a.data();
        }
        break;
    case Tok_Hat:
        {
            next();
            Ref<UnExpr> deref = new UnExpr();
            deref->d_op = UnExpr::DEREF;
            deref->d_loc = d_cur.toRowCol();
            return deref.data();
        }
        break;
    case Tok_Lpar:
        {
            next();
            Ref<ArgExpr> a = new ArgExpr();
            a->d_op = UnExpr::CALL; // provisorical, could become CAST
            a->d_loc = d_cur.toRowCol();
            if( d_la != Tok_Rpar )
                a->d_args = expList();
            MATCH( Tok_Rpar, tr("expecting ')' to terminate typeguard or actual parameter list") );
            return a.data();
        }
        break;
    default:
        syntaxError( tr("invalid selector") );
        break;
    }
    return 0;
}

ExpList Parser::expList()
{
    ExpList res;
    Ref<Expression> e = expression();
    if( !e.isNull() )
        res << e;
    while( d_la == Tok_Comma )
    {
        next();
        e = expression();
        if( !e.isNull() )
            res << e;
    }
    return res;
}

Ref<Expression> Parser::simpleExpression()
{
    Token minus;
    if( d_la == Tok_Plus || d_la == Tok_Minus )
    {
        next();
        minus = d_cur;
    }

    Ref<Expression> lhs = term();
    if( lhs.isNull() )
        return 0;

    if( minus.d_type == Tok_Minus ) // minus associates with the first lhs! -(5 + 6) = 11, -5 + 6 = 1
    {
        Ref<UnExpr> u = new UnExpr();
        u->d_op = UnExpr::NEG;
        u->d_loc = minus.toRowCol();
        u->d_sub = lhs;
        lhs = u.data();
    }

    while( d_la == Tok_Plus || d_la == Tok_Minus || d_la == Tok_OR )
    {
        next();
        Token op = d_cur;

        Ref<Expression> rhs = term();
        if( rhs.isNull() )
            return 0;

        Ref<BinExpr> res = new BinExpr();
        res->d_lhs = lhs;
        res->d_rhs = rhs;

        switch( op.d_type )
        {
        case Tok_Plus:
            res->d_op = BinExpr::ADD;
            break;
        case Tok_Minus:
            res->d_op = BinExpr::SUB;
            break;
        case Tok_OR:
            res->d_op = BinExpr::OR;
            break;
        default:
            Q_ASSERT( false );
        }
        res->d_loc = op.toRowCol();
        lhs = res.data();
    }
    return lhs;
}

Ref<Expression> Parser::term()
{
    Ref<Expression> lhs = factor();
    if( lhs.isNull() )
        return 0;

    while( d_la == Tok_Star || d_la == Tok_Slash || d_la == Tok_DIV || d_la == Tok_MOD || d_la == Tok_Amp )
    {
        next();
        Token op = d_cur;

        Ref<Expression> rhs = factor();
        if( rhs.isNull() )
            return 0;

        Ref<BinExpr> res = new BinExpr();
        res->d_lhs = lhs;
        res->d_rhs = rhs;

        switch( op.d_type )
        {
        case Tok_Star:
            res->d_op = BinExpr::MUL;
            break;
        case Tok_Slash:
            res->d_op = BinExpr::FDIV;
            break;
        case Tok_DIV:
            res->d_op = BinExpr::DIV;
            break;
        case Tok_MOD:
            res->d_op = BinExpr::MOD;
            break;
        case Tok_Amp:
            res->d_op = BinExpr::AND;
            break;
        default:
            Q_ASSERT( false );
        }

        res->d_loc = op.toRowCol();
        lhs = res.data();
    }
    return lhs;
}

Ref<Expression> Parser::factor()
{
    static const TokSet toks = TokSet() << Tok_Lpar << Tok_Lbrace << Tok_Tilde << Tok_FALSE << Tok_NIL <<
                                     Tok_TRUE << Tok_ident << Tok_integer << Tok_real <<
                                     Tok_string << Tok_hexchar << Tok_hexstring;
    if( d_sync )
        sync(toks);

    switch( d_la )
    {
    case Tok_Lbrace:
    case Tok_FALSE:
    case Tok_NIL:
    case Tok_TRUE:
    case Tok_integer:
    case Tok_real:
    case Tok_string:
    case Tok_hexchar:
    case Tok_hexstring:
        return literal();

    case Tok_ident:
        return variableOrFunctionCall();

    case Tok_Lpar:
        {
            next();
            Ref<Expression> e = expression();
            if( e.isNull() )
                return 0;
            MATCH( Tok_Rpar, tr("expecting ')' to terminate sub-expression") );
            return e;
        }
        break;

    case Tok_Tilde:
        {
            next();
            Token tilde = d_cur;
            Ref<Expression> f = factor();
            if( f.isNull() )
                return 0;
            Ref<Expression> res = new UnExpr(UnExpr::NOT,f.data() );
            res->d_loc = tilde.toRowCol();
            return res;
        }
        break;

    default:
        syntaxError( tr("invalid factor") );
        break;
    }
    return 0;
}

Ref<Expression> Parser::set()
{
    MATCH( Tok_Lbrace, tr("expecting '{' to start set literal") );

    Ref<SetExpr> set = new SetExpr();
    set->d_loc = d_cur.toRowCol();
    // set->d_type = setType();

    if( d_la != Tok_Rbrace )
    {
        Ref<Expression> e = element();
        if( !e.isNull() )
            set->d_parts << e;
        while( d_la == Tok_Comma )
        {
            next();
            e = element();
            if( !e.isNull() )
                set->d_parts << e;
        }
    }
    MATCH( Tok_Rbrace, tr("expecting '}' to end set literal") );
    return set.data();
}

Ref<Expression> Parser::variableOrFunctionCall()
{
    return designator();
}

Ref<Expression> Parser::element()
{
    Ref<Expression> lhs = expression();
    if( d_la == Tok_2Dot )
    {
        next();
        Ref<BinExpr> range = new BinExpr();
        range->d_op = BinExpr::Range;
        range->d_loc = d_cur.toRowCol();
        range->d_lhs = lhs;
        range->d_rhs = expression();
        lhs = range.data();
    }
    return lhs;
}

Ref<Statement> Parser::statement(Scope* scope)
{
    switch( d_la )
    {
    case Tok_ident:
        return assignmentOrProcedureCall();
    case Tok_IF:
        return ifStatement(scope);
    case Tok_CASE:
        return caseStatement(scope);
    case Tok_WHILE:
        return whileStatement(scope);
    case Tok_REPEAT:
        return repeatStatement(scope);
    case Tok_FOR:
        return forStatement(scope);
    case Tok_WITH:
        return withStatement(scope);
#ifndef OBN07
    case Tok_RETURN:
        return returnStatement(scope); // OBN2
#endif
    case Tok_LOOP:
        return loopStatement(scope);
    case Tok_EXIT:
        return exitStatement(scope);
    default:
        syntaxError( tr( "invalid statement" ) );
        break;
    }
    return 0;
}

Ref<Statement> Parser::assignmentOrProcedureCall()
{
    Ref<Expression> lhs = designator();
    if( d_la == Tok_ColonEq )
    {
        next();
        Ref<Assign> a = new Assign();
        a->d_loc = d_cur.toRowCol();
        a->d_lhs = lhs;
        a->d_rhs = expression();
        return a.data();
    }else
    {
        Ref<Call> c = new Call();
        if( !lhs.isNull() )
            c->d_loc = lhs->d_loc;
        c->d_what = lhs;
        return c.data();
    }
}

Ref<Statement> Parser::ifStatement(Scope* scope)
{
    MATCH( Tok_IF, tr("expecting the IF keyword") );
    Ref<IfLoop> c = new IfLoop();
    c->d_op = IfLoop::IF;
    c->d_loc = d_cur.toRowCol();
    c->d_if << expression();
    MATCH( Tok_THEN, tr("expecting the THEN keyword after the IF expression") );
    c->d_then.append( statementSequence(scope) );
    while( d_la == Tok_ELSIF )
    {
        StatSeq seq;
        Ref<Expression> e = elsifStatement(scope, false, seq);
        if( !e.isNull() )
        {
            c->d_if << e;
            c->d_then.append(seq);
        }
    }
    if( d_la == Tok_ELSE )
    {
        elseStatement(scope, c->d_else);
    }
    MATCH( Tok_END, tr("expecting a closing END in an IF statement") );
    return c.data();
}

Ref<Statement> Parser::caseStatement(Scope* scope)
{
    MATCH( Tok_CASE, tr("expecting the CASE keyword") );
    Ref<CaseStmt> s = new CaseStmt();
    s->d_loc = d_cur.toRowCol();
    s->d_exp = expression();
    MATCH( Tok_OF, tr("expecting the OF keyword after the CASE expression") );
    CaseStmt::Case c;
    if( case_(scope, c) )
        s->d_cases << c;
    while( d_la == Tok_Bar )
    {
        next();
        if( case_(scope, c) )
            s->d_cases << c;
    }
    if( d_la == Tok_ELSE )
    {
        StatSeq seq;
        elseStatement(scope,seq);
        s->d_else = seq;
    }
    MATCH( Tok_END, tr("expecting a closing END in a CASE statement") );
    return s.data();
}

Ref<Statement> Parser::whileStatement(Scope* scope)
{
    MATCH( Tok_WHILE, tr("expecting the WHILE keyword") );
    Ref<IfLoop> s = new IfLoop();
    s->d_op = IfLoop::WHILE;
    s->d_loc = d_cur.toRowCol();
    s->d_if << expression();
    MATCH( Tok_DO, tr("expecting the DO keyword after the WHILE expression") );    
    s->d_then.append( statementSequence(scope) );
    while( d_la == Tok_ELSIF )
    {
        StatSeq seq;
        Ref<Expression> e = elsifStatement(scope, true,seq);
        if( !e.isNull() )
        {
            s->d_if << e;
            s->d_then.append(seq);
        }
    }
    MATCH( Tok_END, tr("expecting a closing END in an WHILE statement") );
    return s.data();
}

Ref<Statement> Parser::repeatStatement(Scope* scope)
{
    MATCH( Tok_REPEAT, tr("expecting the REPEAT keyword") );
    Ref<IfLoop> s = new IfLoop();
    s->d_op = IfLoop::REPEAT;
    s->d_loc = d_cur.toRowCol();
    s->d_then.append(statementSequence(scope));
    MATCH( Tok_UNTIL, tr("expecting the UNTIL keyword in a REPEAT statement") );
    s->d_if << expression();
    return s.data();
}

Ref<Statement> Parser::forStatement(Scope* scope)
{
    MATCH( Tok_FOR, tr("expecting the FOR keyword") );
    Ref<ForLoop> f = new ForLoop();
    f->d_loc = d_cur.toRowCol();
    MATCH( Tok_ident, tr("expecting an identifier after the FOR keyword") );
    Ref<IdentLeaf> id = new IdentLeaf();
    id->d_loc = d_cur.toRowCol();
    id->d_name = d_cur.d_val;
    f->d_id = id.data();
    MATCH( Tok_ColonEq, tr("expecting ':=' to assign the start value of the FOR statement") );
    f->d_from = expression();
    MATCH( Tok_TO, tr("expecting the TO keyword") );
    f->d_to = expression();
    if( d_la == Tok_BY )
    {
        next();
        f->d_by = constExpression();
    }else
    {
        Ref<Literal> one = new Literal( Literal::Integer, d_next.toRowCol(), 1);
        f->d_by = one.data();
    }
    MATCH( Tok_DO, tr("expecting the DO keyword") );
    f->d_do = statementSequence(scope);
    MATCH( Tok_END, tr("expecting a closing END in an FOR statement") );
    return f.data();
}

Ref<Statement> Parser::withStatement(Scope* scope)
{
    MATCH( Tok_WITH, tr("expecting the WITH keyword") );

    Ref<IfLoop> c = new IfLoop();
    c->d_op = IfLoop::WITH;
    c->d_loc = d_cur.toRowCol();
    c->d_if << guard();
    MATCH( Tok_DO, tr("expecting the DO keyword after the WITH expression") );
    c->d_then.append( statementSequence(scope) );
    while( d_la == Tok_Bar )
    {
        next();
        c->d_if << guard();
        MATCH( Tok_DO, tr("expecting the DO keyword after the '|' expression") );
        c->d_then.append( statementSequence(scope) );
    }
    if( d_la == Tok_ELSE )
    {
        elseStatement(scope,c->d_else);
    }

    MATCH( Tok_END, tr("expecting a closing END in a WITH statement") );
    return c.data();
}

Ref<Statement> Parser::loopStatement(Scope* scope)
{
    MATCH( Tok_LOOP, tr("expecting the LOOP keyword") );
    Ref<IfLoop> c = new IfLoop();
    c->d_op = IfLoop::LOOP;
    c->d_loc = d_cur.toRowCol();
    c->d_then.append( statementSequence(scope) );
    MATCH( Tok_END, tr("expecting a closing END in a LOOP statement") );
    return c.data();
}

Ref<Statement> Parser::exitStatement(Scope* scope)
{
    MATCH( Tok_EXIT, tr("expecting the EXIT keyword") );
    Ref<Exit> r = new Exit();
    r->d_loc = d_cur.toRowCol();
    return r.data();
}

static inline bool firstOfStatement( quint8 t )
{
    switch(t)
    {
    case Tok_ident:
    case Tok_IF:
    case Tok_CASE:
    case Tok_WHILE:
    case Tok_REPEAT:
    case Tok_FOR:
    case Tok_WITH:
    case Tok_RETURN:
    case Tok_LOOP:
    case Tok_EXIT:
        return true;
    default:
        return false;
    }
}

StatSeq Parser::statementSequence(Scope* scope)
{
    static const TokSet toks = TokSet() << Tok_Semi << Tok_CASE << Tok_FOR << Tok_WITH << Tok_RETURN
                                        << Tok_IF << Tok_REPEAT << Tok_WHILE << Tok_LOOP << Tok_EXIT
                                        << Tok_ident ;
    if( d_sync )
        sync(toks);

    StatSeq seq;
    while( d_la == Tok_Semi )
    {
        next();
    }
    while( firstOfStatement(d_la) )
    {
        Ref<Statement> s = statement(scope);
        if( !s.isNull() )
            seq << s;
        while( d_la == Tok_Semi )
        {
            next();
        }
    }
    return seq;
}

Ref<Expression> Parser::elsifStatement(Scope* scope, bool inWhile, StatSeq& seq)
{
    MATCH( Tok_ELSIF, tr("expecting the ELSIF keyword") );
    Ref<Expression> e = expression();
    if( inWhile )
    {
        MATCH( Tok_DO, tr("expecting the DO keyword after an ELSIF expression") );
    }else
    {
        MATCH( Tok_THEN, tr("expecting the THEN keyword after an ELSIF expression") );
    }
    seq = statementSequence(scope);
    return e;
}

Ref<Expression> Parser::guard()
{
    Ref<Expression> q = qualident();
    if( q.isNull() )
        return 0;
    MATCH( Tok_Colon, tr("expecting ':' between qualident and named type in quard") );
    Ref<BinExpr> e = new BinExpr();
    e->d_op = BinExpr::IS;
    e->d_loc = d_cur.toRowCol();
    e->d_lhs = q;
    e->d_rhs = qualident();
    return e.data();
}

void Parser::elseStatement( Scope* scope, StatSeq& seq)
{
    MATCH( Tok_ELSE, tr("expecting the ELSE keyword") );
    seq = statementSequence(scope);
}

static inline bool firstOfExpression( quint8 t )
{
    switch( t )
    {
    case Tok_Lpar:
    case Tok_Plus:
    case Tok_Minus:
    case Tok_Lbrace:
    case Tok_Tilde:
    case Tok_FALSE:
    case Tok_NIL:
    case Tok_TRUE:
    case Tok_ident:
    case Tok_integer:
    case Tok_real:
    case Tok_string:
    case Tok_hexchar:
    case Tok_hexstring:
        return true;
    default:
        return false;
    }
}

bool Parser::case_(Scope* scope, CaseStmt::Case& c)
{
#ifdef OBN07
    switch( d_la )
    {
    case Tok_ident:
    case Tok_integer:
    case Tok_string:
    case Tok_hexchar:
    case Tok_hexstring:
        c.d_labels = caseLabelList();
        MATCH( Tok_Colon, tr("expecting ':' after the case label list") );
        c.d_block = statementSequence(scope);
        return true;

    // follows
    case Tok_Bar:
    case Tok_END:
        break;

    default:
        syntaxError( tr("invalid case") );
        break;
    }
#else
    if( firstOfExpression(d_la) )
    {
        c.d_labels = caseLabelList();
        MATCH( Tok_Colon, tr("expecting ':' after the case label list") );
        c.d_block = statementSequence(scope);
        return true;
    }else if( d_la != Tok_Bar && d_la != Tok_END && d_la != Tok_ELSE )
        syntaxError( tr("invalid case") );
#endif
    return false;
}

ExpList Parser::caseLabelList()
{
    ExpList res;
    Ref<Expression> e = labelRange();
    if( !e.isNull() )
        res << e;
    while( d_la == Tok_Comma )
    {
        next();
        e = labelRange();
        if( !e.isNull() )
            res << e;
    }
    return res;
}

Ref<Expression> Parser::labelRange()
{
    Ref<Expression> lhs = label();
    if( lhs.isNull() )
        return 0;
    if( d_la == Tok_2Dot )
    {
        next();
        Ref<BinExpr> bin = new BinExpr();
        bin->d_op = BinExpr::Range;
        bin->d_loc = d_cur.toRowCol();
        bin->d_lhs = lhs;
        bin->d_rhs = label();
        if( !bin->d_rhs.isNull() )
            lhs = bin.data();
    }
    return lhs;
}

Ref<Expression> Parser::label()
{
#ifdef OBN07
    switch( d_la )
    {
    case Tok_integer:
        return number().data();

    case Tok_string:
        next();
        return new Literal( stringType(),d_cur.toRowCol(),d_cur.d_val.mid(1,d_cur.d_val.size()-2));
    case Tok_hexchar:
        next();
        return new Literal(charType(),d_cur.toRowCol(), QByteArray::fromHex( d_cur.d_val.left( d_cur.d_val.size() - 1 ) ));
    case Tok_hexstring:
        next();
        return new Literal( stringType(),d_cur.toRowCol(),QByteArray::fromHex( d_cur.d_val.mid(1, d_cur.d_val.size() - 2)));

    case Tok_ident:
        return qualident();

    default:
        syntaxError( tr("invalid label") );
    }
    return 0;
#else
    return constExpression(); // OBN2
#endif
}

Ref<Procedure> Parser::procedureDeclaration(bool headingOnly,Scope* scope)
{
    Ref<Procedure> res = new Procedure();
    const int kind = procedureHeading(res.data(), scope);
    if( kind == ProcForward )
        return 0; // just ignore this declaration
    if( !headingOnly )
    {
        if( kind == ProcCImp )
        {
            res->d_imp = literal();
        }
        if( d_la == Tok_Semi )
        {
            next();
        }
        switch( kind )
        {
        case ProcNormal:
            procedureBody( res.data() );
            MATCH( Tok_ident, tr("expecting procedure name after END keyword") );
            if( d_cur.isValid() && d_cur.d_val != res->d_name )
                semanticError( d_next.toLoc(), tr("the ident '%1' after the END keyword must be equal "
                                  "to the procedure name").arg(d_next.d_val.constData()));
            break;
        case ProcCImp:
            res->d_end = d_cur.toRowCol();
            break;
        }
    }
    return res;
}

int Parser::procedureHeading(Procedure* p, Scope* scope)
{
    if( d_la == Tok_PROC )
    {
        MATCH( Tok_PROC, tr("expecting the PROCEDURE keyword") );
    }else
    {
        MATCH( Tok_PROCEDURE, tr("expecting the PROCEDURE keyword") );
    }

    int kind = ProcNormal;
    if( d_la == Tok_Star || d_la == Tok_Hat || d_la == Tok_Minus || d_la == Tok_Plus )
    {
        next();
        // Oberon-2 feature
        switch( d_cur.d_type )
        {
        case Tok_Hat:
            kind = ProcForward; // forward declarations are ignored
            break;
        case Tok_Minus:
            // Tok_Minus is used by Ofront to define explicit C code as implementations
            kind = ProcCImp;
            break;
        case Tok_Star:
            // Tok_Star is the way Oberon 87 marks procs the address of which can be taken; like ProcNormal
            break;
        case Tok_Plus:
            // happens in ETH Oberon V3 and V4
            break;
       }
    }

    if( d_la == Tok_Lt )
    {
        p->d_metaParams = typeParams();
        Q_ASSERT( p->d_names.isEmpty() );
        for( int i = 0; i < p->d_metaParams.size(); i++ )
        {
            if( !p->add( p->d_metaParams[i].data() ) )
                semanticError( p->d_metaParams[i]->d_loc,tr("name of type parameter must be unique"));
        }
    }
    if( d_la == Tok_Lpar )
    {
        p->d_receiver = receiver();
        if( !p->d_receiver.isNull() )
        {
            if( !p->add( p->d_receiver.data() ) )
                semanticError( p->d_receiver->d_loc, tr("receiver name collides with type parameter names"));
        }
    }
    identdef(p,scope);
    if( kind == ProcNormal || kind == ProcCImp )
    {
        if( p->d_receiver.isNull() )
        {
            // add to scope
            if( !scope->add(p) )
                semanticError( p->d_loc, tr("procedure name is not unique") );
        }else
        {
            // add to module order to be resolved in a later phase
            scope->d_order.append( p );
            p->d_scope = scope;
        }
    }

    Ref<ProcType> t = new ProcType();
    t->d_ident = p;
    p->d_type = t.data();
    if( d_la == Tok_Lpar )
    {
        formalParameters(p, t.data());
        for( int i = 0; i < t->d_formals.size(); i++ )
        {
            if( !p->add( t->d_formals[i].data() ) )
                semanticError( t->d_formals[i]->d_loc, tr("parameter name collides with other name in scope"));
        }
    }
    return kind;
}

void Parser::procedureBody(Procedure* p)
{
    declarationSequence(false, p);
    if( d_la == Tok_BEGIN || d_la == Tok_DO
#ifndef OBN07
            || d_la == Tok_RETURN
#endif
            )
    {
#ifndef OBN07
        if( d_la == Tok_RETURN )
            p->d_body << returnStatement(p);
        else
        {
            next();
            p->d_body = statementSequence(p);
        }
#else
        next();
        p->d_body = statementSequence(p);
#endif
    }
#ifdef OBN07
    if( d_la == Tok_RETURN )
    {
        Ref<Statement> ret = returnStatement(p);
        if( !ret.isNull() )
            p->d_body << ret;
        if( d_la == Tok_Semi )
            next();
    }
#endif
    MATCH( Tok_END, tr("expecting a closing END in procedure") );
    p->d_end = d_cur.toRowCol();
}

Ref<Parameter> Parser::receiver()
{
    MATCH( Tok_Lpar, tr("expecting '(' to start a receiver") );

    Ref<Parameter> v = new Parameter();

    if( d_la == Tok_VAR )
    {
        next();
        v->d_var = true;
    }
    MATCH( Tok_ident, tr("expecting the receiver variable name") );
    v->d_name = d_cur.d_val;
    v->d_loc = d_cur.toRowCol();

    MATCH( Tok_Colon, tr("expecting ':'") );

    // like namedType, but only one ident, not qualident
    MATCH( Tok_ident, tr("expecting the type name") );
    Ref<IdentLeaf> id = new IdentLeaf();
    id->d_name = d_cur.d_val;
    id->d_loc = d_cur.toRowCol();

    Ref<QualiType> q = new QualiType();
    q->d_quali = id.data();
    q->d_ident = v.data();
    if( d_la == Tok_Lt )
    {
        q->d_metaActuals = typeActuals();
    }
    v->d_type = q.data();

    MATCH( Tok_Rpar, tr("expecting ')' to end a receiver") );

    return v;
}

void Parser::declarationSequence(bool definition, Scope* scope )
{
    static const TokSet toks = TokSet() << Tok_CONST << Tok_PROC << Tok_PROCEDURE << Tok_TYPE << Tok_VAR;
    if( d_sync )
        sync(toks);

    while( d_la == Tok_CONST || d_la == Tok_TYPE || d_la == Tok_VAR || d_la == Tok_PROCEDURE || d_la == Tok_PROC )
    {
        switch( d_la )
        {
        case Tok_CONST:
            next();
            while( d_la == Tok_ident )
            {
                constDeclaration(scope);
                if( d_la == Tok_Semi )
                    next();
            }
            break;
        case Tok_TYPE:
            next();
            while( d_la == Tok_ident )
            {
                typeDeclaration(scope);
                if( d_la == Tok_Semi )
                    next();
            }
            break;
        case Tok_VAR:
            next();
            while( d_la == Tok_ident )
            {
                variableDeclaration(scope);
                if( d_la == Tok_Semi )
                    next();
            }
            break;
        case Tok_PROCEDURE:
        case Tok_PROC:
            procedureDeclaration(definition,scope);
            if( d_la == Tok_Semi )
                next();
            break;
        default:
            Q_ASSERT(false);
            break;
        }
    }
}

Ref<Statement> Parser::returnStatement(Scope* scope)
{
    MATCH( Tok_RETURN, tr("expecting the RETURN keyword") );
    Ref<Return> r = new Return();
    r->d_loc = d_cur.toRowCol();

    bool needsExpression = false;
    if( !scope->d_type.isNull() && scope->d_type->getTag() == Thing::T_ProcType )
    {
        ProcType* t = cast<ProcType*>(scope->d_type.data());
        needsExpression = !t->d_return.isNull();
    }
    if( needsExpression )
        r->d_what = expression();

    return r.data();
}

bool Parser::fPSection(Scope* scope, ProcType* pt)
{
    bool var = false;
    bool in = false;
    if( d_la == Tok_VAR || d_la == Tok_IN )
    {
        next();
        var = true;
        in = d_cur.d_type == Tok_IN;
    }
    QList<Token> names;
    MATCH( Tok_ident, tr("expecting formal parameter name") );
    if( d_cur.isValid() )
        names << d_cur;
    while( d_la == Tok_Comma )
    {
        next();
        MATCH( Tok_ident, tr("expecting formal parameter name") );
        if( d_cur.isValid() )
            names << d_cur;
    }
    MATCH( Tok_Colon, tr("expecting ':' to separate parameter names from their type") );
    if( !d_cur.isValid() )
        return false;
    Ref<Type> t = formalType(scope);

    foreach( const Token& name, names )
    {
        Ref<Parameter> p = new Parameter();
        p->d_type = t;
        p->d_name = name.d_val;
        p->d_loc = name.toRowCol();
        p->d_var = var;
        p->d_const = in;
        if( pt->find( p->d_name ) != 0 )
            semanticError(p->d_loc,tr("name is not unique in parameter list") );
        pt->d_formals.append(p);
    }
    return true;
}

Ref<Type> Parser::formalType(Scope* scope)
{
#ifdef OBN07
    int arrayOfCount = 0;
    if( d_la == Tok_Lbrack )
    {
        while( d_la == Tok_Lbrack )
        {
            next();
            MATCH( Tok_Rbrack, tr("expecting ']' after '['") );
            arrayOfCount++;
        }
    }else
    {
        while( d_la == Tok_ARRAY )
        {
            next();
            MATCH( Tok_OF, tr("expecting the OF keyword after ARRAY") );
            arrayOfCount++;
        }
    }
    Ref<Type> t = namedType(0).data();
    if( arrayOfCount )
    {
        QList< Ref<Array> > tmp;
        while( arrayOfCount-- )
        {
           Ref<Array> a  = new Array();
           if( !tmp.isEmpty() )
               tmp.back()->d_type = a.data();
           tmp.push_back(a);
        }
        tmp.back()->d_type = t;
        t = tmp.front().data();
    }
    return t;
#else
    return type(scope,0);
#endif
}

void Parser::importList()
{
    MATCH( Tok_IMPORT, tr("expecting the IMPORT keyword") );
    import();
    while( d_la == Tok_Comma )
    {
        next();
        import();
    }
    if( d_la == Tok_Semi )
        next();
}

void Parser::import()
{
    Ref<Import> imp = new Import();

    Token name, suff;
    bool hasErr = false;

    if( peek(2) == Tok_ColonEq )
    {
        MATCH( Tok_ident, tr("expecting import name alias") );
        if( d_cur.isValid() )
            name = d_cur;
        else
            hasErr = true;
        next(); // :=
    }
    MATCH( Tok_ident, tr("expecting external module name or path") );
    if( d_cur.isValid() )
    {
        suff = d_cur;
        imp->d_path << suff.d_val;
    }else
        hasErr = true;
    while( d_la == Tok_Slash )
    {
        next();
        MATCH( Tok_ident, tr("expecting external module path") );
        if( d_cur.isValid() )
        {
            suff = d_cur;
            imp->d_path << suff.d_val;
        }else
            hasErr = true;
    }
    if( !name.isValid() )
        name = suff;
    imp->d_name = name.d_val;
    if( !d_mod->add( imp.data() ) )
    {
        semanticError( name.toLoc(), tr("name '%1' is not unique in module; use a unique name alias (ident := path)").arg(imp->d_name.constData()) );
        hasErr = true;
    }else
        d_mod->d_imports << imp.data();
    imp->d_loc = name.toRowCol();
#if 0
    imp->d_mod = findModule( path );
    if( imp->d_mod.isNull() )
    {
        semanticError( name.toLoc(), tr("module '%1' could not be found").arg( path.join('/').constData() ) );
        hasErr = true;
    }
#endif
    if( hasErr )
    {
        imp->d_hasErrors = true;
        d_mod->d_hasErrors = true;
    }
}

void Parser::ofrontTag()
{
    if( d_la == Tok_Lbrack )
    {
        MATCH( Tok_Lbrack, tr("expecting '['") );
        MATCH( Tok_integer, tr("expecting '1'") );
        MATCH( Tok_Rbrack, tr("expecting ']'") );
    }
}

void Parser::next()
{
    if( d_errCount > 25 )
        throw MaximumErrorCountExceeded();
    d_cur = d_next;
    d_next = d_lex->nextToken();
    while( d_next.d_type == Tok_Invalid )
    {
        d_sync = true;
        d_next = d_lex->nextToken();
    }
    d_la = (Ob::TokenType)d_next.d_type;
}

bool Parser::sync(const Parser::TokSet& ts)
{
    d_sync = false;
    if( ts.none() )
        return true;
    while( !ts.test(d_la) && d_la != Tok_Eof && d_la != Tok_Invalid )
        next();
    if( d_la == Tok_Eof )
        throw PrematureEndFound();
    return ts.test(d_la);
}

quint8 Parser::peek(quint16 la)
{
    if( la == 1 )
        return d_la;
    else if( la == 0 )
        return d_cur.d_type;
    else
        return d_lex->peekToken(la-1).d_type;
}

void Parser::syntaxError(const QString& err)
{
    if( !d_mod.isNull() )
        d_mod->d_hasErrors = true;
    d_errCount++;
    d_sync = true;
    if( d_next.d_lineNr == 0 )
        d_errs->error( Errors::Syntax, d_cur.toLoc(), err );
    else
        d_errs->error( Errors::Syntax, d_next.toLoc(), err );
}

void Parser::semanticError(const Loc& l, const QString& err)
{
    d_mod->d_hasErrors = true;
    d_errCount++;
    d_errs->error( Errors::Semantics, l, err );
}

void Parser::semanticError(const RowCol& rc, const QString& err)
{
    semanticError( Loc( rc.d_row, rc.d_col, d_mod->d_file ), err );
}

