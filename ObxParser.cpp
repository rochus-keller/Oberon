/*
* Copyright 2021 Rochus Keller <mailto:me@rochus-keller.ch>
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
#include <limits>
using namespace Obx;
using namespace Ob;

#if !defined(OB_OBNX) || !defined(OB_OBN2)
#error "OB_OBNX or OB_OBN2 must be defined for ObxParser"
#endif

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
            d_mod->d_isExt = d_lex->isEnabledExt();
            break;
        case Tok_DEFINITION:
            next();
            module(true);
            d_mod->d_isExt = d_lex->isEnabledExt();
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

    if( definition )
    {
        if( d_la == Tok_Lbrack )
        {
            m->d_externC = true;
            m->d_sysAttrs = systemAttrs();
        }
    }else
    {
        if( d_la == Tok_Lt || d_la == Tok_Lpar )
        {
            m->d_metaParams = typeParams();
            for( int i = 0; i < m->d_metaParams.size(); i++ )
            {
                m->d_metaParams[i]->d_slot = i;
                m->d_metaParams[i]->d_slotValid = true;
                if( !m->add( m->d_metaParams[i].data() ) )
                    semanticError(m->d_metaParams[i]->d_loc,tr("name of type parameter must be unique"));
            }
        }
    }

    if( d_la == Tok_Semi )
        next();
    declarationSequence(m->d_isDef, m.data() );
    if( d_la == Tok_BEGIN || d_la == Tok_DO )
    {
        next();
        m->d_begin = d_cur.toRowCol();
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
    Ref<Literal> res;
    switch( d_la )
    {
    case Tok_integer:
        {
            next();
            bool isInt = false;
            bool isLong = false;
            if( d_cur.d_val.endsWith('I') || d_cur.d_val.endsWith('i') )
            {
                isInt = true;
                d_cur.d_val.chop(1);
            }else if(d_cur.d_val.endsWith('L') || d_cur.d_val.endsWith('l') )
            {
                isLong = true;
                d_cur.d_val.chop(1);
            }
            if( d_cur.d_val.endsWith('H') || d_cur.d_val.endsWith('h') )
            {
                bool ok;
                const quint64 v = d_cur.d_val.left(d_cur.d_val.size()-1).toULongLong(&ok,16);
                if( isInt )
                {
                    if( !ok || v > std::numeric_limits<quint32>::max() )
                        semanticError(d_cur.toLoc(), tr("hex literal too large for INTEGER"));
                    val = (qint32)(quint32)v;
                }else
                {
                    if( !ok )
                        semanticError(d_cur.toLoc(), tr("hex literal too large for LONGINT"));
                    val = (qint64)v;
                }
            }else
            {
                bool ok;
                const qint64 v = d_cur.d_val.toLongLong(&ok);
                if( isInt )
                {
                    if( !ok || v > std::numeric_limits<qint32>::max() || v < std::numeric_limits<qint32>::min() )
                        semanticError(d_cur.toLoc(), tr("literal too large for INTEGER"));
                }
                val = v;
            }
            res = new Literal( Literal::Integer, d_cur.toRowCol(),val);
            res->d_wide = isLong;
            res->d_minInt = isInt;
        }
        break;
    case Tok_real:
        {
            next();
            QByteArray str = d_cur.d_val;
            // NOTE strtof is not useful to find out whether float is good enough precision;
            // a number like pi is just cut to float without error or HUGE_VALF
            if( str.contains('d') || str.contains('D') )
            {
                str.replace('d','e');
                str.replace('D','E');
            }else if( str.contains('s') || str.contains('S') )
            {
                str.replace('s','e');
                str.replace('S','E');
            }
            val = str.toDouble(); // we save double in any case (JitComposer depends on it)
            res = new Literal( Literal::Real, d_cur.toRowCol(),val);
            res->d_wide = d_cur.d_double;
        }
        break;
    default:
        syntaxError( tr("expecting a number") );
        return 0;
    }
    return res;
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
        id->d_mod = d_mod.data();
        cur = id.data();
        next(); // dot
    }
    MATCH( Tok_ident, tr("expecting an identifier") );
    if( cur.isNull() )
    {
        Ref<IdentLeaf> id = new IdentLeaf();
        id->d_name = d_cur.d_val;
        id->d_loc = d_cur.toRowCol();
        id->d_mod = d_mod.data();
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
        if( d_mod->d_isDef )
            warning(d_cur.toLoc(), tr("'*' ineffective in DEFINITION since all elements are public"));
        if( scope->getTag() != Thing::T_Procedure )
            n->d_visibility = Named::ReadWrite;
        else
            semanticError(d_cur.toLoc(), tr("export mark only allowed on module level"));
    }else if( d_la == Tok_Minus )
    {
        next();
        if( scope->getTag() != Thing::T_Procedure )
            n->d_visibility = Named::ReadOnly;
        else
            semanticError(d_cur.toLoc(), tr("export mark only allowed on module level"));
    }else
    {
        if( d_mod->d_isDef && scope->getTag() != Thing::T_Procedure )
            n->d_visibility = Named::ReadWrite;
        else
            n->d_visibility = Named::Private;
    }
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
        semanticError( res->d_loc, tr("constant name '%1' is not unique").arg(res->d_name.constData()) );
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
    MATCH( Tok_Eq, tr("expecting '=' after ident in type declaration") );

    if( !scope->add(nt.data()) )
        semanticError(nt->d_loc,tr("name of type declaration must be unique"));

    nt->d_type = type(scope, nt.data(), 0);

    // self reference is now checked in validator
    return nt;
}

MetaParams Parser::typeParams()
{
    bool usesPar = false;
    if( d_la == Tok_Lt )
        next();
    else if( d_la == Tok_Lpar )
    {
        next();
        usesPar = true;
    }else
        syntaxError( tr("expecting '<' or '(' to start type parameters") );
#ifndef _HAS_GENERICS
    syntaxError(tr("this version of the parser doesn't support generic types") );
#endif
    MATCH( Tok_ident, tr("at least one identifier required as type parameter") );
    MetaParams res;
    Ref<GenericName> t = new GenericName();
    t->d_name = d_cur.d_val;
    t->d_loc = d_cur.toRowCol();
    res << t;
    while( d_la == Tok_Comma || d_la == Tok_ident ) // comma is optional
    {
        if( d_la == Tok_Comma )
            next();
        MATCH( Tok_ident, tr("identifier expected in type parameter list") );
        Ref<GenericName> t = new GenericName();
        t->d_name = d_cur.d_val;
        t->d_loc = d_cur.toRowCol();
        res << t;
    }
    if( usesPar )
    {
        MATCH( Tok_Rpar, tr("expecting ')' to end type parameters") );
    }else
    {
        MATCH( Tok_Gt, tr("expecting '>' to end type parameters") );
    }
    return res;
}

Ref<Type> Parser::type(Scope* scope, Named* id, Type* binding)
{
    Q_ASSERT( ( id != 0 ) != ( binding != 0 ) ); // xor, either id or binding

    static const TokSet toks = TokSet() << Tok_Lpar << Tok_Lbrack << Tok_Hat << Tok_ARRAY << Tok_CARRAY
                                        << Tok_UNSAFE << Tok_CPOINTER << Tok_CSTRUCT << Tok_CUNION
                                  << Tok_POINTER << Tok_PROC << Tok_PROCEDURE << Tok_RECORD << Tok_ident;
    if( d_sync )
        sync(toks);

    switch( d_la )
    {
    case Tok_ident:
        return namedType(id, binding).data();
    case Tok_Lpar:
        return enumeration(scope,id, binding);
    case Tok_ARRAY:
    case Tok_CARRAY:
    case Tok_Lbrack:
        return arrayType(scope, id, binding);
    case Tok_RECORD:
    case Tok_CSTRUCT:
    case Tok_CUNION:
        return recordType(scope,id, binding);
    case Tok_POINTER:
    case Tok_UNSAFE:
    case Tok_CPOINTER:
    case Tok_Hat:
    case Tok_Star:
        return pointerType(scope, id, binding);
    case Tok_PROCEDURE:
    case Tok_PROC:
        return procedureType(scope, id, binding);
    default:
        syntaxError( tr("expecting type"));
        break;
    }
    return 0;
}

MetaActuals Parser::typeActuals()
{
    bool usesPar = false;
    if( d_la == Tok_Lt )
        next();
    else if( d_la == Tok_Lpar )
    {
        next();
        usesPar = true;
    }else
        syntaxError( tr("expecting '<' or '(' to start type actuals") );
#ifndef _HAS_GENERICS
    syntaxError(tr("this version of the parser doesn't support generic types") );
#endif
    MetaActuals res;
    Ref<Type> t = typeActual();
    if( !t.isNull() )
        res << t;
    while( d_la == Tok_Comma || d_la == Tok_ident ) // comma is optional
    {
        if( d_la == Tok_Comma )
            next();
        t = typeActual();
        if( !t.isNull() )
            res << t;
    }
    if( usesPar )
    {
        MATCH( Tok_Rpar, tr("expecting ')' to end type actuals") );
    }else
    {
        MATCH( Tok_Gt, tr("expecting '>' to end type actuals") );
    }
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
        c->d_name = t.d_val;
        c->d_loc = t.toRowCol();
        c->d_constExpr = new Literal(Literal::Enum,c->d_loc,e->d_items.count(),e);
        // type and val are evaluated in Validator
        if( e->d_decl )
            c->d_visibility = e->d_decl->d_visibility;
        e->d_items.append(c);
        scope->add(c.data());
    }
}

Ref<Type> Parser::enumeration(Scope* scope,Named* id, Type* binding)
{
    Ref<Enumeration> e = new Enumeration();
    e->d_decl = id;
    e->d_binding = binding;
    MATCH( Tok_Lpar, tr("expecting '(' to start enumeration") );
    e->d_loc = d_cur.toRowCol();
    MATCH( Tok_ident, tr("at least one identifier required in enumeration") );
    addEnum( scope, e.data(), d_cur );
    while( d_la == Tok_Comma || d_la == Tok_ident ) // optional comma
    {
        if( d_la == Tok_Comma )
            next();
        MATCH( Tok_ident, tr("identifier expected after ',' in enumeration") );
        addEnum( scope, e.data(), d_cur );
    }
    MATCH( Tok_Rpar, tr("expecting ')' to end enumeration") );
    return e.data();
}

Ref<QualiType> Parser::namedType(Named* id, Type* binding)
{
    Ref<Expression> e = qualident();
    if( e.isNull() )
        return 0; // error was already reported

    Ref<QualiType> q = new QualiType();
    q->d_quali = e;
    q->d_decl = id;
    q->d_binding = binding;
    q->d_loc = e->d_loc;

    return q.data();
}

Ref<Type> Parser::returnType(Scope* scope, Type* binding)
{
#if 0
    switch( d_la )
    {
    case Tok_POINTER:
    case Tok_CPOINTER:
        {
            next();
            Ref<Pointer> p = new Pointer();
            p->d_loc = d_cur.toRowCol();
            if( d_cur.d_type == Tok_CPOINTER )
                p->d_unsafe = true;
            MATCH( Tok_TO, tr("expecting the TO keyword after the POINTER keyword") );
            p->d_to = namedType(0,0).data();
            if( p->d_to )
                p->d_to->d_binding = p.data();
            p->d_binding = binding;
            return p.data();
        }
        break;
    case Tok_Hat:
    case Tok_Star:
        {
            next();
            Ref<Pointer> p = new Pointer();
            p->d_loc = d_cur.toRowCol();
            if( d_cur.d_type == Tok_Star )
                p->d_unsafe = true;
            p->d_to = namedType(0,0).data();
            if( p->d_to )
                p->d_to->d_binding = p.data();
            p->d_binding = binding;
            return p.data();
        }
        break;
    default:
        return namedType(0,binding).data();
    }
#else
    return type(scope,0,binding);
#endif
}

Ref<Type> Parser::arrayType(Scope* scope, Named* id, Type* binding)
{
    Ref<Array> arr = new Array();
    arr->d_decl = id;
    arr->d_binding = binding;
    QList< Ref<Expression> > dims;
    if( d_la == Tok_ARRAY || d_la == Tok_CARRAY )
    {
        if( d_la == Tok_CARRAY )
            arr->d_unsafe = true;

        next();
        arr->d_loc = d_cur.toRowCol();
        /*arr->d_flag =*/ systemFlag(); // backward compatiblity
        if( d_la == Tok_VAR )
        {
            next();
            arr->d_vla = true;
        }
        if( d_la != Tok_OF )
        {
            dims = lengthList();
        }
        MATCH( Tok_OF, tr("expecting OF after ARRAY keyword or length list") );
    }else
    {
        MATCH( Tok_Lbrack, tr("expecting '['") );
        arr->d_loc = d_cur.toRowCol();
        if( d_la == Tok_VAR )
        {
            next();
            arr->d_vla = true;
        }
        if( d_la != Tok_Rbrack )
        {
            dims = lengthList();
        }
        MATCH( Tok_Rbrack, tr("expecting ']'") );
    }

    Ref<Type> t = type(scope,0, arr.data());
    arr->d_type = t;

    if( !dims.isEmpty() )
    {
        arr->d_lenExpr = dims[0];
        Ref<Array> last = arr;
        for( int i = 1; i < dims.size(); i++ )
        {
            Ref<Array> cur = new Array();
            cur->d_type = t;
            cur->d_lenExpr = dims[i];
            cur->d_vla = last->d_vla;
            last->d_type = cur.data();
            last = cur;
        }
    }

    return arr.data();
}

Ref<Type> Parser::recordType(Scope* scope, Named* id, Type* binding)
{
    Ref<Record> res = new Record();
    res->d_decl = id;
    res->d_binding = binding;
    switch( d_la )
    {
    case Tok_RECORD:
        MATCH( Tok_RECORD, tr("expecting the RECORD keyword") );
        break;
    case Tok_CSTRUCT:
        next();
        res->d_unsafe = true;
        break;
    case Tok_CUNION:
        next();
        res->d_unsafe = true;
        res->d_union = true;
        break;
    default:
        break;
    }

    res->d_loc = d_cur.toRowCol();
    /* res->d_flag = */ systemFlag(); // backward compatibility
    if( d_la == Tok_Lpar )
    {
        next();
        res->d_base = baseType(res.data());
        MATCH( Tok_Rpar, tr("expecting ')' after record base type") );
    }
    if( d_la != Tok_END )
        fieldListSequence( scope, res.data() );
    MATCH( Tok_END, tr("expecting END in record type") );
    return res.data();
}

Ref<Type> Parser::pointerType(Scope* scope, Named* id, Type* binding)
{
    Ref<Pointer> p = new Pointer();
    p->d_decl = id;
    p->d_binding = binding;
    bool forceUnsafeType = false;
    if( d_la == Tok_Hat )
    {
        next();
        p->d_loc = d_cur.toRowCol();
    }else if( d_la == Tok_Star )
    {
        next();
        p->d_loc = d_cur.toRowCol();
        p->d_unsafe = true;
        if( d_la == Tok_Lbrack )
            forceUnsafeType = true;
    }else if( d_la == Tok_CPOINTER )
    {
        next();
        p->d_loc = d_cur.toRowCol();
        p->d_unsafe = true;
        MATCH( Tok_TO, tr("expecting the TO keyword after the CPOINTER keyword") );
    }else
    {
        if( d_la == Tok_UNSAFE )
        {
            next();
            p->d_unsafe = true;
        }
        MATCH( Tok_POINTER, tr("expecting the POINTER keyword") );
        p->d_loc = d_cur.toRowCol();

        /* p->d_flag = */ systemFlag(); // backward compatibility
        MATCH( Tok_TO, tr("expecting the TO keyword after the POINTER keyword") );
    }
    p->d_to = type(scope, 0, p.data() );
    if( p->d_to && forceUnsafeType )
    {
        Type* t = p->d_to.data();
        while( t && t->getTag() == Thing::T_Array)
        {
            Array* a = cast<Array*>(t);
            a->d_unsafe = true;
            t = a->d_type.data();
        }
    }
    return p.data();
}

Ref<Type> Parser::procedureType(Scope* scope, Named* id, Type* binding)
{
    Ref<ProcType> p = new ProcType();
    p->d_decl = id;
    p->d_binding = binding;
    if( d_mod->d_externC )
        p->d_unsafe = true;
    if( d_la == Tok_PROC )
    {
        MATCH( Tok_PROC, tr("expecting the PROCEDURE keyword") );
    }else
    {
        MATCH( Tok_PROCEDURE, tr("expecting the PROCEDURE keyword") );
    }
    p->d_loc = d_cur.toRowCol();
    if( d_la == Tok_Hat ) // TODO: deprecated, will be replaced by (^), see below
    {
        next();
        p->d_typeBound = true;
    }
    if( d_la == Tok_Lpar )
    {
        if( peek(2) == Tok_POINTER || peek(2) == Tok_Hat )
        {
            next(); next();
            p->d_typeBound = true;
            MATCH( Tok_Rpar, tr("expecting ')' to close type-bound procedure type") );
            if( d_la == Tok_Lpar )
                formalParameters(scope, p.data());
        }else
            formalParameters(scope, p.data());
    }
    return p.data();
}

Ref<Type> Parser::typeActual()
{
    switch( d_la )
    {
    case Tok_ident:
        return namedType(0,0).data(); // TODO

    case Tok_integer:
    case Tok_real:
    case Tok_string:
    case Tok_hexstring:
    case Tok_hexchar:
    case Tok_NIL:
    case Tok_TRUE:
    case Tok_FALSE:
    case Tok_Lbrace:
        //return literal().data();

    default:
        syntaxError(tr("expecting type as actual type parameters"));
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
        {
            next();
            const QByteArray utf8 = d_cur.d_val.mid(1,d_cur.d_val.size()-2); // remove "" and '' around string
            const QString tmp = QString::fromUtf8( utf8 );
            Ref<Literal> lit =  new Literal(Literal::String, d_cur.toRowCol(), utf8);
            lit->d_strLen = tmp.size();
            lit->d_wide = Literal::isWide(tmp);
            return lit.data();
        }
        break;
    case Tok_hexstring:
        {
            next();
            const QByteArray bytes = QByteArray::fromHex( d_cur.d_val );
            Ref<Literal> lit =  new Literal(Literal::Bytes, d_cur.toRowCol(), bytes );
            lit->d_strLen = bytes.size();
            return lit.data();
        }
        break;
    case Tok_hexchar:
        {
            next();
            const quint16 ch = d_cur.d_val.left( d_cur.d_val.size() - 1 ).toUInt(0,16);
            Ref<Literal> lit = new Literal( Literal::Char, d_cur.toRowCol(), ch);
            if( ch > 255 )
                lit->d_wide = true;
            return lit.data();
        }
        break;
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

Ref<QualiType> Parser::baseType(Record* rec)
{
    Ref<QualiType> res = namedType(0, rec);
    switch( res->d_quali->getTag() )
    {
    case Thing::T_IdentLeaf:
        cast<IdentLeaf*>(res->d_quali.data())->d_role = SuperRole;
        break;
    case Thing::T_IdentSel:
        cast<IdentSel*>(res->d_quali.data())->d_role = SuperRole;
        break;
    }
    return res;
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
    f->d_owner = r;
    f->d_scope = scope; // this is not the record but the module or procedure where the record is declared
    f->d_unsafe = r->d_unsafe;
    fields << f;
    identdef(f,scope);
    while( d_la == Tok_Comma || d_la == Tok_ident ) // optional comma
    {
        if( d_la == Tok_Comma )
            next();
        f = new Field();
        f->d_owner = r;
        f->d_scope = scope;
        f->d_unsafe = r->d_unsafe;
        fields << f;
        identdef(fields.back().data(),scope);
    }
    MATCH( Tok_Colon, tr("expecting ':' between identifier list and type in field list") );
    Ref<Type> t = type(scope, fields.first().data(), 0);
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
        p->d_return = returnType(scope, p).data();
    }
}

void Parser::variableDeclaration(Scope* scope)
{
    QList<Ref<Named> > vars;
    const bool moduleLevel = scope == d_mod.data();
    if( moduleLevel )
        vars << new Variable();
    else
        vars << new LocalVar();
    identdef(vars.back().data(),scope);
    while( d_la == Tok_Comma || d_la == Tok_ident ) // optional comma
    {
        if( d_la == Tok_Comma )
            next();
        if( moduleLevel )
            vars << new Variable();
        else
            vars << new LocalVar();
        identdef(vars.back().data(),scope);
    }
    MATCH( Tok_Colon, tr("expecting ':' between identifier list and type in field list") );
    Ref<Type> t = type(scope,vars.first().data(), 0);
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
            Ref<UnExpr> first = u;
            while( !first->d_sub.isNull() )
                first = cast<UnExpr*>(first->d_sub.data());
            first->d_sub = e;
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
            ExpList l = expList();
            Ref<ArgExpr> a, prev;
            foreach( const Ref<Expression>& e, l )
            {
                a = new ArgExpr();
                a->d_loc = d_cur.toRowCol();
                a->d_args.append(e);
                a->d_op = UnExpr::IDX;
                a->d_sub = prev.data();
                prev = a;
            }
            MATCH( Tok_Rbrack, tr("expecting ']' to terminate expression list in index") );
            return a.data(); // a can be one or a chain of ArgExpr with one arg each with a being the last in the list
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
    MATCH( Tok_END, tr("expecting a statement or closing END") );
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
    MATCH( Tok_END, tr("expecting a statement or closing END") );
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
    MATCH( Tok_END, tr("expecting a statement or closing END") );
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
    id->d_mod = d_mod.data();
    f->d_id = id.data();
    MATCH( Tok_ColonEq, tr("expecting ':=' to assign the start value of the FOR statement") );
    f->d_from = expression();
    MATCH( Tok_TO, tr("expecting the TO keyword") );
    f->d_to = expression();
    if( d_la == Tok_BY )
    {
        next();
        f->d_by = constExpression();
    }
#if 0
    else // done in validator
    {
        Ref<Literal> one = new Literal( Literal::Integer, d_next.toRowCol(), 1);
        f->d_by = one.data();
    }
#endif
    MATCH( Tok_DO, tr("expecting the DO keyword") );
    f->d_do = statementSequence(scope);
    MATCH( Tok_END, tr("expecting a statement or closing END") );
    return f.data();
}

Ref<Statement> Parser::withStatement(Scope* scope)
{
    MATCH( Tok_WITH, tr("expecting the WITH keyword") );

    Ref<IfLoop> c = new IfLoop();
    c->d_op = IfLoop::WITH;
    c->d_loc = d_cur.toRowCol();
    if( d_la == Tok_Bar )
        next();
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

    MATCH( Tok_END, tr("expecting a statement or closing END") );
    return c.data();
}

Ref<Statement> Parser::loopStatement(Scope* scope)
{
    MATCH( Tok_LOOP, tr("expecting the LOOP keyword") );
    Ref<IfLoop> c = new IfLoop();
    c->d_op = IfLoop::LOOP;
    c->d_loc = d_cur.toRowCol();
    c->d_then.append( statementSequence(scope) );
    MATCH( Tok_END, tr("expecting a statement or closing END") );
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

static inline bool followOfStatement( quint8 t )
{
    switch(t)
    {
    case Tok_Semi:
    case Tok_Bar:
    case Tok_ELSE:
    case Tok_ELSIF:
    case Tok_END:
    case Tok_RETURN:
    case Tok_UNTIL:
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
    bool returnFound = false;
    while( firstOfStatement(d_la) )
    {
        if( returnFound )
        {
            returnFound = false;
            warning(d_next.toLoc(), tr("this statement will not be executed") );
        }
        Ref<Statement> s = statement(scope);
        if( !s.isNull() )
            seq << s;
        switch(s->getTag())
        {
        case Thing::T_Return:
        case Thing::T_Exit:
            returnFound = true;
            break;
        }
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

Procedure* Parser::procedureDeclaration(bool headingOnly,Scope* scope)
{
#if 0
    if( d_mod->d_isDef )
        headingOnly = true;
#endif
    Ref<Procedure> res = new Procedure();
    const int kind = procedureHeading(res.data(), scope);
    if( kind == ProcForward )
        return 0; // just ignore this declaration
    if( !headingOnly )
    {
        bool hasEndIdent = false;
        if( kind == ProcCImp )
        {
            /*res->d_imp = */ literal(); // only backward compatibility
        }
        if( d_la == Tok_Semi )
        {
            next();
        }
        if( kind == ProcNormal )
        {
            if( procedureBody( res.data() ) || res->d_order.size() > res->d_parCount )
            {
                MATCH( Tok_ident, tr("expecting procedure name after END keyword") );
                hasEndIdent = true;
            }else if( d_la == Tok_ident )
            {
                next();
                hasEndIdent = true;
            }
            if( hasEndIdent && d_cur.isValid() && d_cur.d_val != res->d_name )
                semanticError( d_next.toLoc(), tr("the ident '%1' after the END keyword must be equal "
                                  "to the procedure name").arg(d_next.d_val.constData()));
        }else if( kind == ProcCImp )
        {
            res->d_end = d_cur.toRowCol();
        }
    }
    return res.data();
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
    t->d_decl = p;
    if( !p->d_receiver.isNull() )
        t->d_typeBound = true;
    p->d_type = t.data();
    t->d_loc = p->d_loc;
    if(d_mod->d_externC )
    {
        t->d_unsafe = true;
        p->d_unsafe = true;
    }
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

bool Parser::procedureBody(Procedure* p)
{
    declarationSequence(false, p);
    bool hasBody = false;
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
            hasBody = true;
            next();
            p->d_body = statementSequence(p);
        }
#else
        hasBody = true;
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
    MATCH( Tok_END, tr("expecting a statement or closing END") );
    p->d_end = d_cur.toRowCol();
    p->d_noBody = !hasBody;
    return hasBody;
}

Ref<Parameter> Parser::receiver()
{
    MATCH( Tok_Lpar, tr("expecting '(' to start a receiver") );

    Ref<Parameter> v = new Parameter();
    v->d_receiver = true;

    if( d_la == Tok_VAR || d_la == Tok_IN )
    {
        next();
        v->d_var = true;
        if( d_cur.d_type == Tok_IN )
            v->d_const = true;
    }
    MATCH( Tok_ident, tr("expecting the receiver variable name") );
    v->d_name = d_cur.d_val;
    v->d_loc = d_cur.toRowCol();

    MATCH( Tok_Colon, tr("expecting ':'") );

    // like namedType, but only one ident, not qualident
    MATCH( Tok_ident, tr("expecting the type name") );
    Ref<IdentLeaf> id = new IdentLeaf();
    id->d_name = d_cur.d_val;
    id->d_role = MethRole;
    id->d_loc = d_cur.toRowCol();
    id->d_mod = d_mod.data();

    Ref<QualiType> q = new QualiType();
    q->d_quali = id.data();
    q->d_decl = v.data();
    v->d_type = q.data();

    MATCH( Tok_Rpar, tr("expecting ')' to end a receiver") );

    return v;
}

void Parser::declarationSequence(bool definition, Scope* scope )
{
    static const TokSet toks = TokSet() << Tok_CONST << Tok_PROC << Tok_PROCEDURE << Tok_TYPE << Tok_VAR << Tok_IMPORT;
    if( d_sync )
        sync(toks);

    while( d_la == Tok_CONST || d_la == Tok_TYPE || d_la == Tok_VAR ||
           d_la == Tok_PROCEDURE || d_la == Tok_PROC || d_la == Tok_IMPORT )
    {
        switch( d_la )
        {
        case Tok_IMPORT:
            Q_ASSERT( scope );
            if( scope->getTag() != Thing::T_Module )
                syntaxError( tr("IMPORT only supported on module level") );
            importList();
            break;
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
            {
                Procedure* p = procedureDeclaration(definition,scope);
                if( definition && d_la == Tok_Lbrack )
                    p->d_sysAttrs = systemAttrs();
                if( d_la == Tok_Semi )
                    next();
            }
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
    else
    {
        if( !firstOfStatement(d_la) && !followOfStatement(d_la) )
            // if what follows is not what is expected in an expression less return then try expression;
            // in this case the user has likely forgot to specify a return value of the procedure
            r->d_what = expression();
    }

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
    while( d_la == Tok_Comma || d_la == Tok_ident ) // comma is optional
    {
        if( d_la == Tok_Comma )
            next();
        MATCH( Tok_ident, tr("expecting formal parameter name") );
        if( d_cur.isValid() )
            names << d_cur;
    }
    MATCH( Tok_Colon, tr("expecting ':' to separate parameter names from their type") );
    if( !d_cur.isValid() )
        return false;
    Ref<Type> t = formalType(scope,pt);

    foreach( const Token& name, names )
    {
        Ref<Parameter> p = new Parameter();
        p->d_type = t;
        p->d_name = name.d_val;
        p->d_loc = name.toRowCol();
        p->d_var = var;
        p->d_const = in;
        p->d_unsafe = pt->d_unsafe;
        if( pt->find( p->d_name ) != 0 )
            semanticError(p->d_loc,tr("name is not unique in parameter list") );
        pt->d_formals.append(p);
    }
    Q_ASSERT( !pt->d_formals.isEmpty() );
    if( t )
        t->d_decl = pt->d_formals.first().data();
    return true;
}

Ref<Type> Parser::formalType(Scope* scope, Type* binding)
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
    return type(scope,0,binding);
#endif
}

void Parser::importList()
{
    MATCH( Tok_IMPORT, tr("expecting the IMPORT keyword") );
    import();
    while( d_la == Tok_Comma || d_la == Tok_ident ) // comma is optional
    {
        if( d_la == Tok_Comma )
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
    bool hasAlias = false;

    if( peek(2) == Tok_ColonEq )
    {
        MATCH( Tok_ident, tr("expecting import name alias") );
        if( d_cur.isValid() )
        {
            name = d_cur;
            imp->d_aliasPos = d_cur.toRowCol();
        }else
            hasErr = true;
        hasAlias = true;
        next(); // :=
    }
    MATCH( Tok_ident, tr("expecting external module name or path") );
    if( d_cur.isValid() )
    {
        suff = d_cur;
        imp->d_path << suff.d_val;
    }else
        hasErr = true;
    while( d_la == Tok_Slash || d_la == Tok_Dot )
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
    if( d_la == Tok_Lt || d_la == Tok_Lpar )
    {
        imp->d_metaActuals = typeActuals();
    }
    if( !hasAlias ) // no alias present
        imp->d_name = suff.d_val;
    else
        imp->d_name = name.d_val;
    imp->d_loc = suff.toRowCol();

    if( !d_mod->add( imp.data() ) )
    {
        semanticError( imp->d_aliasPos.isValid() ? imp->d_aliasPos : imp->d_loc,
                       tr("name '%1' is not unique in module; use a unique name alias (ident := path)").arg(imp->d_name.constData()) );
        hasErr = true;
    }else
        d_mod->d_imports << imp.data();

    if( hasErr )
    {
        imp->d_hasErrors = true;
        d_mod->d_hasErrors = true;
    }
}

Ref<Expression> Parser::systemFlag()
{
    Ref<Expression> res;
    if( d_la == Tok_Lbrack )
    {
        MATCH( Tok_Lbrack, tr("expecting '['") );
        res = expression();
        MATCH( Tok_Rbrack, tr("expecting ']'") );
    }
    return res;
}

SysAttrs Parser::systemAttrs()
{
    SysAttrs res;
    MATCH( Tok_Lbrack, tr("expecting '['") );
    while( d_la != Tok_Rbrack )
    {
        MATCH( Tok_ident, tr("expecting an identifier") );
        Ref<SysAttr> attr = new SysAttr();
        attr->d_loc = d_cur.toRowCol();
        attr->d_name = d_cur.d_val;
        while( d_la != Tok_Comma && d_la != Tok_Rbrack )
            attr->d_valExpr.append( constExpression() );
        if( res.contains(attr->d_name ) )
            semanticError(attr->d_loc, tr("duplicate system attribute name") );
        else
            res.insert(attr->d_name,attr);
        if( d_la == Tok_Comma )
            next();
    }
    if( res.isEmpty() )
    {
        Ref<SysAttr> attr = new SysAttr(); // at least one (empty) attr
        attr->d_loc = d_cur.toRowCol();
        res.insert(attr->d_name,attr);
    }
    MATCH( Tok_Rbrack, tr("expecting ']'") );
    return res;
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

void Parser::warning(const Loc& l, const QString& err)
{
    d_errs->warning( Errors::Semantics, l, err );
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

