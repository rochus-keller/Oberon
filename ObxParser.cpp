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
        d_cur = Token(); \
        syntaxError( msg ); \
    }

struct MaximumErrorCountExceeded {};
struct ForceEndOfFile {};

Parser::Parser(Ob::Lexer* l, Ob::Errors* e, QObject *parent) : QObject(parent),d_lex(l),d_errs(e),
    d_errCount(0)
{

}

bool Parser::parse()
{
    d_errCount = 0;
    next();
    try
    {
        while( d_la == Tok_MODULE || d_la == Tok_DEFINITION )
        {
            switch( d_la )
            {
            case Tok_MODULE:
                module();
                break;
            case Tok_DEFINITION:
                definition();
                break;
            default:
                syntaxError( tr("expecting MODULE or DEFINITION keyword"));
                break;
            }
        }
    }catch( const MaximumErrorCountExceeded& )
    {
        d_errs->error( Errors::Syntax, d_cur.toLoc(), tr("maximum number of errors exceeded, stop parsing") );
    }catch( const ForceEndOfFile& )
    {
        // ok
        if( peek(1) != Tok_Eof )
            d_errs->warning( Errors::Syntax, d_cur.toLoc(), tr("ignoring text after first terminating '.'") );
    }
    return d_errCount == 0;
}

void Parser::module()
{
    next();
    MATCH( Tok_ident, tr("expecting module name after MODULE keyword") );
    const Token id = d_cur;
    if( d_la == Tok_Semi )
        next();
    if( d_la == Tok_IMPORT )
        importList();
    declarationSequence(false);
    if( d_la == Tok_BEGIN || d_la == Tok_DO )
    {
        next();
        statementSequence();
    }
    MATCH( Tok_END, tr("expecting END keyword at the end of the module") );
    MATCH( Tok_ident, tr("expecting module name after END keyword") );
    if( d_cur.d_val != id.d_val )
        semanticError( tr("the ident '%1' after the END keyword must be equal "
                          "to the module name").arg(d_cur.d_val.constData()));
    if( d_la == Tok_Dot )
    {
        next();
        throw ForceEndOfFile();
    }
}

void Parser::definition()
{
    next();
    MATCH( Tok_ident, tr("expecting module name after MODULE keyword") );
    const Token id = d_cur;
    if( d_la == Tok_Semi )
        next();
    if( d_la == Tok_IMPORT )
        importList();
    declarationSequence(true);
    MATCH( Tok_END, tr("expecting END keyword at the end of the definition") );
    MATCH( Tok_ident, tr("expecting definition name after END keyword") );
    if( d_cur.d_val != id.d_val )
        semanticError( tr("the ident '%1' after the END keyword must be equal "
                          "to the definition name").arg(d_cur.d_val.constData()));
    if( d_la == Tok_Dot )
    {
        next();
        throw ForceEndOfFile();
    }
}

void Parser::number()
{
    switch( d_la )
    {
    case Tok_integer:
        next();
        break;
    case Tok_real:
        next();
        break;
    default:
        syntaxError( tr("expecting a number") );
        break;
    }
}

void Parser::qualident()
{
    if( d_la == Tok_ident && peek(2) == Tok_Dot )
    {
        next(); // ident
        next(); // dot
    }
    MATCH( Tok_ident, tr("expecting an identifier") );
}

void Parser::identdef(Token& id)
{
    MATCH( Tok_ident, tr("expecting an identifier") );
    id = d_cur;
    if( d_la == Tok_Star )
    {
        next();
    }
}

void Parser::constDeclaration()
{
    Token id;
    identdef(id);
    MATCH( Tok_Eq, tr("expecting '=' after ident in constant declaration") );
    constExpression();
}

void Parser::constExpression()
{
    expression();
}

static inline bool relation( quint16 t )
{
    switch(t)
    {
    case Tok_Eq:
    case Tok_Hash:
    case Tok_Lt:
    case Tok_Leq:
    case Tok_Gt:
    case Tok_Geq:
    case Tok_IN:
    case Tok_IS:
        return true;
    default:
        return false;
    }
}

void Parser::expression()
{
    simpleExpression();
    if( relation(d_la) )
    {
        next();
        simpleExpression();
    }
}

void Parser::typeDeclaration()
{
    Token id;
    identdef(id);
    if( d_la == Tok_Lt )
    {
        typeParams();
    }
    MATCH( Tok_Eq, tr("expecting '=' after ident in type declaration") );
    type();
}

void Parser::typeParams()
{
    MATCH( Tok_Lt, tr("expecting '<' to start type parameters") );
    MATCH( Tok_ident, tr("at least one identifier required as type parameter") );
    while( d_la == Tok_Comma )
    {
        next();
        MATCH( Tok_ident, tr("identifier expected in type parameter list") );
    }
    MATCH( Tok_Gt, tr("expecting '>' to end type parameters") );
}

void Parser::type()
{
    // TODO sync at start

    switch( d_la )
    {
    case Tok_ident:
        namedType();
        break;
    case Tok_Lpar:
        enumeration();
        break;
    case Tok_ARRAY:
    case Tok_Lbrack:
        arrayType();
        break;
    case Tok_RECORD:
        recordType();
        break;
    case Tok_POINTER:
    case Tok_Hat:
        pointerType();
        break;
    case Tok_PROCEDURE:
    case Tok_PROC:
        procedureType();
        break;
    default:
        syntaxError( tr("expecting type"));
        break;
    }
}

void Parser::typeActuals()
{
    MATCH( Tok_Lt, tr("expecting '<' to start type actuals") );
    typeActual();
    while( d_la == Tok_Comma )
    {
        next();
        typeActual();
    }
    MATCH( Tok_Gt, tr("expecting '>' to end type actuals") );
}

void Parser::enumeration()
{
    MATCH( Tok_Lt, tr("expecting '(' to start enumeration") );
    MATCH( Tok_ident, tr("at least one identifier required in enumeration") );
    while( d_la == Tok_Comma )
    {
        next();
        MATCH( Tok_ident, tr("identifier expected after ',' in enumeration") );
    }
    MATCH( Tok_Gt, tr("expecting ')' to end enumeration") );
}

void Parser::namedType()
{
    qualident();
    if( d_la == Tok_Lt )
    {
        typeActuals();
    }
}

void Parser::arrayType()
{
    if( d_la == Tok_ARRAY )
    {
        MATCH( Tok_ARRAY, tr("expecting the ARRAY keyword") );
        if( d_la != Tok_OF )
        {
            lengthList();
        }
        MATCH( Tok_OF, tr("expecting OF after ARRAY keyword or length list") );
    }else
    {
        MATCH( Tok_Lbrack, tr("expecting '['") );
        if( d_la != Tok_Rbrack )
        {
            lengthList();
        }
        MATCH( Tok_Rbrack, tr("expecting ']'") );
    }
    type();
}

void Parser::recordType()
{
    MATCH( Tok_RECORD, tr("expecting the RECORD keyword") );
    if( d_la == Tok_Lpar )
    {
        next();
        baseType();
        MATCH( Tok_Rpar, tr("expecting ')' after record base type") );
    }
    if( d_la != Tok_END )
        fieldListSequence();
    MATCH( Tok_END, tr("expecting END in record type") );
}

void Parser::pointerType()
{
    if( d_la == Tok_Hat )
    {
        next();
    }else
    {
        MATCH( Tok_POINTER, tr("expecting the POINTER keyword") );
        MATCH( Tok_TO, tr("expecting the TO keyword after the POINTER keyword") );
    }
    type();
}

void Parser::procedureType()
{
    if( d_la == Tok_PROC )
    {
        MATCH( Tok_PROC, tr("expecting the PROCEDURE keyword") );
    }else
    {
        MATCH( Tok_PROCEDURE, tr("expecting the PROCEDURE keyword") );
    }
    if( d_la == Tok_Lpar )
    {
        formalParameters();
    }
}

void Parser::typeActual()
{
    switch( d_la )
    {
    case Tok_ident:
    case Tok_Lpar:
    case Tok_ARRAY:
    case Tok_Lbrack:
    case Tok_RECORD:
    case Tok_POINTER:
    case Tok_Hat:
    case Tok_PROCEDURE:
    case Tok_PROC:
        type();
        break;
    case Tok_integer:
    case Tok_real:
    case Tok_string:
    case Tok_hexstring:
    case Tok_hexchar:
    case Tok_NIL:
    case Tok_TRUE:
    case Tok_FALSE:
    case Tok_Lbrace:
        literal();
        break;
    default:
        syntaxError(tr("expecting type or literal as actual type parameters"));
        break;
    }
}

void Parser::literal()
{
    switch( d_la )
    {
    case Tok_integer:
    case Tok_real:
        number();
        break;
    case Tok_string:
    case Tok_hexstring:
    case Tok_hexchar:
    case Tok_NIL:
    case Tok_TRUE:
    case Tok_FALSE:
        next();
        break;
    case Tok_Lbrace:
        set();
        break;
    default:
        syntaxError(tr("expecting type or literal as actual type parameters"));
        break;
    }
}

void Parser::lengthList()
{
    length();
    while( d_la == Tok_Comma )
    {
        next();
        length();
    }
}

void Parser::length()
{
    constExpression();
}

void Parser::baseType()
{
    qualident();
}

void Parser::fieldListSequence()
{
    fieldList();
    if( d_la == Tok_Semi )
    {
        next();
    }
    while( d_la == Tok_ident )
    {
        fieldList();
        if( d_la == Tok_Semi )
        {
            next();
        }
    }
}

void Parser::fieldList()
{
    identList();
    MATCH( Tok_Colon, tr("expecting ':' between identifier list and type in field list") );
    type();
}

void Parser::identList()
{
    Token id;
    identdef(id);
    while( d_la == Tok_Comma )
    {
        next();
        identdef(id);
    }
}

void Parser::formalParameters()
{
    MATCH( Tok_Lpar, tr("expecting '(' to start formal parameter list") );
    if( d_la != Tok_Rpar )
    {
        fPSection();
        while( d_la != Tok_Rpar )
        {
            if( d_la == Tok_Semi )
                next();
            fPSection();
        }
    }
    MATCH( Tok_Rpar, tr("expecting ')' at the end of the formal parameter list") );
    if( d_la == Tok_Colon )
    {
        next();
        qualident();
    }
}

void Parser::variableDeclaration()
{
    identList();
    MATCH( Tok_Colon, tr("expecting ':' between identifier list and tape in variable declaration") );
    type();
}

void Parser::designator()
{
    qualident();
    while( d_la == Tok_Dot || d_la == Tok_Lbrack || d_la == Tok_Hat || d_la == Tok_Lpar )
    {
        selector();
    }
}

void Parser::selector()
{
    switch( d_la )
    {
    case Tok_Dot:
        next();
        MATCH( Tok_ident, tr("expecting ident after '.' in selector") );
        break;
    case Tok_Lbrack:
        next();
        expList();
        MATCH( Tok_Rbrack, tr("expecting ']' to terminate expression list in index") );
        break;
    case Tok_Hat:
        next();
        break;
    case Tok_Lpar:
        next();
        if( d_la != Tok_Rpar )
            expList();
        MATCH( Tok_Rpar, tr("expecting ')' to terminate typeguard or actual parameter list") );
        break;
    default:
        syntaxError( tr("invalid selector") );
        break;
    }
}

void Parser::expList()
{
    expression();
    while( d_la == Tok_Comma )
    {
        next();
        expression();
    }
}

void Parser::simpleExpression()
{
    if( d_la == Tok_Plus || d_la == Tok_Minus )
    {
        next();
    }
    term();
    while( d_la == Tok_Plus || d_la == Tok_Minus || d_la == Tok_OR )
    {
        next();
        term();
    }
}

void Parser::term()
{
    factor();
    while( d_la == Tok_Star || d_la == Tok_Slash || d_la == Tok_DIV || d_la == Tok_MOD || d_la == Tok_Amp )
    {
        next();
        factor();
    }
}

void Parser::factor()
{
    // TODO sync at start
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
        literal();
        break;
    case Tok_ident:
        variableOrFunctionCall();
        break;
    case Tok_Lpar:
        next();
        expression();
        MATCH( Tok_Rpar, tr("expecting ')' to terminate sub-expression") );
        break;
    case Tok_Tilde:
        next();
        factor();
        break;
    default:
        syntaxError( tr("invalid factor") );
        break;
    }
}

void Parser::set()
{
    MATCH( Tok_Lbrace, tr("expecting '{' to start set literal") );
    if( d_la != Tok_Rbrace )
    {
        element();
        while( d_la == Tok_Comma )
        {
            next();
            element();
        }
    }
    MATCH( Tok_Rbrace, tr("expecting '}' to end set literal") );
}

void Parser::variableOrFunctionCall()
{
    designator();
}

void Parser::element()
{
    expression();
    if( d_la == Tok_2Dot )
    {
        next();
        expression();
    }
}

void Parser::statement()
{
    switch( d_la )
    {
    case Tok_ident:
        assignmentOrProcedureCall();
        break;
    case Tok_IF:
        ifStatement();
        break;
    case Tok_CASE:
        caseStatement();
        break;
    case Tok_WHILE:
        whileStatement();
        break;
    case Tok_REPEAT:
        repeatStatement();
        break;
    case Tok_FOR:
        forStatement();
        break;
    default:
        syntaxError( tr( "invalid statement" ) );
        break;
    }
}

void Parser::assignmentOrProcedureCall()
{
    designator();
    if( d_la == Tok_ColonEq )
    {
        next();
        expression();
    }
}

void Parser::ifStatement()
{
    MATCH( Tok_IF, tr("expecting the IF keyword") );
    expression();
    MATCH( Tok_THEN, tr("expecting the THEN keyword after the IF expression") );
    statementSequence();
    while( d_la == Tok_ELSIF )
    {
        elsifStatement(false);
    }
    if( d_la == Tok_ELSE )
    {
        elseStatement();
    }
    MATCH( Tok_END, tr("expecting a closing END in an IF statement") );
}

void Parser::caseStatement()
{
    MATCH( Tok_CASE, tr("expecting the CASE keyword") );
    expression();
    MATCH( Tok_OF, tr("expecting the OF keyword after the CASE expression") );
    case_();
    while( d_la == Tok_Bar )
    {
        next();
        case_();
    }
    MATCH( Tok_END, tr("expecting a closing END in a CASE statement") );
}

void Parser::whileStatement()
{
    MATCH( Tok_WHILE, tr("expecting the WHILE keyword") );
    expression();
    MATCH( Tok_DO, tr("expecting the DO keyword after the WHILE expression") );
    statementSequence();
    while( d_la == Tok_ELSIF )
    {
        elsifStatement(true);
    }
    MATCH( Tok_END, tr("expecting a closing END in an WHILE statement") );
}

void Parser::repeatStatement()
{
    MATCH( Tok_REPEAT, tr("expecting the REPEAT keyword") );
    statementSequence();
    MATCH( Tok_UNTIL, tr("expecting the UNTIL keyword in a REPEAT statement") );
    expression();
}

void Parser::forStatement()
{
    MATCH( Tok_FOR, tr("expecting the FOR keyword") );
    MATCH( Tok_ident, tr("expecting an identifier after the FOR keyword") );
    MATCH( Tok_ColonEq, tr("expecting ':=' to assign the start value of the FOR statement") );
    expression();
    MATCH( Tok_TO, tr("expecting the TO keyword") );
    expression();
    if( d_la == Tok_BY )
    {
        next();
        constExpression();
    }
    MATCH( Tok_DO, tr("expecting the DO keyword") );
    statementSequence();
    MATCH( Tok_END, tr("expecting a closing END in an FOR statement") );
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
        return true;
    default:
        return false;
    }
}

void Parser::statementSequence()
{
    // TODO sync at start if no statement first token read

    while( d_la == Tok_Semi )
    {
        next();
    }
    while( firstOfStatement(d_la) )
    {
        statement();
        while( d_la == Tok_Semi )
        {
            next();
        }
    }
}

void Parser::elsifStatement(bool inWhile)
{
    MATCH( Tok_ELSIF, tr("expecting the ELSIF keyword") );
    expression();
    if( inWhile )
    {
        MATCH( Tok_DO, tr("expecting the DO keyword after an ELSIF expression") );
    }else
    {
        MATCH( Tok_THEN, tr("expecting the THEN keyword after an ELSIF expression") );
    }
    statementSequence();
}

void Parser::elseStatement()
{
    MATCH( Tok_ELSE, tr("expecting the ELSE keyword") );
    statementSequence();
}

void Parser::case_()
{
    switch( d_la )
    {
    case Tok_ident:
    case Tok_integer:
    case Tok_string:
    case Tok_hexchar:
    case Tok_hexstring:
        caseLabelList();
        MATCH( Tok_Colon, tr("expecting ':' after the case label list") );
        statementSequence();
        break;

    // follows
    case Tok_Bar:
    case Tok_END:
        break;

    default:
        syntaxError( tr("invalid case") );
        break;
    }
}

void Parser::caseLabelList()
{
    labelRange();
    while( d_la == Tok_Comma )
    {
        next();
        labelRange();
    }
}

void Parser::labelRange()
{
    label();
    if( d_la == Tok_2Dot )
    {
        next();
        label();
    }
}

void Parser::label()
{
    switch( d_la )
    {
    case Tok_integer:
        next();
        break;
    case Tok_string:
        next();
        break;
    case Tok_hexchar:
        next();
        break;
    case Tok_hexstring:
        next();
        break;
    case Tok_ident:
        qualident();
        break;
    default:
        syntaxError( tr("invalid label") );
    }
}

void Parser::procedureDeclaration()
{
    Token id;
    procedureHeading(id);
    if( d_la == Tok_Semi )
    {
        next();
    }
    procedureBody();
    MATCH( Tok_ident, tr("expecting procedure name after END keyword") );
    if( d_cur.d_val != id.d_val )
        semanticError( tr("the ident '%1' after the END keyword must be equal "
                          "to the procedure name").arg(d_cur.d_val.constData()));
}

void Parser::procedureHeading(Token& id)
{
    if( d_la == Tok_PROC )
    {
        MATCH( Tok_PROC, tr("expecting the PROCEDURE keyword") );
    }else
    {
        MATCH( Tok_PROCEDURE, tr("expecting the PROCEDURE keyword") );
    }
    if( d_la == Tok_Lt )
    {
        typeParams();
    }
    if( d_la == Tok_Lpar )
    {
        receiver();
    }
    identdef(id);
    if( d_la == Tok_Lpar )
    {
        formalParameters();
    }
}

void Parser::procedureBody()
{
    declarationSequence(false);
    if( d_la == Tok_BEGIN || d_la == Tok_DO )
    {
        next();
        statementSequence();
    }
    if( d_la == Tok_RETURN )
    {
        returnStatement();
    }
    MATCH( Tok_END, tr("expecting a closing END in procedure") );
}

void Parser::receiver()
{
    MATCH( Tok_Lpar, tr("expecting '(' to start a receiver") );
    if( d_la == Tok_VAR )
    {
        next();
    }
    MATCH( Tok_ident, tr("expecting the receiver variable name") );
    MATCH( Tok_Colon, tr("expecting ':'") );
    MATCH( Tok_ident, tr("expecting the type name") );
    if( d_la == Tok_Lt )
    {
        typeActuals();
    }
    MATCH( Tok_Rpar, tr("expecting ')' to end a receiver") );

}

void Parser::declarationSequence(bool definition)
{
    // TODO sync at start
    if( d_la == Tok_CONST )
    {
        next();
        while( d_la == Tok_ident )
        {
            constDeclaration();
            if( d_la == Tok_Semi )
                next();
        }
    }
    if( d_la == Tok_TYPE )
    {
        next();
        while( d_la == Tok_ident )
        {
            typeDeclaration();
            if( d_la == Tok_Semi )
                next();
        }
    }
    if( d_la == Tok_VAR )
    {
        next();
        while( d_la == Tok_ident )
        {
            variableDeclaration();
            if( d_la == Tok_Semi )
                next();
        }
    }
    while( d_la == Tok_PROCEDURE || d_la == Tok_PROC )
    {
        Token id;
        if( definition )
            procedureHeading(id);
        else
            procedureDeclaration();
        if( d_la == Tok_Semi )
            next();
    }
}

void Parser::returnStatement()
{
    MATCH( Tok_RETURN, tr("expecting the RETURN keyword") );
    expression();
    if( d_la == Tok_Semi )
        next();
}

void Parser::fPSection()
{
    if( d_la == Tok_VAR || d_la == Tok_IN )
    {
        next();
    }
    MATCH( Tok_ident, tr("expecting formal parameter name") );
    while( d_la == Tok_Comma )
    {
        next();
        MATCH( Tok_ident, tr("expecting formal parameter name") );
    }
    MATCH( Tok_Colon, tr("expecting ':' to separate parameter names from their type") );
    formalType();
}

void Parser::formalType()
{
    if( d_la == Tok_Lbrack )
    {
        while( d_la == Tok_Lbrack )
        {
            next();
            MATCH( Tok_Rbrack, tr("expecting ']' after '['") );
        }
    }else
    {
        while( d_la == Tok_ARRAY )
        {
            next();
            MATCH( Tok_OF, tr("expecting the OF keyword after ARRAY") );
        }
    }
    qualident();
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
    if( peek(2) == Tok_ColonEq )
    {
        MATCH( Tok_ident, tr("expecting import name alias") );
        next(); // :=
    }
    MATCH( Tok_ident, tr("expecting external module name or path") );
    while( d_la == Tok_Slash )
    {
        next();
        MATCH( Tok_ident, tr("expecting external module path") );
    }
}

void Parser::next()
{
    if( d_errCount > 25 )
        throw MaximumErrorCountExceeded();
    d_cur = d_next;
    d_next = d_lex->nextToken();
    d_la = (Ob::TokenType)d_next.d_type;
}

bool Parser::sync(const Parser::TokSet& ts)
{
    if( ts.none() )
        return true;
    while( !ts.test(d_la) && d_la != Tok_Eof && d_la != Tok_Invalid )
        next();
    return ts.test(d_la);
}

bool Parser::match(quint8 tokenType, const QString& err)
{
    if( d_la == tokenType )
    {
        next();
        return true;
    }else
    {
        d_errs->error( Errors::Syntax, d_next.toLoc(), err );
        return false;
    }
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
    d_errCount++;
    d_errs->error( Errors::Syntax, d_next.toLoc(), err );
}

void Parser::semanticError(const QString& err)
{
    d_errCount++;
    d_errs->error( Errors::Semantics, d_cur.toLoc(), err );
}

