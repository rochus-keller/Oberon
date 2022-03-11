#ifndef OBXPARSER_H
#define OBXPARSER_H

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

#include <QObject>
#include <Oberon/ObLexer.h>
#include <Oberon/ObxAst.h>
#include <bitset>

namespace Obx
{
    // This is the "Oberon with extensions" parser.
    // Official name is "Oberon+", abbreviation is OBX

    class Parser : public QObject
    {
    public:
        explicit Parser(Ob::Lexer*, Ob::Errors*, QObject *parent = 0);

        Ref<Module> parse();
    protected:
        bool module(bool definition);
        Ref<Literal> number();
        Ref<Expression> qualident();
        void identdef(Named* n, Scope* scope);
        void constDeclaration(Scope* scope);
        Ref<Expression> constExpression();
        Ref<Expression> expression();
        Ref<NamedType> typeDeclaration(Scope* scope);
        MetaParams typeParams();
        Ref<Type> type(Scope* scope, Named* id, Type* binding );
        MetaActuals typeActuals();
        Ref<Type> enumeration(Scope* scope, Named* id, Type* binding);
        Ref<QualiType> namedType(Named* id, Type* binding);
        Ref<Type> returnType(Scope* scope,Type* binding);
        Ref<Type> arrayType(Scope* scope, Named* id, Type* binding);
        Ref<Type> recordType(Scope* scope, Named* id, Type* binding);
        Ref<Type> pointerType(Scope* scope, Named* id, Type* binding);
        Ref<Type> procedureType(Scope* scope, Named* id, Type* binding);
        Ref<Type> typeActual();
        Ref<Expression> literal();
        QList< Ref<Expression> > lengthList();
        Ref<Expression> length();
        Ref<QualiType> baseType(Record* rec);
        void fieldListSequence(Scope* scope, Record*);
        void fieldList(Scope* scope, Record* r);
        void formalParameters(Scope*,ProcType* p);
        void variableDeclaration(Scope* scope);
        Ref<Expression> designator();
        Ref<UnExpr> selector();
        ExpList expList();
        Ref<Expression> simpleExpression();
        Ref<Expression> term();
        Ref<Expression> factor();
        Ref<Expression> set();
        Ref<Expression> variableOrFunctionCall();
        Ref<Expression> element();
        Ref<Statement> statement(Scope* scope);
        Ref<Statement> assignmentOrProcedureCall();
        Ref<Statement> ifStatement(Scope* scope);
        Ref<Statement> caseStatement(Scope* scope);
        Ref<Statement> whileStatement(Scope* scope);
        Ref<Statement> repeatStatement(Scope* scope);
        Ref<Statement> forStatement(Scope* scope);
        Ref<Statement> withStatement(Scope* scope);
        Ref<Statement> loopStatement(Scope* scope);
        Ref<Statement> exitStatement(Scope* scope);
        StatSeq statementSequence(Scope* scope);
        Ref<Expression> elsifStatement(Scope* scope, bool inWhile, StatSeq& seq);
        Ref<Expression> guard();
        void elseStatement(Scope* scope, StatSeq& seq);
        bool case_(Scope* scope, CaseStmt::Case& c);
        ExpList caseLabelList();
        Ref<Expression> labelRange();
        Ref<Expression> label();
        Procedure* procedureDeclaration(bool headingOnly, Scope* scope);
        enum { ProcNormal, ProcForward, ProcCImp };
        int procedureHeading(Procedure* proc, Scope* scope);
        bool procedureBody(Procedure* p);
        Ref<Parameter> receiver();
        void declarationSequence(bool definition, Scope* scope);
        Ref<Statement> returnStatement(Scope* scope);
        bool fPSection(Scope* scope, ProcType* p);
        Ref<Type> formalType(Scope* scope, Type* binding);
        void importList();
        void import();
        Ref<Expression> systemFlag();
        SysAttrs systemAttrs();

        void next();

        struct TokSet : public std::bitset<Ob::TT_MaxToken>
        {
            TokSet(){}
            TokSet(quint8 t1){ set(t1); }
            TokSet(quint8 t1, quint8 t2){ set(t1);set(t2); }
            TokSet& operator<<( int tok )
            {
                set(tok);
                return *this;
            }
        };
        bool sync( const TokSet& );
        quint8 peek(quint16 la);

        void syntaxError( const QString& err );
        void warning(const Ob::Loc&, const QString& err );
        void semanticError(const Ob::Loc&, const QString& err );
        void semanticError(const Ob::RowCol&, const QString& err );

        void addEnum( Scope* scope, Enumeration* e, const Ob::Token& t );
    private:
        Ob::Lexer* d_lex;
        Ob::Errors* d_errs;
        quint32 d_errCount;
        Ob::Token d_cur;
        Ob::Token d_next;
        Ref<Module> d_mod;
#ifdef _DEBUG
        Ob::TokenType d_la;
#else
        quint8 d_la; // Ob::TokenType
#endif
        bool d_sync;
    };
}

#endif // OBXPARSER_H
