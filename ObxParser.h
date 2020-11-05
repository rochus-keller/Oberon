#ifndef OBXPARSER_H
#define OBXPARSER_H

/*
* Copyright 2020 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon+ parser/code model library.
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
#include <bitset>

namespace Obx
{
    class Parser : public QObject
    {
    public:
        explicit Parser(Ob::Lexer*, Ob::Errors*, QObject *parent = 0);

        bool parse();
    protected:
        void module();
        void definition();
        void number();
        void qualident();
        void identdef(Ob::Token& id);
        void constDeclaration();
        void constExpression();
        void expression();
        void typeDeclaration();
        void typeParams();
        void type();
        void typeActuals();
        void enumeration();
        void arrayType();
        void recordType();
        void pointerType();
        void procedureType();
        void typeActual();
        void literal();
        void lengthList();
        void length();
        void baseType();
        void fieldListSequence();
        void fieldList();
        void identList();
        void formalParameters();
        void variableDeclaration();
        void designator();
        void selector();
        void expList();
        void simpleExpression();
        void term();
        void factor();
        void set();
        void variableOrFunctionCall();
        void element();
        void statement();
        void assignmentOrProcedureCall();
        void ifStatement();
        void caseStatement();
        void whileStatement();
        void repeatStatement();
        void forStatement();
        void statementSequence();
        void elsifStatement(bool inWhile);
        void elseStatement();
        void case_();
        void caseLabelList();
        void labelRange();
        void label();
        void procedureDeclaration();
        void procedureHeading(Ob::Token& id);
        void procedureBody();
        void receiver();
        void declarationSequence(bool definition);
        void returnStatement();
        void fPSection();
        void formalType();
        void importList();
        void import();

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
        bool match(quint8 tokenType, const QString& err );
        quint8 peek(quint16 la);

        void syntaxError( const QString& err );
        void semanticError( const QString& err );
    private:
        Ob::Lexer* d_lex;
        Ob::Errors* d_errs;
        quint32 d_errCount;
        Ob::Token d_cur;
        Ob::Token d_next;
#ifdef _DEBUG
        Ob::TokenType d_la;
#else
        quint8 d_la; // Ob::TokenType
#endif
    };
}

#endif // OBXPARSER_H
