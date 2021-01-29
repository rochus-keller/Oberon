#ifndef OBOBXGEN_H
#define OBOBXGEN_H

/*
* Copyright 2020-2021 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include <Oberon/ObCodeModel.h>
#include <Oberon/ObErrors.h>
#include <QTextStream>

namespace Ob
{
    class ObxGen : public QObject
    {
    public:
        explicit ObxGen(CodeModel*);
        void setGenStubs(bool on) { d_genStubs = on; }
        bool emitModules(const QString& outdir, const QString& mod = QString() );
    protected:
        void emitModule(const CodeModel::Module*);

        void module(const CodeModel::Unit* u, SynTree* st);
        void module(const CodeModel::Module* m);
        void importList(const CodeModel::Unit* u, SynTree* st);
        void import(const CodeModel::Unit* u, SynTree* st);
        void ident(const CodeModel::Unit* u, SynTree* st, bool leaf = true);
        void declarationSequence(const CodeModel::Unit* u, SynTree* st);
        void declarationSequence2(const CodeModel::Unit* u, SynTree* st);
        void statementSequence(const CodeModel::Unit* u, SynTree* st);
        void constDeclaration(const CodeModel::Unit* u, SynTree* st);
        void typeDeclaration(const CodeModel::Unit* u, SynTree* st);
        void variableDeclaration(const CodeModel::Unit* u, SynTree* st);
        void procedureDeclaration(const CodeModel::Unit* u, SynTree* st);
        const CodeModel::Procedure* findProc(const CodeModel::Unit* u, SynTree* st);
        const CodeModel::Procedure* procedureHeading(const CodeModel::Unit* u, SynTree* st);
        void identdef(const CodeModel::Unit* u, SynTree* st);
        void constExpression(const CodeModel::Unit* u, SynTree* st);
        void expression(const CodeModel::Unit* u, SynTree* st);
        void simpleExpression(const CodeModel::Unit* u, SynTree* st);
        void term(const CodeModel::Unit* u, SynTree* st);
        void factor(const CodeModel::Unit* u, SynTree* st);
        void literal(const CodeModel::Unit* u, SynTree* st);
        void number(const CodeModel::Unit* u, SynTree* st);
        void variableOrFunctionCall(const CodeModel::Unit* u, SynTree* st);
        void designator(const CodeModel::Unit* u, SynTree* st);
        void set(const CodeModel::Unit* u, SynTree* st);
        void qualident(const CodeModel::Unit* u, SynTree* st);
        void selector(const CodeModel::Unit* u, SynTree* st);
        void expList(const CodeModel::Unit* u, SynTree* st);
        void type(const CodeModel::Unit* u, SynTree* st);
        SynTree* getTypeSysFlag(const CodeModel::Unit* u, SynTree* st);
        void type(const CodeModel::Unit* u, const CodeModel::Type* );
        void identList(const CodeModel::Unit* u, SynTree* st);
        void namedType(const CodeModel::Unit* u, SynTree* st);
        void arrayType(const CodeModel::Unit* u, SynTree* st);
        void recordType(const CodeModel::Unit* u, SynTree* st);
        void pointerType(const CodeModel::Unit* u, SynTree* st);
        void procedureType(const CodeModel::Unit* u, SynTree* st);
        void lengthList(const CodeModel::Unit* u, SynTree* st);
        void baseType(const CodeModel::Unit* u, SynTree* st);
        void sysFlag(const CodeModel::Unit* u, SynTree* st);
        void fieldListSequence(const CodeModel::Unit* u, SynTree* st);
        void fieldList(const CodeModel::Unit* u, SynTree* st);
        void element(const CodeModel::Unit* u, SynTree* st);
        void receiver(const CodeModel::Unit* u, SynTree* st);
        void formalParameters(const CodeModel::Unit* u, SynTree* st);
        void procedureBody(const CodeModel::Unit* u, SynTree* st);
        void fpSection(const CodeModel::Unit* u, SynTree* st);
        void formalType(const CodeModel::Unit* u, SynTree* st);
        void returnStatement(const CodeModel::Unit* u, SynTree* st);
        void statement(const CodeModel::Unit* u, SynTree* st);
        void assignment(const CodeModel::Unit* u, SynTree* st);
        void procedureCall(const CodeModel::Unit* u, SynTree* st);
        void actualParameters(const CodeModel::Unit* u, SynTree* st);
        void ifStatement(const CodeModel::Unit* u, SynTree* st);
        void caseStatement(const CodeModel::Unit* u, SynTree* st);
        void withStatement(const CodeModel::Unit* u, SynTree* st);
        void loopStatement(const CodeModel::Unit* u, SynTree* st);
        void exitStatement(const CodeModel::Unit* u, SynTree* st);
        void whileStatement(const CodeModel::Unit* u, SynTree* st);
        void repeatStatement(const CodeModel::Unit* u, SynTree* st);
        void forStatement(const CodeModel::Unit* u, SynTree* st);
        void elsifStatement(const CodeModel::Unit* u, SynTree* st);
        void elseStatement(const CodeModel::Unit* u, SynTree* st);
        void case_(const CodeModel::Unit* u, SynTree* st);
        void caseLabelList(const CodeModel::Unit* u, SynTree* st);
        void labelRange(const CodeModel::Unit* u, SynTree* st);
        void label(const CodeModel::Unit* u, SynTree* st);
        void guard(const CodeModel::Unit* u, SynTree* st);

        inline void semi(SynTree* st)
        {
            if( st && st->d_tok.d_lineNr == prevRow )
                out << "; ";
        }
        inline void space()
        {
            out << " ";
        }
        inline QString ind()
        {
            return QString(level,QChar('\t'));
        }
        QByteArray escapeName(const QByteArray& id);
        void print(const QByteArray&, SynTree* = 0);
        void print(SynTree* , bool toLower = false);
        void printComments(SynTree* st = 0);
        int printComment(const QByteArray& str, bool mayEndOfLine);
        void newLine(bool comment = true);
        void assureNl(SynTree* next, quint8 soll = 1, bool comment = true );
        void skipTo(SynTree* to);
        bool ifStatAfterBegin(SynTree* statSeq);
    private:
        CodeModel* d_mdl;
        Errors* d_errs;
        QString d_outdir, d_mod;
        QList<Token> d_cmts;
        quint16 d_nextCmt;
        quint16 d_nameNr;
        bool d_genStubs;
        QTextStream out;
        SynTree* root;
        SynTree* last;
        int level, prevRow;
        SynTree* prevSym;
        QSet<QByteArray> obxKeyWords;
        QSet<QByteArray> builtIns, obxBuiltIns;
        bool d_isDef;
        bool rowPrinted;

        // formating options
        bool nlAfterDeclHeader, nlPerDecl, nlPerStat,
            nlAfterBegin, // includes THEN, DO, etc.
            nlBeforeEnd, // includes ELSE, UNTIL, etc.
            switchBBoxTypes, // change REAL to LONGREAL, SHORTREAL to REAL, SHORTCHAR to CHAR, CHAR to WCHAR
            hasWchar,      // in case of switchBBoxTypes CHAR is mapped to WCHAR
            genSysFlags,
            genUnsafe
        ;
    };
}

#endif // OBOBXGEN_H
