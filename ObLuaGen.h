#ifndef OBLUAGEN_H
#define OBLUAGEN_H

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

#include <Oberon/ObCodeModel.h>
#include <Oberon/ObErrors.h>
#include <QTextStream>

namespace Ob
{	
    class LuaGen : public QObject
    {
    public:
        LuaGen(CodeModel*);
        bool emitModules(const QString& outdir, const QString& mod );
        QByteArray emitModule(const CodeModel::Module*);
        static QByteArray escape( const QByteArray& );
    protected:
        void preprocVarParams(const CodeModel::Unit*, const SynTree* stmt, QTextStream& out, int level );
        void preprocVarParams(const CodeModel::Unit*, const CodeModel::DesigOpList&, QTextStream& out, int level );
        static QString ws(int level);
        void emitExpression( const CodeModel::Unit*, const SynTree*, QTextStream&, int level );
        void emitSimpleExpression(const CodeModel::Unit* ds,const SynTree* st, QTextStream& out, int level);
        void emitTerm(const CodeModel::Unit* ds,const SynTree* st, QTextStream& out, int level );
        void emitTerm(const CodeModel::Unit* ds,const QList<SynTree*> st, int i, QTextStream& out, int level );
        void emitFactor(const CodeModel::Unit* ds,const SynTree* st, QTextStream& out, int level );
        void emitLiteral(const CodeModel::Unit* ds,const SynTree* st, QTextStream& out, int level );
        bool emitDesigList(const CodeModel::Unit*,const CodeModel::DesigOpList&, bool procCall, QTextStream&, int level);
        void emitAssig(const CodeModel::Unit* ds, const SynTree*,QTextStream& out, int level);
        bool emitAssig(const CodeModel::Unit* ds, const SynTree* lhs, const SynTree* rhs,QTextStream& out, int level);
        bool emitDesig(const CodeModel::Unit*, const CodeModel::NamedThing* prev, const CodeModel::DesigOp&, QTextStream&, int level);
        void emitTypeDecl(const CodeModel::Unit*,const CodeModel::Type* , QTextStream&, int level);
        void emitVarDecl(const CodeModel::Unit*, const CodeModel::Element* st, QTextStream&, int level);
        void emitProc(const CodeModel::Unit*, const CodeModel::Procedure*p, QTextStream&out, int level);
        void emitDecls(const CodeModel::Unit*, QTextStream&, int level);
        void emitStatementSeq( const CodeModel::Unit*, const QList<SynTree*>& seq, QTextStream&, int level );
        bool emitPredefProc( const CodeModel::Unit*, const CodeModel::DesigOpList&, QTextStream&, int level );
        void emitIfStatement(const CodeModel::Unit* ds, const SynTree*,QTextStream& out, int level);
        void emitWhileStatement(const CodeModel::Unit* ds, const SynTree*,QTextStream& out, int level);
        void emitCaseStatement(const CodeModel::Unit* ds, const SynTree*,QTextStream& out, int level);
        void emitRepeatStatement(const CodeModel::Unit* ds, const SynTree*,QTextStream& out, int level);
        void emitForStatement(const CodeModel::Unit* ds, const SynTree*,QTextStream& out, int level);
        void emitSet( const CodeModel::Unit*, const SynTree*,QTextStream& out, int level );
        void emitComment(const SynTree*, QTextStream& out, int level);
        void emitLabel( const SynTree*, QTextStream& out );
        void emitActualParam( const CodeModel::Unit*, const SynTree*, bool isVarParam, QTextStream& out, int level );
        void emitVarThunk(const CodeModel::Unit*, const SynTree*, QTextStream& out, int level );
        void emitArrayOp(const CodeModel::Unit*, const SynTree*, QTextStream& out, int level , bool omitLast = false);
        void initMatrix(const CodeModel::Unit*, QTextStream& out, const QList<const CodeModel::Type*>& mat, const QByteArray& name, int level , int i, const CodeModel::Type* rec);
        void initRecord(const CodeModel::Unit*, QTextStream& out, const CodeModel::Type* rec, const QByteArray& name, bool var, int level , bool label);
        void initArray(const CodeModel::Unit*, QTextStream& out, const CodeModel::Type* arr, const QByteArray& name, int level , bool label);
        typedef QPair<const CodeModel::Type*,const CodeModel::Type*> RecRef; // ref -> rec
        RecRef getRecRefFrom( const CodeModel::Unit* ds, SynTree* );
        bool error(Errors::Source s, const SynTree*, const QString& msg);
        bool warning(Errors::Source s, const SynTree*, const QString& msg);
    private:
        CodeModel* d_mdl;
        Errors* d_errs;
        const CodeModel::Module* d_curMod;
        QList<Token> d_cmts;
        QHash<const SynTree*,QByteArray> d_thunkNames;
        quint16 d_nextCmt;
        bool d_suppressVar;
    };
}

#endif // OBLUAGEN_H
