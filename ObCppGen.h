#ifndef OBCPPGEN_H
#define OBCPPGEN_H

/*
* Copyright 2019 Rochus Keller <mailto:me@rochus-keller.ch>
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
    // Automatically translate Lola-2 compiler to C++.
	
    class CppGen : public QObject
    {
    public:
        CppGen(CodeModel*);
        void setGenStubs(bool on) { d_genStubs = on; }
        bool emitModules(const QString& outdir, const QString& ns = QString(), const QString& mod = QString() );
        bool emitObnUtils( const QString& outdir, const QString& ns = QString(), const QString& mod = QString() );
    protected:
        void emitModule(const CodeModel::Module*);
        void emitHeader(const CodeModel::Module*, QTextStream& out, int level);
        void emitBody(const CodeModel::Module*, QTextStream& out);
        QString includeFileName( const QString& modName );
        QString ws(int level);
        QString basicType( const CodeModel::Type* );
        void emitType(const CodeModel::Unit*,const CodeModel::Type* , QTextStream&, int level);
        void emitProcType(const CodeModel::Unit*, const CodeModel::Type*, const QByteArray& name , QTextStream&, int level );
        void emitExpression( const CodeModel::Unit*, const SynTree*, QTextStream&, int level );
        void emitSimpleExpression(const CodeModel::Unit* ds,const SynTree* st, QTextStream& out, int level);
        void emitTerm(const CodeModel::Unit* ds,const SynTree* st, QTextStream& out, int level );
        void emitTerm(const CodeModel::Unit* ds,const QList<SynTree*> st, int i, QTextStream& out, int level );
        void emitFactor(const CodeModel::Unit* ds,const SynTree* st, QTextStream& out, int level );
        bool emitDesig(const CodeModel::Unit*,const SynTree* st, bool procCall, QTextStream&, int level);
        void emitTypeDecl(const CodeModel::Unit*,const CodeModel::Type* , QTextStream&, int level);
        void emitVarDecl(const CodeModel::Unit*, const CodeModel::Element* st, QTextStream&, int level);
        void emitStubProcDecl(const CodeModel::Unit*, const CodeModel::Element* st, QTextStream&, int level);
        void emitRecDecl(const CodeModel::Unit*,const CodeModel::Type*, const QByteArray& name, QTextStream&, int level);
        void emitProcDecl( const CodeModel::Procedure*, QTextStream&, int level);
        void emitProcParams(const CodeModel::Unit*, const QList<CodeModel::Element*>&, QTextStream&);
        void emitProcBody(const CodeModel::Procedure*p, QTextStream&out);
        void emitDecls(const CodeModel::Unit*, QTextStream&, int level);
        void emitStatementSeq( const CodeModel::Unit*, const QList<SynTree*>& seq, QTextStream&, int level );
        bool emitPredefProc( const CodeModel::Unit*, const CodeModel::DesigOpList&, QTextStream&, int level );
        void emitIfStatement(const CodeModel::Unit* ds, const SynTree*,QTextStream& out, int level);
        void emitWhileStatement(const CodeModel::Unit* ds, const SynTree*,QTextStream& out, int level);
        void emitRepeatStatement(const CodeModel::Unit* ds, const SynTree*,QTextStream& out, int level);
        void emitSet( const CodeModel::Unit*, const SynTree*,QTextStream& out, int level );
        void emitScopedName( const CodeModel::Unit*, const CodeModel::NamedThing*, QTextStream& out );
        void emitComment(const SynTree*, QTextStream& out, int level);
        static bool isSimpleType( const CodeModel::Type* );
        static QByteArray escape( const QByteArray& );
    private:
        CodeModel* d_mdl;
        Errors* d_errs;
        QString d_ns, d_outdir, d_mod;
        QList<Token> d_cmts;
        quint16 d_nextCmt;
        quint16 d_nameNr;
        bool d_genStubs;
    };
}

#endif // OBCPPGEN_H
