#include "ObLjbcGen.h"
#include "ObErrors.h"
#include <QDir>
#include <QtDebug>
#include <LjTools/LuaJitComposer.h>
using namespace Ob;

LjbcGen::LjbcGen(CodeModel* mdl) : d_mdl(mdl),d_errs(0)
{
    Q_ASSERT( mdl != 0 );
    d_errs = mdl->getErrs();
}

bool LjbcGen::emitModules(const QString& outdir)
{
    QDir dir(outdir);

    const int precount = d_errs->getErrCount();
    foreach( CodeModel::Module* m, d_mdl->getGlobalScope().d_mods )
    {
        qDebug() << "translating module" << m->d_name;
        const QByteArray bc = emitModule(m);
        if( !bc.isEmpty() )
        {
            QFile out( dir.absoluteFilePath( m->d_name + ".bc" ) );
            if( !out.open(QIODevice::WriteOnly) )
                d_errs->error(Errors::Generator,m->d_name, 0,0,QString("cannot open file '%1' for writing").arg(out.fileName()) );
            else
                out.write(bc);
        }
    }
    return precount == int(d_errs->getErrCount());
}

QByteArray LjbcGen::emitModule(CodeModel::Module* m)
{
    Q_ASSERT( m && m->d_def );
    Lua::JitComposer jc;
    jc.openFunction(0,m->d_def->d_tok.d_sourcePath.toUtf8(),
                    m->d_def->d_tok.d_lineNr, m->d_def->d_children.last()->d_tok.d_lineNr );

    // TODO: handle imports

    // DeclarationSequence
    emitDecls( m, jc );

    foreach( const CodeModel::Procedure* v, m->d_procs )
        emitProc(v,jc);

    // StatementSequence
    emitStatementSeq(m, m->d_body, jc);

    jc.closeFunction();
    return QByteArray();
}

void LjbcGen::emitDecls(const CodeModel::Unit* ds, Lua::JitComposer& out)
{
    // A ConstExpression is an expression containing constants only. More precisely, its evaluation must
    // be possible by a mere textual scan without execution of the program
    QList<CodeModel::Element*> consts = ds->getConsts();
    foreach( const CodeModel::Element* c, consts )
    {
        qDebug() << SynTree::rToStr(c->d_st->d_tok.d_type);
        Q_ASSERT( c->d_st != 0 && c->d_st->d_tok.d_type == SynTree::R_expression );
        const QVariant val = d_mdl->evalExpression( ds, c->d_st );
        const int slot = out.getLocalSlot(c->d_name);
        if( val.canConvert<CodeModel::Set>() )
            qDebug() << "const Set" << c->d_name << val.value<CodeModel::Set>().to_string().c_str();
        else
            qDebug() << "const" << c->d_name << val;
    }

}

void LjbcGen::emitProc(const CodeModel::Unit* ds, Lua::JitComposer& out)
{

}

void LjbcGen::emitStatementSeq(const CodeModel::Unit* ds, const QList<SynTree*>& seq, Lua::JitComposer& out)
{

}

