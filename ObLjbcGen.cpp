#include "ObLjbcGen.h"
#include "ObErrors.h"
#include <QDir>
#include <QtDebug>
#include <LjTools/LuaJitComposer.h>
using namespace Ob;
using namespace Lua;

// register allocation
// Luajit lj_parse.c line 368
// https://github.com/deplinenoise/deluxe68
// https://gist.github.com/leegao/1372243
// ev. https://www.geeksforgeeks.org/register-allocations-in-code-generation/

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

    d_curMod = m;

    Lua::JitComposer jc;
    jc.openFunction(0,m->d_def->d_tok.d_sourcePath.toUtf8(),
                    m->d_def->d_tok.d_lineNr, m->d_def->d_children.last()->d_tok.d_lineNr );

    jc.addOp( JitBytecode::OP_TNEW, 0, 0, m->d_def->d_tok.d_lineNr );
    jc.addOp( JitBytecode::OP_GSET, 0, jc.getConstSlot( m->d_def->d_tok.d_val ), m->d_def->d_tok.d_lineNr );

    // TODO: handle imports
    // imports are either the global module tables or members of this module table pointing to the other module table
    // under a separate name

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
    // constants don't need a special treatment here; out.getConstSlot is called wherever the constant is required

    // variables could be slots of the top function (chunk) or entries in the module table or both
    // no initialization required

    // named record types should be a member of the module table to be used as meta and enable typeof tests
}

void LjbcGen::emitProc(const CodeModel::Unit* ds, Lua::JitComposer& out)
{
    // module level procs have to be members of the module table to be referencible from outside
    // pass by reference: http://lua-users.org/lists/lua-l/2004-06/msg00258.html
    // we cannot use upvals for VAR because in constrast to PUC Lua you cannot select the upval references with FNEW,
    // but they are predefined in the function; to use different variables as VAR param you need versions of the function.
}

void LjbcGen::emitStatementSeq(const CodeModel::Unit* ds, const QList<SynTree*>& seq, Lua::JitComposer& out)
{

}

