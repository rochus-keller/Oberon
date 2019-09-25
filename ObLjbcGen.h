#ifndef OBLJBCGEN_H
#define OBLJBCGEN_H

#include <Oberon/ObCodeModel.h>

namespace Lua
{
    class JitComposer;
}
namespace Ob
{
    class CodeModel;
    class Errors;

    class LjbcGen : public QObject
    {
    public:
        explicit LjbcGen(CodeModel*);
        bool emitModules(const QString& outdir );
        QByteArray emitModule( CodeModel::Module* );
    protected:
        void emitDecls(const CodeModel::Unit* ds, Lua::JitComposer& out );
        void emitProc(const CodeModel::Unit* ds, Lua::JitComposer& out );
        void emitStatementSeq(const CodeModel::Unit* ds, const QList<SynTree*>& seq, Lua::JitComposer& out );
    private:
        CodeModel* d_mdl;
        Errors* d_errs;
        CodeModel::Module* d_curMod;
    };
}

#endif // OBLJBCGEN_H
