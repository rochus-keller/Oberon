#ifndef OBLJBCGEN_H
#define OBLJBCGEN_H

#include <QObject>

namespace Ob
{
    class CodeModel;
    class Errors;

    class LjbcGen : public QObject
    {
    public:
        explicit LjbcGen(CodeModel*);
        bool emitModules(const QString& outdir );
    private:
        CodeModel* d_mdl;
        Errors* d_errs;
    };
}

#endif // OBLJBCGEN_H
