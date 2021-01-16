#ifndef OBXEVALUATOR_H
#define OBXEVALUATOR_H

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

#include <Oberon/ObxAst.h>
#include <Oberon/ObErrors.h>

namespace Obx
{
    class Evaluator : public QObject // for tr()
    {
    public:
        Evaluator();

        QVariant eval( Expression*, Module*, Ob::Errors* = 0 );

    protected:
        QVariant eval(Expression* e);

        QVariant NEG(const QVariant&, Expression* e );
        QVariant NOT(const QVariant&, Expression* e );
        QVariant ADD(const QVariant& lhs, const QVariant& rhs, Expression* e );
        QVariant SUB(const QVariant& lhs, const QVariant& rhs, Expression* e );
        QVariant FDIV(const QVariant& lhs, const QVariant& rhs, Expression* e );
        QVariant MUL(const QVariant& lhs, const QVariant& rhs, Expression* e );
        QVariant DIV(const QVariant& lhs, const QVariant& rhs, Expression* e );
        QVariant MOD(const QVariant& lhs, const QVariant& rhs, Expression* e );
        QVariant AND(const QVariant& lhs, const QVariant& rhs, Expression* e );
        QVariant OR(const QVariant& lhs, const QVariant& rhs, Expression* e );
        QVariant EQ(const QVariant& lhs, const QVariant& rhs, Expression* e );
        QVariant NEQ(const QVariant& lhs, const QVariant& rhs, Expression* e );
        QVariant LE(const QVariant& lhs, const QVariant& rhs, Expression* e );
        QVariant LEQ(const QVariant& lhs, const QVariant& rhs, Expression* e );
        QVariant GT(const QVariant& lhs, const QVariant& rhs, Expression* e );
        QVariant GEQ(const QVariant& lhs, const QVariant& rhs, Expression* e );
        QVariant IN(const QVariant& lhs, const QVariant& rhs, Expression* e );
        QVariant binOp(BinExpr* e, const QVariant& lhs, const QVariant& rhs );
        QVariant unOp(UnExpr* e, const QVariant& rhs );

        QVariant expErr( Expression* e, const QString& msg );
        QVariant evalNamedConst(Expression* e);
        QVariant evalBuiltIn( BuiltIn* f, ArgExpr* );
        bool setSet( Set& s, Expression* e );
        bool setSet( Set& s, Expression* lhs, Expression* rhs );

    private:
        Ob::Errors* d_err;
        Module* d_mod;
    };
}

#endif // OBXEVALUATOR_H
