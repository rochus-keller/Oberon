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

#include "ObxEvaluator.h"
using namespace Obx;
using namespace Ob;

Q_DECLARE_METATYPE( Obx::Set )

QVariant Evaluator::expErr( Expression* e, const QString& msg )
{
    if( d_err )
        d_err->error( Errors::Semantics, d_mod->d_file, e->d_loc.d_row, e->d_loc.d_col, msg );
    return QVariant();
}

QVariant Evaluator::evalNamedConst(Expression* e)
{
    Named* n = e->getIdent();
    QVariant res;
    if( n && n->getTag() == Thing::T_Const )
        res = cast<Const*>(n)->d_val;
    if( !res.isValid() )
        return expErr( e, tr("not a const expression") );
    else
        return res;
}

bool Evaluator::setSet( Set& s, Expression* e )
{
    const QVariant v = eval(e);
    const qint64 b = v.toLongLong();
    if( v.type() != QVariant::LongLong || b < 0 || b >= SET_BIT_LEN )
    {
        expErr( e, tr("invalid set part") );
        return false;
    }
    s.set(b);
    return true;
}

bool Evaluator::setSet( Set& s, Expression* lhs, Expression* rhs )
{
    const QVariant lv = Evaluator::eval(lhs);
    const qint64 l = lv.toLongLong();
    const QVariant rv = Evaluator::eval(rhs);
    const qint64 r = rv.toLongLong();
    if( lv.type() != QVariant::LongLong || l < 0 || l >= SET_BIT_LEN ||
            rv.type() != QVariant::LongLong || r < 0 || r >= SET_BIT_LEN )
    {
        expErr( lhs, Evaluator::tr("invalid set part") );
        return false;
    }
    if( l <= r )
        for( int b = l; b <= r; b++ )
            s.set(b);
    else
        for( int b = r; b <= l; b++ )
            s.set(b);
    return true;
}

Evaluator::Evaluator():d_err(0),d_mod(0)
{

}

QVariant Evaluator::eval(Expression* e, Module* m, Errors* err)
{
    Q_ASSERT( m != 0 && e != 0 );
    d_err = err;
    d_mod = m;
    if( e == 0 )
        return QVariant();
    else
        return eval(e);
}

QVariant Evaluator::eval(Expression* e)
{
    switch( e->getTag() )
    {
    case Thing::T_Literal:
        return cast<Literal*>(e)->d_val;
    case Thing::T_SetExpr:
        {
            SetExpr* se = cast<SetExpr*>(e);
            Set s;
            for(int i = 0; i < se->d_parts.size(); i++ )
            {
                if( se->d_parts[i]->getTag() == Thing::T_BinExpr )
                {
                    BinExpr* be = cast<BinExpr*>(se->d_parts[i].data());
                    if( be->d_op != BinExpr::Range )
                        return expErr( e, tr("invalid set part") );
                    if( !setSet( s, be->d_lhs.data(), be->d_rhs.data() ) )
                        return QVariant();
                }else if( !setSet( s, se->d_parts[i].data() ) )
                        return QVariant();
            }
            return QVariant::fromValue(s);
        }
        break;
    case Thing::T_IdentLeaf:
    case Thing::T_IdentSel:
        return evalNamedConst(e);
    case Thing::T_UnExpr:
        {
            UnExpr* ue = cast<UnExpr*>(e);
            if( ue->d_op == UnExpr::SEL )
                return evalNamedConst(e);
            else
            {
                const QVariant v = eval(ue->d_sub.data());
                if( !v.isValid() )
                    return v;

                return unOp(ue, v);
            }
        }
        break;
    case Thing::T_BinExpr:
        {
            BinExpr* be = cast<BinExpr*>(e);
            switch( be->d_op )
            {
            case BinExpr::Range:
            case BinExpr::IS:
                return expErr(e,tr("operator not supported for constants"));
            }
            const QVariant lhs = eval(be->d_lhs.data() );
            if( !lhs.isValid() )
                return lhs;
            const QVariant rhs = eval(be->d_rhs.data() );
            if( !rhs.isValid() )
                return rhs;
            return binOp( be, lhs, rhs );
        }
        break;
    case Thing::T_ArgExpr:
        return expErr( e, tr("operation not supported in a const expression"));
    default:
        Q_ASSERT( false );
    }
    return QVariant();
}

QVariant Evaluator::NEG(const QVariant& v, Expression* e )
{
    if( v.type() == QVariant::Double )
        return -v.toDouble();
    if( v.type() == QVariant::LongLong )
        return -v.toLongLong();
    else
        return expErr( e,tr("cannot invert sign of non numerical expression"));
}

QVariant Evaluator::NOT(const QVariant& v, Expression* e )
{
    if( v.type() == QVariant::Bool )
        return !v.toBool();
    return expErr( e,tr("cannot negate non boolean expression"));
}

QVariant Evaluator::ADD(const QVariant& lhs, const QVariant& rhs, Expression* e )
{
    if( rhs.type() == QVariant::LongLong )
        return lhs.toLongLong() + rhs.toLongLong();
    else if( rhs.type() == QVariant::Double )
        return lhs.toDouble() + rhs.toDouble();
    else if( lhs.canConvert<Set>() && rhs.canConvert<Set>() )
        return QVariant::fromValue( lhs.value<Set>() | rhs.value<Set>() );
    else
        return expErr( e,tr("operand types incompatible with operator") );
}

QVariant Evaluator::SUB(const QVariant& lhs, const QVariant& rhs, Expression* e )
{
    if( rhs.type() == QVariant::LongLong )
        return lhs.toLongLong() - rhs.toLongLong();
    else if( rhs.type() == QVariant::Double )
        return lhs.toDouble() - rhs.toDouble();
    else if( lhs.canConvert<Set>() && rhs.canConvert<Set>() )
    {
        const Set a = lhs.value<Set>();
        const Set b = rhs.value<Set>();
        Set res;
        for( int j = 0; j < a.size(); j++ )
            res.set( a.test(j) && !b.test(j) );
        return QVariant::fromValue( res );
    }else
        return expErr(e,tr("operand types incompatible with operator") );
}

QVariant Evaluator::FDIV(const QVariant& lhs, const QVariant& rhs, Expression* e)
{
    if( rhs.type() == QVariant::LongLong )
        return lhs.toLongLong() / rhs.toLongLong();
    else if( rhs.type() == QVariant::Double )
        return lhs.toDouble() / rhs.toDouble();
    else if( lhs.canConvert<Set>() && rhs.canConvert<Set>() )
    {
        const Set a = lhs.value<Set>();
        const Set b = rhs.value<Set>();
        Set res;
        for( int j = 0; j < a.size(); j++ )
            res.set( ( a.test(j) || b.test(j) ) && !( a.test(j) && b.test(j) ) );
        return QVariant::fromValue( res );
    }else
        return expErr( e,tr("operand types incompatible with operator") );
}

QVariant Evaluator::MUL(const QVariant& lhs, const QVariant& rhs, Expression* e )
{
    if( rhs.type() == QVariant::LongLong )
        return lhs.toLongLong() * rhs.toLongLong();
    else if( rhs.type() == QVariant::Double )
        return lhs.toDouble() * rhs.toDouble();
    else if( lhs.canConvert<Set>() && rhs.canConvert<Set>() )
        return QVariant::fromValue( lhs.value<Set>() & rhs.value<Set>() );
    else
        return expErr( e,tr("operand types incompatible with operator") );
}

QVariant Evaluator::DIV(const QVariant& lhs, const QVariant& rhs, Expression* e )
{
    if( rhs.type() == QVariant::LongLong )
    {
        const qint64 a = lhs.toLongLong();
        const qint64 b = rhs.toLongLong();
        // res = ( a - ( ( a % b + b ) % b ) ) / b;
        // source: http://lists.inf.ethz.ch/pipermail/oberon/2019/013353.html
        if (a < 0)
            return qint64( (a - b + 1) / b );
        else
            return qint64( a / b );
    }else
        return expErr( e,tr("operand types incompatible with operator") );
}

QVariant Evaluator::MOD(const QVariant& lhs, const QVariant& rhs, Expression* e )
{
    if( rhs.type() == QVariant::LongLong )
    {
        const qint64 a = lhs.toLongLong();
        const qint64 b = rhs.toLongLong();
        // res = ( a % b + b ) % b;
        // source: http://lists.inf.ethz.ch/pipermail/oberon/2019/013353.html
        if (a < 0)
            return qint64( (b - 1) + ((a - b + 1)) % b );
        else
            return qint64( a % b );
    }else
        return expErr(e,tr("operand types incompatible with operator") );
}

QVariant Evaluator::AND(const QVariant& lhs, const QVariant& rhs, Expression* e )
{
    if( rhs.type() == QVariant::Bool )
        return lhs.toBool() && rhs.toBool();
    else
        return expErr( e,tr("operand types incompatible with operator") );
}

QVariant Evaluator::OR(const QVariant& lhs, const QVariant& rhs, Expression* e )
{
    if( rhs.type() == QVariant::Bool )
        return lhs.toBool() || rhs.toBool();
    else
        return expErr( e,tr("operand types incompatible with operator") );
}

QVariant Evaluator::EQ(const QVariant& lhs, const QVariant& rhs, Expression* e )
{
    if( rhs.type() == QVariant::LongLong )
        return lhs.toLongLong() == rhs.toLongLong();
    else if( rhs.type() == QVariant::Double )
        return lhs.toDouble() == rhs.toDouble();
    else if( rhs.type() == QVariant::Bool )
        return lhs.toBool() == rhs.toBool();
    else if( lhs.canConvert<Set>() && rhs.canConvert<Set>() )
        return QVariant::fromValue( lhs.value<Set>() == rhs.value<Set>() );
    else if( lhs.type() == QVariant::ByteArray && rhs.type() == QVariant::ByteArray )
        return lhs.toByteArray() == rhs.toByteArray();
    else
        return expErr( e,tr("operand types incompatible with operator") );
}

QVariant Evaluator::NEQ(const QVariant& lhs, const QVariant& rhs, Expression* e )
{
    if( rhs.type() == QVariant::LongLong )
        return lhs.toLongLong() != rhs.toLongLong();
    else if( rhs.type() == QVariant::Double )
        return lhs.toDouble() != rhs.toDouble();
    else if( rhs.type() == QVariant::Bool )
        return lhs.toBool() != rhs.toBool();
    else if( lhs.canConvert<Set>() && rhs.canConvert<Set>() )
        return QVariant::fromValue( lhs.value<Set>() != rhs.value<Set>() );
    else if( lhs.type() == QVariant::ByteArray && rhs.type() == QVariant::ByteArray )
        return lhs.toByteArray() != rhs.toByteArray();
    else
        return  expErr(e,tr("operand types incompatible with operator") );
}

QVariant Evaluator::LE(const QVariant& lhs, const QVariant& rhs, Expression* e )
{
    if( rhs.type() == QVariant::LongLong )
        return lhs.toLongLong() < rhs.toLongLong();
    else if( rhs.type() == QVariant::Double )
        return lhs.toDouble() < rhs.toDouble();
    else if( lhs.type() == QVariant::ByteArray && rhs.type() == QVariant::ByteArray )
        return lhs.toByteArray() < rhs.toByteArray();
    else
        return  expErr(e,tr("operand types incompatible with operator") );
}

QVariant Evaluator::LEQ(const QVariant& lhs, const QVariant& rhs, Expression* e )
{
    if( rhs.type() == QVariant::LongLong )
        return lhs.toLongLong() <= rhs.toLongLong();
    else if( rhs.type() == QVariant::Double )
        return lhs.toDouble() <= rhs.toDouble();
    else if( lhs.type() == QVariant::ByteArray && rhs.type() == QVariant::ByteArray )
        return lhs.toByteArray() <= rhs.toByteArray();
    else
        return  expErr(e,tr("operand types incompatible with operator") );
}

QVariant Evaluator::GT(const QVariant& lhs, const QVariant& rhs, Expression* e )
{
    if( rhs.type() == QVariant::LongLong )
        return lhs.toLongLong() > rhs.toLongLong();
    else if( rhs.type() == QVariant::Double )
        return lhs.toDouble() > rhs.toDouble();
    else if( lhs.type() == QVariant::ByteArray && rhs.type() == QVariant::ByteArray )
        return lhs.toByteArray() > rhs.toByteArray();
    else
        return  expErr(e,tr("operand types incompatible with operator") );
}

QVariant Evaluator::GEQ(const QVariant& lhs, const QVariant& rhs, Expression* e )
{
    if( rhs.type() == QVariant::LongLong )
        return lhs.toLongLong() >= rhs.toLongLong();
    else if( rhs.type() == QVariant::Double )
        return lhs.toDouble() >= rhs.toDouble();
    else if( lhs.type() == QVariant::ByteArray && rhs.type() == QVariant::ByteArray )
        return lhs.toByteArray() >= rhs.toByteArray();
    else
        return  expErr(e,tr("operand types incompatible with operator") );
}

QVariant Evaluator::IN(const QVariant& lhs, const QVariant& rhs, Expression* e )
{
    const qint64 b = lhs.toLongLong();
    if( lhs.type() != QVariant::LongLong || !rhs.canConvert<Set>() || b < 0 || b >= SET_BIT_LEN )
        return  expErr(e,tr("invalid data type for operator IN") );
    return rhs.value<Set>().test(b);
}

QVariant Evaluator::binOp(BinExpr* e, const QVariant& lhs, const QVariant& rhs )
{
    const quint8 op = e->d_op;
    if( op == BinExpr::IN )
        return IN(lhs,rhs,e);
    if( rhs.type() != lhs.type() )
        return expErr( e, tr("operands not of same type") );

    switch( op )
    {
    case BinExpr::ADD:
        return ADD(lhs,rhs,e);
    case BinExpr::SUB:
        return SUB(lhs,rhs,e);
    case BinExpr::FDIV:
        return FDIV(lhs,rhs,e);
    case BinExpr::MUL:
        return MUL(lhs,rhs,e);
    case BinExpr::DIV:
        return DIV(lhs,rhs,e);
    case BinExpr::MOD:
        return MOD(lhs,rhs,e);
    case BinExpr::AND:
        return AND(lhs,rhs,e);
    case BinExpr::OR:
        return OR(lhs,rhs,e);
    case BinExpr::EQ:
        return EQ(lhs,rhs,e);
    case BinExpr::NEQ:
        return NEQ(lhs,rhs,e);
    case BinExpr::LT:
        return LE(lhs,rhs,e);
    case BinExpr::LEQ:
        return LEQ(lhs,rhs,e);
    case BinExpr::GT:
        return GT(lhs,rhs,e);
    case BinExpr::GEQ:
        return GEQ(lhs,rhs,e);
    default:
        return expErr( e, tr("operator not supported"));
    }
    return QVariant();
}

QVariant Evaluator::unOp(UnExpr* e, const QVariant& v )
{
    switch( e->d_op )
    {
    case UnExpr::NEG:
        return NEG(v,e);
    case UnExpr::NOT:
        return NOT(v,e);
    default:
        return  expErr(e, tr("operator not supported"));
    }
    return QVariant();
}
