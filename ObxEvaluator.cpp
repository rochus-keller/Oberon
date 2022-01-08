/*
* Copyright 2020-2021 Rochus Keller <mailto:me@rochus-keller.ch>
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
#include <math.h>
using namespace Obx;
using namespace Ob;

#ifndef OBX_AST_DECLARE_SET_METATYPE_IN_HEADER
Q_DECLARE_METATYPE( Obx::Literal::SET )
#endif

struct EvalVisitor : public AstVisitor
{
    Scope* mod;
    Ob::Errors* errs;
    bool supportVla;
    Evaluator::Result val;

    EvalVisitor(Scope* m, bool b, Ob::Errors* e):mod(m),errs(e),supportVla(b){}

    bool error( Expression* e, const QString& msg )
    {
        if( errs )
            errs->error( Errors::Semantics, Loc(e->d_loc,mod->getModule()->d_file), msg );
        throw "";
        return false;
    }

    void push( const QVariant& value, quint8 vtype, bool wide = false, bool minInt = false, int strlen = 0 )
    {
        val.d_value = value;
        val.d_vtype = vtype;
        val.d_wide = wide;
        val.d_minInt = minInt;
        val.d_strLen = strlen;
    }

    void NEG(const Evaluator::Result& r, Expression* e )
    {
        if( r.d_vtype == Literal::Real )
            push( -r.d_value.toDouble(), r.d_vtype, r.d_wide );
        if( r.d_vtype == Literal::Integer )
            push( -r.d_value.toLongLong(), r.d_vtype, r.d_wide, r.d_minInt );
        else
            error( e, Evaluator::tr("cannot invert sign of non numerical expression"));
    }

    void NOT(const Evaluator::Result& r, Expression* e )
    {
        if( r.d_vtype == Literal::Boolean )
            push( !r.d_value.toBool(), r.d_vtype );
        else
            error( e, Evaluator::tr("cannot negate non boolean expression"));
    }

    static QByteArray toString(const Evaluator::Result& v )
    {
        if( v.d_wide )
            return QString(1,QChar((ushort)v.d_value.toUInt())).toUtf8();
        else
            return QByteArray(1,(quint8)v.d_value.toUInt());
    }

    void ADD(const Evaluator::Result& lhs, const Evaluator::Result& rhs, Expression* e )
    {
        if( lhs.d_vtype == Literal::Integer && rhs.d_vtype == Literal::Integer )
            push(lhs.d_value.toLongLong() + rhs.d_value.toLongLong(),lhs.d_vtype, lhs.d_wide || rhs.d_wide, lhs.d_minInt || rhs.d_minInt );
        else if( lhs.d_vtype == Literal::Real && rhs.d_vtype == Literal::Real )
            push(lhs.d_value.toDouble() + rhs.d_value.toDouble(),lhs.d_vtype, lhs.d_wide || rhs.d_wide );
        else if( lhs.d_vtype == Literal::Set && rhs.d_vtype == Literal::Set )
            push(QVariant::fromValue( lhs.d_value.value<Literal::SET>() | rhs.d_value.value<Literal::SET>() ),
                 lhs.d_vtype);
        else if( lhs.d_vtype == Literal::String && rhs.d_vtype == Literal::String )
            push( lhs.d_value.toByteArray() + rhs.d_value.toByteArray(), Literal::String, lhs.d_wide || rhs.d_wide,
                  false, lhs.d_strLen + rhs.d_strLen );
        else if( lhs.d_vtype == Literal::String && rhs.d_vtype == Literal::Char )
            push( lhs.d_value.toByteArray() + toString(rhs), Literal::String, lhs.d_wide || rhs.d_wide,
                  false, lhs.d_strLen + 1 );
        else if( lhs.d_vtype == Literal::Char && rhs.d_vtype == Literal::String )
            push( toString(lhs) + rhs.d_value.toByteArray(), Literal::String, lhs.d_wide || rhs.d_wide,
                  false, lhs.d_strLen + 1 );
        else if( lhs.d_vtype == Literal::Char && rhs.d_vtype == Literal::Char )
            push( toString(lhs) + toString(rhs), Literal::String, lhs.d_wide || rhs.d_wide,
                  false, 1 + 1 );
        else
            error( e,Evaluator::tr("operand types incompatible with operator") );
    }

    void SUB(const Evaluator::Result& lhs, const Evaluator::Result& rhs, Expression* e )
    {
        if( lhs.d_vtype == Literal::Integer && rhs.d_vtype == Literal::Integer )
            push(lhs.d_value.toLongLong() - rhs.d_value.toLongLong(),lhs.d_vtype, lhs.d_wide || rhs.d_wide, lhs.d_minInt || rhs.d_minInt);
        else if( lhs.d_vtype == Literal::Real && rhs.d_vtype == Literal::Real )
            push(lhs.d_value.toDouble() - rhs.d_value.toDouble(),lhs.d_vtype, lhs.d_wide || rhs.d_wide );
        else if( lhs.d_vtype == Literal::Set && rhs.d_vtype == Literal::Set )
        {
            const Literal::SET a = lhs.d_value.value<Literal::SET>();
            const Literal::SET b = rhs.d_value.value<Literal::SET>();
            Literal::SET res;
            for( int j = 0; j < a.size(); j++ )
                res.set( a.test(j) && !b.test(j) );
            push(QVariant::fromValue( res ),lhs.d_vtype);
        }else
            error(e,Evaluator::tr("operand types incompatible with operator") );
    }

    void FDIV(const Evaluator::Result& lhs, const Evaluator::Result& rhs, Expression* e )
    {
        if( lhs.d_vtype == Literal::Integer && rhs.d_vtype == Literal::Integer )
            push(lhs.d_value.toLongLong() / double(rhs.d_value.toLongLong()),Literal::Real, lhs.d_wide || rhs.d_wide);
        else if( lhs.d_vtype == Literal::Real && rhs.d_vtype == Literal::Real )
            push(lhs.d_value.toDouble() / rhs.d_value.toDouble(),lhs.d_vtype, lhs.d_wide || rhs.d_wide );
        else if( lhs.d_vtype == Literal::Set && rhs.d_vtype == Literal::Set )
        {
            const Literal::SET a = lhs.d_value.value<Literal::SET>();
            const Literal::SET b = rhs.d_value.value<Literal::SET>();
            Literal::SET res;
            for( int j = 0; j < a.size(); j++ )
                res.set( ( a.test(j) || b.test(j) ) && !( a.test(j) && b.test(j) ) );
            push(QVariant::fromValue( res ),lhs.d_vtype);
        }else
            error(e,Evaluator::tr("operand types incompatible with operator") );
    }

    void MUL(const Evaluator::Result& lhs, const Evaluator::Result& rhs, Expression* e )
    {
        if( lhs.d_vtype == Literal::Integer && rhs.d_vtype == Literal::Integer )
            push(lhs.d_value.toLongLong() * rhs.d_value.toLongLong(),lhs.d_vtype, lhs.d_wide || rhs.d_wide, lhs.d_minInt || rhs.d_minInt);
        else if( lhs.d_vtype == Literal::Real && rhs.d_vtype == Literal::Real )
            push(lhs.d_value.toDouble() * rhs.d_value.toDouble(),lhs.d_vtype, lhs.d_wide || rhs.d_wide );
        else if( lhs.d_vtype == Literal::Set && rhs.d_vtype == Literal::Set )
        {
            const Literal::SET a = lhs.d_value.value<Literal::SET>();
            const Literal::SET b = rhs.d_value.value<Literal::SET>();
            push(QVariant::fromValue( a & b ),lhs.d_vtype);
        }else
            error(e,Evaluator::tr("operand types incompatible with operator") );
    }

    void DIV(const Evaluator::Result& lhs, const Evaluator::Result& rhs, Expression* e )
    {
        if( lhs.d_vtype == Literal::Integer && rhs.d_vtype == Literal::Integer )
        {
            const qint64 a = lhs.d_value.toLongLong();
            const qint64 b = rhs.d_value.toLongLong();
            // res = ( a - ( ( a % b + b ) % b ) ) / b;
            // source: http://lists.inf.ethz.ch/pipermail/oberon/2019/013353.html
            if (a < 0)
                push(qint64( (a - b + 1) / b ),lhs.d_vtype, lhs.d_wide || rhs.d_wide, lhs.d_minInt || rhs.d_minInt);
            else
                push(qint64( a / b ),lhs.d_vtype, lhs.d_wide || rhs.d_wide, lhs.d_minInt || rhs.d_minInt);
        }else
            error(e,Evaluator::tr("operand types incompatible with operator") );
    }

    void MOD(const Evaluator::Result& lhs, const Evaluator::Result& rhs, Expression* e )
    {
        if( lhs.d_vtype == Literal::Integer && rhs.d_vtype == Literal::Integer )
        {
            const qint64 a = lhs.d_value.toLongLong();
            const qint64 b = rhs.d_value.toLongLong();
            // res = ( a % b + b ) % b;
            // source: http://lists.inf.ethz.ch/pipermail/oberon/2019/013353.html
            if (a < 0)
                push(qint64( (b - 1) + ((a - b + 1)) % b ),lhs.d_vtype, lhs.d_wide || rhs.d_wide, lhs.d_minInt || rhs.d_minInt);
            else
                push(qint64( a % b ), lhs.d_vtype, lhs.d_wide || rhs.d_wide, lhs.d_minInt || rhs.d_minInt);
        }else
            error(e,Evaluator::tr("operand types incompatible with operator") );
    }

    void AND(const Evaluator::Result& lhs, const Evaluator::Result& rhs, Expression* e )
    {
        if( lhs.d_vtype == Literal::Boolean && rhs.d_vtype == Literal::Boolean )
            push(lhs.d_value.toBool() && rhs.d_value.toBool(), lhs.d_vtype);
        else
            error(e,Evaluator::tr("operand types incompatible with operator") );
    }

    void OR(const Evaluator::Result& lhs, const Evaluator::Result& rhs, Expression* e )
    {
        if( lhs.d_vtype == Literal::Boolean && rhs.d_vtype == Literal::Boolean )
            push(lhs.d_value.toBool() || rhs.d_value.toBool(), lhs.d_vtype);
        else
            error(e,Evaluator::tr("operand types incompatible with operator") );
    }

    void EQ(const Evaluator::Result& lhs, const Evaluator::Result& rhs, Expression* e )
    {
        if( lhs.d_vtype == Literal::Integer && rhs.d_vtype == Literal::Integer )
            push(lhs.d_value.toLongLong() == rhs.d_value.toLongLong(),Literal::Boolean);
        else if( lhs.d_vtype == Literal::Real && rhs.d_vtype == Literal::Real )
            push(lhs.d_value.toDouble() == rhs.d_value.toDouble(),Literal::Boolean);
        else if( lhs.d_vtype == Literal::Boolean && rhs.d_vtype == Literal::Boolean )
            push(lhs.d_value.toBool() == rhs.d_value.toBool(), Literal::Boolean);
        else if( lhs.d_vtype == Literal::Set && rhs.d_vtype == Literal::Set )
            push(QVariant::fromValue( lhs.d_value.value<Literal::SET>() == rhs.d_value.value<Literal::SET>() ),
                 Literal::Boolean);
        else if( lhs.d_vtype == Literal::String && rhs.d_vtype == Literal::String )
            push(lhs.d_value.toByteArray() == rhs.d_value.toByteArray(), Literal::Boolean);
        else
            error(e,Evaluator::tr("operand types incompatible with operator") );
    }

    void NEQ(const Evaluator::Result& lhs, const Evaluator::Result& rhs, Expression* e )
    {
        if( lhs.d_vtype == Literal::Integer && rhs.d_vtype == Literal::Integer )
            push(lhs.d_value.toLongLong() != rhs.d_value.toLongLong(),Literal::Boolean);
        else if( lhs.d_vtype == Literal::Real && rhs.d_vtype == Literal::Real )
            push(lhs.d_value.toDouble() != rhs.d_value.toDouble(),Literal::Boolean);
        else if( lhs.d_vtype == Literal::Boolean && rhs.d_vtype == Literal::Boolean )
            push(lhs.d_value.toBool() != rhs.d_value.toBool(), Literal::Boolean);
        else if( lhs.d_vtype == Literal::Set && rhs.d_vtype == Literal::Set )
            push(QVariant::fromValue( lhs.d_value.value<Literal::SET>() != rhs.d_value.value<Literal::SET>() ),
                 Literal::Boolean);
        else if( lhs.d_vtype == Literal::String && rhs.d_vtype == Literal::String )
            push(lhs.d_value.toByteArray() != rhs.d_value.toByteArray(), Literal::Boolean);
        else
            error(e,Evaluator::tr("operand types incompatible with operator") );
    }

    void LE(const Evaluator::Result& lhs, const Evaluator::Result& rhs, Expression* e )
    {
        if( lhs.d_vtype == Literal::Integer && rhs.d_vtype == Literal::Integer )
            push(lhs.d_value.toLongLong() < rhs.d_value.toLongLong(),Literal::Boolean);
        else if( lhs.d_vtype == Literal::Real && rhs.d_vtype == Literal::Real )
            push(lhs.d_value.toDouble() < rhs.d_value.toDouble(),Literal::Boolean);
        else if( lhs.d_vtype == Literal::String && rhs.d_vtype == Literal::String )
            push(QString::fromUtf8(lhs.d_value.toByteArray()) < QString::fromUtf8(rhs.d_value.toByteArray()),
                 Literal::Boolean);
        else
            error(e,Evaluator::tr("operand types incompatible with operator") );
    }

    void LEQ(const Evaluator::Result& lhs, const Evaluator::Result& rhs, Expression* e )
    {
        if( lhs.d_vtype == Literal::Integer && rhs.d_vtype == Literal::Integer )
            push(lhs.d_value.toLongLong() <= rhs.d_value.toLongLong(),Literal::Boolean);
        else if( lhs.d_vtype == Literal::Real && rhs.d_vtype == Literal::Real )
            push(lhs.d_value.toDouble() <= rhs.d_value.toDouble(),Literal::Boolean);
        else if( lhs.d_vtype == Literal::String && rhs.d_vtype == Literal::String )
            push(QString::fromUtf8(lhs.d_value.toByteArray()) <= QString::fromUtf8(rhs.d_value.toByteArray()),
                 Literal::Boolean);
        else
            error(e,Evaluator::tr("operand types incompatible with operator") );
    }

    void GT(const Evaluator::Result& lhs, const Evaluator::Result& rhs, Expression* e )
    {
        if( lhs.d_vtype == Literal::Integer && rhs.d_vtype == Literal::Integer )
            push(lhs.d_value.toLongLong() > rhs.d_value.toLongLong(),Literal::Boolean);
        else if( lhs.d_vtype == Literal::Real && rhs.d_vtype == Literal::Real )
            push(lhs.d_value.toDouble() > rhs.d_value.toDouble(),Literal::Boolean);
        else if( lhs.d_vtype == Literal::String && rhs.d_vtype == Literal::String )
            push(QString::fromUtf8(lhs.d_value.toByteArray()) > QString::fromUtf8(rhs.d_value.toByteArray()),
                 Literal::Boolean);
        else
            error(e,Evaluator::tr("operand types incompatible with operator") );
    }

    void GEQ(const Evaluator::Result& lhs, const Evaluator::Result& rhs, Expression* e )
    {
        if( lhs.d_vtype == Literal::Integer && rhs.d_vtype == Literal::Integer )
            push(lhs.d_value.toLongLong() >= rhs.d_value.toLongLong(),Literal::Boolean);
        else if( lhs.d_vtype == Literal::Real && rhs.d_vtype == Literal::Real )
            push(lhs.d_value.toDouble() >= rhs.d_value.toDouble(),Literal::Boolean);
        else if( lhs.d_vtype == Literal::String && rhs.d_vtype == Literal::String )
            push(QString::fromUtf8(lhs.d_value.toByteArray()) >= QString::fromUtf8(rhs.d_value.toByteArray()),
                 Literal::Boolean);
        else
            error(e,Evaluator::tr("operand types incompatible with operator") );
    }

    void IN(const Evaluator::Result& lhs, const Evaluator::Result& rhs, Expression* e )
    {
        if( lhs.d_vtype == Literal::Integer && rhs.d_vtype == Literal::Set )
        {
            const qint64 b = lhs.d_value.toLongLong();
            if( b < 0 || b >= Literal::SET_BIT_LEN )
                error(e,Evaluator::tr("lhs is out of range MIN(SET)..MAX(SET)") );
            push( rhs.d_value.value<Literal::SET>().test(b), Literal::Boolean );
        }else
            error(e,Evaluator::tr("operand types incompatible with operator") );
    }

    void visit( Literal* me)
    {
        val.d_value = me->d_val;
        val.d_vtype = (Literal::ValueType)me->d_vtype;
        val.d_wide = me->d_wide;
        val.d_minInt = me->d_minInt;
        val.d_strLen = me->d_strLen;
    }

    void visit( SetExpr* me)
    {
        Literal::SET s;
        for(int i = 0; i < me->d_parts.size(); i++ )
        {
            if( me->d_parts[i]->getTag() == Thing::T_BinExpr )
            {
                BinExpr* be = cast<BinExpr*>(me->d_parts[i].data());
                if( be->d_op != BinExpr::Range )
                    error(me,Evaluator::tr("invalid set part") );

                Evaluator::Result lhs, rhs;
                if( be->d_lhs )
                {
                    be->d_lhs->accept(this);
                    lhs = val;
                }
                if( be->d_rhs )
                {
                    be->d_rhs->accept(this);
                    rhs = val;
                }
                if( lhs.d_vtype != Literal::Integer || rhs.d_vtype != Literal::Integer )
                    error(me,Evaluator::tr("operand types incompatible with range operator") );

                const qint64 l = lhs.d_value.toLongLong();
                const qint64 r = lhs.d_value.toLongLong();
                if( l < 0 || l >= Literal::SET_BIT_LEN || r < 0 || r >= Literal::SET_BIT_LEN )
                    error(me,Evaluator::tr("lhs or rhs is out of range MIN(SET)..MAX(SET)") );
                if( l <= r )
                    for( int b = l; b <= r; b++ )
                        s.set(b);
                else
                    for( int b = r; b <= l; b++ )
                        s.set(b);
            }else
            {
                me->d_parts[i]->accept(this);
                if( val.d_vtype != Literal::Integer )
                {
                    me->d_parts[i]->accept(this);
                    error(me,Evaluator::tr("operand type incompatible with set literal") );
                }

                const qint64 l = val.d_value.toLongLong();
                if( l < 0 || l >= Literal::SET_BIT_LEN  )
                    error(me,Evaluator::tr("value is out of range MIN(SET)..MAX(SET)") );
                s.set(l);
            }
        }
        push(QVariant::fromValue(s),Literal::Set);
    }

    void evalConst( Const* me)
    {
        val.d_vtype = (Literal::ValueType)me->d_vtype;
        val.d_value = me->d_val;
    }

    void visit( IdentLeaf* me)
    {
        Named* n = me->getIdent();
        if( n && n->getTag() == Thing::T_Const )
            evalConst( cast<Const*>(n) );
        else if( n && n->getTag() == Thing::T_Parameter && supportVla )
        {
            Parameter* p = cast<Parameter*>(n);
            if( p->d_scope == mod )
                throw 0;
            else
                error( me, Evaluator::tr("non-local access to parameter not supported") );
        }else
            error( me, Evaluator::tr("operation not supported in constant expressions") );
    }

    void visit( UnExpr* me)
    {
        if( me->d_sub )
            me->d_sub->accept(this);
        else
            val = Evaluator::Result();
        switch( me->d_op )
        {
        case UnExpr::NEG:
            NEG(val,me);
            break;
        case UnExpr::NOT:
            NOT(val,me);
            break;
        default:
            error( me, Evaluator::tr("unary operator not supported in constant expressions") );
        }
    }

    void visit( IdentSel* me)
    {
        Named* n = me->getIdent();
        if( n && n->getTag() == Thing::T_Const )
            evalConst( cast<Const*>(n) );
        else
            error( me, Evaluator::tr("operation not supported in constant expressions") );
    }

    void visit( ArgExpr* me)
    {
        if( me->d_op != ArgExpr::CALL )
            error( me, Evaluator::tr("this operation is not supported in a const expression"));
        Named* id = me->d_sub->getIdent();
        if( id && id->getTag() == Thing::T_BuiltIn )
            evalBuiltIn( cast<BuiltIn*>(id), me );
        else
            error( me, Evaluator::tr("this procedure call is not supported in a const expression"));
    }

    Type* derefed( Type* t ) const
    {
        if( t )
            return t->derefed();
        else
            return 0;
    }

    bool evalBitOps( quint8 func, ArgExpr* me )
    {
        if( me->d_args.size() == 2 )
        {
            me->d_args.first()->accept(this);
            if( val.d_vtype == Literal::Integer )
            {
                bool lwide = val.d_wide;
                const qint64 lhs = val.d_value.toLongLong();
                me->d_args.last()->accept(this);
                if( val.d_vtype == Literal::Integer )
                {
                    bool rwide = val.d_wide;
                    const qint64 rhs = val.d_value.toLongLong();
                    switch( func )
                    {
                    case BuiltIn::BITAND:
                        val.d_value = lhs & rhs;
                        break;
                    case BuiltIn::BITOR:
                        val.d_value = lhs | rhs;
                        break;
                    case BuiltIn::BITXOR:
                        val.d_value = lhs ^ rhs;
                        break;
                    case BuiltIn::BITSHL:
                        val.d_value = lhs << rhs;
                        break;
                    case BuiltIn::BITSHR:
                        val.d_value = lhs >> rhs;
                        break;
                    case BuiltIn::BITASR:
                        val.d_value = lhs >> rhs | ~(~((quint64)0) >> rhs);
                        break;
                    default:
                        Q_ASSERT(false);
                    }
                    val.d_wide = lwide || rwide;
                    val.d_minInt = !val.d_wide;
                }else
                    return error( me->d_args.last().data(), Evaluator::tr("invalid argument type") );
            }else
                return error( me->d_args.first().data(), Evaluator::tr("invalid argument type") );
        }else
            return error( me, Evaluator::tr("invalid number of arguments") );
        return true;
    }

    void evalBuiltIn(BuiltIn* f, ArgExpr* me)
    {
        // NOTE: this is currently just a minimal implementation to meet BBOX Win/Mini, POS and Hennessy

        switch( f->d_func )
        {
        case BuiltIn::MAX:
        case BuiltIn::MIN:
            if( me->d_args.size() == 1 )
            {
                Named* n = me->d_args.first()->getIdent();
                Type* t = derefed(me->d_args.first()->d_type.data());
                if( n && n->getTag() == Thing::T_NamedType && t && t->getTag() == Thing::T_BaseType )
                {
                    BaseType* bi = cast<BaseType*>(t);
                    if( f->d_func == BuiltIn::MAX )
                        val.d_value = bi->maxVal();
                    else
                        val.d_value = bi->minVal();
                    val.d_vtype = Literal::Invalid;
                    if( bi->d_baseType == BaseType::CHAR || bi->d_baseType == BaseType::WCHAR )
                        val.d_vtype = Literal::Char;
                    else if( bi->d_baseType >= BaseType::BYTE && bi->d_baseType <= BaseType::LONGINT )
                        val.d_vtype = Literal::Integer;
                    else if( bi->d_baseType >= BaseType::REAL && bi->d_baseType <= BaseType::LONGREAL )
                        val.d_vtype = Literal::Real;
                    else if( bi->d_baseType == BaseType::SET )
                        val.d_vtype = Literal::Integer;
                    return;
                }else
                    error( me, Evaluator::tr("base type argument required") );
            }else
                error( me, Evaluator::tr("invalid number of arguments") );
            break;
        case BuiltIn::LEN:
        case BuiltIn::STRLEN:
            if( me->d_args.size() == 1 )
            {
                Type* t = derefed(me->d_args.first()->d_type.data());
                if( t && t->getTag() == Thing::T_Pointer )
                    t = derefed(cast<Pointer*>(t)->d_to.data());
                const int tag = t ? t->getTag() : 0;
                if( tag == Thing::T_Array )
                {
                    Array* a = cast<Array*>(t);
                    if( f->d_func == BuiltIn::STRLEN )
                    {
                        t = derefed(a->d_type.data());
                        if( t && t->isChar() )
                        {
                            if( supportVla )
                                throw 0;
                            else
                                error( me, Evaluator::tr("cannot determine string length in a const expression") );
                        }else
                            error( me, Evaluator::tr("incompatible argument type") );
                    }// else

                    if( !a->d_lenExpr.isNull() )
                    {
                        val.d_value = a->d_len;
                        val.d_vtype = Literal::Integer;
                        return;
                    }else
                        error( me, Evaluator::tr("cannot determine length of an open array in a const expression") );
                }else if( tag == Thing::T_BaseType )
                {
                    BaseType* bt = cast<BaseType*>(t);
                    if( bt->d_baseType == BaseType::STRING || bt->d_baseType == BaseType::WSTRING )
                    {
                        me->d_args.first()->accept(this);
                        if( val.d_vtype == Literal::String )
                        {
                            val.d_value = QString::fromUtf8(val.d_value.toByteArray()).size() + 1; // including \0
                            val.d_vtype = Literal::Integer;
                            return;
                        }
                    }else if( bt->d_baseType == BaseType::BYTEARRAY )
                    {
                        me->d_args.first()->accept(this);
                        if( val.d_vtype == Literal::Bytes )
                        {
                            val.d_value = val.d_value.toByteArray().size();
                            val.d_vtype = Literal::Integer;
                            return;
                        }
                    }
                    error( me, Evaluator::tr("cannot determine the length of this argument in a const expression") );
                }else
                    error( me, Evaluator::tr("invalid argument in a const expression") );
            }else
                error( me, Evaluator::tr("invalid number of arguments") );
            break;
        case BuiltIn::ASH:
            if( me->d_args.size() == 2 )
            {
                Evaluator::Result x,n;
                me->d_args.first()->accept(this);
                x = val;
                me->d_args.last()->accept(this);
                n = val;
                if( x.d_vtype == Literal::Integer && n.d_vtype == Literal::Integer )
                {
                    val.d_value = x.d_value.toInt() * ::pow(2,n.d_value.toInt());
                    return;
                }else
                    error( me, Evaluator::tr("invalid argument types") );
            }else
                error( me, Evaluator::tr("invalid number of arguments") );
            break;
        case BuiltIn::ORD:
            if( me->d_args.size() == 1 )
            {
                me->d_args.first()->accept(this);
                if( val.d_vtype == Literal::Char || val.d_vtype == Literal::Boolean || val.d_vtype == Literal::Enum )
                {
                    val.d_vtype = Literal::Integer;
                    return;
                }else if( val.d_vtype == Literal::String )
                {
                    const QString str = QString::fromUtf8(val.d_value.toByteArray());
                    if( str.size() == 1 )
                    {
                        val.d_value = str[0].unicode();
                        val.d_vtype = Literal::Integer;
                        return;
                    }else
                        error( me, Evaluator::tr("argument is not a character") );
                }else
                    error( me, Evaluator::tr("invalid argument type") );
            }else
                error( me, Evaluator::tr("invalid number of arguments") );
            break;
        case BuiltIn::CHR:
            if( me->d_args.size() == 1 )
            {
                me->d_args.first()->accept(this);
                val.d_vtype = Literal::Char;
                return;
            }else
                error( me, Evaluator::tr("invalid number of arguments") );
            break;
        case BuiltIn::ABS:
            if( me->d_args.size() == 1 )
            {
                me->d_args.first()->accept(this);
                if( val.d_vtype == Literal::Real )
                    val.d_value = qAbs(val.d_value.toDouble());
                else if( val.d_vtype == Literal::Integer )
                    val.d_value = qAbs(val.d_value.toULongLong());
                else
                    error( me, Evaluator::tr("invalid argument type") );
                return;
            }else
                error( me, Evaluator::tr("invalid number of arguments") );
            break;
        case BuiltIn::ODD:
            if( me->d_args.size() == 1 )
            {
                me->d_args.first()->accept(this);
                if( val.d_vtype == Literal::Integer )
                {
                    val.d_value = ( val.d_value.toULongLong() % 2 == 0 );
                    val.d_vtype = Literal::Boolean;
                }else
                    error( me, Evaluator::tr("invalid argument type") );
                return;
            }else
                error( me, Evaluator::tr("invalid number of arguments") );
            break;
        case BuiltIn::LSL:
            if( me->d_args.size() == 2 )
            {
                me->d_args.first()->accept(this);
                if( val.d_vtype == Literal::Integer )
                {
                    qint64 x = val.d_value.toULongLong();
                    me->d_args.last()->accept(this);
                    if( val.d_vtype == Literal::Integer )
                    {
                        const qint32 n = val.d_value.toInt();
                        if( n < 0 )
                            x = x >> n;
                        else
                            x = x << n;
                        val.d_value = x;
                    }else
                        error( me, Evaluator::tr("invalid argument type") );
                }else
                    error( me, Evaluator::tr("invalid argument type") );
                return;
            }else
                error( me, Evaluator::tr("invalid number of arguments") );
            break;
        case BuiltIn::ASR:
            if( me->d_args.size() == 2 )
            {
                me->d_args.first()->accept(this);
                Evaluator::Result lhs = val;
                if( lhs.d_vtype == Literal::Integer )
                {
                    me->d_args.last()->accept(this);
                    const qint32 n = val.d_value.toInt();
                    if( val.d_vtype != Literal::Integer )
                        error( me, Evaluator::tr("invalid argument type") );
                    if( lhs.d_wide )
                    {
                        qint64 x = lhs.d_value.toLongLong();
                        if( x < 0 && n > 0 )
                            x = x >> n | ~(~((quint64)0) >> n);
                        else
                            x = x >> n;
                        val.d_wide = true;
                        val.d_minInt = false;
                        val.d_value = x;
                    }else
                    {
                        qint32 x = lhs.d_value.toInt();
                        if( x < 0 && n > 0 )
                            x = x >> n | ~(~((quint32)0) >> n);
                        else
                            x = x >> n;
                        val.d_minInt = true;
                        val.d_wide = false;
                        val.d_value = x;
                    }
                }else
                    error( me, Evaluator::tr("invalid argument type") );
                return;
            }else
                error( me, Evaluator::tr("invalid number of arguments") );
            break;
        case BuiltIn::ROR:
            if( me->d_args.size() == 2 )
            {
                me->d_args.first()->accept(this);
                if( val.d_vtype == Literal::Integer )
                {
                    qint64 x = val.d_value.toULongLong();
                    me->d_args.last()->accept(this);
                    if( val.d_vtype == Literal::Integer )
                    {
                        x = x >> val.d_value.toULongLong();
                        val.d_value = x;
                    }else
                        error( me, Evaluator::tr("invalid argument type") );
                }else
                    error( me, Evaluator::tr("invalid argument type") );
                return;
            }else
                error( me, Evaluator::tr("invalid number of arguments") );
            break;
        case BuiltIn::FLOOR:
            if( me->d_args.size() == 1 )
            {
                me->d_args.first()->accept(this);
                if( val.d_vtype == Literal::Real )
                {
                    val.d_value = floor( val.d_value.toDouble() );
                    val.d_vtype = Literal::Integer;
                }else
                    error( me, Evaluator::tr("invalid argument type") );
                return;
            }else
                error( me, Evaluator::tr("invalid number of arguments") );
            break;
        case BuiltIn::FLT:
            if( me->d_args.size() == 1 )
            {
                me->d_args.first()->accept(this);
                if( val.d_vtype == Literal::Integer )
                    val.d_vtype = Literal::Real;
                else
                    error( me, Evaluator::tr("invalid argument type") );
                return;
            }else
                error( me, Evaluator::tr("invalid number of arguments") );
            break;
        case BuiltIn::BITNOT:
            if( me->d_args.size() == 1 )
            {
                me->d_args.first()->accept(this);
                if( val.d_vtype == Literal::Integer )
                    val.d_value = ~val.d_value.toUInt();
                else
                    error( me, Evaluator::tr("invalid argument type") );
                return;
            }else
                error( me, Evaluator::tr("invalid number of arguments") );
            break;
        case BuiltIn::BITAND:
        case BuiltIn::BITOR:
        case BuiltIn::BITXOR:
        case BuiltIn::BITSHL:
        case BuiltIn::BITSHR:
        case BuiltIn::BITASR:
            if( evalBitOps(f->d_func,me) )
                return;
            break;
        case BuiltIn::INC:
        case BuiltIn::DEC:
        case BuiltIn::INCL:
        case BuiltIn::EXCL:
        case BuiltIn::NEW:
        case BuiltIn::ASSERT:
        case BuiltIn::PACK:
        case BuiltIn::UNPK:
        case BuiltIn::LED:
        case BuiltIn::TRAP:
        case BuiltIn::TRAPIF:
        case BuiltIn::SYS_ADR:
        case BuiltIn::SYS_BIT:
        case BuiltIn::SYS_GET:
        case BuiltIn::SYS_H:
        case BuiltIn::SYS_LDREG:
        case BuiltIn::SYS_PUT:
        case BuiltIn::SYS_REG:
        case BuiltIn::SYS_VAL:
        case BuiltIn::SYS_COPY:
        case BuiltIn::CAP:
        case BuiltIn::LONG:
        case BuiltIn::SHORT:
        case BuiltIn::HALT:
        case BuiltIn::COPY:
        case BuiltIn::BYTESIZE:
        case BuiltIn::ENTIER:
        case BuiltIn::BITS:
        case BuiltIn::SYS_MOVE:
        case BuiltIn::SYS_NEW:
        case BuiltIn::SYS_ROT:
        case BuiltIn::SYS_LSH:
        case BuiltIn::SYS_GETREG:
        case BuiltIn::SYS_PUTREG:
        case BuiltIn::SYS_TYP:
        case BuiltIn::CAST:
        case BuiltIn::WCHR:
        default:
            // TODO: some should be implemented!
            error( me, Evaluator::tr("built-in procedure not supported in const expressions") );
            break;
        }
        val = Evaluator::Result();
    }

    void visit( BinExpr* me)
    {
        Evaluator::Result lhs, rhs;
        if( me->d_lhs )
        {
            me->d_lhs->accept(this);
            lhs = val;
        }
        if( me->d_rhs )
        {
            me->d_rhs->accept(this);
            rhs = val;
        }
        switch( me->d_op )
        {
        case BinExpr::IN:
            IN(lhs,rhs,me);
            break;
        case BinExpr::ADD:
            ADD(lhs,rhs,me);
            break;
        case BinExpr::SUB:
            SUB(lhs,rhs,me);
            break;
        case BinExpr::FDIV:
            FDIV(lhs,rhs,me);
            break;
        case BinExpr::MUL:
            MUL(lhs,rhs,me);
            break;
        case BinExpr::DIV:
            DIV(lhs,rhs,me);
            break;
        case BinExpr::MOD:
            MOD(lhs,rhs,me);
            break;
        case BinExpr::AND:
            AND(lhs,rhs,me);
            break;
        case BinExpr::OR:
            OR(lhs,rhs,me);
            break;
        case BinExpr::EQ:
            EQ(lhs,rhs,me);
            break;
        case BinExpr::NEQ:
            NEQ(lhs,rhs,me);
            break;
        case BinExpr::LT:
            LE(lhs,rhs,me);
            break;
        case BinExpr::LEQ:
            LEQ(lhs,rhs,me);
            break;
        case BinExpr::GT:
            GT(lhs,rhs,me);
            break;
        case BinExpr::GEQ:
            GEQ(lhs,rhs,me);
            break;
        default:
            error(me,Evaluator::tr("operator not supported for constants"));
        }
    }
};

Evaluator::Result Evaluator::eval(Expression* e, Scope* m, bool supportVla, Errors* err)
{
    Q_ASSERT( m != 0 && e != 0 );
    EvalVisitor ev(m, supportVla,err);
    try
    {
        e->accept( &ev );
        return ev.val;
    }catch(int)
    {
        Result r;
        r.d_vtype = Literal::Integer;
        r.d_dyn = true;
        return r;
    }catch(const char*)
    { 
        return Result();
    }
}
