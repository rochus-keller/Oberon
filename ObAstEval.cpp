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

#include "ObAstEval.h"
#include "ObAst.h"
#include <QTextStream>
using namespace Ob;
using namespace Ob::Ast;


static inline QVariant expErr( QString* err, const QString& msg )
{
    if( err )
        *err = msg;
    return QVariant();
}

static QVariant evalNamedConst(Ast::Expression* e, QString* err)
{
    Ast::Named* n = e->getIdent();
    QVariant res;
    if( n && n->getTag() == Ast::Thing::T_Const )
        res = static_cast<Ast::Const*>(n)->d_val;
    if( !res.isValid() )
        return expErr( err, Ast::Model::tr("not a const expression") );
    else
        return res;
}

static bool setSet( Ast::Set& s, Ast::Expression* e, QString* err )
{
    const QVariant v = Ast::Eval::evalConstExpr(e,err);
    const qint64 b = v.toLongLong();
    if( v.type() != QVariant::LongLong || b < 0 || b >= Ast::SET_BIT_LEN )
    {
        if( err )
            *err = Ast::Model::tr("invalid set part");
        return false;
    }
    s.set(b);
    return true;
}

static bool setSet( Ast::Set& s, Ast::Expression* lhs, Ast::Expression* rhs, QString* err )
{
    const QVariant lv = Ast::Eval::evalConstExpr(lhs,err);
    const qint64 l = lv.toLongLong();
    const QVariant rv = Ast::Eval::evalConstExpr(rhs,err);
    const qint64 r = rv.toLongLong();
    if( lv.type() != QVariant::LongLong || l < 0 || l >= Ast::SET_BIT_LEN ||
            rv.type() != QVariant::LongLong || r < 0 || r >= Ast::SET_BIT_LEN )
    {
        if( err )
            *err = Ast::Model::tr("invalid set part");
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

QVariant Ast::Eval::evalConstExpr(Ast::Expression* e, QString* err)
{
    if( e == 0 )
        return QVariant();
    switch( e->getTag() )
    {
    case Thing::T_Literal:
        return static_cast<Literal*>(e)->d_val;
    case Thing::T_SetExpr:
        {
            SetExpr* se = static_cast<SetExpr*>(e);
            Set s;
            for(int i = 0; i < se->d_parts.size(); i++ )
            {
                if( se->d_parts[i]->getTag() == Thing::T_BinExpr )
                {
                    BinExpr* be = static_cast<BinExpr*>(se->d_parts[i].data());
                    if( be->d_op != BinExpr::Range )
                        return expErr( err, tr("invalid set part") );
                    if( !setSet( s, be->d_lhs.data(), be->d_rhs.data(), err) )
                        return QVariant();
                }else if( !setSet( s, se->d_parts[i].data(), err) )
                        return QVariant();
            }
            return QVariant::fromValue(s);
        }
        break;
    case Thing::T_IdentLeaf:
    case Thing::T_IdentSel:
        return evalNamedConst(e,err);
    case Thing::T_UnExpr:
        {
            UnExpr* ue = static_cast<UnExpr*>(e);
            if( ue->d_op == UnExpr::SEL )
                return evalNamedConst(e,err);
            else
            {
                const QVariant v = evalConstExpr(ue->d_sub.data(),err);
                if( !v.isValid() )
                    return v;

                return Eval::unOp(ue->d_op, v,err);
            }
        }
        break;
    case Thing::T_BinExpr:
        {
            BinExpr* be = static_cast<BinExpr*>(e);
            switch( be->d_op )
            {
            case BinExpr::Range:
            case BinExpr::Index:
            case BinExpr::IS:
                return expErr(err,tr("operator not supported for constants"));
            }
            const QVariant lhs = evalConstExpr(be->d_lhs.data(), err );
            if( !lhs.isValid() )
                return lhs;
            const QVariant rhs = evalConstExpr(be->d_rhs.data(), err );
            if( !rhs.isValid() )
                return rhs;
            return binOp( be->d_op, lhs, rhs, err );
        }
        break;
    case Thing::T_CallExpr:
        return expErr(err,tr("operation not supported in a const expression"));
    default:
        Q_ASSERT( false );
    }
    return QVariant();
}

QVariant Eval::NEG(const QVariant& v, QString* err)
{
    if( v.type() == QVariant::Double )
        return -v.toDouble();
    if( v.type() == QVariant::LongLong )
        return -v.toLongLong();
    else
        return expErr(err,tr("cannot invert sign of non numerical expression"));
}

QVariant Eval::NOT(const QVariant& v, QString* err)
{
    if( v.type() == QVariant::Bool )
        return !v.toBool();
    return expErr(err,tr("cannot negate non boolean expression"));
}

QVariant Eval::ADD(const QVariant& lhs, const QVariant& rhs, QString* err)
{
    if( rhs.type() == QVariant::LongLong )
        return lhs.toLongLong() + rhs.toLongLong();
    else if( rhs.type() == QVariant::Double )
        return lhs.toDouble() + rhs.toDouble();
    else if( lhs.canConvert<Set>() && rhs.canConvert<Set>() )
        return QVariant::fromValue( lhs.value<Set>() | rhs.value<Set>() );
    else
        return expErr(err,tr("operand types incompatible with operator") );
}

QVariant Eval::SUB(const QVariant& lhs, const QVariant& rhs, QString* err)
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
        return expErr(err,tr("operand types incompatible with operator") );
}

QVariant Eval::FDIV(const QVariant& lhs, const QVariant& rhs, QString* err)
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
        return expErr(err,tr("operand types incompatible with operator") );
}

QVariant Eval::MUL(const QVariant& lhs, const QVariant& rhs, QString* err)
{
    if( rhs.type() == QVariant::LongLong )
        return lhs.toLongLong() * rhs.toLongLong();
    else if( rhs.type() == QVariant::Double )
        return lhs.toDouble() * rhs.toDouble();
    else if( lhs.canConvert<Set>() && rhs.canConvert<Set>() )
        return QVariant::fromValue( lhs.value<Set>() & rhs.value<Set>() );
    else
        return expErr(err,tr("operand types incompatible with operator") );
}

QVariant Eval::DIV(const QVariant& lhs, const QVariant& rhs, QString* err)
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
        return expErr(err,tr("operand types incompatible with operator") );
}

QVariant Eval::MOD(const QVariant& lhs, const QVariant& rhs, QString* err)
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
        return expErr(err,tr("operand types incompatible with operator") );
}

QVariant Eval::AND(const QVariant& lhs, const QVariant& rhs, QString* err)
{
    if( rhs.type() == QVariant::Bool )
        return lhs.toBool() && rhs.toBool();
    else
        return expErr(err,tr("operand types incompatible with operator") );
}

QVariant Eval::OR(const QVariant& lhs, const QVariant& rhs, QString* err)
{
    if( rhs.type() == QVariant::Bool )
        return lhs.toBool() || rhs.toBool();
    else
        return expErr(err,tr("operand types incompatible with operator") );
}

QVariant Eval::EQ(const QVariant& lhs, const QVariant& rhs, QString* err)
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
        return expErr(err,tr("operand types incompatible with operator") );
}

QVariant Eval::NEQ(const QVariant& lhs, const QVariant& rhs, QString* err)
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
        return expErr(err,tr("operand types incompatible with operator") );
}

QVariant Eval::LE(const QVariant& lhs, const QVariant& rhs, QString* err)
{
    if( rhs.type() == QVariant::LongLong )
        return lhs.toLongLong() < rhs.toLongLong();
    else if( rhs.type() == QVariant::Double )
        return lhs.toDouble() < rhs.toDouble();
    else if( lhs.type() == QVariant::ByteArray && rhs.type() == QVariant::ByteArray )
        return lhs.toByteArray() < rhs.toByteArray();
    else
        return expErr(err,tr("operand types incompatible with operator") );
}

QVariant Eval::LEQ(const QVariant& lhs, const QVariant& rhs, QString* err)
{
    if( rhs.type() == QVariant::LongLong )
        return lhs.toLongLong() <= rhs.toLongLong();
    else if( rhs.type() == QVariant::Double )
        return lhs.toDouble() <= rhs.toDouble();
    else if( lhs.type() == QVariant::ByteArray && rhs.type() == QVariant::ByteArray )
        return lhs.toByteArray() <= rhs.toByteArray();
    else
        return expErr(err,tr("operand types incompatible with operator") );
}

QVariant Eval::GT(const QVariant& lhs, const QVariant& rhs, QString* err)
{
    if( rhs.type() == QVariant::LongLong )
        return lhs.toLongLong() > rhs.toLongLong();
    else if( rhs.type() == QVariant::Double )
        return lhs.toDouble() > rhs.toDouble();
    else if( lhs.type() == QVariant::ByteArray && rhs.type() == QVariant::ByteArray )
        return lhs.toByteArray() > rhs.toByteArray();
    else
        return expErr(err,tr("operand types incompatible with operator") );
}

QVariant Eval::GEQ(const QVariant& lhs, const QVariant& rhs, QString* err)
{
    if( rhs.type() == QVariant::LongLong )
        return lhs.toLongLong() >= rhs.toLongLong();
    else if( rhs.type() == QVariant::Double )
        return lhs.toDouble() >= rhs.toDouble();
    else if( lhs.type() == QVariant::ByteArray && rhs.type() == QVariant::ByteArray )
        return lhs.toByteArray() >= rhs.toByteArray();
    else
        return expErr(err,tr("operand types incompatible with operator") );
}

QVariant Eval::IN(const QVariant& lhs, const QVariant& rhs, QString* err)
{
    const qint64 b = lhs.toLongLong();
    if( lhs.type() != QVariant::LongLong || !rhs.canConvert<Set>() || b < 0 || b >= SET_BIT_LEN )
        return expErr(err,tr("invalid data type for operator IN") );
    return rhs.value<Set>().test(b);
}

QVariant Eval::binOp(quint8 op, const QVariant& lhs, const QVariant& rhs, QString* err)
{
    if( op == BinExpr::IN )
        return IN(lhs,rhs,err);
    if( rhs.type() != lhs.type() )
        return expErr(err,tr("operands not of same type") );

    switch( op )
    {
    case BinExpr::ADD:
        return ADD(lhs,rhs,err);
    case BinExpr::SUB:
        return SUB(lhs,rhs,err);
    case BinExpr::FDIV:
        return FDIV(lhs,rhs,err);
    case BinExpr::MUL:
        return MUL(lhs,rhs,err);
    case BinExpr::DIV:
        return DIV(lhs,rhs,err);
    case BinExpr::MOD:
        return MOD(lhs,rhs,err);
    case BinExpr::AND:
        return AND(lhs,rhs,err);
    case BinExpr::OR:
        return OR(lhs,rhs,err);
    case BinExpr::EQ:
        return EQ(lhs,rhs,err);
    case BinExpr::NEQ:
        return NEQ(lhs,rhs,err);
    case BinExpr::LT:
        return LE(lhs,rhs,err);
    case BinExpr::LEQ:
        return LEQ(lhs,rhs,err);
    case BinExpr::GT:
        return GT(lhs,rhs,err);
    case BinExpr::GEQ:
        return GEQ(lhs,rhs,err);
    default:
        return expErr(err,tr("operator not supported"));
    }
    return QVariant();
}

QVariant Eval::unOp(quint8 op, const QVariant& v, QString* err)
{
    switch( op )
    {
    case UnExpr::NEG:
        return NEG(v,err);
    case UnExpr::NOT:
        return NOT(v,err);
    default:
        return expErr(err,tr("operator not supported"));
    }
    return QVariant();
}

struct Printer : public AstVisitor
{
    bool namedType( Type* t )
    {
        if( t->d_ident && t->d_ident != curNamed )
        {
            out << "( TREF " << t->d_ident->d_name << " ) ";
            return true;
        }
        return false;
    }

    void visit( BaseType* t)
    {
        if( namedType(t) )
            return;
        out << BaseType::s_typeName[t->d_type] << " ";
    }
    void visit( Pointer* t)
    {
        if( namedType(t) )
            return;
        out << "POINTER ";
        if( t->d_to.isNull() )
            out << "? ";
        else
            t->d_to->accept(this);
    }
    void visit( Array* t )
    {
        if( namedType(t) )
            return;
        out << "ARRAY " << t->d_len << " ";
        if( t->d_type.isNull() )
            out << "? ";
        else
            t->d_type->accept(this);
    }
    void visit( Record* t )
    {
        if( namedType(t) )
            return;
        out << "RECORD ";
        if( !t->d_base.isNull() )
        {
            out << "( TREF ";
            t->d_base->accept(this);
            out << " )";
        }
        d_level++;
        for( int i = 0; i < t->d_fields.size(); i++ )
        {
            out << endl;
            out << ws() << t->d_fields[i]->d_name << " ";
            if( t->d_fields[i].isNull() )
                out << "? ";
            else
                t->d_fields[i]->d_type->accept(this);
        }
        d_level--;
    }
    void visit( ProcType* t )
    {
        if( namedType(t) )
            return;
        out << "PROC ";
        if( !t->d_formals.isEmpty() )
            out << "( ";
        for( int i = 0; i < t->d_formals.size(); i++ )
        {
            out << t->d_formals[i]->d_name << " ";
            // formals also appear as part of scope names
        }
        if( !t->d_formals.isEmpty() )
            out << ")";
    }
    void visit( QualiType* t )
    {
        if( namedType(t) )
            return;
        t->d_quali->accept(this);
    }
    void visit( Field* n)
    {
        out << ws() << n->d_name << " ";
        if( n->d_type.isNull() )
            out << "? ";
        else
            n->d_type->accept(this);
        out << endl;
    }

    void renderLive( Named* r )
    {
        out << " ";
        if( r->d_liveFrom )
            out << "live " << r->d_liveFrom << "-" << r->d_liveTo << " ";
        if( r->d_usedFromSubs )
            out << "subs ";
        if( r->d_slotValid )
            out << "slot " << r->d_slot;
    }

    void renderVar( Named* r )
    {
        out << r->d_name << " ";
        if( r->d_type.isNull() )
            out << "?";
        else
            r->d_type->accept(this);
        renderLive(r);
        out << endl;
    }

    void visit( Variable* n )
    {
        out << ws() << "V ";
        renderVar( n );
    }

    void visit( LocalVar* n )
    {
        out << ws() << "V ";
        renderVar( n );
    }

    void visit( Parameter* n )
    {
        out << ws() << "P ";
        renderVar( n );
    }

    void visit( NamedType* n )
    {
        curNamed = n;
        out << ws() << "T " << n->d_name << " ";
        if( n->d_type.isNull() )
            out << "? ";
        else
            n->d_type->accept(this);
        out << endl;
        curNamed = 0;
    }
    void visit( Const* n )
    {
        out << ws() << "C " << n->d_name << " ";
        if( n->d_type.isNull() )
            out << "? ";
        else
            n->d_type->accept(this);
        out << "'" << n->d_val.toByteArray().simplified() << "'";
        out << " " << endl;
    }
    void visit( Import* n)
    {
        out << ws() << "I " << n->d_name << " ";
        out << n->d_mod->d_name;
        out << endl;
    }
    void visit( Procedure* m )
    {
        out << ws() << "PROCEDURE " << m->d_name << " ";
        if( m->d_type.isNull() )
            out << "? ";
        else
            m->d_type->accept(this);
        renderLive(m);
        out << endl;
        d_level++;
        for( int i = 0; i < m->d_order.size(); i++ )
            m->d_order[i]->accept(this);
        if( !m->d_body.isEmpty() )
        {
            out << ws() << "BEGIN" << endl;
            d_level++;

            for( int i = 0; i < m->d_body.size(); i++ )
                m->d_body[i]->accept(this);
            d_level--;
        }
        d_level--;
    }
    void visit( BuiltIn* ) {}
    void visit( Module* m )
    {
        out << ws() << ( m->d_isDef ? "DEFINITION " : "MODULE " ) << m->d_name << endl;
        d_level++;
        for( int i = 0; i < m->d_order.size(); i++ )
            m->d_order[i]->accept(this);
        if( !m->d_body.isEmpty() )
        {
            out << ws() << "BEGIN" << endl;
            d_level++;

            for( int i = 0; i < m->d_body.size(); i++ )
                m->d_body[i]->accept(this);
            d_level--;
        }
        d_level--;
    }
    void visit( Call* s)
    {
        out << ws();
        s->d_what->accept(this);
        out << endl;
    }
    void visit( Return* s)
    {
        out << ws() << "RETURN ";
        s->d_what->accept(this);
        out << endl;
    }
    void visit( Assign* s )
    {
        out << ws() << "ASSIG ";
        s->d_lhs->accept(this);
        out << ":= ";
        s->d_rhs->accept(this);
        out << endl;
    }
    void visit( IfLoop* s)
    {
        Q_ASSERT( s->d_if.size() == s->d_then.size() );
        for( int i = 0; i < s->d_if.size(); i++ )
        {
            out << ws();
            if( i == 0 )
                out << ( s->d_op == IfLoop::IF ? "IF " : ( s->d_op == IfLoop::WHILE ? "WHILE " : "REPEAT " ) );
            else
                out << "ELSIF ";
            s->d_if[i]->accept(this);
            out << "THEN " << endl;
            d_level++;
            const StatSeq& body = s->d_then[i];
            for( int j = 0; j < body.size(); j++ )
                body[j]->accept(this);
            d_level--;
        }
        if( !s->d_else.isEmpty() )
        {
            out << ws() << "ELSE" << endl;
            d_level++;
            const StatSeq& body = s->d_else;
            for( int j = 0; j < body.size(); j++ )
                body[j]->accept(this);
            d_level--;
        }
    }
    void visit( ForLoop* s )
    {
        out << ws() << "FOR " << s->d_id->d_name << " := ";
        s->d_from->accept(this);
        out << "TO ";
        s->d_to->accept(this);
        out << "BY ";
        s->d_by->accept(this);
        out << "DO " << endl;
        d_level++;
        const StatSeq& body = s->d_do;
        for( int j = 0; j < body.size(); j++ )
            body[j]->accept(this);
        d_level--;
    }
    void visit( CaseStmt* s )
    {
        out << ws() << "SWITCH ";
        s->d_exp->accept(this);
        out << endl;
        d_level++;
        for( int i = 0; i < s->d_cases.size(); i++ )
        {
            out << ws() << "CASE ";
            for( int j = 0; j < s->d_cases[i].d_labels.size(); j++ )
            {
                if( j != 0 )
                    out << "| ";
                s->d_cases[i].d_labels[j]->accept(this);
            }
            out << endl;
            d_level++;
            const StatSeq& body = s->d_cases[i].d_block;
            for( int j = 0; j < body.size(); j++ )
                body[j]->accept(this);
            d_level--;
        }
        d_level--;
    }
    void visit( Literal* e )
    {
        out << "'" << e->d_val.toByteArray().simplified() << "' ";
    }
    void visit( SetExpr* e )
    {
        out << "( SET ";
        for( int i = 0; i < e->d_parts.size(); i++ )
            e->d_parts[i]->accept(this);
        out << ") ";
    }
    void visit( IdentLeaf* e )
    {
        out << e->d_ident->d_name << " ";
    }
    void visit( UnExpr* e)
    {
        out << "( " << UnExpr::s_opName[e->d_op] << " ";
        e->d_sub->accept(this);
        if( e->d_op == UnExpr::CAST && !e->d_type.isNull() )
            e->d_type->accept(this);
        out << ") ";
    }
    void visit( IdentSel* e)
    {
        out << "( . ";
        e->d_sub->accept(this);
        out << e->d_ident->d_name << " ) ";
    }
    void visit( CallExpr* e)
    {
        out << "( CALL ";
        e->d_sub->accept(this);
        for( int i = 0; i < e->d_actuals.size(); i++ )
            e->d_actuals[i]->accept(this);
        out << ") ";
    }
    void visit( BinExpr* e )
    {
        out << "( " << BinExpr::s_opName[e->d_op] << " ";
        e->d_lhs->accept(this);
        e->d_rhs->accept(this);
        out << ") ";
    }
    QByteArray ws() const
    {
        QByteArray ws;
        for( int i = 0; i < d_level; i++ )
            ws += "|  ";
        return ws;
    }
    QTextStream& out;
    Named* curNamed;
    int d_level;
    Printer(QTextStream& o):out(o),d_level(0),curNamed(0) {}
};

void Eval::render(QTextStream& out, Thing* m)
{
    Printer p(out);
    m->accept(&p);
}

