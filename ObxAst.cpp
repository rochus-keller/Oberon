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

#include "ObxAst.h"
#include "ObLexer.h"
#include <QtDebug>
#include <limits>
using namespace Obx;
using namespace Ob;

const char* Thing::s_tagName[] =
{
    "Thing", "Module", "Import", "Pointer", "Record", "BaseType", "Array", "ProcType", "NamedType",
    "CallExpr", "Literal", "SetExpr", "IdentLeaf", "UnExpr", "IdentSel", "BinExpr", "Field",
    "Const", "BuiltIn", "Parameter", "Return", "Procedure", "Variable", "LocalVar",
    "QualiType", "Call", "Assign", "IfLoop", "ForLoop", "CaseStmt", "Scope",
    "Enumeration", "Generic", "Exit"
};

const char* BaseType::s_typeName[] =
{
    "ANY", "NIL", "STRING", "WSTRING", "BOOLEAN", "CHAR", "WCHAR", "BYTE",
    "SHORTINT", "INTEGER", "LONGINT", "REAL", "LONGREAL", "SET"
};

const char* BuiltIn::s_typeName[] =
{
    "ABS", "ODD", "LEN", "LSL", "ASR", "ROR", "FLOOR", "FLT", "ORD",
    "CHR", "INC", "DEC", "INCL", "EXCL", "NEW", "ASSERT", "PACK", "UNPK",
    "LED", "TRAP", "TRAPIF",
    "ADR", "BIT", "GET", "H", "LDREG", "PUT", "REG", "VAL", "COPY",
    "MAX", "CAP", "LONG", "SHORT", "HALT", "COPY", "ASH", "MIN", "SIZE", "ENTIER",
    "BITS",
    // Oberon-2 SYSTEM
    "MOVE", "NEW", "ROT", "LSH", "GETREG", "PUTREG",
    // Blackbox
    "TYP",
    // Oberon+
    "VAL", "STRLEN", "WCHR", "PRINTLN"
};

const char* UnExpr::s_opName[] =
{
    "???",
    "NEG", "NOT", "DEREF", "ADDROF", "CAST", "SEL", "CALL", "IDX"
};

const char* BinExpr::s_opName[] =
{
    "???",
    "Range",
    "EQ", "NEQ", "LE", "LEQ", "GT", "GEQ", "IN", "IS",
    "ADD", "SUB", "OR",
    "MUL", "FDIV", "DIV", "MOD", "AND"
};

#define _USE_VISITED_SET

struct ObxAstPrinter : public AstVisitor
{
#ifdef _USE_VISITED_SET
    QSet<Type*> visited;
    bool namedType( Type* t )
    {
        //if( t->d_minst )
            out << t << " ";
            if( t->d_slotValid )
                out << "slot " << t->d_slot << " ";
        return false;
    }
#else
    bool namedType( Type* t )
    {
        if( t->d_decl && t->d_decl->getTag() == Thing::T_NamedType && t->d_decl != curNamed && !t->d_minst )
        {
            out << "( TREF " << t->d_decl->d_name << " ) ";
            return true;
        }
        return false;
    }
#endif

    void visit( BaseType* t)
    {
        if( namedType(t) )
            return;
        out << BaseType::s_typeName[t->d_baseType] << " ";
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
        d_level++;
        if( !t->d_base.isNull() )
        {
            out << endl << ws();
            out << "BASE ";
            out << t->d_baseRec << " ";
            t->d_base->accept(this);
            out << " ";
        }
        for( int i = 0; i < t->d_fields.size(); i++ )
        {
            out << endl;
            out << ws() << "F " << t->d_fields[i]->d_name << " ";
            if( t->d_fields[i]->d_type.isNull() )
                out << "? ";
            else
                t->d_fields[i]->d_type->accept(this);
        }
        for( int i = 0; i < t->d_methods.size(); i++ )
        {
            out << endl;
            out << ws() << "M " << t->d_methods[i]->d_name << " ";
            if( t->d_methods[i]->d_type.isNull() )
                out << "? ";
            else
                t->d_methods[i]->d_type->accept(this);
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
        Q_ASSERT( false );
        out << ws() << "F " << n->d_name << " ";
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
        if( r->d_slotValid )
            out << "slot " << r->d_slot;
        if( r->d_upvalSource )
            out << "uvsrc ";
        if( r->d_upvalIntermediate )
            out << "uvint ";
        if( r->d_upvalSink )
            out << "uvsnk ";
    }

    void renderVar( Named* r )
    {
        out << r->d_name << " ";
        if( r->d_slotValid )
            out << "slot " << r->d_slot << " ";
        if( r->d_type.isNull() )
            out << "?";
        else
            r->d_type->accept(this);
        //renderLive(r);
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
#ifdef _USE_VISITED_SET
        visited.insert(n->d_type.data());
#endif
        out << ws() << "T " << n->d_name << " ";
        if( n->d_slotValid )
            out << "slot " << n->d_slot << " ";
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
        if( n->d_slotValid )
            out << "slot " << n->d_slot << " ";
        out << n->d_mod->d_name;
        if( !n->d_metaActuals.isEmpty() )
        {
            out << "( METAINST ";

            d_level++;
            for( int i = 0; i < n->d_metaActuals.size(); i++ )
            {
                out << endl << ws();
                out << "TPAR ";
                n->d_metaActuals[i]->accept(this);
            }
            d_level--;

            out << " ) ";
        }
        out << endl;
    }
    void visit( Procedure* m )
    {
        if( m->d_receiver )
        {
            out << ws() << "METHOD ";
            m->d_receiver->d_type->accept(this);
            out << ". " << m->d_name << " ";
        }else
            out << ws() << "PROCEDURE " << m->d_name << " ";
        if( m->d_slotValid )
            out << "slot " << m->d_slot << " ";
        if( m->d_type.isNull() )
            out << "? ";
        else
            m->d_type->accept(this);
        //renderLive(m);
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
        out << ws() << ( m->d_isDef ? "DEFINITION " : "MODULE " ) << m->getName() << " " << m << endl;
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
            {
                switch( s->d_op )
                {
                case IfLoop::IF:
                    out << "IF ";
                    break;
                case IfLoop::WHILE:
                    out << "WHILE ";
                    break;
                case IfLoop::REPEAT:
                    out << "REPEAT ";
                    break;
                case IfLoop::WITH:
                    out << "WITH ";
                    break;
                case IfLoop::LOOP:
                    out << "LOOP ";
                    break;
                default:
                    Q_ASSERT(false);
                }
            }else
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
        out << ws() << "FOR " << s->d_id->getIdent()->d_name << " := ";
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
        if( !e->d_ident.isNull() )
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
        if( !e->d_ident.isNull() )
            out << e->d_ident->d_name;
        else
            out << "? ";
        out << " ) ";
    }
    void visit( ArgExpr* e)
    {
        out << "( ";
        switch( e->d_op )
        {
        case ArgExpr::CALL:
            out << "CALL ";
            break;
        case ArgExpr::CAST:
            out << "CAST ";
            break;
        case ArgExpr::IDX:
            out << "IDX ";
            break;
        default:
            Q_ASSERT( false );
        }

        e->d_sub->accept(this);
        for( int i = 0; i < e->d_args.size(); i++ )
            e->d_args[i]->accept(this);
        out << ") ";
    }
    void visit( BinExpr* e )
    {
        out << "( " << BinExpr::s_opName[e->d_op] << " ";
        e->d_lhs->accept(this);
        e->d_rhs->accept(this);
        out << ") ";
    }

    void visit( Exit* me)
    {
        out << ws() << "EXIT ";
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
    ObxAstPrinter(QTextStream& o):out(o),d_level(0),curNamed(0) {}
};

Named*Scope::find(const QByteArray& name, bool recursive) const
{
    Names::const_iterator i = d_names.find( name.constData() );
    if( i != d_names.end() )
        return i.value();
    if( recursive && d_scope )
        return d_scope->find(name);
    else
        return 0;
}

bool Scope::add(Named* n)
{
    Q_ASSERT( n != 0 );
    if( find(n->d_name,false) )
        return false;
    // else
    d_names[n->d_name.constData()] = n;
    d_order.append(n);
    n->d_scope = this;
    switch( n->getTag() )
    {
    case Thing::T_Parameter:
        d_parCount++;
        break;
    case Thing::T_Variable:
    case Thing::T_LocalVar:
        d_varCount++;
        break;
    }

    return true;
}

BuiltIn::BuiltIn(quint8 f, ProcType* pt):d_func(f)
{
    d_name = Lexer::getSymbol(s_typeName[f]);
    if( pt )
        d_type = pt;
    else
        d_type = new ProcType();
    Q_ASSERT( d_type->d_decl == 0 );
    d_type->d_decl = this;
}

ProcType::ProcType(const Type::List& f, Type* r):d_return(r)
{
    for( int i = 0; i < f.size(); i++ )
    {
        Ref<Parameter> p = new Parameter();
        p->d_type = f[i];
        d_formals.append(p);
    }
}

ProcType::ProcType(const Type::List& f, const ProcType::Vars& var, Type* r)
{
    Q_ASSERT( f.size() == var.size() );
    for( int i = 0; i < f.size(); i++ )
    {
        Ref<Parameter> p = new Parameter();
        p->d_type = f[i];
        p->d_var = var[i];
        d_formals.append(p);
    }
}

Parameter*ProcType::find(const QByteArray& name) const
{
    if( name.isEmpty() )
        return 0;
    for( int i = 0; i < d_formals.size(); i++ )
    {
        if( d_formals[i]->d_name.constData() == name.constData() )
            return d_formals[i].data();
    }
    return 0;
}

bool ProcType::isBuiltIn() const
{
    return d_decl && d_decl->getTag() == Thing::T_BuiltIn;
}

ProcType*ArgExpr::getProcType() const
{
    Q_ASSERT( !d_sub.isNull() && !d_sub->d_type.isNull() && d_sub->d_type->derefed()->getTag() == Thing::T_ProcType );
    return cast<ProcType*>( d_sub->d_type->derefed() );
}

ArgExpr*Call::getCallExpr() const
{
    Q_ASSERT( !d_what.isNull() && d_what->getTag() == Thing::T_ArgExpr );
    return cast<ArgExpr*>( d_what.data() );
}

Module* Named::getModule()
{
    if( getTag() == Thing::T_Module )
        return cast<Module*>(this);
    else if( d_scope )
        return d_scope->getModule();
    else
        return 0;
}

const char*Named::visibilitySymbol() const
{
    switch( d_visibility)
    {
    case NotApplicable:
    case Private:
    default:
        return "";
    case ReadWrite:
        return "*";
    case ReadOnly:
        return "-";
    }
}

void Thing::setSlot(quint32 s)
{
    Q_ASSERT( !d_slotValid );
    d_slotValid = true;
    Q_ASSERT( s <= MAX_SLOT );
    d_slot = s;
}

void Thing::dump(QIODevice* d)
{
    if( d )
    {
        QTextStream out(d);
        ObxAstPrinter p(out);
        accept(&p);
    }else
    {
        QTextStream out(stdout);
        ObxAstPrinter p(out);
        accept(&p);
    }
}

QualiType::ModItem QualiType::getQuali() const
{
    Q_ASSERT( !d_quali.isNull() );
    ModItem res;
    res.second = d_quali->getIdent();

    const int tag = d_quali->getTag();
    switch( tag )
    {
    case Thing::T_IdentLeaf:
        break; // NOP
    case Thing::T_IdentSel:
        {
            IdentSel* i = cast<IdentSel*>( d_quali.data() );
            Q_ASSERT( !i->d_sub.isNull() && i->d_sub->getTag() == Thing::T_IdentLeaf );
            res.first = i->d_sub->getIdent();
        }
        break;
    default:
        Q_ASSERT( false );
        break;
    }

    return res;
}

bool QualiType::hasByteSize() const
{
    if( !d_quali->d_type.isNull() )
        return d_quali->d_type->hasByteSize();
    else
        return false;
}

Type*QualiType::derefed()
{
    Q_ASSERT( !d_quali.isNull() );
    if( d_quali->d_type.isNull() )
        return this; // never return 0 with derefed
    else if( d_quali->d_type.data() == this )
    {
        qWarning() << "qualident referring to itself" << d_quali->getModule()->d_file << d_loc.d_row << d_loc.d_col;
        return this;
    }else
        return d_quali->d_type->derefed();
}

QString QualiType::pretty() const
{
    Type* t = const_cast<QualiType*>(this)->derefed();
    if( t && t != this )
        return t->pretty();
    if( d_quali.isNull() )
        return "?";
    const int qtag = d_quali->getTag();
    if( qtag == Thing::T_IdentSel )
    {
        IdentSel* s = cast<IdentSel*>( d_quali.data() );
        if( s->d_sub && s->d_sub->getTag() == Thing::T_IdentLeaf )
            return QString("%1.%2").arg(cast<IdentLeaf*>(s->d_sub.data())->d_name.constData()).arg(s->d_name.constData());
        else
            return QString("?.%1").arg(s->d_name.constData());
    }else if( qtag == Thing::T_IdentLeaf )
        return cast<IdentLeaf*>(d_quali.data())->d_name;
    return "?";
}

IdentLeaf::IdentLeaf(Named* id, const Ob::RowCol& loc, Module* mod, Type* t, IdentRole r):d_ident(id),d_role(r)
{
    d_loc = loc;
    d_type = t;
    d_mod = mod;
}

#ifdef _DEBUG

QSet<Thing*> Thing::insts;

Thing::Thing():d_slot(0),d_slotValid(false),d_slotAllocated(false),d_visited(false)
{
    insts.insert(this);
}

Thing::~Thing()
{
    insts.remove(this);
}

#endif

Named*Record::find(const QByteArray& name, bool recursive) const
{
    Names::const_iterator i = d_names.find(name.constData());
    if( i != d_names.end() )
        return i.value();
    if( recursive && d_baseRec != 0 )
        return d_baseRec->find(name,recursive);
    return 0;
}

QList<Field*> Record::getOrderedFields() const
{
    QList<Field*> res;
    if( d_baseRec )
        res = d_baseRec->getOrderedFields();
    for( int i = 0; i < d_fields.size(); i++ )
    {
        if( d_fields[i]->d_super )
        {
            Q_ASSERT( d_fields[i]->d_super->d_slotValid && d_fields[i]->d_super->d_slot < d_fields.size() );
            res[ d_fields[i]->d_super->d_slot ] = d_fields[i].data();
        }else
            res.append( d_fields[i].data() );
    }
    return res;
}

bool Array::hasByteSize() const
{
    if( d_lenExpr.isNull() )
        return false;
    else if( d_type )
        return d_type->hasByteSize();
    else
        return false;
}

Type*Array::getTypeDim(int& dims, bool openOnly) const
{
    Type* t = d_type.isNull() ? 0 : d_type->derefed();
    if( t && t->getTag() == Thing::T_Array )
    {
        Array* a = cast<Array*>( t );
        if( openOnly )
        {
            if( !a->d_lenExpr.isNull() )
                return d_type.data();
        }else
        {
            if( a->d_lenExpr.isNull() )
                return d_type.data();
        }
        dims += 1;
        return a->getTypeDim(dims, openOnly);
    }else
    {
        dims = 1;
        return d_type.data();
    }
}

QString Array::pretty() const
{
    if( d_type.isNull() )
        return "ARRAY OF ?";
    return QString("ARRAY OF %1").arg(d_type->pretty());
}

QList<Array*> Array::getDims()
{
    QList<Array*> res;
    res << this;
    Type* t = d_type.isNull() ? 0 : d_type->derefed();
    if( t && t->getTag() == Thing::T_Array )
        res << cast<Array*>(t)->getDims();
    return res;
}

ProcType*Procedure::getProcType() const
{
    Q_ASSERT( !d_type.isNull() && d_type->getTag() == Thing::T_ProcType );
    return cast<ProcType*>( d_type.data() );
}

quint8 IdentSel::visibilityFor(Module* m) const
{
    if( !d_ident.isNull() )
    {
        Module* im = d_ident->getModule();
        if( im == m )
            return Named::NotApplicable;
        if( d_ident->d_visibility == Named::ReadOnly )
            return Named::ReadOnly;

    }
    return UnExpr::visibilityFor(m);
}


Const::Const(const QByteArray& name, Literal* lit)
{
    d_name = name;
    d_constExpr = lit;
    d_vtype = 0;
    d_wide = false;
    if( lit )
    {
        d_type = lit->d_type.data();
        d_val = lit->d_val;
        d_vtype = lit->d_vtype;
        d_wide = lit->d_wide;
    }
}


QString Pointer::pretty() const
{
    if( d_to.isNull() )
        return "POINTER TO ?";
    return QString("POINTER TO %1").arg(d_to->pretty());
}

QList<Expression*> Expression::getSubList() const
{
    QList<Expression*> res;
    res.push_back(const_cast<Expression*>(this));
    Expression* sub = getSub();
    while( sub )
    {
        res.prepend(sub);
        sub = sub->getSub();
    }
    return res;
}


QVariant BaseType::maxVal() const
{
    switch( d_baseType )
    {
    case BOOLEAN:
        return true;
    case CHAR:
        return std::numeric_limits<quint8>::max();
    case WCHAR:
        return std::numeric_limits<quint16>::max();
    case BYTE:
        return std::numeric_limits<quint8>::max();
    case SET:
        return 31;
    case SHORTINT:
        return std::numeric_limits<qint16>::max();
    case INTEGER:
        return std::numeric_limits<qint32>::max();
    case LONGINT:
        return std::numeric_limits<qint64>::max();
    case REAL:
        return std::numeric_limits<float>::max();
    case LONGREAL:
        return std::numeric_limits<double>::max();
    }
    return QVariant();
}

QVariant BaseType::minVal() const
{
    switch( d_baseType )
    {
    case BOOLEAN:
        return false;
    case CHAR:
    case WCHAR:
    case BYTE:
    case SET:
        return 0;
    case SHORTINT:
        return std::numeric_limits<qint16>::min();
    case INTEGER:
        return std::numeric_limits<qint32>::min();
    case LONGINT:
        return std::numeric_limits<qint64>::min();
    case REAL:
        return std::numeric_limits<float>::min();
    case LONGREAL:
        return std::numeric_limits<double>::min();
    }
    return QVariant();
}

Named*Type::findDecl(bool recursive) const
{
    if( d_decl )
        return d_decl;
    else if( d_binding )
    {
        if( recursive )
            return d_binding->findDecl();
        else
            return d_binding->d_decl;
    }else
        return 0;
}

bool Type::isText(bool* wide) const
{
    if( isString() || isChar() )
    {
        if( wide )
            *wide = d_baseType == WCHAR || d_baseType == WSTRING;
        return true;
    }
    if( getTag() == Thing::T_Array )
    {
        Array* a = cast<Array*>(const_cast<Type*>(this));
        Type* t = a->d_type.data();
        if( t )
            t = t->derefed();
        if( t && t->isChar() )
        {
            if( wide )
                *wide = d_baseType == WCHAR;
            return true;
        }
    }
    if( wide )
        *wide = false;
    return false;
}

Record*Type::toRecord() const
{
    Type* t = const_cast<Type*>(this)->derefed();
    if( t && t->getTag() == Thing::T_Pointer )
        t = cast<Pointer*>(t)->d_to.data();
    if( t )
        t = t->derefed();
    if( t && t->getTag() == Thing::T_Record )
        return cast<Record*>(t);
    else
        return 0;
}

bool Literal::isWide(const QString& str)
{
    for( int i = 0; i < str.size(); i++ )
    {
        if( str[i].unicode() > 255 )
            return true;
    }
    return false;
}

QByteArray Module::getName() const
{
    if( d_fullName.isEmpty() )
        return d_name;
#if 0
    if( d_instSuffix.isEmpty() )
        return d_fullName.join('.');
    else
        return d_fullName.join('.') + "." + d_instSuffix;
#else
    QByteArray name = d_fullName.join('.');
    if( !d_metaActuals.isEmpty() )
    {
        name += "("; // use () instead of <> so the name can be used in the file system too
        for( int i = 0; i < d_metaActuals.size(); i++ )
        {
            if( i != 0 )
                name += ",";
            Type* td = d_metaActuals[i]->derefed();
            Q_ASSERT( td );
            Named* n = td->findDecl();
            if( n )
                name += n->getName();
            else
                name += "?";
        }
        name += ")";
    }
    return name;
#endif
}
