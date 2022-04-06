/*
* Copyright 2021 Rochus Keller <mailto:me@rochus-keller.ch>
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
    "NONE", "ANY", "ANYREC", "VOID", "NIL", "#BYTEARRAY", "#STRING", "#WSTRING", "BOOLEAN", "CHAR", "WCHAR", "BYTE",
    "SHORTINT", "INTEGER", "LONGINT", "REAL", "LONGREAL", "SET", "#ENUMINT"
};

const char* BuiltIn::s_typeName[] =
{
    "ABS", "ODD", "LEN", "LSL", "ASR", "ROR", "FLOOR", "FLT", "ORD",
    "CHR", "INC", "DEC", "INCL", "EXCL", "NEW", "ASSERT", "PACK", "UNPK",
    "LED", "TRAP", "TRAPIF", "TRACE", "NOP", "LDMOD", "LDCMD",
    "ADR", "BIT", "GET", "H", "LDREG", "PUT", "REG", "VAL", "COPY",
    "MAX", "CAP", "LONG", "SHORT", "HALT", "COPY", "ASH", "MIN", "SIZE", "ENTIER",
    "BITS",
    // Oberon-2 SYSTEM
    "MOVE", "NEW", "ROT", "LSH", "GETREG", "PUTREG",
    // Blackbox
    "TYP",
    // Oberon+
    "CAST", "STRLEN", "WCHR", "PRINTLN", "DEFAULT", "BITAND", "BITNOT", "BITOR", "BITXOR",
    "BITSHL", "BITSHR", "BITASR", "ADR"
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
        out << "ARRAY ";
        if( t->d_vla )
            out << "var ";
        else
            out << t->d_len << " ";
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
        if( s->d_what )
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
        // LOOP has no if, only then; Q_ASSERT( s->d_if.size() == s->d_then.size() );
        for( int i = 0; i < s->d_then.size(); i++ )
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
            if( i < s->d_if.size() )
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

QByteArrayList BuiltIn::getValidNames()
{
    QByteArrayList res;
    for( int i = 0; i < MAXBUILTIN; i++ )
    {
        if( (i >= SYS_ADR && i <= SYS_COPY) ||
                (i >= SYS_MOVE && i <= SYS_TYP) )
            continue;
        res.append(s_typeName[i]);
    }
    return res;
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

void ProcType::addNonLocal(Named* n)
{
    if( !d_nonLocals.contains(n) )
        d_nonLocals.append(n);
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

Module* Named::getModule() const
{
    if( getTag() == Thing::T_Module )
        return cast<Module*>(const_cast<Named*>(this));
    else if( d_scope )
        return d_scope->getModule();
    else
        return 0;
}

QByteArrayList Named::getQualifiedName() const
{
    QByteArrayList res;
    res.append( d_name );
    Named* scope = d_scope;
    while( scope && !scope->d_name.isEmpty() )
    {
        res.prepend(scope->getName());
        scope = scope->d_scope;
    }
    return res;
}

bool Named::isPublic() const
{
    if( d_visibility == ReadWrite || d_visibility == ReadOnly )
        return true;
    Module* m = getModule();
    return m && m->d_isDef;
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

QByteArrayList QualiType::getQualiString() const
{
    Q_ASSERT( !d_quali.isNull() );
    QByteArrayList res;

    switch( d_quali->getTag() )
    {
    case Thing::T_IdentLeaf:
        {
            IdentLeaf* i = cast<IdentLeaf*>( d_quali.data() );
            res.append(i->d_name);
        }
        break;
    case Thing::T_IdentSel:
        {
            IdentSel* i = cast<IdentSel*>( d_quali.data() );
            Q_ASSERT( !i->d_sub.isNull() && i->d_sub->getTag() == Thing::T_IdentLeaf );
            IdentLeaf* j = cast<IdentLeaf*>( i->d_sub.data() );
            res.append(j->d_name);
            res.append(i->d_name);
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

quint32 QualiType::getByteSize() const
{
    if( !d_quali->d_type.isNull() )
        return d_quali->d_type->getByteSize();
    else
        return 0;
}

quint32 QualiType::getAlignment() const
{
    if( !d_quali->d_type.isNull() )
        return d_quali->d_type->getAlignment();
    else
        return 0;
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

bool QualiType::isDotted() const
{
    if( d_quali && d_quali->getTag() == Thing::T_IdentSel )
        return true;
    else
        return false;
}

IdentLeaf::IdentLeaf(Named* id, const Ob::RowCol& loc, Module* mod, Type* t, IdentRole r):d_ident(id),d_role(r)
{
    d_loc = loc;
    d_type = t;
    d_mod = mod;
}

quint8 IdentLeaf::visibilityFor(Module*) const
{
    if( d_ident.isNull() )
        return Named::NotApplicable;
    switch( d_ident->getTag() )
    {
    case Thing::T_Import:
    case Thing::T_Const:
    case Thing::T_GenericName:
    case Thing::T_NamedType:
    case Thing::T_Procedure:
    case Thing::T_BuiltIn:
        return Named::ReadOnly;
    case Thing::T_LocalVar:
    case Thing::T_Variable:
        return Named::LocalAccess;
    case Thing::T_Parameter:
        {
            Parameter* p = cast<Parameter*>(d_ident.data());
            if( p->d_const )
                return Named::ReadOnly;
            else
                return Named::LocalAccess;
        }
        break;
    default:
        Q_ASSERT(false);
    }
    return Named::NotApplicable;
}

#ifdef _DEBUG

QSet<Thing*> Thing::insts;

Thing::Thing():d_slot(0),d_slotValid(false),d_slotAllocated(false),d_visited(false),d_unsafe(false)
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
            if( d_fields[i]->d_super->d_slotValid && d_fields[i]->d_super->d_slot < d_fields.size() )
                res[ d_fields[i]->d_super->d_slot ] = d_fields[i].data();
            else
            {
                QByteArray name;
                Named* id = findDecl(true);
                if( id )
                {
                    name = id->d_name;
                    Module* m = id->getModule();
                    if( m )
                        name += m->getName() + "." + id->d_name + " " + QByteArray::number(id->d_loc.d_row);
                }
                qWarning() << "Record::getOrderedFields: invalid super field slot:" << d_fields[i]->d_name
                           << name.constData();
            }
        }else
            res.append( d_fields[i].data() );
    }
    return res;
}

QList<Procedure*> Record::getOrderedMethods() const
{
    QList<Procedure*> res;
    if( d_baseRec )
        res = d_baseRec->getOrderedMethods();
    foreach( const Ref<Procedure>& p, d_methods )
    {
        const int pos = res.indexOf(p->d_super);
        if( pos != -1 )
            res[pos] = p.data();
        else
            res.append(p.data());
    }
    return res;
}

Record*Record::findBySlot(int slot) const
{
    if( d_slot == slot )
        return const_cast<Record*>(this);
    for( int i = 0; i < d_subRecs.size(); i++ )
    {
        Record* r = d_subRecs[i]->findBySlot(slot);
        if( r )
            return r;
    }
    return 0;
}

quint32 Record::getByteSize() const
{
    if( d_alignment > 0 )
        return d_byteSize;

    Record* r = const_cast<Record*>(this);
    // http://www.catb.org/esr/structure-packing/#_structure_alignment_and_padding
    if( d_union )
    {
        r->d_byteSize = 0;
        r->d_alignment = 1;
        for( int i = 0; i < d_fields.size(); i++ )
        {
            const int size = d_fields[i]->d_type->getByteSize();
            const int alig = d_fields[i]->d_type->getAlignment();
            if( size > r->d_byteSize )
                r->d_byteSize = size;
            if( alig > r->d_alignment )
                r->d_alignment = alig;
            d_fields[i]->d_slot = 0;
            d_fields[i]->d_slotValid = true;
        }
    }else
    {
        int off = 0;
        int maxAlig = 1;
        for( int i = 0; i < d_fields.size(); i++ )
        {
            Type* t = d_fields[i]->d_type.data();
            const int size = t->getByteSize();
            const int alig = t->getAlignment();
            if( alig > maxAlig )
                maxAlig = alig;
            if( i != 0 )
            {
                // https://en.wikipedia.org/wiki/Data_structure_alignment#Computing_padding
                const int padding = (alig - (off % alig)) % alig;
                off += padding;
            }
            d_fields[i]->d_slot = off;
            d_fields[i]->d_slotValid = true;
            // qDebug() << i << "off" << off << "size" << size;
            off += size;
        }
        r->d_alignment = maxAlig;
        r->d_byteSize = off + (maxAlig - (off % maxAlig)) % maxAlig;
        // qDebug() << "struct size" << r->d_byteSize << "alig" << r->d_alignment;
    }
    return d_byteSize;
}

Field*Record::nextField(Field* f) const
{
    for( int i = 0; i < d_fields.size(); i++ )
    {
        if( d_fields[i].data() == f )
        {
            if( i+1 < d_fields.size() )
                return d_fields[i+1].data();
            break;
        }
    }
    return 0;
}

static bool includesAnyOf( const QSet<Record*>& part, const QSet<Record*>& all )
{
    foreach( Record* r, part )
    {
        if( all.contains(r) )
            return true;
    }
    return false;
}

static QSet<Record*> valueRecordInType(Type* t, const QSet<Record*>& all )
{
    // we only care about value fields
    Type* td =  t == 0 ? 0 : t->derefed();
    QSet<Record*> res;
    if( td == 0 )
        return res;
    switch( td->getTag() )
    {
    case Thing::T_Record:
        {
            Record* rr = cast<Record*>(td);
            if( all.contains(rr) ) // we only care about records in the all set
                res.insert(rr);
        }
        break;
    case Thing::T_Array:
        {
            Array* a = cast<Array*>(td);
            QList<Array*> dims = a->getDims();
            Q_ASSERT(!dims.isEmpty());
            td = dims.last()->d_type.isNull() ? 0 : dims.last()->d_type->derefed();
            if( td && td->getTag() == Thing::T_Record )
            {
                Record* rr = cast<Record*>(td);
                if( all.contains(rr) )
                    res.insert(rr);
            }
        }
        break;
    case Thing::T_ProcType:
        {
            ProcType* pt = cast<ProcType*>(td);
            foreach( const Ref<Parameter>& p, pt->d_formals )
            {
                if( !p->d_var )
                    res += valueRecordInType(p->d_type.data(),all);
            }
            if( pt->d_return )
                res += valueRecordInType(pt->d_return.data(),all);
        }
        break;
    }
    return res;
}

static QSet<Record*> valueRecordFieldTypes(Record* r, const QSet<Record*>& all)
{
    QList<Field*> ff = r->getOrderedFields();
    QSet<Record*> res;
    foreach( Field* f, ff )
    {
        res += valueRecordInType(f->d_type.data(),all);
    }
    return res;
}

QSet<Record*> Record::calcDependencyOrder(QList<Record*>& inout)
{
    QSet<Record*> recs, all;
    QList<Record*> ordered;
    recs = inout.toSet();
    all = recs;
    QHash<Record*,QSet<Record*> > valueFields;
    QSet<Record*> used;
    foreach( Record* r, recs )
    {
        // Find all leafs
        QSet<Record*> v = valueRecordFieldTypes(r, all);
        valueFields[r] = v;
        if( !includesAnyOf(v,all) )
        {
            ordered.append(r);
            used.insert(r);
        }
    }
    recs -= used;

    while( !recs.isEmpty() )
    {
        foreach( Record* r, recs )
        {
            if( used.contains( valueFields[r] ) )
                // as soon as all dependencies of r are fulfilled, add r to the order
            {
                used.insert(r);
                ordered.append(r);
            }
        }
        const int count = recs.size();
        recs -= used;
        if( count == recs.size() )
            break; // recs has no longer modified, exit loop
    }

    if( recs.isEmpty() )
    {
        inout = ordered;
        return recs;
    }else
        return recs; // there are circular dependencies (can this happen at all?)
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

quint32 Array::getByteSize() const
{
    if( d_lenExpr.isNull() )
        return 0;
    else if( d_type )
        return d_len * d_type->getByteSize();
    else
        return 0;
}

quint32 Array::getAlignment() const
{
    if( d_type )
        return d_type->getAlignment();
    else
        return 1;
}

Type*Array::getTypeDim(int& dims) const
{
    Type* t = d_type.isNull() ? 0 : d_type->derefed();
    if( t && t->getTag() == Thing::T_Array )
    {
        t = cast<Array*>( t )->getTypeDim(dims);
        dims += 1;
        return t;
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
    if( d_unsafe )
        return QString("CARRAY OF %1").arg(d_type->pretty());
    else
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
    if( d_sub.isNull() || d_ident.isNull() )
        return Named::NotApplicable;
    if( d_sub->getTag() == Thing::T_IdentLeaf )
    {
        IdentLeaf* leaf = cast<IdentLeaf*>(d_sub.data());
        if( !leaf->d_ident.isNull() && leaf->d_ident->getTag() == Thing::T_Import )
            return d_ident->d_visibility;
    }
    quint8 v = d_sub->visibilityFor(m);
    switch( v )
    {
    case Named::LocalAccess:
    case Named::NotApplicable:
    case Named::Private:
        break; // sub is stronger than this
    case Named::ReadOnly:
        switch( d_ident->d_visibility )
        {
        case Named::ReadOnly:
        case Named::NotApplicable:
        case Named::ReadWrite:
            break; // sub is stronger than this
        case Named::Private:
            v = d_ident->d_visibility; // this is stronger than sub
            break;
        default:
            Q_ASSERT( false );
        }
        break;
    case Named::ReadWrite:
        v = d_ident->d_visibility; // this is stronger
        break;
    }
    return v;
}

Const::Const(const QByteArray& name, Literal* lit)
{
    d_name = name;
    d_constExpr = lit;
    d_vtype = 0;
    d_wide = false;
    d_minInt = false;
    if( lit )
    {
        d_type = lit->d_type.data();
        d_val = lit->d_val;
        d_vtype = lit->d_vtype;
        d_wide = lit->d_wide;
        d_minInt = lit->d_minInt;
    }
}

quint8 Pointer::s_pointerByteSize = sizeof(void*);

QString Pointer::pretty() const
{
    if( d_to.isNull() )
        return "POINTER TO ?";
    if( d_unsafe )
        return QString("CPOINTER TO %1").arg(d_to->pretty());
    else
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
    return maxVal(d_baseType);
}

QVariant BaseType::minVal() const
{
    return minVal(d_baseType);
}

QVariant BaseType::maxVal(quint8 baseType)
{
    switch( baseType )
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

QVariant BaseType::minVal(quint8 baseType)
{
    switch( baseType )
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

quint32 BaseType::getByteSize() const
{
    switch( d_baseType )
    {
    case BOOLEAN:
    case CHAR:
    case BYTE:
        return 1;
    case WCHAR:
    case SHORTINT:
        return 2;
    case SET:
    case INTEGER:
    case REAL:
    case ENUMINT:
        return 4;
    case LONGINT:
    case LONGREAL:
        return 8;
    }
    return 0;
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

bool Type::isString(bool* wide) const
{
    if( wide )
        *wide = d_baseType == WSTRING;
    return d_baseType == STRING || d_baseType == WSTRING;
}

bool Type::isChar(bool* wide) const
{
    if(wide)
        *wide = d_baseType == WCHAR;
    return d_baseType == CHAR || d_baseType == WCHAR;
}

bool Type::isText(bool* wide, bool resolvePtr) const
{
    Type* t = const_cast<Type*>(this);
    int tag = t->getTag();
    if( resolvePtr && tag == Thing::T_Pointer )
    {
        Pointer* p = cast<Pointer*>(t);
        t = p->d_to.data();
        if( t )
            t = t->derefed();
        tag = t ? t->getTag() : 0;
    }
    if( t->isString() || t->isChar() )
    {
        // we also accept pointer to string literal or pointer to char as text
        // this can happen with automatic ADDROF
        if( wide )
            *wide = t->d_baseType == WCHAR || t->d_baseType == WSTRING;
        return true;
    }
    if( tag == Thing::T_Array )
    {
        Array* a = cast<Array*>(t);
        t = a->d_type.data();
        if( t )
            t = t->derefed();
        if( t && t->isChar() )
        {
            if( wide )
                *wide = t->d_baseType == WCHAR;
            return true;
        }
    }
    if( wide )
        *wide = false;
    return false;
}

bool Type::isByteArray(bool resolvePtr) const
{
    Type* t = const_cast<Type*>(this);
    int tag = t->getTag();
    if( resolvePtr && tag == Thing::T_Pointer )
    {
        Pointer* p = cast<Pointer*>(t);
        t = p->d_to.data();
        if( t )
            t = t->derefed();
        tag = t ? t->getTag() : 0;
    }
    if( t->getBaseType() == Type::BYTEARRAY )
    {
        // we also accept pointer to bytearray literal
        // this can happen with automatic ADDROF
        return true;
    }
    if( tag == Thing::T_Array )
    {
        Array* a = cast<Array*>(t);
        t = a->d_type.data();
        if( t )
            t = t->derefed();
        if( t && t->getBaseType() == Type::BYTE )
            return true;
    }
    return false;
}

Record*Type::toRecord(bool* isPtr) const
{
    if( isPtr )
        *isPtr = false;
    Type* t = const_cast<Type*>(this)->derefed();
    if( t && t->getTag() == Thing::T_Pointer )
    {
        if( isPtr )
            *isPtr = true;
        t = cast<Pointer*>(t)->d_to.data();
    }
    if( t )
        t = t->derefed();
    if( t && t->getTag() == Thing::T_Record )
        return cast<Record*>(t);
    else
        return 0;
}

Module*Type::declaredIn()
{
    Named* n = findDecl(true);
    if(n)
        return n->getModule();
    else
        return 0; // should not happen
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
    return getFullName() + formatMetaActuals();
#endif
}

QByteArray Module::getFullName() const
{
    if( d_fullName.isEmpty() )
        return d_name;
    return d_fullName.join('.');
}

QByteArray Module::formatMetaActuals() const
{
    QByteArray name;
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
                name += n->getQualifiedName().join('.'); // n->getName();
            else if( d_metaActuals[i]->getTag() == Thing::T_QualiType )
            {
                QualiType* q = cast<QualiType*>( d_metaActuals[i].data() );
                name += q->getQualiString().join('.');
            }else
                name += "?";
        }
        name += ")";
    }
    return name;
}

bool Module::isFullyInstantiated() const
{
    if( d_metaParams.isEmpty() )
    {
        Q_ASSERT( d_metaActuals.isEmpty() );
        return true;
    }
    if( d_metaActuals.isEmpty() )
        return false;

    for( int i = 0; i < d_metaActuals.size(); i++ )
    {
        Type* td = d_metaActuals[i].data();
        if( td )
            td = td->derefed();
        if( td == 0 || ( td->getTag() == Thing::T_BaseType && td->d_baseType == Type::ANY ) )
            return false;
    }
    return true;
}

Import*Module::findImport(Module* m) const
{
    foreach( Import* i, d_imports )
    {
        if( i->d_mod == m )
            return i;
    }
    return 0;
}

void Module::findAllInstances(QList<Module*>& result) const
{
    foreach( Import* imp, d_imports )
    {
        Q_ASSERT( imp->d_mod.data() );
        if( !imp->d_mod->d_metaParams.isEmpty() )
        {
            imp->d_mod->findAllInstances(result);
            result.append(imp->d_mod.data());
        }
    }
}

quint8 UnExpr::visibilityFor(Module* m) const
{
    if( !d_sub.isNull() )
        return d_sub->visibilityFor(m);
    else
        return Named::NotApplicable;
}


bool BinExpr::isRelation() const
{
    return ( d_op >= EQ && d_op <= GEQ );
}

bool BinExpr::isArithOp() const
{
    return ( d_op >= ADD && d_op <= SUB ) || ( d_op >= MUL && d_op <= MOD );
}


quint32 Enumeration::getByteSize() const
{
    return BaseType(Type::ENUMINT).getByteSize();
}
