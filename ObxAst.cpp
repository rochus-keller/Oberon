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
    "ANY", "ANYNUM", "NIL", "STRING", "BOOLEAN", "CHAR", "BYTE",
    "SHORTINT", "INTEGER", "LONGINT", "REAL", "LONGREAL", "SET"
};

const char* BuiltIn::s_typeName[] =
{
    "ABS", "ODD", "LEN", "LSL", "ASR", "ROR", "FLOOR", "FLT", "ORD",
    "CHR", "INC", "DEC", "INCL", "EXCL", "NEW", "ASSERT", "PACK", "UNPK",
    "WriteInt", "WriteReal", "WriteChar", "WriteLn",
    "LED", "TRAP", "TRAPIF",
    "ADR", "BIT", "GET", "H", "LDREG", "PUT", "REG", "VAL", "COPY",
    "MAX", "CAP", "LONG", "SHORT", "HALT", "COPY", "ASH", "MIN", "SIZE", "ENTIER",
    "BITS",
    // Oberon-2 SYSTEM
    "MOVE", "NEW", "ROT", "LSH", "GETREG", "PUTREG",
    // Blackbox
    "TYP",
    // Oberon+
    "VAL", "STRLEN"
};

const char* UnExpr::s_opName[] =
{
    "???",
    "NEG", "NOT", "DEREF", "CAST", "SEL", "CALL", "IDX"
};

const char* BinExpr::s_opName[] =
{
    "???",
    "Range",
    "EQ", "NEQ", "LE", "LEQ", "GT", "GEQ", "IN", "IS",
    "ADD", "SUB", "OR",
    "MUL", "FDIV", "DIV", "MOD", "AND"
};

static void markUsed( Type* t )
{
    if( t == 0 )
        return;

    if( t->d_ident )
    {
        if( t->d_ident->d_usedFromLive )
            return; // already visited
        t->d_ident->d_usedFromLive = true;
    }

    const int tag = t->getTag();
    switch( tag )
    {
    case Thing::T_Pointer:
        markUsed( cast<Pointer*>(t)->d_to.data() );
        break;
    case Thing::T_Array:
        markUsed( cast<Array*>(t)->d_type.data() );
        break;
    case Thing::T_Record:
        {
            Record* r = cast<Record*>(t);
            if( !r->d_base.isNull() )
                markUsed( r->d_base.data() );
            if( r->d_binding && r->d_binding->d_ident )
                r->d_binding->d_ident->d_usedFromLive = true;
        }
        break;
    case Thing::T_QualiType:
        markUsed( cast<QualiType*>(t)->d_quali->d_type.data() );
        break;
    }

    // if d_ident is in another module then it must be public, otherwise it could not be
    // used as type here; with d_usedFromLive we cover also local use for records
    // not public and with no direct live range
}

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
    return true;
}

BuiltIn::BuiltIn(quint8 f, ProcType* pt):d_func(f)
{
    d_name = Lexer::getSymbol(s_typeName[f]);
    if( pt )
        d_type = pt;
    else
        d_type = new ProcType();
    Q_ASSERT( d_type->d_ident == 0 );
    d_type->d_ident = this;
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
    return d_ident && d_ident->getTag() == Thing::T_BuiltIn;
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


IdentLeaf::IdentLeaf(Named* id, const Ob::RowCol& loc, Module* mod, Type* t):d_ident(id)
{
    d_loc = loc;
    d_type = t;
    d_mod = mod;
}

#ifdef _DEBUG

QSet<Thing*> Thing::insts;

Thing::Thing()
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

Type*Array::getTypeDim(int& dims) const
{
    Type* t = d_type.isNull() ? 0 : d_type->derefed();
    if( t && t->getTag() == Thing::T_Array )
    {
        Array* a = cast<Array*>( t );
        dims += 1;
        return a->getTypeDim(dims);
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
    if( lit )
    {
        d_type = lit->d_type.data();
        d_val = lit->d_val;
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
