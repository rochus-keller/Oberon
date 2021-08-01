/*
* Copyright 2021 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon+ parser/compiler library.
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

#include "ObxCGen.h"
#include "ObxAst.h"
#include "ObxEvaluator.h"
#include "ObErrors.h"
#include <QTextStream>
#include <QtDebug>
using namespace Obx;
using namespace Ob;


static inline Type* derefed(Type* t )
{
    if( t )
        return t->derefed();
    else
        return t;
}

static inline Record* toRecord(Type* t)
{
    // no deref!
    if( t && t->getTag() == Thing::T_Pointer )
        t = cast<Pointer*>(t)->d_to.data();
    if( t && t->getTag() == Thing::T_Record )
        return cast<Record*>(t);
    else
        return 0;
}

bool CGen::generateLjFfiBinding(Module* m, QIODevice* d, Ob::Errors* err)
{
    if( !m->d_externC )
        return false;
    QByteArray lib, pfx;
    SysAttr* a = m->d_sysAttrs.value("lib").data();
    if( a && a->d_values.size() == 1 )
        lib = a->d_values.first().toByteArray();
    a = m->d_sysAttrs.value("pfx").data();
    if( a && a->d_values.size() == 1 )
        pfx = a->d_values.first().toByteArray();

    QTextStream hout(d);
    QByteArray str;
    QTextStream bout(&str);

    bout << "local module = {}" << endl;

    hout << "local ffi = require 'ffi'" << endl;
    hout << "ffi.cdef[[" << endl;

    foreach( const Ref<Named>& n, m->d_order )
    {
        switch( n->getTag() )
        {
        case Thing::T_NamedType:
            if( Record* r = toRecord(n->d_type.data()) ) // all others are aliasses or can be rendered inline
            {
                hout << "typedef ";
                QByteArray nameType = pfx + n->d_name;
                renderNameType( n->d_type.data(), nameType, pfx );
                hout << nameType << ";" << endl;
                bout << "module[" << r->d_slot << "] = ffi.typeof(\"" << (pfx+n->d_name) << "\")" << endl;
            }
            break;
        }
    }

    foreach( const Ref<Named>& n, m->d_order )
    {
        switch( n->getTag() )
        {
        case Thing::T_Procedure:
            {
                ProcType* pt = cast<Procedure*>(n.data())->getProcType();
                QByteArray function = pfx + n->d_name + renderFormals(pt,pfx);
                if( pt->d_return )
                    renderNameType( pt->d_return.data(), function, pfx );
                else
                    function = "void " + function;
                hout << function << ";" << endl;
                bout << "module[" << n->d_slot << "] = C." << (pfx+n->d_name) << endl;

            }
            break;
        }
    }

    hout << "]]" << endl;
    if( lib.isEmpty() )
        hout << "local C = ffi.C" << endl;
    else
        hout << "local C = ffi.load('" << lib << "')" << endl;

    bout << "return module" << endl;
    bout.flush();
    hout.flush();

    d->write(str);

    return true;
}

bool CGen::generateHeader(Module* m, QIODevice* out, Ob::Errors* err)
{
    return false; // pending
}

bool CGen::renderNameType(Type* t, QByteArray& name, const QByteArray& pfx, bool vla)
{
    if( t == 0 )
        return false;

    switch( t->getTag() )
    {
    case Thing::T_QualiType:
        {
            Type* td = derefed(t);
            const int tag = td->getTag();
            switch( tag )
            {
            case Thing::T_Array:
            case Thing::T_BaseType:
            case Thing::T_Enumeration:
            case Thing::T_Pointer:
            case Thing::T_ProcType:
                renderNameType(td,name, pfx);
                break;
            default:
                {
                    Q_ASSERT( tag == Thing::T_Record ); // because of derefed it cannot be QualiType
                    Record* r = cast<Record*>(td);
                    QualiType* q = cast<QualiType*>(t);
                    QualiType::ModItem names = q->getQuali();
                    // TODO: what about qualified names?
                    if( names.second )
                        name = ( r->d_union ? "union " : "struct " ) + pfx + names.second->d_name + " " + name;
                    else
                        name = "???";
                }
                break;
            }
        }
        break;
    case Thing::T_BaseType:
        name = renderBasicType(t) + " " + name;
        break;
    case Thing::T_Array:
        {
            Array* a = cast<Array*>(t);
            if( a->d_lenExpr.isNull() )
            {
                if( vla )
                    name += "[?]";
                else
                    name = "*" + name;
            }else
            {
                name += "[";
                name += QByteArray::number(a->d_len);
                name += "]";
            }
            renderNameType( a->d_type.data(), name, pfx );
        }
        break;
    case Thing::T_Pointer:
        {
            name = "*" + name;
            Pointer* p = cast<Pointer*>(t);
            Type* td = derefed(p->d_to.data());
            if( td && td->getTag() == Thing::T_Array )
                renderNameType( cast<Array*>(td)->d_type.data(), name, pfx ); // ignore the first array the pointer points to
            else
                renderNameType( p->d_to.data(), name, pfx );
        }
        break;
    case Thing::T_Enumeration:
        name = "int " + name;
        break;
    case Thing::T_Record:
        {
            QByteArray rec;
            Record* r = cast<Record*>(t);
            if( r->d_union )
                rec = "union ";
            else
                rec = "struct ";
            Named* n = r->findDecl();
            if( n )
                rec += pfx + n->d_name;
            if( !r->d_fields.isEmpty() )
                rec += " {";
            rec += "\n";
            for( int i = 0; i < r->d_fields.size(); i++ )
            {
                QByteArray nameType = r->d_fields[i]->d_name;
                renderNameType( r->d_fields[i]->d_type.data(), nameType, pfx );
                rec += "    " + nameType;
                rec += ";\n";
            }
            if( !r->d_fields.isEmpty() )
                rec += "} ";
            name = rec + name;
        }
        break;
    case Thing::T_ProcType:
        {
            ProcType* pt = cast<ProcType*>(t);
            QByteArray returnType;
            if( pt->d_return )
                renderNameType( pt->d_return.data(), returnType, pfx );
            else
                returnType = "void";
            name = returnType + "(*" + name + ")" + renderFormals(pt,pfx);
        }
        break;
    default:
        name = "???" + name;
        break;
    }

    return true;
}

QByteArray CGen::renderBasicType(Type* t)
{
    switch( t->getBaseType() )
    {
    case Type::BOOLEAN:
        return "_Bool";
    case Type::CHAR:
        return "char";
    case Type::WCHAR:
        return "uint16_t";
    case Type::BYTE:
        return "uint8_t";
    case Type::SHORTINT:
        return "int16_t";
    case Type::INTEGER:
        return "int32_t";
    case Type::LONGINT:
        return "int64_t";
    case Type::REAL:
        return "float";
    case Type::LONGREAL:
        return "double";
    case Type::SET:
        return "uint32_t";
    case Type::CVOID:
        return "void";
    default:
        return "???";
    }
}

QByteArray CGen::renderFormals(ProcType* pt, const QByteArray& pfx)
{
    QByteArray res = "(";
    if( pt->d_formals.isEmpty() )
        res += "void";
    for( int i = 0; i < pt->d_formals.size(); i++ )
    {
        if( i != 0 )
            res += ", ";
        QByteArray nameType = pt->d_formals[i]->d_name;
        renderNameType(pt->d_formals[i]->d_type.data(), nameType, pfx );
        res += nameType;
    }
    res += ")";
    return res;
}
