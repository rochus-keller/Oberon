/*
* Copyright 2019 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon Viewer application.
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

#include "NamedThingsMdl.h"
#include <QBrush>
#include <QPixmap>
#include <QtDebug>
#include <QTreeView>
using namespace Ob;

NamedThingsMdl::NamedThingsMdl(QTreeView* parent) :
    QAbstractItemModel(parent)
{

}

QTreeView*NamedThingsMdl::getParent() const
{
    return static_cast<QTreeView*>(QObject::parent());
}

void NamedThingsMdl::setSyntax( CodeModel* syn )
{
    beginResetModel();
    d_root = Slot();
    d_syn = syn;
    fillTop();
    endResetModel();
}

const CodeModel::NamedThing* NamedThingsMdl::getSymbol(const QModelIndex& index) const
{
    if( !index.isValid() || d_syn == 0 )
        return 0;
    Slot* s = static_cast<Slot*>( index.internalPointer() );
    Q_ASSERT( s != 0 );
    return s->d_sym;
}

QModelIndex NamedThingsMdl::findSymbol(const CodeModel::NamedThing* nt)
{
    return findSymbol( &d_root, nt );
}

QVariant NamedThingsMdl::data(const QModelIndex& index, int role) const
{
    if( !index.isValid() || d_syn == 0 )
        return QVariant();

    Slot* s = static_cast<Slot*>( index.internalPointer() );
    Q_ASSERT( s != 0 );
    switch( role )
    {
    case Qt::DisplayRole:
        switch( s->d_kind )
        {
        case Slot::Symbol:
        default:
            if( const CodeModel::Element* v = dynamic_cast<const CodeModel::Element*>(s->d_sym) )
            {
                if( v->isStub() )
                    return s->d_sym->d_name + " (" + CodeModel::Element::s_kindName[v->d_kind] + ")";
            }else if( const CodeModel::Procedure* v = dynamic_cast<const CodeModel::Procedure*>(s->d_sym) )
            {
                return s->d_sym->d_name + ( s->d_sym->d_public ? "*" : "" ) + "(" +
                        ( v->d_vals.isEmpty() ? "" : "," ) + ")";
            }
            // else
            return s->d_sym->d_name + ( s->d_sym->d_public ? "*" : "" );
        case Slot::Consts:
            return "CONST";
        case Slot::Vars:
            return "VAR";
        case Slot::Procs:
            return "PROC";
        case Slot::Types:
            return "TYPE";
        case Slot::Params:
            return "PARAM";
        case Slot::Stubs:
            return "STUB";
        }
        break;
    case Qt::FontRole:
        if( s->d_kind != Slot::Symbol )
        {
            QFont f = getParent()->font();
            f.setItalic(true);
            return f;
        }
        break;
    case Qt::ForegroundRole:
        if( s->d_sym == 0 || s->d_sym->d_id != 0 )
            return QBrush( Qt::black );
        else
            return QBrush( Qt::gray );
        break;
    }
    return QVariant();
}

QModelIndex NamedThingsMdl::parent ( const QModelIndex & index ) const
{
    if( index.isValid() )
    {
        Slot* s = static_cast<Slot*>( index.internalPointer() );
        Q_ASSERT( s != 0 );
        if( s->d_parent == &d_root )
            return QModelIndex();
        // else
        Q_ASSERT( s->d_parent != 0 );
        Q_ASSERT( s->d_parent->d_parent != 0 );
        return createIndex( s->d_parent->d_parent->d_children.indexOf( s->d_parent ), 0, s->d_parent );
    }else
        return QModelIndex();
}

int NamedThingsMdl::rowCount ( const QModelIndex & parent ) const
{
    if( parent.isValid() )
    {
        Slot* s = static_cast<Slot*>( parent.internalPointer() );
        Q_ASSERT( s != 0 );
        return s->d_children.size();
    }else
        return d_root.d_children.size();
}

QModelIndex NamedThingsMdl::index ( int row, int column, const QModelIndex & parent ) const
{
    const Slot* s = &d_root;
    if( parent.isValid() )
    {
        s = static_cast<Slot*>( parent.internalPointer() );
        Q_ASSERT( s != 0 );
    }
    if( row < s->d_children.size() && column < columnCount( parent ) )
        return createIndex( row, column, s->d_children[row] );
    else
        return QModelIndex();
}

Qt::ItemFlags NamedThingsMdl::flags( const QModelIndex & index ) const
{
    Q_UNUSED(index)
    return Qt::ItemIsEnabled | Qt::ItemIsSelectable; //  | Qt::ItemIsDragEnabled;
}

void NamedThingsMdl::fillTop()
{
    if( d_syn == 0 )
        return;

    typedef QMap<QByteArray,const CodeModel::Module*> Sorter;
    Sorter sorter;

    foreach( const CodeModel::Module* m, d_syn->getGlobalScope().d_mods )
        sorter.insert( m->d_name, m );

    for( Sorter::const_iterator j = sorter.begin(); j != sorter.end(); ++j )
    {
        Slot* super = new Slot();
        super->d_parent = &d_root;
        super->d_sym = j.value();
        d_root.d_children.append( super );

        fill( super, j.value() );
    }
}

QModelIndex NamedThingsMdl::findSymbol(NamedThingsMdl::Slot* slot, const CodeModel::NamedThing* nt) const
{
    for( int i = 0; i < slot->d_children.size(); i++ )
    {
        Slot* s = slot->d_children[i];
        if( s->d_sym == nt )
            return createIndex( i, 0, s );
        QModelIndex index = findSymbol( s, nt );
        if( index.isValid() )
            return index;
    }
    return QModelIndex();

}

void NamedThingsMdl::fillSection( Slot* super, const Sorter& sorter, quint8 kind )
{
    if( sorter.isEmpty() )
        return;

    if( kind != Slot::Symbol)
        super = new Slot(super,kind);

    for( Sorter::const_iterator j = sorter.begin(); j != sorter.end(); ++j )
    {
        Slot* s = new Slot();
        s->d_parent = super;
        s->d_sym = j.value();
        super->d_children.append( s );
        if( const CodeModel::Procedure* p = dynamic_cast<const CodeModel::Procedure*>(j.value()) )
            fill(s, p );
        else if( const CodeModel::Type* r = dynamic_cast<const CodeModel::Type*>(j.value()) )
        {
            if( r->d_kind == CodeModel::Type::Pointer )
                r = r->d_type;
            if( r == 0 || r->d_kind != CodeModel::Type::Record )
                continue;
            Sorter s2;
            CodeModel::Type::Vals::const_iterator i;
            for( i = r->d_vals.begin(); i != r->d_vals.end(); ++i )
                s2.insert( i.value()->d_name.toLower(), i.value() );
#ifdef OB_OBN2
            CodeModel::Type::Procs::const_iterator j;
            for( j = r->d_procs.begin(); j != r->d_procs.end(); ++j )
                s2.insert( j.value()->d_name.toLower(), j.value() );
#endif
            fillSection( s, s2, Slot::Symbol );
        }else if( const CodeModel::Element* t = dynamic_cast<const CodeModel::Element*>(j.value() ) )
        {
            if( t->isStub() )
            {
                Sorter s2;
                foreach( const CodeModel::Element* v, t->d_vals )
                    s2.insert( v->d_name.toLower(), v );
                fillSection( s, s2, Slot::Symbol );
            }
        }
    }
}

void NamedThingsMdl::fill(Slot* super, const CodeModel::Unit* ds )
{
    if( ds == 0 )
        return;

    Sorter sorter;

    if( const CodeModel::Procedure* p = dynamic_cast<const CodeModel::Procedure*>(ds) )
    {
        foreach( const CodeModel::Element* x, p->d_vals )
            if( !x->d_name.isEmpty() )
                sorter.insert( x->d_name.toLower(), x );
        fillSection( super, sorter, Slot::Params );
    }

    sorter.clear();
    foreach( const CodeModel::Element* x, ds->getConsts() )
        if( !x->d_name.isEmpty() )
            sorter.insert( x->d_name.toLower(), x );
    fillSection( super, sorter, Slot::Consts );

    sorter.clear();
    foreach( const CodeModel::Type* x, ds->d_types )
        if( !x->d_name.isEmpty() )
            sorter.insert( x->d_name.toLower(), x );
    fillSection( super, sorter, Slot::Types );

    sorter.clear();
    foreach( const CodeModel::Element* x, ds->getVars() )
        if( !x->d_name.isEmpty() )
            sorter.insert( x->d_name.toLower(), x );
    fillSection( super, sorter, Slot::Vars );

    sorter.clear();
    foreach( const CodeModel::Element* x, ds->getUnknowns() )
        if( !x->d_name.isEmpty() )
            sorter.insert( x->d_name.toLower(), x );
    foreach( const CodeModel::Element* x, ds->getStubProcs() )
        if( !x->d_name.isEmpty() )
            sorter.insert( x->d_name.toLower(), x );
    fillSection( super, sorter, Slot::Stubs );

    sorter.clear();
    foreach( const CodeModel::Procedure* x, ds->d_procs )
        if( !x->d_name.isEmpty() && x->d_receiver == 0 )
            sorter.insert( x->d_name.toLower(), x );
    fillSection( super, sorter, Slot::Procs );
}

