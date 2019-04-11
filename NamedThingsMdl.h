#ifndef NamedThingsMdl_H
#define NamedThingsMdl_H

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

#include <QAbstractItemModel>
#include <Oberon/ObCodeModel.h>

class QTreeView;

namespace Ob
{
    class NamedThingsMdl : public QAbstractItemModel
    {
    public:
        explicit NamedThingsMdl(QTreeView *parent = 0);

        QTreeView* getParent() const;
        void setSyntax( CodeModel* );
        const CodeModel::NamedThing* getSymbol( const QModelIndex & ) const;
        QModelIndex findSymbol( const CodeModel::NamedThing* );

        // overrides
        int columnCount ( const QModelIndex & parent = QModelIndex() ) const { return 1; }
        QVariant data ( const QModelIndex & index, int role = Qt::DisplayRole ) const;
        QModelIndex index ( int row, int column, const QModelIndex & parent = QModelIndex() ) const;
        QModelIndex parent ( const QModelIndex & index ) const;
        int rowCount ( const QModelIndex & parent = QModelIndex() ) const;
        Qt::ItemFlags flags ( const QModelIndex & index ) const;

    private:
        struct Slot
        {
            enum Kind { Symbol, Params, Consts, Vars, Types, Procs, Stubs };
            const CodeModel::NamedThing* d_sym;
            quint8 d_kind; // Kind
            QList<Slot*> d_children;
            Slot* d_parent;
            Slot(Slot* p = 0, quint8 k = Symbol ):d_parent(p),d_kind(k),d_sym(0){ if( p ) p->d_children.append(this); }
            ~Slot() { foreach( Slot* s, d_children ) delete s; }
        };
        typedef QMap<QByteArray,const CodeModel::NamedThing*> Sorter;
        void fillSection( Slot* super, const Sorter& sorter, quint8 kind );
        void fill(Slot* super, const CodeModel::Unit* sym);
        void fillTop();
        QModelIndex findSymbol(Slot*, const CodeModel::NamedThing* nt) const;
        Slot d_root;
        CodeModel* d_syn;
    };
}


#endif // NamedThingsMdl_H
