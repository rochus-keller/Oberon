#ifndef OBERONVIEWER_H
#define OBERONVIEWER_H

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

#include <QMainWindow>
#include <Oberon/ObCodeModel.h>

class QPlainTextEdit;
class QTreeWidget;
class QTreeView;
class QLabel;
class QDir;
class QModelIndex;

namespace Ob
{
    class NamedThingsMdl;

    class OberonViewer : public QMainWindow
    {
        Q_OBJECT
    public:
        explicit OberonViewer(QWidget *parent = 0);

        void showFiles( const QStringList& );
        void logMessage( const QString& );
        static QStringList collectFiles( const QDir& dir );

    protected:
        void createModuleList();
        void createUsedBy();
        void createLog();
        void pushLocation( const SynTree* );
        void fillUsedBy( const SynTree*,const CodeModel::NamedThing* );

        // overrides
        void closeEvent(QCloseEvent* event);

    protected slots:
        void onModuleDblClick(const QModelIndex&);
        void onCursorPositionChanged();
        void onUsedByDblClicked();
        void onGoBack();
        void onGoForward();
        void onGotoLine();
        void onFindInFile();
        void onFindAgain();
        void onGotoDefinition();
        void onOpen();
        void onTranslate();

    private:
        class Viewer;
        Viewer* d_view;
        QLabel* d_loc;
        QPlainTextEdit* d_msgLog;
        QTreeView* d_things;
        NamedThingsMdl* d_ntm;
        QTreeWidget* d_usedBy;
        QLabel* d_usedByTitle;
        CodeModel* d_mdl;
        const SynTree* d_cur;
        QList<const SynTree*> d_backHisto;
        QList<const SynTree*> d_forwardHisto;
        bool d_pushBackLock;
    };
}

#endif // OBERONVIEWER_H
