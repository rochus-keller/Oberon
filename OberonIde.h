#ifndef _OberonIde_H
#define _OberonIde_H

/*
* Copyright 2020 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon parser/compiler library.
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

class QTreeWidget;
class QTreeWidgetItem;
class QLabel;

namespace Lua
{
class Engine2;
class BcViewer2;
class Terminal2;
class JitEngine;
}
namespace Gui
{
class AutoMenu;
}

namespace Ob
{
    class Highlighter;
    class Project;
    namespace Ast
    {
        class Named;
    }

    class OberonIde : public QMainWindow
    {
        Q_OBJECT

    public:
        OberonIde(QWidget *parent = 0);
        ~OberonIde();

        void loadFile( const QString& path );
        void logMessage(const QString& , bool err = false);

    protected:
        class Editor;
        void createTerminal();
        void createDumpView();
        void createMods();
        void createErrs();
        void createXref();
        void createMenu();
        void closeEvent(QCloseEvent* event);
        bool checkSaved( const QString& title );
        void compile(bool generate = false);
        void fillMods();
        void showDocument( const QString& filePath );
        void addTopCommands(Gui::AutoMenu * pop);
        void showEditor( const QString& path, int row = -1, int col = -1 );
        void showEditor( Ast::Named* );
        void createMenu( Editor* );
        void luaRuntimeMessage(const QByteArray&, const QString& file);
        void fillXref();
        struct Location
        {
            // Qt-Koordinaten
            int d_line;
            int d_col;
            QString d_file;
            bool operator==( const Location& rhs ) { return d_line == rhs.d_line && d_col == rhs.d_col &&
                        d_file == rhs.d_file; }
            Location(const QString& f, int l, int c ):d_file(f),d_line(l),d_col(c){}
        };
        void pushLocation( const Location& );

    protected slots:
        void onCompile();
        void onRun();
        void onNewPro();
        void onOpenPro();
        void onSavePro();
        void onSaveFile();
        void onSaveAs();
        void onCaption();
        void onGotoLnr(int);
        void onFullScreen();
        void onCursor();
        void onExportBc();
        void onExportAsm();
        void onModsDblClicked(QTreeWidgetItem*,int);
        void onTabChanged();
        void onTabClosing(int);
        void onEditorChanged();
        void onErrorsDblClicked();
        void onErrors();
        void onOpenFile();
        void onOakwood();
        void onAddFiles();
        void onRemoveFile();
        void handleGoBack();
        void handleGoForward();
        void onUpdateLocation(int line, int col );
        void onXrefDblClicked();

    private:
        class DocTab;
        Project* d_pro;
        DocTab* d_tab;
        Lua::Engine2* d_lua;
        Lua::BcViewer2* d_bcv;
        Lua::Terminal2* d_term;
        QTreeWidget* d_mods;
        QLabel* d_xrefTitle;
        QTreeWidget* d_xref;
        QTreeWidget* d_errs;
        Highlighter* d_hl;
        QList<Location> d_backHisto; // d_backHisto.last() ist aktuell angezeigtes Objekt
        QList<Location> d_forwardHisto;
        bool d_lock;
        bool d_filesDirty;
        bool d_pushBackLock;
    };
}

#endif // _OberonIde_H
