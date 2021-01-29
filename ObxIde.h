#ifndef _OberonIde_H
#define _OberonIde_H

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

namespace Obx
{
    class Project;
    class Named;
    class Module;
    class Expression;

    class Ide : public QMainWindow
    {
        Q_OBJECT

    public:
        Ide(QWidget *parent = 0);
        ~Ide();

        void loadFile( const QString& path );
        void logMessage(const QString& , bool err = false);

    protected:
        class Editor;
        struct Location
        {
            // Qt-Koordinaten
            quint32 d_line;
            quint16 d_col;
            quint16 d_yoff;
            QString d_file;
            bool operator==( const Location& rhs ) { return d_line == rhs.d_line && d_col == rhs.d_col &&
                        d_file == rhs.d_file; }
            Location(const QString& f, quint32 l, quint16 c, quint16 y ):d_file(f),d_line(l),d_col(c),d_yoff(y){}
        };
        void createTerminal();
        void createDumpView();
        void createMods();
        void createMod();
        void createHier();
        void createErrs();
        void createXref();
        void createStack();
        void createLocals();
        void createMenu();
        void createMenuBar();
        void closeEvent(QCloseEvent* event);
        bool checkSaved( const QString& title );
        bool compile(bool generate = false);
        void fillMods();
        void showDocument( const QString& filePath );
        void addTopCommands(Gui::AutoMenu * pop);
        Editor* showEditor(const QString& path, int row = -1, int col = -1, bool setMarker = false , bool center = false);
        void showEditor(Named*, bool setMarker = false, bool center = false);
        void showEditor( const Location& );
        void createMenu( Editor* );
        void addDebugMenu(Gui::AutoMenu * pop);
        bool luaRuntimeMessage(const QByteArray&, const QString& file);
        void fillXref();
        void fillXref(Named*);
        void fillStack();
        void fillLocals();
        void fillModule(Module*);
        void fillHier(Named*);
        void removePosMarkers();
        void enableDbgMenu();
        void pushLocation( const Location& );

    protected slots:
        void onCompile();
        void onRun();
        void onAbort();
        void onGenerate();
        void onNewPro();
        void onOpenPro();
        void onSavePro();
        void onSaveFile();
        void onSaveAs();
        void onCaption();
        void onGotoLnr(quint32);
        void onFullScreen();
        void onCursor();
        void onExportBc();
        void onExportAsm();
        void onModsDblClicked(QTreeWidgetItem*,int);
        void onModDblClicked(QTreeWidgetItem*,int);
        void onHierDblClicked(QTreeWidgetItem*,int);
        void onStackDblClicked(QTreeWidgetItem*,int);
        void onTabChanged();
        void onTabClosing(int);
        void onEditorChanged();
        void onErrorsDblClicked();
        void onErrors();
        void onOpenFile();
        void onOakwood();
        void onObSysInner();
        void onAddFiles();
        void onRemoveFile();
        void onEnableDebug();
        void onBreak();
        void handleGoBack();
        void handleGoForward();
        void onUpdateLocation(int line, int col );
        void onXrefDblClicked();
        void onToggleBreakPt();
        void onSingleStep();
        void onContinue();
        void onShowLlBc();
        void onWorkingDir();
        void onLuaNotify( int messageType, QByteArray val1, int val2 );
        void onAbout();
        void onQt();
    private:
        class DocTab;
        class Debugger;
        Project* d_pro;
        DocTab* d_tab;
        Debugger* d_dbg;
        Lua::Engine2* d_lua;
        Lua::BcViewer2* d_bcv;
        Lua::Terminal2* d_term;
        QTreeWidget* d_mods;
        QTreeWidget* d_mod;
        QTreeWidget* d_hier;
        QHash<Named*,QTreeWidgetItem*> d_modIdx;
        QTreeWidget* d_stack;
        QTreeWidget* d_locals;
        QLabel* d_xrefTitle;
        QLabel* d_modTitle;
        QLabel* d_hierTitle;
        QTreeWidget* d_xref;
        QTreeWidget* d_errs;
        QList<Location> d_backHisto; // d_backHisto.last() ist aktuell angezeigtes Objekt
        QList<Location> d_forwardHisto;
        QAction* d_dbgBreak;
        QAction* d_dbgAbort;
        QAction* d_dbgContinue;
        QAction* d_dbgStepIn;
        QByteArray d_curBc;
        bool d_lock, d_lock2, d_lock3, d_lock4;
        bool d_filesDirty;
        bool d_pushBackLock;
    };
}

#endif // _OberonIde_H
