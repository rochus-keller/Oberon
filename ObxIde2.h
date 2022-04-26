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
#include <MonoTools/MonoDebugger.h>

class QTreeWidget;
class QTreeWidgetItem;
class QLabel;
class QTextBrowser;
class QProcess;

namespace Mono
{
class Engine;
class IlView;
}
namespace Gui
{
class AutoMenu;
}

namespace Obx
{
    class Project;
    struct Named;
    struct Module;
    struct Record;
    struct Expression;
    struct Scope;
    struct Type;
    using Mono::DebuggerEvent;

    class Ide : public QMainWindow
    {
        Q_OBJECT

    public:
        Ide(QWidget *parent = 0);
        ~Ide();

        void loadFile( const QString& path );
        enum LogLevel { LogInfo, SysInfo, LogWarning, LogError };
        void logMessage(const QString& , LogLevel = LogInfo , bool addNewLine = true);

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
        void createIlView();
        void createMods();
        void createMod();
        void createHier();
        void createErrs();
        void createXref();
        void createStack();
        void createLocals();
        void createModsMenu();
        void createMenuBar();
        void closeEvent(QCloseEvent* event);
        bool checkSaved( const QString& title );
        bool compile(bool doGenerate = false);
        bool generate();
        bool run();
        void fillMods();
        void showDocument( const QString& filePath );
        void addTopCommands(Gui::AutoMenu * pop);
        Editor* showEditor(const QString& path, int row = -1, int col = -1, bool setMarker = false , bool center = false);
        void showEditor(Named*, bool setMarker = false, bool center = false);
        void showEditor( const Location& );
        void createModsMenu( Editor* );
        void addDebugMenu(Gui::AutoMenu * pop);
        void fillXref();
        void fillXref(Named*);
        void fillStack();
        void fillLocals();
        void printLocalVal( QTreeWidgetItem* item, Type* type, int depth );
        void fillModule(Module*);
        void fillHier(Named*);
        void removePosMarkers();
        void pushLocation( const Location& );
        void clear();
        bool checkEngine(bool withFastasm = false);
        quint32 getMonoModule( Module* ); // returns typeId
        bool updateBreakpoint(Module*, quint32 line, bool add );

    protected slots:
        void onParse();
        void onRun();
        void onAbort();
        void onCompile();
        void onNewPro();
        void onOpenPro();
        void onSavePro();
        void onSaveFile();
        void onSaveAs();
        void onCaption();
        void onFullScreen();
        void onCursor();
        void onExportIl();
        void onExportC();
        void onModsDblClicked(QTreeWidgetItem*,int);
        void onModDblClicked(QTreeWidgetItem*,int);
        void onHierDblClicked(QTreeWidgetItem*,int);
        void onStackDblClicked(QTreeWidgetItem*,int);
        void onLocalExpanded(QTreeWidgetItem*);
        void onTabChanged();
        void onTabClosing(int);
        void onEditorChanged();
        void onErrorsDblClicked();
        void onErrors();
        void onOpenFile();
        void onOakwood();
        void onAddFiles();
        void onNewModule();
        void onAddDir();
        void onRemoveFile();
        void onRemoveDir();
        void onEnableDebug();
        void onBreak();
        void handleGoBack();
        void handleGoForward();
        void onUpdateLocation(int line, int col );
        void onXrefDblClicked();
        void onToggleBreakPt();
        void onStepIn();
        void onStepOver();
        void onStepOut();
        void onContinue();
        void onWorkingDir();
        void onBuildDir();
        void onAbout();
        void onQt();
        void onExpMod();
        void onExpDepTree();
        void onByteMode();
        void onSetRunCommand();
        void onConsole( const QString& msg, bool err );
        void onError( const QString& );
        void onFinished(int exitCode, bool normalExit);
        void onDbgEvent( const DebuggerEvent& );
        void onRemoveAllBreakpoints();
        void onBreakOnExceptions();
        void onRowColMode();
        void onSetInputFile();
        void onSetOptions();
    private:
        class DocTab;
        DocTab* d_tab;
        Mono::Debugger* d_dbg;
        Mono::Engine* d_eng;
        Project* d_pro;
        Mono::IlView* d_il;
        QLabel* d_ilTitle;
        QTextBrowser* d_term;
        QTreeWidget* d_mods;
        QTreeWidget* d_mod;
        QTreeWidget* d_hier;
        QHash<Named*,QTreeWidgetItem*> d_modIdx;
        QTreeWidget* d_stackView;
        QVector<Scope*> d_scopes;
        QTreeWidget* d_localsView;
        QLabel* d_xrefTitle;
        QLabel* d_modTitle;
        QLabel* d_hierTitle;
        QTreeWidget* d_xref;
        QTreeWidget* d_errs;
        QList<Location> d_backHisto; // d_backHisto.last() ist aktuell angezeigtes Objekt
        QList<Location> d_forwardHisto;
        enum Mode { LineMode, RowColMode, BytecodeMode };
        quint8 d_mode;
        bool d_lock, d_lock2, d_lock3, d_lock4;
        bool d_filesDirty;
        bool d_pushBackLock;
        bool d_debugging;
        bool d_suspended; // we are in debugger but code is suspended
        bool d_breakOnExceptions;
        quint32 d_curThread;
        enum Status { Idle, Compiling, Generating, Running };
        Status d_status;
        QList<Mono::Debugger::Frame> d_stack;
        qint32 d_curRow;
        qint16 d_curCol;
        quint8 d_curLevel;
        QHash<QByteArray, QSet<quint32> > d_breakPoints; // module name -> line number
        QHash<Module*,quint32> d_loadedAssemblies; // -> assemblyId
    };
}

#endif // _OberonIde_H
