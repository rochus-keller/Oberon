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

#include "OberonIde.h"
#include "ObnHighlighter.h"
#include "ObFileCache.h"
#include "ObLjLib.h"
#include "ObAst.h"
#include "ObAstEval.h"
#include "ObErrors.h"
#include "ObLjbcGen.h"
#include "ObLjProject.h"
#include "ObSysInnerLib.h"
#include <LjTools/Engine2.h>
#include <LjTools/Terminal2.h>
#include <LjTools/BcViewer2.h>
#include <LjTools/BcViewer.h>
#include <LjTools/LuaJitEngine.h>
#include <QtDebug>
#include <QDockWidget>
#include <QApplication>
#include <QStandardPaths>
#include <QDir>
#include <QDateTime>
#include <QSettings>
#include <QShortcut>
#include <QMessageBox>
#include <QFileDialog>
#include <QFileInfo>
#include <QBuffer>
#include <QHeaderView>
#include <QLabel>
#include <QVBoxLayout>
#include <QDesktopWidget>
#include <QInputDialog>
#include <GuiTools/AutoMenu.h>
#include <GuiTools/CodeEditor.h>
#include <GuiTools/AutoShortcut.h>
#include <GuiTools/DocTabWidget.h>
using namespace Ob;
using namespace Lua;

#ifdef Q_OS_MAC
#define OBN_BREAK_SC "SHIFT+F8"
#define OBN_ABORT_SC "CTRL+SHIFT+Y"
#define OBN_CONTINUE_SC "CTRL+Y"
#define OBN_STEPIN_SC "CTRL+SHIFT+I"
#define OBN_ENDBG_SC "F4"
#define OBN_TOGBP_SC "F8"
#define OBN_GOBACK_SC "ALT+CTRL+Left"
#define OBN_GOFWD_SC "ALT+CTRL+Right"
#define OBN_NEXTDOC_SC "ALT+TAB"
#define OBN_PREVDOC_SC "ALT+SHIFT+TAB"
#else
#define OBN_BREAK_SC "SHIFT+F9"
#define OBN_ABORT_SC "SHIFT+F5"
#define OBN_CONTINUE_SC "F5"
#define OBN_STEPIN_SC "F11"
#define OBN_ENDBG_SC "F8"
#define OBN_TOGBP_SC "F9"
#define OBN_GOBACK_SC "ALT+Left"
#define OBN_GOFWD_SC "ALT+Right"
#define OBN_NEXTDOC_SC "CTRL+TAB"
#define OBN_PREVDOC_SC "CTRL+SHIFT+TAB"
#endif

struct ScopeRef : public Ast::Ref<Ast::Scope>
{
    ScopeRef(Ast::Scope* s = 0):Ref(s) {}
};
Q_DECLARE_METATYPE(ScopeRef)
struct ExRef : public Ast::Ref<Ast::Expression>
{
    ExRef(Ast::Expression* n = 0):Ref(n) {}
};
Q_DECLARE_METATYPE(ExRef)

class OberonIde::Editor : public CodeEditor
{
public:
    Editor(OberonIde* p, Project* pro):CodeEditor(p),d_pro(pro),d_ide(p)
    {
        setCharPerTab(3);
        setTypingLatency(400);
        setPaintIndents(false);
        d_hl = new Highlighter( document() );
        updateTabWidth();
    }

    ~Editor()
    {
    }

    OberonIde* d_ide;
    Highlighter* d_hl;
    Project* d_pro;

    void clearBackHisto()
    {
        d_backHisto.clear();
    }


    typedef QList<Ast::Expression*> ExList;

    void markNonTerms(const ExList& syms)
    {
        d_nonTerms.clear();
        QTextCharFormat format;
        format.setBackground( QColor(237,235,243) );
        foreach( Ast::Expression* s, syms )
        {
            Ast::Named* ident = s->getIdent();
            Q_ASSERT( ident );
            QTextCursor c( document()->findBlockByNumber( s->d_loc.d_row - 1) );
            c.setPosition( c.position() + s->d_loc.d_col - 1 );
            int pos = c.position();
            c.setPosition( pos + ident->d_name.size(), QTextCursor::KeepAnchor );

            QTextEdit::ExtraSelection sel;
            sel.format = format;
            sel.cursor = c;

            d_nonTerms << sel;
        }
        updateExtraSelections();
    }

    void updateExtraSelections()
    {
        ESL sum;

        QTextEdit::ExtraSelection line;
        line.format.setBackground(QColor(Qt::yellow).lighter(170));
        line.format.setProperty(QTextFormat::FullWidthSelection, true);
        line.cursor = textCursor();
        line.cursor.clearSelection();
        sum << line;

        sum << d_nonTerms;

        if( !d_pro->getErrs()->getErrors().isEmpty() )
        {
            QTextCharFormat errorFormat;
            errorFormat.setUnderlineStyle(QTextCharFormat::WaveUnderline);
            errorFormat.setUnderlineColor(Qt::magenta);
            Errors::EntryList::const_iterator i;
            for( i = d_pro->getErrs()->getErrors().begin(); i != d_pro->getErrs()->getErrors().end(); ++i )
            {
                if( (*i).d_file != getPath() )
                    continue;
                QTextCursor c( document()->findBlockByNumber((*i).d_line - 1) );

                c.setPosition( c.position() + (*i).d_col - 1 );
                c.movePosition(QTextCursor::EndOfWord, QTextCursor::KeepAnchor);

                QTextEdit::ExtraSelection sel;
                sel.format = errorFormat;
                sel.cursor = c;
                sel.format.setToolTip((*i).d_msg);

                sum << sel;
            }
        }

        sum << d_link;

        setExtraSelections(sum);
    }

    void mousePressEvent(QMouseEvent* e)
    {
        if( !d_link.isEmpty() )
        {
            QTextCursor cur = cursorForPosition(e->pos());
            d_ide->pushLocation( OberonIde::Location( getPath(), cur.blockNumber(), cur.positionInBlock() ) );
            QApplication::restoreOverrideCursor();
            d_link.clear();
        }
        if( QApplication::keyboardModifiers() == Qt::ControlModifier )
        {
            QTextCursor cur = cursorForPosition(e->pos());
            Ast::Ref<Ast::Expression> e = d_pro->findSymbolBySourcePos(
                        getPath(),cur.blockNumber() + 1,cur.positionInBlock() + 1);
            if( e )
            {
                Ast::Named* sym = e->getIdent();
                d_ide->pushLocation( OberonIde::Location( getPath(), cur.blockNumber(), cur.positionInBlock() ) );
                if( sym->getTag() == Ast::Thing::T_Import && e->d_loc == sym->d_loc )
                    sym = Ast::thing_cast<Ast::Import*>(sym)->d_mod.data();
                if( sym->getTag() == Ast::Thing::T_Module && Ast::thing_cast<Ast::Module*>(sym)->d_synthetic )
                    d_ide->fillXref(sym);
                else
                    d_ide->showEditor( sym );
                //setCursorPosition( sym->d_loc.d_row - 1, sym->d_loc.d_col - 1, true );
            }
            updateExtraSelections();
        }else
            QPlainTextEdit::mousePressEvent(e);
    }

    void mouseMoveEvent(QMouseEvent* e)
    {
        QPlainTextEdit::mouseMoveEvent(e);
        if( QApplication::keyboardModifiers() == Qt::ControlModifier )
        {
            QTextCursor cur = cursorForPosition(e->pos());
            Ast::Ref<Ast::Expression> e = d_pro->findSymbolBySourcePos(
                        getPath(),cur.blockNumber() + 1,cur.positionInBlock() + 1);
            const bool alreadyArrow = !d_link.isEmpty();
            d_link.clear();
            if( e )
            {
                Ast::Named* sym = e->getIdent();
                const int off = cur.positionInBlock() + 1 - e->d_loc.d_col;
                cur.setPosition(cur.position() - off);
                cur.setPosition( cur.position() + sym->d_name.size(), QTextCursor::KeepAnchor );

                QTextEdit::ExtraSelection sel;
                sel.cursor = cur;
                sel.format.setFontUnderline(true);
                d_link << sel;
                /*
                d_linkLineNr = sym->d_loc.d_row - 1;
                d_linkColNr = sym->d_loc.d_col - 1;
                */
                if( !alreadyArrow )
                    QApplication::setOverrideCursor(Qt::ArrowCursor);
            }
            if( alreadyArrow && d_link.isEmpty() )
                QApplication::restoreOverrideCursor();
            updateExtraSelections();
        }else if( !d_link.isEmpty() )
        {
            QApplication::restoreOverrideCursor();
            d_link.clear();
            updateExtraSelections();
        }
    }

    void onUpdateModel()
    {
        d_ide->compile();
        if( !d_nonTerms.isEmpty() && !d_pro->getErrs()->getErrors().isEmpty() )
        {
            d_nonTerms.clear();
            updateExtraSelections();
        }
    }
};

class OberonIde::DocTab : public DocTabWidget
{
public:
    DocTab(QWidget* p):DocTabWidget(p,false) {}

    // overrides
    bool isUnsaved(int i)
    {
        OberonIde::Editor* edit = static_cast<OberonIde::Editor*>( widget(i) );
        return edit->isModified();
    }

    bool save(int i)
    {
        OberonIde::Editor* edit = static_cast<OberonIde::Editor*>( widget(i) );
        if( !edit->saveToFile( edit->getPath(), false ) )
            return false;
        return true;
    }
};

class OberonIde::Debugger : public DbgShell
{
public:
    OberonIde* d_ide;
    Debugger(OberonIde* ide):d_ide(ide){}
    void handleBreak( Engine2* lua, const QByteArray& source, quint32 line )
    {
        d_ide->enableDbgMenu();
        d_ide->fillStack();
        d_ide->fillLocals();

        QByteArray msg = lua->getValueString(1).simplified();
        msg = msg.mid(1,msg.size()-2); // remove ""
        if( !lua->isBreakHit() )
        {
            if( source == "=[C]" && !msg.startsWith('[') )
                msg = "[\"=[C]\"]:" + QByteArray::number(line) + ":" + msg;
            if( d_ide->luaRuntimeMessage(msg,source) )
                d_ide->onErrors();
        }

        while( lua->isWaiting() )
        {
            QApplication::processEvents(QEventLoop::AllEvents | QEventLoop::WaitForMoreEvents );
            QApplication::flush();
        }
        d_ide->removePosMarkers();
        d_ide->enableDbgMenu();
        d_ide->d_stack->clear();
        d_ide->d_locals->clear();
    }
    void handleAliveSignal(Engine2* e)
    {
        QApplication::processEvents(QEventLoop::AllEvents | QEventLoop::WaitForMoreEvents );
        QApplication::flush();
    }
};

static OberonIde* s_this = 0;
static void report(QtMsgType type, const QString& message )
{
    if( s_this )
    {
        switch(type)
        {
        case QtDebugMsg:
            // NOP s_this->logMessage(QLatin1String("INF: ") + message);
            break;
        case QtWarningMsg:
            s_this->logMessage(QLatin1String("WRN: ") + message);
            break;
        case QtCriticalMsg:
        case QtFatalMsg:
            s_this->logMessage(QLatin1String("ERR: ") + message, true);
            break;
        }
    }
}
static QtMessageHandler s_oldHandler = 0;
void messageHander(QtMsgType type, const QMessageLogContext& ctx, const QString& message)
{
    if( s_oldHandler )
        s_oldHandler(type, ctx, message );
    report(type,message);
}

static void loadLuaLib( Lua::Engine2* lua, const QByteArray& name )
{
    QFile lib( QString(":/runtime/%1.lua").arg(name.constData()) );
    lib.open(QIODevice::ReadOnly);
    if( !lua->addSourceLib( lib.readAll(), name ) )
        qCritical() << "compiling" << name << ":" << lua->getLastError();
}

static bool preloadLib( Project* pro, const QByteArray& name )
{
    QFile f( QString(":/oakwood/%1.Def" ).arg(name.constData() ) );
    if( !f.open(QIODevice::ReadOnly) )
    {
        qCritical() << "unknown preload" << name;
        return false;
    }
    pro->getFc()->addFile( name, f.readAll(), true );
    return true;
}

OberonIde::OberonIde(QWidget *parent)
    : QMainWindow(parent),d_lock(false),d_filesDirty(false),d_pushBackLock(false)
{
    s_this = this;

    d_pro = new Project(this);

    d_lua = new Engine2(this);
    d_lua->addStdLibs();
    d_lua->addLibrary(Engine2::PACKAGE);
    d_lua->addLibrary(Engine2::IO);
    d_lua->addLibrary(Engine2::BIT);
    d_lua->addLibrary(Engine2::JIT);
    d_lua->addLibrary(Engine2::FFI);
    d_lua->addLibrary(Engine2::OS);
    LjLib::install(d_lua->getCtx());
    loadLuaLib( d_lua, "obnlj" );

    d_dbg = new Debugger(this);
    d_lua->setDbgShell(d_dbg);
    // d_lua->setAliveSignal(true); // reduces performance by factor 2 to 5
    connect( d_lua, SIGNAL(onNotify(int,QByteArray,int)),this,SLOT(onLuaNotify(int,QByteArray,int)) );

    Engine2::setInst(d_lua);

    d_tab = new DocTab(this);
    d_tab->setCloserIcon( ":/images/close.png" );
    Gui::AutoMenu* pop = new Gui::AutoMenu( d_tab, true );
    pop->addCommand( tr("Forward Tab"), d_tab, SLOT(onDocSelect()), tr(OBN_NEXTDOC_SC) );
    pop->addCommand( tr("Backward Tab"), d_tab, SLOT(onDocSelect()), tr(OBN_PREVDOC_SC) );
    pop->addCommand( tr("Close Tab"), d_tab, SLOT(onCloseDoc()), tr("CTRL+W") );
    pop->addCommand( tr("Close All"), d_tab, SLOT(onCloseAll()) );
    pop->addCommand( tr("Close All Others"), d_tab, SLOT(onCloseAllButThis()) );
    addTopCommands( pop );

    new Gui::AutoShortcut( tr(OBN_NEXTDOC_SC), this, d_tab, SLOT(onDocSelect()) );
    new Gui::AutoShortcut( tr(OBN_PREVDOC_SC), this, d_tab, SLOT(onDocSelect()) );
    new Gui::AutoShortcut( tr("CTRL+W"), this, d_tab, SLOT(onCloseDoc()) );

    connect( d_tab, SIGNAL( currentChanged(int) ), this, SLOT(onTabChanged() ) );
    connect( d_tab, SIGNAL(closing(int)), this, SLOT(onTabClosing(int)) );

    setDockNestingEnabled(true);
    setCorner( Qt::BottomRightCorner, Qt::RightDockWidgetArea );
    setCorner( Qt::BottomLeftCorner, Qt::LeftDockWidgetArea );
    setCorner( Qt::TopRightCorner, Qt::RightDockWidgetArea );
    setCorner( Qt::TopLeftCorner, Qt::LeftDockWidgetArea );

    d_dbgBreak = new QAction(tr("Break"),this);
    d_dbgBreak->setShortcutContext(Qt::ApplicationShortcut);
    d_dbgBreak->setShortcut(tr(OBN_BREAK_SC));
    addAction(d_dbgBreak);
    connect( d_dbgBreak, SIGNAL(triggered(bool)),this,SLOT(onBreak()) );
    d_dbgAbort = new QAction(tr("Abort"),this);
    d_dbgAbort->setShortcutContext(Qt::ApplicationShortcut);
    d_dbgAbort->setShortcut(tr(OBN_ABORT_SC));
    addAction(d_dbgAbort);
    connect( d_dbgAbort, SIGNAL(triggered(bool)),this,SLOT(onAbort()) );
    d_dbgContinue = new QAction(tr("Continue"),this);
    d_dbgContinue->setShortcutContext(Qt::ApplicationShortcut);
    d_dbgContinue->setShortcut(tr(OBN_CONTINUE_SC));
    addAction(d_dbgContinue);
    connect( d_dbgContinue, SIGNAL(triggered(bool)),this,SLOT(onContinue()) );
    d_dbgStepIn = new QAction(tr("Step In"),this);
    d_dbgStepIn->setShortcutContext(Qt::ApplicationShortcut);
    d_dbgStepIn->setShortcut(tr(OBN_STEPIN_SC));
    addAction(d_dbgStepIn);
    connect( d_dbgStepIn, SIGNAL(triggered(bool)),this,SLOT(onSingleStep()) );

    enableDbgMenu();

    createTerminal();
    createDumpView();
    createMods();
    createErrs();
    createXref();
    createStack();
    createLocals();
    createMenu();

    setCentralWidget(d_tab);

    createMenuBar();

    s_oldHandler = qInstallMessageHandler(messageHander);

    QSettings s;

    const QRect screen = QApplication::desktop()->screenGeometry();
    resize( screen.width() - 20, screen.height() - 30 ); // so that restoreState works
    if( s.value("Fullscreen").toBool() )
        showFullScreen();
    else
        showMaximized();

    const QVariant state = s.value( "DockState" );
    if( !state.isNull() )
        restoreState( state.toByteArray() );


    connect( d_pro,SIGNAL(sigRenamed()),this,SLOT(onCaption()) );
    connect( d_pro,SIGNAL(sigModified(bool)),this,SLOT(onCaption()) );
}

OberonIde::~OberonIde()
{
    delete d_dbg;
}

void OberonIde::loadFile(const QString& path)
{
    QFileInfo info(path);

    if( info.isDir() && info.suffix() != ".obnpro" )
    {
        d_pro->initializeFromDir( path );
    }else
    {
        d_pro->loadFrom(path);
    }

    onCaption();

    onCompile();
}

void OberonIde::logMessage(const QString& str, bool err)
{
    d_term->printText(str,err);
}

void OberonIde::closeEvent(QCloseEvent* event)
{
    QSettings s;
    s.setValue( "DockState", saveState() );
    const bool ok = checkSaved( tr("Quit Application"));
    event->setAccepted(ok);
    if( ok )
    {
        d_lua->terminate(true);
        SysInnerLib::quit();
    }
}

void OberonIde::createTerminal()
{
    QDockWidget* dock = new QDockWidget( tr("Terminal"), this );
    dock->setObjectName("Terminal");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    d_term = new Terminal2(dock, d_lua);
    dock->setWidget(d_term);
    addDockWidget( Qt::BottomDockWidgetArea, dock );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+C"), this, d_term, SLOT(onClear()) );
}

void OberonIde::createDumpView()
{
    QDockWidget* dock = new QDockWidget( tr("Bytecode"), this );
    dock->setObjectName("Bytecode");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    d_bcv = new BcViewer2(dock);
    dock->setWidget(d_bcv);
    addDockWidget( Qt::RightDockWidgetArea, dock );
    connect(d_bcv,SIGNAL(sigGotoLine(quint32)),this,SLOT(onGotoLnr(quint32)));

    Gui::AutoMenu* pop = new Gui::AutoMenu( d_bcv, true );
    pop->addCommand( "Run on LuaJIT", this, SLOT(onRun()), tr("CTRL+R"), false );
    addDebugMenu(pop);
    pop->addSeparator();
    pop->addCommand( "Show low level bytecode", this, SLOT(onShowLlBc()) );
    pop->addCommand( "Export binary...", this, SLOT(onExportBc()) );
    pop->addCommand( "Export LjAsm...", this, SLOT(onExportAsm()) );
    addTopCommands(pop);
}

void OberonIde::createMods()
{
    QDockWidget* dock = new QDockWidget( tr("Modules"), this );
    dock->setObjectName("Modules");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable );
    d_mods = new QTreeWidget(dock);
    d_mods->setHeaderHidden(true);
    d_mods->setExpandsOnDoubleClick(false);
    d_mods->setAlternatingRowColors(true);
    dock->setWidget(d_mods);
    addDockWidget( Qt::LeftDockWidgetArea, dock );
    connect( d_mods, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)),this,SLOT(onModsDblClicked(QTreeWidgetItem*,int)) );
}

void OberonIde::createErrs()
{
    QDockWidget* dock = new QDockWidget( tr("Issues"), this );
    dock->setObjectName("Issues");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    d_errs = new QTreeWidget(dock);
    d_errs->setSizePolicy(QSizePolicy::MinimumExpanding,QSizePolicy::Preferred);
    d_errs->setAlternatingRowColors(true);
    d_errs->setHeaderHidden(true);
    d_errs->setSortingEnabled(false);
    d_errs->setAllColumnsShowFocus(true);
    d_errs->setRootIsDecorated(false);
    d_errs->setColumnCount(3);
    d_errs->header()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    d_errs->header()->setSectionResizeMode(1, QHeaderView::ResizeToContents);
    d_errs->header()->setSectionResizeMode(2, QHeaderView::Stretch);
    dock->setWidget(d_errs);
    addDockWidget( Qt::BottomDockWidgetArea, dock );
    connect(d_errs, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)), this, SLOT(onErrorsDblClicked()) );
    connect( new QShortcut( tr("ESC"), this ), SIGNAL(activated()), dock, SLOT(hide()) );
}

void OberonIde::createXref()
{
    QDockWidget* dock = new QDockWidget( tr("Xref"), this );
    dock->setObjectName("Xref");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    QWidget* pane = new QWidget(dock);
    QVBoxLayout* vbox = new QVBoxLayout(pane);
    vbox->setMargin(0);
    vbox->setSpacing(0);
    d_xrefTitle = new QLabel(pane);
    d_xrefTitle->setMargin(2);
    d_xrefTitle->setWordWrap(true);
    vbox->addWidget(d_xrefTitle);
    d_xref = new QTreeWidget(pane);
    d_xref->setAlternatingRowColors(true);
    d_xref->setHeaderHidden(true);
    d_xref->setAllColumnsShowFocus(true);
    d_xref->setRootIsDecorated(false);
    vbox->addWidget(d_xref);
    dock->setWidget(pane);
    addDockWidget( Qt::LeftDockWidgetArea, dock );
    connect(d_xref, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)), this, SLOT(onXrefDblClicked()) );
}

void OberonIde::createStack()
{
    QDockWidget* dock = new QDockWidget( tr("Stack"), this );
    dock->setObjectName("Stack");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    d_stack = new QTreeWidget(dock);
    d_stack->setHeaderHidden(true);
    d_stack->setAlternatingRowColors(true);
    d_stack->setColumnCount(4); // Level, Name, Pos, Mod
    d_stack->header()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    d_stack->header()->setSectionResizeMode(1, QHeaderView::ResizeToContents);
    d_stack->header()->setSectionResizeMode(2, QHeaderView::ResizeToContents);
    d_stack->header()->setSectionResizeMode(3, QHeaderView::Stretch);
    dock->setWidget(d_stack);
    addDockWidget( Qt::LeftDockWidgetArea, dock );
    connect( d_stack, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)),this,SLOT(onStackDblClicked(QTreeWidgetItem*,int)) );
}

void OberonIde::createLocals()
{
    QDockWidget* dock = new QDockWidget( tr("Locals"), this );
    dock->setObjectName("Locals");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    d_locals = new QTreeWidget(dock);
    d_locals->setHeaderHidden(true);
    d_locals->setAlternatingRowColors(true);
    d_locals->setColumnCount(2); // Name, Value
    d_locals->header()->setSectionResizeMode(0, QHeaderView::ResizeToContents);
    d_locals->header()->setSectionResizeMode(1, QHeaderView::Stretch);
    dock->setWidget(d_locals);
    addDockWidget( Qt::LeftDockWidgetArea, dock );
}

void OberonIde::createMenu()
{
    Gui::AutoMenu* pop = new Gui::AutoMenu( d_mods, true );
    pop->addCommand( "Show File", this, SLOT(onOpenFile()) );
    pop->addAction("Expand all", d_mods, SLOT(expandAll()) );
    pop->addSeparator();
    pop->addCommand( "New Project", this, SLOT(onNewPro()), tr("CTRL+N"), false );
    pop->addCommand( "Open Project...", this, SLOT(onOpenPro()), tr("CTRL+O"), false );
    pop->addCommand( "Save Project", this, SLOT(onSavePro()), tr("CTRL+SHIFT+S"), false );
    pop->addCommand( "Save Project as...", this, SLOT(onSaveAs()) );
    pop->addSeparator();
    pop->addCommand( "Add Modules...", this, SLOT(onAddFiles()) );
    pop->addCommand( "Remove Module...", this, SLOT(onRemoveFile()) );
    pop->addSeparator();
    pop->addCommand( "Built-in Oakwood", this, SLOT(onOakwood()) );
    pop->addCommand( "Built-in Oberon System Inner", this, SLOT(onObSysInner()) );
    pop->addCommand( "Set Working Directory...", this, SLOT( onWorkingDir() ) );
    pop->addSeparator();
    pop->addCommand( "Compile", this, SLOT(onCompile()), tr("CTRL+T"), false );
    pop->addCommand( "Compile && Generate", this, SLOT(onGenerate()), tr("CTRL+SHIFT+T"), false );
    pop->addCommand( "Run on LuaJIT", this, SLOT(onRun()), tr("CTRL+R"), false );
    addDebugMenu(pop);
    addTopCommands(pop);

    new Gui::AutoShortcut( tr("CTRL+O"), this, this, SLOT(onOpenPro()) );
    new Gui::AutoShortcut( tr("CTRL+N"), this, this, SLOT(onNewPro()) );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+S"), this, this, SLOT(onSavePro()) );
    new Gui::AutoShortcut( tr("CTRL+S"), this, this, SLOT(onSaveFile()) );
    new Gui::AutoShortcut( tr("CTRL+R"), this, this, SLOT(onRun()) );
    new Gui::AutoShortcut( tr("CTRL+T"), this, this, SLOT(onCompile()) );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+T"), this, this, SLOT(onGenerate()) );
    new Gui::AutoShortcut( tr(OBN_GOBACK_SC), this, this, SLOT(handleGoBack()) );
    new Gui::AutoShortcut( tr(OBN_GOFWD_SC), this, this, SLOT(handleGoForward()) );
    new Gui::AutoShortcut( tr(OBN_TOGBP_SC), this, this, SLOT(onToggleBreakPt()) );
    new Gui::AutoShortcut( tr(OBN_ENDBG_SC), this, this, SLOT(onEnableDebug()) );
}

void OberonIde::createMenuBar()
{
    Gui::AutoMenu* pop = new Gui::AutoMenu( tr("File"), this );
    pop->addCommand( "New Project", this, SLOT(onNewPro()), tr("CTRL+N"), false );
    pop->addCommand( "Open Project...", this, SLOT(onOpenPro()), tr("CTRL+O"), false );
    pop->addCommand( "Save Project", this, SLOT(onSavePro()), tr("CTRL+SHIFT+S"), false );
    pop->addCommand( "Save Project as...", this, SLOT(onSaveAs()) );
    pop->addSeparator();
    pop->addCommand( "Save", this, SLOT(onSaveFile()), tr("CTRL+S"), false );
    pop->addCommand( tr("Close file"), d_tab, SLOT(onCloseDoc()), tr("CTRL+W") );
    pop->addCommand( tr("Close all"), d_tab, SLOT(onCloseAll()) );
    pop->addSeparator();
    pop->addCommand( "Export binary...", this, SLOT(onExportBc()) );
    pop->addCommand( "Export LjAsm...", this, SLOT(onExportAsm()) );
    pop->addSeparator();
    pop->addAutoCommand( "Print...", SLOT(handlePrint()), tr("CTRL+P"), true );
    pop->addAutoCommand( "Export PDF...", SLOT(handleExportPdf()), tr("CTRL+SHIFT+P"), true );
    pop->addSeparator();
    pop->addAction(tr("Quit"),qApp,SLOT(quit()), tr("CTRL+Q") );

    pop = new Gui::AutoMenu( tr("Edit"), this );
    pop->addAutoCommand( "Undo", SLOT(handleEditUndo()), tr("CTRL+Z"), true );
    pop->addAutoCommand( "Redo", SLOT(handleEditRedo()), tr("CTRL+Y"), true );
    pop->addSeparator();
    pop->addAutoCommand( "Cut", SLOT(handleEditCut()), tr("CTRL+X"), true );
    pop->addAutoCommand( "Copy", SLOT(handleEditCopy()), tr("CTRL+C"), true );
    pop->addAutoCommand( "Paste", SLOT(handleEditPaste()), tr("CTRL+V"), true );
    pop->addSeparator();
    pop->addAutoCommand( "Find...", SLOT(handleFind()), tr("CTRL+F"), true );
    pop->addAutoCommand( "Find again", SLOT(handleFindAgain()), tr("F3"), true );
    pop->addAutoCommand( "Replace...", SLOT(handleReplace()) );
    pop->addSeparator();
    pop->addAutoCommand( "&Go to line...", SLOT(handleGoto()), tr("CTRL+G"), true );
    pop->addSeparator();
    pop->addAutoCommand( "Indent", SLOT(handleIndent()) );
    pop->addAutoCommand( "Unindent", SLOT(handleUnindent()) );
    pop->addAutoCommand( "Fix Indents", SLOT(handleFixIndent()) );
    pop->addAutoCommand( "Set Indentation Level...", SLOT(handleSetIndent()) );

    pop = new Gui::AutoMenu( tr("Project"), this );
    pop->addCommand( "Add Modules...", this, SLOT(onAddFiles()) );
    pop->addCommand( "Remove Module...", this, SLOT(onRemoveFile()) );
    pop->addSeparator();
    pop->addCommand( "Built-in Oakwood", this, SLOT(onOakwood()) );
    pop->addCommand( "Built-in Oberon System Inner", this, SLOT(onObSysInner()) );
    pop->addCommand( "Set Working Directory...", this, SLOT( onWorkingDir() ) );

    pop = new Gui::AutoMenu( tr("Build && Run"), this );
    pop->addCommand( "Compile", this, SLOT(onCompile()), tr("CTRL+T"), false );
    pop->addCommand( "Compile && Generate", this, SLOT(onGenerate()), tr("CTRL+SHIFT+T"), false );
    pop->addCommand( "Run on LuaJIT", this, SLOT(onRun()), tr("CTRL+R"), false );

    pop = new Gui::AutoMenu( tr("Debug"), this );
    pop->addCommand( "Enable Debugging", this, SLOT(onEnableDebug()),tr(OBN_ENDBG_SC), false );
    pop->addCommand( "Toggle Breakpoint", this, SLOT(onToggleBreakPt()), tr(OBN_TOGBP_SC), false);
    pop->addAction( d_dbgStepIn );
    pop->addAction( d_dbgBreak );
    pop->addAction( d_dbgContinue );
    pop->addAction( d_dbgAbort );


    pop = new Gui::AutoMenu( tr("Window"), this );
    pop->addCommand( tr("Next Tab"), d_tab, SLOT(onDocSelect()), tr(OBN_NEXTDOC_SC) );
    pop->addCommand( tr("Previous Tab"), d_tab, SLOT(onDocSelect()), tr(OBN_PREVDOC_SC) );
    pop->addSeparator();
    pop->addCommand( "Go Back", this, SLOT(handleGoBack()), tr(OBN_GOBACK_SC), false );
    pop->addCommand( "Go Forward", this, SLOT(handleGoForward()), tr(OBN_GOFWD_SC), false );
    pop->addSeparator();
    pop->addAutoCommand( "Set &Font...", SLOT(handleSetFont()) );
    pop->addAutoCommand( "Show &Linenumbers", SLOT(handleShowLinenumbers()) );
    pop->addCommand( "Show Fullscreen", this, SLOT(onFullScreen()) );
    pop->addSeparator();
    QMenu* sub2 = createPopupMenu();
    sub2->setTitle( tr("Show Window") );
    pop->addMenu( sub2 );

    Gui::AutoMenu* help = new Gui::AutoMenu( tr("Help"), this, true );
    help->addCommand( "&About this application...", this, SLOT(onAbout()) );
    help->addCommand( "&About Qt...", this, SLOT(onQt()) );
}

void OberonIde::onCompile()
{
    ENABLED_IF(true);
    compile();
}

void OberonIde::onRun()
{
    ENABLED_IF( !d_pro->getFiles().isEmpty() && !d_lua->isExecuting() );

    if( !compile(true) )
        return;

    QDir::setCurrent(d_pro->getWorkingDir(true));

    Project::FileList files = d_pro->getFilesInExecOrder();
    if( files.isEmpty() )
    {
        qWarning() << "nothing to run";
        return;
    }

    if( d_pro->useBuiltInOakwood() )
    {
        loadLuaLib(d_lua,"In");
        loadLuaLib(d_lua,"Out");
        loadLuaLib(d_lua,"Files");
        loadLuaLib(d_lua,"Input");
        loadLuaLib(d_lua,"Math");
        loadLuaLib(d_lua,"Strings");
        loadLuaLib(d_lua,"Coroutines");
        loadLuaLib(d_lua,"XYPlane");
    }

    if( d_pro->useBuiltInObSysInner() )
        SysInnerLib::install(d_lua->getCtx());

    bool hasErrors = false;
    foreach( const Project::File& f, files )
    {
        qDebug() << "loading" << f.d_mod->d_name;
        if( !d_lua->addSourceLib( f.d_bc, f.d_mod->d_name ) )
        {
            hasErrors = true;
        }
        if( d_lua->isAborted() )
        {
            removePosMarkers();
            return;
        }
    }

    if( hasErrors )
    {
        removePosMarkers();
        onErrors();
        return;
    }

    Project::ModProc main = d_pro->getMain();
    if( main.first.isNull() )
    {
        Q_ASSERT( !files.isEmpty() );
        main.first = files.back().d_mod->d_name;
    }

    QByteArray src;
    QTextStream out(&src);

    //out << "jit.off()" << endl;
    //out << "jit.opt.start(3)" << endl;
    //out << "jit.opt.start(\"-abc\")" << endl;
    //out << "jit.opt.start(\"-fuse\")" << endl;
    //out << "jit.opt.start(\"hotloop=10\", \"hotexit=2\")" << endl;

    if( !main.second.isEmpty() )
    {
        out << "local " << main.first << " = require '" << main.first << "'" << endl;
        out << main.first << "." << main.second << "()" << endl;
    }
    out.flush();
    if( !src.isEmpty() )
        d_lua->executeCmd(src,"terminal");
    removePosMarkers();

}

void OberonIde::onAbort()
{
    // ENABLED_IF( d_lua->isWaiting() );
    d_lua->terminate();
}

void OberonIde::onGenerate()
{
    ENABLED_IF(true);
    compile(true);
}

void OberonIde::onNewPro()
{
    ENABLED_IF(true);

    if( !checkSaved( tr("New Project")) )
        return;

    d_pro->createNew();
    d_tab->onCloseAll();
    compile();
}

void OberonIde::onOpenPro()
{
    ENABLED_IF( true );

    if( !checkSaved( tr("New Project")) )
        return;

    const QString fileName = QFileDialog::getOpenFileName(this, tr("Open Project"),QString(),
                                                          tr("Oberon Project (*.obnpro)") );
    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    d_tab->onCloseAll();
    d_pro->loadFrom(fileName);

    compile();
}

void OberonIde::onSavePro()
{
    ENABLED_IF( d_pro->isDirty() );

    if( !d_pro->getFilePath().isEmpty() )
        d_pro->save();
    else
        onSaveAs();
}

void OberonIde::onSaveFile()
{
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    ENABLED_IF( edit && edit->isModified() );

    edit->saveToFile( edit->getPath() );
    d_pro->getFc()->removeFile( edit->getPath() );
}

void OberonIde::onSaveAs()
{
    ENABLED_IF(true);

    QString fileName = QFileDialog::getSaveFileName(this, tr("Save Project"),
                                                          QFileInfo(d_pro->getFilePath()).absolutePath(),
                                                          tr("Oberon Project (*.obnpro)") );

    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    if( !fileName.endsWith(".obnpro",Qt::CaseInsensitive ) )
        fileName += ".obnpro";

    d_pro->saveTo(fileName);
    onCaption();
}

void OberonIde::onCaption()
{
    const QString star = d_pro->isDirty() || d_filesDirty ? "*" : "";
    if( d_pro->getFilePath().isEmpty() )
    {
        setWindowTitle(tr("<unnamed>%2 - %1").arg(qApp->applicationName()).arg(star));
    }else
    {
        QFileInfo info(d_pro->getFilePath());
        setWindowTitle(tr("%1%2 - %3").arg(info.fileName()).arg(star).arg(qApp->applicationName()) );
    }
}

void OberonIde::onGotoLnr(quint32 lnr)
{
    if( d_lock )
        return;
    d_lock = true;
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    if( edit )
    {
        if( RowCol::isPacked(lnr) )
            edit->setCursorPosition(RowCol::unpackRow(lnr)-1,RowCol::unpackCol(lnr)-1);
        else
            edit->setCursorPosition(lnr-1,0);
        edit->setFocus();
    }
    d_lock = false;
}

void OberonIde::onFullScreen()
{
    CHECKED_IF(true,isFullScreen());
    QSettings s;
    if( isFullScreen() )
    {
        showMaximized();
        s.setValue("Fullscreen", false );
    }else
    {
        showFullScreen();
        s.setValue("Fullscreen", true );
    }
}

void OberonIde::onCursor()
{
    fillXref();
    if( d_lock )
        return;
    d_lock = true;
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    if( edit )
    {
        QTextCursor cur = edit->textCursor();
        const int line = cur.blockNumber() + 1;
        d_bcv->gotoLine(RowCol(line,cur.positionInBlock() + 1).packed());
    }
    d_lock = false;
}

void OberonIde::onExportBc()
{
    const QString curPath = d_tab->getCurrentDoc().toString();
    ENABLED_IF(d_tab->getCurrentTab() != 0 && !d_pro->getFiles().value(curPath).d_bc.isEmpty() );

    QString fileName = QFileDialog::getSaveFileName(this, tr("Save Binary"), curPath, tr("*.ljbc") );

    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    if( !fileName.endsWith(".ljbc",Qt::CaseInsensitive ) )
        fileName += ".ljbc";
    QFile out(fileName);
    out.open(QIODevice::WriteOnly);
    out.write(d_pro->getFiles().value(curPath).d_bc);
}

void OberonIde::onExportAsm()
{
    const QString curPath = d_tab->getCurrentDoc().toString();
    ENABLED_IF(d_tab->getCurrentTab() != 0 && !d_pro->getFiles().value(curPath).d_bc.isEmpty() );

    QString fileName = QFileDialog::getSaveFileName(this, tr("Save Assembler"),
                                                          d_tab->getCurrentDoc().toString(),
                                                          tr("*.ljasm") );

    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    if( !fileName.endsWith(".ljasm",Qt::CaseInsensitive ) )
        fileName += ".ljasm";

    d_bcv->saveTo(fileName);
}

void OberonIde::onModsDblClicked(QTreeWidgetItem* item, int)
{
    ScopeRef s = item->data(0,Qt::UserRole).value<ScopeRef>();
    if( s.isNull() )
        return;

    showEditor( s.data() );
}

void OberonIde::onStackDblClicked(QTreeWidgetItem* item, int)
{
    if( item )
    {
        const QString source = item->data(3,Qt::UserRole).toString();
        if( !source.isEmpty() )
        {
            const quint32 line = item->data(2,Qt::UserRole).toUInt();
            if( RowCol::isPacked(line) )
                showEditor( source, RowCol::unpackRow(line), RowCol::unpackCol(line) );
            else
                showEditor( source, line, 1 );
        }
        const int level = item->data(0,Qt::UserRole).toInt();
        d_lua->setActiveLevel(level);
        fillLocals();
    }
}

void OberonIde::onTabChanged()
{
    const QString path = d_tab->getCurrentDoc().toString();

    onEditorChanged();

    if( !path.isEmpty() )
    {
        d_curBc = d_pro->getFiles().value( path ).d_bc;
        if( !d_curBc.isEmpty() )
        {
            QBuffer buf( &d_curBc );
            buf.open(QIODevice::ReadOnly);
            d_bcv->loadFrom(&buf);
            onCursor();
            return;
        }
    }
    // else
    d_bcv->clear();
}

void OberonIde::onTabClosing(int i)
{
    d_pro->getFc()->removeFile( d_tab->getDoc(i).toString() );
}

void OberonIde::onEditorChanged()
{
    // only fired once when editor switches from unmodified to modified and back
    // not fired for every key press
    d_filesDirty = false;
    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* e = static_cast<Editor*>( d_tab->widget(i) );
        if( e->isModified() )
            d_filesDirty = true;
        QFileInfo info( d_tab->getDoc(i).toString() );
        d_tab->setTabText(i, info.fileName() + ( e->isModified() ? "*" : "" ) );
    }
    onCaption();
}

void OberonIde::onErrorsDblClicked()
{
    QTreeWidgetItem* item = d_errs->currentItem();
    showEditor( item->data(0, Qt::UserRole ).toString(),
                item->data(1, Qt::UserRole ).toInt(), item->data(2, Qt::UserRole ).toInt() );
}

static bool errorEntryLessThan(const Errors::Entry &s1, const Errors::Entry &s2)
{
    return s1.d_nr < s2.d_nr;
}

void OberonIde::onErrors()
{
    d_errs->clear();
    QList<Errors::Entry> errs = d_pro->getErrs()->getErrors().toList();
    std::sort(errs.begin(), errs.end(), errorEntryLessThan );

    for( int i = 0; i < errs.size(); i++ )
    {
        QTreeWidgetItem* item = new QTreeWidgetItem(d_errs);
        item->setText(2, errs[i].d_msg );
        item->setToolTip(2, item->text(2) );
        if( errs[i].d_isErr )
            item->setIcon(0, QPixmap(":/images/exclamation-red.png") );
        else
            item->setIcon(0, QPixmap(":/images/exclamation-circle.png") );
        item->setText(0, QFileInfo(errs[i].d_file).baseName() );
        item->setText(1, QString("%1:%2").arg(errs[i].d_line).arg(errs[i].d_col));
        item->setData(0, Qt::UserRole, errs[i].d_file );
        item->setData(1, Qt::UserRole, errs[i].d_line );
        item->setData(2, Qt::UserRole, errs[i].d_col );
    }
    if( errs.size() )
        d_errs->parentWidget()->show();

    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* e = static_cast<Editor*>( d_tab->widget(i) );
        Q_ASSERT( e );
        e->updateExtraSelections();
    }
}

void OberonIde::onOpenFile()
{
    ENABLED_IF( d_mods->currentItem() );

    onModsDblClicked( d_mods->currentItem(), 0 );
}

void OberonIde::onOakwood()
{
    CHECKED_IF( true, d_pro->useBuiltInOakwood() );

    d_pro->setUseBuiltInOakwood( !d_pro->useBuiltInOakwood() );
    if( d_pro->useBuiltInOakwood() )
        d_pro->setUseBuiltInObSysInner(false);
}

void OberonIde::onObSysInner()
{
    CHECKED_IF( true, d_pro->useBuiltInObSysInner() );

    d_pro->setUseBuiltInObSysInner( !d_pro->useBuiltInObSysInner() );
    if( d_pro->useBuiltInObSysInner() )
        d_pro->setUseBuiltInOakwood(false);
}

void OberonIde::onAddFiles()
{
    ENABLED_IF(true);

    QString filter;
    foreach( const QString& suf, d_pro->getSuffixes() )
        filter += " *" + suf;
    const QStringList files = QFileDialog::getOpenFileNames(this,tr("Add Modules"),QString(),filter );
    foreach( const QString& f, files )
    {
        if( !d_pro->addFile(f) )
            qWarning() << "cannot add module" << f;
    }
    compile();
}

void OberonIde::onRemoveFile()
{
    ENABLED_IF( d_mods->currentItem() );

    ScopeRef s = d_mods->currentItem()->data(0,Qt::UserRole).value<ScopeRef>();
    if( s.isNull() )
        return;

    Ast::Module* m = s->getModule();
    if( m == 0 )
        return;

    if( QMessageBox::warning( this, tr("Remove Module"),
                              tr("Do you really want to remove module '%1' from project?").arg(m->d_name.constData()),
                           QMessageBox::Yes | QMessageBox::Cancel, QMessageBox::Yes ) != QMessageBox::Yes )
        return;
    if( !d_pro->removeFile( m->d_file ) )
        qWarning() << "cannot remove module" << m->d_name;
    else
        compile();
}

void OberonIde::onEnableDebug()
{
    CHECKED_IF( true, d_lua->isDebug() );

    d_lua->setDebug( !d_lua->isDebug() );
    enableDbgMenu();
}

void OberonIde::onBreak()
{
    // normal call because called during processEvent which doesn't seem to enable
    // the functions: ENABLED_IF( d_lua->isExecuting() );
    d_lua->runToNextLine();
}

bool OberonIde::checkSaved(const QString& title)
{
    if( d_filesDirty )
    {
        switch( QMessageBox::critical( this, title, tr("There are modified files; do you want to save them?"),
                               QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel, QMessageBox::Yes ) )
        {
        case QMessageBox::Yes:
            // TODO
            break;
        case QMessageBox::No:
            break;
        default:
            return false;
        }
    }
    if( d_pro->isDirty() )
    {
        switch( QMessageBox::critical( this, title, tr("The the project has not been saved; do you want to save it?"),
                               QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel, QMessageBox::Yes ) )
        {
        case QMessageBox::Yes:
            if( !d_pro->getFilePath().isEmpty() )
                return d_pro->save();
            else
            {
                const QString path = QFileDialog::getSaveFileName( this, title, QString(), "Oberon Project (*.obnpro)" );
                if( path.isEmpty() )
                    return false;
                QDir::setCurrent(QFileInfo(path).absolutePath());
                return d_pro->saveTo(path);
            }
            break;
        case QMessageBox::No:
            return true;
        default:
            return false;
        }
    }
    return true;
}

bool OberonIde::compile(bool generate )
{
    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* e = static_cast<Editor*>( d_tab->widget(i) );
        if( e->isModified() )
            d_pro->getFc()->addFile( e->getPath(), e->toPlainText().toUtf8() );
        else
            d_pro->getFc()->removeFile( e->getPath() );
    }
    if( d_pro->useBuiltInOakwood() )
    {
        preloadLib(d_pro,"In");
        preloadLib(d_pro,"Out");
        preloadLib(d_pro,"Files");
        preloadLib(d_pro,"Input");
        preloadLib(d_pro,"Math");
        preloadLib(d_pro,"Strings");
        preloadLib(d_pro,"Coroutines");
        preloadLib(d_pro,"XYPlane");
    }
    const quint32 errCount = d_pro->getErrs()->getErrCount();
    d_pro->recompile();
    if( generate )
        d_pro->generate();
    onErrors();
    fillMods();
    onTabChanged();
    return errCount == d_pro->getErrs()->getErrCount();
}

static bool sortNamed( Ast::Named* lhs, Ast::Named* rhs )
{
    return lhs->d_name.toLower() < rhs->d_name.toLower();
}

static void fillScope( QTreeWidgetItem* p, Ast::Scope* s )
{
    //bool foundConst = false, foundType = false, foundVar = false;
    QList<Ast::Named*> sort;
    for( int j = 0; j < s->d_order.size(); j++ )
        sort << s->d_order[j];
    std::sort( sort.begin(), sort.end(), sortNamed );
    foreach( Ast::Named* n, sort )
    {
        if( n->getTag() == Ast::Thing::T_Procedure )
        {
            QTreeWidgetItem* item = new QTreeWidgetItem( p );
            item->setText(0, n->d_name + ( n->d_public ? "*" : "" ) );
            item->setData(0,Qt::UserRole, QVariant::fromValue(ScopeRef(Ast::thing_cast<Ast::Scope*>(n))) );
            fillScope(item, Ast::thing_cast<Ast::Scope*>(n) );
        }
    }
}

void OberonIde::fillMods()
{
    d_mods->clear();
    const Project::FileHash& files = d_pro->getFiles();
    Project::FileHash::const_iterator i;
    QList< Ast::Ref<Ast::Module> > temp;
    QList<Ast::Module*> sort;
    for( i = files.begin(); i != files.end(); ++i )
    {
        if( i.value().d_mod.isNull() )
        {
            Ast::Ref<Ast::Module> m = new Ast::Module();
            m->d_hasErrors;
            m->d_file = i.key();
            m->d_name = QFileInfo(i.key()).baseName().toUtf8();
            temp << m;
            sort << m.data();
        }else
            sort << i.value().d_mod.data();
    }
    std::sort( sort.begin(), sort.end(), sortNamed );
    foreach( Ast::Module* n, sort )
    {
        QTreeWidgetItem* item = new QTreeWidgetItem(d_mods);
        item->setText(0, n->d_name);
        item->setToolTip(0,n->d_file);
        item->setData(0,Qt::UserRole,QVariant::fromValue(ScopeRef( n ) ) );
        fillScope( item, Ast::thing_cast<Ast::Scope*>(n));
    }
}

void OberonIde::addTopCommands(Gui::AutoMenu* pop)
{
    Q_ASSERT( pop != 0 );
    pop->addSeparator();
    pop->addCommand( "Go Back", this, SLOT(handleGoBack()), tr(OBN_GOBACK_SC), false );
    pop->addCommand( "Go Forward", this, SLOT(handleGoForward()), tr(OBN_GOFWD_SC), false );
    pop->addSeparator();
    pop->addAutoCommand( "Set &Font...", SLOT(handleSetFont()) );
    pop->addAutoCommand( "Show &Linenumbers", SLOT(handleShowLinenumbers()) );
    pop->addCommand( "Show Fullscreen", this, SLOT(onFullScreen()) );
    pop->addSeparator();
    pop->addAction(tr("Quit"),qApp,SLOT(quit()) );
}

void OberonIde::showEditor(const QString& path, int row, int col, bool setMarker )
{
    if( !d_pro->getFiles().contains(path) )
        return;

    const int i = d_tab->findDoc(path);
    Editor* edit = 0;
    if( i != -1 )
    {
        d_tab->setCurrentIndex(i);
        edit = static_cast<Editor*>( d_tab->widget(i) );
    }else
    {
        edit = new Editor(this,d_pro);
        createMenu(edit);

        connect(edit, SIGNAL(modificationChanged(bool)), this, SLOT(onEditorChanged()) );
        connect(edit,SIGNAL(cursorPositionChanged()),this,SLOT(onCursor()));
        connect(edit,SIGNAL(sigUpdateLocation(int,int)),this,SLOT(onUpdateLocation(int,int)));

        edit->loadFromFile(path);

        const Engine2::Breaks& br = d_lua->getBreaks( path.toUtf8() );
        Engine2::Breaks::const_iterator j;
        for( j = br.begin(); j != br.end(); ++j )
            edit->addBreakPoint((*j) - 1);

        d_tab->addDoc(edit,path);
        onEditorChanged();
    }
    if( row > 0 && col > 0 )
    {
        edit->setCursorPosition( row-1, col-1, false );
        if( setMarker )
            edit->setPositionMarker(row-1);
    }
    edit->setFocus();
}

void OberonIde::showEditor(Ast::Named* n, bool setMarker)
{
    Ast::Module* mod = n->getModule();
    if( mod )
        showEditor( mod->d_file, n->d_loc.d_row, n->d_loc.d_col, setMarker );
}

void OberonIde::createMenu(OberonIde::Editor* edit)
{
    Gui::AutoMenu* pop = new Gui::AutoMenu( edit, true );
    pop->addCommand( "Save", this, SLOT(onSaveFile()), tr("CTRL+S"), false );
    pop->addSeparator();
    pop->addCommand( "Compile", this, SLOT(onCompile()), tr("CTRL+T"), false );
    pop->addCommand( "Run on LuaJIT", this, SLOT(onRun()), tr("CTRL+R"), false );
    addDebugMenu(pop);
    pop->addSeparator();
    pop->addCommand( "Export binary...", this, SLOT(onExportBc()) );
    pop->addCommand( "Export LjAsm...", this, SLOT(onExportAsm()) );
    pop->addSeparator();
    pop->addCommand( "Undo", edit, SLOT(handleEditUndo()), tr("CTRL+Z"), true );
    pop->addCommand( "Redo", edit, SLOT(handleEditRedo()), tr("CTRL+Y"), true );
    pop->addSeparator();
    pop->addCommand( "Cut", edit, SLOT(handleEditCut()), tr("CTRL+X"), true );
    pop->addCommand( "Copy", edit, SLOT(handleEditCopy()), tr("CTRL+C"), true );
    pop->addCommand( "Paste", edit, SLOT(handleEditPaste()), tr("CTRL+V"), true );
    pop->addSeparator();
    pop->addCommand( "Find...", edit, SLOT(handleFind()), tr("CTRL+F"), true );
    pop->addCommand( "Find again", edit, SLOT(handleFindAgain()), tr("F3"), true );
    pop->addCommand( "Replace...", edit, SLOT(handleReplace()) );
    pop->addSeparator();
    pop->addCommand( "&Goto...", edit, SLOT(handleGoto()), tr("CTRL+G"), true );
    pop->addSeparator();
    pop->addCommand( "Indent", edit, SLOT(handleIndent()) );
    pop->addCommand( "Unindent", edit, SLOT(handleUnindent()) );
    pop->addCommand( "Fix Indents", edit, SLOT(handleFixIndent()) );
    pop->addCommand( "Set Indentation Level...", edit, SLOT(handleSetIndent()) );
    pop->addSeparator();
    pop->addCommand( "Print...", edit, SLOT(handlePrint()), tr("CTRL+P"), true );
    pop->addCommand( "Export PDF...", edit, SLOT(handleExportPdf()), tr("CTRL+SHIFT+P"), true );
    addTopCommands(pop);
}

void OberonIde::addDebugMenu(Gui::AutoMenu* pop)
{
    Gui::AutoMenu* sub = new Gui::AutoMenu(tr("Debugger"), this, false );
    pop->addMenu(sub);
    sub->addCommand( "Enable Debugging", this, SLOT(onEnableDebug()),tr(OBN_ENDBG_SC), false );
    sub->addCommand( "Toggle Breakpoint", this, SLOT(onToggleBreakPt()), tr(OBN_TOGBP_SC), false);
    sub->addAction( d_dbgStepIn );
    sub->addAction( d_dbgBreak );
    sub->addAction( d_dbgContinue );
    sub->addAction( d_dbgAbort );

}

bool OberonIde::luaRuntimeMessage(const QByteArray& msg, const QString& file )
{
    // TODO use Engine2::decodeRuntimeMessage
    const int rbrack = msg.indexOf(']'); // cannot directly search for ':' because Windows "C:/"
    if( rbrack == -1 )
        return false;
    const int firstColon = msg.indexOf(':', rbrack);
    if( firstColon != -1 )
    {
        const int secondColon = msg.indexOf(':',firstColon + 1);
        if( secondColon != -1 )
        {
            QString path = msg.left(firstColon);
            const int firstTick = path.indexOf('"');
            if( firstTick != -1 )
            {
                const int secondTick = path.indexOf('"',firstTick+1);
                path = path.mid(firstTick+1,secondTick-firstTick-1);
            }else
                path.clear();
            const quint32 id = msg.mid(firstColon+1, secondColon - firstColon - 1 ).toInt(); // lua deliveres negative numbers
            const bool packed = RowCol::isPacked(id);
            const quint32 row = packed ? RowCol::unpackRow(id) : id;
            const quint32 col = packed ? RowCol::unpackCol(id) : 1;

            d_pro->getErrs()->error(Errors::Runtime, path.isEmpty() ? file : path, row, col, msg.mid(secondColon+1) );
            return true;
        }
    }
    return false;
    // qWarning() << "Unknown Lua error message format:" << msg;
}

static bool sortExList( const Ast::Expression* lhs, Ast::Expression* rhs )
{
    Ast::Module* lm = lhs->getModule();
    Ast::Module* rm = rhs->getModule();
    const QByteArray ln = lm ? lm->d_name : QByteArray();
    const QByteArray rn = rm ? rm->d_name : QByteArray();
    const quint32 ll = lhs->d_loc.packed();
    const quint32 rl = rhs->d_loc.packed();

    return ln < rn || (!(rn < ln) && ll < rl);
}

void OberonIde::fillXref()
{
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    if( edit == 0 )
    {
        d_xref->clear();
        d_xrefTitle->clear();
        return;
    }
    int line, col;
    edit->getCursorPosition( &line, &col );
    line += 1;
    col += 1;
    Ast::Expression* hitEx = d_pro->findSymbolBySourcePos(edit->getPath(), line, col);
    if( hitEx )
    {
        Ast::Named* hitSym = hitEx->getIdent();
        Q_ASSERT( hitSym != 0 );

        Ast::Model::ExpList exp = d_pro->getUsage(hitSym);

        Editor::ExList l1, l2;
        foreach( const Ast::Ref<Ast::Expression> e, exp )
        {
            l2 << e.data();
            Ast::Module* mod = e->getModule();
            if( mod && mod->d_file == edit->getPath() )
                l1 << e.data();
        }

        edit->markNonTerms(l1);

        std::sort( l2.begin(), l2.end(), sortExList );

        QFont f = d_xref->font();
        f.setBold(true);

        QString type;
        switch( hitSym->getTag() )
        {
        case Ast::Thing::T_Field:
            type = "Field";
            break;
        case Ast::Thing::T_Variable:
        case Ast::Thing::T_LocalVar:
            type = "Variable";
            break;
        case Ast::Thing::T_Parameter:
            type = "Parameter";
            break;
        case Ast::Thing::T_NamedType:
            type = "Type";
            break;
        case Ast::Thing::T_Const:
            type = "Const";
            break;
        case Ast::Thing::T_Import:
            type = "Import";
            break;
        case Ast::Thing::T_BuiltIn:
            type = "BuiltIn";
            break;
        case Ast::Thing::T_Procedure:
            type = "Procedure";
            break;
        case Ast::Thing::T_Module:
            type = "Module";
            break;
        }

        d_xrefTitle->setText(QString("%1 '%2'").arg(type).arg(hitSym->d_name.constData()));

        d_xref->clear();
        foreach( Ast::Expression* e, l2 )
        {
            Ast::Named* ident = e->getIdent();
            Ast::Module* mod = e->getModule();
            if( mod == 0 )
                continue;
            Q_ASSERT( ident != 0 && mod != 0 );
            QTreeWidgetItem* i = new QTreeWidgetItem(d_xref);
            i->setText( 0, QString("%1 (%2:%3%4)")
                        .arg(e->getModule()->d_name.constData())
                        .arg(e->d_loc.d_row).arg(e->d_loc.d_col)
                        .arg( ident->d_loc == e->d_loc ? " decl" : "" ));
            if( e == hitEx )
                i->setFont(0,f);
            i->setToolTip( 0, i->text(0) );
            i->setData( 0, Qt::UserRole, QVariant::fromValue( ExRef(e) ) );
            if( mod->d_file != edit->getPath() )
                i->setForeground( 0, Qt::gray );
        }
    }
}

void OberonIde::fillXref(Ast::Named* sym)
{
    d_xref->clear();
    d_xrefTitle->clear();

    Ast::Model::ExpList exp = d_pro->getUsage(sym);

    Editor::ExList l2;
    foreach( const Ast::Ref<Ast::Expression> e, exp )
    {
        l2 << e.data();
    }

    std::sort( l2.begin(), l2.end(), sortExList );

    d_xrefTitle->setText(sym->d_name);

    foreach( Ast::Expression* e, l2 )
    {
        Ast::Named* ident = e->getIdent();
        Ast::Module* mod = e->getModule();
        if( mod == 0 )
            continue;
        Q_ASSERT( ident != 0 && mod != 0 );
        QTreeWidgetItem* i = new QTreeWidgetItem(d_xref);
        i->setText( 0, e->getModule()->d_name );
        i->setToolTip( 0, i->text(0) );
        i->setData( 0, Qt::UserRole, QVariant::fromValue( ExRef(e) ) );
    }
}

void OberonIde::fillStack()
{
    d_stack->clear();
    Engine2::StackLevels ls = d_lua->getStackTrace();

    bool opened = false;
    for( int level = 0; level < ls.size(); level++ )
    {
        const Engine2::StackLevel& l = ls[level];
        // Level, Name, Pos, Mod
        QTreeWidgetItem* item = new QTreeWidgetItem(d_stack);
        item->setText(0,QString::number(l.d_level));
        item->setData(0,Qt::UserRole,l.d_level);
        item->setText(1,l.d_name);
        if( l.d_inC )
        {
            item->setText(3,"(native)");
        }else
        {
            const int row = RowCol::unpackRow2(l.d_line);
            const int col = RowCol::unpackCol2(l.d_line);
            item->setText(2,QString("%1:%2").arg(row).arg(col));
            item->setData(2, Qt::UserRole, l.d_line );
            item->setText(3, QFileInfo(l.d_source).baseName() );
            item->setData(3, Qt::UserRole, l.d_source );
            item->setToolTip(3, l.d_source );
            if( !opened )
            {
                showEditor(l.d_source, row, col, true );
                d_lua->setActiveLevel(level);
                opened = true;
            }
        }
    }

    d_stack->parentWidget()->show();
}

static void typeAddr( QTreeWidgetItem* item, const QVariant& val )
{
    if( val.canConvert<Lua::Engine2::VarAddress>() )
    {
        Lua::Engine2::VarAddress addr = val.value<Lua::Engine2::VarAddress>();
        if( addr.d_addr )
            item->setToolTip(1, QString("address 0x%1").arg(ptrdiff_t(addr.d_addr),8,16,QChar('0')));
        switch( addr.d_type )
        {
        case Engine2::LocalVar::NIL:
            item->setText(1, "nil");
            break;
        case Engine2::LocalVar::FUNC:
            item->setText(1, "func");
            break;
        case Engine2::LocalVar::TABLE:
            item->setText(1, "table");
            break;
        case Engine2::LocalVar::STRUCT:
            item->setText(1, "struct");
            break;
        }
    }else if( val.type() == QMetaType::QVariantMap)
    {
        QVariantMap map = val.toMap();
        typeAddr( item, map.value(QString()) );
    }
}

static void fillLocalSubs( QTreeWidgetItem* super, const QVariantMap& vals )
{
    QVariantMap::const_iterator i;
    for( i = vals.begin(); i != vals.end(); i++ )
    {
        if( i.key().isEmpty() )
            continue;
        QTreeWidgetItem* item = new QTreeWidgetItem(super);
        item->setText(0, i.key() );

        if( i.value().canConvert<Lua::Engine2::VarAddress>() )
        {
            typeAddr(item,i.value());
        }else if( i.value().type() == QMetaType::QVariantMap)
        {
            typeAddr(item,i.value());
            fillLocalSubs(item,i.value().toMap() );
        }else if( i.value().type() == QMetaType::QByteArray )
        {
            item->setText(1, "\"" + i.value().toString().simplified() + "\"");
            item->setToolTip(1, i.value().toString());
        }else
            item->setText(1,i.value().toString());
    }
}

void OberonIde::fillLocals()
{
    d_locals->clear();
    Engine2::LocalVars vs = d_lua->getLocalVars(true,2,50);
    foreach( const Engine2::LocalVar& v, vs )
    {
        QTreeWidgetItem* item = new QTreeWidgetItem(d_locals);
        QString name = v.d_name;
        if( v.d_isUv )
            name = "(" + name + ")";
        item->setText(0,name);
        if( v.d_value.canConvert<Lua::Engine2::VarAddress>() )
        {
            typeAddr(item,v.d_value);
        }else if( v.d_value.type() == QMetaType::QVariantMap )
        {
            typeAddr(item,v.d_value);
            fillLocalSubs(item,v.d_value.toMap() );
        }else if( JitBytecode::isString(v.d_value) )
        {
            item->setText(1, "\"" + v.d_value.toString().simplified() + "\"");
            item->setToolTip(1, v.d_value.toString() );
        }else if( !v.d_value.isNull() )
            item->setText(1,v.d_value.toString());
        else
        {
            switch( v.d_type )
            {
            case Engine2::LocalVar::NIL:
                item->setText(1, "nil");
                break;
            case Engine2::LocalVar::FUNC:
                item->setText(1, "func");
                break;
            case Engine2::LocalVar::TABLE:
                item->setText(1, "table");
                break;
            case Engine2::LocalVar::STRUCT:
                item->setText(1, "struct");
                break;
            case Engine2::LocalVar::STRING:
                item->setText(1, "\"" + v.d_value.toString().simplified() + "\"");
                break;
            default:
                break;
           }
        }
    }
}

void OberonIde::removePosMarkers()
{
    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* e = static_cast<Editor*>( d_tab->widget(i) );
        e->setPositionMarker(-1);
    }
}

void OberonIde::enableDbgMenu()
{
    d_dbgBreak->setEnabled(!d_lua->isWaiting() && d_lua->isExecuting() && d_lua->isDebug() );
    d_dbgAbort->setEnabled(d_lua->isWaiting());
    d_dbgContinue->setEnabled(d_lua->isWaiting());
    d_dbgStepIn->setEnabled(d_lua->isWaiting() && d_lua->isDebug() );
}

void OberonIde::handleGoBack()
{
    ENABLED_IF( d_backHisto.size() > 1 );

    d_pushBackLock = true;
    d_forwardHisto.push_back( d_backHisto.last() );
    d_backHisto.pop_back();
    showEditor( d_backHisto.last().d_file, d_backHisto.last().d_line+1, d_backHisto.last().d_col+1 );
    d_pushBackLock = false;
}

void OberonIde::handleGoForward()
{
    ENABLED_IF( !d_forwardHisto.isEmpty() );

    Location cur = d_forwardHisto.last();
    d_forwardHisto.pop_back();
    showEditor( cur.d_file, cur.d_line+1, cur.d_col+1 );
}

void OberonIde::onUpdateLocation(int line, int col)
{
    Editor* e = static_cast<Editor*>( sender() );
    e->clearBackHisto();
    pushLocation(Location(e->getPath(), line,col));
}

void OberonIde::onXrefDblClicked()
{
    QTreeWidgetItem* item = d_xref->currentItem();
    if( item )
    {
        ExRef e = item->data(0,Qt::UserRole).value<ExRef>();
        Q_ASSERT( !e.isNull() );
        showEditor( e->getModule()->d_file, e->d_loc.d_row, e->d_loc.d_col );
    }
}

void OberonIde::onToggleBreakPt()
{
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    ENABLED_IF( edit );

    quint32 line;
    const bool on = edit->toggleBreakPoint(&line);
    if( on )
        d_lua->addBreak( edit->getPath().toUtf8(), line + 1 );
    else
        d_lua->removeBreak( edit->getPath().toUtf8(), line + 1 );
}

void OberonIde::onSingleStep()
{
    // ENABLED_IF( d_lua->isWaiting() );

    d_lua->runToNextLine();
}

void OberonIde::onContinue()
{
    // ENABLED_IF( d_lua->isWaiting() );

    d_lua->runToBreakPoint();
}

void OberonIde::onShowLlBc()
{
    ENABLED_IF( d_bcv->topLevelItemCount() );

    BcViewer* bc = new BcViewer();
    QBuffer buf( &d_curBc );
    buf.open(QIODevice::ReadOnly);
    bc->loadFrom( &buf );
    bc->show();
    bc->setAttribute(Qt::WA_DeleteOnClose);
}

void OberonIde::onWorkingDir()
{
    ENABLED_IF(true);

    bool ok;
    const QString res = QInputDialog::getText(this,tr("Set Working Directory"), QString(), QLineEdit::Normal,
                                              d_pro->getWorkingDir(), &ok );
    if( !ok )
        return;
    d_pro->setWorkingDir(res);
}

void OberonIde::onLuaNotify(int messageType, QByteArray val1, int val2)
{
    switch( messageType )
    {
    case Engine2::Started:
    case Engine2::Continued:
    case Engine2::LineHit:
    case Engine2::BreakHit:
    case Engine2::ErrorHit:
    case Engine2::Finished:
    case Engine2::Aborted:
        enableDbgMenu();
        break;
    }
}

void OberonIde::pushLocation(const OberonIde::Location& loc)
{
    if( d_pushBackLock )
        return;
    if( !d_backHisto.isEmpty() && d_backHisto.last() == loc )
        return; // o ist bereits oberstes Element auf dem Stack.
    d_backHisto.removeAll( loc );
    d_backHisto.push_back( loc );
}

void OberonIde::onAbout()
{
    ENABLED_IF(true);

    QMessageBox::about( this, qApp->applicationName(),
      tr("<html>Release: %1   Date: %2<br><br>"

      "Welcome to the Oberon IDE.<br>"
      "See <a href=\"https://github.com/rochus-keller/Oberon\">"
         "here</a> for more information.<br><br>"

      "Author: Rochus Keller, me@rochus-keller.ch<br><br>"

      "Licese: <a href=\"https://www.gnu.org/licenses/license-list.html#GNUGPL\">GNU GPL v2 or v3</a>"
      "</html>" ).arg( qApp->applicationVersion() ).arg( QDateTime::currentDateTime().toString("yyyy-MM-dd") ));
}

void OberonIde::onQt()
{
    ENABLED_IF(true);
    QMessageBox::aboutQt(this,tr("About the Qt Framework") );
}

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    a.setOrganizationName("me@rochus-keller.ch");
    a.setOrganizationDomain("github.com/rochus-keller/Oberon");
    a.setApplicationName("Oberon IDE");
    a.setApplicationVersion("0.6.8");
    a.setStyle("Fusion");

    OberonIde w;

    if( a.arguments().size() > 1 )
        w.loadFile(a.arguments()[1] );

    return a.exec();
}
