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

#include "ObxIde.h"
#include "ObnHighlighter.h"
#include "ObFileCache.h"
#include "ObLjLib.h"
#include "ObxAst.h"
#include "ObxEvaluator.h"
#include "ObErrors.h"
#include "ObLjbcGen.h"
#include "ObxProject.h"
#include "ObxModel.h"
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
#include <QScrollBar>
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
using namespace Obx;
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

struct ScopeRef : public Ref<Scope>
{
    ScopeRef(Scope* s = 0):Ref(s) {}
};
Q_DECLARE_METATYPE(ScopeRef)
struct NamedRef : public Ref<Named>
{
    NamedRef(Named* s = 0):Ref(s) {}
};
Q_DECLARE_METATYPE(NamedRef)
struct ExRef : public Ref<Expression>
{
    ExRef(Expression* n = 0):Ref(n) {}
};
Q_DECLARE_METATYPE(ExRef)

class Ide::Editor : public CodeEditor
{
public:
    Editor(Ide* p, Project* pro):CodeEditor(p),d_pro(pro),d_ide(p)
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

    Ide* d_ide;
    Highlighter* d_hl;
    Project* d_pro;

    void setExt( bool on )
    {
        d_hl->setEnableExt(on);
        d_hl->addBuiltIn("WCHAR");
        d_hl->addBuiltIn("WCHR");
    }

    void clearBackHisto()
    {
        d_backHisto.clear();
    }


    typedef QList<Expression*> ExList;

    void markNonTerms(const ExList& syms)
    {
        d_nonTerms.clear();
        QTextCharFormat format;
        format.setBackground( QColor(237,235,243) );
        foreach( Expression* s, syms )
        {
            Named* ident = s->getIdent();
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
            d_ide->pushLocation( Ide::Location( getPath(), cur.blockNumber(), cur.positionInBlock(), verticalScrollBar()->value() ) );
            QApplication::restoreOverrideCursor();
            d_link.clear();
        }
        if( QApplication::keyboardModifiers() == Qt::ControlModifier )
        {
            QTextCursor cur = cursorForPosition(e->pos());
            Ref<Expression> e = d_pro->findSymbolBySourcePos(
                        getPath(),cur.blockNumber() + 1,cur.positionInBlock() + 1);
            if( e )
            {
                Named* sym = e->getIdent();
                d_ide->pushLocation( Ide::Location( getPath(), cur.blockNumber(), cur.positionInBlock(), verticalScrollBar()->value() ) );
                const int tag = sym->getTag();
                if( tag == Thing::T_Import && e->d_loc == sym->d_loc )
                    sym = cast<Import*>(sym)->d_mod.data();
                else if( tag == Thing::T_Procedure )
                {
                    Procedure* p = cast<Procedure*>(sym);
                    if( p->d_receiverRec && p->d_receiverRec->d_baseRec )
                    {
                        Named* n = p->d_receiverRec->d_baseRec->find(p->d_name,true);
                        if( n && n->getTag() == Thing::T_Procedure )
                            sym = n;
                    }
                }
                if( sym->getTag() == Thing::T_Module && cast<Module*>(sym)->d_synthetic )
                    d_ide->fillXref(sym);
                else
                    d_ide->showEditor( sym, false, true );
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
            Ref<Expression> e = d_pro->findSymbolBySourcePos(
                        getPath(),cur.blockNumber() + 1,cur.positionInBlock() + 1);
            const bool alreadyArrow = !d_link.isEmpty();
            d_link.clear();
            if( e )
            {
                Named* sym = e->getIdent();
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
#if 0 // TODO
        d_ide->compile();
        if( !d_nonTerms.isEmpty() && !d_pro->getErrs()->getErrors().isEmpty() )
        {
            d_nonTerms.clear();
            updateExtraSelections();
        }
#endif
    }
};

class Ide::DocTab : public DocTabWidget
{
public:
    DocTab(QWidget* p):DocTabWidget(p,false) {}

    // overrides
    bool isUnsaved(int i)
    {
        Ide::Editor* edit = static_cast<Ide::Editor*>( widget(i) );
        return edit->isModified();
    }

    bool save(int i)
    {
        Ide::Editor* edit = static_cast<Ide::Editor*>( widget(i) );
        if( !edit->saveToFile( edit->getPath(), false ) )
            return false;
        return true;
    }
};

class Ide::Debugger : public DbgShell
{
public:
    Ide* d_ide;
    Debugger(Ide* ide):d_ide(ide){}
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

static Ide* s_this = 0;
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
    QFile lib( QString(":/scripts/%1.lua").arg(name.constData()) );
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

Ide::Ide(QWidget *parent)
    : QMainWindow(parent),d_lock(false),d_filesDirty(false),d_pushBackLock(false),d_lock2(false),d_lock3(false),d_lock4(false)
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
    createMod();
    createHier();
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

Ide::~Ide()
{
    delete d_dbg;
}

void Ide::loadFile(const QString& path)
{
    QFileInfo info(path);

    if( info.isDir() && info.suffix() != ".obxpro" )
    {
        d_pro->initializeFromDir( path );
    }else
    {
        d_pro->loadFrom(path);
    }

    QDir::setCurrent(QFileInfo(path).absolutePath());

    onCaption();

    onCompile();
}

void Ide::logMessage(const QString& str, bool err)
{
    d_term->printText(str,err);
}

void Ide::closeEvent(QCloseEvent* event)
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

void Ide::createTerminal()
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

void Ide::createDumpView()
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

void Ide::createMods()
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

void Ide::createMod()
{
    QDockWidget* dock = new QDockWidget( tr("Module"), this );
    dock->setObjectName("Module");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable );
    QWidget* pane = new QWidget(dock);
    QVBoxLayout* vbox = new QVBoxLayout(pane);
    vbox->setMargin(0);
    vbox->setSpacing(0);
    d_modTitle = new QLabel(pane);
    d_modTitle->setMargin(2);
    d_modTitle->setWordWrap(true);
    vbox->addWidget(d_modTitle);
    d_mod = new QTreeWidget(dock);
    d_mod->setHeaderHidden(true);
    d_mod->setExpandsOnDoubleClick(false);
    d_mod->setAlternatingRowColors(true);
    vbox->addWidget(d_mod);
    dock->setWidget(pane);
    addDockWidget( Qt::LeftDockWidgetArea, dock );
    connect( d_mod, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)),this,SLOT(onModDblClicked(QTreeWidgetItem*,int)) );
}

void Ide::createHier()
{
    QDockWidget* dock = new QDockWidget( tr("Hierarchy"), this );
    dock->setObjectName("Hierarchy");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable );
    QWidget* pane = new QWidget(dock);
    QVBoxLayout* vbox = new QVBoxLayout(pane);
    vbox->setMargin(0);
    vbox->setSpacing(0);
    d_hierTitle = new QLabel(pane);
    d_hierTitle->setMargin(2);
    d_hierTitle->setWordWrap(true);
    vbox->addWidget(d_hierTitle);
    d_hier = new QTreeWidget(dock);
    d_hier->setHeaderHidden(true);
    d_hier->setExpandsOnDoubleClick(false);
    d_hier->setAlternatingRowColors(true);
    vbox->addWidget(d_hier);
    dock->setWidget(pane);
    addDockWidget( Qt::LeftDockWidgetArea, dock );
    connect( d_hier, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)),this,SLOT(onHierDblClicked(QTreeWidgetItem*,int)) );
}

void Ide::createErrs()
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

void Ide::createXref()
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

void Ide::createStack()
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

void Ide::createLocals()
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

void Ide::createMenu()
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

void Ide::createMenuBar()
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

void Ide::onCompile()
{
    ENABLED_IF(true);
    compile();
}

void Ide::onRun()
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
        if( !d_lua->addSourceLib( f.d_sourceCode, f.d_mod->d_name ) )
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

void Ide::onAbort()
{
    // ENABLED_IF( d_lua->isWaiting() );
    d_lua->terminate();
}

void Ide::onGenerate()
{
    ENABLED_IF(true);
    compile(true);
}

void Ide::onNewPro()
{
    ENABLED_IF(true);

    if( !checkSaved( tr("New Project")) )
        return;

    d_pro->createNew();
    d_tab->onCloseAll();
    compile();
}

void Ide::onOpenPro()
{
    ENABLED_IF( true );

    if( !checkSaved( tr("New Project")) )
        return;

    const QString fileName = QFileDialog::getOpenFileName(this, tr("Open Project"),QString(),
                                                          tr("Oberon+ Project (*.obxpro)") );
    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    d_tab->onCloseAll();
    d_pro->loadFrom(fileName);

    compile();
}

void Ide::onSavePro()
{
    ENABLED_IF( d_pro->isDirty() );

    if( !d_pro->getFilePath().isEmpty() )
        d_pro->save();
    else
        onSaveAs();
}

void Ide::onSaveFile()
{
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    ENABLED_IF( edit && edit->isModified() );

    edit->saveToFile( edit->getPath() );
    d_pro->getFc()->removeFile( edit->getPath() );
}

void Ide::onSaveAs()
{
    ENABLED_IF(true);

    QString fileName = QFileDialog::getSaveFileName(this, tr("Save Project"),
                                                          QFileInfo(d_pro->getFilePath()).absolutePath(),
                                                          tr("Oberon+ Project (*.obxpro)") );

    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    if( !fileName.endsWith(".obxpro",Qt::CaseInsensitive ) )
        fileName += ".obxpro";

    d_pro->saveTo(fileName);
    onCaption();
}

void Ide::onCaption()
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

void Ide::onGotoLnr(quint32 lnr)
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

void Ide::onFullScreen()
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

void Ide::onCursor()
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

void Ide::onExportBc()
{
    const QString curPath = d_tab->getCurrentDoc().toString();
    ENABLED_IF(d_tab->getCurrentTab() != 0 && !d_pro->getFiles().value(curPath).d_sourceCode.isEmpty() );

    QString fileName = QFileDialog::getSaveFileName(this, tr("Save Binary"), curPath, tr("*.ljbc") );

    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    if( !fileName.endsWith(".ljbc",Qt::CaseInsensitive ) )
        fileName += ".ljbc";
    QFile out(fileName);
    out.open(QIODevice::WriteOnly);
    out.write(d_pro->getFiles().value(curPath).d_sourceCode);
}

void Ide::onExportAsm()
{
    const QString curPath = d_tab->getCurrentDoc().toString();
    ENABLED_IF(d_tab->getCurrentTab() != 0 && !d_pro->getFiles().value(curPath).d_sourceCode.isEmpty() );

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

void Ide::onModsDblClicked(QTreeWidgetItem* item, int)
{
    ScopeRef s = item->data(0,Qt::UserRole).value<ScopeRef>();
    if( s.isNull() )
        return;

    showEditor( s.data() );
}

void Ide::onModDblClicked(QTreeWidgetItem* item, int)
{
    NamedRef s = item->data(0,Qt::UserRole).value<NamedRef>();
    if( s.isNull() )
        return;

    d_lock2 = true;
    const QString path = d_tab->getCurrentDoc().toString();
    item->setExpanded(true);
    showEditor( path, s->d_loc.d_row, s->d_loc.d_col, false, true );
    d_lock2 = false;
}

void Ide::onHierDblClicked(QTreeWidgetItem* item, int)
{
    NamedRef s = item->data(0,Qt::UserRole).value<NamedRef>();
    if( s.isNull() )
        return;

    d_lock4 = true;

    showEditor( s->getModule()->d_file, s->d_loc.d_row, s->d_loc.d_col, false, true );
    d_lock4 = false;
}

void Ide::onStackDblClicked(QTreeWidgetItem* item, int)
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

void Ide::onTabChanged()
{
    const QString path = d_tab->getCurrentDoc().toString();

    onEditorChanged();

    if( !path.isEmpty() )
    {
        d_curBc = d_pro->getFiles().value( path ).d_sourceCode;
        if( !d_curBc.isEmpty() )
        {
            QBuffer buf( &d_curBc );
            buf.open(QIODevice::ReadOnly);
            d_bcv->loadFrom(&buf);
            onCursor();
            return;
        }
    }
    fillModule(d_pro->getFiles().value(path).d_mod.data());
    // else
    d_bcv->clear();
}

void Ide::onTabClosing(int i)
{
    d_pro->getFc()->removeFile( d_tab->getDoc(i).toString() );
}

void Ide::onEditorChanged()
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

void Ide::onErrorsDblClicked()
{
    QTreeWidgetItem* item = d_errs->currentItem();
    showEditor( item->data(0, Qt::UserRole ).toString(),
                item->data(1, Qt::UserRole ).toInt(), item->data(2, Qt::UserRole ).toInt() );
}

static bool errorEntryLessThan(const Errors::Entry &s1, const Errors::Entry &s2)
{
    return qMakePair(s1.d_file,s1.d_line) < qMakePair(s2.d_file,s2.d_line);
}

void Ide::onErrors()
{
    d_errs->clear();
    QList<Errors::Entry> errs = d_pro->getErrs()->getErrors().toList();
    std::sort(errs.begin(), errs.end(), errorEntryLessThan );

    for( int i = 0; i < errs.size(); i++ )
    {
#if 0
        if( !errs[i].d_isErr)
            continue; // TEST
#endif
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

void Ide::onOpenFile()
{
    ENABLED_IF( d_mods->currentItem() );

    onModsDblClicked( d_mods->currentItem(), 0 );
}

void Ide::onOakwood()
{
    CHECKED_IF( true, d_pro->useBuiltInOakwood() );

    d_pro->setUseBuiltInOakwood( !d_pro->useBuiltInOakwood() );
    if( d_pro->useBuiltInOakwood() )
        d_pro->setUseBuiltInObSysInner(false);
}

void Ide::onObSysInner()
{
    CHECKED_IF( true, d_pro->useBuiltInObSysInner() );

    d_pro->setUseBuiltInObSysInner( !d_pro->useBuiltInObSysInner() );
    if( d_pro->useBuiltInObSysInner() )
        d_pro->setUseBuiltInOakwood(false);
}

void Ide::onAddFiles()
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

void Ide::onRemoveFile()
{
    ENABLED_IF( d_mods->currentItem() );

    ScopeRef s = d_mods->currentItem()->data(0,Qt::UserRole).value<ScopeRef>();
    if( s.isNull() )
        return;

    Module* m = s->getModule();
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

void Ide::onEnableDebug()
{
    CHECKED_IF( true, d_lua->isDebug() );

    d_lua->setDebug( !d_lua->isDebug() );
    enableDbgMenu();
}

void Ide::onBreak()
{
    // normal call because called during processEvent which doesn't seem to enable
    // the functions: ENABLED_IF( d_lua->isExecuting() );
    d_lua->runToNextLine();
}

bool Ide::checkSaved(const QString& title)
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
                const QString path = QFileDialog::getSaveFileName( this, title, QString(), "Oberon+ Project (*.obxpro)" );
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

bool Ide::compile(bool generate )
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
    const QTime start = QTime::currentTime();
    d_pro->recompile();
    qDebug() << "recompiled in" << start.msecsTo(QTime::currentTime()) << "[ms]";
    if( generate )
        d_pro->generate();
    onErrors();
    fillMods();
    onTabChanged();
    return errCount == d_pro->getErrs()->getErrCount();
}

static bool sortNamed( Named* lhs, Named* rhs )
{
    return lhs->d_name.toLower() < rhs->d_name.toLower();
}

static void fillScope( QTreeWidgetItem* p, Scope* s )
{
    //bool foundConst = false, foundType = false, foundVar = false;
    QList<Named*> sort;
    for( int j = 0; j < s->d_order.size(); j++ )
        sort << s->d_order[j].data();
    std::sort( sort.begin(), sort.end(), sortNamed );
    foreach( Named* n, sort )
    {
        if( n->getTag() == Thing::T_Procedure )
        {
            QTreeWidgetItem* item = new QTreeWidgetItem( p );
            item->setText(0, n->d_name + n->visibilitySymbol() );
            item->setData(0,Qt::UserRole, QVariant::fromValue(ScopeRef(cast<Scope*>(n))) );
            fillScope(item, cast<Scope*>(n) );
        }
    }
}

void Ide::fillMods()
{
    d_mods->clear();
    const Project::FileHash& files = d_pro->getFiles();
    Project::FileHash::const_iterator i;
    QList< Ref<Module> > temp;
    QList<Module*> sort;
    for( i = files.begin(); i != files.end(); ++i )
    {
        if( i.value().d_mod.isNull() )
        {
            Ref<Module> m = new Module();
            // TODO m->d_hasErrors;
            m->d_file = i.key();
            m->d_name = QFileInfo(i.key()).baseName().toUtf8();
            temp << m;
            sort << m.data();
        }else
            sort << i.value().d_mod.data();
    }
    std::sort( sort.begin(), sort.end(), sortNamed );
    foreach( Module* n, sort )
    {
        QTreeWidgetItem* item = new QTreeWidgetItem(d_mods);
        item->setText(0, n->d_name);
        item->setToolTip(0,n->d_file);
        item->setIcon(0, QPixmap(":/images/module.png") );
        item->setData(0,Qt::UserRole,QVariant::fromValue(ScopeRef( n ) ) );
        // fillScope( item, cast<Scope*>(n));
    }
}

void Ide::addTopCommands(Gui::AutoMenu* pop)
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

Ide::Editor* Ide::showEditor(const QString& path, int row, int col, bool setMarker, bool center )
{
    Project::File pf = d_pro->getFiles().value(path);
    //if( pf.d_mod.isNull() )
    //    return;

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

        if( pf.d_mod.isNull() )
            edit->setExt(true);
        else
            edit->setExt(pf.d_mod->d_isExt);
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
        edit->setCursorPosition( row-1, col-1, center );
        if( setMarker )
            edit->setPositionMarker(row-1);
    }
    edit->setFocus();
    return edit;
}

void Ide::showEditor(Named* n, bool setMarker, bool center)
{
    Module* mod = n->getModule();
    if( mod )
        showEditor( mod->d_file, n->d_loc.d_row, n->d_loc.d_col, setMarker, center );
}

void Ide::showEditor(const Ide::Location& loc)
{
    Editor* e = showEditor( loc.d_file, loc.d_line+1, loc.d_col+1 );
    if( e )
        e->verticalScrollBar()->setValue(loc.d_yoff);
}

void Ide::createMenu(Ide::Editor* edit)
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

void Ide::addDebugMenu(Gui::AutoMenu* pop)
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

bool Ide::luaRuntimeMessage(const QByteArray& msg, const QString& file )
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

static bool sortExList( const Expression* lhs, Expression* rhs )
{
    Module* lm = lhs->getModule();
    Module* rm = rhs->getModule();
    const QByteArray ln = lm ? lm->d_name : QByteArray();
    const QByteArray rn = rm ? rm->d_name : QByteArray();
    const quint32 ll = lhs->d_loc.packed();
    const quint32 rl = rhs->d_loc.packed();

    return ln < rn || (!(rn < ln) && ll < rl);
}

static const char* roleName( Expression* e )
{
    switch( e->getIdentRole() )
    {
    case DeclRole:
        return "Decl";
    case LhsRole:
        return "Lhs";
//    case RhsRole:
//        return "Rhs";
    case VarRole:
        return "Vpar";
    case CallRole:
        return "Call";
    case ImportRole:
        return "Impt";
    case ThisRole:
        return "This";
    case MethRole:
        return "Meth";
    case SuperRole:
        return "Base";
    default:
        break;
    }
    return "";
}

void Ide::fillXref()
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
    Expression* hitEx = d_pro->findSymbolBySourcePos(edit->getPath(), line, col);
    if( hitEx )
    {
        Named* hitSym = hitEx->getIdent();
        Q_ASSERT( hitSym != 0 );

        QTreeWidgetItem* mi = d_modIdx.value(hitSym);
        if( mi && !d_lock2 )
        {
            d_mod->scrollToItem(mi,QAbstractItemView::PositionAtCenter);
            mi->setExpanded(true);
            d_mod->setCurrentItem(mi);
        }
        fillHier(hitSym);

        ExpList exp = d_pro->getUsage(hitSym);

        Editor::ExList l1, l2;
        foreach( const Ref<Expression> e, exp )
        {
            l2 << e.data();
            Module* mod = e->getModule();
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
        case Thing::T_Field:
            type = "Field";
            break;
        case Thing::T_Variable:
        case Thing::T_LocalVar:
            type = "Variable";
            break;
        case Thing::T_Parameter:
            type = "Parameter";
            break;
        case Thing::T_NamedType:
            type = "Type";
            break;
        case Thing::T_Const:
            type = "Const";
            break;
        case Thing::T_Import:
            type = "Import";
            break;
        case Thing::T_BuiltIn:
            type = "BuiltIn";
            break;
        case Thing::T_Procedure:
            type = "Procedure";
            break;
        case Thing::T_Module:
            type = "Module";
            break;
        }

        d_xrefTitle->setText(QString("%1 '%2'").arg(type).arg(hitSym->d_name.constData()));

        d_xref->clear();
        QTreeWidgetItem* black = 0;
        foreach( Expression* e, l2 )
        {
            Named* ident = e->getIdent();
            Module* mod = e->getModule();
            if( mod == 0 )
                continue;
            Q_ASSERT( ident != 0 && mod != 0 );
            QTreeWidgetItem* i = new QTreeWidgetItem(d_xref);
            i->setText( 0, QString("%1 (%2:%3 %4)")
                        .arg(e->getModule()->d_name.constData())
                        .arg(e->d_loc.d_row).arg(e->d_loc.d_col)
                        .arg( roleName(e) ));
            if( e == hitEx )
            {
                i->setFont(0,f);
                black = i;
            }
            i->setToolTip( 0, i->text(0) );
            i->setData( 0, Qt::UserRole, QVariant::fromValue( ExRef(e) ) );
            if( mod->d_file != edit->getPath() )
                i->setForeground( 0, Qt::gray );
        }
        if( black && !d_lock3 )
        {
            d_xref->scrollToItem(black, QAbstractItemView::PositionAtCenter);
            d_xref->setCurrentItem(black);
        }
    }
}

void Ide::fillXref(Named* sym)
{
    d_xref->clear();
    d_xrefTitle->clear();

    ExpList exp = d_pro->getUsage(sym);

    Editor::ExList l2;
    foreach( const Ref<Expression> e, exp )
    {
        l2 << e.data();
    }

    std::sort( l2.begin(), l2.end(), sortExList );

    d_xrefTitle->setText(sym->d_name);

    foreach( Expression* e, l2 )
    {
        Named* ident = e->getIdent();
        Module* mod = e->getModule();
        if( mod == 0 )
            continue;
        Q_ASSERT( ident != 0 && mod != 0 );
        QTreeWidgetItem* i = new QTreeWidgetItem(d_xref);
        i->setText( 0, e->getModule()->d_name );
        i->setToolTip( 0, i->text(0) );
        i->setData( 0, Qt::UserRole, QVariant::fromValue( ExRef(e) ) );
    }
}

void Ide::fillStack()
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

void Ide::fillLocals()
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

static void fillModItems( QTreeWidgetItem* item, Named* n, Scope* p, Record* r, bool sort, QHash<Named*,QTreeWidgetItem*>& idx );

template<class T>
static void createModItem(T* parent, Named* n, bool nonbound, bool sort, QHash<Named*,QTreeWidgetItem*>& idx )
{
    if( n->d_type.isNull() )
        return;
    if( idx.contains(n) )
    {
        // qWarning() << "fillMod recursion at" << n->getModule()->d_file << n->d_loc.d_row << n->d_name;
        return; // can legally happen if record decl contains a typedef using record, as e.g. Meta.Item.ParamCallVal.IP
    }
    switch( n->getTag() )
    {
    case Thing::T_NamedType:
        switch( n->d_type->getTag() )
        {
        case Thing::T_Record:
            {
                QTreeWidgetItem* item = new QTreeWidgetItem(parent);
                Record* r = cast<Record*>(n->d_type.data());
                fillModItems(item,n, 0, r, sort, idx);
                if( r->d_baseRec )
                    item->setText(0, item->text(0) + " ⇑");
                if( !r->d_subRecs.isEmpty() )
                    item->setText(0, item->text(0) + QString(" ⇓%1").arg(r->d_subRecs.size()));
                item->setToolTip( 0, item->text(0) );
            }
            break;
        case Thing::T_Pointer:
            {
                Pointer* p = cast<Pointer*>(n->d_type.data());
                Type* t = p->d_to ? p->d_to->derefed() : 0;
                if( t && t->getTag() == Thing::T_Record )
                {
                    QTreeWidgetItem* item = new QTreeWidgetItem(parent);
                    Record* r = cast<Record*>(t);
                    fillModItems(item,n, 0, r, sort, idx );
                    if( r->d_baseRec )
                        item->setText(0, item->text(0) + " ⇑");
                    if( !r->d_subRecs.isEmpty() )
                        item->setText(0, item->text(0) + QString(" ⇓%1").arg(r->d_subRecs.size()));
                    item->setToolTip( 0, item->text(0) );
                }
            }
            break;
        }
        break;
    case Thing::T_Procedure:
        {
            Procedure* p = cast<Procedure*>(n);
            if( !nonbound || p->d_receiver.isNull())
            {
                QTreeWidgetItem* item = new QTreeWidgetItem(parent);
                fillModItems(item,n, p, 0, sort, idx);
                if( p->d_super )
                    item->setText(0, item->text(0) + " ⇑");
                if( !p->d_subs.isEmpty() )
                    item->setText(0, item->text(0) + QString(" ⇓%1").arg(p->d_subs.size()));
                item->setToolTip( 0, item->text(0) );
                }
        }
        break;
    }
}

template <class T>
static void walkModItems(T* parent, Scope* p, Record* r, bool sort, QHash<Named*,QTreeWidgetItem*>& idx)
{
    typedef QMap<QByteArray,Named*> Sort;
    if( p && sort)
    {
        Sort tmp;
        foreach( const Ref<Named>& n, p->d_order )
            tmp.insert( n->d_name.toLower(), n.data() );
        Sort::const_iterator i;
        for( i = tmp.begin(); i != tmp.end(); ++i )
            createModItem(parent,i.value(),true, sort, idx);
    }else if( p )
    {
        foreach( const Ref<Named>& n, p->d_order )
            createModItem(parent,n.data(),true, sort, idx);
    }
    if( r && sort )
    {
        Sort tmp;
        foreach( const Ref<Procedure>& n, r->d_methods )
            tmp.insert( n->d_name.toLower(), n.data() );
        Sort::const_iterator i;
        for( i = tmp.begin(); i != tmp.end(); ++i )
            createModItem(parent,i.value(),false, sort, idx);
    }else if( r )
    {
        foreach( const Ref<Procedure>& n, r->d_methods )
            createModItem(parent,n.data(),false, sort, idx);
    }
}

static void fillModItems( QTreeWidgetItem* item, Named* n, Scope* p, Record* r, bool sort, QHash<Named*,QTreeWidgetItem*>& idx )
{
    const bool pub = n->d_visibility >= Named::Private;
    item->setText(0,n->d_name);
    item->setData(0, Qt::UserRole, QVariant::fromValue(NamedRef(n)) );
    idx.insert(n,item);
    switch( n->getTag() )
    {
    case Thing::T_NamedType:
        if( r && r->d_baseRec == 0 && r->d_methods.isEmpty() )
            item->setIcon(0, QPixmap( pub ? ":/images/struct.png" : ":/images/struct_priv.png" ) );
        else
            item->setIcon(0, QPixmap( pub ? ":/images/class.png" : ":/images/class_priv.png" ) );
        break;
    case Thing::T_Procedure:
        item->setIcon(0, QPixmap( pub ? ":/images/func.png" : ":/images/func_priv.png" ) );
        break;
    }

    walkModItems(item,p,r,sort, idx);
}

void Ide::fillModule(Module* m)
{
    d_mod->clear();
    d_modIdx.clear();
    d_modTitle->clear();
    if( m == 0 )
        return;
    d_modTitle->setText( QString("'%1'").arg(m->d_name.constData()) );
    walkModItems(d_mod, m, 0, true, d_modIdx );
}

template<class T>
static QTreeWidgetItem* fillHierProc( T* parent, Procedure* p, Named* ref )
{
    QTreeWidgetItem* item = new QTreeWidgetItem(parent);
    Q_ASSERT( p->d_receiver && !p->d_receiver->d_type.isNull() );
    Q_ASSERT( p->d_receiver->d_type->getTag() == Thing::T_QualiType );
    item->setText(0, QString("%1.%2").arg(p->getModule()->d_name.constData())
                  .arg(cast<QualiType*>(p->d_receiver->d_type.data())->d_quali->getIdent()->d_name.constData()));
    item->setData(0, Qt::UserRole, QVariant::fromValue( NamedRef(p) ) );
    item->setIcon(0, QPixmap( p->d_visibility >= Named::ReadWrite ? ":/images/func.png" : ":/images/func_priv.png" ) );
    item->setToolTip(0,item->text(0));

    QTreeWidgetItem* ret = 0;
    foreach( Procedure* sub, p->d_subs )
    {
        QTreeWidgetItem* tmp = fillHierProc(item, sub, ref);
        if( tmp )
            ret = tmp;
    }
    if( ret == 0 && p == ref )
            ret = item;
    return ret;
}

template<class T>
static QTreeWidgetItem* fillHierClass( T* parent, Record* p, Record* ref )
{
    QTreeWidgetItem* item = new QTreeWidgetItem(parent);
    Named* name = p->d_ident;
    if( name == 0 && p->d_binding )
        name = p->d_binding->d_ident;
    Q_ASSERT( name != 0 );
    item->setText(0, QString("%1.%2").arg(name->getModule()->d_name.constData()).arg(name->d_name.constData()));
    item->setData(0, Qt::UserRole, QVariant::fromValue( NamedRef(name) ) );
    item->setIcon(0, QPixmap( name->d_visibility >= Named::ReadWrite ? ":/images/class.png" : ":/images/class_priv.png" ) );
    item->setToolTip(0,item->text(0));
    QTreeWidgetItem* ret = 0;
    foreach( Record* sub, p->d_subRecs )
    {
        QTreeWidgetItem* tmp = fillHierClass(item, sub, ref);
        if( tmp )
            ret = tmp;
    }
    if( ret == 0 && p == ref )
            ret = item;
    return ret;
}
void Ide::fillHier(Named* n)
{
    if( d_lock4 )
        return;
    d_hier->clear();
    d_hierTitle->clear();
    if( n == 0 )
        return;
    QFont f = d_hier->font();
    f.setBold(true);
    QTreeWidgetItem* ref = 0;
    switch( n->getTag() )
    {
    case Thing::T_NamedType:
        switch( n->d_type->getTag() )
        {
        case Thing::T_Record:
            {
                Record* r = cast<Record*>(n->d_type.data());
                Record* r0 = r;
                d_hierTitle->setText( QString("Inheritance of class '%1'").arg( n->d_name.constData() ) );
                while( r->d_baseRec )
                    r = r->d_baseRec;
                ref = fillHierClass( d_hier, r, r0 );
                Q_ASSERT( ref );
            }
            break;
        case Thing::T_Pointer:
            {
                Pointer* p = cast<Pointer*>(n->d_type.data());
                Type* t = p->d_to ? p->d_to->derefed() : 0;
                if( t && t->getTag() == Thing::T_Record )
                {
                    Record* r = cast<Record*>(t);
                    Record* r0 = r;
                    d_hierTitle->setText( QString("Inheritance of class '%1'").arg( n->d_name.constData() ) );
                    while( r->d_baseRec )
                        r = r->d_baseRec;
                    ref = fillHierClass( d_hier, r, r0 );
                    Q_ASSERT( ref );
                }
            }
            break;
        }
        break;
    case Thing::T_Procedure:
        {
            Procedure* p = cast<Procedure*>(n);
            if( p->d_receiver.isNull() )
                return;
            d_hierTitle->setText( QString("Overrides of procedure '%1'").arg( n->d_name.constData() ) );
            while( p->d_super )
                p = p->d_super;
            ref = fillHierProc( d_hier, p, n );
            Q_ASSERT( ref );
        }
        break;
    }
    d_hier->sortByColumn(0,Qt::AscendingOrder);
    if( ref )
    {
        ref->setFont(0,f);
        d_hier->expandAll();
        d_hier->scrollToItem(ref,QAbstractItemView::PositionAtCenter);
        d_hier->setCurrentItem(ref);
    }
}

void Ide::removePosMarkers()
{
    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* e = static_cast<Editor*>( d_tab->widget(i) );
        e->setPositionMarker(-1);
    }
}

void Ide::enableDbgMenu()
{
    d_dbgBreak->setEnabled(!d_lua->isWaiting() && d_lua->isExecuting() && d_lua->isDebug() );
    d_dbgAbort->setEnabled(d_lua->isWaiting());
    d_dbgContinue->setEnabled(d_lua->isWaiting());
    d_dbgStepIn->setEnabled(d_lua->isWaiting() && d_lua->isDebug() );
}

void Ide::handleGoBack()
{
    ENABLED_IF( d_backHisto.size() > 1 );

    d_pushBackLock = true;
    d_forwardHisto.push_back( d_backHisto.last() );
    d_backHisto.pop_back();
    showEditor( d_backHisto.last() );
    d_pushBackLock = false;
}

void Ide::handleGoForward()
{
    ENABLED_IF( !d_forwardHisto.isEmpty() );

    Location cur = d_forwardHisto.last();
    d_forwardHisto.pop_back();
    showEditor( cur );
}

void Ide::onUpdateLocation(int line, int col)
{
    Editor* e = static_cast<Editor*>( sender() );
    e->clearBackHisto();
    pushLocation(Location(e->getPath(), line,col,e->verticalScrollBar()->value()));
}

void Ide::onXrefDblClicked()
{
    QTreeWidgetItem* item = d_xref->currentItem();
    if( item )
    {
        ExRef e = item->data(0,Qt::UserRole).value<ExRef>();
        Q_ASSERT( !e.isNull() );
        d_lock3 = true;
        showEditor( e->getModule()->d_file, e->d_loc.d_row, e->d_loc.d_col, false, true );
        d_lock3 = false;
    }
}

void Ide::onToggleBreakPt()
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

void Ide::onSingleStep()
{
    // ENABLED_IF( d_lua->isWaiting() );

    d_lua->runToNextLine();
}

void Ide::onContinue()
{
    // ENABLED_IF( d_lua->isWaiting() );

    d_lua->runToBreakPoint();
}

void Ide::onShowLlBc()
{
    ENABLED_IF( d_bcv->topLevelItemCount() );

    BcViewer* bc = new BcViewer();
    QBuffer buf( &d_curBc );
    buf.open(QIODevice::ReadOnly);
    bc->loadFrom( &buf );
    bc->show();
    bc->setAttribute(Qt::WA_DeleteOnClose);
}

void Ide::onWorkingDir()
{
    ENABLED_IF(true);

    bool ok;
    const QString res = QInputDialog::getText(this,tr("Set Working Directory"), QString(), QLineEdit::Normal,
                                              d_pro->getWorkingDir(), &ok );
    if( !ok )
        return;
    d_pro->setWorkingDir(res);
}

void Ide::onLuaNotify(int messageType, QByteArray val1, int val2)
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

void Ide::pushLocation(const Ide::Location& loc)
{
    if( d_pushBackLock )
        return;
    if( !d_backHisto.isEmpty() && d_backHisto.last() == loc )
        return; // o ist bereits oberstes Element auf dem Stack.
    d_backHisto.removeAll( loc );
    d_backHisto.push_back( loc );
}

void Ide::onAbout()
{
    ENABLED_IF(true);

    QMessageBox::about( this, qApp->applicationName(),
      tr("<html>Release: %1   Date: %2<br><br>"

      "Welcome to the Oberon+ IDE.<br>"
      "See <a href=\"https://github.com/rochus-keller/Oberon\">"
         "here</a> for more information.<br><br>"

      "Author: Rochus Keller, me@rochus-keller.ch<br><br>"

      "Licese: <a href=\"https://www.gnu.org/licenses/license-list.html#GNUGPL\">GNU GPL v2 or v3</a>"
      "</html>" ).arg( qApp->applicationVersion() ).arg( QDateTime::currentDateTime().toString("yyyy-MM-dd") ));
}

void Ide::onQt()
{
    ENABLED_IF(true);
    QMessageBox::aboutQt(this,tr("About the Qt Framework") );
}

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    a.setOrganizationName("me@rochus-keller.ch");
    a.setOrganizationDomain("github.com/rochus-keller/Oberon");
    a.setApplicationName("Oberon+ IDE");
    a.setApplicationVersion("0.3");
    a.setStyle("Fusion");

    Ide w;

    if( a.arguments().size() > 1 )
        w.loadFile(a.arguments()[1] );

    return a.exec();
}
