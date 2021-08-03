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
#include "ObxAst.h"
#include "ObxEvaluator.h"
#include "ObErrors.h"
#include "ObxProject.h"
#include "ObxModel.h"
#include "ObxLibFfi.h"
#include "ObsDisplay.h"
#include "ObxLjbcGen.h"
#include "ObxLjRuntime.h"
#include <LjTools/Engine2.h>
#include <LjTools/Terminal2.h>
#include <LjTools/BcViewer2.h>
#include <LjTools/BcViewer.h>
#include <LjTools/LuaJitEngine.h>
#include <LjTools/LjBcDebugger.h>
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
#include <lua.hpp>
using namespace Obx;
using namespace Ob;

#ifdef Q_OS_MAC
#define OBN_BREAK_SC "SHIFT+F8"
#define OBN_ABORT_SC "CTRL+SHIFT+Y"
#define OBN_CONTINUE_SC "CTRL+Y"
#define OBN_STEPIN_SC "CTRL+SHIFT+I"
#define OBN_STEPOVER_SC "CTRL+SHIFT+O"
#define OBN_STEPOUT_SC "SHIFT+F11" // TODO
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
#define OBN_STEPOVER_SC "F10"
#define OBN_STEPOUT_SC "SHIFT+F11"
#define OBN_ENDBG_SC "F8"
#define OBN_TOGBP_SC "F9"
#define OBN_GOBACK_SC "ALT+Left"
#define OBN_GOFWD_SC "ALT+Right"
#define OBN_NEXTDOC_SC "CTRL+TAB"
#define OBN_PREVDOC_SC "CTRL+SHIFT+TAB"
#endif

struct ModRef : public Ref<Module>
{
    ModRef(Module* s = 0):Ref(s) {}
};
Q_DECLARE_METATYPE(ModRef)
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
    Editor(Ide* p, Project* pro):CodeEditor(p),d_pro(pro),d_ide(p),dbgRow(0),dbgCol(0)
    {
        setCharPerTab(3);
        setTypingLatency(400);
        setPaintIndents(false);
        d_hl = new Highlighter( document() );
        updateTabWidth();
        QSettings set;
        if( !set.contains("CodeEditor/Font") )
        {
            QFont monospace("Monospace",9);
            if( !monospace.exactMatch() )
            {
                monospace = QFont("DejaVu Sans Mono",9);
                monospace.setStyleName("Book");
            }
            setFont(monospace);
        }
    }

    ~Editor()
    {
    }

    Ide* d_ide;
    Highlighter* d_hl;
    Project* d_pro;
    int dbgRow, dbgCol;

    void setExt( bool on )
    {
        d_hl->setEnableExt(on);
        for( int i = BuiltIn::ABS; i < BuiltIn::MAXBUILTIN; i++ )
            d_hl->addBuiltIn(BuiltIn::s_typeName[i]);
        for( int i = Type::ANY; i < Type::SET; i++ )
            d_hl->addBuiltIn(BaseType::s_typeName[i]);
        d_hl->addBuiltIn("ANYREC");
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
            QTextCursor c( document()->findBlockByNumber( qMax(int(s->d_loc.d_row - 1),0)) );
            c.setPosition( c.position() + qMax(int(s->d_loc.d_col - 1), 0) );
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

        if( d_ide->d_rt->getLua()->isDebug() && d_ide->d_rt->getLua()->isExecuting() &&
                ( d_ide->d_rt->getLua()->getMode() == Lua::Engine2::RowColMode ||
                  d_ide->d_rt->getLua()->getMode() == Lua::Engine2::PcMode ) )
        {
            if( d_ide->d_rt->getLua()->getMode() == Lua::Engine2::RowColMode )
            {
                dbgRow = RowCol::unpackRow(d_ide->d_rt->getLua()->getCurRowCol())-1;
                dbgCol = RowCol::unpackCol(d_ide->d_rt->getLua()->getCurRowCol())-1;
            }
            QTextEdit::ExtraSelection line;
            line.format.setBackground(QColor(Qt::yellow));
            line.format.setUnderlineStyle(QTextCharFormat::SingleUnderline);
            line.format.setUnderlineColor(Qt::red);
            line.cursor = QTextCursor(document()->findBlockByNumber(dbgRow));
            line.cursor.setPosition(line.cursor.position() + dbgCol);
            line.cursor.select(QTextCursor::WordUnderCursor);
            sum << line;
        }

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
        markNonTerms(Editor::ExList());
#if 0 // TODO
        d_ide->compile();
        if( !d_nonTerms.isEmpty() && !d_rt->getPro()->getErrs()->getErrors().isEmpty() )
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

class Ide::Debugger : public Lua::DbgShell
{
public:
    Ide* d_ide;
    Debugger(Ide* ide):d_ide(ide){}
    void handleBreak( Lua::Engine2* lua, const QByteArray& source, quint32 line )
    {
        QByteArray msg = lua->getValueString(1).simplified();

        d_ide->enableDbgMenu();
        d_ide->fillStack();
        d_ide->fillLocals();

        msg = msg.mid(1,msg.size()-2); // remove ""
        if( !lua->isBreakHit() )
        {
            if( source == "=[C]" && !msg.startsWith('[') )
                msg = "[\"=[C]\"]:" + QByteArray::number(RowCol::unpackRow2(line)) + ":" + msg;
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
    void handleAliveSignal(Lua::Engine2* e)
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

static void log( const QString& msg )
{
    if( s_this )
        s_this->logMessage(msg);
}

Ide::Ide(QWidget *parent)
    : QMainWindow(parent),d_lock(false),d_filesDirty(false),d_pushBackLock(false),
      d_lock2(false),d_lock3(false),d_lock4(false)
{
    s_this = this;
    LibFfi::setSendToLog(log);

    d_rt = new LjRuntime(this);

    d_dbg = new Debugger(this);
    d_rt->getLua()->setDbgShell(d_dbg);
    // d_rt->getLua()->setAliveSignal(true); // reduces performance by factor 2 to 5
    connect( d_rt->getLua(), SIGNAL(onNotify(int,QByteArray,int)),this,SLOT(onLuaNotify(int,QByteArray,int)) );

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
    d_dbgStepOver = new QAction(tr("Step Over"),this);
    d_dbgStepOver->setShortcutContext(Qt::ApplicationShortcut);
    d_dbgStepOver->setShortcut(tr(OBN_STEPOVER_SC));
    addAction(d_dbgStepOver);
    connect( d_dbgStepOver, SIGNAL(triggered(bool)),this,SLOT(onStepOver()) );
    d_dbgStepOut = new QAction(tr("Step Out"),this);
    d_dbgStepOut->setShortcutContext(Qt::ApplicationShortcut);
    d_dbgStepOut->setShortcut(tr(OBN_STEPOUT_SC));
    addAction(d_dbgStepOut);
    connect( d_dbgStepOut, SIGNAL(triggered(bool)),this,SLOT(onStepOut()) );

    enableDbgMenu();

    createMods();
    createMod();
    createHier();
    createXref();
    createErrs();
    createDumpView();
    createLocals();
    createStack();
    createTerminal();
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


    connect( d_rt->getPro(),SIGNAL(sigRenamed()),this,SLOT(onCaption()) );
    connect( d_rt->getPro(),SIGNAL(sigModified(bool)),this,SLOT(onCaption()) );
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
        d_rt->getPro()->initializeFromDir( path );
    }else
    {
        d_rt->getPro()->loadFrom(path);
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
        d_rt->getLua()->terminate(true);
        // TODO SysInnerLib::quit();
    }
}

void Ide::createTerminal()
{
    QDockWidget* dock = new QDockWidget( tr("Terminal"), this );
    dock->setObjectName("Terminal");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    d_term = new Lua::Terminal2(dock, d_rt->getLua());
    dock->setWidget(d_term);
    addDockWidget( Qt::BottomDockWidgetArea, dock );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+C"), this, d_term, SLOT(onClear()) );
    connect( d_rt->getLua(), SIGNAL(onNotify(int,QByteArray,int)), dock, SLOT(show()) );
}

void Ide::createDumpView()
{
    QDockWidget* dock = new QDockWidget( tr("Bytecode"), this );
    dock->setVisible(false);
    dock->setObjectName("Bytecode");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    d_bcv = new Lua::BcViewer2(dock);
    dock->setWidget(d_bcv);
    addDockWidget( Qt::RightDockWidgetArea, dock );
    connect(d_bcv,SIGNAL(sigGotoLine(quint32)),this,SLOT(onGotoLnr(quint32)));

    Gui::AutoMenu* pop = new Gui::AutoMenu( d_bcv, true );
    pop->addCommand( "Run on LuaJIT", this, SLOT(onRun()), tr("CTRL+R"), false );
    addDebugMenu(pop);
    pop->addSeparator();
    pop->addCommand( "Show low level bytecode", this, SLOT(onShowLlBc()) );
    pop->addCommand( "Export current bytecode...", this, SLOT(onExportBc()) );
    pop->addCommand( "Show bytecode file...", this, SLOT(onShowBcFile()) );
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
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
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
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
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
    dock->setVisible(false);
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
    addDockWidget( Qt::RightDockWidgetArea, dock );
    connect( d_stack, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)),this,SLOT(onStackDblClicked(QTreeWidgetItem*,int)) );
}

void Ide::createLocals()
{
    QDockWidget* dock = new QDockWidget( tr("Locals"), this );
    dock->setVisible(false);
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
    addDockWidget( Qt::RightDockWidgetArea, dock );
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
    pop->addCommand( "New Module...", this, SLOT(onNewModule()), tr("CTRL+SHIFT+N"), false );
    pop->addCommand( "Add existing Modules...", this, SLOT(onAddFiles()) );
    pop->addCommand( "Remove Module...", this, SLOT(onRemoveFile()) );
    pop->addCommand( "Export minimized Module...", this, SLOT(onExpMod()) );
    pop->addSeparator();
    pop->addCommand( "Add Import Path...", this, SLOT(onAddDir()) );
    pop->addCommand( "Remove Import Path...", this, SLOT(onRemoveDir()) );
    pop->addSeparator();
    pop->addCommand( "Built-in Oakwood", this, SLOT(onOakwood()) );
    pop->addCommand( "Built-in Oberon Sys. Inner Mod.", this, SLOT(onObSysInner()) );
    pop->addCommand( "Oberon File System Root...", this, SLOT( onWorkingDir() ) );
    pop->addSeparator();
    pop->addCommand( "Compile", this, SLOT(onCompile()), tr("CTRL+T"), false );
    pop->addCommand( "Compile && Generate", this, SLOT(onGenerate()), tr("CTRL+SHIFT+T"), false );
    pop->addCommand( "JIT Enabled", this, SLOT(onJitEnabled()) );
    pop->addCommand( "Restart LuaJIT", this, SLOT(onRestartLua()), tr("CTRL+SHIFT+R"), false );
    pop->addCommand( "Run on LuaJIT", this, SLOT(onRun()), tr("CTRL+R"), false );
    addDebugMenu(pop);
    addTopCommands(pop);

    new Gui::AutoShortcut( tr("CTRL+O"), this, this, SLOT(onOpenPro()) );
    new Gui::AutoShortcut( tr("CTRL+N"), this, this, SLOT(onNewPro()) );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+N"), this, this, SLOT(onNewModule()) );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+S"), this, this, SLOT(onSavePro()) );
    new Gui::AutoShortcut( tr("CTRL+S"), this, this, SLOT(onSaveFile()) );
    new Gui::AutoShortcut( tr("CTRL+R"), this, this, SLOT(onRun()) );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+R"), this, this, SLOT(onRestartLua()) );
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
    pop->addCommand( "Export current bytecode...", this, SLOT(onExportBc()) );
    pop->addCommand( "Export all bytecode...", this, SLOT(onExportAllBc()) );
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
    pop->addCommand( "New Module...", this, SLOT(onNewModule()), tr("CTRL+SHIFT+N"), false );
    pop->addCommand( "Add existing Modules...", this, SLOT(onAddFiles()) );
    pop->addCommand( "Remove Module...", this, SLOT(onRemoveFile()) );
    pop->addSeparator();
    pop->addCommand( "Built-in Oakwood", this, SLOT(onOakwood()) );
    pop->addCommand( "Built-in Oberon System Inner Modules", this, SLOT(onObSysInner()) );
    pop->addCommand( "Oberon File System Root...", this, SLOT( onWorkingDir() ) );

    pop = new Gui::AutoMenu( tr("Build && Run"), this );
    pop->addCommand( "Compile", this, SLOT(onCompile()), tr("CTRL+T"), false );
    pop->addCommand( "Compile && Generate", this, SLOT(onGenerate()), tr("CTRL+SHIFT+T"), false );
    pop->addCommand( "JIT Enabled", this, SLOT(onJitEnabled()) );
    pop->addCommand( "Restart LuaJIT", this, SLOT(onRestartLua()), tr("CTRL+SHIFT+R"), false );
    pop->addCommand( "Run on LuaJIT", this, SLOT(onRun()), tr("CTRL+R"), false );

    pop = new Gui::AutoMenu( tr("Debug"), this );
    pop->addCommand( "Enable Debugging", this, SLOT(onEnableDebug()),tr(OBN_ENDBG_SC), false );
    pop->addCommand( "Row/Column mode", this, SLOT(onRowColMode()) );
    pop->addCommand( "Bytecode mode", this, SLOT(onBcDebug()) );
    pop->addCommand( "Toggle Breakpoint", this, SLOT(onToggleBreakPt()), tr(OBN_TOGBP_SC), false);
    pop->addAction( d_dbgStepIn );
    pop->addAction( d_dbgStepOver );
    pop->addAction( d_dbgStepOut );
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
    ENABLED_IF( !d_rt->getPro()->getFiles().isEmpty() && !d_rt->getLua()->isExecuting() );

    const bool res = d_rt->run();
    removePosMarkers();
    if( !res )
        onErrors();
}

void Ide::onAbort()
{
    // ENABLED_IF( d_rt->getLua()->isWaiting() );
    d_rt->getLua()->terminate();
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

    // we need a path up front because this path is also the first root path to the source code
    QString fileName = QFileDialog::getSaveFileName(this, tr("New Project"),
                                                          QFileInfo(d_rt->getPro()->getProjectPath()).absolutePath(),
                                                          tr("Oberon+ Project (*.obxpro)") );

    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    if( !fileName.endsWith(".obxpro",Qt::CaseInsensitive ) )
        fileName += ".obxpro";

    d_rt->getPro()->createNew();
    d_tab->onCloseAll();
    compile();


    d_rt->getPro()->saveTo(fileName);

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
    clear();
    d_rt->getPro()->loadFrom(fileName);

    compile();
}

void Ide::onSavePro()
{
    ENABLED_IF( d_rt->getPro()->isDirty() );

    if( !d_rt->getPro()->getProjectPath().isEmpty() )
        d_rt->getPro()->save();
    else
        onSaveAs();
}

void Ide::onSaveFile()
{
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    ENABLED_IF( edit && edit->isModified() );

    edit->saveToFile( edit->getPath() );
    d_rt->getPro()->getFc()->removeFile( edit->getPath() );
}

void Ide::onSaveAs()
{
    ENABLED_IF(true);

    QString fileName = QFileDialog::getSaveFileName(this, tr("Save Project"),
                                                          QFileInfo(d_rt->getPro()->getProjectPath()).absolutePath(),
                                                          tr("Oberon+ Project (*.obxpro)") );

    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    if( !fileName.endsWith(".obxpro",Qt::CaseInsensitive ) )
        fileName += ".obxpro";

    d_rt->getPro()->saveTo(fileName);
    onCaption();
}

void Ide::onCaption()
{
    const QString star = d_rt->getPro()->isDirty() || d_filesDirty ? "*" : "";
    if( d_rt->getPro()->getProjectPath().isEmpty() )
    {
        setWindowTitle(tr("<unnamed>%2 - %1").arg(qApp->applicationName()).arg(star));
    }else
    {
        QFileInfo info(d_rt->getPro()->getProjectPath());
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
        {
            const int row = RowCol::unpackRow(lnr)-1;
            const int col = RowCol::unpackCol(lnr)-1;
            edit->setCursorPosition(row,col);
        }else
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
        int row, col;
        edit->getCursorPosition(&row,&col);
        d_bcv->gotoLine(RowCol(row+1,col+1).packed());
    }
    d_lock = false;
}

void Ide::onExportBc()
{
    ENABLED_IF(d_tab->getCurrentTab() != 0 );

    const QString dirPath = QFileDialog::getExistingDirectory(this, tr("Save Binary") );

    if (dirPath.isEmpty())
        return;

    QDir dir(dirPath);

    const QString curPath = d_tab->getCurrentDoc().toString();

    LjRuntime::BytecodeList l = d_rt->findByteCode(curPath);
    for( int i = 0; i < l.size(); i++ )
    {
        QString path = dir.absoluteFilePath(l[i].first->getName() + ".lua");
        QFile out(path);
        out.open(QIODevice::WriteOnly);
        out.write(l[i].second);
    }
    if( l.isEmpty() )
        QMessageBox::warning(this,tr("Export Bytecode"), tr("No bytecode was found for given module") );
}

void Ide::onExportAllBc()
{
    ENABLED_IF( d_rt->hasBytecode() && !d_rt->hasBuildErrors() );

    const QString dirPath = QFileDialog::getExistingDirectory(this, tr("Save Bytecode") );

    if (dirPath.isEmpty())
        return;

    d_rt->saveBytecode(dirPath,".lua");
}

void Ide::onModsDblClicked(QTreeWidgetItem* item, int)
{
    ModRef s = item->data(0,Qt::UserRole).value<ModRef>();
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
    item->setExpanded(true);
    d_lock4 = false;
}

void Ide::onStackDblClicked(QTreeWidgetItem* item, int)
{
    if( item )
    {
        const QString source = item->data(3,Qt::UserRole).toString();
        if( d_rt->getLua()->getMode() == Lua::Engine2::PcMode )
        {
            if( !source.isEmpty() )
            {
                const quint32 line = item->data(2,Qt::UserRole).toUInt();
                const quint32 func = item->data(1,Qt::UserRole).toUInt();
                showEditor(source, RowCol::unpackRow2(func), RowCol::unpackCol2(func), false );
                d_bcv->parentWidget()->show();
                d_bcv->gotoFuncPc(func,line, false, false);
            }
        }else
        {
            if( !source.isEmpty() )
            {
                const quint32 line = item->data(2,Qt::UserRole).toUInt();
                showEditor( source, RowCol::unpackRow(line), RowCol::unpackCol(line) );
            }
        }
        const int level = item->data(0,Qt::UserRole).toInt();
        d_rt->getLua()->setActiveLevel(level);
        fillLocals();
   }
}

void Ide::onTabChanged()
{
    const QString path = d_tab->getCurrentDoc().toString();

    onEditorChanged();

    if( !path.isEmpty() )
    {
        Project::FileMod f = d_rt->getPro()->findFile(path);
        if( f.first )
        {
            fillModule(f.first->d_mod.data());
            showBc(d_rt->findByteCode(f.second));
            onCursor();
            return;
        }
    }
    // else
    d_bcv->clear();
    fillModule(0);
}

void Ide::onTabClosing(int i)
{
    d_rt->getPro()->getFc()->removeFile( d_tab->getDoc(i).toString() );
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
        const QString path = d_tab->getDoc(i).toString();
        Project::FileMod f = d_rt->getPro()->findFile(path);
        QString name;
        if( f.first && f.first->d_mod )
            name = f.first->d_mod->getName();
        else
            name = QFileInfo( path ).fileName();
        d_tab->setTabText(i, name + ( e->isModified() ? "*" : "" ) );
        d_tab->setTabToolTip( i, path );
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
    if( s1.d_file < s2.d_file )
        return true;
    if( s1.d_file > s2.d_file )
        return false;
    if( s1.d_line < s2.d_line )
        return true;
    if( s1.d_line > s2.d_line )
        return false;
    if( s1.d_col < s2.d_col )
        return true;
    if( s1.d_col > s2.d_col )
        return false;
    return s1.d_nr < s2.d_nr;
}

void Ide::onErrors()
{
    d_errs->clear();
    QList<Errors::Entry> errs = d_rt->getPro()->getErrs()->getErrors().toList();
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
        Project::FileMod f = d_rt->getPro()->findFile(errs[i].d_file);
        if( f.first )
            item->setText(0, f.second->getName() );
        else
            item->setText(0, QFileInfo(errs[i].d_file).completeBaseName() );
        item->setToolTip(0, errs[i].d_file );
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
    CHECKED_IF( true, d_rt->getPro()->useBuiltInOakwood() );

    d_rt->getPro()->setUseBuiltInOakwood( !d_rt->getPro()->useBuiltInOakwood() );
    if( d_rt->getPro()->useBuiltInOakwood() )
        d_rt->getPro()->setUseBuiltInObSysInner(false);
}

void Ide::onObSysInner()
{
    CHECKED_IF( true, d_rt->getPro()->useBuiltInObSysInner() );

    d_rt->getPro()->setUseBuiltInObSysInner( !d_rt->getPro()->useBuiltInObSysInner() );
    if( d_rt->getPro()->useBuiltInObSysInner() )
        d_rt->getPro()->setUseBuiltInOakwood(false);
}

void Ide::onAddFiles()
{
    ENABLED_IF(true);

    QByteArrayList path;

    QList<QTreeWidgetItem*> sel = d_mods->selectedItems();
    if( sel.size() == 1 && sel.first()->type() == 1 )
        path = sel.first()->text(0).toLatin1().split('.');

    QString filter;
    foreach( const QString& suf, d_rt->getPro()->getSuffixes() )
        filter += " *" + suf;
    const QStringList files = QFileDialog::getOpenFileNames(this,tr("Add Modules"),QString(),filter );
    foreach( const QString& f, files )
    {
        if( !d_rt->getPro()->addFile(f,path) )
            qWarning() << "cannot add module" << f;
    }
    compile();
}

void Ide::onNewModule()
{
    ENABLED_IF(true);

    QByteArrayList path;

    QList<QTreeWidgetItem*> sel = d_mods->selectedItems();
    if( sel.size() == 1 && sel.first()->type() == 1 )
        path = sel.first()->text(0).toLatin1().split('.');

    const QByteArray name = QInputDialog::getText(this,tr("New Module"), tr("Enter a unique module name:") ).toLatin1();
    if( name.isEmpty() )
        return;

    if( !Lexer::isValidIdent(name) )
    {
        QMessageBox::critical(this,tr("New Module"), tr("'%1' is not a valid module name").arg(name.constData()) );
        return;
    }
    QDir dir;
    Project::FileGroup fg = d_rt->getPro()->findFileGroup(path);
    for( int i = 0; i < fg.d_files.size(); i++ )
    {
        if( i == 0 )
            dir = fg.d_files[i]->d_filePath;
        if( !fg.d_files[i]->d_mod.isNull() && fg.d_files[i]->d_mod->d_name == name )
        {
            QString where;
            if( !path.isEmpty() )
                where = " in " + path.join('.');
            QMessageBox::critical(this,tr("New Module"), tr("'%1' is not unique%2")
                                  .arg(name.constData()).arg(where) );
            return;
        }
    }

    QString filePath = QFileDialog::getSaveFileName(this,tr("New Module"), dir.absoluteFilePath(name + ".obx"),"*.obx");
    if( filePath.isEmpty() )
        return;

    if( !filePath.toLower().endsWith(".obx") )
        filePath += ".obx";

    QFile f(filePath);
    if( !f.open(QIODevice::WriteOnly) )
    {
        QMessageBox::critical(this,tr("New Module"), tr("Cannot open file for writing: '%1'").arg(filePath) );
        return;
    }
    f.write("module ");
    f.write(name);
    f.write("\n\n\n");
    f.write("end ");
    f.write(name);
    f.write("\n");
    f.close();

    if( !d_rt->getPro()->addFile(filePath,path) )
        qWarning() << "cannot add module" << filePath;
    compile();
}

void Ide::onAddDir()
{
    ENABLED_IF(true);

    bool ok;
    const QByteArray path = QInputDialog::getText(this,tr("Add Inport Path"), tr("Idents separated by '.':"),
                                                  QLineEdit::Normal, QString(), &ok ).toLatin1();
    if( !ok )
        return;
    if( !path.isEmpty() && !::isalpha( path[0] ) )
    {
        QMessageBox::critical(this,tr("Add Inport Path"),tr("import path starts with invalid character: %1").arg(path[0]));
        return;
    }
    for( int i = 1; i < path.size(); i++ )
    {
        const char ch = path[i];
        if( !::isalnum(ch) && ch != '.' )
        {
            QMessageBox::critical(this,tr("Add Inport Path"),tr("invalid character in import path: %1").arg(ch));
            return;
        }
    }
    QByteArrayList segments = path.split('.');
    for( int i = 0; i < segments.size(); i++ )
    {
        if( segments[i].isEmpty() )
        {
            QMessageBox::critical(this,tr("Add Inport Path"),tr("identifier cannot be empty"));
            return;
        }
    }
    d_rt->getPro()->addPackagePath(segments);
    fillMods();
}

void Ide::onRemoveFile()
{
    ENABLED_IF( d_mods->currentItem() && d_mods->currentItem()->type() == 0 );

    ModRef s = d_mods->currentItem()->data(0,Qt::UserRole).value<ModRef>();
    if( s.isNull() )
        return;

    Module* m = s->getModule();
    if( m == 0 )
        return;

    if( QMessageBox::warning( this, tr("Remove Module"),
                              tr("Do you really want to remove module '%1' from project?").arg(m->d_name.constData()),
                           QMessageBox::Yes | QMessageBox::Cancel, QMessageBox::Yes ) != QMessageBox::Yes )
        return;
    if( !d_rt->getPro()->removeFile( m->d_file ) )
        qWarning() << "cannot remove module" << m->d_name;
    else
        compile();
}

void Ide::onRemoveDir()
{
    ENABLED_IF( d_mods->currentItem() && d_mods->currentItem()->type() == 1
                && d_mods->currentItem()->childCount() == 0 );

    if( QMessageBox::warning( this, tr("Remove Import Path"),
                              tr("Do you really want to remove '%1' from project?").arg(d_mods->currentItem()->text(0)),
                           QMessageBox::Yes | QMessageBox::Cancel, QMessageBox::Yes ) != QMessageBox::Yes )
        return;
    QByteArrayList path = d_mods->currentItem()->text(0).toLatin1().split('.');
    if( !d_rt->getPro()->removePackagePath( path ) )
        qWarning() << "cannot remove import path" << d_mods->currentItem()->text(0);
    fillMods();
}

void Ide::onEnableDebug()
{
    CHECKED_IF( true, d_rt->getLua()->isDebug() );

    d_rt->getLua()->setDebug( !d_rt->getLua()->isDebug() );
    enableDbgMenu();
}

void Ide::onBreak()
{
    // normal call because called during processEvent which doesn't seem to enable
    // the functions: ENABLED_IF( d_rt->getLua()->isExecuting() );
    d_rt->getLua()->runToNextLine();
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
    if( d_rt->getPro()->isDirty() )
    {
        switch( QMessageBox::critical( this, title, tr("The the project has not been saved; do you want to save it?"),
                               QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel, QMessageBox::Yes ) )
        {
        case QMessageBox::Yes:
            if( !d_rt->getPro()->getProjectPath().isEmpty() )
                return d_rt->getPro()->save();
            else
            {
                const QString path = QFileDialog::getSaveFileName( this, title, QString(), "Oberon+ Project (*.obxpro)" );
                if( path.isEmpty() )
                    return false;
                QDir::setCurrent(QFileInfo(path).absolutePath());
                return d_rt->getPro()->saveTo(path);
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

bool Ide::compile(bool doGenerate )
{
    for( int i = 0; i < d_tab->count(); i++ )
    {
        Editor* e = static_cast<Editor*>( d_tab->widget(i) );
        if( e->isModified() )
            d_rt->getPro()->getFc()->addFile( e->getPath(), e->toPlainText().toUtf8() );
        else
            d_rt->getPro()->getFc()->removeFile( e->getPath() );
    }
    const bool res = d_rt->compile(doGenerate);
    onErrors();
    fillMods();
    fillModule(0);
    fillHier(0);
    fillXref();
    onTabChanged();
    return true;
}

static bool sortNamed( Named* lhs, Named* rhs )
{
    return lhs->d_name.toLower() < rhs->d_name.toLower();
}

typedef QPair<QByteArray,QList<Project::File*> > Group;

static bool sortNamed1( const Group& lhs, const Group& rhs )
{
    return lhs.first.toLower() < rhs.first.toLower();
}

template<class T>
static void fillModTree( T* parent, const QList<Module*>& mods )
{
    foreach( Module* m, mods )
    {
        QTreeWidgetItem* item = new QTreeWidgetItem(parent);
        item->setText(0, m->d_name);
        item->setToolTip(0,m->d_file);
        if( m->d_isDef )
            item->setIcon(0, QPixmap(":/images/definition.png") );
        else
            item->setIcon(0, QPixmap(":/images/module.png") );
        item->setData(0,Qt::UserRole,QVariant::fromValue(ModRef( m ) ) );
    }
}

void Ide::fillMods()
{
    d_mods->clear();

    const Project::FileGroups& paths = d_rt->getPro()->getFileGroups();
    typedef QList<Group> Sort1;
    Sort1 sort1;
    foreach( const Project::FileGroup& fg, paths )
        sort1.append( qMakePair( fg.d_package.join('.'), fg.d_files ) );
    std::sort( sort1.begin(), sort1.end(), sortNamed1 );

    for( int j = 0; j < sort1.size(); j++ )
    {
        QTreeWidgetItem* item = 0;
        if( !sort1[j].first.isEmpty() )
        {
            item = new QTreeWidgetItem(d_mods,1);
            item->setText(0, sort1[j].first);
            item->setToolTip( 0, item->text(0) );
            item->setIcon(0, QPixmap(":/images/folder.png") );
        }

        const QList<Project::File*>& files = sort1[j].second;
        QList< Ref<Module> > temp;
        QList<Module*> sort;
        for( int i = 0; i < files.size(); i++ )
        {
            if( files[i]->d_mod.isNull() )
            {
                Ref<Module> m = new Module();
                m->d_file = files[i]->d_filePath;
                m->d_name = QFileInfo(files[i]->d_filePath).baseName().toUtf8();
                temp << m;
                sort << m.data();
            }else
                sort << files[i]->d_mod.data();
        }
        std::sort( sort.begin(), sort.end(), sortNamed );
        if( item )
            fillModTree(item,sort);
        else
            fillModTree( d_mods, sort );
    }
    d_mods->expandAll();
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
    QString filePath = path;
    Project::FileMod f = d_rt->getPro()->findFile(path);
    if( f.first != 0 )
        filePath = f.first->d_filePath;

    const int i = d_tab->findDoc(filePath);
    Editor* edit = 0;
    if( i != -1 )
    {
        d_tab->setCurrentIndex(i);
        edit = static_cast<Editor*>( d_tab->widget(i) );
    }else
    {
        edit = new Editor(this,d_rt->getPro());
        createMenu(edit);

        connect(edit, SIGNAL(modificationChanged(bool)), this, SLOT(onEditorChanged()) );
        connect(edit,SIGNAL(cursorPositionChanged()),this,SLOT(onCursor()));
        connect(edit,SIGNAL(sigUpdateLocation(int,int)),this,SLOT(onUpdateLocation(int,int)));

        if( f.second == 0 )
            edit->setExt(true);
        else
            edit->setExt(f.second->d_isExt);
        edit->loadFromFile(filePath);

        if( f.first && f.first->d_mod )
        {
            const Lua::Engine2::Breaks& br = d_rt->getLua()->getBreaks( f.first->d_mod->getName() );
            Lua::Engine2::Breaks::const_iterator j;
            for( j = br.begin(); j != br.end(); ++j )
                edit->addBreakPoint((*j) - 1);
        }

        d_tab->addDoc(edit,filePath);
        onEditorChanged();
    }
    showBc( d_rt->findByteCode(f.second) );
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
    pop->addCommand( "Export current bytecode...", this, SLOT(onExportBc()) );
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
    pop->addAction( d_dbgStepOver );
    pop->addAction( d_dbgStepOut );
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

            d_rt->getPro()->getErrs()->error(Errors::Runtime, path.isEmpty() ? file : path, row, col, msg.mid(secondColon+1) );
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
    case StringRole:
        return "Str";
    default:
        break;
    }
    return "";
}

static Named* adjustForModIdx( Named* sym )
{
    while( sym )
    {
        switch( sym->getTag() )
        {
        case Thing::T_Procedure:
        case Thing::T_Module:
            return sym;
        }
        sym = sym->d_scope;
    }
    return 0;
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
    Scope* scope = 0;
    Expression* hitEx = d_rt->getPro()->findSymbolBySourcePos(edit->getPath(), line, col, &scope);
    if( hitEx )
    {
        Named* hitSym = hitEx->getIdent();
        Q_ASSERT( hitSym != 0 );

        QTreeWidgetItem* mi = d_modIdx.value(hitSym);
        if( mi == 0 )
            mi = d_modIdx.value(adjustForModIdx(scope));
        if( mi && !d_lock2 )
        {
            d_mod->scrollToItem(mi,QAbstractItemView::PositionAtCenter);
            mi->setExpanded(true);
            d_mod->setCurrentItem(mi);
        }
        fillHier(hitSym);

        ExpList exp = d_rt->getPro()->getUsage(hitSym);

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
        QString name = hitSym->getName();
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
        case Thing::T_GenericName:
            type = "Type Parameter";
            break;
        case Thing::T_Enumeration:
            type = "Enumeration";
            break;
        }

        d_xrefTitle->setText(QString("%1 '%2'").arg(type).arg(name));

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
                        .arg(e->getModule()->getName().constData())
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
    }else
        edit->markNonTerms(Editor::ExList());
}

void Ide::fillXref(Named* sym)
{
    d_xref->clear();
    d_xrefTitle->clear();

    if( sym == 0 )
        return;

    ExpList exp = d_rt->getPro()->getUsage(sym);

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
        i->setText( 0, e->getModule()->getName() );
        i->setToolTip( 0, i->text(0) );
        i->setData( 0, Qt::UserRole, QVariant::fromValue( ExRef(e) ) );
    }
}

void Ide::fillStack()
{
    d_stack->clear();

    Lua::Engine2::StackLevels ls = d_rt->getLua()->getStackTrace();
    d_scopes = QVector<Scope*>(ls.size());

    bool opened = false;
    for( int level = 0; level < ls.size(); level++ )
    {
        const Lua::Engine2::StackLevel& l = ls[level];
        // Level, Name, Pos, Mod
        QTreeWidgetItem* item = new QTreeWidgetItem(d_stack);
        item->setText(0,QString::number(l.d_level));
        item->setData(0,Qt::UserRole,l.d_level);
        item->setText(1,l.d_name);
        if( l.d_inC )
        {
            item->setText(3,"(native)");
        }else if( d_rt->getLua()->getMode() != Lua::Engine2::PcMode )
        {
            const int row = RowCol::unpackRow2(l.d_line);
            const int col = RowCol::unpackCol2(l.d_line);
            const int row2 = RowCol::unpackRow2(l.d_lineDefined);
            const int col2 = RowCol::unpackCol2(l.d_lineDefined);
            Project::FileMod fm = d_rt->getPro()->findFile( l.d_source );
            Expression* e = d_rt->getPro()->findSymbolBySourcePos(l.d_source,row2,col2);
            if( e && e->getIdent() )
            {
                const int tag = e->getIdent()->getTag();
                if( tag == Thing::T_Procedure || Thing::T_Module )
                {
                    item->setText(1,e->getIdent()->getName() );
                    d_scopes[level] = cast<Scope*>(e->getIdent());
                }
            }
            //qDebug() << "level" << level << ( d_scopes[level] ? d_scopes[level]->getModule()->getName() : QByteArray("???") );
            item->setText(2,QString("%1:%2").arg(row).arg(col));
            item->setData(2, Qt::UserRole, l.d_line );
            if( fm.first )
                item->setText(3, fm.second->getName() );
            else
                item->setText(3, QString("<unknown> %1").arg(l.d_source.constData()) );
            item->setData(3, Qt::UserRole, l.d_source );
            item->setToolTip(3, l.d_source );
            if( !opened )
            {
                showEditor(l.d_source, row, col, true );
                d_rt->getLua()->setActiveLevel(level);
                opened = true;
            }
        }else
        {
            item->setData(1,Qt::UserRole,l.d_lineDefined );
            item->setText(2,QString("%1").arg( l.d_line - 1 ));
            item->setData(2, Qt::UserRole, l.d_line );
            item->setText(3, l.d_source );
            item->setData(3, Qt::UserRole, l.d_source );
            item->setToolTip(3, l.d_source );
            if( !opened )
            {
                Editor* edit = showEditor(l.d_source, RowCol::unpackRow2(l.d_lineDefined), RowCol::unpackCol2(l.d_lineDefined), false );
                d_rt->getLua()->setActiveLevel(level);
                d_bcv->parentWidget()->show();
                const quint32 rowCol = d_bcv->gotoFuncPc(l.d_lineDefined,l.d_line, center, true);
                if( rowCol && edit )
                {
                    d_lock = true;
                    edit->dbgRow = RowCol::unpackRow2(rowCol) - 1;
                    edit->dbgCol = RowCol::unpackCol2(rowCol) - 1;
                    edit->setCursorPosition( edit->dbgRow, edit->dbgCol, center );
                    edit->setPositionMarker(edit->dbgRow);
                    d_lock = false;
                }
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
        case Lua::Engine2::LocalVar::NIL:
            item->setText(1, "nil");
            break;
        case Lua::Engine2::LocalVar::FUNC:
            item->setText(1, "func");
            break;
        case Lua::Engine2::LocalVar::TABLE:
            item->setText(1, "table");
            break;
        case Lua::Engine2::LocalVar::STRUCT:
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

static void fillRawLocals(QTreeWidget* locals, Lua::Engine2* lua)
{
    Lua::Engine2::LocalVars vs = lua->getLocalVars(true,2,55,true);
    foreach( const Lua::Engine2::LocalVar& v, vs )
    {
        QTreeWidgetItem* item = new QTreeWidgetItem(locals);
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
        }else if( Lua::JitBytecode::isString(v.d_value) )
        {
            item->setText(1, "\"" + v.d_value.toString().simplified() + "\"");
            item->setToolTip(1, v.d_value.toString() );
        }else if( !v.d_value.isNull() )
            item->setText(1,v.d_value.toString());
        else
        {
            switch( v.d_type )
            {
            case Lua::Engine2::LocalVar::NIL:
                item->setText(1, "nil");
                break;
            case Lua::Engine2::LocalVar::FUNC:
                item->setText(1, "func");
                break;
            case Lua::Engine2::LocalVar::TABLE:
                item->setText(1, "table");
                break;
            case Lua::Engine2::LocalVar::STRUCT:
                item->setText(1, "struct");
                break;
            case Lua::Engine2::LocalVar::STRING:
                item->setText(1, "\"" + v.d_value.toString().simplified() + "\"");
                break;
            default:
                break;
           }
        }
    }
}

void Ide::fillLocals()
{
    d_locals->clear();

    if( d_rt->getLua()->getMode() == Lua::Engine2::PcMode )
    {
        fillRawLocals(d_locals, d_rt->getLua());
        d_locals->parentWidget()->show();
        return;
    }

    lua_Debug ar;
    const int level = d_rt->getLua()->getActiveLevel();
    Scope* scope = d_scopes[ level ];
    if( scope && lua_getstack( d_rt->getLua()->getCtx(), level, &ar ) )
    {
        foreach( const Ref<Named>& n, scope->d_order )
        {
            const int tag = n->getTag();
            if( tag == Thing::T_Parameter || tag == Thing::T_LocalVar )
            {
                QTreeWidgetItem* item = new QTreeWidgetItem(d_locals);
                const int before = lua_gettop(d_rt->getLua()->getCtx());
                if( lua_getlocal( d_rt->getLua()->getCtx(), &ar, n->d_slot + 1 ) )
                {
                    item->setText(0,n->d_name);
                    printLocalVal(item,n->d_type.data(), 0);
                    lua_pop( d_rt->getLua()->getCtx(), 1 );
                }else
                    item->setText(0,"<invalid>");
                Q_ASSERT( before == lua_gettop(d_rt->getLua()->getCtx()) );
            }
        }
#if 1
        Module* m = scope->getModule();
        QTreeWidgetItem* parent = new QTreeWidgetItem(d_locals);
        QString name = m->getName();
        parent->setToolTip(0,name);
        if( name.size() > 20 )
            name = name.left(20) + "...";
        parent->setText(0,name);
        parent->setText(1,"<module>");
        const int before = lua_gettop(d_rt->getLua()->getCtx());
        lua_getglobal( d_rt->getLua()->getCtx(), m->getName() );
        if( !lua_isnil( d_rt->getLua()->getCtx(), -1 ) )
        {
            const int mod = lua_gettop( d_rt->getLua()->getCtx() );
            foreach( const Ref<Named>& n, m->d_order )
            {
                if( n->getTag() == Thing::T_Variable )
                {
                    QTreeWidgetItem* item = new QTreeWidgetItem(parent);
                    item->setText(0,n->d_name);
                    const int before = lua_gettop(d_rt->getLua()->getCtx());
                    lua_rawgeti( d_rt->getLua()->getCtx(), mod, n->d_slot );
                    printLocalVal(item,n->d_type.data(), 0);
                    lua_pop( d_rt->getLua()->getCtx(), 1 );
                    Q_ASSERT( before == lua_gettop(d_rt->getLua()->getCtx()) );
                }
            }
        }else
            parent->setText(1,"<???>");
        lua_pop( d_rt->getLua()->getCtx(), 1 ); // module
        Q_ASSERT( before == lua_gettop(d_rt->getLua()->getCtx()) );
#endif
    }
#if 0 // TEST, usually 0
    fillRawLocals(d_locals, d_rt->getLua());
#endif
    d_locals->parentWidget()->show();
}

static inline Type* derefed( Type* type )
{
    if( type == 0 )
        return 0;
    return type->derefed();
}

template <class T>
static inline void createArrayElems( QTreeWidgetItem* parent, const void* ptr,
                                     int bytecount, int numOfFetchedElems, char format = 0 )
{
    const int count = bytecount / sizeof(T);
    parent->setText(1, QString("<array length %1>").arg(count) );
    const T* arr = (const T*)ptr;
    for( int i = 0; i < qMin(count,numOfFetchedElems); i++ )
    {
        QTreeWidgetItem* item = new QTreeWidgetItem(parent);
        item->setText(0,QString::number(i));
        switch(format)
        {
        case 'b':
            item->setText(1, arr[i] ? "true" : "false" );
            break;
        case 's':
            item->setText(1,QString("{%1}").arg((quint32)arr[i],32,2,QChar('0')));
            break;
        default:
            item->setText(1,QString::number(arr[i]));
            break;
        }
    }
}

static QString nameOf( Record* r, bool frame = false )
{
    QString name;
    Named* decl = r->findDecl();
    if( decl )
        name = decl->d_name;
    else if( frame )
        name = "<record>";
    else
        name = "record";
    return name;
}

void Ide::printLocalVal(QTreeWidgetItem* item, Type* type, int depth)
{
    static const int numOfFetchedElems = 55;
    static const int numOfLevels = 5;

    if( depth > numOfLevels )
        return;
    lua_State* L = d_rt->getLua()->getCtx();
    type = derefed(type);
    Q_ASSERT( type );
    int tag = type->getTag();
    if( tag == Thing::T_Pointer )
    {
        type = derefed(cast<Pointer*>(type)->d_to.data());
        tag = type->getTag();
    }
    if( tag == Thing::T_BaseType )
    {
        switch( type->getBaseType() )
        {
        case Type::CVOID:
            item->setText(1,QString("0x%1").arg((ptrdiff_t)lua_topointer(L,-1),0,16));
            return;
        case Type::BOOLEAN:
            if( lua_toboolean(L, -1) )
                item->setText(1,"true");
            else
                item->setText(1,"false");
            return;
        case Type::CHAR:
        case Type::WCHAR:
            {
                const ushort uc = lua_tointeger(L,-1);
                const QString ch = QChar( uc );
                item->setText(1,QString("'%1' %2x").arg(ch.simplified()).arg(uc,0,16));
            }
            return;
        case Type::BYTE:
            item->setText(1,QString("%1h").arg(lua_tointeger(L,-1),0,16));
            return;
        case Type::SHORTINT:
        case Type::INTEGER:
        case Type::LONGINT:
            item->setText(1,QString::number(lua_tointeger(L,-1)));
            return;
        case Type::REAL:
        case Type::LONGREAL:
            item->setText(1,QString::number(lua_tonumber(L,-1)));
            return;
        case Type::SET:
            item->setText(1,QString("{%1}").arg((quint32)lua_tointeger(L,-1),32,2,QChar('0')));
            return;
        case Type::ANY:
            item->setText(1,QString("<any> %1").arg(lua_tostring(L,-1)));
            return;
        case Type::NIL:
        case Type::STRING:
        case Type::WSTRING:
            Q_ASSERT( false );
            break;
        }
    } // else
    switch( tag )
    {
    case Thing::T_Array:
        {
            Array* a = cast<Array*>(type);
            Type* at = derefed(a->d_type.data());
            Q_ASSERT( at );
            const int arr = lua_gettop(L);
            const int luatype = lua_type(L, arr );
            if( luatype == LUA_TNIL )
                item->setText(1, QString("nil") );
            else if( luatype == 10 ) // cdata
            {
                const void* ptr = lua_topointer(L, -1);
                if( at->isChar() )
                {
                    QString str;
                    if( at->getBaseType() == Type::CHAR )
                    {
                        const quint8* buf = (const quint8*)ptr;
                        for(int i = 0; i < numOfFetchedElems; i++ )
                        {
                            quint8 ch = buf[i];
                            if( ch == 0 )
                                break;
                            str += QChar((ushort) ch );
                        }
                    }else
                    {
                        const quint16* buf = (const quint16*)ptr;
                        for(int i = 0; i < numOfFetchedElems; i++ )
                        {
                            quint16 ch = buf[i];
                            if( ch == 0 )
                                break;
                            str += QChar((ushort) ch );
                        }
                    }
                    item->setText(1,QString("\"%1\"").arg(str));
                }else
                {
                    lua_getglobal(L, "obxlj");
                    lua_rawgeti(L, -1, 26 ); // bytesize
                    lua_pushvalue( L, arr );
                    lua_pcall( L, 1, 1, 0 );
                    const int bytesize = lua_tointeger( L, -1 );
                    lua_pop( L, 2 ); // obxlj, count
                    switch( at->getBaseType() )
                    {
                    case Type::BOOLEAN:
                        createArrayElems<quint8>( item, ptr, bytesize, numOfFetchedElems, 'b');
                        break;
                    case Type::BYTE:
                        createArrayElems<quint8>( item, ptr, bytesize, numOfFetchedElems);
                        break;
                    case Type::SHORTINT:
                        createArrayElems<qint16>( item, ptr, bytesize, numOfFetchedElems);
                        break;
                    case Type::INTEGER:
                        createArrayElems<qint32>( item, ptr, bytesize, numOfFetchedElems);
                        break;
                    case Type::LONGINT:
                        createArrayElems<qint64>( item, ptr, bytesize, numOfFetchedElems);
                        break;
                    case Type::REAL:
                        createArrayElems<float>( item, ptr, bytesize, numOfFetchedElems);
                        break;
                    case Type::LONGREAL:
                        createArrayElems<double>( item, ptr, bytesize, numOfFetchedElems);
                        break;
                    case Type::SET:
                        createArrayElems<quint32>( item, ptr, bytesize, numOfFetchedElems, 's');
                        break;
                    case Type::NIL:
                    case Type::STRING:
                    case Type::WSTRING:
                        Q_ASSERT( false );
                        break;
                    }
                }
            }else if( luatype != LUA_TTABLE )
            {
                item->setText(1, QString("<invalid array> %1").arg(lua_tostring(L, arr) ) );
            }else
            {
                lua_getfield( L, arr, "count" );
                const int count = lua_tointeger( L, -1 );
                lua_pop( L, 1 );
                item->setText(1, QString("<array length %1>").arg(count) );
                for( int i = 0; i < qMin(count,numOfFetchedElems); i++ )
                {
                    QTreeWidgetItem* sub = new QTreeWidgetItem(item);
                    sub->setText(0,QString::number(i));
                    lua_rawgeti( L, arr, i );
                    printLocalVal(sub,at,depth+1);
                    lua_pop( L, 1 );
                }
            }
        }
        break;
    case Thing::T_Enumeration:
        {
            const int e = lua_tointeger(L,-1);
            Enumeration* et = cast<Enumeration*>(type);
            if( e >= 0 && e < et->d_items.size() )
                item->setText(1,et->d_items[e]->d_name);
            else
                item->setText(1,"<invalid enum value>");
        }
        break;
    case Thing::T_ProcType:
        if( lua_isnil( L, -1 ) )
            item->setText(1,"nil");
        else
        {
            const void* ptr = lua_topointer(L, -1);
#if Q_PROCESSOR_WORDSIZE == 4
            const quint32 ptr2 = (quint32)ptr;
#else
            const quint64 ptr2 = (quint64)ptr;
#endif
            item->setText(1,QString("<proc 0x%1>").arg(ptr2,0,16));
        }
        break;
    case Thing::T_Record:
        {
            const int rec = lua_gettop(L);
            Record* r = cast<Record*>(type);
            const int type = lua_type( L, rec );
            if( type == LUA_TNIL )
            {
                item->setText(1,"nil");
                break;
            }else if( type != LUA_TTABLE )
            {
                item->setText(1,tr("<invalid %1>").arg(nameOf(r)));
                // qWarning() << "wrong type, expecting table, got type" << type;
                // happens when a procedure is entered before initialization code could run
                break;
            }
            if( !r->d_subRecs.isEmpty() )
            {
                // look for the dynamic type
                if( lua_getmetatable(L,rec) )
                {
                    lua_getfield(L, -1, "@cls");
                    if( !lua_isnil( L, -1 ) )
                    {
                        Record* rd = r->findBySlot( lua_tointeger(L, -1) );
                        if( rd )
                            r = rd;
                    }
                    lua_pop(L,2); // meta + field
                }
            }
            item->setText(1,nameOf(r,true));
            QList<Field*> fs = r->getOrderedFields();
            foreach( Field* f, fs )
            {
                QTreeWidgetItem* sub = new QTreeWidgetItem(item);
                sub->setText(0,f->d_name);
                lua_rawgeti( L, rec, f->d_slot );
                printLocalVal(sub,derefed(f->d_type.data()),depth+1);
                lua_pop( L, 1 );
            }
        }
        break;
    default:
        qWarning() << "unexpected type" << type->getTagName();
        Q_ASSERT(false);
        break;
    }
}

static void fillModItems(QTreeWidgetItem* item, Named* n, Scope* p, Record* r, bool sort, QHash<Named*,QTreeWidgetItem*>& idx );

static void fillRecord(QTreeWidgetItem* item, Named* n, Record* r, bool sort, QHash<Named*,QTreeWidgetItem*>& idx )
{
    fillModItems(item,n, 0, r, sort, idx);
    if( r->d_baseRec )
        item->setText(0, item->text(0) + " ⇑");
    if( !r->d_subRecs.isEmpty() )
        item->setText(0, item->text(0) + QString(" ⇓%1").arg(r->d_subRecs.size()));
    item->setToolTip( 0, item->text(0) );
}

template<class T>
static void createModItem(T* parent, Named* n, Type* t, bool nonbound, bool sort, QHash<Named*,QTreeWidgetItem*>& idx )
{
    bool isAlias = false;
    if( t == 0 )
        t = n->d_type.data();
    else
        isAlias = true;
    if( t == 0 )
        return;
    if( idx.contains(n) )
    {
        // qWarning() << "fillMod recursion at" << n->getModule()->d_file << n->d_loc.d_row << n->d_name;
        return; // can legally happen if record decl contains a typedef using record, as e.g. Meta.Item.ParamCallVal.IP
    }
    switch( n->getTag() )
    {
    case Thing::T_NamedType:
        switch( t->getTag() )
        {
        case Thing::T_Record:
            {
                QTreeWidgetItem* item = new QTreeWidgetItem(parent);
                if( !isAlias  )
                    fillRecord(item,n,cast<Record*>(t),sort,idx);
                else
                    fillModItems(item,n, 0, 0, sort, idx);
            }
            break;
        case Thing::T_Pointer:
            {
                Pointer* p = cast<Pointer*>(t);
                if( p->d_to && p->d_to->getTag() == Thing::T_Record )
                {
                    QTreeWidgetItem* item = new QTreeWidgetItem(parent);
                    if( !isAlias )
                        fillRecord(item,n,cast<Record*>(p->d_to.data()),sort,idx);
                    else
                        fillModItems(item,n, 0, 0, sort, idx);
                }
            }
            break;
        case Thing::T_QualiType:
            if( t->toRecord() )
                createModItem(parent,n,t->derefed(),nonbound, sort, idx);
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
    typedef QMultiMap<QByteArray,Named*> Sort;
    if( p && sort)
    {
        Sort tmp;
        foreach( const Ref<Named>& n, p->d_order )
            tmp.insert( n->d_name.toLower(), n.data() );
        Sort::const_iterator i;
        for( i = tmp.begin(); i != tmp.end(); ++i )
            createModItem(parent,i.value(),0,true, sort, idx);
    }else if( p )
    {
        foreach( const Ref<Named>& n, p->d_order )
            createModItem(parent,n.data(),0,true, sort, idx);
    }
    if( r && sort )
    {
        Sort tmp;
        foreach( const Ref<Procedure>& n, r->d_methods )
            tmp.insert( n->d_name.toLower(), n.data() );
        Sort::const_iterator i;
        for( i = tmp.begin(); i != tmp.end(); ++i )
            createModItem(parent,i.value(),0,false, sort, idx);
    }else if( r )
    {
        foreach( const Ref<Procedure>& n, r->d_methods )
            createModItem(parent,n.data(),0,false, sort, idx);
    }
}

static void fillModItems( QTreeWidgetItem* item, Named* n, Scope* p, Record* r,
                          bool sort, QHash<Named*,QTreeWidgetItem*>& idx )
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
        else if( r == 0 && p == 0 )
            item->setIcon(0, QPixmap( pub ? ":/images/alias.png" : ":/images/alias_priv.png" ) );
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
    d_modTitle->setText( QString("'%1'").arg(m->getName().constData()) );
    walkModItems(d_mod, m, 0, true, d_modIdx );
}

template<class T>
static QTreeWidgetItem* fillHierProc( T* parent, Procedure* p, Named* ref )
{
    QTreeWidgetItem* item = new QTreeWidgetItem(parent);
    Q_ASSERT( p->d_receiver && !p->d_receiver->d_type.isNull() );
    Q_ASSERT( p->d_receiver->d_type->getTag() == Thing::T_QualiType );
    QualiType* rqt = cast<QualiType*>(p->d_receiver->d_type.data());
    Named* rid = rqt->d_quali->getIdent();
    if( rid )
        item->setText(0, rid->getQualifiedName().join('.'));
    else
        item->setText(0, QString("%1.%2").arg(p->getModule()->getName().constData())
                      .arg(rqt->getQualiString().join('.').constData()));
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
    Named* name = p->findDecl(true);
    if( name )
        item->setText(0, name->getQualifiedName().join('.') );
    else
        item->setText(0, name->d_name);
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
        // d_hier->expandAll();
        ref->setExpanded(true);
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
    d_bcv->clearMarker();
}

void Ide::enableDbgMenu()
{
    d_dbgBreak->setEnabled(!d_rt->getLua()->isWaiting() && d_rt->getLua()->isExecuting() && d_rt->getLua()->isDebug() );
    d_dbgAbort->setEnabled(d_rt->getLua()->isWaiting());
    d_dbgContinue->setEnabled(d_rt->getLua()->isWaiting());
    d_dbgStepIn->setEnabled(d_rt->getLua()->isWaiting() && d_rt->getLua()->isDebug() );
    d_dbgStepOver->setEnabled(d_rt->getLua()->isWaiting() && d_rt->getLua()->isDebug() );
    d_dbgStepOut->setEnabled(d_rt->getLua()->isWaiting() && d_rt->getLua()->isDebug() );
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
    Project::FileMod fm = d_rt->getPro()->findFile(edit->getPath());
    Q_ASSERT( fm.first && fm.first->d_mod );
    if( on )
        d_rt->getLua()->addBreak( fm.first->d_mod->getName(), line + 1 );
    else
        d_rt->getLua()->removeBreak( fm.first->d_mod->getName(), line + 1 );
}

void Ide::onSingleStep()
{
    // ENABLED_IF( d_rt->getLua()->isWaiting() );

    d_rt->getLua()->runToNextLine();
}

void Ide::onStepOver()
{
    d_rt->getLua()->runToNextLine(Lua::Engine2::StepOver);
}

void Ide::onStepOut()
{
    d_rt->getLua()->runToNextLine(Lua::Engine2::StepOut);
}

void Ide::onContinue()
{
    // ENABLED_IF( d_rt->getLua()->isWaiting() );

    d_rt->getLua()->runToBreakPoint();
}

void Ide::onShowLlBc()
{
    ENABLED_IF( d_bcv->topLevelItemCount() );

    Lua::BcViewer* bc = new Lua::BcViewer();
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
    const QString res = QInputDialog::getText(this,tr("Oberon File System Root"),
                                              tr("Enter Path (supports %PRODIR% and %APPDIR%):"), QLineEdit::Normal,
                                              d_rt->getPro()->getWorkingDir(), &ok );
    if( !ok )
        return;
    d_rt->getPro()->setWorkingDir(res);
}

void Ide::onLuaNotify(int messageType, QByteArray val1, int val2)
{
    switch( messageType )
    {
    case Lua::Engine2::Started:
    case Lua::Engine2::Continued:
    case Lua::Engine2::LineHit:
    case Lua::Engine2::BreakHit:
    case Lua::Engine2::ErrorHit:
    case Lua::Engine2::Finished:
    case Lua::Engine2::Aborted:
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

void Ide::clear()
{
    d_backHisto.clear();
    d_forwardHisto.clear();
    d_rt->getPro()->clear();
    d_mods->clear();
    d_mod->clear();
    d_hier->clear();
    d_modIdx.clear();
    d_stack->clear();
    d_scopes.clear();
    d_locals->clear();
    d_xrefTitle->clear();
    d_modTitle->clear();
    d_hierTitle->clear();
    d_xref->clear();
    d_errs->clear();
}

void Ide::showBc(const QByteArray& bc)
{
    if( !bc.isEmpty() )
    {
        d_curBc = bc;
        QBuffer buf( &d_curBc );
        buf.open(QIODevice::ReadOnly);
        d_bcv->loadFrom(&buf);
    }
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

void Ide::onExpMod()
{
    ENABLED_IF( d_mods->currentItem() );

    ModRef s = d_mods->currentItem()->data(0,Qt::UserRole).value<ModRef>();
    if( s.isNull() || s->getTag() != Thing::T_Module )
        return;

    Module* m = cast<Module*>(s.data());

    const QString path = QFileDialog::getSaveFileName( this, tr("Export Module"), m->getName() + ".obx" );

    if( path.isEmpty() )
        return;

    d_rt->getPro()->printTreeShaken( m->d_file, path );
}

void Ide::onBcDebug()
{
    CHECKED_IF( true, d_rt->getLua()->getMode() == Lua::Engine2::PcMode );

    if( d_rt->getLua()->getMode() == Lua::Engine2::PcMode )
        d_rt->getLua()->setDebugMode( Lua::Engine2::LineMode );
    else
        d_rt->getLua()->setDebugMode( Lua::Engine2::PcMode );
}

void Ide::onRowColMode()
{
    CHECKED_IF( true, d_rt->getLua()->getMode() == Lua::Engine2::RowColMode );

    if( d_rt->getLua()->getMode() == Lua::Engine2::RowColMode )
        d_rt->getLua()->setDebugMode( Lua::Engine2::LineMode );
    else
        d_rt->getLua()->setDebugMode( Lua::Engine2::RowColMode );
}

void Ide::onShowBcFile()
{
    ENABLED_IF(true);

    const QString path = QFileDialog::getOpenFileName(this,tr("Open bytecode file"),QString());

    if( path.isEmpty() )
        return;

    QDir::setCurrent(QFileInfo(path).absolutePath());


    Lua::BcViewer2* view = new Lua::BcViewer2();
    view->loadFrom(path);
    view->setWindowTitle( tr("%1 - Bytecode View").arg( QFileInfo(path).fileName() ) );
    view->show();
}

void Ide::onJitEnabled()
{
    CHECKED_IF(true,d_rt->jitEnabled());

    d_rt->setJitEnabled( !d_rt->jitEnabled() );
}

void Ide::onRestartLua()
{
    ENABLED_IF( !d_rt->getLua()->isExecuting() );

    d_rt->restartEngine();
}

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    a.setOrganizationName("me@rochus-keller.ch");
    a.setOrganizationDomain("github.com/rochus-keller/Oberon");
    a.setApplicationName("Oberon+ IDE");
    a.setApplicationVersion("0.8.8");
    a.setStyle("Fusion");    
    QFontDatabase::addApplicationFont(":/font/DejaVuSansMono.ttf"); // "DejaVu Sans Mono"

    Ide w;
    if( a.arguments().size() > 1 )
        w.loadFile(a.arguments()[1] );

    return a.exec();
}
