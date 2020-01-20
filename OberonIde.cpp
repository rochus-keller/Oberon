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
#include <LjTools/Engine2.h>
#include <LjTools/Terminal2.h>
#include <LjTools/BcViewer2.h>
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
#include <GuiTools/AutoMenu.h>
#include <GuiTools/CodeEditor.h>
#include <GuiTools/AutoShortcut.h>
#include <GuiTools/DocTabWidget.h>
using namespace Ob;
using namespace Lua;

struct ScopeRef : public Ast::Ref<Ast::Scope>
{
    ScopeRef(Ast::Scope* s = 0):Ref(s) {}
};
Q_DECLARE_METATYPE(ScopeRef)

class OberonIde::Editor : public CodeEditor
{
public:
    Editor(QWidget* p, Project* pro):CodeEditor(p),d_pro(pro)
    {
        setCharPerTab(3);
        d_hl = new Highlighter( document() );
        updateTabWidth();
   }
    ~Editor()
    {
    }
    Highlighter* d_hl;
    Project* d_pro;
#if 0
    // TODO: modify AST so that all qualifier types are represented by IdentLeaf/IdentSel so usedBy can be constructed
    Ljas::Assembler::Xref* d_xref;

    typedef QList<const Ljas::Assembler::Xref*> SymList;

    void markNonTerms(const SymList& syms)
    {
        d_nonTerms.clear();
        QTextCharFormat format;
        format.setBackground( QColor(247,245,243) );
        foreach( const Ljas::Assembler::Xref* s, syms )
        {
            if( s == 0 )
                continue;
            QTextCursor c( document()->findBlockByNumber( s->d_line - 1) );
            c.setPosition( c.position() + s->d_col - 1 );
            c.setPosition( c.position() + s->d_name.size(), QTextCursor::KeepAnchor );

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

        if( false ) // does not work yet !d_err.getErrors().isEmpty() )
        {
            QTextCharFormat errorFormat;
            errorFormat.setUnderlineStyle(QTextCharFormat::WaveUnderline);
            errorFormat.setUnderlineColor(Qt::magenta);
            Ljas::Errors::EntryList::const_iterator i;
            for( i = d_err.getErrors(getPath()).begin(); i != d_err.getErrors(getPath()).end(); ++i )
            {
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
    const Ljas::Assembler::Xref* findSymbolBySourcePos(const Ljas::Assembler::Xref* node, quint32 line, quint16 col ) const
    {
        if( node == 0 )
            return 0;
        if( node->d_line > line )
            return 0;
        if( line == node->d_line && col >= node->d_col && col <= node->d_col + node->d_name.size() )
            return node;
        // else
        foreach( const Ljas::Assembler::Xref* n, node->d_subs )
        {
            const Ljas::Assembler::Xref* res = findSymbolBySourcePos( n, line, col );
            if( res )
                return res;
        }
        return 0;
    }
    void mousePressEvent(QMouseEvent* e)
    {
        if( !d_link.isEmpty() )
        {
            QTextCursor cur = cursorForPosition(e->pos());
            pushLocation( Location( cur.blockNumber(), cur.positionInBlock() ) );
            QApplication::restoreOverrideCursor();
            d_link.clear();
            setCursorPosition( d_linkLineNr, d_linkColNr, true );
        }else if( QApplication::keyboardModifiers() == Qt::ControlModifier )
        {
            QTextCursor cur = cursorForPosition(e->pos());
            const Ljas::Assembler::Xref* sym = findSymbolBySourcePos(d_xref,cur.blockNumber() + 1,cur.positionInBlock() + 1);
            if( sym )
            {
                const Ljas::Assembler::Xref* d = sym->d_decl;
                if( d )
                {
                    pushLocation( Location( cur.blockNumber(), cur.positionInBlock() ) );
                    setCursorPosition( d->d_line - 1, d->d_col - 1, true );
                }
           }
        }else
            QPlainTextEdit::mousePressEvent(e);
    }

    void mouseMoveEvent(QMouseEvent* e)
    {
        QPlainTextEdit::mouseMoveEvent(e);
        if( QApplication::keyboardModifiers() == Qt::ControlModifier && d_xref )
        {
            QTextCursor cur = cursorForPosition(e->pos());
            const Ljas::Assembler::Xref* sym = findSymbolBySourcePos(d_xref,cur.blockNumber() + 1, cur.positionInBlock() + 1);
            const bool alreadyArrow = !d_link.isEmpty();
            d_link.clear();
            if( sym )
            {
                const int off = cur.positionInBlock() + 1 - sym->d_col;
                cur.setPosition(cur.position() - off);
                cur.setPosition( cur.position() + sym->d_name.size(), QTextCursor::KeepAnchor );
                const Ljas::Assembler::Xref* d = sym->d_decl;
                if( d )
                {
                    QTextEdit::ExtraSelection sel;
                    sel.cursor = cur;
                    sel.format.setFontUnderline(true);
                    d_link << sel;
                    d_linkLineNr = d->d_line - 1;
                    d_linkColNr = d->d_col - 1;
                    if( !alreadyArrow )
                        QApplication::setOverrideCursor(Qt::ArrowCursor);
                }
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
#endif
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

OberonIde::OberonIde(QWidget *parent)
    : QMainWindow(parent),d_lock(false),d_filesDirty(false)
{
    s_this = this;

    d_pro = new Project(this);

    d_lua = new Engine2(this);
    d_lua->addStdLibs();
    d_lua->addLibrary(Engine2::PACKAGE);
    d_lua->addLibrary(Engine2::IO);
    d_lua->addLibrary(Engine2::BIT);
    d_lua->addLibrary(Engine2::JIT);
    d_lua->addLibrary(Engine2::OS);
    LjLib::install(d_lua->getCtx());
    loadLuaLib( d_lua, "obnlj" );

    Engine2::setInst(d_lua);

    d_tab = new DocTab(this);
    d_tab->setCloserIcon( ":/images/close.png" );
    Gui::AutoMenu* pop = new Gui::AutoMenu( d_tab, true );
    pop->addCommand( tr("Forward Tab"), d_tab, SLOT(onDocSelect()), tr("CTRL+TAB") );
    pop->addCommand( tr("Backward Tab"), d_tab, SLOT(onDocSelect()), tr("CTRL+SHIFT+TAB") );
    pop->addCommand( tr("Close Tab"), d_tab, SLOT(onCloseDoc()), tr("CTRL+W") );
    pop->addCommand( tr("Close All"), d_tab, SLOT(onCloseAll()) );
    pop->addCommand( tr("Close All Others"), d_tab, SLOT(onCloseAllButThis()) );
    addTopCommands( pop );

    new Gui::AutoShortcut( tr("CTRL+TAB"), this, d_tab, SLOT(onDocSelect()) );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+TAB"), this, d_tab, SLOT(onDocSelect()) );
    new Gui::AutoShortcut( tr("CTRL+W"), this, d_tab, SLOT(onCloseDoc()) );

    connect( d_tab, SIGNAL( currentChanged(int) ), this, SLOT(onTabChanged() ) );
    connect( d_tab, SIGNAL(closing(int)), this, SLOT(onTabClosing(int)) );

    setDockNestingEnabled(true);
    setCorner( Qt::BottomRightCorner, Qt::RightDockWidgetArea );
    setCorner( Qt::BottomLeftCorner, Qt::LeftDockWidgetArea );
    setCorner( Qt::TopRightCorner, Qt::RightDockWidgetArea );
    setCorner( Qt::TopLeftCorner, Qt::LeftDockWidgetArea );

    createTerminal();
    createDumpView();
    createMods();
    createErrs();
    createMenu();

    setCentralWidget(d_tab);

    s_oldHandler = qInstallMessageHandler(messageHander);

    QSettings s;

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

}

void OberonIde::loadFile(const QString& path)
{
    QFileInfo info(path);

    if( info.isDir() || info.suffix() != ".obnpro" )
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
    event->setAccepted(checkSaved( tr("Quit Application")));
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
    connect(d_bcv,SIGNAL(sigGotoLine(int)),this,SLOT(onGotoLnr(int)));

    Gui::AutoMenu* pop = new Gui::AutoMenu( d_bcv, true );
    pop->addCommand( "Run on LuaJIT", this, SLOT(onRun()), tr("CTRL+R"), false );
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
    pop->addCommand( "Compile", this, SLOT(onCompile()), tr("CTRL+T"), false );
    pop->addCommand( "Run on LuaJIT", this, SLOT(onRun()), tr("CTRL+R"), false );
    addTopCommands(pop);

    new QShortcut(tr("CTRL+Q"),this,SLOT(close()));
    new Gui::AutoShortcut( tr("CTRL+O"), this, this, SLOT(onOpenPro()) );
    new Gui::AutoShortcut( tr("CTRL+N"), this, this, SLOT(onNewPro()) );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+S"), this, this, SLOT(onSavePro()) );
    new Gui::AutoShortcut( tr("CTRL+S"), this, this, SLOT(onSaveFile()) );
    new Gui::AutoShortcut( tr("CTRL+R"), this, this, SLOT(onRun()) );
    new Gui::AutoShortcut( tr("CTRL+T"), this, this, SLOT(onCompile()) );
}

void OberonIde::onCompile()
{
    ENABLED_IF(true);
    compile();
}

void OberonIde::onRun()
{
    ENABLED_IF( !d_pro->getFiles().isEmpty() );

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

    bool hasErrors = false;
    foreach( const Project::File& f, files )
    {
        qDebug() << "loading" << f.d_mod->d_name;
        if( !d_lua->addSourceLib( f.d_bc, f.d_mod->d_name ) )
        {
            QByteArray msg = d_lua->getLastError();
            hasErrors = true;
            luaRuntimeMessage(msg,f.d_file);
            qCritical() << msg.constData();
        }
    }

    if( hasErrors )
    {
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
    d_lua->executeCmd(src,"terminal");

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

void OberonIde::onGotoLnr(int lnr)
{
    if( d_lock )
        return;
    d_lock = true;
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    if( edit )
        edit->setCursorPosition(lnr-1,0);
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
    if( d_lock )
        return;
    d_lock = true;
    Editor* edit = static_cast<Editor*>( d_tab->getCurrentTab() );
    if( edit )
    {
        QTextCursor cur = edit->textCursor();
        const int line = cur.blockNumber() + 1;
        d_bcv->gotoLine(line);
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
    ENABLED_IF(d_tab->getCurrentTab() != 0);

    if( d_bcv->topLevelItemCount() == 0 )
        onCompile();
    if( d_bcv->topLevelItemCount() == 0 )
        return;

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

    Ast::Module* m = s->getModule();
    if( m == 0 )
        return;

    showEditor( m->d_file, s->d_loc.d_row, s->d_loc.d_col );
}

void OberonIde::onTabChanged()
{
    const QString path = d_tab->getCurrentDoc().toString();

    onEditorChanged();

    if( !path.isEmpty() )
    {
        QByteArray bc = d_pro->getFiles().value( path ).d_bc;
        if( !bc.isEmpty() )
        {
            QBuffer buf( &bc );
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

void OberonIde::compile()
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
    d_pro->recompile();
    d_pro->generate();
    onErrors();
    fillMods();
    onTabChanged();
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
            item->setData(0,Qt::UserRole, QVariant::fromValue(ScopeRef(static_cast<Ast::Scope*>(n))) );
            fillScope(item, static_cast<Ast::Scope*>(n) );
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
        fillScope( item, static_cast<Ast::Scope*>(n));
    }
}

void OberonIde::addTopCommands(Gui::AutoMenu* pop)
{
    Q_ASSERT( pop != 0 );
    pop->addSeparator();
    // TODO
    pop->addAutoCommand( "Set &Font...", SLOT(handleSetFont()) );
    pop->addAutoCommand( "Show &Linenumbers", SLOT(handleShowLinenumbers()) );
    pop->addCommand( "Show Fullscreen", this, SLOT(onFullScreen()) );
    pop->addSeparator();
    pop->addAction(tr("Quit"),qApp,SLOT(quit()), tr("CTRL+Q") );
}

void OberonIde::showEditor(const QString& path, int row, int col)
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

        edit->loadFromFile(path);
        d_tab->addDoc(edit,path);
        onEditorChanged();
    }
    if( row > 0 && col > 0 )
        edit->setCursorPosition( row-1, col-1, true );
    edit->setFocus();
}

void OberonIde::createMenu(OberonIde::Editor* edit)
{
    Gui::AutoMenu* pop = new Gui::AutoMenu( edit, true );
    pop->addCommand( "Save", this, SLOT(onSaveFile()), tr("CTRL+S"), false );
    pop->addSeparator();
    pop->addCommand( "Compile", this, SLOT(onCompile()), tr("CTRL+T"), false );
    pop->addCommand( "Run on LuaJIT", this, SLOT(onRun()), tr("CTRL+R"), false );
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
    pop->addCommand( "Go Back", edit, SLOT(handleGoBack()), tr("ALT+Left"), true );
    pop->addCommand( "Go Forward", edit, SLOT(handleGoForward()), tr("ALT+Right"), true );
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

void OberonIde::luaRuntimeMessage(const QByteArray& msg, const QString& file )
{
    const int firstColon = msg.indexOf(':');
    if( firstColon != -1 )
    {
        const int secondColon = msg.indexOf(':',firstColon + 1);
        if( secondColon != -1 )
        {
            d_pro->getErrs()->error(Errors::Runtime, file,
                                    msg.mid(firstColon+1, secondColon - firstColon - 1 ).toInt(), 1,
                                    msg.mid(secondColon+1) );
            return;
        }
    }
    qWarning() << "Unknown Lua error message format:" << msg;
}

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    a.setOrganizationName("me@rochus-keller.ch");
    a.setOrganizationDomain("github.com/rochus-keller/Oberon");
    a.setApplicationName("Oberon IDE");
    a.setApplicationVersion("0.1.0");
    a.setStyle("Fusion");

    OberonIde w;

    if( a.arguments().size() > 1 )
        w.loadFile(a.arguments()[1] );

    /* TEST
    Ast::Loc l;
    l.d_col = 1;
    l.d_row = 1;
    quint32 p = l.packed();
    qDebug() << QByteArray::number(p,2) << Ast::Loc::isPacked(p) << Ast::Loc::packedCol(p) << Ast::Loc::packedRow(p);
    */
    return a.exec();
}
