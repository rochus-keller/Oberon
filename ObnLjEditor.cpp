/*
* Copyright 2019 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include "ObnLjEditor.h"
#include "ObnHighlighter.h"
#include "ObCodeModel.h"
#include "ObFileCache.h"
#include "ObLuaGen.h"
#include "ObLjLib.h"
#include "ObAst.h"
#include "ObAstEval.h"
#include "ObLuaGen2.h"
#include "ObLjbcGen.h"
#include "ObLexer.h"
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
#include <GuiTools/AutoMenu.h>
#include <GuiTools/CodeEditor.h>
#include <GuiTools/AutoShortcut.h>
using namespace Ob;
using namespace Lua;

#define USE_LJBC_GEN

static LjEditor* s_this = 0;
static void report(QtMsgType type, const QString& message )
{
    if( s_this )
    {
        switch(type)
        {
        case QtDebugMsg:
            s_this->logMessage(QLatin1String("INF: ") + message);
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
    QFile obnlj( QString(":/runtime/%1.lua").arg(name.constData()) );
    if( !obnlj.open(QIODevice::ReadOnly) )
        qCritical() << "cannot load Lua lib" << name;
    if( !lua->addSourceLib( obnlj.readAll(), name ) )
        qCritical() << "compiling" << name << ":" << lua->getLastError();
}

LjEditor::LjEditor(QWidget *parent)
    : QMainWindow(parent),d_lock(false),d_useGen(Gen2)
{
    s_this = this;

    d_lua = new Engine2(this);
    d_lua->addStdLibs();
    d_lua->addLibrary(Engine2::PACKAGE);
    d_lua->addLibrary(Engine2::IO);
    d_lua->addLibrary(Engine2::DBG);
    d_lua->addLibrary(Engine2::BIT);
    d_lua->addLibrary(Engine2::JIT);
    d_lua->addLibrary(Engine2::OS);
    d_lua->addLibrary(Engine2::FFI);
    LjLib::install(d_lua->getCtx());

    Engine2::setInst(d_lua);

    loadLuaLib( d_lua, "obnlj" );
    loadLuaLib(d_lua,"In");
    loadLuaLib(d_lua,"Out");
    loadLuaLib(d_lua,"Files");
    loadLuaLib(d_lua,"Input");
    loadLuaLib(d_lua,"Math");
    loadLuaLib(d_lua,"Strings");
    loadLuaLib(d_lua,"Coroutines");
    loadLuaLib(d_lua,"XYPlane");


    d_eng = new JitEngine(this);

    d_edit = new CodeEditor(this);
    d_edit->setCharPerTab(3);
    d_edit->setPaintIndents(false);
    d_hl = new Highlighter( d_edit->document() );
    d_hl->setEnableExt(true); // TODO TEST
    d_edit->updateTabWidth();

    setDockNestingEnabled(true);
    setCorner( Qt::BottomRightCorner, Qt::RightDockWidgetArea );
    setCorner( Qt::BottomLeftCorner, Qt::LeftDockWidgetArea );
    setCorner( Qt::TopRightCorner, Qt::RightDockWidgetArea );
    setCorner( Qt::TopLeftCorner, Qt::LeftDockWidgetArea );

    createTerminal();
    createDumpView();
    createMenu();

    setCentralWidget(d_edit);

    s_oldHandler = qInstallMessageHandler(messageHander);

    QSettings s;

    if( s.value("Fullscreen").toBool() )
        showFullScreen();
    else
        showMaximized();

    const QVariant state = s.value( "DockState" );
    if( !state.isNull() )
        restoreState( state.toByteArray() );


    connect(d_edit, SIGNAL(modificationChanged(bool)), this, SLOT(onCaption()) );
    connect(d_bcv,SIGNAL(sigGotoLine(quint32)),this,SLOT(onGotoLnr(quint32)));
    connect(d_edit,SIGNAL(cursorPositionChanged()),this,SLOT(onCursor()));
    connect(d_eng,SIGNAL(sigPrint(QString,bool)), d_term, SLOT(printText(QString,bool)) );
}

LjEditor::~LjEditor()
{

}

static bool preloadLib( Ast::Model& mdl, const QByteArray& name )
{
    QFile f( QString(":/oakwood/%1.Def" ).arg(name.constData() ) );
    if( !f.open(QIODevice::ReadOnly) )
    {
        qCritical() << "unknown preload" << name;
        return false;
    }
    mdl.addPreload( name, f.readAll() );
    return true;
}

void LjEditor::loadFile(const QString& path)
{
    QFile in(path);
    if( !in.open(QIODevice::ReadOnly) )
        return;
    if( Ob::Lexer::skipOberonHeader(&in) )
    {
        QBuffer buf;
        buf.buffer() = in.readAll();
        buf.buffer().replace( '\r', '\n' );
        buf.open(QIODevice::ReadOnly);
        d_edit->loadFromFile(&buf,path);
    }else
        d_edit->loadFromFile(&in,path);
    QDir::setCurrent(QFileInfo(path).absolutePath());
    onCaption();

    onParse();
}

void LjEditor::logMessage(const QString& str, bool err)
{
    d_term->printText(str,err);
}

void LjEditor::closeEvent(QCloseEvent* event)
{
    QSettings s;
    s.setValue( "DockState", saveState() );
    event->setAccepted(checkSaved( tr("Quit Application")));
}

void LjEditor::createTerminal()
{
    QDockWidget* dock = new QDockWidget( tr("Terminal"), this );
    dock->setObjectName("Terminal");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable );
    d_term = new Terminal2(dock, d_lua);
    dock->setWidget(d_term);
    addDockWidget( Qt::BottomDockWidgetArea, dock );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+C"), this, d_term, SLOT(onClear()) );
}

void LjEditor::createDumpView()
{
    QDockWidget* dock = new QDockWidget( tr("Bytecode"), this );
    dock->setObjectName("Bytecode");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable );
    d_bcv = new BcViewer2(dock);
    dock->setWidget(d_bcv);
    addDockWidget( Qt::RightDockWidgetArea, dock );
}

void LjEditor::createMenu()
{
    Gui::AutoMenu* pop = new Gui::AutoMenu( d_edit, true );
    pop->addCommand( "New", this, SLOT(onNew()), tr("CTRL+N"), false );
    pop->addCommand( "Open...", this, SLOT(onOpen()), tr("CTRL+O"), false );
    pop->addCommand( "Save", this, SLOT(onSave()), tr("CTRL+S"), false );
    pop->addCommand( "Save as...", this, SLOT(onSaveAs()) );
    pop->addSeparator();
    pop->addCommand( "Compile", this, SLOT(onParse()), tr("CTRL+T"), false );
    pop->addCommand( "Run on LuaJIT", this, SLOT(onRun()), tr("CTRL+R"), false );
    pop->addCommand( "Run on test VM", this, SLOT(onRun2()), tr("CTRL+SHIFT+R"), false );
    pop->addCommand( "Export binary...", this, SLOT(onExportBc()) );
    pop->addCommand( "Export LjAsm...", this, SLOT(onExportAsm()) );
    pop->addCommand( "Export Lua...", this, SLOT(onExportLua()) );
    pop->addSeparator();
    pop->addCommand( "Undo", d_edit, SLOT(handleEditUndo()), tr("CTRL+Z"), true );
    pop->addCommand( "Redo", d_edit, SLOT(handleEditRedo()), tr("CTRL+Y"), true );
    pop->addSeparator();
    pop->addCommand( "Cut", d_edit, SLOT(handleEditCut()), tr("CTRL+X"), true );
    pop->addCommand( "Copy", d_edit, SLOT(handleEditCopy()), tr("CTRL+C"), true );
    pop->addCommand( "Paste", d_edit, SLOT(handleEditPaste()), tr("CTRL+V"), true );
    pop->addSeparator();
    pop->addCommand( "Find...", d_edit, SLOT(handleFind()), tr("CTRL+F"), true );
    pop->addCommand( "Find again", d_edit, SLOT(handleFindAgain()), tr("F3"), true );
    pop->addCommand( "Replace...", d_edit, SLOT(handleReplace()) );
    pop->addSeparator();
    pop->addCommand( "&Goto...", d_edit, SLOT(handleGoto()), tr("CTRL+G"), true );
    pop->addCommand( "Go Back", d_edit, SLOT(handleGoBack()), tr("ALT+Left"), true );
    pop->addCommand( "Go Forward", d_edit, SLOT(handleGoForward()), tr("ALT+Right"), true );
    pop->addSeparator();
    pop->addCommand( "Indent", d_edit, SLOT(handleIndent()) );
    pop->addCommand( "Unindent", d_edit, SLOT(handleUnindent()) );
    pop->addCommand( "Fix Indents", d_edit, SLOT(handleFixIndent()) );
    pop->addCommand( "Set Indentation Level...", d_edit, SLOT(handleSetIndent()) );
    pop->addSeparator();
    pop->addCommand( "Print...", d_edit, SLOT(handlePrint()), tr("CTRL+P"), true );
    pop->addCommand( "Export PDF...", d_edit, SLOT(handleExportPdf()), tr("CTRL+SHIFT+P"), true );
    pop->addSeparator();
    pop->addCommand( "Set &Font...", d_edit, SLOT(handleSetFont()) );
    pop->addCommand( "Show &Linenumbers", d_edit, SLOT(handleShowLinenumbers()) );
    pop->addCommand( "Show Fullscreen", this, SLOT(onFullScreen()) );
    pop->addSeparator();
    pop->addAction(tr("Quit"),qApp,SLOT(quit()), tr("CTRL+Q") );

    new QShortcut(tr("CTRL+Q"),this,SLOT(close()));
    new Gui::AutoShortcut( tr("CTRL+O"), this, this, SLOT(onOpen()) );
    new Gui::AutoShortcut( tr("CTRL+N"), this, this, SLOT(onNew()) );
    new Gui::AutoShortcut( tr("CTRL+O"), this, this, SLOT(onOpen()) );
    new Gui::AutoShortcut( tr("CTRL+S"), this, this, SLOT(onSave()) );
    new Gui::AutoShortcut( tr("CTRL+R"), this, this, SLOT(onRun()) );
    new Gui::AutoShortcut( tr("CTRL+SHIFT+R"), this, this, SLOT(onRun2()) );
    new Gui::AutoShortcut( tr("CTRL+T"), this, this, SLOT(onParse()) );
}

void LjEditor::onParse()
{
    ENABLED_IF(true);
    compile();
}

void LjEditor::onRun()
{
    ENABLED_IF(true);
    compile();
#ifndef USE_LJBC_GEN
    d_lua->addSourceLib( d_luaCode, d_moduleName );
    //d_lua->executeCmd( d_luaCode, d_edit->getPath().toUtf8() );
#else
    // d_lua->addSourceLib( d_luaBc, d_moduleName );
    d_lua->executeCmd( d_luaBc, d_edit->getPath().toUtf8() );
#endif
}

void LjEditor::onRun2()
{
    ENABLED_IF(true);
    compile();
#ifndef USE_LJBC_GEN
    QDir dir( QStandardPaths::writableLocation(QStandardPaths::TempLocation) );
    const QString path = dir.absoluteFilePath(QDateTime::currentDateTime().toString("yyMMddhhmmsszzz")+".bc");
    d_lua->saveBinary(d_luaCode, d_edit->getPath().toUtf8(),path.toUtf8());
    JitBytecode bc;
    if( bc.parse(path) )
        d_eng->run( &bc );
    dir.remove(path);
#else
    QBuffer buf( &d_luaBc );
    buf.open(QIODevice::ReadOnly);
    JitBytecode bc;
    if( bc.parse(&buf,d_moduleName) )
        d_eng->run( &bc );
#endif
}

void LjEditor::onNew()
{
    ENABLED_IF(true);

    if( !checkSaved( tr("New File")) )
        return;

    d_edit->newFile();
    onCaption();
}

void LjEditor::onOpen()
{
    ENABLED_IF( true );

    if( !checkSaved( tr("New File")) )
        return;

    const QString fileName = QFileDialog::getOpenFileName(this, tr("Open File"),QString(),
                                                          tr("Oberon Files (*.Mod *.obn)") );
    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    d_edit->loadFromFile(fileName);
    onCaption();

    compile();
}

void LjEditor::onSave()
{
    ENABLED_IF( d_edit->isModified() );

    if( !d_edit->getPath().isEmpty() )
        d_edit->saveToFile( d_edit->getPath() );
    else
        onSaveAs();
}

void LjEditor::onSaveAs()
{
    ENABLED_IF(true);

    QString fileName = QFileDialog::getSaveFileName(this, tr("Save File"),
                                                          QFileInfo(d_edit->getPath()).absolutePath(),
                                                          tr("Oberon Files (*.Mod *.obn)") );

    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    if( !fileName.endsWith(".Mod",Qt::CaseInsensitive ) && !fileName.endsWith(".obn",Qt::CaseInsensitive ) )
        fileName += ".Mod";

    d_edit->saveToFile(fileName);
    onCaption();
}

void LjEditor::onCaption()
{
    if( d_edit->getPath().isEmpty() )
    {
        setWindowTitle(tr("<unnamed> - %1").arg(qApp->applicationName()));
    }else
    {
        QFileInfo info(d_edit->getPath());
        QString star = d_edit->isModified() ? "*" : "";
        setWindowTitle(tr("%1%2 - %3").arg(info.fileName()).arg(star).arg(qApp->applicationName()) );
    }
}

void LjEditor::onGotoLnr(quint32 lnr)
{
    if( d_lock )
        return;
    d_lock = true;
    if( RowCol::isPacked(lnr) )
        d_edit->setCursorPosition(RowCol::unpackRow(lnr)-1,RowCol::unpackCol(lnr)-1);
    else
        d_edit->setCursorPosition(lnr-1,0);
    d_lock = false;
}

void LjEditor::onFullScreen()
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

void LjEditor::onCursor()
{
    if( d_lock )
        return;
    d_lock = true;
    QTextCursor cur = d_edit->textCursor();
    const int line = cur.blockNumber() + 1;
    d_bcv->gotoLine(RowCol(line,cur.positionInBlock() + 1).packed());
    d_lock = false;
}

void LjEditor::onExportBc()
{
    ENABLED_IF(true);
    QString fileName = QFileDialog::getSaveFileName(this, tr("Save Binary"),
                                                          d_edit->getPath(),
                                                          tr("*.ljbc") );

    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    if( !fileName.endsWith(".ljbc",Qt::CaseInsensitive ) )
        fileName += ".ljbc";
#ifndef USE_LJBC_GEN
    d_lua->saveBinary(d_edit->toPlainText().toUtf8(), d_edit->getPath().toUtf8(),fileName.toUtf8());
#else
    QFile out(fileName);
    out.open(QIODevice::WriteOnly);
    out.write(d_luaBc);
#endif
}

void LjEditor::onExportAsm()
{
    ENABLED_IF(true);

    if( d_bcv->topLevelItemCount() == 0 )
        onParse();
    if( d_bcv->topLevelItemCount() == 0 )
        return;

    QString fileName = QFileDialog::getSaveFileName(this, tr("Save Assembler"),
                                                          d_edit->getPath(),
                                                          tr("*.ljasm") );

    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    if( !fileName.endsWith(".ljasm",Qt::CaseInsensitive ) )
        fileName += ".ljasm";

    d_bcv->saveTo(fileName);
}

void LjEditor::onExportLua()
{
    ENABLED_IF(!d_luaCode.isEmpty());

    QString fileName = QFileDialog::getSaveFileName(this, tr("Save Lua Source"),
                                                          d_edit->getPath(),
                                                          tr("*.lua") );

    if (fileName.isEmpty())
        return;

    QDir::setCurrent(QFileInfo(fileName).absolutePath());

    if( !fileName.endsWith(".lua",Qt::CaseInsensitive ) )
        fileName += ".lua";
    QFile out(fileName);
    if( !out.open(QIODevice::WriteOnly) )
        qCritical() << "cannot write to" << fileName;
    else
        out.write( d_luaCode );
}

bool LjEditor::checkSaved(const QString& title)
{
    if( d_edit->isModified() )
    {
        switch( QMessageBox::critical( this, title, tr("The file has not been saved; do you want to save it?"),
                               QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel, QMessageBox::Yes ) )
        {
        case QMessageBox::Yes:
            if( !d_edit->getPath().isEmpty() )
                return d_edit->saveToFile(d_edit->getPath());
            else
            {
                const QString path = QFileDialog::getSaveFileName( this, title, QString(), "Oberon Files (*.Mod *.obn)" );
                if( path.isEmpty() )
                    return false;
                QDir::setCurrent(QFileInfo(path).absolutePath());
                return d_edit->saveToFile(path);
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

void LjEditor::compile()
{
    return; // TODO
    QString path = d_edit->getPath();
    if( path.isEmpty() )
        path = "<unnamed>";

    if( d_useGen == Gen2 )
    {
        Ast::Model mdl;
        mdl.setSenseExt(true);
        mdl.getFc()->addFile(path,d_edit->toPlainText().toUtf8() );
        mdl.getErrs()->setReportToConsole(true);
        preloadLib(mdl,"In");
        preloadLib(mdl,"Out");
        preloadLib(mdl,"Files");
        preloadLib(mdl,"Input");
        preloadLib(mdl,"Math");
        preloadLib(mdl,"Strings");
        preloadLib(mdl,"Coroutines");
        preloadLib(mdl,"XYPlane");
        mdl.parseFiles(QStringList() << path );

        if( mdl.getErrs()->getErrCount() != 0 )
            return;
        Ast::Model::Modules mods = mdl.getModules();
        Ast::Module* mod = 0;
        for( int i = 0; i < mods.size(); i++ )
        {
            if( mods[i]->d_file == path )
            {
                mod = mods[i].data();
                break;
            }
        }

        Q_ASSERT( mod != 0 );
        d_hl->setEnableExt(mod->d_useExt);
        d_moduleName = mod->d_name;

        QBuffer buf;
        buf.open(QIODevice::WriteOnly);
#ifndef USE_LJBC_GEN
        Ob::LuaGen2::translate(mod, &buf);
        buf.close();
        d_luaCode = buf.buffer();
        toByteCode();
#else
        Ob::LjbcGen::translate(mod, &buf);
        buf.close();
        d_luaBc = buf.buffer();
        d_luaCode.clear();
        buf.open(QIODevice::ReadOnly);
        d_bcv->loadFrom(&buf);
#endif
#if 0
        QTextStream out( stdout );
        Ast::Eval::render( out, mod );
#endif

    }else
    {
        CodeModel mdl;
        mdl.setSenseExt(true);
        mdl.setSynthesize(false);
        mdl.setTrackIds(false);
        mdl.getFc()->addFile(path,d_edit->toPlainText().toUtf8() );
        mdl.parseFiles( QStringList() << path );

        if( mdl.getErrs()->getErrCount() != 0 )
            return;

        Q_ASSERT( mdl.getGlobalScope().d_mods.size() == 1 );
        const CodeModel::Module* m = mdl.getGlobalScope().d_mods.first();

        d_hl->setEnableExt(m->d_isExt);
        d_moduleName = m->d_name;

        LuaGen gen(&mdl);
        d_luaCode = gen.emitModule(m);
        toByteCode();
    }
}

void LjEditor::toByteCode()
{
    if( !d_luaCode.isEmpty() )
    {
        if( d_lua->pushFunction(d_luaCode) )
        {
            QByteArray code = Engine2::getBinaryFromFunc( d_lua->getCtx() );
            d_lua->pop();
            QBuffer buf(&code);
            buf.open(QIODevice::ReadOnly);
            d_bcv->loadFrom(&buf);
        }
    }else
        d_bcv->clear();
}

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    a.setOrganizationName("me@rochus-keller.ch");
    a.setOrganizationDomain("github.com/rochus-keller/Oberon");
    a.setApplicationName("ObnLjEditor");
    a.setApplicationVersion("0.6.0");
    a.setStyle("Fusion");

    LjEditor w;

    if( a.arguments().size() > 1 )
        w.loadFile(a.arguments()[1] );

    return a.exec();
}
