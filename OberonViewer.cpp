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

#include "NamedThingsMdl.h"
#include "ObCppGen.h"
#include "OberonViewer.h"
#include "ObnHighlighter.h"
#include "ObLexer.h"
#include "ObObxGen.h"
#include <QDockWidget>
#include <QFile>
#include <QPainter>
#include <QPlainTextEdit>
#include <QTreeWidget>
#include <QtDebug>
#include <QApplication>
#include <QSettings>
#include <QFileInfo>
#include <QVBoxLayout>
#include <QLabel>
#include <QShortcut>
#include <QInputDialog>
#include <QDir>
#include <QFileDialog>
#include <QDate>
#include <algorithm>
#include <QFormLayout>
#include <QDialogButtonBox>
#include <QBuffer>
using namespace Ob;

Q_DECLARE_METATYPE(Ob::CodeModel::Module*)
Q_DECLARE_METATYPE(const Ob::SynTree*)

static inline int positionInBlock( const QTextCursor& cur )
{
#if QT_VERSION > 0x050000
	return cur.positionInBlock();
#else
	return cur.position() - cur.block().position();
#endif
}

class OberonViewer::Viewer : public QPlainTextEdit
{
public:
    QString d_path;
    typedef QList<QTextEdit::ExtraSelection> ESL;
    ESL d_link;
    const CodeModel::NamedThing* d_goto;
    ESL d_nonTerms;
    OberonViewer* d_that;
    Highlighter* d_hl;
    QString d_find;

    Viewer(OberonViewer* p):QPlainTextEdit(p),d_goto(0),d_that(p)
    {
        setReadOnly(true);
        setLineWrapMode( QPlainTextEdit::NoWrap );
        setTabStopWidth( 30 );
        setTabChangesFocus(true);
        setMouseTracking(true);
        d_hl = new Highlighter( document() );
        QFont f;
        f.setStyleHint( QFont::TypeWriter );
        f.setFamily("Mono");
        f.setPointSize(9);
        setFont(f);
    }
    OberonViewer* that() { return d_that; }

    bool loadFile( const QString& path )
    {
        if( d_path == path )
            return true;
        d_path = path;
        bool isExt = false;
        foreach( const CodeModel::Module* m, d_that->d_mdl->getGlobalScope().d_mods )
        {
            if( m->d_def && m->d_def->d_tok.d_sourcePath == path )
            {
                isExt = m->d_isExt;
                break;
            }
        }

        QFile in(d_path);
        if( !in.open(QIODevice::ReadOnly) )
            return false;

        d_hl->setEnableExt(isExt);
        if( Lexer::isV4File(&in) )
        {
            setPlainText( Lexer::readV4Text(&in) );
        }else if( Lexer::skipOberonHeader(&in) )
        {
            QBuffer buf;
            buf.buffer() = in.readAll();
            buf.buffer().replace( '\r', '\n' );
            buf.open(QIODevice::ReadOnly);
            setPlainText( QString::fromLatin1(buf.readAll()) );
        }else
            setPlainText( QString::fromLatin1(in.readAll()) );
        return true;
    }

    void mouseMoveEvent(QMouseEvent* e)
    {
        QPlainTextEdit::mouseMoveEvent(e);
        if( QApplication::keyboardModifiers() == Qt::ControlModifier )
        {
            QTextCursor cur = cursorForPosition(e->pos());
			CodeModel::IdentUse id = that()->d_mdl->findSymbolBySourcePos(d_path,cur.blockNumber() + 1,
																		  positionInBlock(cur) + 1);
            const bool alreadyArrow = !d_link.isEmpty();
            d_link.clear();
            if( id.first )
            {
				const int off = positionInBlock(cur) + 1 - id.first->d_tok.d_colNr;
                cur.setPosition(cur.position() - off);
                cur.setPosition( cur.position() + id.first->d_tok.d_len, QTextCursor::KeepAnchor );
                const CodeModel::NamedThing* d = id.second;

                if( d->d_id )
                {
                    QTextEdit::ExtraSelection sel;
                    sel.cursor = cur;
                    sel.format.setFontUnderline(true);
                    d_link << sel;
                    d_goto = d;
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

    void mousePressEvent(QMouseEvent* e)
    {
        QPlainTextEdit::mousePressEvent(e);
        if( !d_link.isEmpty() )
        {
            QApplication::restoreOverrideCursor();
            d_link.clear();
            Q_ASSERT( d_goto );
            setCursorPosition( d_goto->d_id, true );
        }else if( QApplication::keyboardModifiers() == Qt::ControlModifier )
        {
            QTextCursor cur = cursorForPosition(e->pos());
			CodeModel::IdentUse id = that()->d_mdl->findSymbolBySourcePos(d_path,cur.blockNumber() + 1,positionInBlock(cur) + 1);
            if( id.first && id.second->d_id )
            {
                //pushLocation( Location( cur.blockNumber(), cur.positionInBlock() ) );
                setCursorPosition( id.second->d_id, true );
            }
        }else
            updateExtraSelections();
    }

    void updateExtraSelections()
    {
        ESL sum;

        QTextEdit::ExtraSelection line;
        line.format.setBackground(QColor(Qt::yellow).lighter(150));
        line.format.setProperty(QTextFormat::FullWidthSelection, true);
        line.cursor = textCursor();
        line.cursor.clearSelection();
        sum << line;

        sum << d_nonTerms;

        sum << d_link;

        setExtraSelections(sum);
    }

    void setCursorPosition(const SynTree* id, bool center )
    {
        const int line = id->d_tok.d_lineNr - 1;
        const int col = id->d_tok.d_colNr - 1;
        loadFile( id->d_tok.d_sourcePath );
        // Qt-Koordinaten
        if( line >= 0 && line < document()->blockCount() )
        {
            QTextBlock block = document()->findBlockByNumber(line);
            QTextCursor cur = textCursor();
            cur.setPosition( block.position() + col );
            setTextCursor( cur );
            if( center )
                centerCursor();
            else
                ensureCursorVisible();
        }
    }

    void setCursorPosition(int line, int col, bool center, int sel = -1 )
    {
        // Qt-Koordinaten
        if( line >= 0 && line < document()->blockCount() )
        {
            QTextBlock block = document()->findBlockByNumber(line);
            QTextCursor cur = textCursor();
            cur.setPosition( block.position() + col );
            if( sel > 0 )
                cur.setPosition( block.position() + col + sel, QTextCursor::KeepAnchor );
            setTextCursor( cur );
            if( center )
                centerCursor();
            else
                ensureCursorVisible();
            updateExtraSelections();
        }
    }

    void markNonTermsFromCursor()
    {
        QTextCursor cur = textCursor();
		CodeModel::IdentUse id = that()->d_mdl->findSymbolBySourcePos(d_path,cur.blockNumber() + 1,positionInBlock(cur) + 1);
        if( id.first )
        {
            QList<const SynTree*> syms = that()->d_mdl->findReferencingSymbols( id.second, d_path );
            markNonTerms(syms);
        }
    }

    void markNonTerms(const QList<const SynTree*>& s)
    {
        d_nonTerms.clear();
        QTextCharFormat format;
        format.setBackground( QColor(247,245,243).darker(120) );
        foreach( const SynTree* n, s )
        {
            QTextCursor c( document()->findBlockByNumber( n->d_tok.d_lineNr - 1) );
            c.setPosition( c.position() + n->d_tok.d_colNr - 1 );
            c.setPosition( c.position() + n->d_tok.d_val.size(), QTextCursor::KeepAnchor );

            QTextEdit::ExtraSelection sel;
            sel.format = format;
            sel.cursor = c;

            d_nonTerms << sel;
        }
        updateExtraSelections();
    }

    void find( bool fromTop )
    {
        QTextCursor cur = textCursor();
        int line = cur.block().blockNumber();
		int col = positionInBlock(cur);

        if( fromTop )
        {
            line = 0;
            col = 0;
        }else
            col++;
        const int count = document()->blockCount();
        int pos = -1;
        const int start = qMax(line,0);
        bool turnedAround = false;
        for( int i = start; i < count; i++ )
        {
            pos = document()->findBlockByNumber(i).text().indexOf( d_find, col, Qt::CaseInsensitive );
            if( pos != -1 )
            {
                line = i;
                col = pos;
                break;
            }else if( i < count )
                col = 0;
            if( pos == -1 && start != 0 && !turnedAround && i == count - 1 )
            {
                turnedAround = true;
                i = -1;
            }
        }
        if( pos != -1 )
        {
            setCursorPosition( line, col, true, d_find.size() );
        }
    }

};

static OberonViewer* s_this = 0;
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
			s_this->logMessage(QLatin1String("ERR: ") + message);
			break;
		}
	}
}

#if QT_VERSION > 0x050000
static QtMessageHandler s_oldHandler = 0;
void messageHander(QtMsgType type, const QMessageLogContext& ctx, const QString& message)
{
    if( s_oldHandler )
        s_oldHandler(type, ctx, message );
	report(type,message);
}
#else
static QtMsgHandler s_oldHandler = 0;
void messageHander(QtMsgType type, const char * message)
{
	if( s_oldHandler )
		s_oldHandler(type, message );
	report(type,message);
}
#endif

OberonViewer::OberonViewer(QWidget *parent) : QMainWindow(parent),d_pushBackLock(false),d_cur(0)
{
    s_this = this;

    d_mdl = new Ob::CodeModel(this);
    d_mdl->setSynthesize(true);
    d_mdl->setSenseExt(true);
    d_mdl->setTrackIds(true);

    QWidget* pane = new QWidget(this);
    QVBoxLayout* vbox = new QVBoxLayout(pane);
    vbox->setMargin(0);
    vbox->setSpacing(0);

    d_loc = new QLabel(this);
    d_loc->setMargin(2);
    vbox->addWidget(d_loc);

    d_view = new Viewer(this);
    vbox->addWidget(d_view);

    setCentralWidget(pane);

    setDockNestingEnabled(true);
    setCorner( Qt::BottomRightCorner, Qt::RightDockWidgetArea );
    setCorner( Qt::BottomLeftCorner, Qt::LeftDockWidgetArea );
    setCorner( Qt::TopRightCorner, Qt::RightDockWidgetArea );
    setCorner( Qt::TopLeftCorner, Qt::LeftDockWidgetArea );

    createModuleList();
    createUsedBy();
    createLog();

    connect( d_view, SIGNAL( cursorPositionChanged() ), this, SLOT(  onCursorPositionChanged() ) );

    QSettings s;
    const QVariant state = s.value( "DockState" );
    if( !state.isNull() )
        restoreState( state.toByteArray() );

    new QShortcut(tr("ALT+LEFT"),this,SLOT(onGoBack()) );
    new QShortcut(tr("ALT+RIGHT"),this,SLOT(onGoForward()) );
    new QShortcut(tr("CTRL+Q"),this,SLOT(close()) );
    new QShortcut(tr("CTRL+L"),this,SLOT(onGotoLine()) );
    new QShortcut(tr("CTRL+F"),this,SLOT(onFindInFile()) );
    new QShortcut(tr("CTRL+G"),this,SLOT(onFindAgain()) );
    new QShortcut(tr("F3"),this,SLOT(onFindAgain()) );
    new QShortcut(tr("F2"),this,SLOT(onGotoDefinition()) );
    new QShortcut(tr("CTRL+O"),this,SLOT(onOpen()) );
    new QShortcut(tr("CTRL+T"),this,SLOT(onTranslate()) );
    new QShortcut(tr("CTRL+SHIFT+T"),this,SLOT(onTranslate2()) );

#if QT_VERSION > 0x050000
	s_oldHandler = qInstallMessageHandler(messageHander);
#else
	s_oldHandler = qInstallMsgHandler(messageHander);
#endif

    setWindowTitle( tr("%1 v%2").arg( qApp->applicationName() ).arg( qApp->applicationVersion() ) );

    logMessage(tr("Welcome to %1 %2\nAuthor: %3\nSite: %4\nLicense: GPL\n").arg( qApp->applicationName() )
               .arg( qApp->applicationVersion() ).arg( qApp->organizationName() ).arg( qApp->organizationDomain() ));
    logMessage(tr("Shortcuts:"));
    logMessage(tr("CTRL+O to open the directory containing the Oberon files") );
    logMessage(tr("Double-click on the elements in the Modules or Uses lists to show in source code") );
    logMessage(tr("CTRL-click or F2 on the idents in the source to navigate to declarations") );
    logMessage(tr("CTRL+L to go to a specific line in the source code file") );
    logMessage(tr("CTRL+F to find a string in the current file") );
    logMessage(tr("CTRL+G or F3 to find another match in the current file") );
    logMessage(tr("CTRL+T to translate the Oberon files to C++") );
    logMessage(tr("ALT+LEFT to move backwards in the navigation history") );
    logMessage(tr("ALT+RIGHT to move forward in the navigation history") );
    logMessage(tr("ESC to close Message Log") );
}

void OberonViewer::showFiles(const QStringList& files)
{
    d_msgLog->clear();
    d_mdl->parseFiles(files);
    d_ntm->setSyntax(d_mdl);
    d_usedBy->clear();
    d_view->d_path.clear();
    d_view->clear();
    d_loc->clear();
    d_usedByTitle->clear();
    d_cur = 0;
    d_backHisto.clear();
    d_forwardHisto.clear();
}

void OberonViewer::logMessage(const QString& str)
{
    d_msgLog->parentWidget()->show();
    d_msgLog->appendPlainText(str);
}

QStringList OberonViewer::collectFiles(const QDir& dir)
{
    QStringList res;
    QStringList files = dir.entryList( QDir::Dirs | QDir::NoDotAndDotDot, QDir::Name );

    foreach( const QString& f, files )
        res += collectFiles( QDir( dir.absoluteFilePath(f) ) );

    files = dir.entryList( QStringList() << QString("*.Mod")
                                           << QString("*.mod") << QString("*.Def") << QString("*.def")
                           << QString("*.obn") << QString("*.obx"),
                                           QDir::Files, QDir::Name );
    foreach( const QString& f, files )
    {
        res.append( dir.absoluteFilePath(f) );
    }
    return res;
}

void OberonViewer::createModuleList()
{
    QDockWidget* dock = new QDockWidget( tr("Modules"), this );
    dock->setObjectName("Modules");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable );
    d_things = new QTreeView(dock);
    d_things->setAlternatingRowColors(true);
    d_things->setHeaderHidden(true);
    d_things->setSortingEnabled(false);
    d_things->setAllColumnsShowFocus(true);
    d_things->setRootIsDecorated(true);
    d_things->setExpandsOnDoubleClick(false);
    d_ntm = new NamedThingsMdl(d_things);
    d_things->setModel(d_ntm);
    dock->setWidget(d_things);
    addDockWidget( Qt::LeftDockWidgetArea, dock );
    connect( d_things,SIGNAL(doubleClicked(QModelIndex)), this, SLOT(onModuleDblClick(QModelIndex)) );
}

void OberonViewer::createUsedBy()
{
    QDockWidget* dock = new QDockWidget( tr("Uses"), this );
    dock->setObjectName("UsedBy");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable );
    QWidget* pane = new QWidget(dock);
    QVBoxLayout* vbox = new QVBoxLayout(pane);
    vbox->setMargin(0);
    vbox->setSpacing(0);
    d_usedByTitle = new QLabel(pane);
    d_usedByTitle->setWordWrap(true);
    d_usedByTitle->setMargin(2);
    vbox->addWidget(d_usedByTitle);
    d_usedBy = new QTreeWidget(pane);
    d_usedBy->setAlternatingRowColors(true);
    d_usedBy->setHeaderHidden(true);
    d_usedBy->setSortingEnabled(false);
    d_usedBy->setAllColumnsShowFocus(true);
    d_usedBy->setRootIsDecorated(false);
    vbox->addWidget(d_usedBy);
    dock->setWidget(pane);
    addDockWidget( Qt::RightDockWidgetArea, dock );
    connect(d_usedBy, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)), this, SLOT(onUsedByDblClicked()) );
}

void OberonViewer::createLog()
{
    QDockWidget* dock = new QDockWidget( tr("Message Log"), this );
    dock->setObjectName("Log");
    dock->setAllowedAreas( Qt::AllDockWidgetAreas );
    dock->setFeatures( QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetClosable );
    d_msgLog = new QPlainTextEdit(dock);
    d_msgLog->setReadOnly(true);
    d_msgLog->setLineWrapMode( QPlainTextEdit::NoWrap );
    new LogPainter(d_msgLog->document());
    dock->setWidget(d_msgLog);
    addDockWidget( Qt::BottomDockWidgetArea, dock );
    new QShortcut(tr("ESC"), dock, SLOT(close()) );
}

void OberonViewer::pushLocation(const SynTree* st)
{
    if( d_pushBackLock || st == 0 )
        return;
    if( d_cur == st )
        return;
    if( d_cur )
        d_backHisto.push_back( d_cur );
    d_cur = st;
}

static bool UsedByLessThan( const SynTree* lhs, const SynTree* rhs )
{
    return lhs->d_tok.d_sourcePath < rhs->d_tok.d_sourcePath ||
            (!(rhs->d_tok.d_sourcePath < lhs->d_tok.d_sourcePath) &&
             lhs->d_tok.d_lineNr < rhs->d_tok.d_lineNr );
}

void OberonViewer::fillUsedBy(const SynTree* id, const CodeModel::NamedThing* nt)
{
    d_usedBy->clear();
    if( id )
        d_usedByTitle->setText(QString("%1 '%2'").arg(nt->typeName().data()).arg(id->d_tok.d_val.data()) );
    else if( !nt->d_name.isEmpty() )
        d_usedByTitle->setText(QString("%1 '%2'").arg(nt->typeName().data()).arg(nt->d_name.data()) );
    else
        d_usedByTitle->setText(QString("%1").arg(nt->typeName().data()) );

    QList<const SynTree*> all = d_mdl->findReferencingSymbols( nt );
    std::sort( all.begin(), all.end(), UsedByLessThan );

    typedef QMap< QPair<QString,quint32>, QList<const SynTree*> > Groups;
    Groups groups;

    QList<const SynTree*> part;
    foreach( const SynTree* id, all )
    {
        groups[qMakePair(id->d_tok.d_sourcePath,id->d_tok.d_lineNr)].append(id);
        if( id->d_tok.d_sourcePath == d_view->d_path )
            part << id;
    }

    Groups::const_iterator i;
    QTreeWidgetItem* curItem = 0;
    for( i = groups.begin(); i != groups.end(); ++i )
    {
        const SynTree* st = i.value().first();
        QTreeWidgetItem* item = new QTreeWidgetItem(d_usedBy);
        item->setText( 0, QString("%1 (%2 %3%4)").arg(QFileInfo(st->d_tok.d_sourcePath).fileName())
                    .arg(st->d_tok.d_lineNr).arg( i.value().size() )
                       .arg( st == nt->d_id ? " decl" : "" ) );
        if( id && st->d_tok.d_lineNr == id->d_tok.d_lineNr &&
                st->d_tok.d_sourcePath == id->d_tok.d_sourcePath )
        {
            QFont f = item->font(0);
            f.setBold(true);
            item->setFont(0,f);
            curItem = item;
        }
        item->setToolTip( 0, item->text(0) );
        item->setData( 0, Qt::UserRole, QVariant::fromValue(st) );
        if( st->d_tok.d_sourcePath != d_view->d_path )
            item->setForeground( 0, Qt::gray );
        else if( curItem == 0 )
            curItem = item;
    }
    if( curItem )
        d_usedBy->scrollToItem( curItem );
    if( id )
    {
        QTextCursor tc = d_view->textCursor();
        const int line = tc.blockNumber() + 1;
		const int col = positionInBlock(tc) + 1;
        d_view->markNonTerms(part);
        d_loc->setText( QString("%1   %2:%3   %5 '%4'").arg(d_view->d_path).arg(line).arg(col)
                        .arg(id->d_tok.d_val.data() ).arg(nt->typeName().data() ) );
        pushLocation(id);
    }
}

void OberonViewer::closeEvent(QCloseEvent* event)
{
    QSettings s;
    s.setValue( "DockState", saveState() );
    event->setAccepted(true);
}

void OberonViewer::onModuleDblClick(const QModelIndex& i)
{
    const CodeModel::NamedThing* nt = d_ntm->getSymbol(i);

    if( nt == 0 )
        return;

    if( nt->d_id )
    {
        d_loc->setText(nt->d_id->d_tok.d_sourcePath);
        d_view->setCursorPosition( nt->d_id, true );
    }else
        fillUsedBy( 0, nt );
}

void OberonViewer::onCursorPositionChanged()
{
    QTextCursor cur = d_view->textCursor();
    const int line = cur.blockNumber() + 1;
	const int col = positionInBlock(cur) + 1;
    CodeModel::IdentUse id = d_mdl->findSymbolBySourcePos(d_view->d_path,line,col);
    if( id.first )
    {
        fillUsedBy( id.first, id.second );
    }else
        d_loc->setText( QString("%1   %2:%3").arg(d_view->d_path).arg(line).arg(col) );

}

void OberonViewer::onUsedByDblClicked()
{
    if( d_usedBy->currentItem() == 0 )
        return;

    const SynTree* st = d_usedBy->currentItem()->data(0,Qt::UserRole).value<const SynTree*>();
    if( st == 0 )
        return;
    d_view->setCursorPosition( st, true );
}

void OberonViewer::onGoBack()
{
    if( d_backHisto.isEmpty() )
        return;

    d_pushBackLock = true;
    const SynTree* last = d_backHisto.last();
    d_backHisto.pop_back();
    d_forwardHisto.push_back( d_cur );
    d_cur = last;
    d_view->setCursorPosition( last, true );
    d_pushBackLock = false;
}

void OberonViewer::onGoForward()
{
    if( d_forwardHisto.isEmpty() )
        return;
    const SynTree* last = d_forwardHisto.last();
    d_forwardHisto.pop_back();
    d_backHisto.push_back( d_cur );
    d_cur = last;
    d_view->setCursorPosition( d_cur, true );
}

void OberonViewer::onGotoLine()
{
    QTextCursor cur = d_view->textCursor();
    int line = cur.blockNumber();
	bool ok	= false;
#if QT_VERSION > 0x050000
	line = QInputDialog::getInt(
#else
	line = QInputDialog::getInteger(
#endif
				this, tr("Goto Line"),
        tr("Enter a valid line number:"),
        line + 1, 1, 999999, 1,	&ok );
    if( !ok )
        return;
    QTextBlock block = d_view->document()->findBlockByNumber(line-1);
    cur.setPosition( block.position() );
    d_view->setTextCursor( cur );
    d_view->centerCursor();
    d_view->updateExtraSelections();
}

void OberonViewer::onFindInFile()
{
    bool ok	= false;
    const QString sel = d_view->textCursor().selectedText();
    QString res = QInputDialog::getText( this, tr("Find in File"),
        tr("Enter search string:"), QLineEdit::Normal, sel, &ok );
    if( !ok )
        return;
    d_view->d_find = res;
    d_view->find( sel.isEmpty() );
}

void OberonViewer::onFindAgain()
{
    if( !d_view->d_find.isEmpty() )
        d_view->find( false );
}

void OberonViewer::onGotoDefinition()
{
    QTextCursor cur = d_view->textCursor();
	CodeModel::IdentUse id = d_mdl->findSymbolBySourcePos(d_view->d_path,cur.blockNumber() + 1,positionInBlock(cur) + 1);
    if( id.first && id.second->d_id )
        d_view->setCursorPosition( id.second->d_id, true );
    if( id.second )
    {
        QModelIndex i = d_ntm->findSymbol( id.second );
        if( i.isValid() )
        {
            d_things->setCurrentIndex(i);
            d_things->scrollTo( i ,QAbstractItemView::PositionAtCenter );
        }
    }
}

void OberonViewer::onOpen()
{
    QString path = QFileDialog::getExistingDirectory(this,tr("Open Project Directory"),QDir::currentPath() );
    if( path.isEmpty() )
        return;
    QStringList files = collectFiles(path);
    QDir::setCurrent(path);
    showFiles(files);
}

void OberonViewer::onTranslate()
{
    QDialog dlg(this);
    dlg.setWindowTitle(tr("Translate to C++"));
    QVBoxLayout* vbox = new QVBoxLayout(&dlg);
    QFormLayout* form = new QFormLayout();
    QLabel* intro = new QLabel(&dlg);
    intro->setText(tr("Convert all loaded Oberon files to C++. Note that currently only the\n"
                      "subset of Oberon-07 used by the Lola-2 compiler is supported\n"
                      "and that no garbage collector code is currently generated.\n"
                      "Check for errors or warnings in the log. For stubs only headers are generated.\n\n"
                      "WARNING: existing C++ files are overwritten without warning!") );
    vbox->addWidget(intro);
    vbox->addLayout(form);
    QSettings  s;
    QLineEdit* package = new QLineEdit(&dlg);
    QLineEdit* ns = new QLineEdit(&dlg);
    QLineEdit* path = new QLineEdit(&dlg);
    package->setText(s.value("PackageName",tr("Lolac")).toString());
    ns->setText(s.value("Namespace",tr("Ll")).toString());
    path->setText(s.value("GenerateTo",QDir::currentPath()).toString());
    form->addRow(tr("Package name:"), package );
    form->addRow(tr("Namespace:"), ns );
    form->addRow(tr("Generate to:"), path );
    QDialogButtonBox* bb = new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel, &dlg );
    vbox->addWidget(bb);
    connect(bb, SIGNAL(accepted()), &dlg, SLOT(accept()));
    connect(bb, SIGNAL(rejected()), &dlg, SLOT(reject()));
    if( dlg.exec() != QDialog::Accepted )
        return;
    QString p = package->text().simplified();
    p.replace(' ',"");
    s.setValue( "PackageName",p );
    QString n = ns->text().simplified();
    n.replace(' ',"");
    s.setValue( "Namespace",n );
    s.setValue("GenerateTo", path->text() );

    d_msgLog->clear();
    qDebug() << "generating C++ files...";
    Ob::CppGen g(d_mdl);
    g.emitModules(path->text(),n,p);
    qDebug() << "finished";
}

void OberonViewer::onTranslate2()
{
    QDialog dlg(this);
    dlg.setWindowTitle(tr("Translate to Oberon+"));
    QVBoxLayout* vbox = new QVBoxLayout(&dlg);
    QFormLayout* form = new QFormLayout();
    QLabel* intro = new QLabel(&dlg);
    intro->setText(tr("Convert all loaded Oberon files to Oberon+.\n"
                      "Check for errors or warnings in the log. For stubs only headers are generated.\n\n"
                      "WARNING: existing .obx files are overwritten without warning!") );
    vbox->addWidget(intro);
    vbox->addLayout(form);
    QSettings  s;
    QLineEdit* package = new QLineEdit(&dlg);
    QLineEdit* path = new QLineEdit(&dlg);
    package->setText(s.value("PackageName2",tr("")).toString());
    path->setText(s.value("GenerateTo2",QDir::currentPath()).toString());
    form->addRow(tr("Package name:"), package );
    form->addRow(tr("Generate to:"), path );
    QDialogButtonBox* bb = new QDialogButtonBox(QDialogButtonBox::Ok | QDialogButtonBox::Cancel, &dlg );
    vbox->addWidget(bb);
    connect(bb, SIGNAL(accepted()), &dlg, SLOT(accept()));
    connect(bb, SIGNAL(rejected()), &dlg, SLOT(reject()));
    if( dlg.exec() != QDialog::Accepted )
        return;
    QString p = package->text().simplified();
    p.replace(' ',"");
    s.setValue( "PackageName2",p );
    s.setValue("GenerateTo2", path->text() );

    d_msgLog->clear();
    qDebug() << "generating Oberon+ files...";
    Ob::ObxGen g(d_mdl);
    g.emitModules(path->text(),p);
    qDebug() << "finished";
}



