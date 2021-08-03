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

#include "ObxLjRuntime.h"
#include "ObxAst.h"
#include "ObxProject.h"
#include <LjTools/Engine2.h>
#include <QDir>
#include <QStringList>
#include <QTextStream>
#ifdef QT_GUI_LIB
#include <QApplication>
#include "ObsDisplay.h"
#else
#include <QCoreApplication>
#endif
#include <QtDebug>

static QStringList collectFiles( const QDir& dir )
{
    QStringList res;
    QStringList files = dir.entryList( QDir::Dirs | QDir::NoDotAndDotDot, QDir::Name );

    foreach( const QString& f, files )
        res += collectFiles( QDir( dir.absoluteFilePath(f) ) );

    files = dir.entryList( QStringList() << QString("*.Mod")
                                           << QString("*.mod")
                           << QString("*.obx")
                           << QString("*.Def")
                           << QString("*.def")
                                            << QString("*.obn"),
                                           QDir::Files, QDir::Name );
    foreach( const QString& f, files )
    {
        res.append( dir.absoluteFilePath(f) );
    }
    return res;
}

static QtMessageHandler s_oldHandler = 0;
void messageHander(QtMsgType type, const QMessageLogContext& ctx, const QString& message)
{
    switch(type)
    {
    case QtDebugMsg:
    case QtWarningMsg:
#ifdef _DEBUG_ // TODO
        if( s_oldHandler )
            s_oldHandler(type, ctx, message );
#endif
        break;
    case QtCriticalMsg:
    case QtFatalMsg:
        {
            QTextStream err(stderr);
            err << message << endl;
        }
        break;
    }
}

int main(int argc, char *argv[])
{
#ifdef QT_GUI_LIB
    QApplication a(argc, argv);
#else
    QCoreApplication a(argc, argv);
#endif

    s_oldHandler = qInstallMessageHandler(messageHander);

    a.setOrganizationName("Rochus Keller");
    a.setOrganizationDomain("https://github.com/rochus-keller/Oberon");
    a.setApplicationName("OBXLJ");
    a.setApplicationVersion("2021-08-02");

    QTextStream out(stdout);
    QTextStream err(stderr);
    out << "OBXLJ version: " << a.applicationVersion() <<
                 " author: me@rochus-keller.ch  license: GPL" << endl;


    Obx::LjRuntime rt;
    rt.getLua()->setPrintToStdout(true);

    QStringList dirOrFilePaths;
    QString outPath;
    bool doRun = false;
    bool fromBc = false;
    QStringList args = QCoreApplication::arguments();
    if( args.size() <= 1 )
    {
        // if there are no args look in the application directory for a file called obxljconfig which includes
        // the command line ("" sections not supported); this is useful e.g. on macOS to include the bytecode in the bundle
        QStringList newArgs;
        newArgs.append( args.isEmpty() ? QString(): args.first() );
        QFile config(QDir(QCoreApplication::applicationDirPath()).absoluteFilePath("obxljconfig"));
        if( config.open(QIODevice::ReadOnly) )
            newArgs += QString::fromUtf8(
                        config.readAll().simplified()).split(' '); // RISK: this also affects whitespace included in " "
        args = newArgs;
    }
    for( int i = 1; i < args.size(); i++ ) // arg 0 enthaelt Anwendungspfad
    {
        if(  args[i] == "-h" || args.size() == 1 )
        {
            out << "usage: OBXLJ [options] files or directory" << endl;
            out << "  reads Oberon+ source or project files and runs them." << endl;
            out << "options:" << endl;
            out << "  -out=path     path where to save generated files (default don't generate)" << endl;
            out << "  -run=A[.B]    run module A or procedure B in module A and quit" << endl;
            out << "  -oak          use built-in oakwood definitions" << endl;
            out << "  -obs          use built-in Oberon System backend definitions" << endl;
            out << "  -fsroot=path  Oberon file system root (supports %PRODIR% and %APPDIR%)" << endl;
            out << "  -frombc       run from bytecode (expecting the bytecode directory)" << endl;
            out << "  -h            display this information" << endl;
            return 0;
        }else if( args[i] == "-oak" )
            rt.getPro()->setUseBuiltInOakwood(true);
        else if( args[i] == "-obs" )
            rt.getPro()->setUseBuiltInObSysInner(true);
        else if( args[i].startsWith("-out=") )
        {
            outPath = args[i].mid(5);
            QFileInfo info(outPath);
            if( info.isRelative() )
                outPath = QDir::current().absoluteFilePath(outPath);
        }else if( args[i].startsWith("-run=") )
        {
            QStringList run = args[i].mid(5).split('.');
            if( run.size() > 2 )
            {
                err << "invalid -run option" << endl;
                return -1;
            }
            Obx::Project::ModProc modProc;
            if( run.size() == 2 )
            {
                modProc.first = run[0].toUtf8();
                modProc.second = run[1].toUtf8();
            }else
                modProc.first = run[0].toUtf8();
            doRun = true;
            rt.getPro()->setMain(modProc);
        }else if( args[i] == "-run" )
        {
            Obx::Project::ModProc modProc;
            rt.getPro()->setMain(modProc);
            doRun = true;
        }else if( args[i].startsWith("-fsroot=") )
        {
            rt.getPro()->setWorkingDir(args[i].mid(8));
        }else if( args[i] == "-frombc" )
            fromBc = true;
        else if( !args[ i ].startsWith( '-' ) )
        {
            dirOrFilePaths += args[ i ];
        }else
        {
            err << "error: invalid command line option " << args[i] << endl;
            return -1;
        }
    }
    if( dirOrFilePaths.isEmpty() )
    {
        out << "no file or directory to process; quitting (use -h option for help)" << endl;
        return -1;
    }

    Obx::PackageList pl;
    Obx::Package p;
    QString pro;
    foreach( const QString& path, dirOrFilePaths )
    {
        QFileInfo info(path);
        if( info.isDir() )
        {
            const QStringList tmp = collectFiles( info.absoluteFilePath() );
            p.d_files += tmp;
        }else
        {
            if( pro.isEmpty() && ( path.endsWith(".obxpro") || path.endsWith(".obnpro") ) )
                pro = path;
            else
                p.d_files << path;
        }
    }
    pl << p;
    if( !pro.isEmpty() )
    {
        if( !p.d_files.isEmpty() )
        {
            err << "expecting either a project file or source files/directories, but not both" << endl;
            return -1;
        }
        qDebug() << "loading project" << pro;
        if( !rt.getPro()->loadFrom(pro) )
            return -1;
        if( fromBc )
        {
            err << "-frombc is not allowed when a project file is loaded" << endl;
            return -1;
        }
    }else if( fromBc )
    {
        if( dirOrFilePaths.size() != 1 || !QFileInfo(dirOrFilePaths.first()).isDir() )
        {
            err << "expecting the bytecode directory when -frombc is enabled" << endl;
            return -1;
        }
        if( !outPath.isEmpty() )
        {
            err << "-out invalid when -frombc is enabled" << endl;
            return -1;
        }
        if( !doRun || rt.getPro()->getMain().first.isEmpty() )
        {
            err << "-run with at least a module name required when -frombc is enabled" << endl;
            return -1;
        }
        const char* loader = "for i=#package.loaders,2,-1 do package.loaders[i+1]=package.loaders[i] end\n"
                "package.loaders[2] = function(name) return loadfile(name..'.lua') end";
        rt.getLua()->executeCmd(loader);
    }else
    {
        qDebug() << "processing" << p.d_files.size() << "files...";
        rt.getPro()->initializeFromPackageList(pl);
    }

#ifdef _DEBUG_
        const quint32 before = Obx::Thing::insts.size();
        qDebug() << "things count before" << before;
#endif

#ifdef _DEBUG_
        const quint32 after = Obx::Thing::insts.size();
        qDebug() << "things count after" << after;
        QHash<int,int> counts; // tag -> inst count
        foreach( Obx::Thing* t, Obx::Thing::insts )
            counts[t->getTag()]++;
        QHash<int,int>::const_iterator i;
        for( i = counts.begin(); i != counts.end(); ++i )
            qDebug() << Obx::Thing::s_tagName[i.key()] << i.value();
#endif

    if( !fromBc && !rt.compile(!outPath.isEmpty() || doRun) )
        return -1;

    if( !outPath.isEmpty() )
    {
        if( rt.saveBytecode(outPath) )
            out << "generated bytecode files in " << outPath << endl;
    }

    if( doRun )
    {
        if( !rt.loadLibraries() )
            return -1;
        if( fromBc )
        {
            QDir::setCurrent(dirOrFilePaths.first());
            out << "running bytecode from" << QDir::currentPath() << endl;
        }
        else if( !rt.loadBytecode() )
            return -1;
        if( !rt.executeMain() )
        {
            err << rt.getLua()->getLastError() << endl;
            return -1;
        }
#ifdef QT_GUI_LIB
        if( Obs::Display::isOpen() )
            return a.exec();
        else
            return 0;
#else
        return 0;
#endif
    }else
    {
        if( outPath.isEmpty() )
            out << "finished (didn't run nor generate bytecode)" << endl;
        return 0;
    }
}
