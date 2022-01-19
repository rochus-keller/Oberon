/*
* Copyright 2021 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon to Mono CLI compiler.
*
* The following is the license that applies to this copy of the
* application. For a license to use the application under conditions
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

#include <QCoreApplication>
#include <QFile>
#include <QtDebug>
#include <QFileInfo>
#include <QDir>
#include <QDateTime>
#include <QProcess>
#include "ObxModel.h"
#include "ObErrors.h"
#include "ObxProject.h"
#include "ObxCilGen.h"
#include "ObFileCache.h"

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

static bool preloadLib( Obx::Project* pro, const QByteArray& name )
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

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);

    a.setOrganizationName("Rochus Keller");
    a.setOrganizationDomain("https://github.com/rochus-keller/Oberon");
    a.setApplicationName("OBXMC");
    a.setApplicationVersion("2022-01-19");

    QTextStream out(stdout);
    QTextStream err(stderr);
    out << "OBXMC version: " << a.applicationVersion() <<
                 " author: me@rochus-keller.ch  license: GPL" << endl;


    Obx::Project pro;

    QStringList dirOrFilePaths;
    QString outPath;
    QStringList args = QCoreApplication::arguments();
    bool genAsm = false;
    bool run = false;
    bool build = false;
    bool debug = false;
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
            out << "usage: OBXMC [options] files or directory" << endl;
            out << "  reads Oberon+ source or project files and compiles them to assemblies or IL assembler." << endl;
            out << "options:" << endl;
            out << "  -h            display this information" << endl;
            out << "  -out=path     path where to save generated files" << endl;
            out << "  -asm          generate IL assembler (binary assemblies otherwise)" << endl;
            out << "  -debug        generate debug information" << endl;
            out << "  -build        run the generated build.sh script (Linux only)" << endl;
            out << "  -run          run the generated run.sh script (Linux only)" << endl;
            out << "  the following options are overridden if a project file is loaded" << endl;
            out << "  -main=A[.B]   run module A or procedure B in module A and quit" << endl;
            out << "  -oak          use built-in oakwood definitions" << endl;
            out << "  -obs          use built-in Oberon System backend definitions" << endl;
            return 0;
        }else if( args[i] == "-oak" )
            pro.setUseBuiltInOakwood(true);
        else if( args[i] == "-obs" )
            pro.setUseBuiltInObSysInner(true);
        else if( args[i] == "-asm" )
            genAsm = true;
        else if( args[i] == "-run" )
            run = true;
        else if( args[i] == "-debug" )
            debug = true;
        else if( args[i] == "-build" )
            build = true;
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
            pro.setMain(modProc);
        }else if( !args[ i ].startsWith( '-' ) )
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

#if 0 // #ifndef _DEBUG
    if( !genAsm )
        qWarning() << "the direct assembly generator is currently only available in debug mode (i.e. not ready for use)";
    genAsm = true;
#endif
    Obx::PackageList pl;
    Obx::Package p;
    QString pfile;
    foreach( const QString& path, dirOrFilePaths )
    {
        QFileInfo info(path);
        if( info.isDir() )
        {
            const QStringList tmp = collectFiles( info.absoluteFilePath() );
            p.d_files += tmp;
        }else
        {
            if( pfile.isEmpty() && ( path.endsWith(".obxpro") || path.endsWith(".obnpro") ) )
                pfile = path;
            else
                p.d_files << path;
        }
    }
    pl << p;
    if( !pfile.isEmpty() )
    {
        if( !p.d_files.isEmpty() )
        {
            err << "expecting either a project file or source files/directories, but not both" << endl;
            return -1;
        }
        qDebug() << "loading project" << pfile;
        if( !pro.loadFrom(pfile) ) // This overrides most command line settings!
            return -1;
    }else
    {
        qDebug() << "processing" << p.d_files.size() << "files...";
        pro.initializeFromPackageList(pl);
    }

    if( pro.useBuiltInOakwood() )
    {
        preloadLib(&pro,"In");
        preloadLib(&pro,"Out");
        preloadLib(&pro,"Files");
        preloadLib(&pro,"Input");
        preloadLib(&pro,"Math");
        preloadLib(&pro,"MathL");
        preloadLib(&pro,"Strings");
        preloadLib(&pro,"Coroutines");
        preloadLib(&pro,"XYPlane");
    }

    QTime start = QTime::currentTime();
    if( !pro.reparse() )
        return -1;
    qDebug() << "recompiled in" << start.msecsTo(QTime::currentTime()) << "[ms]";
    start = QTime::currentTime();
    Obx::CilGen::How how;
    if( genAsm )
        how = Obx::CilGen::Ilasm;
    else
        how = Obx::CilGen::Pelib;
    Obx::CilGen::translateAll(&pro, how, debug, outPath );
    qDebug() << "translated in" << start.msecsTo(QTime::currentTime()) << "[ms]";
    QDir::setCurrent(outPath);
    QDir dir(outPath);
    if( build && genAsm )
    {
        start = QTime::currentTime();
        if( QProcess::execute(dir.absoluteFilePath("build.sh")) < 0 )
            return -1;
        qDebug() << "built with ilasm in" << start.msecsTo(QTime::currentTime()) << "[ms]";
    }
    if( run )
    {
        if( QProcess::execute(dir.absoluteFilePath("run.sh")) < 0 )
            return -1;
    }

    return 0;
}
