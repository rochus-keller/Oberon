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
#include "ObxModel.h"
#include "ObErrors.h"
#include "ObxPelibGen.h"

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

static bool preloadLib( Obx::Model& mdl, const QByteArray& name )
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

static int docompile2(const Obx::Model::FileGroups& files,
                      const QString& outPath, bool forceObnExt, bool useOakwood, bool dump)
{
    Obx::Model model;
    model.getErrs()->setReportToConsole(true);

    if( useOakwood )
    {
        preloadLib(model,"In");
        preloadLib(model,"Out");
        preloadLib(model,"Files");
        preloadLib(model,"Input");
        preloadLib(model,"Math");
        preloadLib(model,"Strings");
        preloadLib(model,"Coroutines");
        preloadLib(model,"XYPlane");
    }

    model.parseFiles(files);

    qDebug() << "parsed" << model.getSloc() << "physical lines (LOC, no whitespace or comment only) in total";
    if( model.getErrs()->getErrCount() == 0 && model.getErrs()->getWrnCount() == 0 )
        qDebug() << "no errors or warnings found";
    else
    {
        qDebug() << "completed with" << model.getErrs()->getErrCount() << "errors and" <<
                    model.getErrs()->getWrnCount() << "warnings";
    }
#ifdef _DEBUG_
        qDebug() << "things count peak" << Obx::Thing::insts.size();
#endif

    if( model.getErrs()->getErrCount() != 0 )
        return -1;

    qDebug() << "generating files using gen=4 ...";

    Obx::PelibGen::translate(&model, outPath);

    return 0;
}

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);

    a.setOrganizationName("Rochus Keller");
    a.setOrganizationDomain("https://github.com/rochus-keller/Oberon");
    a.setApplicationName("OBXMC");
    a.setApplicationVersion("2021-04-11");

    QTextStream out(stdout);
    out << "OBXMC version: " << a.applicationVersion() <<
                 " author: me@rochus-keller.ch  license: GPL" << endl;

    QStringList dirOrFilePaths;
    QString outPath;
    bool dump = false;
    QString mod;
    QString run;
    int n = 1;
    bool forceObnExt = false;
    bool useOakwood = false;
    bool ok;
    const QStringList args = QCoreApplication::arguments();
    for( int i = 1; i < args.size(); i++ ) // arg 0 enthaelt Anwendungspfad
    {
        if(  args[i] == "-h" || args.size() == 1 )
        {
            out << "usage: OBXMC [options] sources" << endl;
            out << "  reads Oberon sources (files or directories) and translates them to corresponding assemblies." << endl;
            out << "options:" << endl;
            out << "  -dst          dump syntax trees to files" << endl;
            out << "  -path=path    path where to save generated files (default like first source)" << endl;
            out << "  -dir=name     directory of the generated files (default empty)" << endl;
            out << "  -ext          force Oberon extensions (default autosense)" << endl;
            out << "  -oak          use built-in oakwood definitions" << endl;
            out << "  -h            display this information" << endl;
            return 0;
        }else if( args[i] == "-dst" )
            dump = true;
        else if( args[i] == "-oak" )
                    useOakwood = true;
        else if( args[i].startsWith("-path=") )
            outPath = args[i].mid(6);
        else if( args[i].startsWith("-run=") )
            run = args[i].mid(5);
        else if( args[i] == "-run" )
            run = "?";
        else if( args[i].startsWith("-n=") )
        {
            n = args[i].mid(3).toUInt(&ok);
            if( !ok )
            {
                qCritical() << "invalid -n value";
                return -1;
            }
        }
        else if( args[i] == "-ext" )
            forceObnExt = true;
        else if( !args[ i ].startsWith( '-' ) )
        {
            dirOrFilePaths += args[ i ];
        }else
        {
            qCritical() << "error: invalid command line option " << args[i] << endl;
            return -1;
        }
    }
    if( dirOrFilePaths.isEmpty() )
    {
        qWarning() << "no file or directory to process; quitting (use -h option for help)" << endl;
        return -1;
    }

    Obx::Model::FileGroups fgs;
    fgs << Obx::Model::FileGroup();
    QStringList files;
    foreach( const QString& path, dirOrFilePaths )
    {
        QFileInfo info(path);
        if( outPath.isEmpty() )
            outPath = info.isDir() ? info.absoluteFilePath() : info.absolutePath();
        if( info.isDir() )
        {
            const QStringList tmp = collectFiles( info.absoluteFilePath() );
            files += tmp;
            fgs.back().d_files += tmp;
        }else
        {
            files << path;
            fgs.back().d_files << path;
        }
    }

    qDebug() << "processing" << files.size() << "files...";

    const int res = docompile2(fgs,outPath,forceObnExt,useOakwood,dump);
    if( res < 0 )
        return res;

    // return dorun( files, run, mod, outPath, useOakwood, n );

    return 0;
}
