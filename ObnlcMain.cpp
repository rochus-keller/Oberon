/*
* Copyright 2019 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon to Lua (OBNLC) transpiler.
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
#include <QThread>
#include "ObCodeModel.h"
#include "ObLuaGen.h"
#include "ObErrors.h"
#ifdef OBNLC_USING_LUAJIT
#include <LjTools/Engine2.h>
#include "ObLjLib.h"
#endif

static QStringList collectFiles( const QDir& dir )
{
    QStringList res;
    QStringList files = dir.entryList( QDir::Dirs | QDir::NoDotAndDotDot, QDir::Name );

    foreach( const QString& f, files )
        res += collectFiles( QDir( dir.absoluteFilePath(f) ) );

    files = dir.entryList( QStringList() << QString("*.Mod")
                                           << QString("*.mod")
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

static bool preloadLib( Ob::CodeModel& mdl, const QByteArray& name )
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

static void loadLuaLib( Lua::Engine2& lua, const QByteArray& name )
{
    QFile obnlj( QString(":/scripts/%1.lua").arg(name.constData()) );
    obnlj.open(QIODevice::ReadOnly);
    if( !lua.addSourceLib( obnlj.readAll(), name ) )
        qCritical() << "compiling" << name << ":" << lua.getLastError();
}

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);

    a.setOrganizationName("Rochus Keller");
    a.setOrganizationDomain("https://github.com/rochus-keller/Oberon");
    a.setApplicationName("OBNLC");
    a.setApplicationVersion("2019-10-13");

    QTextStream out(stdout);
    out << "OBNLC version: " << a.applicationVersion() <<
                 " author: me@rochus-keller.ch  license: GPL" << endl;

    QStringList dirOrFilePaths;
    QString outPath;
    bool dump = false;
    QString mod;
    QString run;
    int n = 1;
    bool forceObnExt = false;
    bool ok;
    const QStringList args = QCoreApplication::arguments();
    for( int i = 1; i < args.size(); i++ ) // arg 0 enthaelt Anwendungspfad
    {
        if(  args[i] == "-h" || args.size() == 1 )
        {
            out << "usage: OBNLC [options] sources" << endl;
            out << "  reads Oberon sources (files or directories) and translates them to corresponding Lua code." << endl;
            out << "options:" << endl;
            out << "  -dst          dump syntax trees to files" << endl;
            out << "  -path=path    path where to save generated files (default like first source)" << endl;
            out << "  -dir=name     directory of the generated files (default empty)" << endl;
#ifdef OBNLC_USING_LUAJIT
            out << "  -run=A[.B]    run module A or procedure B in module A and quit" << endl;
            out << "  -n=x          number of times to run A or A.B" << endl;
#endif
            out << "  -ext          force Oberon extensions (default autosense)" << endl;
            out << "  -h        display this information" << endl;
            return 0;
        }else if( args[i] == "-dst" )
            dump = true;
        else if( args[i].startsWith("-path=") )
            outPath = args[i].mid(6);
        else if( args[i].startsWith("-n=") )
        {
            n = args[i].mid(3).toUInt(&ok);
            if( !ok )
            {
                qCritical() << "invalid -n value";
                return -1;
            }
        }else if( args[i].startsWith("-run=") )
            run = args[i].mid(5);
        else if( args[i] == "-ext" )
            forceObnExt = true;
        else if( args[i].startsWith("-dir=") )
            mod = args[i].mid(5);
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

    QStringList files;
    foreach( const QString& path, dirOrFilePaths )
    {
        QFileInfo info(path);
        if( outPath.isEmpty() )
            outPath = info.isDir() ? info.absoluteFilePath() : info.absolutePath();
        if( info.isDir() )
            files += collectFiles( info.absoluteFilePath() );
        else
            files << path;
    }

    qDebug() << "processing" << files.size() << "files...";
    Ob::CodeModel m;
    m.getErrs()->setRecord(false);
    m.setSynthesize(false);
    if( forceObnExt )
        m.setEnableExt(true);
    else
        m.setSenseExt(true);
    preloadLib(m,"In");
    preloadLib(m,"Out");
    preloadLib(m,"Files");
    preloadLib(m,"Input");
    preloadLib(m,"Math");
    preloadLib(m,"Strings");
    preloadLib(m,"Coroutines");
    preloadLib(m,"XYPlane");
    ok = m.parseFiles(files);

    if( dump )
    {
        qDebug() << "dumping module syntax trees to files...";
        foreach( const Ob::CodeModel::Module* o, m.getGlobalScope().d_mods )
        {
            if( o->d_def == 0 )
                continue;
            QFileInfo fi(o->d_def->d_tok.d_sourcePath );
            QDir dir = fi.dir();
            if( !mod.isEmpty() )
            {
                dir.mkpath(mod);
                dir.cd(mod);
            }
            QFile out( dir.absoluteFilePath( fi.baseName() + ".txt") );
            if( !out.open(QIODevice::WriteOnly) )
            {
                qDebug() << "error: cannot open dump file for writing" << out.fileName();
                continue;
            }
            QTextStream ts(&out);
            Ob::CodeModel::dump(ts,o->d_def);
        }
    }

    if( ok )
    {
        qDebug() << "generating files...";
        Ob::LuaGen g(&m);
        g.emitModules(outPath,mod);
    }

    if( m.getErrs()->getErrCount() == 0 && m.getErrs()->getWrnCount() == 0 )
        qDebug() << "files successfully generated";
    else
    {
        qDebug() << "completed with" << m.getErrs()->getErrCount() << "errors and" <<
                    m.getErrs()->getWrnCount() << "warnings";
        return -1;
    }

#ifdef OBNLC_USING_LUAJIT
    if( !run.isEmpty() )
    {
        QByteArrayList modProc = run.toUtf8().split('.');
        Q_ASSERT( !modProc.isEmpty() );
        for( int i = 0; i < modProc.size(); i++ )
            modProc[i] = Ob::LuaGen::escape(modProc[i]);

        if( dynamic_cast<const Ob::CodeModel::Module*>(
                    m.getGlobalScope().findByName(modProc.first())) == 0 )
        {
            qCritical() << "cannot run" << run << ", unknown module";
            return -1;
        }

        qDebug() << "";
        qDebug() << "running" << run << flush;

        if( !mod.isEmpty() )
            QDir::setCurrent(QDir(outPath).absoluteFilePath(mod) );
        else
            QDir::setCurrent(outPath);

        Lua::Engine2 lua;
        lua.setPrintToStdout(true);
        lua.addStdLibs();
        lua.addLibrary(Lua::Engine2::PACKAGE);
        lua.addLibrary(Lua::Engine2::IO);
        lua.addLibrary(Lua::Engine2::DBG);
        lua.addLibrary(Lua::Engine2::BIT);
        lua.addLibrary(Lua::Engine2::JIT);
        lua.addLibrary(Lua::Engine2::OS);
        Ob::LjLib::install(lua.getCtx());
        loadLuaLib( lua, "obnlj" );
        loadLuaLib(lua,"In");
        loadLuaLib(lua,"Out");
        loadLuaLib(lua,"Files");
        loadLuaLib(lua,"Input");
        loadLuaLib(lua,"Math");
        loadLuaLib(lua,"Strings");
        loadLuaLib(lua,"Coroutines");
        loadLuaLib(lua,"XYPlane");
        Lua::Engine2::setInst(&lua);
        QByteArray src;
        QTextStream out(&src);
        out << "print(\">>> starting \".._VERSION..\" on \"..jit.version)" << endl;

        //out << "jit.off()" << endl;
        //out << "jit.opt.start(3)" << endl;
        //out << "jit.opt.start(\"-abc\")" << endl;
        //out << "jit.opt.start(\"-fuse\")" << endl;
        //out << "jit.opt.start(\"hotloop=10\", \"hotexit=2\")" << endl;

        out << "print(\"LuaJIT status:\",jit.status())" << endl;
        out << "local " << modProc.first() << " = require '" << modProc.first() << "'" << endl;
        for( int i = 0; i < n; i++ )
        {
            if( modProc.size() > 1 )
                out << modProc.join('.') << "()" << endl;
        }
        out.flush();
        lua.executeCmd(src,"terminal");
        printf(">>> finished\n");
    }
#endif

    return 0;
}
