/*
* Copyright 2019, 2020 Rochus Keller <mailto:me@rochus-keller.ch>
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
#include "ObAst.h"
#include "ObAstEval.h"
#include "ObxParser.h"
#ifdef OBNLC_USING_LUAJIT
#include <LjTools/Engine2.h>
#include "ObAstValidator.h"
#include "ObLjLib.h"
#include "ObLjbcGen.h"
#include "ObLuaGen2.h"
#include "ObxModel.h"
#include "ObxValidator.h"
#include "ObxLjbcGen.h"
#include "ObxLibFfi.h"
#endif

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

static bool preloadLib( Ob::Ast::Model& mdl, const QByteArray& name )
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

#ifdef OBNLC_USING_LUAJIT
static void loadLuaLib( Lua::Engine2& lua, const QByteArray& name )
{
    QFile lib( QString(":/runtime/%1.lua").arg(name.constData()) );
    lib.open(QIODevice::ReadOnly);
    if( !lua.addSourceLib( lib.readAll(), name ) )
        qCritical() << "compiling" << name << ":" << lua.getLastError();
}
#endif

static int docompile1(const QStringList& files, int gen, const QString& run, const QString& mod,
                      const QString& outPath, bool forceObnExt, bool useOakwood, bool dump)
{
    Ob::Ast::Model astm;
    astm.getErrs()->setReportToConsole(true);

    if( forceObnExt )
        astm.setEnableExt(true);
    else
        astm.setSenseExt(true);
    if( useOakwood )
    {
        preloadLib(astm,"In");
        preloadLib(astm,"Out");
        preloadLib(astm,"Files");
        preloadLib(astm,"Input");
        preloadLib(astm,"Math");
        preloadLib(astm,"Strings");
        preloadLib(astm,"Coroutines");
        preloadLib(astm,"XYPlane");
    }
    astm.parseFiles(files);

    if( dump )
    {
        qDebug() << "dumping module syntax trees to files...";
        Ob::Ast::Model::Modules mods = astm.getModules();
        for( int m = 0; m < mods.size(); m++ )
        {
            QFileInfo fi(mods[m]->d_file );
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
            Ob::Ast::Eval::render(ts,mods[m].data());
        }
    }

    if( astm.getErrs()->getErrCount() == 0 && astm.getErrs()->getWrnCount() == 0 )
        qDebug() << "files successfully parsed";
    else
    {
        qDebug() << "completed with" << astm.getErrs()->getErrCount() << "errors and" <<
                    astm.getErrs()->getWrnCount() << "warnings";
        return -1;
    }

#if 0
    Ob::Ast::Model::Modules mods = astm.getModules();
    for( int i = 0; i < mods.size(); i++ )
    {
        if( mods[i]->d_file.isEmpty() )
            continue;
        QFileInfo info(mods[i]->d_file);
        QFile f( info.absoluteDir().absoluteFilePath("dump.lua") );
        f.open(QIODevice::WriteOnly);
        Ob::LuaGen2::translate(mods[i].data(),&f,0);
    }
#endif

#ifdef _DEBUG
    if( run.isEmpty() )
    {
        qDebug() << "things count before clear" << Ob::Ast::Thing::insts.size();
        astm.clearclear();
        qDebug() << "things count after clear" << Ob::Ast::Thing::insts.size();
        QHash<int,int> counts; // tag -> inst count
        foreach( Ob::Ast::Thing* t, Ob::Ast::Thing::insts )
            counts[t->getTag()]++;
        QHash<int,int>::const_iterator i;
        for( i = counts.begin(); i != counts.end(); ++i )
            qDebug() << Ob::Ast::Thing::s_tagName[i.key()] << i.value();
        return 0;
    }
#endif

#ifdef OBNLC_USING_LUAJIT
    if( gen == 1 )
    {
        Ob::CodeModel m;
        m.getErrs()->setRecord(false);
        m.setSynthesize(false);
        if( forceObnExt )
            m.setEnableExt(true);
        else
            m.setSenseExt(true);
        if( useOakwood )
        {
            preloadLib(m,"In");
            preloadLib(m,"Out");
            preloadLib(m,"Files");
            preloadLib(m,"Input");
            preloadLib(m,"Math");
            preloadLib(m,"Strings");
            preloadLib(m,"Coroutines");
            preloadLib(m,"XYPlane");
        }
        bool ok = m.parseFiles(files);

        if( ok )
        {
            qDebug() << "generating files using gen=1 ...";
            Ob::LuaGen g(&m);
            g.emitModules(outPath,mod);
        }else
            return -1;
    }else if( gen == 2 )
    {
        qDebug() << "generating files using gen=2 ...";
        Ob::LuaGen2::translate(&astm, outPath,mod);
    }else if( gen == 3 )
    {
        qDebug() << "generating files using gen=3 ...";
        Ob::LjbcGen::translate(&astm, outPath,mod);
    }else
    {
        qCritical() << "invalid generator selected (see -h for more information):" << gen;
        return -1;
    }
#endif
    return 0;
}

static int dorun( const QStringList& files, QString run, const QString& mod,
                  const QString& outPath, bool useOakwood, int n )
{
#ifdef OBNLC_USING_LUAJIT
    if( run == "?" )
    {
        if( files.size() > 1 )
        {
            qCritical() << "cannot apply -run without module name because more than one file was compiled";
            return -1;
        }
        run = QFileInfo(files.first()).baseName();
    }

    if( !run.isEmpty() )
    {
        QByteArrayList modProc = run.toUtf8().split('.');
        Q_ASSERT( !modProc.isEmpty() );
        for( int i = 0; i < modProc.size(); i++ )
            modProc[i] = Ob::LuaGen::escape(modProc[i]);

        /*
        if( dynamic_cast<const Ob::CodeModel::Module*>(
                    m.getGlobalScope().findByName(modProc.first())) == 0 )
        {
            qCritical() << "cannot run" << run << ", unknown module";
            return -1;
        }
        */

        qDebug() << "";
        qDebug() << "running" << run << flush;

        if( !mod.isEmpty() )
            QDir::setCurrent(QDir(outPath).absoluteFilePath(mod) );
        else
            QDir::setCurrent(outPath);

        Lua::Engine2 lua;
        Lua::Engine2::setInst(&lua);
        lua.setPrintToStdout(true);
        lua.addStdLibs();
        lua.addLibrary(Lua::Engine2::PACKAGE);
        lua.addLibrary(Lua::Engine2::IO);
        // lua.addLibrary(Lua::Engine2::DBG);
        lua.addLibrary(Lua::Engine2::BIT);
        lua.addLibrary(Lua::Engine2::JIT);
        lua.addLibrary(Lua::Engine2::OS);
        lua.addLibrary(Lua::Engine2::FFI);
        Ob::LjLib::install(lua.getCtx());
        loadLuaLib( lua, "obnlj" );
        if( useOakwood )
        {
            loadLuaLib(lua,"In");
            loadLuaLib(lua,"Out");
            loadLuaLib(lua,"Files");
            loadLuaLib(lua,"Input");
            loadLuaLib(lua,"Math");
            loadLuaLib(lua,"Strings");
            loadLuaLib(lua,"Coroutines");
            loadLuaLib(lua,"XYPlane");
        }
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

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);

    a.setOrganizationName("Rochus Keller");
    a.setOrganizationDomain("https://github.com/rochus-keller/Oberon");
    a.setApplicationName("OBNLC");
    a.setApplicationVersion("2021-09-03");

    QTextStream out(stdout);
    out << "OBNLC version: " << a.applicationVersion() <<
                 " author: me@rochus-keller.ch  license: GPL" << endl;

    QStringList dirOrFilePaths;
    QString outPath;
    bool dump = false;
    QString mod;
    QString run;
    int n = 1;
    int gen = 3;
    bool forceObnExt = false;
    bool useOakwood = false;
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
            out << "  -gen=n        n=1..thunk for VAR; n=2..multi-return for VAR; n=3..bytecode (default)" << endl;
#endif
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
        else if( args[i].startsWith("-n=") )
        {
            n = args[i].mid(3).toUInt(&ok);
            if( !ok )
            {
                qCritical() << "invalid -n value";
                return -1;
            }
        }
#ifdef OBNLC_USING_LUAJIT
        else if( args[i].startsWith("-run=") )
            run = args[i].mid(5);
        else if( args[i] == "-run" )
            run = "?";
        else if( args[i].startsWith("-gen=") )
            gen = args[i].mid(5).toUInt();
#endif
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
        {
            const QStringList tmp = collectFiles( info.absoluteFilePath() );
            files += tmp;
        }else
        {
            files << path;
        }
    }

    qDebug() << "processing" << files.size() << "files...";

    if( gen < 4 )
    {
        if( docompile1(files,gen,run,mod,outPath,forceObnExt,useOakwood,dump) < 0 )
            return -1;
    }else
    {
        qCritical() << "invalid generator selected (see -h for more information):" << gen;
        return -1;
    }

    return dorun( files, run, mod, outPath, useOakwood, n );

    return 0;
}
