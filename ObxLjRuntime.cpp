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
#include "ObxProject.h"
#include "ObFileCache.h"
#include "ObErrors.h"
#include "ObxLibFfi.h"
#ifdef QT_GUI_LIB
#include "ObsDisplay.h"
#endif
#include "ObxLjbcGen.h"
#include <LjTools/Engine2.h>
#include <QFile>
#include <QDir>
#include <QBuffer>
#include <QtDebug>
#include <QTime>
using namespace Obx;

static void loadLuaLib( Lua::Engine2* lua, const QByteArray& path, QByteArray name = QByteArray() )
{
    QFile lib( QString(":/scripts/%1.lua").arg(path.constData()) );
    if( !lib.open(QIODevice::ReadOnly) )
        qCritical() << "cannot find" << path;
    if( name.isEmpty() )
        name = path;
    if( !lua->addSourceLib( lib.readAll(), name ) )
        qCritical() << "compiling" << path << ":" << lua->getLastError();
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

LjRuntime::LjRuntime(QObject*p):QObject(p), d_jitEnabled(true)
{
    d_pro = new Project(this);

    d_lua = new Lua::Engine2(this);
    Lua::Engine2::setInst(d_lua);
    LibFfi::install(d_lua->getCtx());
#ifdef QT_GUI_LIB
    Obs::Display::install(d_lua->getCtx());
#endif
    d_lua->addStdLibs();
    d_lua->addLibrary(Lua::Engine2::PACKAGE);
    d_lua->addLibrary(Lua::Engine2::IO);
    d_lua->addLibrary(Lua::Engine2::BIT);
    d_lua->addLibrary(Lua::Engine2::JIT);
    d_lua->addLibrary(Lua::Engine2::FFI);
    d_lua->addLibrary(Lua::Engine2::OS);
    // d_lua->setJit(false); // must be called after addLibrary! doesn't have any effect otherwise
    loadLuaLib( d_lua, "obxlj" );


}

bool LjRuntime::compile(bool doGenerate)
{
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
    if( !d_pro->reparse() )
        return false;
    qDebug() << "recompiled in" << start.msecsTo(QTime::currentTime()) << "[ms]";
    if( doGenerate )
        generate();
    return errCount == d_pro->getErrs()->getErrCount();
}

bool LjRuntime::run()
{
    QDir::setCurrent(d_pro->getWorkingDir(true));

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
    {
#ifdef QT_GUI_LIB
        loadLuaLib(d_lua,"Obs/Input", "Input");
        loadLuaLib(d_lua,"Obs/Kernel", "Kernel");
        loadLuaLib(d_lua,"Obs/Display", "Display");
        loadLuaLib(d_lua,"Obs/Modules", "Modules");
        loadLuaLib(d_lua,"Obs/FileDir", "FileDir");
        loadLuaLib(d_lua,"Obs/Files", "Files");
#else
        qCritical() << "this version doesn't support the Oberon System backend modules";
        return false;
#endif
    }

    if( !compile(true) )
        return false;

    if( d_byteCode.isEmpty() )
    {
        qWarning() << "nothing to run";
        return true;
    }

    bool hasErrors = false;
    try
    {
        for( int i = 0; i < d_byteCode.size(); i++ )
        {
            const QByteArray name = d_byteCode[i].first->getName();
            qDebug() << "loading" << name;
            if( !d_lua->addSourceLib( d_byteCode[i].second, name ) )
                hasErrors = true;
            if( d_lua->isAborted() )
            {
                return true;
            }
        }
    }catch(...)
    {
        hasErrors = true;
        qCritical() << "LuaJIT crashed"; // doesn't help if the JIT crashes!
    }

    if( hasErrors )
    {
        return false;
    }

    Project::ModProc main = d_pro->getMain();
    if( main.first.isNull() )
    {
        Q_ASSERT( !d_byteCode.isEmpty() );
        main.first = d_byteCode.back().first->getName();
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
    return true;
}

QByteArray LjRuntime::findByteCode(Module* m) const
{
    for( int i = 0; i < d_byteCode.size(); i++ )
    {
        if( d_byteCode[i].first == m )
            return d_byteCode[i].second;
    }
    return QByteArray();
}

LjRuntime::BytecodeList LjRuntime::findByteCode(const QString& filePath) const
{
    BytecodeList res;
    for( int i = 0; i < d_byteCode.size(); i++ )
    {
        if( d_byteCode[i].first->d_file == filePath )
            res.append(d_byteCode[i]);
    }
    return res;
}

void LjRuntime::saveBytecode(const QString& outPath) const
{
    QDir dir(outPath);
    for( int i = 0; i < d_byteCode.size(); i++ )
    {
        QString path = dir.absoluteFilePath(d_byteCode[i].first->getName() + ".ljbc");
        QFile out(path);
        out.open(QIODevice::WriteOnly);
        out.write(d_byteCode[i].second);
    }
}

void LjRuntime::setJitEnabled(bool on)
{
    d_jitEnabled = on;
    d_lua->setJit(on);
}

void LjRuntime::generate()
{
    QList<Module*> mods = d_pro->getModulesToGenerate();
    d_byteCode.clear();

    QSet<Module*> generated;
    foreach( Module* m, mods )
    {
        if( m->d_synthetic )
            ; // NOP
        else if( m->d_hasErrors )
        {
            qDebug() << "terminating because of errors in" << m->d_name;
            return;
        }else if( m->d_isDef )
        {
            qDebug() << "allocating" << m->d_name;
#ifdef _DEBUG
            QBuffer buf;
            buf.open(QIODevice::WriteOnly);
            LjbcGen::allocateDef(m, &buf, d_pro->getErrs());
            buf.close();
            qDebug() << "********** Definition of" << m->d_name;
            qDebug() << buf.buffer();
#else
            LjbcGen::allocateDef(m, 0, d_pro->getErrs());
#endif
        }else
        {
            if( m->d_metaParams.isEmpty() )
            {
                LjbcGen::allocateSlots(m);
                // module slots are allocated before generic instances are generated because records of
                // the module could be used in the generic instance

                QList<Module*> result;
                m->findAllInstances(result);
                foreach( Module* inst, result )
                {
                    // instances must be generated after the modules using them, otherwise we get !slotValid assertions
                    if( !generated.contains(inst) )
                    {
                        generated.insert(inst);
                        LjbcGen::allocateSlots(inst);
                        generate(inst);
                    }
                }
                // module is generated after the generic instances it depends on because there are required slots
                generate(m);
            }
        }
    }
}

void LjRuntime::generate(Module* m)
{
    Q_ASSERT( m && m->isFullyInstantiated() );
    qDebug() << "generating" << m->getName();
    QBuffer buf;
    buf.open(QIODevice::WriteOnly);
    LjbcGen::translate(m, &buf, false, d_pro->getErrs() );
    buf.close();
    d_byteCode << qMakePair(m,buf.buffer());
}

