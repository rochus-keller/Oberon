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
#include "ObsFiles.h"
#include "ObxLjbcGen.h"
#include "ObxCGen.h"
#include <LjTools/Engine2.h>
#include <QFile>
#include <QDir>
#include <QBuffer>
#include <QtDebug>
#include <QTime>
using namespace Obx;

static void printLoadError(Lua::Engine2* lua, const QByteArray& what)
{
    Lua::Engine2::ErrorMsg msg = Lua::Engine2::decodeRuntimeMessage(lua->getLastError());
    QString str;
    if( msg.d_line )
    {
        if( Ob::RowCol::isPacked(msg.d_line) )
            str = QString("%1:%2: %3").arg(Ob::RowCol::unpackRow(msg.d_line)).arg(Ob::RowCol::unpackCol(msg.d_line))
                    .arg(msg.d_message.constData());
    }else
        str = msg.d_message;
    qCritical() << "error loading" << what << str.toUtf8().constData();
}

static void loadLuaLib( Lua::Engine2* lua, const QByteArray& path, QByteArray name = QByteArray() )
{
    QFile lib( QString(":/runtime/%1.lua").arg(path.constData()) );
    if( !lib.open(QIODevice::ReadOnly) )
        qCritical() << "cannot find" << path;
    if( name.isEmpty() )
        name = path;
    if( !lua->addSourceLib( lib.readAll(), name ) )
        printLoadError( lua, path );
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

LjRuntime::LjRuntime(QObject*p):QObject(p), d_jitEnabled(true),d_buildErrors(false)
{
    d_pro = new Project(this);

    d_lua = new Lua::Engine2(this);
    Lua::Engine2::setInst(d_lua);
    prepareEngine();
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
        preloadLib(d_pro,"MathL");
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
    if( !compile(true) )
        return false;

    if( !loadLibraries() )
        return false;

    if( !loadBytecode() )
        return false;

    return executeMain();
}

bool LjRuntime::loadLibraries()
{
    if( d_pro->useBuiltInOakwood() )
    {
        loadLuaLib(d_lua,"In");
        loadLuaLib(d_lua,"Out");
        loadLuaLib(d_lua,"Files");
        loadLuaLib(d_lua,"Input");
        loadLuaLib(d_lua,"Math");
        loadLuaLib(d_lua,"MathL");
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
    const QString root = d_pro->getWorkingDir(true);
    Obs::Files::setFileSystemRoot(root);
    if( d_pro->useBuiltInObSysInner() )
        d_lua->print(QString("Oberon file system root: %1\n").arg(root).toUtf8().constData());
    return true;
}

bool LjRuntime::loadBytecode()
{
    if( d_byteCode.isEmpty() )
    {
        qWarning() << "nothing to load";
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
            {
                printLoadError(d_lua,name);
                hasErrors = true;
            }
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

    return !hasErrors;
}

bool LjRuntime::executeMain()
{
    Project::ModProc main = d_pro->getMain();
    if( main.first.isEmpty() )
    {
        Q_ASSERT( main.second.isEmpty() ); // we either need module or module.proc
        return true; // nothing to do
    }

    QByteArray src;
    QTextStream out(&src);

    out << "local " << main.first << " = require '" << main.first << "'" << endl;
    if( !main.second.isEmpty() )
        out << main.first << "." << main.second << "()" << endl;
    out.flush();
    return d_lua->executeCmd(src,"terminal");
}

bool LjRuntime::restartEngine()
{
    if( !d_lua->restart() )
        return false;
    prepareEngine();
    d_lua->setJit(d_jitEnabled);
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

bool LjRuntime::saveBytecode(const QString& outPath, const QString& suffix) const
{
    QDir dir(outPath);
    if( !dir.exists() && !dir.mkpath(outPath) )
    {
        qCritical() << "cannot create directory for writing bytecode files" << outPath;
        return false;
    }
    for( int i = 0; i < d_byteCode.size(); i++ )
    {
#if 0
        if( d_byteCode[i].first->d_fullName.size() > 1 )
        {
            // there is a virtual path
            const QString subDir = d_byteCode[i].first->d_fullName.mid(0, d_byteCode[i].first->d_fullName.size() - 1 ).join('/');
            if( !dir.mkpath( subDir ) )
            {
                qCritical() << "cannot create subdirectory for writing bytecode files" << dir.absoluteFilePath(subDir);
                return false;
            }
        }
        QString path = d_byteCode[i].first->d_fullName.join('/') + d_byteCode[i].first->formatMetaActuals();
        path = dir.absoluteFilePath(path + suffix);
#else
        QString path = dir.absoluteFilePath(d_byteCode[i].first->getName() + suffix);
#endif
        QFile out(path);
        if( !out.open(QIODevice::WriteOnly) )
        {
            qCritical() << "cannot open file for writing bytecode" << path;
            return false;
        }
        out.write(d_byteCode[i].second);
    }
    return true;
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
    d_buildErrors = false;

    const quint32 errCount = d_pro->getErrs()->getErrCount();
    QSet<Module*> generated;
    foreach( Module* m, mods )
    {
        if( m->d_synthetic )
            ; // NOP
        else if( m->d_hasErrors )
        {
            qDebug() << "terminating because of errors in" << m->d_name;
            d_buildErrors = true;
            return;
        }else if( m->d_isDef )
        {
            qDebug() << "allocating" << m->d_name;
            if( m->d_externC )
            {
                LjbcGen::allocateSlots(m);
                qDebug() << "generating binding for" << m->getName();
                QBuffer buf;
                buf.open(QIODevice::WriteOnly);
                CGen::generateLjFfiBinding(m, &buf, d_pro->getErrs() );
                buf.close();
#if 0
                //qDebug() << buf.buffer(); // TEST
                QFile f(QDir::current().absoluteFilePath(m->getName() + ".lua"));
                if( f.open(QIODevice::WriteOnly) )
                {
                    qDebug() << "writing generated binding for" << m->getName() << "to" << f.fileName();
                    f.write(buf.buffer());
                }else
                    qCritical() << "could not open for writing" << f.fileName();
#endif
                d_byteCode << qMakePair(m,buf.buffer());
            }else
            {
#ifdef _DEBUG_
                QBuffer buf;
                buf.open(QIODevice::WriteOnly);
                LjbcGen::allocateSlots(m, &buf);
                buf.close();
                qDebug() << "********** Definition of" << m->d_name;
                qDebug() << buf.buffer();
#else
                LjbcGen::allocateSlots(m);
#endif
            }
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

    d_buildErrors = d_pro->getErrs()->getErrCount() != errCount;
}

void LjRuntime::generate(Module* m)
{
    Q_ASSERT( m && m->isFullyInstantiated() );
    //qDebug() << "generating" << m->getName();
    QBuffer buf;
    buf.open(QIODevice::WriteOnly);
    LjbcGen::translate(m, &buf, false, d_pro->getErrs() );
    buf.close();
    d_byteCode << qMakePair(m,buf.buffer());
}

void LjRuntime::prepareEngine()
{
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

