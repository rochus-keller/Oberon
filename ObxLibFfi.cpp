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

#include "ObxLibFfi.h"
#include <LjTools/Engine2.h>
#include <lua.hpp>
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <math.h>
#include <QString>
#include <QtDebug>
#include <QFile>
#include <QDir>
using namespace Obx;

#ifdef _WIN32
#define DllExport __declspec(dllexport)
#else
#define DllExport
#endif

static SendToLog s_sendToLog = 0;

void LibFfi::setSendToLog(SendToLog f)
{
    s_sendToLog = f;
}

extern "C" {
    DllExport void ObxFfi_DBGTRACE( const char* str );

    static int _DBGTRACE(lua_State* L)
    {
        ObxFfi_DBGTRACE(lua_tostring(L,1));
        return 0;
    }

    static int OBX_ASSERT(lua_State* L)
    {
        const bool ok = lua_toboolean(L,1);
        if( !ok )
        {
            //qCritical() << "failed at line" << lua_tointeger(L,3);
            luaL_error(L,"assert fail at %s %d", lua_tostring(L,2), lua_tointeger(L,3) );
        }
    #ifdef _DEBUG_
        else
            qDebug() << "passed line" << lua_tointeger(L,3);
    #endif
        return 0;
    }

    static int _TRACE(lua_State* L)
    {
    #ifdef _DEBUG
        QByteArray str;
        for( int i = 1; i <= lua_gettop(L); i++ )
        {
            if( i != 0 )
                str += "\t";
            str += lua_tostring(L,i);
        }
        qDebug() << "TRACE:" << str.constData();
    #endif
        return 0;
    }

    static int OBX_ADDRESSOF(lua_State* L)
    {
        const void* ptr = lua_topointer(L, 1);
        lua_pushinteger(L,(lua_Integer)ptr);
        return 1;
    }

}

void LibFfi::install(lua_State* L)
{
    lua_pushcfunction( L, Lua::Engine2::TRAP );
    lua_setglobal( L, "TRAP" );
    lua_pushcfunction( L, Lua::Engine2::ABORT );
    lua_setglobal( L, "ABORT" );
    lua_pushcfunction( L, OBX_ASSERT );
    lua_setglobal( L, "ASSERT" );
    lua_pushcfunction( L, _TRACE );
    lua_setglobal( L, "TRACE" );
    lua_pushcfunction( L, _DBGTRACE );
    lua_setglobal( L, "DBGTRACE" );
    lua_pushcfunction( L, OBX_ADDRESSOF );
    lua_setglobal( L, "ADDRESSOF" );
}

static void copystr( char* to, int len, const char* from )
{
    QByteArray tmp = QString::fromUtf8(from).toLatin1();
    memcpy(to,tmp.constData(),qMin(len-1,tmp.size()));
    to[len-1] = 0;
}

static void copywstr( uint16_t* to, int len, const char* from )
{
    QString tmp = QString::fromUtf8(from);
    for( int i = 0; i < qMin(len-1,tmp.size()); i++ )
        to[i] = tmp[i].unicode();
    to[len-1] = 0;
}

static void printstring( const char* str )
{
    const QString tmp = QString::fromLatin1(str);
    qDebug() << tmp;
    if( s_sendToLog )
        s_sendToLog(tmp);
}

static void printwstring( const uint16_t* str, int len )
{
    const QString tmp = QString::fromRawData((QChar*)str, len);
    qDebug() << tmp;
    if( s_sendToLog )
        s_sendToLog(tmp);
}

static int relOp( const QString& l, const QString& r, int op )
{
    switch( op )
    {
    case 1: // EQ
        return l.compare(r) == 0;
    case 2: // NEQ
        return l.compare(r) != 0;
    case 3: // LT
        return l.compare(r) < 0;
    case 4: // LEQ
        return l.compare(r) <= 0;
    case 5: // GT
        return l.compare(r) > 0;
    case 6: // GEQ
        return l.compare(r) >= 0;
    }
    return 0;
}

static int strRelOp( const char* lhs, const char* rhs, int op )
{
    const QString l = QString::fromLatin1(lhs);
    const QString r = QString::fromLatin1(rhs);
    return relOp(l,r,op);
}

static int wstrRelOp( const uint16_t* lhs, int lcount, const uint16_t* rhs, int rcount, int op )
{
    const QString l = QString::fromRawData((QChar*)lhs, lcount);
    const QString r = QString::fromRawData((QChar*)rhs, rcount);
    return relOp(l,r,op);
}

extern "C"
{
DllExport int ObxFfi_DIV( int a, int b )
{
    // source: http://lists.inf.ethz.ch/pipermail/oberon/2019/013353.html
    assert( b != 0 );
    if( a < 0 )
        return (a - b + 1) / b;
    else
        return a / b;
}

DllExport int ObxFfi_MOD( int a, int b )
{
    // source: http://lists.inf.ethz.ch/pipermail/oberon/2019/013353.html
    assert( b != 0 );
    if (a < 0)
        return (b - 1) + (a - b + 1) % b;
    else
        return a % b;
}

typedef uint8_t CharArray[];
typedef uint16_t WcharArray[];
typedef uint8_t ByteArray[];


DllExport void ObxFfi_initString( CharArray data, int count, const char* utf8 )
{
    copystr((char*)data,count,utf8);
}

DllExport void ObxFfi_initWstring( WcharArray wa, int count, const char* utf8 )
{
    copywstr(wa,count,utf8);
}

DllExport void ObxFfi_initByteArray( ByteArray ba, int count, const char* data )
{
    memcpy(ba,data,count);
}

DllExport void ObxFfi_initByteArrayFromHex( ByteArray ba, int count, const char* hex )
{
    QByteArray data = QByteArray::fromHex(hex);
    if( data.size() > count )
        qWarning() << "hex string too big for byte array" << data.size() << count;
    memcpy(ba,data.constData(),qMin(count,data.size())); // data might or might not have terminating zero
}

DllExport int ObxFfi_strRelOp( char* lhs, char* rhs, int op )
{
    return strRelOp(lhs,rhs,op);
}

DllExport int ObxFfi_wstrRelOp( WcharArray lhs, int lcount, WcharArray rhs, int rcount, int op )
{
    return wstrRelOp(lhs, lcount,rhs, rcount,op);
}

DllExport void ObxFfi_printString( const char* str )
{
    printstring(str);
}

DllExport void ObxFfi_printWcharArray( WcharArray wa, int count )
{
    printwstring(wa, count);
}

DllExport void ObxFfi_DBGTRACE( const char* str )
{
    static QFile log( QDir::home().absoluteFilePath("trace.log") );
    if( !log.isOpen() )
        log.open(QIODevice::Append );
    log.write(str);
    log.write("\n");
    log.flush();
}

DllExport void ObxFfi_TRACE( const char* str )
{
    qDebug() << "TRACE:" << str;
}

DllExport void ObxFfi_NOP()
{
    qDebug() << "NOP";

    // works, but ends pcall with error:
    //lua_pushstring(Lua::Engine2::getInst()->getCtx(),"hello from nop");
    //lua_error(Lua::Engine2::getInst()->getCtx());
}


DllExport void ObxFfi_CRASH(int)
{
    *(int*)0 = 0;
}

}


