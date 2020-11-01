/*
* Copyright 2019, 2020 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include "ObLjLib.h"
#include <lua.hpp>
#include <stdio.h>
#include <math.h>
#include <vector>
#include <stdlib.h>
#include <string.h>
#include <LjTools/Engine2.h>
#ifdef _DEBUG
#include <QtDebug>
#endif
using namespace Ob;

#define LIBNAME		"obnljlib"
#define SET_METANAME "ObnSet"
#define STRING_METANAME "ObnString"

// TODO: to make full use of LuaJIT optimizations, replace this library by a full Lua implementation.


_Set* LjLib::setCheck(lua_State *L, int narg, bool justPeek )
{
    if( !justPeek )
        return static_cast<_Set*>( luaL_checkudata( L, narg, SET_METANAME ) );
    // else
    luaL_getmetatable( L, SET_METANAME );
    if( !lua_istable(L, -1 ) )
        luaL_error( L, "internal error: no meta table for '%s'", SET_METANAME );
    if( lua_getmetatable( L, narg ) == 0 )
    {
        lua_pop(L,1);
        return 0;
    }
    // else
    _Set* res = 0;
    if( lua_rawequal( L, -1, -2 ) )
        res = static_cast<_Set*>( lua_touserdata(L,narg) );
    lua_pop(L,2);
    return res;
}

_Set* LjLib::setCreate(lua_State* L)
{
    void* buf = lua_newuserdata( L, sizeof(_Set) );
    _Set* s = ::new( buf ) _Set();

    luaL_getmetatable( L, SET_METANAME );
    if( !lua_istable(L, -1 ) )
        luaL_error( L, "internal error: no meta table for '%s'", SET_METANAME );
    lua_setmetatable( L, -2 );
    return s;
}

_String* LjLib::strCheck(lua_State *L, int narg )
{
    return static_cast<_String*>( luaL_checkudata( L, narg, STRING_METANAME ) );
}

_String* LjLib::strCreate(lua_State* L, int len)
{
    void* buf = lua_newuserdata( L, sizeof(_String) );
    _String* s = ::new( buf ) _String();

    if( len != -1 )
    {
        s->string.resize(len);
        for( int i = 0; i < len; i++ )
            s->string[i] = 0;
    }

    luaL_getmetatable( L, STRING_METANAME );
    if( !lua_istable(L, -1 ) )
        luaL_error( L, "internal error: no meta table for '%s'", STRING_METANAME );
    lua_setmetatable( L, -2 );
    return s;
}

static const luaL_Reg Reg[] =
{
    { "instance", LjLib::instance },
    { "is_a", LjLib::is_a },
    { "SET", LjLib::SET },
    { "IN", LjLib::IN },
    { "ORD", LjLib::ORD },
    { "ASSERT", LjLib::ASSERT },
    { "TRACE", LjLib::TRACE },
    { "INCL", LjLib::INCL },
    { "EXCL", LjLib::EXCL },
    { "PACK", LjLib::PACK },
    { "UNPK", LjLib::UNPK },
    { "Str", LjLib::Str },
    { "Char", LjLib::Char },
    { "TRAP", LjLib::TRAP },
    { "Copy", LjLib::Copy },
   { NULL,		NULL	}
};

void LjLib::installSet(lua_State* L)
{
    if( luaL_newmetatable( L, SET_METANAME ) == 0 )
        luaL_error( L, "metatable '%s' already registered", SET_METANAME );

    const int metaTable = lua_gettop(L);

    lua_pushliteral(L, "__add");
    lua_pushcfunction(L, setAdd );
    lua_rawset(L, metaTable);

    lua_pushliteral(L, "__sub");
    lua_pushcfunction(L, setSub );
    lua_rawset(L, metaTable);

    lua_pushliteral(L, "__mul");
    lua_pushcfunction(L, setMul );
    lua_rawset(L, metaTable);

    lua_pushliteral(L, "__div");
    lua_pushcfunction(L, setDiv );
    lua_rawset(L, metaTable);

    lua_pushliteral(L, "__unm");
    lua_pushcfunction(L, setUnm );
    lua_rawset(L, metaTable);

    lua_pushliteral(L, "__eq");
    lua_pushcfunction(L, setEq );
    lua_rawset(L, metaTable);

    lua_pushliteral(L, "__tostring");
    lua_pushcfunction(L, setToString );
    lua_rawset(L, metaTable);

    lua_pop(L, 1);  // drop metaTable
}

int LjLib::install(lua_State* L)
{
    luaL_register( L, LIBNAME, Reg );

    installSet(L);
    installString(L);
    return 1;
}

int LjLib::instance(lua_State* L)
{
    const bool hasMeta = lua_istable(L,1);
    lua_createtable(L,0,0);
    const int tbl = lua_gettop(L);
    if( hasMeta )
    {
        lua_pushvalue(L,1);
        lua_setmetatable(L,tbl);
    }
    return 1;
}

int LjLib::is_a(lua_State* L)
{
    if( lua_istable(L,1) && lua_istable(L,2) )
    {
        // t1, t2
        if( !lua_getmetatable(L,1) )
            lua_pushboolean(L,0); // +0
        else
        {
            // t1, t2, mt1
            while( true )
            {
                if( lua_rawequal(L,2,-1) )
                {
                    lua_pushboolean(L,1);
                    break;
                }
                if( !lua_getmetatable(L,-1) )
                {
                    lua_pushboolean(L,0);
                    break;
                }
                lua_remove(L, -2);
            }
        }
    }else
        lua_pushboolean(L,0);
    return 1;
}

int LjLib::SET(lua_State* L)
{
    const int max = lua_gettop(L);
    _Set* s = setCreate(L);
    if( max == 1 )
    {
        const lua_Integer a = lua_tointeger(L,1);
        s->bits = std::bitset<32>(a);
    }else if( max > 1 )
    {
        for( int i = 1; i <= max; i += 2 )
        {
            const lua_Integer a = lua_tointeger(L,i);
            const lua_Integer b = lua_tointeger(L,i+1);
            if( a >= 0 && b >= 0 )
            {
                if( a <= b )
                {
                    for( int j = a; j <= b; j++ )
                        s->bits.set(j);
                }/*else
                {
                    // anscheinend nicht möglich in Oberon, siehe obnc T4Expressions.obn
                    for( int j = b; j <= a; j++ )
                        s->bits.set(j);
                }*/
            }else if( a >= 0 && a < s->bits.size() )
            {
                s->bits.set(a);
            }else
                luaL_argerror(L,i,"argument must be between 0 and 31");
        }
    }
    return 1;
}

int LjLib::IN(lua_State* L)
{
    const lua_Integer a = lua_tointeger(L,1);
    _Set* s = setCheck(L,2);
    lua_pushboolean(L, s->bits.test(a) );
    return 1;
}

int LjLib::ORD(lua_State* L)
{
    if( lua_type(L,1) == LUA_TSTRING )
    {
        const char* str = lua_tostring(L,1);
        lua_pushinteger(L, *str );
    }else if( lua_type(L,1) == LUA_TBOOLEAN )
    {
        lua_pushinteger(L,lua_toboolean(L,1));
    }else if( lua_type(L,1) == LUA_TNUMBER )
    {
        lua_pushinteger(L, lua_tonumber(L,1) + 0.5 );
    }else if( lua_type(L,1) == LUA_TTABLE )
    {
        lua_pushnumber(L, (ptrdiff_t)lua_topointer(L,1) );
    }else
    {
        _Set* a = setCheck(L,1,true);
        if( a )
            lua_pushinteger(L, a->bits.to_ulong());
        else
        {
            _String* s = strCheck(L,1);
            if( s->string.size()>0 )
                lua_pushinteger(L,(unsigned char)s->string[0]);
            else
                lua_pushinteger(L,0);
        }
    }

    return 1;
}

int LjLib::ASSERT(lua_State* L)
{
    const bool ok = lua_toboolean(L,1);
    if( !ok )
    {
        //qCritical() << "failed at line" << lua_tointeger(L,3);
        luaL_error(L,"assert fail at %s %d", lua_tostring(L,2), lua_tointeger(L,3) );
    }
#ifdef _DEBUG
    else
        qDebug() << "passed line" << lua_tointeger(L,3);
#endif
    return 0;
}

int LjLib::TRACE(lua_State* L)
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

int LjLib::INCL(lua_State* L)
{
    _Set* v = setCheck(L,1);
    const lua_Integer x = lua_tointeger(L,2);
    if( x >= 0 && x < v->bits.size() )
        v->bits.set(x);
    else
        luaL_argerror(L,2,"argument must be between 0 and 31");
    return 0;
}

int LjLib::EXCL(lua_State* L)
{
    _Set* v = setCheck(L,1);
    const lua_Integer x = lua_tointeger(L,2);
    if( x >= 0 && x < v->bits.size() )
        v->bits.set(x, false);
    else
        luaL_argerror(L,2,"argument must be between 0 and 31");
    return 0;
}

int LjLib::PACK(lua_State* L)
{
    lua_Number x = lua_tonumber(L,1);
    const lua_Integer n = lua_tointeger(L,2);
    x = ldexp( x, (int) n);
    lua_pushnumber(L,x);
    return 1;
}

int LjLib::UNPK(lua_State* L)
{
    double x = lua_tonumber(L,1);
    int n = lua_tointeger(L,2);
    x = frexp(x, &n);
    x += x;
    (n)--;
    lua_pushnumber(L,x);
    lua_pushnumber(L,n);
    return 2;
}

int LjLib::Str(lua_State* L)
{
    _String* s = 0;
    if( lua_type(L,1) == LUA_TSTRING )
    {
        s = strCreate(L);
        const char* rhs = lua_tostring(L,1);
        const int len = lua_objlen(L,1);
        s->string.resize(len+1);
        for( int i = 0; i < len; i++ )
            s->string[i] = rhs[i];
        s->string[len] = 0;
    }else
    {
        const int len = luaL_checkinteger(L,1);
        if( len <= 0 )
            luaL_argerror(L,2,"argument must be greater than zero");
        s = strCreate(L);
        s->string.resize(len);
    }
    return 1;
}

int LjLib::Char(lua_State* L)
{
    const int c = luaL_checkinteger(L,1);
    if( c < 0 || c > 255 )
        luaL_argerror(L,2,"char out of range");
    _String* s = strCreate(L);
    s->string.resize(1);
    s->string[0] = c;
    return 1;
}

int LjLib::TRAP(lua_State* L)
{
    return Lua::Engine2::TRAP(L);
}

int LjLib::Copy(lua_State* L)
{
    const int lhs = 1;
    const int rhs = 2;
    luaL_checktype(L,lhs,LUA_TTABLE);
    luaL_checktype(L,rhs,LUA_TTABLE);

    /* table is in the stack at index 't' */
    lua_pushnil(L);  /* first key */
    while (lua_next(L, rhs) != 0)
    {
        lua_pushvalue(L,-2); // key value key
        lua_pushvalue(L,-2); // key value key value
        lua_rawset(L, lhs);
        /* removes 'value'; keeps 'key' for next iteration */
        lua_pop(L, 1);
    }
    return 0;
}

int LjLib::setAdd(lua_State* L)
{
    _Set* a = setCheck(L,1);
    _Set* b = setCheck(L,2);
    _Set* s = setCreate(L);
    s->bits = a->bits | b->bits;
    return 1;
}

int LjLib::setSub(lua_State* L)
{
    _Set* a = setCheck(L,1);
    _Set* b = setCheck(L,2);
    _Set* s = setCreate(L);
    s->bits =  a->bits & ~b->bits;
    return 1;
}

int LjLib::setMul(lua_State* L)
{
    _Set* a = setCheck(L,1);
    _Set* b = setCheck(L,2);
    _Set* s = setCreate(L);
    s->bits = a->bits & b->bits;
    return 1;
}

int LjLib::setDiv(lua_State* L)
{
    _Set* a = setCheck(L,1);
    _Set* b = setCheck(L,2);
    _Set* s = setCreate(L);
    s->bits = ( a->bits | b->bits ) & ~( a->bits & b->bits );
    return 1;
}

int LjLib::setEq(lua_State* L)
{
    _Set* a = setCheck(L,1);
    _Set* b = setCheck(L,2);
    lua_pushboolean(L,a->bits == b->bits);
    return 1;
}

int LjLib::setUnm(lua_State* L)
{
    _Set* a = setCheck(L,1);
    _Set* s = setCreate(L);
    s->bits = ~a->bits;
    return 1;
}

int LjLib::setToString(lua_State* L)
{
    _Set* a = setCheck(L,1);
    lua_pushstring(L,a->bits.to_string().c_str());
    return 1;
}

void LjLib::installString(lua_State* L)
{
    if( luaL_newmetatable( L, STRING_METANAME ) == 0 )
        luaL_error( L, "metatable '%s' already registered", STRING_METANAME );

    const int metaTable = lua_gettop(L);

    lua_pushliteral(L, "__index");
    lua_pushcfunction(L, strIndex );
    lua_rawset(L, metaTable );

    lua_pushliteral(L, "__newindex");
    lua_pushcfunction(L, strNewindex );
    lua_rawset(L, metaTable );

    lua_pushliteral(L, "__tostring");
    lua_pushcfunction(L, strToString );
    lua_rawset(L, metaTable);

    lua_pushliteral(L, "__gc");
    lua_pushcfunction(L , strGc );
    lua_rawset(L, metaTable);

    lua_pushliteral(L, "__eq");
    lua_pushcfunction(L , strEq );
    lua_rawset(L, metaTable);

    lua_pushliteral(L, "__lt");
    lua_pushcfunction(L , strLt );
    lua_rawset(L, metaTable);

    lua_pushliteral(L, "__le");
    lua_pushcfunction(L , strLe );
    lua_rawset(L, metaTable);

    lua_pushliteral(L, "__lt");
    lua_pushcfunction(L , strLt );
    lua_rawset(L, metaTable);

    lua_pushliteral(L, "__len");
    lua_pushcfunction(L , strLen );
    lua_rawset(L, metaTable);

    lua_pop(L, 1);  // drop metaTable
}

int LjLib::strToString(lua_State* L)
{
    _String* s = strCheck(L,1);
    lua_pushstring(L,s->string.c_str());
    return 1;
}

int LjLib::strGc(lua_State* L)
{
    _String* s = strCheck(L,1);
    s->~_String();  // call destructor
    return 0;
}

int LjLib::strIndex(lua_State* L)
{
    _String* s = strCheck(L,1);
    if( lua_type(L,2) == LUA_TSTRING )
    {
        if( strcmp(lua_tostring(L,2),"assig") == 0 )
            lua_pushcfunction(L,strAssig);
        else if( strcmp(lua_tostring(L,2),"n") == 0 )
            lua_pushinteger(L,s->string.size());
        else
            luaL_argerror(L,2,"invalid index");
    }else
    {
        const int i = luaL_checkinteger(L, 2) - 1;
        if( i < 0 || i >= s->string.size() )
            luaL_argerror(L,2,"string index out of range");
#if 0
        char buf[2];
        buf[0] = s->string[i];
        buf[1] = 0;
        lua_pushstring(L, buf);
#endif
        // TODO: inefficient but necessary because Lua __eq only works if lhs and rhs same type
        _String* s2 = strCreate(L);
        s2->string.resize(2);
        s2->string[0] = s->string[i];
    }
    return 1;
}

int LjLib::strNewindex(lua_State* L)
{
    _String* s = strCheck(L,1);
    const int i = luaL_checkinteger(L, 2) - 1;
    if( i < 0 || i >= s->string.size() )
        luaL_argerror(L,2,"string index out of range");
    if( lua_type(L,3) == LUA_TNUMBER )
    {
        const int rhs = luaL_checkinteger(L, 3);
        if( rhs < 0 || rhs > 255 )
            luaL_argerror(L,2,"char out of range");
        s->string[i] = rhs;
    }else if( lua_type(L,3) == LUA_TSTRING )
    {
        const char* rhs = lua_tostring(L,3);
        if( lua_objlen(L,3) > 1 )
            luaL_argerror(L,2,"expecting single char");
        s->string[i] = *rhs;
    }else
    {
        _String* rhs = strCheck(L,3);
        if( ::strlen(rhs->string.c_str()) > 1 )
            luaL_argerror(L,2,"expecting single char");
        s->string[i] = rhs->string[0];
    }
    return 0;
}

int LjLib::strLen(lua_State* L)
{
    _String* s = strCheck(L,1);
    lua_pushinteger(L, s->string.size() );
    return 1;
}

int LjLib::strAssig(lua_State* L)
{
    _String* lhs = strCheck(L,1);
    if( lua_isstring(L,2) )
    {
        const char* rhs = lua_tostring(L,2);
        const int len = lua_objlen(L,2) + 1;
        if( len > lhs->string.size() )
            luaL_argerror(L,2,"rhs is longer than lhs");
        for( int i = 0; i < len; i++ )
            lhs->string[i] = rhs[i];
    }else if( lua_isnumber(L,2) )
    {
        // char
        const int rhs = lua_tointeger(L,2);
        if( rhs < 0 || rhs > 255 )
            luaL_argerror(L,2,"char out of range");
        if( lhs->string.size() < 2 )
            luaL_argerror(L,2,"rhs is longer than lhs");
        lhs->string[0] = rhs;
        lhs->string[1] = 0;
    }else
    {
        _String* rhs = strCheck(L,2);
        const char* raw = rhs->string.c_str();
        const int len = ::strlen(raw) + 1;
        if( len > lhs->string.size() )
            luaL_argerror(L,2,"rhs is longer than lhs");
        for( int i = 0; i < len; i++ )
            lhs->string[i] = raw[i];
    }
    return 0;
}

int LjLib::strEq(lua_State* L)
{
    // Lua requires that lhs and rhs same type!
    _String* lhs = strCheck(L,1);
    _String* rhs = strCheck(L,2);
    lua_pushboolean(L, ::strcmp(lhs->string.c_str(),rhs->string.c_str()) == 0 );
    return 1;
}

int LjLib::strLt(lua_State* L)
{
    _String* lhs = strCheck(L,1);
    _String* rhs = strCheck(L,2);
    lua_pushboolean(L, ::strcmp(lhs->string.c_str(),rhs->string.c_str()) < 0 );
    return 1;
}

int LjLib::strLe(lua_State* L)
{
    _String* lhs = strCheck(L,1);
    _String* rhs = strCheck(L,2);
    lua_pushboolean(L, ::strcmp(lhs->string.c_str(),rhs->string.c_str()) <= 0 );
    return 1;
}

extern "C"
{
#ifdef _WIN32
    __declspec(dllexport)
#endif
    int luaopen_obnljlib(lua_State *L)
    {
        return LjLib::install(L);
    }
}

#ifdef _WIN32
#include <windows.h>


BOOL APIENTRY DllMain( HMODULE hModule,
                       DWORD  ul_reason_for_call,
                       LPVOID lpReserved
                     )
{
    return TRUE;
}

#endif


bool _String::assign(const QByteArray& rhs)
{
    if( rhs.size() > string.size() - 1 )
        return false;
    const char* str = rhs.constData();
    for( int i = 0; i < rhs.size(); i++ )
        string[i] = str[i];
    string[rhs.size()] = 0;
    return true;
}

bool _String::assign(const std::string& rhs)
{
    if( rhs.size() > string.size() - 1 )
        return false;
    for( int i = 0; i < rhs.size(); i++ )
        string[i] = rhs[i];
    string[rhs.size()] = 0;
    return true;
}
