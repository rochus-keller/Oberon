/*
* Copyright 2019 Rochus Keller <mailto:me@rochus-keller.ch>
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
#include <bitset>
#include <stdio.h>
#include <math.h>
using namespace Ob;

#define LIBNAME		"_obnljlib"
#define METANAME "ObnSet"

struct _Set
{
    std::bitset<32> bits;
};

static _Set* check(lua_State *L, int narg = 1 )
{
    return static_cast<_Set*>( luaL_checkudata( L, narg, METANAME ) );
}

static _Set* create(lua_State* L)
{
    void* buf = static_cast<_Set*>( lua_newuserdata( L, sizeof(_Set) ) );
    _Set* s = ::new( buf ) _Set();

    luaL_getmetatable( L, METANAME );
    if( !lua_istable(L, -1 ) )
        luaL_error( L, "internal error: no meta table for '%s'", METANAME );
    lua_setmetatable( L, -2 );
    return s;
}

static const luaL_Reg Reg[] =
{
    { "instance", LjLib::instance },
    { "is_a", LjLib::is_a },
    { "DIV", LjLib::DIV },
    { "MOD", LjLib::MOD },
    { "SET", LjLib::SET },
    { "IN", LjLib::IN },
    { "ORD", LjLib::ORD },
    { "ASSERT", LjLib::ASSERT },
    { "TRACE", LjLib::TRACE },
    { "INCL", LjLib::INCL },
    { "EXCL", LjLib::EXCL },
    { "PACK", LjLib::PACK },
    { "UNPK", LjLib::UNPK },
    { NULL,		NULL	}
};

int LjLib::install(lua_State* L)
{
    luaL_register( L, LIBNAME, Reg );

    if( luaL_newmetatable( L, METANAME ) == 0 )
        luaL_error( L, "metatable '%s' already registered", METANAME );

    const int metaTable = lua_gettop(L);

    lua_newtable(L);
    const int methodTable = lua_gettop(L);

    // setze Attribut __metatable von meta. Dies ist der Wert, der
    // von getmetatable( obj ) zurückgegeben wird. Trick, um echten Metatable zu verstecken.
    // Wirkt nicht, wenn debug.getmetatable aufgerufen wird.
    lua_pushliteral(L, "__metatable");
    lua_pushvalue(L, methodTable );
    lua_settable(L, metaTable);

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
    lua_pushcfunction(L, setDiv );
    lua_rawset(L, metaTable);

    lua_pushliteral(L, "__eq");
    lua_pushcfunction(L, setEq );
    lua_rawset(L, metaTable);

    lua_pushliteral(L, "__tostring");
    lua_pushcfunction(L, setToString );
    lua_rawset(L, metaTable);

    lua_pop(L, 1);  // drop metaTable
    lua_pop(L, 1);  // drop method table

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

int LjLib::DIV(lua_State* L)
{
    const lua_Integer a = lua_tointeger(L,1);
    const lua_Integer b = lua_tointeger(L,2);
    if( b == 0 )
        luaL_argerror(L,1,"may not be zero");
    // source: http://lists.inf.ethz.ch/pipermail/oberon/2019/013353.html
    if (a < 0)
        lua_pushinteger( L, (a - b + 1) / b );
    else
        lua_pushinteger( L, a / b );
    return 1;
}

int LjLib::MOD(lua_State* L)
{
    const lua_Integer a = lua_tointeger(L,1);
    const lua_Integer b = lua_tointeger(L,2);
    if( b == 0 )
        luaL_argerror(L,1,"may not be zero");
    // source: http://lists.inf.ethz.ch/pipermail/oberon/2019/013353.html
    if (a < 0)
        lua_pushinteger( L, (b - 1) + ((a - b + 1)) % b );
    else
        lua_pushinteger( L, a % b );
    return 1;
}

int LjLib::SET(lua_State* L)
{
    const int max = lua_gettop(L);
    _Set* s = create(L);
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
            }else
            {
                for( int j = b; j <= a; j++ )
                    s->bits.set(j);
            }
        }else if( a >= 0 && a < s->bits.size() )
        {
            s->bits.set(a);
        }else
            luaL_argerror(L,i,"argument must be between 0 and 31");
    }
    return 1;
}

int LjLib::IN(lua_State* L)
{
    const lua_Integer a = lua_tointeger(L,1);
    _Set* s = check(L,2);
    lua_pushboolean(L, s->bits.test(a) );
    return 1;
}

int LjLib::ORD(lua_State* L)
{
    if( lua_isstring(L,1) )
    {
        const char* str = lua_tostring(L,1);
        lua_pushinteger(L, *str );
    }else if( lua_isboolean(L,1) )
    {
        lua_pushinteger(L,lua_toboolean(L,1));
    }else if( lua_isnumber(L,1) )
    {
        lua_pushnumber(L, lua_tonumber(L,1) );
    }else
    {
        _Set* a = check(L,1);
        lua_pushinteger(L, a->bits.to_ulong());
    }

    return 1;
}

#include <QtDebug>
int LjLib::ASSERT(lua_State* L)
{
    const bool ok = lua_toboolean(L,1);
    if( !ok )
    {
        qCritical() << "failed at line" << lua_tointeger(L,3);
        luaL_error(L,"assert fail at %s %d", lua_tostring(L,2), lua_tointeger(L,3) );
    }else
        qDebug() << "passed line" << lua_tointeger(L,3);
    return 0;
}

int LjLib::TRACE(lua_State* L)
{
    QByteArray str;
    for( int i = 1; i <= lua_gettop(L); i++ )
    {
        if( i != 0 )
            str += "\t";
        str += lua_tostring(L,i);
    }
    qDebug() << "TRACE" << str.constData();
    return 0;
}

int LjLib::INCL(lua_State* L)
{
    _Set* v = check(L,1);
    const lua_Integer x = lua_tointeger(L,2);
    if( x >= 0 && x < v->bits.size() )
        v->bits.set(x);
    else
        luaL_argerror(L,2,"argument must be between 0 and 31");
    return 0;
}

int LjLib::EXCL(lua_State* L)
{
    _Set* v = check(L,1);
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
    lua_Number x = lua_tonumber(L,1);
    lua_Integer n = lua_tointeger(L,2);
    x = frexp(x, &n);
    x += x;
    (n)--;
    lua_pushnumber(L,x);
    lua_pushnumber(L,n);
    return 2;
}

int LjLib::setAdd(lua_State* L)
{
    _Set* a = check(L,1);
    _Set* b = check(L,2);
    _Set* s = create(L);
    s->bits = a->bits | b->bits;
    return 1;
}

int LjLib::setSub(lua_State* L)
{
    _Set* a = check(L,1);
    _Set* b = check(L,2);
    _Set* s = create(L);
    for( int j = 0; j < a->bits.size(); j++ )
        s->bits.set( a->bits.test(j) && !b->bits.test(j) );
    return 1;
}

int LjLib::setMul(lua_State* L)
{
    _Set* a = check(L,1);
    _Set* b = check(L,2);
    _Set* s = create(L);
    s->bits = a->bits & b->bits;
    return 1;
}

int LjLib::setDiv(lua_State* L)
{
    _Set* a = check(L,1);
    _Set* b = check(L,2);
    _Set* s = create(L);
    for( int j = 0; j < a->bits.size(); j++ )
        s->bits.set( a->bits.test(j) || b->bits.test(j) && !( a->bits.test(j) && b->bits.test(j) ) );
    return 1;
}

int LjLib::setEq(lua_State* L)
{
    _Set* a = check(L,1);
    _Set* b = check(L,2);
    lua_pushboolean(L,a->bits == b->bits);
    return 1;
}

int LjLib::setUnm(lua_State* L)
{
    _Set* a = check(L,1);
    _Set* s = create(L);
    s->bits = ~a->bits;
    return 1;
}

int LjLib::setToString(lua_State* L)
{
    _Set* a = check(L,1);
    lua_pushstring(L,a->bits.to_string().c_str());
    return 1;
}
