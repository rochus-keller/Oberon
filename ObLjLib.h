#ifndef OBLJLIB_H
#define OBLJLIB_H

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

#include <bitset>
#include <string>

typedef struct lua_State lua_State;
class QByteArray;

namespace Ob
{
    struct _Set
    {
        std::bitset<32> bits;
    };

    struct _String
    {
        std::string string;
        bool assign( const QByteArray& rhs );
        bool assign( const std::string& rhs );
    };

    class LjLib
    {
    public:
        static int install(lua_State *L);

        static int instance(lua_State *L);
        static int is_a(lua_State *L);
        static int DIV(lua_State *L);
        static int MOD(lua_State *L);
        static int SET(lua_State *L);
        static int IN(lua_State *L);
        static int ORD(lua_State *L);
        static int ASSERT(lua_State *L);
        static int TRACE(lua_State *L);
        static int INCL(lua_State *L);
        static int EXCL(lua_State *L);
        static int PACK(lua_State *L);
        static int UNPK(lua_State *L);
        static int Str(lua_State *L);
        static int Char(lua_State *L);
        static int TRAP(lua_State *L);
        static int Copy(lua_State *L);

        static _String* strCreate(lua_State* L, int len = -1);
        static _String* strCheck(lua_State *L, int narg = 1 );
        static _Set* setCreate(lua_State* L);
        static _Set* setCheck(lua_State *L, int narg = 1, bool justPeek = false );
    protected:
        static void installSet(lua_State* L);
        static int setAdd(lua_State *L);
        static int setSub(lua_State *L);
        static int setMul(lua_State *L);
        static int setDiv(lua_State *L);
        static int setEq(lua_State *L);
        static int setUnm(lua_State *L);
        static int setToString(lua_State *L);

        static void installString(lua_State* L);
        static int strToString(lua_State *L);
        static int strGc(lua_State *L);
        static int strIndex(lua_State *L);
        static int strNewindex(lua_State *L);
        static int strLen(lua_State *L);
        static int strAssig(lua_State *L);
        static int strEq(lua_State *L);
        static int strLt(lua_State *L);
        static int strLe(lua_State *L);
   private:
        LjLib();
    };
}

#endif // OBLJSET_H
