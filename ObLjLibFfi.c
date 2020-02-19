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

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#define DllExport __declspec(dllexport)
#else
#define DllExport export
#endif

DllExport int LjLibFfi_DIV( int a, int b )
{
    // source: http://lists.inf.ethz.ch/pipermail/oberon/2019/013353.html
    assert( b != 0 );
    if( a < 0 )
        return (a - b + 1) / b;
    else
        return a / b;
}

DllExport int LjLibFfi_MOD( int a, int b )
{
    // source: http://lists.inf.ethz.ch/pipermail/oberon/2019/013353.html
    assert( b != 0 );
    if (a < 0)
        return (b - 1) + (a - b + 1) % b;
    else
        return a % b;
}

typedef struct {
    char str[0];
} ArrayOfChar;

DllExport void LjLibFfi_strAssign( ArrayOfChar* lhs, int lhsLen,  const char rhs[] )
{
    const int rhsLen = strlen(rhs);
    assert( lhsLen > rhsLen );
    strcpy( lhs->str, rhs );
}

DllExport const char* LjLibFfi_toCstr(ArrayOfChar* str)
{
    return str->str;
}

DllExport int LjLibFfi_strIndex(ArrayOfChar* str, int strLen, int index )
{
    assert( index > 0 && index <= strLen );
    return str->str[index-1];
}

DllExport void LjLibFfi_strNewindex(ArrayOfChar* str, int strLen, int index, int ch )
{
    assert( index > 0 && index <= strLen );
    assert( ch >= 0 && ch <= 255 );
    str->str[index-1] = ch;
}

DllExport int LjLibFfi_strEq(ArrayOfChar* lhs, ArrayOfChar* rhs )
{
    return strcmp( lhs->str, rhs->str ) == 0;
}

DllExport int LjLibFfi_strLe(ArrayOfChar* lhs, ArrayOfChar* rhs )
{
    return strcmp( lhs->str, rhs->str ) <= 0;
}

DllExport int LjLibFfi_strLt(ArrayOfChar* lhs, ArrayOfChar* rhs )
{
    return strcmp( lhs->str, rhs->str ) < 0;
}
