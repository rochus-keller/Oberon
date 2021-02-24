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

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <QString>

#ifdef _WIN32
#define DllExport __declspec(dllexport)
#else
#define DllExport
#endif

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

typedef struct{
    int count;
    uint8_t data[];
} ByteArray;
typedef struct{
    int count;
    uint16_t data[];
} WordArray;
typedef struct{
    int count;
    int16_t data[];
} ShortArray;
typedef struct{
    int count;
    int32_t data[];
} IntArray;
typedef struct{
    int count;
    uint32_t data[];
} UIntArray;
typedef struct{
    int count;
    int64_t data[];
} LongArray;
typedef struct{
    int count;
    float data[];
} FloatArray;
typedef struct{
    int count;
    double data[];
} DoubleArray;

DllExport void ObxFfi_initString( ByteArray* ba, const char* utf8 )
{
    copystr((char*)ba->data,ba->count,utf8);
}

DllExport void ObxFfi_initWstring( WordArray* wa, const char* utf8 )
{
    copywstr(wa->data,wa->count,utf8);
}

DllExport void ObxFfi_initByteArray( ByteArray* ba, const char* data )
{
    memcpy(ba->data,data,ba->count-1); // data might or might not have terminating zero
    ba->data[ba->count-1] = 0;
}

DllExport int ObxFfi_strRelOp( ByteArray* lhs, ByteArray* rhs, int op )
{
    return strRelOp((char*)lhs->data,(char*)rhs->data,op);
}

DllExport int ObxFfi_wstrRelOp( WordArray* lhs, WordArray* rhs, int op )
{
    return wstrRelOp(lhs->data,lhs->count,rhs->data,rhs->count,op);
}

}
