/*
* Copyright 2019, 2020 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon parser/code model library.
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

#include "ObLexer.h"
#include "ObErrors.h"
#include "ObFileCache.h"
#include <QBuffer>
#include <QFile>
#include <QIODevice>
#include <QtDebug>
#include <ctype.h>
using namespace Ob;

QHash<QByteArray,QByteArray> Lexer::d_symbols;

Lexer::Lexer(QObject *parent) : QObject(parent),
    d_lastToken(Tok_Invalid),d_lineNr(0),d_colNr(0),d_in(0),d_err(0),d_fcache(0),
    d_ignoreComments(true), d_packComments(true),d_enableExt(false), d_sensExt(false),
    d_sensed(false), d_sloc(0), d_lineCounted(false)
{

}

void Lexer::setStream(QIODevice* in, const QString& sourcePath)
{
    if( in == 0 )
        setStream( sourcePath );
    else
    {
        if( d_in != 0 && d_in->parent() == this )
            d_in->deleteLater();
        d_in = in;
        d_lineNr = 0;
        d_colNr = 0;
        d_sourcePath = sourcePath;
        d_lastToken = Tok_Invalid;
        d_sensed = false;
        d_sloc = 0;
        d_lineCounted = false;

        if( skipOberonHeader( d_in ) )
        {
            QObject* parent = d_in;
            if( d_in->parent() == this )
                parent = this;
            QBuffer* b = new QBuffer( parent );
            b->buffer() = d_in->readAll();
            b->buffer().replace( '\r', '\n' );
            b->open(QIODevice::ReadOnly);
            d_in = b;
        }else
            skipBom( d_in );
    }
}

bool Lexer::setStream(const QString& sourcePath)
{
    QIODevice* in = 0;

    if( d_fcache )
    {
        bool found;
        FileCache::Entry content = d_fcache->getFile(sourcePath, &found );
        if( found )
        {
            QBuffer* buf = new QBuffer(this);
            buf->setData( content.d_code );
            buf->open(QIODevice::ReadOnly);
            in = buf;
        }
    }

    if( in == 0 )
    {
        QFile* file = new QFile(sourcePath, this);
        if( !file->open(QIODevice::ReadOnly) )
        {
            if( d_err )
            {
                d_err->error(Errors::Lexer, sourcePath, 0, 0,
                                 tr("cannot open file from path %1").arg(sourcePath) );
            }
            delete file;
            return false;
        }
        in = file;
    }
    // else
    setStream( in, sourcePath );
    return true;
}

Token Lexer::nextToken()
{
    Token t;
    if( !d_buffer.isEmpty() )
    {
        t = d_buffer.first();
        d_buffer.pop_front();
    }else
        t = nextTokenImp();
    if( t.d_type == Tok_Comment && d_ignoreComments )
        t = nextToken();
    return t;
}

Token Lexer::peekToken(quint8 lookAhead)
{
    Q_ASSERT( lookAhead > 0 );
    while( d_buffer.size() < lookAhead )
        d_buffer.push_back( nextTokenImp() );
    return d_buffer[ lookAhead - 1 ];
}

QList<Token> Lexer::tokens(const QString& code)
{
    return tokens( code.toLatin1() );
}

QList<Token> Lexer::tokens(const QByteArray& code, const QString& path)
{
    QBuffer in;
    in.setData( code );
    in.open(QIODevice::ReadOnly);
    setStream( &in, path );

    QList<Token> res;
    Token t = nextToken();
    while( t.isValid() )
    {
        res << t;
        t = nextToken();
    }
    return res;
}

QByteArray Lexer::getSymbol(const QByteArray& str)
{
    if( str.isEmpty() )
        return str;
    QByteArray& sym = d_symbols[str];
    if( sym.isEmpty() )
        sym = str;
    return sym;
}

static inline bool isHexDigit( char c )
{
    return ::isdigit(c) || c == 'A' || c == 'B' || c == 'C' || c == 'D' || c == 'E' || c == 'F'
            || c == 'a' || c == 'b' || c == 'c' || c == 'd' || c == 'e' || c == 'f';
}

Token Lexer::nextTokenImp()
{
    if( d_in == 0 )
        return token(Tok_Eof);
    skipWhiteSpace();

    while( d_colNr >= d_line.size() )
    {
        if( d_in->atEnd() )
        {
            Token t = token( Tok_Eof, 0 );
            if( d_in->parent() == this )
                d_in->deleteLater();
            return t;
        }
        nextLine();
        skipWhiteSpace();
    }
    Q_ASSERT( d_colNr < d_line.size() );
    while( d_colNr < d_line.size() )
    {
        const char ch = quint8(d_line[d_colNr]);

        if( ch == '"' || ch == '\'' )
            return string();
        else if( ch == '$' )
        {
#ifdef OB_BBOX
            if( !isHexstring(1) )
                return token( Tok_Dlr );
            else
#endif
                return hexstring();
        }
        else if( ::isalpha(ch) || ( ch == '_' ) )
            return ident();
        else if( ::isdigit(ch) )
            return number();
        // else
        int pos = d_colNr;
        TokenType tt = tokenTypeFromString(d_line,&pos);

        if( tt == Tok_Latt )
            return comment();
        else if( tt == Tok_2Slash )
        {
            const int len = d_line.size() - d_colNr;
            return token( Tok_Comment, len, d_line.mid(d_colNr,len) );
        }else if( tt == Tok_Invalid || pos == d_colNr )
            return token( Tok_Invalid, 1, QString("unexpected character '%1' %2").arg(char(ch)).arg(int(ch)).toUtf8() );
        else {
            const int len = pos - d_colNr;
            return token( tt, len, d_line.mid(d_colNr,len) );
        }
    }
    Q_ASSERT(false);
    return token(Tok_Invalid);
}

int Lexer::skipWhiteSpace()
{
    const int colNr = d_colNr;
    while( d_colNr < d_line.size() && ( ( ::isspace( d_line[d_colNr] ) || d_line[d_colNr] == char(28) ) ) )
        d_colNr++;
    return d_colNr - colNr;
}

void Lexer::nextLine()
{
    d_colNr = 0;
    d_lineNr++;
    d_line = d_in->readLine();
    d_lineCounted = false;

    if( d_line.endsWith("\r\n") )
        d_line.chop(2);
    else if( d_line.endsWith('\n') || d_line.endsWith('\r') || d_line.endsWith('\025') )
        d_line.chop(1);
}

int Lexer::lookAhead(int off) const
{
    if( int( d_colNr + off ) < d_line.size() )
    {
        return d_line[ d_colNr + off ];
    }else
        return 0;
}

Token Lexer::token(TokenType tt, int len, const QByteArray& val)
{
    if( tt != Tok_Invalid && tt != Tok_Comment && tt != Tok_Eof )
        countLine();

    QByteArray v = val;
    if( tt != Tok_Comment && tt != Tok_Invalid )
        v = getSymbol(v);
    Token t( tt, d_lineNr, d_colNr + 1, len, v );
    d_lastToken = t;
    d_colNr += len;
    t.d_sourcePath = d_sourcePath;
    if( tt == Tok_Invalid && d_err != 0 )
        d_err->error(Errors::Syntax, t.d_sourcePath, t.d_lineNr, t.d_colNr, t.d_val );
    return t;
}

static inline bool isAllLowerCase( const QByteArray& str )
{
    for( int i = 0; i < str.size(); i++ )
    {
        if( !::islower(str[i] ) )
                return false;
    }
    return true;
}

Token Lexer::ident()
{
    int off = 1;
    while( true )
    {
        const char c = lookAhead(off);
        if( !::isalnum(c) && c != '_' )
            break;
        else
            off++;
    }
    const QByteArray str = d_line.mid(d_colNr, off );
    if( !isAscii(str) )
        return token( Tok_Invalid, off, "invalid characters in identifier" );
    Q_ASSERT( !str.isEmpty() );
    int pos = 0;
    QByteArray keyword = str;
    if( d_sensExt && !d_sensed )
    {
        d_sensed = true;
        if( isAllLowerCase(keyword) )
        {
            keyword = keyword.toUpper();
            TokenType t = tokenTypeFromString( keyword, &pos );
            if( t != Tok_Invalid && pos == keyword.size() )
            {
                d_enableExt = true;
                return token( t, off );
            }
        }
    }else if( d_enableExt && isAllLowerCase(keyword) )
        keyword = keyword.toUpper();
    TokenType t = tokenTypeFromString( keyword, &pos );
    if( t != Tok_Invalid && pos != keyword.size() )
        t = Tok_Invalid;
    if( t != Tok_Invalid )
        return token( t, off );
    else
        return token( Tok_ident, off, str );
}

static inline bool checkHexNumber( QByteArray str )
{
    const int pos = str.indexOf('\'');
    if( pos != -1 )
        str = str.left(pos);
    if( str.size() < 2 || ( !str.endsWith('H') && !str.endsWith('h')
                            && !str.endsWith('L') && !str.endsWith('l')
                            && !str.endsWith('I') && !str.endsWith('i')
                            && !str.endsWith('X') && !str.endsWith('x') ) )
        return false;
    else
        return true;
}

Token Lexer::number()
{
    // integer      ::=  digit {digit} ['I' | 'L'] | digit {hexDigit} 'H'
    // real         ::=  digit {digit} '.' {digit} [ScaleFactor] ['L']
    // ScaleFactor  ::=  'E' ['+' | '-'] digit {digit}
    const int startLine = d_lineNr;
    const int startCol = d_colNr;
    int lhsPlaces = 0, rhsPlaces = 0, expPlaces = 0;
    int off = 1;
    while( true )
    {
        const char c = lookAhead(off);
        if( !isHexDigit(c) )
            break;
        else
            off++;
    }
    lhsPlaces = off;
    bool isHex = false;
    bool isLong = false;
    bool isInt = false;
    bool isChar = false;
    bool isReal = false;
    const char o1 = lookAhead(off);
    if( o1 == 'L' || o1 == 'l' )
    {
        isLong = true;
        off++;
    }else if( o1 == 'I' || o1 == 'i' )
    {
        isInt = true;
        off++;
    }else if( o1 == 'H' || o1 == 'h' )
    {
        isHex = true;
        off++;
        const char o2 = lookAhead(off);
        if( o2 == 'L' || o2 == 'l' )
        {
            isLong = true;
            off++;
        }else if( o2 == 'I' || o2 == 'i' )
        {
            isInt = true;
            off++;
        }
    }else if( o1 == 'X' || o1 == 'x' )
    {
        isChar = true;
        off++;
    }else if( o1 == '.' && lookAhead(off+1) == '.' )
    {
        ; // look for decimal point but not for range
    }else if( o1 == '.'  )
    {
        off++;
        isReal = true;
        while( true )
        {
            const char c = lookAhead(off);
            if( !::isdigit(c) )
                break;
            else
                off++;
            rhsPlaces++;
        }
        const char de = lookAhead(off); // Oberon-2 allows E (REAL) or D (LONGREAL)
        if( de == 'E' || de == 'D' || de == 'e' || de == 'd' )
        {
            isLong = ( de == 'D' || de == 'd' );
            off++;
            char o = lookAhead(off);
            if( o == '+' || o == '-' )
            {
                off++;
                o = lookAhead(off);
            }
            if( !::isdigit(o) )
                return token( Tok_Invalid, off, "invalid real" );
            while( true )
            {
                const char c = lookAhead(off);
                if( !::isdigit(c) )
                    break;
                else
                    off++;
                expPlaces++;
            }
        }
    }
    QByteArray str = d_line.mid(d_colNr, off );
    Q_ASSERT( !str.isEmpty() );
    if( isHex && !checkHexNumber(str) )
        return token( Tok_Invalid, off, "invalid hexadecimal integer" );
    else if( isChar && !checkHexNumber(str) )
        return token( Tok_Invalid, off, "invalid hexadecimal string" );

    if( isChar )
    {
#if 0
        const quint32 ch = str.left(str.size()-1).toUInt(0,16);
        if( d_err && ch > 255 )
            d_err->warning( Errors::Semantics, d_sourcePath, startLine, startCol,
                            tr("character literal %1 using more than one byte").arg(str.constData()));
#endif
        return token( Tok_hexchar, off, str );
    }
    else if( isReal)
    {
        Token tok = token( Tok_real, off, str );
        if( (lhsPlaces+rhsPlaces) > 7 || expPlaces > 2 || isLong ) // double has 52 bit mantissa, i.e. ~15 decimal digits
            tok.d_double = true; // TODO should we trade decimal places with exponent width?
        return tok;
    }else
        return token( Tok_integer, off, str );
}

void Lexer::parseComment( const QByteArray& str, int& pos, int& level )
{
    enum State { Idle, Lpar, Star } state = Idle;
    while( pos < str.size() )
    {
        const char c = str[pos++];
        switch( state )
        {
        case Idle:
            if( c == '(')
                state = Lpar;
            else if( c == '*' )
                state = Star;
            break;
        case Lpar:
            if( c == '*' )
                level++;
            state = Idle;
            break;
        case Star:
            if( c == ')')
                level--;
            else if( c != '*' )
                state = Idle;
            if( level <= 0 )
                return;
            break;
        }
    }
}

static inline bool versionMatch( const quint8* raw )
{
    return ( raw[0] == 0xf0 && raw[1] == 0x01 ) ||
            ( raw[0] == 0x01 && raw[1] == 0xf0 ) ||

            ( raw[0] == 0xf0 && raw[1] == 0x00 ) ||
            ( raw[0] == 0x00 && raw[1] == 0xf0 ) ;
}

bool Lexer::skipOberonHeader(QIODevice* in)
{
    Q_ASSERT( in != 0 );
    const QByteArray buf = in->peek(6);
    const quint8* raw = (const quint8*)buf.constData();
    if( buf.size() >= 2 && versionMatch(raw) )
    {
        // get rid of Oberon file header
        const quint32 len = raw[2] + ( raw[3] << 8 ) + ( raw[4] << 16 ) + ( raw[5] << 24 );
        in->read(len);
        return true;
    }else
        return false;
}

bool Lexer::skipBom(QIODevice* in)
{
    const QByteArray buf = in->peek(3);
    if( buf.size() == 3 && buf[0] == 0xef && buf[1] == 0xbb && buf[2] == 0xbf )
    {
        in->read(3);
        return true;
    }else
        return false;
}

Token Lexer::comment()
{
    const int startLine = d_lineNr;
    const int startCol = d_colNr;


    int level = 0;
    int pos = d_colNr;
    parseComment( d_line, pos, level );
    QByteArray str = d_line.mid(d_colNr,pos-d_colNr);
    while( level > 0 && !d_in->atEnd() )
    {
        nextLine();
        pos = 0;
        parseComment( d_line, pos, level );
        if( !str.isEmpty() )
            str += '\n';
        str += d_line.mid(d_colNr,pos-d_colNr);
    }
    if( d_packComments && level > 0 && d_in->atEnd() )
    {
        d_colNr = d_line.size();
        Token t( Tok_Invalid, startLine, startCol + 1, str.size(), tr("non-terminated comment").toLatin1() );
        t.d_sourcePath = d_sourcePath;
        if( d_err )
            d_err->error(Errors::Syntax, t.d_sourcePath, t.d_lineNr, t.d_colNr, t.d_val );
        return t;
    }
    // Col + 1 weil wir immer bei Spalte 1 beginnen, nicht bei Spalte 0
    Token t( ( d_packComments ? Tok_Comment : Tok_Latt ), startLine, startCol + 1, str.size(), str );
    t.d_sourcePath = d_sourcePath;
    d_lastToken = t;
    d_colNr = pos;
    if( !d_packComments && level == 0 )
    {
        Token t(Tok_Ratt,d_lineNr, pos - 2 + 1, 2 );
        t.d_sourcePath = d_sourcePath;
        d_lastToken = t;
        d_buffer.append( t );
    }
    return t;
}

Token Lexer::string()
{
    const char quote = lookAhead(0);
    int off = 1;
    while( true )
    {
        const char c = lookAhead(off);
        off++;
        if( c == quote )
            break;
        if( c == 0 )
            return token( Tok_Invalid, off, "non-terminated string" );
    }
    const QByteArray str = d_line.mid(d_colNr, off );
    return token( Tok_string, off, str );
}

Token Lexer::hexstring()
{
    countLine();
    // inofficial extension of Oberon found in ProjectOberon,
    // e.g. arrow := SYSTEM.ADR($0F0F 0060 0070 0038 001C 000E 0007 8003 C101 E300 7700 3F00 1F00 3F00 7F00 FF00$);
    // in Display.Mod; sogar über mehrere Zeilen zulässig

    const int startLine = d_lineNr;
    const int startCol = d_colNr;

    int pos = d_line.indexOf( "$", d_colNr + 1 );
    QByteArray str;
    while( pos == -1 && !d_in->atEnd() )
    {
        if( !str.isEmpty() )
            str += '\n';
        str += d_line.mid( d_colNr );
        nextLine();
        countLine();
        pos = d_line.indexOf( "$" );
    }
    if( pos == -1 )
    {
        d_colNr = d_line.size();
        Token t( Tok_Invalid, startLine, startCol + 1, str.size(), tr("non-terminated hexadecimal string").toLatin1() );
        return t;
    }
    // else
    pos++; // konsumiere $
    if( !str.isEmpty() )
        str += '\n';
    str += d_line.mid( d_colNr, pos - d_colNr );

    bool ok = true;
    for( int i = 1; i < str.size() - 2; i++ )
    {
        if( !isHexDigit( str[i] ) && !::isspace(str[i]) )
            ok = false;
    }
    Token t( Tok_hexstring, startLine, startCol, str.size(), str );
    if( !ok )
    {
        t.d_type = Tok_Invalid;
        t.d_val = "invalid hexadecimal string";
    }
    t.d_sourcePath = d_sourcePath;
    d_lastToken = t;
    t.d_len += 1;
    d_colNr = pos;
    return t;
}

bool Lexer::isHexstring(int off) const
{
    for( int i = d_colNr + off; i < d_line.size(); i++ )
    {
        const char ch = d_line[i];
        if( ch == '$' )
            return true;
        if( !isHexDigit(ch) && !::isspace(ch) )
            return false;
    }
    const QByteArray buf = d_in->peek(1000); // RISK
    for( int i = 0; i < buf.size(); i++ )
    {
        const char ch = buf[i];
        if( ch == '$' )
            return true;
        if( !isHexDigit(ch) && !::isspace(ch) )
            return false;
    }
    return false;
}

void Lexer::countLine()
{
    if( !d_lineCounted )
        d_sloc++;
    d_lineCounted = true;
}

bool Lexer::isUtf8(const QByteArray& str)
{
    if( isAscii(str) )
        return false;
    // Quelle: https://stackoverflow.com/questions/1031645/how-to-detect-utf-8-in-plain-c
    // see also http://www.perlmonks.org/?node=910102
    const unsigned char * bytes = (const unsigned char *)str.constData();
    while(*bytes)
    {
        if( (// ASCII
             // use bytes[0] <= 0x7F to allow ASCII control characters
                bytes[0] == 0x09 ||
                bytes[0] == 0x0A ||
                bytes[0] == 0x0D ||
                (0x20 <= bytes[0] && bytes[0] <= 0x7E)
            )
        ) {
            bytes += 1;
            continue;
        }

        if( (// non-overlong 2-byte
                (0xC2 <= bytes[0] && bytes[0] <= 0xDF) &&
                (0x80 <= bytes[1] && bytes[1] <= 0xBF)
            )
        ) {
            bytes += 2;
            continue;
        }

        if( (// excluding overlongs
                bytes[0] == 0xE0 &&
                (0xA0 <= bytes[1] && bytes[1] <= 0xBF) &&
                (0x80 <= bytes[2] && bytes[2] <= 0xBF)
            ) ||
            (// straight 3-byte
                ((0xE1 <= bytes[0] && bytes[0] <= 0xEC) ||
                    bytes[0] == 0xEE ||
                    bytes[0] == 0xEF) &&
                (0x80 <= bytes[1] && bytes[1] <= 0xBF) &&
                (0x80 <= bytes[2] && bytes[2] <= 0xBF)
            ) ||
            (// excluding surrogates
                bytes[0] == 0xED &&
                (0x80 <= bytes[1] && bytes[1] <= 0x9F) &&
                (0x80 <= bytes[2] && bytes[2] <= 0xBF)
            )
        ) {
            bytes += 3;
            continue;
        }

        if( (// planes 1-3
                bytes[0] == 0xF0 &&
                (0x90 <= bytes[1] && bytes[1] <= 0xBF) &&
                (0x80 <= bytes[2] && bytes[2] <= 0xBF) &&
                (0x80 <= bytes[3] && bytes[3] <= 0xBF)
            ) ||
            (// planes 4-15
                (0xF1 <= bytes[0] && bytes[0] <= 0xF3) &&
                (0x80 <= bytes[1] && bytes[1] <= 0xBF) &&
                (0x80 <= bytes[2] && bytes[2] <= 0xBF) &&
                (0x80 <= bytes[3] && bytes[3] <= 0xBF)
            ) ||
            (// plane 16
                bytes[0] == 0xF4 &&
                (0x80 <= bytes[1] && bytes[1] <= 0x8F) &&
                (0x80 <= bytes[2] && bytes[2] <= 0xBF) &&
                (0x80 <= bytes[3] && bytes[3] <= 0xBF)
            )
        ) {
            bytes += 4;
            continue;
        }

        return false;
    }

    return true;
}

bool Lexer::isAscii(const QByteArray& str)
{
    const quint8* bytes = (const quint8*)str.constData();
    for( int i = 0; i < str.size(); i++ )
    {
        if( bytes[i] >= 0x80 )
            return false;
    }
    return true;
}

bool Lexer::isValidIdent(const QByteArray& str)
{
    if( str.isEmpty() || !isAscii(str) )
        return false;

    if( !(::isalpha(str[0]) || str[0] == '_') )
        return false;
    for( int i = 1; i < str.size(); i++ )
    {
        if( !::isalnum(str[i]) && str[i] != '_')
            return false;
    }
    if( isAllLowerCase(str) && tokenTypeFromString( str.toUpper() ) != Tok_Invalid )
        return false;
    if( tokenTypeFromString( str ) != Tok_Invalid )
        return false;
    return true;
}
