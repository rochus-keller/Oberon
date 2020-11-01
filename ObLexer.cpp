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
    d_ignoreComments(true), d_packComments(true),d_enableExt(false), d_sensExt(false), d_sensed(false)
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

        if( ch == '"' )
            return string();
        else if( ch == '$')
            return hexstring();
        else if( ::isalpha(ch) || ( d_enableExt && ch == '_' ) )
            return ident();
        else if( ::isdigit(ch) )
            return number();
        // else
        int pos = d_colNr;
        TokenType tt = tokenTypeFromString(d_line,&pos);

        if( tt == Tok_Latt )
            return comment();
        else if( d_enableExt && tt == Tok_2Slash )
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
    while( d_colNr < d_line.size() && ::isspace( d_line[d_colNr] ) )
        d_colNr++;
    return d_colNr - colNr;
}

void Lexer::nextLine()
{
    d_colNr = 0;
    d_lineNr++;
    d_line = d_in->readLine();

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
        if( !QChar(c).isLetterOrNumber() // QChar wegen möglichen Umlauten
                && ( !d_enableExt || c != '_' ) )
            break;
        else
            off++;
    }
    const QByteArray str = d_line.mid(d_colNr, off );
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

static inline bool isHexDigit( char c )
{
    return ::isdigit(c) || c == 'A' || c == 'B' || c == 'C' || c == 'D' || c == 'E' || c == 'F';
}

static inline bool checkHexNumber( QByteArray str )
{
    const int pos = str.indexOf('\'');
    if( pos != -1 )
        str = str.left(pos);
    if( str.size() < 2 || ( !str.endsWith('H') && !str.endsWith('X') ) )
        return false;
    else
        return true;
}

Token Lexer::number()
{
    // TODO
    // integer ::= // digit {digit} | digit {hexDigit} 'H'
    // real ::= // digit {digit} '.' {digit} [ScaleFactor]
    // ScaleFactor- ::= // 'E' ['+' | '-'] digit {digit}
    int off = 1;
    while( true )
    {
        const char c = lookAhead(off);
        if( !isHexDigit(c) )
            break;
        else
            off++;
    }
    bool isHex = false;
    bool isChar = false;
    bool isReal = false;
    const char o1 = lookAhead(off);
    if( o1 == 'H' )
    {
        isHex = true;
        off++;
    }else if( o1 == 'X' )
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
        }
        if( lookAhead(off) == 'E' )
        {
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
        if( str.size() == 4 )
        {
            if( str[0] != '0' )
                return token( Tok_Invalid, off, "invalid hex char" );
            str = str.mid(1);
        }else if( str.size() > 4 )
            return token( Tok_Invalid, off, "invalid hex char" );
        return token( Tok_hexchar, off, str );
    }
    else if( isReal)
        return token( Tok_real, off, str );
    else
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
    int off = 1;
    while( true )
    {
        const char c = lookAhead(off);
        off++;
        if( c == '"' )
            break;
        if( c == 0 )
            return token( Tok_Invalid, off, "non-terminated string" );
    }
    const QByteArray str = d_line.mid(d_colNr, off );
    return token( Tok_string, off, str );
}

Token Lexer::hexstring()
{
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

