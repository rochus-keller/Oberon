#ifndef OBLEXER_H
#define OBLEXER_H

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

#include <QObject>
#include <Oberon/ObToken.h>
#include <QHash>

class QIODevice;

namespace Ob
{
    class Errors;
    class FileCache;

    class Lexer : public QObject
    {
    public:
        explicit Lexer(QObject *parent = 0);

        void setStream( QIODevice*, const QString& sourcePath );
        bool setStream(const QString& sourcePath);
        void setErrors(Errors* p) { d_err = p; }
        void setCache(FileCache* p) { d_fcache = p; }
        void setIgnoreComments( bool b ) { d_ignoreComments = b; }
        void setPackComments( bool b ) { d_packComments = b; }
        void setEnableExt( bool b ) { d_enableExt = b; }
        bool isEnabledExt() const { return d_enableExt; }
        void setSensExt( bool b ) { d_sensExt = b; }

        Token nextToken();
        Token peekToken(quint8 lookAhead = 1);
        QList<Token> tokens( const QString& code );
        QList<Token> tokens( const QByteArray& code, const QString& path = QString() );
        quint32 getSloc() const { return d_sloc; }

        static QByteArray getSymbol( const QByteArray& );
        static void parseComment( const QByteArray& str, int& pos, int& level );
        static bool skipOberonHeader( QIODevice* );
        static bool isV4File( QIODevice* );
        static QByteArray readV4Text(QIODevice*);
        static bool skipBom( QIODevice* );
        static bool isUtf8(const QByteArray& str);
        static bool isAscii( const QByteArray& str );
        static bool isValidIdent( const QByteArray& str );
    protected:
        Token nextTokenImp();
        int skipWhiteSpace();
        void nextLine();
        int lookAhead(int off = 1) const;
        Token token(TokenType tt, int len = 1, const QByteArray &val = QByteArray());
        Token ident();
        Token number();
        Token comment();
        Token string();
        Token hexstring();
        bool isHexstring(int off = 1) const;
        void countLine();
    private:
        QIODevice* d_in;
        Errors* d_err;
        FileCache* d_fcache;
        quint32 d_lineNr;
        quint16 d_colNr;
        QString d_sourcePath;
        QByteArray d_line;
        QList<Token> d_buffer;
        quint32 d_sloc; // number of lines of code without empty or comment lines
        static QHash<QByteArray,QByteArray> d_symbols;
        Token d_lastToken;
        bool d_ignoreComments;  // don't deliver comment tokens
        bool d_packComments;    // Only deliver one Tok_Comment for (*...*) instead of Tok_Latt and Tok_Ratt
        bool d_enableExt; // Allow for both uppercase and lowercase keywords and for idents with underscores as in C
        bool d_sensExt; // Autosense language extension (first keyword MODULE, module, DEFINITION, definition)
        bool d_sensed;
        bool d_lineCounted;
    };
}

#endif // OBLEXER_H
