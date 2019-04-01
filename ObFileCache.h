#ifndef OBFILECACHE_H
#define OBFILECACHE_H

/*
* Copyright 2019 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include <QHash>
#include <QObject>
#include <QReadWriteLock>
#include <QStringList>

class QIODevice;

namespace Ob
{
    class FileCache : public QObject
    {
        // this class is thread-safe
    public:
        explicit FileCache(QObject *parent = 0);

        void addFile( const QString& path, const QByteArray& content );
        void removeFile( const QString& path );
        QByteArray getFile( const QString& path, bool* found = 0) const;

        void setSupportSvExt( bool b );
        bool supportSvExt() const;
        bool supportSvExt( const QString& path ) const;
        void setSvSuffix( const QStringList& s);
        QStringList svSuffix() const;

        // utility
        QByteArray fetchTextLineFromFile( const QString& path, int line, const QByteArray& defaultString = QByteArray() );
        QIODevice* createFileStreamForReading(const QString& path) const; // caller has to delete afterwards

    private:
        typedef QHash<QString,QByteArray> Files;
        Files d_files; // path->content
        QStringList d_svSuffix;
        bool d_supportSvExt;
        mutable QReadWriteLock d_lock;
    };
}

#endif // OBFILECACHE_H
