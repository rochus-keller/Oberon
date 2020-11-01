#ifndef OBFILECACHE_H
#define OBFILECACHE_H

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
        struct Entry
        {
            QString d_desig; // file path or module name
            bool d_isModuleName;
            QByteArray d_code;
            Entry():d_isModuleName(false) {}
        };

        explicit FileCache(QObject *parent = 0);

        void addFile( const QString& path, const QByteArray& code, bool isModuleName = false );
        void removeFile( const QString& path );
        Entry getFile( const QString& path, bool* found = 0) const;

    private:
        typedef QHash<QString,Entry> Files; // filepath -> Entry
        Files d_files;
        mutable QReadWriteLock d_lock;
    };
}

#endif // OBFILECACHE_H
