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

#include "ObFileCache.h"
#include <QFile>
#include <QBuffer>
#include <QFileInfo>
using namespace Ob;

// TODO: brauchen wir hier canonicalPaths?
// #define _USE_CANONOCALS

FileCache::FileCache(QObject *parent) : QObject(parent)
{
}

void FileCache::addFile(const QString& filePath, const QByteArray& code, bool isModuleName)
{
#ifdef _USE_CANONOCALS
    const QString cpath = isModuleName ? filePath : QFileInfo(filePath).canonicalFilePath();
#else
    const QString cpath = filePath;
#endif
    Entry e;
    e.d_code = code;
    e.d_desig = cpath;
    e.d_isModuleName = isModuleName;
    e.d_modified = QDateTime::currentDateTime();
    d_lock.lockForWrite();
    d_files[e.d_desig] = e;
    d_lock.unlock();
}

void FileCache::removeFile(const QString& path)
{
#ifdef _USE_CANONOCALS
    const QString cpath = QFileInfo(path).canonicalFilePath();
#else
    const QString cpath = path;
#endif
    d_lock.lockForWrite();
    d_files.remove(cpath);
    d_lock.unlock();
}

FileCache::Entry FileCache::getFile(const QString& path, bool* found) const
{
    Entry res;
#ifdef _USE_CANONOCALS
    const QString cpath = QFileInfo(path).canonicalFilePath();
#else
    const QString cpath = path;
#endif

    d_lock.lockForRead();

    if( found )
        *found = false;
    Files::const_iterator i = d_files.find(cpath);
    if( i != d_files.end() )
    {
        res = i.value();
        if( found )
            *found = true;
    }

    d_lock.unlock();

    return res;
}
