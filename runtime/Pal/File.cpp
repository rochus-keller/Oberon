/*
* Copyright 2023 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon+ parser/compiler library.
*
* The following is the license that applies to this copy of the
* file. For a license to use the file under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* This file may be used under the terms of the GNU Lesser
* General Public License version 2.1 or version 3 as published by the Free
* Software Foundation and appearing in the file LICENSE.LGPLv21 and
* LICENSE.LGPLv3 included in the packaging of this file. Please review the
* following information to ensure the GNU Lesser General Public License
* requirements will be met: https://www.gnu.org/licenses/lgpl.html and
* http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
*/

#include <QFile>
#include <QFileInfo>
#include <QDir>
#include <QTemporaryDir>
#include <QTemporaryFile>
#include <QElapsedTimer>
#include <QtDebug>
#include <iostream>

class FileContext
{
public:
    QString fsroot;
    QByteArrayList fileNames; // in Latin-1
    QList<QFile*> files;
    QTemporaryDir tmpdir;
    QHash<QString,int> open;
    QElapsedTimer timer;

    FileContext()
    {
        timer.start();
    }
    ~FileContext()
    {
        for( int i = 0; i < files.size(); i++ )
            if( files[i] )
                delete files[i];
    }

    int32_t nextFreeBuffer()
    {
        for( int i = 0; i < files.size(); i++ )
        {
            if( files[i] == 0 )
            {
                return i;
            }
        }
        files.append(0);
        return files.size() - 1;
    }

    QString getRootPath()
    {
        QString res = fsroot;
        if( res.isEmpty() )
        {
            res = qgetenv("OBERON_FILE_SYSTEM_ROOT"); // an absolute path including the "Files" suffix if need be
            QFileInfo info(res);
            if( !res.isEmpty() && info.isDir() && info.isAbsolute() && info.exists() )
                return res;

            res = QDir::current().absoluteFilePath("Files");
        }
        return res;
    }

    int32_t file_list()
    {
        const QStringList names = QDir(getRootPath()).entryList(QDir::Files,QDir::Name);
        fileNames.clear();
        foreach( const QString& name, names )
            fileNames.append( name.toLatin1() );
        return fileNames.size();
    }

    const char* file_name(int32_t i) // Latin-1
    {
        if( i >= 0 && i < fileNames.size() )
            return fileNames[i].constData();
        else
            return "";
    }

    int32_t file_open(const char* filename)
    {
        const QString name = QString::fromLatin1(filename);
        if( name.isEmpty() )
            return -1;

        if( open.contains(name) )
            return open.value(name);

        QFile f( QDir(getRootPath()).absoluteFilePath(name) );
        if( !f.open(QIODevice::ReadWrite) )
        {
            qCritical() << "cannot open file for reading:" << f.fileName();
            return -1;
        }

        const int id = nextFreeBuffer();
        const QString tmpPath = QDir(tmpdir.path()).absoluteFilePath(name);
        f.copy( tmpPath );
        f.close();

        files[id] = new QFile(tmpPath);
        files[id]->open(QIODevice::ReadWrite);
        open[name] = id;

        return id;
    }

    int32_t file_new()
    {
        const int id = nextFreeBuffer();
        files[id] = new QTemporaryFile();
        files[id]->open(QIODevice::ReadWrite);
        return id;
    }

    void file_free(int32_t id)
    {
        if( id >= 0 && id < files.size() && files[id] )
        {
            files[id]->remove();
            delete files[id];
            files[id] = 0;
            QHash<QString,int>::iterator i;
            for( i = open.begin(); i != open.end(); ++i )
            {
                if( i.value() == id )
                {
                    open.remove(i.key());
                    break;
                }
            }
        }
    }

    int file_save(const char* filename, int32_t id)
    {
        const QString name = QString::fromLatin1(filename);
        if( name.isEmpty() )
            return 0; // cannot save file with empty name
        QHash<QString,int>::const_iterator i = open.find(name);
        if( i != open.end() && i.value() != id )
            return 0; // already saved, but under a different name
        if( id >= 0 && id < files.size() && files[id] )
        {
            const QString to = QDir(getRootPath()).absoluteFilePath(name);
            QFile::remove(to);
            files[id]->copy(to);
            open[name] = id;
            return 1;
        }else
            return -1;
    }

    int file_remove(const char* filename)
    {
        const QString name = QString::fromLatin1(filename);
        if( open.contains(name) )
            file_free(open.value(name));
        const QString to = QDir(getRootPath()).absoluteFilePath(name);
        return QFile::remove(to) ? 1 : 0;
    }

    int file_rename(const char* oldName, const char* newName)
    {
        const QString name = QString::fromLatin1(oldName);
        const QString name2 = QString::fromLatin1(newName);
        if( open.contains(name) )
        {
            const QString from = QDir(tmpdir.path()).absoluteFilePath(name);
            const QString to = QDir(tmpdir.path()).absoluteFilePath(name2);
            if( !QFile::rename(from,to) )
                return 0;
            const int tmp = open.value(name);
            open.remove(name);
            open.insert(name2,tmp);
        }
        const QString from = QDir(getRootPath()).absoluteFilePath(name);
        const QString to = QDir(getRootPath()).absoluteFilePath(name2);
        return QFile::rename(from,to) ? 1 : 0;
    }

    int32_t file_length(int32_t id)
    {
        if( id >= 0 && id < files.size() && files[id] )
        {
            return files[id]->size();
        }else
            return 0;
    }

    int file_seek(int32_t id, int32_t pos)
    {
        if( id >= 0 && id < files.size() && files[id] )
        {
            return files[id]->seek(pos) ? 1 : 0;
        }else
            return 0;
    }

    int32_t file_pos(int32_t id)
    {
        if( id >= 0 && id < files.size() && files[id] )
        {
            return files[id]->pos();
        }else
            return 0;
    }

    int file_eof(int32_t id)
    {
        if( id >= 0 && id < files.size() && files[id] )
        {
            return files[id]->atEnd() ? 1 : 0;
        }else
            return 0;
    }

    int file_write_byte(int32_t id, int32_t byte_)
    {
        if( id >= 0 && id < files.size() && files[id] )
        {
            return files[id]->putChar((char)( byte_ & 0xff ) ) ? 1 : 0;
        }else
            return 0;
    }

    int32_t file_read_byte(int32_t id)
    {
        char ch;
        if( id >= 0 && id < files.size() && files[id] && files[id]->getChar(&ch) )
        {
            return (quint8)ch;
        }else
            return -1;
    }

    int32_t file_key(const char* filename)
    {
        const QString name = QString::fromLatin1(filename);
        QHash<QString,int>::const_iterator i = open.find(name);
        if( i != open.end() )
            return i.value();
        else
            return -1;
    }
};

static FileContext* s_ctx = 0;

static inline FileContext* ctx()
{
    if( s_ctx == 0 )
        s_ctx = new FileContext();
    return s_ctx;
}

extern "C" {
#include "ObxPalApi.h"

int32_t PAL_time()
{
    return ctx()->timer.elapsed();
}

void PAL_dispose()
{
    if( s_ctx )
        delete s_ctx;
    s_ctx = 0;
}

void PAL_printChar(char ch)
{
    std::cout << ch;
    if( ch == '\n' )
        std::cout << std::flush;
}

int32_t PAL_file_list()
{
    return ctx()->file_list();
}

const char* PAL_file_name(int32_t i) // Latin-1
{
    return ctx()->file_name(i);
}

int32_t PAL_file_open(const char* filename)
{
    return ctx()->file_open(filename);
}

int32_t PAL_file_key(const char* filename)
{
    return ctx()->file_key(filename);
}

int32_t PAL_file_new()
{
    return ctx()->file_new();
}

void PAL_file_free(int32_t id)
{
    ctx()->file_free(id);
}

int PAL_file_save(const char* filename, int32_t id)
{
    return ctx()->file_save(filename,id);
}

int PAL_file_remove(const char* filename)
{
    return ctx()->file_remove(filename);
}

int PAL_file_rename(const char* oldName, const char* newName)
{
    return ctx()->file_rename(oldName,newName);
}

int32_t PAL_file_length(int32_t id)
{
    return ctx()->file_length(id);
}

int PAL_file_seek(int32_t id, int32_t pos)
{
    return ctx()->file_seek(id,pos);
}

int32_t PAL_file_pos(int32_t id)
{
    return ctx()->file_pos(id);
}

int PAL_file_eof(int32_t id)
{
    return ctx()->file_eof(id);
}

int PAL_file_write_byte(int32_t id, int32_t byte_)
{
    return ctx()->file_write_byte(id,byte_);
}

int32_t PAL_file_read_byte(int32_t id)
{
    return ctx()->file_read_byte(id);
}

}
