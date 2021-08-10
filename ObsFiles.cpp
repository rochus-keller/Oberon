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

#include "ObsFiles.h"
#include <QDir>
#include <QDateTime>
#include <QBuffer>
#include <stdint.h>
#include <QtDebug>

#ifdef _WIN32
#define DllExport __declspec(dllexport)
#else
#define DllExport
#endif

static QFileInfoList s_files;
static QString s_root;
static QFile s_disk;

void Obs::Files::setFileSystemRoot(const QString& dirPath) // cheat so that ObsFiles need no header just for this
{
    s_root = dirPath;
}

typedef uint8_t CharArray[];

static inline QString getPath()
{
    QString str = s_root;
    if( str.isEmpty() )
        str = QDir::currentPath();
    return str;
}

static QList<QBuffer*> s_buffers;

static int getFreeBufferSlot(QBuffer* b)
{
    for( int i = 0; i < s_buffers.size(); i++ )
    {
        if( s_buffers[i] == 0 )
        {
            s_buffers[i] = b;
            return i;
        }
    }
    s_buffers.append( b );
    return s_buffers.size() - 1;
}

static inline QBuffer* getBuffer(int i)
{
    if( i >= 0 && i < s_buffers.size() )
        return s_buffers[i];
    else
        return 0;
}

extern "C"
{

DllExport int ObsFiles_setRootPath( const char* path )
{
    s_root = QString::fromLatin1(path);
    return 0;
}

DllExport int ObsFiles_listFiles()
{
    QString str = getPath();
    QDir dir( str );
    s_files = dir.entryInfoList( QDir::Files | QDir::Readable | QDir::Writable );
    return s_files.size();
}

DllExport const char* ObsFiles_fileName( int i )
{
    static QByteArray name;
    name = s_files[i].fileName().left(31).toUtf8();
    return name.constData();
}

DllExport uint32_t ObsFiles_fileSize( int i )
{
    return s_files[i].size();
}

DllExport uint32_t ObsFiles_fileTime( int i )
{
    return s_files[i].created().toTime_t();
}

DllExport int ObsFiles_openFile( CharArray filename )
{
    QDir dir( getPath() );
    const QString path = dir.absoluteFilePath( QString::fromLatin1((char*)filename) );
    if( !QFileInfo(path).isFile() )
        return -1;
    QFile f( path );
    if( f.exists() )
    {
        if( !f.open(QIODevice::ReadOnly) )
        {
            qWarning() << "*** could not open for reading" << f.fileName();
            return false;
        }
        QBuffer* b = new QBuffer();
        b->setData(f.readAll());
        b->open( QIODevice::ReadWrite );
        return getFreeBufferSlot(b);
    }else
        return -1;
}

DllExport int ObsFiles_newFile()
{
    QBuffer* b = new QBuffer();
    b->open( QIODevice::ReadWrite );
    return getFreeBufferSlot(b);
}

DllExport void ObsFiles_freeFile(int fb)
{
    if( fb >= 0 && fb < s_buffers.size() )
    {
        if( s_buffers[fb] )
            delete s_buffers[fb];
        s_buffers[fb] = 0;
    }
}

DllExport int ObsFiles_saveFile( CharArray filename, int fb )
{
    QDir dir( getPath() );
    QFile f( dir.absoluteFilePath( QString::fromLatin1((char*)filename)) );
    QBuffer* buf = getBuffer(fb);
    if( buf )
    {
        if( !f.open(QIODevice::WriteOnly) )
            qWarning() << "*** could not open for writing" << f.fileName();
        buf->close();
        f.write(buf->data());
        buf->open( QIODevice::ReadWrite );
        return true;
    }else
        return false;
}

DllExport int ObsFiles_removeFile( CharArray filename )
{
    QDir dir( getPath() );
    return dir.remove(QString::fromLatin1((char*)filename));
}

DllExport int ObsFiles_renameFile( CharArray oldName, CharArray newName )
{
    QDir dir( getPath() );
    const QString old = QString::fromLatin1((const char*)oldName);
    const QString _new = QString::fromLatin1((const char*)newName);
    if( !QFileInfo(dir.absoluteFilePath(old)).exists() )
        return false; // not found
    QFileInfo info(dir.absoluteFilePath(_new));
    if( info.exists() )
    {
        dir.remove(_new);
        return dir.rename(old,_new);
    }else
        return dir.rename(old,_new);
}

DllExport uint32_t ObsFiles_length( int fb )
{
    QBuffer* buf = getBuffer(fb);
    if( buf )
        return buf->size();
    else
        return 0;
}

DllExport int ObsFiles_setPos( int fb, int pos )
{
    QBuffer* buf = getBuffer(fb);
    if( buf )
    {
        if( pos < 0 ) // it happens a few times that -1 instead of 0 is passed; according to Oberon book should always be >= 0
            pos = 0;
        if( !buf->seek(pos) )
        {
            qWarning() << "*** could not seek to" << pos << buf->pos() << buf->size();
            return false;
        }
        return true;
    }else
        return false;
}

DllExport int ObsFiles_getPos( int fb )
{
    QBuffer* buf = getBuffer(fb);
    if( buf )
        return buf->pos();
    else
        return 0;
}


DllExport int ObsFiles_atEnd( int fb )
{
    QBuffer* buf = getBuffer(fb);
    if( buf )
        return buf->atEnd();
    else
        return false;
}

DllExport int ObsFiles_writeByte( int fb, uint32_t byte )
{
    QBuffer* buf = getBuffer(fb);
    if( buf )
        return buf->putChar( (char) (byte & 0xff) );
    else
        return false;
}

DllExport uint32_t ObsFiles_readByte( int fb )
{
    QBuffer* buf = getBuffer(fb);
    if( buf )
    {
        char ch;
        if( buf->getChar( &ch ) )
            return (quint8)ch;
        else
            return 0;
    }else
        return 0;
}

}
