/*
* Copyright 2023 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon+ project.
*
* The following is the license that applies to this copy of the
* file. For a license to use the file under conditions
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

#include "ExtractVirtDisk.h"
#include <QCoreApplication>
#include <QDir>
#include <QFile>
#include <QtDebug>

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);

    if( a.arguments().size() != 2 )
    {
        qCritical() << "expecting one argument: path to the Oberon virtual disk file";
        return -1;
    }

    VirtualDisk disk;

    QFileInfo path(QFileInfo(a.arguments()[1]));
    if( !disk.open(path.filePath()) )
    {
        qCritical() << disk.getError();
        return -1;
    }

    QDir outDir( path.absoluteDir().absoluteFilePath(path.completeBaseName()) );
    if( !outDir.mkpath(outDir.path()) )
    {
        qCritical() << "cannot create directory" << outDir.path();
        return -1;
    }

    Directory files(&disk);
    if( !files.read() )
    {
        qCritical() << files.getError();
        return -1;
    }

    for( int i = 0; i < files.count(); i++ )
    {
        Directory::File f = files.getFile(i);
        if( f.name.isEmpty() && f.data.isEmpty() )
        {
            qCritical() << files.getError();
            return -1;
        }
        QFile out( outDir.absoluteFilePath( QString::fromLatin1(f.name) ) );
        if( !out.open(QIODevice::WriteOnly) )
        {
            qCritical() << "cannot write to file" << out.fileName();
            return -1;
        }
        out.write(f.data);
    }
    qDebug() << "successfully extracted files to" << outDir.path();
    return 0;
}

// NOTE that this code assumes x86 endianness!

bool VirtualDisk::open(const QString& path)
{
    d_file.setFileName(path);
    if( !d_file.open(QIODevice::ReadOnly) )
    {
        d_error = tr("cannot open file for reading: %1").arg(path);
        return false;
    }
    const QByteArray tag = d_file.read(4);
    if( tag.size() < 4 || tag[0] != char(0x8d) || tag[1] != char(0xa3) ||
            tag[2] != char(0x1e) || tag[3] != char(0x9b) )
    {
        d_file.close();
        d_error = tr("file has invalid format: %1").arg(path);
        return false;
    }
    return true;
}

bool VirtualDisk::GetSector(quint32 src, QByteArray& dest)
{
    if( !d_file.isOpen() )
    {
        d_error = "disk not ready";
        return false;
    }
    if( !InSector(src) )
        return false;

    if( src >= d_file.size() )
    {
        d_error = "invalid sector requested";
        return false;
    }
    d_file.seek(src);
    dest = d_file.read(SS);
    if( dest.size() != SS )
    {
        d_error = "sector too short";
        return false;
    }
    return true;
}

bool VirtualDisk::InSector(quint32& sec)
{
    if (sec < 29 || sec % 29 != 0 )
    {
        d_error = "Illegal sector address";
        return false;
    }
    sec = (sec / 29 - 1) * SS;
    return true;
}


Directory::Directory(VirtualDisk* disk)
{
    Q_ASSERT( disk );
    this->disk = disk;
}

bool Directory::read()
{
    files.clear();
    return enumerate(DirRootAdr);
}

Directory::File Directory::getFile(int i) const
{
    File f;
    if( i < 0 || i >= files.size() )
        return f;
    const FileHeader* fh = &files[i];
    f.name = fh->name;
    for( i = 0; i < fh->aleng + 1; i++)
    {
        if( i >= SecTabSize )
        {
            qCritical() << "truncated" << f.name; // we don't support extension tables yet
            return f;
        }
        QByteArray buf;
        if( !disk->GetSector(fh->sec[i], buf) )
        {
            d_error = QString("%1 when fetching %2").arg(disk->getError()).arg(fh->name);
            return File();
        }
        if( i == 0 )
        {
            f.data = buf.mid(HeaderSize);
        }else if( i == fh->aleng )
        {
            f.data += buf.left(fh->bleng);
        }else
        {
            f.data += buf;
        }
    }
    qDebug() << "extracted" << f.name << "with" << f.data.size() << "bytes";
    return f;
}

bool Directory::enumerate(quint32 dpg)
{
    QByteArray buf;
    if( !disk->GetSector(dpg, buf) )
    {
        d_error = disk->getError();
        return false;
    }
    if( buf.size() < sizeof(DirPage) )
    {
        d_error = "invalid directory page";
        return false;
    }
    DirPage* a = (DirPage*) buf.constData();
    quint16 i = 0;
    quint32 dpg1 = 0;
    while (i < a->m )
    {
        if( i == 0 )
            dpg1 = a->p0;
        else
            dpg1 = a->e[i-1].p;
        if( dpg1 != 0 )
            if( !enumerate(dpg1) )
                return false;
        QByteArray buf2;
        if( !disk->GetSector(a->e[i].adr, buf2) )
        {
            d_error = disk->getError();
            return false;
        }
        if( buf2.size() < sizeof(FileHeader) )
        {
            d_error = "invalid file header";
            return false;
        }
        FileHeader* fh = (FileHeader*)buf2.constData();
        files.append(*fh);

        i++;
    }
    if ( i > 0 && a->e[i-1].p != 0)
        if( !enumerate(a->e[i-1].p) )
            return false;
    return true;
}
