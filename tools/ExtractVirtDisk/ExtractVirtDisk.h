#ifndef EXTRACTVIRTDISK_H
#define EXTRACTVIRTDISK_H

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

#include <QFile>

class VirtualDisk : public QObject
{
public:
    enum { SS = 2048, // sector size
         };

    VirtualDisk( QObject* parent = 0):QObject(parent){}
    bool open(const QString& path);
    bool GetSector(quint32 src, QByteArray& dest );
    const QString& getError() const { return d_error; }
protected:
    bool InSector(quint32& sec);
private:
    QString d_path;
    QFile d_file;
    QString d_error;
};

class Directory
{
public:
    enum {
        DirRootAdr = 29,
        FnLength    = 32,
        FillerSize = 36,
        DirPgSize   = 50,
        ExTabSize   = 12,
        SecTabSize   = 64,
        HeaderSize  = 352
    };
    struct File {
        QByteArray name;
        QByteArray data;
    };

    Directory(VirtualDisk*);

    bool read();
    int count() const { return files.size(); }
    File getFile(int i) const;

    const QString& getError() const { return d_error; }
protected:

    struct DirEntry { // B-tree node
        char name[FnLength];
        quint32 adr; // sec no of file header
        quint32 p;   // sec no of descendant in directory
    };

    struct DirPage {
        quint32 mark;
        quint16 m;
        quint32 p0;  //sec no of left descendant in directory
        char fill[FillerSize];
        DirEntry e[DirPgSize];
    };

    struct FileHeader {
        // allocated in the first page of each file on disk
        quint32 mark;
        char name[FnLength];
        quint16 aleng; // number of sectors the file uses in addition to the first sector
        quint16 bleng; // number of bytes used in the not fully consumed sector including the header
        quint32 date, time;
        quint32 ext[ExTabSize];
        quint32 sec[SecTabSize]; // the file header is in sec[0]
        char fill[VirtualDisk::SS - HeaderSize];
        quint32 size() const { return aleng * VirtualDisk::SS + bleng - HeaderSize; }
    };


    bool enumerate(quint32 sector);
private:
    VirtualDisk* disk;
    QList<FileHeader> files;
    mutable QString d_error;
};

#endif // EXTRACTVIRTDISK_H
