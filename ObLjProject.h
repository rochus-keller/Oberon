#ifndef OBLJPROJECT_H
#define OBLJPROJECT_H

/*
* Copyright 2020 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon parser/compiler library.
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
#include <QStringList>
#include <Oberon/ObAst.h>

class QDir;

namespace Ob
{
    class Project : public QObject
    {
        Q_OBJECT
    public:
        struct File
        {
            QString d_file;
            Ast::Ref<Ast::Module> d_mod;
            QByteArray d_bc;
            bool d_isLib;
            File(const QString& f = QString()):d_isLib(false),d_file(f){}
        };
        typedef QHash<QString,File> FileHash;
        typedef QList<File> FileList;
        typedef QPair<QByteArray,QByteArray> ModProc; // module.procedure or just module

        explicit Project(QObject *parent = 0);
        void clear();

        bool loadFrom( const QString& filePath );
        void createNew();
        bool recompile();
        bool generate();
        bool save();
        bool saveTo(const QString& filePath );
        bool initializeFromDir( const QDir&, bool recursive = false );
        void setSuffixes( const QStringList& ); // Form: ".suffix"
        const QStringList& getSuffixes() const { return d_suffixes; }
        void setMain( const ModProc& );
        const ModProc& getMain() const { return d_main; }
        void setUseBuiltInOakwood(bool);
        bool useBuiltInOakwood() const { return d_useBuiltInOakwood; }
        bool addFile( const QString& );
        bool removeFile( const QString& );
        const QString& getFilePath() const { return d_filePath; }
        const FileHash& getFiles() const { return d_files; }
        bool isDirty() const { return d_dirty; }
        FileList getFilesInExecOrder() const;

        Errors* getErrs() const;
        FileCache* getFc() const;
    signals:
        void sigModified(bool);
        void sigRenamed();
        void sigRecompiled();
    protected:
        QStringList findFiles(const QDir& , bool recursive = false);
        void touch();
    private:
        Ast::Model* d_mdl;

        FileHash d_files;
        QString d_filePath;
        QStringList d_suffixes;
        ModProc d_main;
        bool d_dirty;
        bool d_useBuiltInOakwood;
    };
}

#endif // OBLJPROJECT_H
