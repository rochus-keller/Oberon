#ifndef OBXPROJECT_H
#define OBXPROJECT_H

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

#include <QObject>
#include <QStringList>
#include <Oberon/ObxAst.h>

class QDir;

namespace Ob
{
    class Errors;
    class FileCache;
}

namespace Obx
{
    class Model;

    class Project : public QObject
    {
        Q_OBJECT
    public:
        struct File
        {
            QString d_file;
            Ref<Module> d_mod;
            QByteArray d_sourceCode; // lua source or byte code
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
        void setUseBuiltInObSysInner(bool);
        bool useBuiltInObSysInner() const { return d_useBuiltInObSysInner; }
        bool addFile( const QString& );
        bool removeFile( const QString& );
        const QString& getFilePath() const { return d_filePath; }
        const FileHash& getFiles() const { return d_files; }
        bool isDirty() const { return d_dirty; }
        FileList getFilesInExecOrder() const;
        Expression* findSymbolBySourcePos(const QString& file, quint32 line, quint16 col ) const;
        ExpList getUsage( Named* ) const;
        QString getWorkingDir(bool resolved = false) const;
        void setWorkingDir( const QString& );
        bool printTreeShaken( const QString& module, const QString& fileName );

        Ob::Errors* getErrs() const;
        Ob::FileCache* getFc() const;
    signals:
        void sigModified(bool);
        void sigRenamed();
        void sigRecompiled();
    protected:
        QStringList findFiles(const QDir& , bool recursive = false);
        void touch();
    private:
        Model* d_mdl;

        FileHash d_files;
        QString d_filePath;
        QStringList d_suffixes;
        QString d_workingDir;
        ModProc d_main;
        bool d_dirty;
        bool d_useBuiltInOakwood;
        bool d_useBuiltInObSysInner;
    };
}

#endif // OBXPROJECT_H
