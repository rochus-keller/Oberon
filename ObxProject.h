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
#include <QExplicitlySharedDataPointer>
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
    struct Module;

    class Project : public QObject
    {
        Q_OBJECT
    public:
        struct FileGroup;
        struct File : public QSharedData
        {
            QString d_filePath;
            FileGroup* d_group;
            Ref<Module> d_mod;
            bool d_isLib;
            File():d_isLib(false),d_group(0){}
        };
        typedef QExplicitlySharedDataPointer<File> FileRef;

        struct FileGroup
        {
            VirtualPath d_package;
            QList<File*> d_files;
        };

        typedef QList<FileGroup> FileGroups;
        typedef QHash<QString,FileRef> FileHash; // FilePath -> File
        typedef QPair<File*,Module*> FileMod;
        typedef QHash<QByteArray,FileMod> ModuleHash; // Module.fullName -> File
        typedef QList<FileRef> FileList;
        typedef QPair<QByteArray,QByteArray> ModProc; // module.procedure or just module

        explicit Project(QObject *parent = 0);
        void clear();

        void createNew();
        bool initializeFromDir( const QDir&, bool recursive = false );
        bool initializeFromPackageList( const PackageList& );
        bool loadFrom( const QString& filePath );
        bool save();
        bool saveTo(const QString& filePath );
        void setSuffixes( const QStringList& ); // Form: ".suffix"
        const QStringList& getSuffixes() const { return d_suffixes; }
        const QString& getProjectPath() const { return d_filePath; }
        bool isDirty() const { return d_dirty; }

        void setMain( const ModProc& );
        const ModProc& getMain() const { return d_main; }
        QString renderMain() const;
        void setUseBuiltInOakwood(bool);
        bool useBuiltInOakwood() const { return d_useBuiltInOakwood; }
        void setUseBuiltInObSysInner(bool);
        bool useBuiltInObSysInner() const { return d_useBuiltInObSysInner; }
        QString getWorkingDir(bool resolved = false) const;
        void setWorkingDir( const QString& );
        QString getBuildDir(bool resolved = false) const;
        void setBuildDir( const QString& );

        bool addFile(const QString& filePath, const VirtualPath& package = QByteArrayList() );
        bool removeFile( const QString& filePath );
        bool addPackagePath(const VirtualPath& path );
        bool removePackagePath( const VirtualPath& path );

        bool reparse();

        const FileHash& getFiles() const { return d_files; }
        const FileGroups& getFileGroups() const { return d_groups; }
        FileGroup getRootFileGroup() const;
        FileGroup findFileGroup(const VirtualPath& package ) const;
        QList<Module*> getModulesToGenerate(bool includeTemplates=false) const; // in exec/depencency order
        FileMod findFile( const QString& file ) const;
        Model* getMdl() const { return d_mdl; }

        Expression* findSymbolBySourcePos(const QString& file, quint32 line, quint16 col, Scope** = 0 ) const;
        Expression* findSymbolBySourcePos(Module*, quint32 line, quint16 col, Scope** scopePtr) const;
        ExpList getUsage( Named* ) const;
        bool printTreeShaken( const QString& module, const QString& fileName );
        bool printImportDependencies(const QString& fileName , bool pruned);

        Ob::Errors* getErrs() const;
        Ob::FileCache* getFc() const;
    signals:
        void sigModified(bool);
        void sigRenamed();
        void sigReparsed();
    protected:
        QStringList findFiles(const QDir& , bool recursive = false);
        void touch();
        int findPackage(const VirtualPath& path ) const;
        // bool generate( Module* );
    private:
        Model* d_mdl;

        FileHash d_files;
        ModuleHash d_modules;
        FileGroups d_groups;
        QString d_filePath; // path where the project file was loaded from or saved to
        QStringList d_suffixes;
        QString d_workingDir, d_buildDir;
        ModProc d_main;
        bool d_dirty;
        bool d_useBuiltInOakwood;
        bool d_useBuiltInObSysInner;
    };
}

#endif // OBXPROJECT_H
