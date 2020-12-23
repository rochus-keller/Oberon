#ifndef OBXMODEL_H
#define OBXMODEL_H

/*
* Copyright 2020 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the OBX parser/code model library.
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

#include <Oberon/ObxParser.h>

namespace Ob
{
    class Errors;
    class FileCache;
}
namespace Obx
{
    class Model : public QObject
    {
    public:
        struct FileGroup
        {
            QString d_root; // absolute path to root directory of file group
            QStringList d_files; // file paths relative to root
            static QByteArrayList toFullName(const QString& relativeFileName);
            QStringList absolutePaths() const;
            static FileGroup fromPaths( const QString& root, const QStringList& files );
        };
        typedef QList<FileGroup> FileGroups;

        explicit Model(QObject *parent = 0);
        void clear();

        bool parseFiles(const FileGroups& files);
        Ref<Module> parseFile( const QString& path ) const;
        Ref<Module> parseFile(QIODevice* , const QString& path) const;
        Ob::Errors* getErrs() const { return d_errs; }

    protected:
        void unbindFromGlobal();
        void fillGlobals();
        bool resolveImports();
        bool findProcessingOrder();
        bool error( const QString& file, const QString& msg );
        bool error( const Ob::Loc& loc, const QString& msg );
    private:
        Ref<Scope> d_globals;
        QHash<QByteArray,QByteArray> d_preload;
        Ref<BaseType> d_boolType;
        Ref<BaseType> d_charType;
        Ref<BaseType> d_byteType;
        Ref<BaseType> d_intType;
        Ref<BaseType> d_shortType;
        Ref<BaseType> d_longType;
        Ref<BaseType> d_realType;
        Ref<BaseType> d_doubleType;
        Ref<BaseType> d_setType;
        Ref<BaseType> d_stringType;
        Ref<BaseType> d_nilType;
        Ref<BaseType> d_anyType;
        Ref<BaseType> d_anyNum;
        Ref<Module> d_systemModule;
        QList<Module*> d_depOrder; // most (0) to least (n-1) dependent
        typedef QHash<QByteArrayList,Ref<Module> > Modules;
        Modules d_modules;

        Ob::Errors* d_errs;
        Ob::FileCache* d_fc;
    };
}

#endif // OBXMODEL_H
