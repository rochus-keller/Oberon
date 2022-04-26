#ifndef OBXMODEL_H
#define OBXMODEL_H

/*
* Copyright 2021 Rochus Keller <mailto:me@rochus-keller.ch>
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
    class Model : public QObject, Instantiator
    {
    public:
        explicit Model(QObject *parent = 0);
        void clear();

        bool parseFiles(const PackageList& files);
        Ref<Module> parseFile( const QString& filePath );
        Ref<Module> parseFile(QIODevice* , const QString& filePath);
        const QList<Module*>& getDepOrder() const { return d_depOrder; }
        quint32 getSloc() const { return d_sloc; }
        void setOptions(const QByteArrayList& o) { d_options = o; }

        void setFillXref( bool b ) { d_fillXref = b; }
        typedef QHash<Named*,ExpList> XRef; // name used by ident expression
        const XRef& getXref() const { return d_xref; }

        Ref<Module> treeShaken(Module*) const;

        Ob::Errors* getErrs() const { return d_errs; }
        Ob::FileCache* getFc() const { return d_fc; }
        void addPreload(const QByteArray& name, const QByteArray& source);

        // Instantiator imp
        Module* instantiate( Module* generic, const MetaActuals& actuals );
        QList<Module*> instances( Module* generic );
    protected:
        void unbindFromGlobal();
        void fillGlobals();
        bool resolveImports();
        bool resolveImport(Module*);
        bool findProcessingOrder();
        QPair<Module*, Module*> findModule(const VirtualPath& package, const VirtualPath& module);
        bool error( const QString& file, const QString& msg );
        bool error( const Ob::Loc& loc, const QString& msg );
        bool warning( const Ob::Loc& loc, const QString& msg );
    private:
        struct CrossReferencer;
        Ref<Scope> d_globals;
        Ref<Scope> d_globalsLower;
        QHash<QByteArray,QByteArray> d_preload;
        Ref<BaseType> d_noType; // returned by calls to proper procs
        Ref<BaseType> d_boolType;
        Ref<BaseType> d_charType;  // 8 bit latin-1 (ISO-8859-1) character
        Ref<BaseType> d_wcharType; // 16 bit Unicode plane 0 character, the Basic Multilingual Plane (BMP)
        Ref<BaseType> d_byteType;
        Ref<BaseType> d_intType;
        Ref<BaseType> d_shortType;
        Ref<BaseType> d_longType;
        Ref<BaseType> d_realType;
        Ref<BaseType> d_longrealType;
        Ref<BaseType> d_setType;
        Ref<BaseType> d_stringType;  // latin-1 string literal
        Ref<BaseType> d_wstringType; // BMP string literal
        Ref<BaseType> d_byteArrayType;
        Ref<BaseType> d_nilType;
        Ref<BaseType> d_anyType;
        Ref<BaseType> d_voidType;
        Ref<Record> d_anyRec;
        Ref<ProcType> d_cmdType;
        Ref<Module> d_systemModule;
        QList<Module*> d_depOrder; // most (0) to least (n-1) dependent
        typedef QList<Ref<Module> > ModList;
        typedef QHash<Module*,ModList> ModInsts;
        ModInsts d_insts; // generic module -> instances

        typedef QHash<VirtualPath,Ref<Module> > Modules;
        typedef QHash<VirtualPath,QList<Module*> > Packages;
        Modules d_modules, d_others;
        Packages d_packages;
        XRef d_xref;
        quint32 d_sloc;
        QByteArrayList d_options;

        Ob::Errors* d_errs;
        Ob::FileCache* d_fc;
        bool d_fillXref;
    };
}

#endif // OBXMODEL_H
