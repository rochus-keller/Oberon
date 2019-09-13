#ifndef OBCODEMODEL_H
#define OBCODEMODEL_H

/*
* Copyright 2019 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include <QObject>
#include <Oberon/ObSynTree.h>
#include <QMap>
#include <QStringList>
#include <QVector>
#include <QHash>

class QTextStream;

namespace Ob
{
    class Errors;
    class FileCache;

    class CodeModel : public QObject
    {
    public:
        class NamedThing;
        class Procedure;
        class Element;
        class Type;
        class Module;
        class Scope;

        typedef QPair<const SynTree*,const NamedThing*> IdentUse; // idUse -> decl
        typedef QList<IdentUse> IdentUseList;
        typedef QHash<QString,IdentUseList> IdentUseDir; // sourcePath -> ids
        typedef QMultiHash<const NamedThing*,const SynTree*> RevIndex;

        enum DesigOpCode { UnknownOp, IdentOp, PointerOp, TypeOp, ProcedureOp, ArrayOp };
        struct DesigOp
        {
            DesigOpCode d_op;
            SynTree* d_arg;
            const NamedThing* d_sym;
            DesigOp(DesigOpCode op = UnknownOp, SynTree* arg = 0, const NamedThing* t = 0):d_op(op),d_arg(arg),d_sym(t){}
        };
        typedef QList<DesigOp> DesigOpList;

        typedef QPair<QPair<const Module*,const SynTree*>,QPair<const NamedThing*,const SynTree*> > Quali;

        class NamedThing
        {
        public:
            NamedThing():d_def(0),d_id(0),d_public(false),d_scope(0),d_var(false) {}
            virtual ~NamedThing() {}


            template<typename T>
            const T* to() const { return dynamic_cast<const T*>(this); }

            SynTree* d_def; // not owned; zeigt auf Context oder 0 für Stubs
            SynTree* d_id; // not owned; zeit auf Tok_ident oder 0
            Scope* d_scope; // not owned; the scope which owns me (only if named)
            QByteArray d_name;
            bool d_public;
            bool d_var; // var param

            bool isStub() const { return d_def == 0; }
            virtual SynTree* getExpr() const { return 0; }
            virtual const Type* getType() const { return 0; }
            virtual QByteArray typeName() const { return ""; }
        };

        class Element : public NamedThing
        {
        public:
            enum Kind { Unknown,
                ABS, ODD, LEN, LSL, ASR, ROR, FLOOR, FLT, ORD, CHR, INC, DEC, INCL, EXCL,
                        NEW, ASSERT, PACK, UNPK, LED, // LED not global proc in Oberon report, but used as such in Project Oberon
                        Constant, Variable, StubProc
                      };
            static const char* s_kindName[];

            Element(Kind k = Unknown);
            ~Element();

            Kind d_kind;
            SynTree* d_st; // not owned; Const: expr;
            const Type* d_type; // not owned; Variable: type
            QList<Element*> d_vals; // owned, StubProc

            bool isPredefProc() const { return d_kind >= ABS && d_kind <= LED; }
            QByteArray typeName() const;
            virtual const Type* getType() const { return d_type; }
        };

        class Scope : public NamedThing
        {
        public:
            typedef QMap<QByteArray,NamedThing*> Names;
            Scope():d_outer(0){}
            const NamedThing* findByName( const QByteArray& ) const;
            const Module* getModule() const;
            void addToScope( NamedThing* );

            Scope* d_outer;
            Names d_names; // not owned
        };

        class GlobalScope : public Scope
        {
        public:
            GlobalScope():d_boolType(0),d_intType(0),d_realType(0),d_stringType(0),
                d_nilType(0),d_setType(0){}
            ~GlobalScope();

            QList<Element*> d_procs; // owned
            QList<Module*> d_mods; // owned
            QList<Type*> d_types; // owned
            Type* d_boolType; // not owned
            Type* d_intType; // not owned
            Type* d_realType; // not owned
            Type* d_stringType; // not owned
            Type* d_nilType; // not owned
            Type* d_setType; // not owned
            Type* d_charType; // not owned

            virtual QByteArray typeName() const { return "GlobalScope"; }
        };

        class Unit : public Scope
        {
        public:
            Unit() {}
            ~Unit();

            QList<Type*> getNamedTypes() const;
            QList<Element*> getConsts() const;
            QList<Element*> getVars() const;
            QList<Element*> getStubProcs() const;
            QList<Element*> getUnknowns() const;

            QList<Procedure*> d_procs; // owned
            QList<Element*> d_elems; // owned;
            QList<Type*> d_types; // owned
            QList<SynTree*> d_body; // statements, not owned
        };

        class Module : public Unit
        {
        public:
            // Module mit d_st==0 ist ein Stub
            Module() {}
            ~Module() { if( d_def ) delete d_def; } // Module owns full SynTree

            QList<Module*> d_using, d_usedBy; // not owned
            QByteArray typeName() const { return "Module"; }
        };

        class Procedure : public Unit
        {
        public:
            Procedure():d_type(0){}
            ~Procedure();

            QList<Element*> d_vals; // owned; params
            Type* d_type; // not owned; return type
            const Type* getType() const { return d_type; }
            QByteArray typeName() const { return "Procedure"; }
        };

        class Type : public NamedThing
        {
        public:
            enum Kind { Unknown,
                        BOOLEAN, CHAR, INTEGER, REAL, BYTE, SET, // Basic Types
                        STRING, NIL, // Basic Type Helpers
                        TypeRef, Array, Record, Pointer, ProcRef
                      };
            static const char* s_kindName[];
            typedef QMap<QByteArray,Element*> Vals;

            Type(Kind k = Unknown);
            ~Type();

            bool isBasicType() const { return d_kind >= BOOLEAN && d_kind <= NIL; }
            const Type* deref() const;
            QByteArray typeName() const;
            const Element* findByName( const QByteArray& ) const;
            virtual const Type* getType() const { return this; }

            Kind d_kind;
            const Type* d_type; // not owned; Record: base; ProcRef: return; TypeRef, Array, Pointer: type
            SynTree* d_st; // not owned; Array: dim expression; Ref: quali
            Vals d_vals; // owned; Record: fields; ProcRef: Params
        };

        class TypeAlias : public NamedThing // wird nur temporär auf dem Stack verwendet
        {
        public:
            TypeAlias():d_alias(0),d_newType(0){}
            NamedThing* d_alias; // not owned
            const Type* d_newType; // not owned
            const Type* getType() const { return d_newType; }
        };


        explicit CodeModel(QObject *parent = 0);
        void clear();
        void setSynthesize(bool on) { d_synthesize = on; }
        void setTrackIds(bool on) { d_trackIds = on; }
        Errors* getErrs() const { return d_errs; }
        FileCache* getFc() const { return d_fc; }
        void setLowerCaseKeywords( bool b ) { d_lowerCaseKeywords = b; }
        void setLowerCaseBuiltins( bool b ) { d_lowerCaseBuiltins = b; }
        void setUnderscoreIdents( bool b ) { d_underscoreIdents = b; }

        bool parseFiles( const QStringList& );
        const GlobalScope& getGlobalScope() const { return d_scope; }
        const Type* typeOfExpression( const Unit*, SynTree* ) const;
        static SynTree* flatten( SynTree*, int stopAt = 0 );
        DesigOpList derefDesignator( const Unit*,const SynTree*);
        Quali derefQualident(const Unit*,const SynTree*);

        static void dump( QTextStream&, const SynTree*, int level = 0 );

        IdentUse findSymbolBySourcePos(const QString& file, quint32 line, quint16 col ) const;
        QList<const SynTree*> findReferencingSymbols( const NamedThing*, const QString& file = QString() );
        QList<Token> getComments( QString file ) const;

        static SynTree* findFirstChild(const SynTree*, int type , int startWith = 0);
        static QByteArrayList getBuitinIdents();

    protected:
        void parseFile( const QString& );
        void checkModuleDependencies();
        QList<Module*> findProcessingOrder();
        void processDeclSeq(Unit*,SynTree*);
        void processConstDeclaration(Unit*,SynTree*);
        void processTypeDeclaration(Unit*,SynTree*);
        void processVariableDeclaration(Unit*,SynTree*);
        void processProcedureDeclaration(Unit*,SynTree*);
        bool checkNameNotInScope(Scope* scope, SynTree* id);
        bool checkNameNotInRecord(Type* scope, SynTree* id);
        void checkTypeRules(Unit*);
        void checkNames(Unit*);
        void checkNames(Unit*, SynTree*, const Type* expected = 0);
        void checkAssig( Unit*, const DesigOpList&, SynTree* expr );
        void checkCaseStatement(Unit* ds, SynTree* st);
        Type* parseType( Unit* ds, SynTree* t );
        Type* parseTypeRef( Unit* ds, SynTree* t );
        Type* parsePointerType( Unit* ds, SynTree* t );
        Type* parseRecordType( Unit* ds, SynTree* t );
        Type* parseArrayType( Unit* ds, SynTree* t );
        Type* parseProcType( Unit* ds, SynTree* t );
        Type* parseFormalParams(Unit* ds, SynTree* t, QList<Element*>&);
        void resolveTypeRefs( Unit* ds );
        Quali derefQualident(Unit* ds, SynTree* t , bool report, bool synthesize = false );
        DesigOpList derefDesignator( Unit*,SynTree*,bool report, bool synthesize, const CodeModel::Type* expected = 0);
        enum DesigOpErr { NoError, NotFound, InvalidOperation, MissingType };
        const NamedThing* applyDesigOp(Unit* ds, const NamedThing* input, const DesigOp& dop, DesigOpErr* errOut ,
                                       bool synthesize = true, const Type* expected = 0);
        const Type* typeOfExpression( Unit*, Element::Kind, SynTree* args ) const;
        void index( const SynTree* idUse, const NamedThing* decl );

        static QPair<SynTree*,bool> getIdentFromIdentDef(SynTree*);
        static DesigOp getSelectorOp( SynTree* );
    private:
        GlobalScope d_scope;
        QHash<QString,QList<Token> > d_comments;
        Errors* d_errs;
        FileCache* d_fc;
        IdentUseDir d_dir;
        RevIndex d_revDir;
        bool d_synthesize; // create stubs if needed
        bool d_trackIds;
        bool d_lowerCaseKeywords; // Allow for both uppercase and lowercase keywords
        bool d_lowerCaseBuiltins; // Basic types and global procs accessible by lower case name too
        bool d_underscoreIdents; // Allow for idents with underscores as in C
    };
}

#endif // OBCODEMODEL_H
