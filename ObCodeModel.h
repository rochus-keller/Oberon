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

    class CodeModel : public QObject
    {
    public:
        class NamedThing;
        class Procedure;
        class Constant;
        class Type;
        class Variable;
        class Module;
        class FormalParam;
        class PredefProc;
        class StubVal;

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
            NamedThing():d_def(0),d_id(0),d_public(false) {}
            virtual ~NamedThing() {}

            virtual SynTree* getExpr() const { return 0; }

            SynTree* d_def; // not owned
            SynTree* d_id; // not owned
            QByteArray d_name;
            bool d_public;
            virtual QByteArray typeName() const { return ""; }
        };

        class Constant : public NamedThing
        {
        public:
            Constant():d_expr(0){}
            SynTree* getExpr() const { return d_expr; }

            SynTree* d_expr; // not owned
            QByteArray typeName() const { return "Constant"; }
        };

        class Variable : public NamedThing
        {
        public:
            Variable( Type* t = 0 ):d_type(t){}

            Type* d_type; // not owned
            QByteArray typeName() const { return "Variable"; }
        };

        class FormalParam : public Variable
        {
        public:
            FormalParam( Type* t = 0, bool var = false ):Variable(t),d_var(var){}

            bool d_var;
            QByteArray typeName() const { return "Parameter"; }
        };

        class Scope : public NamedThing
        {
        public:
            typedef QMap<QByteArray,NamedThing*> Names;
            Scope():d_outer(0){}
            const NamedThing* findByName( const QByteArray& ) const;

            Scope* d_outer;
            Names d_names; // not owned
        };

        class GlobalScope : public Scope
        {
        public:
            GlobalScope():d_boolType(0),d_intType(0),d_realType(0),d_stringType(0),
                d_nilType(0),d_setType(0){}
            ~GlobalScope();

            QList<PredefProc*> d_procs; // owned
            QList<Module*> d_mods; // owned
            QList<Type*> d_types; // owned
            Type* d_boolType; // not owned
            Type* d_intType; // not owned
            Type* d_realType; // not owned
            Type* d_stringType; // not owned
            Type* d_nilType; // not owned
            Type* d_setType; // not owned
            Type* d_charType; // not owned
        };

        class DeclarationSequence : public Scope
        {
        public:
            DeclarationSequence() {}
            ~DeclarationSequence();

            QList<Procedure*> d_procs; // owned
            QList<Constant*> d_consts; // owned;
            QList<Type*> d_types; // owned
            QList<Variable*> d_vars; // owned
            QList<StubVal*> d_stubs; // owned
            QList<SynTree*> d_body; // statements, not owned
        };

        class Module : public DeclarationSequence
        {
        public:
            // Module mit d_st==0 ist ein Stub
            Module() {}
            ~Module() { if( d_def ) delete d_def; } // Module owns full SynTree

            QList<Module*> d_using, d_usedBy; // not owned
            QByteArray typeName() const { return "Module"; }
        };

        class Procedure : public DeclarationSequence
        {
        public:
            Procedure():d_returns(0){}
            ~Procedure();

            QList<FormalParam*> d_params; // owned
            Type* d_returns; // not owned
            QByteArray typeName() const { return "Procedure"; }
        };

        class PredefProc : public NamedThing
        {
        public:
            enum Meta { ABS, ODD, LEN, LSL, ASR, ROR, FLOOR, FLT, ORD, CHR, INC, DEC, INCL, EXCL,
                        NEW, ASSERT, PACK, UNPK, LED };
            // LED not global proc in Oberon report, but used as such in Project Oberon
            PredefProc(Meta);

            Meta d_proc;
            QByteArray typeName() const { return "Predefined"; }
        };

        class Type : public NamedThing  // abstract
        {
        public:
            virtual const Type* deref() const { return this; }
        };

        class BasicType : public Type
        {
        public:
            enum Meta { BOOLEAN, CHAR, INTEGER, REAL, BYTE, SET,
                        STRING, NIL // Helper
                      };
            BasicType(Meta);

            Meta d_type;
            QByteArray typeName() const { return "Basic Type"; }
        };

        class TypeRef : public Type
        {
        public:
            TypeRef():d_type(0),d_typeSt(0){}
            const Type* deref() const { return ( d_type ? d_type->deref() : 0 ); }

            const Type* d_type; // not owned
            SynTree* d_typeSt; // hier nur qualident, d_st kann auf anderes zeigen
            QByteArray typeName() const { return "Typename"; }
        };

        class ArrayType : public Type
        {
        public:
            ArrayType():d_type(0),d_dim(0){}
            SynTree* getExpr() const { return d_dim; }

            SynTree* d_dim; // expression
            Type* d_type; // not owned
            QByteArray typeName() const { return "Array"; }
        };

        class RecordType : public Type
        {
        public:
            typedef QMap<QByteArray,Variable*> Fields;
            RecordType():d_base(0){}
            ~RecordType();
            const Variable* findByName(const QByteArray& n) const;

            Type* d_base; // not owned
            Fields d_fields; // owned
            QByteArray typeName() const { return "Record"; }
        };

        class PointerType : public Type
        {
        public:
            PointerType(Type* t = 0):d_type(t){}

            Type* d_type; // not owned
            QByteArray typeName() const { return "Pointer"; }
        };

        class ProcType : public Type
        {
        public:
            ProcType():d_returns(0){}
            ~ProcType();

            QList<FormalParam*> d_params; // owned
            Type* d_returns; // not owned
            QByteArray typeName() const { return "ProcRef"; }
        };

        class StubType : public Type
        {
        public:
            enum Kind { Unknown, Record, Array, Pointer, Proc };
            static const char* s_kindName[];
            StubType():d_type(0),d_kind(Unknown),d_mod(0){}
            ~StubType();
            const StubVal* findByName(const QByteArray& n) const { return d_fields.value(n); }

            Kind d_kind;
            Module* d_mod; // not owned
            Type* d_type; // not owned
            QMap<QByteArray,StubVal*> d_fields; // owned
            QByteArray typeName() const { return "Stub Type"; }
        };

        class StubVal : public NamedThing
        {
        public:
            enum Kind { Unknown, Constant, Variable, Procedure, Param };
            static const char* s_kindName[];
            StubVal():d_kind(Unknown),d_mod(0),d_type(0),d_expr(0){}
            ~StubVal();

            Kind d_kind;
            Module* d_mod; // not owned
            const Type* d_type; // not owned
            SynTree* d_expr; // not owned
            QMap<QByteArray,StubVal*> d_params; // owned
            QByteArray typeName() const { return "Stub Value"; }
        };

        class TypeAlias : public NamedThing
        {
        public:
            TypeAlias():d_alias(0),d_newType(0){}
            NamedThing* d_alias; // not owned
            const Type* d_newType; // not owned
        };


        explicit CodeModel(QObject *parent = 0);
        void clear();
        void setSynthesize(bool on) { d_synthesize = on; }
        void setTrackIds(bool on) { d_trackIds = on; }
        Errors* getErrs() const { return d_errs; }

        bool parseFiles( const QStringList& );
        const GlobalScope& getGlobalScope() const { return d_scope; }
        const Type* typeOfExpression( DeclarationSequence*, SynTree* ) const;
        static SynTree* flatten( SynTree*, int stopAt = 0 );
        DesigOpList derefDesignator( DeclarationSequence*,const SynTree*);

        static void dump( QTextStream&, const SynTree*, int level = 0 );

        IdentUse findSymbolBySourcePos(const QString& file, quint32 line, quint16 col ) const;
        QList<const SynTree*> findReferencingSymbols( const NamedThing*, const QString& file = QString() );

    protected:
        void parseFile( const QString& );
        void checkModuleDependencies();
        QList<Module*> findProcessingOrder();
        void processDeclSeq(DeclarationSequence*,SynTree*);
        void processConstDeclaration(DeclarationSequence*,SynTree*);
        void processTypeDeclaration(DeclarationSequence*,SynTree*);
        void processVariableDeclaration(DeclarationSequence*,SynTree*);
        void processProcedureDeclaration(DeclarationSequence*,SynTree*);
        bool checkNameNotInScope(Scope* scope, SynTree* id);
        bool checkNameNotInRecord(RecordType* scope, SynTree* id);
        void checkNames(DeclarationSequence*);
        void checkTypeRules(DeclarationSequence*);
        void checkNames(DeclarationSequence*,SynTree*);
        void checkAssig( DeclarationSequence*, const DesigOpList&, SynTree* expr );
        void checkCaseStatement(DeclarationSequence* ds, SynTree* st);
        Type* parseType( DeclarationSequence* ds, SynTree* t );
        Type* parseTypeRef( DeclarationSequence* ds, SynTree* t );
        Type* parsePointerType( DeclarationSequence* ds, SynTree* t );
        Type* parseRecordType( DeclarationSequence* ds, SynTree* t );
        Type* parseArrayType( DeclarationSequence* ds, SynTree* t );
        Type* parseProcType( DeclarationSequence* ds, SynTree* t );
        Type* parseFormalParams(DeclarationSequence* ds, SynTree* t, QList<FormalParam*>&);
        void resolveTypeRefs( DeclarationSequence* ds );
        Quali derefQualident(DeclarationSequence* ds, SynTree* t , bool report, bool synthesize = false );
        DesigOpList derefDesignator( DeclarationSequence*,SynTree*,bool report, bool synthesize);
        enum DesigOpErr { NoError, NotFound, InvalidOperation, MissingType };
        const NamedThing* applyDesigOp(DeclarationSequence* ds, const NamedThing* input,
                                        const DesigOp& dop, DesigOpErr* errOut , bool synthesize = true);
        const Type* typeOfExpression( DeclarationSequence*, PredefProc::Meta, SynTree* args ) const;
        void index( const SynTree* idUse, const NamedThing* decl );

        static QPair<SynTree*,bool> getIdentFromIdentDef(SynTree*);
        static SynTree* findFirstChild( SynTree*, int type );
        static DesigOp getSelectorOp( SynTree* );
    private:
        GlobalScope d_scope;
        Errors* d_errs;
        IdentUseDir d_dir;
        RevIndex d_revDir;
        bool d_synthesize; // create stubs if needed
        bool d_trackIds;
    };
}

#endif // OBCODEMODEL_H
