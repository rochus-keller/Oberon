#ifndef OBAST_H
#define OBAST_H

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
#include <QSet>
#include <QSharedData>
#include <QVariant>
#include <memory>
#include <bitset>

class QIODevice;

namespace Ob
{
    class Errors;
    class FileCache;
    class SynTree;

    namespace Ast
    {
        enum { SET_BIT_LEN = 32 };
        typedef std::bitset<SET_BIT_LEN> Set;
        struct AstVisitor;

        // adopted from Gough's gpcp

        template <class T>
        struct Ref : public QExplicitlySharedDataPointer<T>
        {
            Ref(T* t = 0):QExplicitlySharedDataPointer<T>(t) {}
            bool isNull() const { return QExplicitlySharedDataPointer<T>::constData() == 0; }
        };

        struct Loc
        {
            enum { ROW_BIT_LEN = 20 };
            uint d_row : ROW_BIT_LEN;
            uint d_col : (32-ROW_BIT_LEN);
            Loc():d_row(0),d_col(0) {}
            Loc(SynTree*);
            bool isValid() const { return d_row > 0 && d_col > 0; } // valid lines and cols start with 1; 0 is invalid
            quint32 packed() const { return ( d_col << ROW_BIT_LEN ) | d_row; }
            static bool isPacked( quint32 rowCol ) { return rowCol >= ( 1 << ROW_BIT_LEN ); }
            static quint32 packedRow(quint32 rowCol ) { return rowCol & ( 1 << ROW_BIT_LEN ) - 1; }
            static quint32 packedCol(quint32 rowCol ) { return ( rowCol >> ROW_BIT_LEN ); }
        };

        struct Thing : public QSharedData
        {
            enum Tag { T_Thing, T_Module, T_Import, T_Pointer, T_Record, T_BaseType, T_Array, T_ProcType, T_NamedType,
                        T_CallExpr, T_SelfRef, T_Literal, T_SetExpr, T_IdentLeaf, T_UnExpr, T_IdentSel, T_BinExpr,
                        T_Const, T_BuiltIn, T_Parameter, T_Return, T_Procedure, T_Variable, T_LocalVar, T_TypeRef,
                     T_MAX };
            // QVariantMap user; // Not used so far; For any use; only eats 4 bytes if not used (QVariant eats 12 instead)
            Thing() {}
            virtual ~Thing() {}
            virtual bool isScope() const { return false; }
            virtual bool isNamed() const { return false; }
            virtual int getTag() const { return T_Thing; }
            virtual void accept(AstVisitor* v){}
        };

        struct Type;
            struct BaseType;
            struct Pointer;
            struct Array;
            struct Record;
            struct ProcType;
            struct SelfRef;
            struct TypeRef;
        struct Named;
            struct Field;
            struct Variable;
            struct LocalVar;
            struct Parameter;
            struct NamedType;
            struct Const;
            struct Import;
        struct Scope;
            struct Procedure;
            struct BuiltIn;
            struct Module;
        struct Statement;
            struct Call;
            struct Return;
            struct Assign;
            struct IfLoop;
            struct ForLoop;
            struct CaseStmt;
        struct Expression;
            struct Literal;
            struct SetExpr;
            struct IdentLeaf;
            struct UnExpr;
            struct IdentSel;
            struct CallExpr;
            struct BinExpr;

        typedef QList< Ref<Statement> > StatSeq;

        struct AstVisitor
        {
            virtual void visit( BaseType* ) {}
            virtual void visit( Pointer* ) {}
            virtual void visit( Array* ) {}
            virtual void visit( Record* ) {}
            virtual void visit( ProcType* ) {}
            virtual void visit( SelfRef* ) {}
            virtual void visit( TypeRef* ) {}
            virtual void visit( Field* ) {}
            virtual void visit( Variable* ) {}
            virtual void visit( LocalVar* ) {}
            virtual void visit( Parameter* ) {}
            virtual void visit( NamedType* ) {}
            virtual void visit( Const* ) {}
            virtual void visit( Import* ) {}
            virtual void visit( Procedure* ) {}
            virtual void visit( BuiltIn* ) {}
            virtual void visit( Module* ) {}
            virtual void visit( Call* ) {}
            virtual void visit( Return* ) {}
            virtual void visit( Assign* ) {}
            virtual void visit( IfLoop* ) {}
            virtual void visit( ForLoop* ) {}
            virtual void visit( CaseStmt* ) {}
            virtual void visit( Literal* ) {}
            virtual void visit( SetExpr* ) {}
            virtual void visit( IdentLeaf* ) {}
            virtual void visit( UnExpr* ) {}
            virtual void visit( IdentSel* ) {}
            virtual void visit( CallExpr* ) {}
            virtual void visit( BinExpr* ) {}
        };

        struct Type : public Thing
        {
            Named* d_ident; // a reference to the ident or null if type is anonymous

            Type():d_ident(0) {}
            typedef QList< Ref<Type> > List;
            virtual bool isStructured() const { return false; }
            virtual Type* derefed() { return this; }
        };

            struct BaseType : public Type
            {
                enum { ANY, ANYNUM, NIL, STRING,
                       BOOLEAN, CHAR, INTEGER, REAL, BYTE, SET };
                static const char* s_typeName[];
                quint8 d_type;

                BaseType(quint8 t = NIL ):d_type(t) {}
                int getTag() const { return T_BaseType; }
                void accept(AstVisitor* v) { v->visit(this); }
            };

            struct Pointer : public Type
            {
                Ref<Type> d_to; // only to Record
                int getTag() const { return T_Pointer; }
                void accept(AstVisitor* v) { v->visit(this); }
            };

            struct Array : public Type
            {
                quint32 d_len;  // zero for open arrays
                Ref<Expression> d_lenExpr;
                Ref<Type> d_type;
                int getTag() const { return T_Array; }
                void accept(AstVisitor* v) { v->visit(this); }
                bool isStructured() const { return true; }
            };

            struct Record : public Type
            {
                Type* d_base; // base type (a Record or TypeRef to Record) or null
                Pointer* d_binding; // points back to pointer type in case of anonymous record
                QList< Ref<Field> > d_fields;

                Record():d_binding(0),d_base(0) {}
                Field* find(const QByteArray& name , bool recursive ) const;
                int getTag() const { return T_Record; }
                void accept(AstVisitor* v) { v->visit(this); }
                bool isStructured() const { return true; }
            };

            struct ProcType : public Type
            {
                typedef QList< Ref<Parameter> > Formals;
                typedef QList<bool> Vars;

                Ref<Type> d_return;
                Formals d_formals;

                ProcType(const Type::List& f, Type* r = 0);
                ProcType(const Type::List& f, const Vars& var, Type* r = 0);
                ProcType(){}
                int getTag() const { return T_ProcType; }
                Parameter* find( const QByteArray& ) const;
                void accept(AstVisitor* v) { v->visit(this); }
                bool isBuiltIn() const;
            };

            struct SelfRef : public Type // can only be resolved after declaration section
            {
                SelfRef(Named* n):d_self(n) {}
                Named* d_self;
                int getTag() const { return T_SelfRef; }
                void accept(AstVisitor* v) { v->visit(this); }
            };

            struct TypeRef : public Type // for named types pointing to types in other modules
            {
                Ref<Type> d_ref;
                int getTag() const { return T_TypeRef; }
                void accept(AstVisitor* v) { v->visit(this); }
                Type* derefed() { return d_ref->derefed(); }
            };

        struct Named : public Thing
        {
            QByteArray d_name;
            Loc d_loc;
            Ref<Type> d_type;
            Scope* d_scope; // owning scope

            // Bytecode generator helpers
            uint d_liveFrom : 24; // 0..undefined
            uint d_slot : 8;
            uint d_liveTo : 25; // 0..undefined
            uint d_slotValid : 1;
            uint d_usedFromSubs : 1;
            uint d_usedFromLive : 1; // indirectly used named types
            uint d_initialized: 1;
            // end helpers

            uint d_public : 1;
            uint d_synthetic: 1;

            Named(const QByteArray& n = QByteArray(), Type* t = 0, Scope* s = 0):d_scope(s),d_type(t),d_name(n),
                d_public(false),d_synthetic(false),d_liveFrom(0),d_liveTo(0),
                d_slot(0),d_slotValid(0),d_usedFromSubs(0),d_initialized(0),d_usedFromLive(0) {}
            bool isNamed() const { return true; }
            virtual bool isVarParam() const { return false; }
            Module* getModule();
        };

            struct Field : public Named // Record field
            {
                void accept(AstVisitor* v) { v->visit(this); }
            };

            struct Variable : public Named // Module variable
            {
                void accept(AstVisitor* v) { v->visit(this); }
                int getTag() const { return T_Variable; }
            };

            struct LocalVar : public Named // Procedure local variable
            {
                void accept(AstVisitor* v) { v->visit(this); }
                int getTag() const { return T_LocalVar; }
            };

            struct Parameter : public Named // Procedure parameter
            {
                bool d_var;
                Parameter():d_var(false) {}
                int getTag() const { return T_Parameter; }
                void accept(AstVisitor* v) { v->visit(this); }
                bool isVarParam() const { return d_var; }
            };

            struct NamedType : public Named
            {
                NamedType( const QByteArray& n, Type* t, Scope* s ):Named(n,t,s) {}
                NamedType() {}
                int getTag() const { return T_NamedType; }
                void accept(AstVisitor* v) { v->visit(this); }
            };

            struct Const : public Named
            {
                QVariant d_val;
                Ref<Expression> d_constExpr;
                int getTag() const { return T_Const; }
                void accept(AstVisitor* v) { v->visit(this); }
            };

            struct Import : public Named
            {
                Ref<Module> d_mod;
                int getTag() const { return T_Import; }
                void accept(AstVisitor* v) { v->visit(this); }
            };

            struct Scope : public Named
            {
                typedef QHash<const char*, Ref<Named> > Names;
                Names d_names;
                QList<Named*> d_order;
                StatSeq d_body;
                Loc d_end;

                bool isScope() const { return true; }

                Named* find( const QByteArray&, bool recursive = true ) const;
            };

                struct Procedure : public Scope
                {
                    void accept(AstVisitor* v) { v->visit(this); }
                    int getTag() const { return T_Procedure; }
                };

                struct BuiltIn : public Named
                {
                    enum { ABS, ODD, LEN, LSL, ASR, ROR, FLOOR, FLT, ORD, CHR, INC, DEC, INCL, EXCL,
                            NEW, ASSERT, PACK, UNPK,
                            WriteInt, WriteReal, WriteChar, WriteLn, // to run oberonc test cases
                            LED, // LED not global proc in Oberon report, but used as such in Project Oberon
                           // SYSTEM
                           ADR, BIT, GET, H, LDREG, PUT, REG, VAL, COPY
                         };
                    static const char* s_typeName[];
                    quint8 d_func;
                    BuiltIn(quint8 f, ProcType* = 0 );
                    int getTag() const { return T_BuiltIn; }
                    void accept(AstVisitor* v) { v->visit(this); }
                };

                struct Module : public Scope
                {
                    bool d_useExt; // use extensions of the language
                    bool d_isDef; // DEFINITION module
                    bool d_hasErrors;
                    QString d_file;

                    Module():d_useExt(false),d_isDef(false),d_hasErrors(false) {}
                    int getTag() const { return T_Module; }
                    void accept(AstVisitor* v) { v->visit(this); }
                };

        struct Statement : public Thing
        {
            Loc d_loc;
        };

            struct Call : public Statement
            {
                Ref<Expression> d_what;
                void accept(AstVisitor* v) { v->visit(this); }
                CallExpr* getCallExpr() const;
            };

            struct Return : public Statement
            {
                Ref<Expression> d_what;
                int getTag() const { return T_Return; }
                void accept(AstVisitor* v) { v->visit(this); }
            };

            struct Assign : public Statement
            {
                Ref<Expression> d_lhs;
                Ref<Expression> d_rhs;
                void accept(AstVisitor* v) { v->visit(this); }
            };

            struct IfLoop : public Statement
            {
                enum { IF, WHILE, REPEAT };
                quint8 d_op;
                QList< Ref<Expression> > d_if;
                QList<StatSeq> d_then;
                StatSeq d_else;
                void accept(AstVisitor* v) { v->visit(this); }
            };

            struct ForLoop : public Statement
            {
                Ref<Named> d_id;
                Ref<Expression> d_from, d_to, d_by;
                QVariant d_byVal;
                StatSeq d_do;
                void accept(AstVisitor* v) { v->visit(this); }
            };

            struct CaseStmt : public Statement
            {
                Ref<Expression> d_exp;
                struct Case
                {
                    QList< Ref<Expression> > d_labels;
                    StatSeq d_block;
                };
                QList<Case> d_cases;
                bool d_typeCase;
                CaseStmt():d_typeCase(false){}
                void accept(AstVisitor* v) { v->visit(this); }
            };

        struct Expression : public Thing
        {
            Ref<Type> d_type;
            Loc d_loc;
            virtual Named* getIdent() const { return 0; }
        };

            struct Literal : public Expression
            {
                QVariant d_val;
                Literal( Type* t = 0, Loc l = Loc(), const QVariant& v = QVariant() ):d_val(v){d_type = t; d_loc = l; }
                int getTag() const { return T_Literal; }
                void accept(AstVisitor* v) { v->visit(this); }
            };

            struct SetExpr : public Expression
            {
                QList< Ref<Expression> > d_parts;
                int getTag() const { return T_SetExpr; }
                void accept(AstVisitor* v) { v->visit(this); }
            };

            struct IdentLeaf : public Expression
            {
                Ref<Named> d_ident;
                Named* getIdent() const { return d_ident.data(); }
                int getTag() const { return T_IdentLeaf; }
                void accept(AstVisitor* v) { v->visit(this); }
            };

            struct UnExpr : public Expression
            {
                enum Op { Invalid, NEG, NOT, DEREF, CAST, // implemented in UnExpr
                          SEL, CALL // implemented in subclasses
                        };
                static const char* s_opName[];
                quint8 d_op;
                Ref<Expression> d_sub;
                UnExpr(quint8 op = Invalid, Expression* e = 0 ):d_op(op),d_sub(e){}
                int getTag() const { return T_UnExpr; }
                void accept(AstVisitor* v) { v->visit(this); }
            };

            struct IdentSel : public UnExpr
            {
                Ref<Named> d_ident;
                Named* getIdent() const { return d_ident.data(); }
                IdentSel():UnExpr(SEL) {}
                int getTag() const { return T_IdentSel; }
                void accept(AstVisitor* v) { v->visit(this); }
            };

            struct CallExpr : public UnExpr
            {
                typedef QList< Ref<Expression> > Actuals;
                Actuals d_actuals;
                CallExpr():UnExpr(CALL) {}
                int getTag() const { return T_CallExpr; }
                void accept(AstVisitor* v) { v->visit(this); }
                ProcType* getProcType() const;
            };

            struct BinExpr : public Expression
            {
                enum Op { Invalid, Index, Range,
                        // relations:
                        EQ, NEQ, LT, LEQ, GT, GEQ, IN, IS,  //'=' | '#' | '<' | '<=' | '>' | '>=' | IN | IS
                        // AddOperator
                        ADD, SUB, OR,  // '+' | '-' | OR
                        // MulOperator
                        MUL, FDIV, DIV, MOD, AND,  // '*' | '/' | DIV | MOD | '&'
                        };
                static const char* s_opName[];
                quint8 d_op;
                Ref<Expression> d_lhs, d_rhs;
                BinExpr():d_op(Invalid){}
                int getTag() const { return T_BinExpr; }
                void accept(AstVisitor* v) { v->visit(this); }
            };

        class Model : public QObject
        {
        public:
            Model( QObject* = 0 );
            ~Model();

            void clear();
            void setEnableExt( bool b ) { d_enableExt = b; }
            void setSenseExt( bool b ) { d_senseExt = b; }
            void setFillXref( bool b ) { d_fillXref = b; }
            void addPreload( const QByteArray& name, const QByteArray& source );

            bool parseFiles(const QStringList&);

            struct ParseResult
            {
                SynTree* d_modName; // sub of d_modRoot
                SynTree* d_modRoot; // set to zero to avoid deletion
                bool d_isExt; // has language extensions
                ParseResult():d_modRoot(0),d_modName(0),d_isExt(false){}
                ~ParseResult(); // deletes d_modRoot if not zero
            };
            typedef QHash<const char*,ParseResult> ParseResultList;
            bool parseFile( const QString&, ParseResultList& res ) const;
            void parseFile(QIODevice* , const QString& path, ParseResultList& res) const;

            typedef QList< Ref<Module> > Modules;
            Modules getModules() const;
            const QList<Module*>& getProcessingOrder() const { return d_depOrder; }

            struct BaseTypes
            {
                BaseType* d_boolType;
                BaseType* d_charType;
                BaseType* d_byteType;
                BaseType* d_intType;
                BaseType* d_realType;
                BaseType* d_setType;
                BaseType* d_stringType;
                BaseType* d_nilType;
                BaseType* d_anyType;
                BaseType* d_anyNum;
            };
            BaseTypes getBaseTypes() const;

            typedef QHash<Named*, QList<Expression*> > XRef; // name used by ident expression
            const XRef& getXref() const { return d_xref; }


            Errors* getErrs() const { return d_errs; }
            FileCache* getFc() const { return d_fc; }

            static bool isSubType( Type* sub, Type* super );
            static Record* toRecord( Ast::Type* t );
            static bool isXInModule( Module* mod, Scope* x );
        protected:
            struct Usage
            {
                QSet<const char*> d_uses, d_usedBy;
                SynTree* d_st;
                Usage(SynTree* st = 0):d_st(st){}
                ~Usage();
            };
            typedef QHash<const char*,Usage> Mods;
            struct Quali
            {
                Named* d_mod;
                Named* d_item;
                QByteArray d_modName,d_itemName;
                Quali():d_mod(0),d_item(0){}
            };

            bool addToScope( Scope*, Named* );

            bool module(Module*,SynTree*);
            bool importList(Module*,SynTree*);
            bool declarationSequence(Scope*,SynTree*,bool definition);
            bool procedureDeclaration(Scope*, SynTree*, bool headingOnly);
            bool procedureHeading(Procedure*,SynTree*);
            bool procedureBody(Procedure*,SynTree*);
            Named* typeDeclaration(Scope*,SynTree*);
            Ref<Type> type(Scope* s, Named* id, SynTree*, Pointer* binding = 0);
            Quali qualident(Scope*, SynTree*, bool report );
            Ref<Type> arrayType(Scope*,SynTree*);
            Ref<Type> pointerType(Scope*, SynTree*);
            Ast::Ref<ProcType> formalParameters(Scope*,SynTree*);
            Ref<Type> recordType(Scope*, SynTree*, Pointer* binding);
            bool variableDeclaration(Scope*,SynTree*);
            bool constDeclaration(Scope*,SynTree*);
            bool fpSection( Scope*, ProcType*, SynTree*);
            bool fieldList(Scope*, Record*, SynTree*);
            bool statementSequence( Scope*, StatSeq&, SynTree* );
            Ref<Statement> statement(Ast::Scope* s, SynTree* st);
            Ref<Statement> assignmentOrProcedureCall(Scope* s, SynTree* st);
            Ref<Statement> ifStatement(Scope* s, SynTree* st);
            Ref<Statement> whileStatement(Scope* s, SynTree* st);
            Ref<Statement> repeatStatement(Scope* s, SynTree* st);
            Ref<Statement> forStatement(Scope* s, SynTree* st);
            Ref<Statement> caseStatement(Scope* s, SynTree* st);
            Ref<Expression> expression( Scope*, SynTree* );
            Ref<Expression> simpleExpression( Scope*, SynTree* );
            Ref<Expression> term( Scope*, SynTree* );
            Ref<Expression> factor( Scope*, SynTree* );
            Ref<Expression> designator( Scope*, SynTree* );
            Ref<Expression> qualident( Scope*, SynTree* );
            Ref<Expression> set( Scope*, SynTree* );
            Ref<Expression> labelRange( Scope*, SynTree* );
            Ref<Expression> label( Scope*, SynTree* );

            bool error( SynTree*, const QString& ) const;
            bool error( Named*, const QString& ) const;
            void unbindFromGlobal();
            static const QList<SynTree*> getImports(const SynTree*);
            void createModules( Mods& mods, ParseResultList& );
            bool resolveImport( Mods& mods, const QByteArray& );
            QList<Module*> findProcessingOrder( Mods& mods);
            void fixTypes();
            void deferredBody(Procedure* p, SynTree* st);
            void deferredBody();
            Ref<Type> getTypeFromQuali(Scope*,SynTree*);
            bool checkSelfRefs(Named* n, Type*, bool top , bool startsWithPointer, bool inRecord);
            void publicWarning(Named* n);
            bool isInThisModule( Scope* ) const;
        private:
            Ref<Scope> d_global;
            Ref<Scope> d_globalLc; // lower & upper case version of global
            Ref<BaseType> d_boolType;
            Ref<BaseType> d_charType;
            Ref<BaseType> d_byteType;
            Ref<BaseType> d_intType;
            Ref<BaseType> d_realType;
            Ref<BaseType> d_setType;
            Ref<BaseType> d_stringType;
            Ref<BaseType> d_nilType;
            Ref<BaseType> d_anyType;
            Ref<BaseType> d_anyNum;
            QList<Module*> d_depOrder; // most to least dependent
            XRef d_xref;

            QByteArray d_system;
            Errors* d_errs;
            FileCache* d_fc;
            Module* d_curModule; // the module currently parsed
            NamedType* d_curTypeDecl;

            struct FixType { Pointer* d_ptr; Scope* d_scope; SynTree* d_st;
                           FixType(Pointer* p, Scope* s, SynTree* t):d_ptr(p),d_scope(s),d_st(t){}};
            QList<FixType> d_fixType;

            struct DeferBody
            {
                SynTree* d_st; // StatementSequence or ReturnStatement
                Procedure* d_p;
                DeferBody(SynTree* st, Procedure* p):d_st(st),d_p(p){}
            };
            QList<DeferBody> d_deferBody;

            bool d_enableExt; // Allow for both uppercase and lowercase keywords and builtins, idents with underscores
            bool d_senseExt; // automacitally determine if extensions enabled (starts with lower case module, etc.)
            bool d_fillXref;
        };
    }
}

Q_DECLARE_METATYPE( Ob::Ast::Set )

#endif // OBAST_H
