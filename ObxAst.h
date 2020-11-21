#ifndef OBXAST_H
#define OBXAST_H

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

#include <Oberon/ObRowCol.h>
#include <bitset>
#include <QSharedData>
#include <QExplicitlySharedDataPointer>
#include <QVariant>
#include <QSet>

namespace Obx
{
// adopted from Ob::Ast

    enum { SET_BIT_LEN = 32 };
    typedef std::bitset<SET_BIT_LEN> Set;
    struct AstVisitor;

    template <class T>
    struct Ref : public QExplicitlySharedDataPointer<T>
    {
        Ref(T* t = 0):QExplicitlySharedDataPointer<T>(t) {}
        bool isNull() const { return QExplicitlySharedDataPointer<T>::constData() == 0; }
    };

    template <class T>
    struct NoRef
    {
        T* d_ptr;
        NoRef(T* t = 0):d_ptr(t){}
        bool isNull() const { return d_ptr == 0; }
        T* data() const { return d_ptr; }
        T* operator->() const { return d_ptr; }
    };

    struct Thing : public QSharedData
    {
        enum Tag { T_Thing, T_Module, T_Import, T_Pointer, T_Record, T_BaseType, T_Array, T_ProcType, T_NamedType,
                   T_ArgExpr, T_Literal, T_SetExpr, T_IdentLeaf, T_UnExpr, T_IdentSel, T_BinExpr, T_Field,
                   T_Const, T_BuiltIn, T_Parameter, T_Return, T_Procedure, T_Variable, T_LocalVar, T_TypeRef,
                   T_QualiType, T_Call, T_Assign, T_IfLoop, T_ForLoop, T_CaseStmt, T_Scope,
                   T_Enumeration, T_GenericName, T_Exit,
                   T_MAX };
        static const char* s_tagName[];
        // QVariantMap user; // Not used so far; For any use; only eats 4 bytes if not used (QVariant eats 12 instead)
    #ifdef _DEBUG
        static QSet<Thing*> insts;
        Thing();
        virtual ~Thing();
    #else
        Thing() {}
        virtual ~Thing() {}
    #endif
        virtual bool isScope() const { return false; }
        virtual bool isNamed() const { return false; }
        virtual int getTag() const { return T_Thing; }
        virtual void accept(AstVisitor* v){}
    };

    template <typename T>
    inline T cast( Thing* in )
    {
    #ifdef _DEBUG
        T out = dynamic_cast<T>( in );
        Q_ASSERT( in == 0 || out != 0 );
    #else
        T out = static_cast<T>( in );
    #endif
        return out;
    }

    struct Type;
    struct BaseType;
    struct Pointer;
    struct Array;
    struct Record;
    struct ProcType;
    struct QualiType;
    struct Named;
    struct Field;
    struct Variable;
    struct LocalVar;
    struct Parameter;
    struct NamedType;
    struct Const;
    struct Import;
    struct BuiltIn;
    struct Scope;
    struct Procedure;
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
    struct ArgExpr;
    struct BinExpr;
    struct Enumeration;
    struct GenericName;
    struct Exit;

    typedef QList< Ref<Statement> > StatSeq;

    struct AstVisitor
    {
        virtual void visit( BaseType* ) {}
        virtual void visit( Pointer* ) {}
        virtual void visit( Array* ) {}
        virtual void visit( Record* ) {}
        virtual void visit( ProcType* ) {}
        virtual void visit( QualiType* ) {}
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
        virtual void visit( ArgExpr* ) {}
        virtual void visit( BinExpr* ) {}
        virtual void visit( Enumeration* ) {}
        virtual void visit( GenericName* ) {}
        virtual void visit( Exit* ) {}
    };

    struct Type : public Thing
    {
        Named* d_ident; // a reference to the ident or null if type is anonymous

        Type():d_ident(0) {}
        typedef QList< Ref<Type> > List;
        virtual bool isStructured() const { return false; }
        virtual bool isSelfRef() const { return false; }
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
        Array():d_len(0) {}
        int getTag() const { return T_Array; }
        void accept(AstVisitor* v) { v->visit(this); }
        bool isStructured() const { return true; }
    };

    struct Record : public Type
    {
        Ref<QualiType> d_base; // base type - a quali to a Record or Pointer or null
        Pointer* d_binding; // points back to pointer type in case of anonymous record
        QList< Ref<Field> > d_fields;

        Record():d_binding(0) {}
        int getTag() const { return T_Record; }
        void accept(AstVisitor* v) { v->visit(this); }
        bool isStructured() const { return true; }
        Field* find( const QByteArray& name ) const;
    };

    struct ProcType : public Type
    {
        typedef QList< Ref<Parameter> > Formals;
        typedef QList<bool> Vars;

        Ref<QualiType> d_return;
        Formals d_formals;

        ProcType(const Type::List& f, Obx::QualiType* r = 0);
        ProcType(){}
        int getTag() const { return T_ProcType; }
        Parameter* find( const QByteArray& ) const;
        void accept(AstVisitor* v) { v->visit(this); }
        bool isBuiltIn() const;
    };

    typedef QList< Ref<Thing> > MetaActuals;

    struct QualiType : public Type
    {
        typedef QPair<Named*,Named*> ModItem; // Module or 0 -> Item

        Ref<Expression> d_quali;
        MetaActuals d_metaActuals;
        bool d_selfRef; // can only be resolved after declaration section

        QualiType():d_selfRef(false){}
        ModItem getQuali() const;
        bool isSelfRef() const { return d_selfRef; }
        int getTag() const { return T_QualiType; }
        void accept(AstVisitor* v) { v->visit(this); }
        Type* derefed();
    };

    struct Named : public Thing
    {
        QByteArray d_name;
        Ob::RowCol d_loc;
        Ref<Type> d_type;
        Scope* d_scope; // owning scope

        // Bytecode generator helpers
        uint d_liveFrom : 24; // 0..undefined
        uint d_slot : 8;
        uint d_liveTo : 24; // 0..undefined
        uint d_slotValid : 1;
        uint d_usedFromSubs : 1;
        uint d_usedFromLive : 1; // indirectly used named types
        uint d_initialized: 1;
        // end helpers

        uint d_public : 1;
        uint d_ro : 1; // read only
        uint d_synthetic: 1;
        uint d_isDef : 1;
        uint d_hasErrors : 1;

        Named(const QByteArray& n = QByteArray(), Type* t = 0, Scope* s = 0):d_scope(s),d_type(t),d_name(n),
            d_public(false),d_synthetic(false),d_liveFrom(0),d_liveTo(0),
            d_slot(0),d_slotValid(0),d_usedFromSubs(0),d_initialized(0),d_usedFromLive(0),
            d_isDef(0),d_hasErrors(0), d_ro(0) {}
        bool isNamed() const { return true; }
        virtual bool isVarParam() const { return false; }
        Module* getModule();
    };

    struct GenericName : public Named
    {
        void accept(AstVisitor* v) { v->visit(this); }
        int getTag() const { return T_GenericName; }
    };
    typedef QList< Ref<GenericName> > MetaParams;

    struct Field : public Named // Record field
    {
        void accept(AstVisitor* v) { v->visit(this); }
        int getTag() const { return T_Field; }
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
        bool d_const;
        Parameter():d_var(false),d_const(false) {}
        int getTag() const { return T_Parameter; }
        void accept(AstVisitor* v) { v->visit(this); }
        bool isVarParam() const { return d_var; }
    };

    struct NamedType : public Named // TypeDeclaration
    {
        MetaParams d_metaParams;

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

    struct Enumeration : public Type
    {
        QList< Ref<Const> > d_items;
        int getTag() const { return T_Enumeration; }
        void accept(AstVisitor* v) { v->visit(this); }
    };

    struct Import : public Named
    {
        Ref<Module> d_mod;
        int getTag() const { return T_Import; }
        void accept(AstVisitor* v) { v->visit(this); }
    };

    struct BuiltIn : public Named
    {
        enum { ABS, ODD, LEN, LSL, ASR, ROR, FLOOR, FLT, ORD, CHR, INC, DEC, INCL, EXCL,
               NEW, ASSERT, PACK, UNPK,
               WriteInt, WriteReal, WriteChar, WriteLn, // to run oberonc test cases
               LED, // LED not global proc in Oberon report, but used as such in Project Oberon
               TRAP, TRAPIF,
               // SYSTEM
               ADR, BIT, GET, H, LDREG, PUT, REG, VAL, COPY
             };
        static const char* s_typeName[];
        quint8 d_func;
        BuiltIn(quint8 f, ProcType* = 0 );
        int getTag() const { return T_BuiltIn; }
        void accept(AstVisitor* v) { v->visit(this); }
    };

    struct Scope : public Named
    {
        typedef QHash<const char*, Ref<Named> > Names;
        Names d_names;
        QList<Named*> d_order, d_tempNamed;
        QList< Ref<IdentLeaf> > d_helper; // filled with all decls when fillXref
        StatSeq d_body;
        Ob::RowCol d_end;

        bool isScope() const { return true; }
        int getTag() const { return T_Scope; }

        Named* find( const QByteArray&, bool recursive = true ) const;
        bool add( Named* );
    };

    struct Procedure : public Scope
    {
        Ref<Parameter> d_receiver;
        MetaParams d_metaParams;
        QByteArray d_imp;
        void accept(AstVisitor* v) { v->visit(this); }
        int getTag() const { return T_Procedure; }
    };

    struct Module : public Scope
    {
        // bool d_isDef; // DEFINITION module
        // bool d_hasErrors;
        QString d_file;

        Module() {}
        int getTag() const { return T_Module; }
        void accept(AstVisitor* v) { v->visit(this); }
    };

    struct Statement : public Thing
    {
        Ob::RowCol d_loc;
    };

    struct Call : public Statement
    {
        Ref<Expression> d_what;
        void accept(AstVisitor* v) { v->visit(this); }
        ArgExpr* getCallExpr() const;
        int getTag() const { return T_Call; }
    };

    struct Return : public Statement
    {
        Ref<Expression> d_what;
        int getTag() const { return T_Return; }
        void accept(AstVisitor* v) { v->visit(this); }
    };

    struct Exit : public Statement
    {
        int getTag() const { return T_Exit; }
        void accept(AstVisitor* v) { v->visit(this); }
    };

    struct Assign : public Statement
    {
        Ref<Expression> d_lhs;
        Ref<Expression> d_rhs;
        void accept(AstVisitor* v) { v->visit(this); }
        int getTag() const { return T_Assign; }
    };

    typedef QList< Ref<Expression> > ExpList;

    struct IfLoop : public Statement
    {
        enum { IF, WHILE, REPEAT, WITH };
        quint8 d_op;
        ExpList d_if;
        QList<StatSeq> d_then;
        StatSeq d_else;
        void accept(AstVisitor* v) { v->visit(this); }
        int getTag() const { return T_IfLoop; }
    };

    struct ForLoop : public Statement
    {
        Ref<Expression> d_id, d_from, d_to, d_by;
        QVariant d_byVal;
        StatSeq d_do;
        void accept(AstVisitor* v) { v->visit(this); }
        int getTag() const { return T_ForLoop; }
    };

    struct CaseStmt : public Statement
    {
        Ref<Expression> d_exp;
        struct Case
        {
            ExpList d_labels;
            StatSeq d_block;
        };
        QList<Case> d_cases;
        StatSeq d_else;
        bool d_typeCase;
        CaseStmt():d_typeCase(false){}
        void accept(AstVisitor* v) { v->visit(this); }
        int getTag() const { return T_CaseStmt; }
    };

    struct Expression : public Thing
    {
        NoRef<Type> d_type;
        Ob::RowCol d_loc;
        virtual Named* getIdent() const { return 0; }
        virtual Module* getModule() const { return 0; }
    };

    struct Literal : public Expression
    {
        QVariant d_val;
        Literal( Type* t = 0, Ob::RowCol l = Ob::RowCol(), const QVariant& v = QVariant() ):d_val(v){d_type = t; d_loc = l; }
        int getTag() const { return T_Literal; }
        void accept(AstVisitor* v) { v->visit(this); }
    };

    struct SetExpr : public Expression
    {
        ExpList d_parts;
        int getTag() const { return T_SetExpr; }
        void accept(AstVisitor* v) { v->visit(this); }
    };

    struct IdentLeaf : public Expression
    {
        NoRef<Named> d_ident;
        QByteArray d_name; // name to be resolved with result written to d_ident
        Module* d_mod;
        IdentLeaf():d_mod(0) {}
        IdentLeaf( Named* id, const Ob::RowCol&, Module* mod, Type* t );
        Named* getIdent() const { return d_ident.data(); }
        Module* getModule() const { return d_mod; }
        int getTag() const { return T_IdentLeaf; }
        void accept(AstVisitor* v) { v->visit(this); }
    };

    struct UnExpr : public Expression
    {
        enum Op { Invalid, NEG, NOT, DEREF, CAST, // implemented in UnExpr
                  SEL, CALL, IDX // implemented in subclasses
                };
        static const char* s_opName[];
        quint8 d_op;
        Ref<Expression> d_sub;
        UnExpr(quint8 op = Invalid, Expression* e = 0 ):d_op(op),d_sub(e){}
        Module* getModule() const { return d_sub->getModule(); }
        int getTag() const { return T_UnExpr; }
        void accept(AstVisitor* v) { v->visit(this); }
    };

    struct IdentSel : public UnExpr
    {
        NoRef<Named> d_ident;
        QByteArray d_name; // name to be resolved with result written to d_ident
        Named* getIdent() const { return d_ident.data(); }
        IdentSel():UnExpr(SEL) {}
        int getTag() const { return T_IdentSel; }
        void accept(AstVisitor* v) { v->visit(this); }
    };

    struct ArgExpr : public UnExpr // Call or Index
    {
        ExpList d_args;
        int getTag() const { return T_ArgExpr; }
        void accept(AstVisitor* v) { v->visit(this); }
        ProcType* getProcType() const;
    };

    struct BinExpr : public Expression
    {
        enum Op { Invalid, Range,
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

}

#endif // OBXAST_H
