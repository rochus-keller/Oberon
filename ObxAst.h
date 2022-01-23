#ifndef OBXAST_H
#define OBXAST_H

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

#include <Oberon/ObRowCol.h>
#include <Oberon/ObxPackage.h>
#include <bitset>
#include <QSharedData>
#include <QExplicitlySharedDataPointer>
#include <QVariant>
#include <QSet>

class QIODevice;

namespace Obx
{
// adopted from Ob::Ast

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
                   T_Const, T_BuiltIn, T_Parameter, T_Return, T_Procedure, T_Variable, T_LocalVar,
                   T_QualiType, T_Call, T_Assign, T_IfLoop, T_ForLoop, T_CaseStmt, T_Scope,
                   T_Enumeration, T_GenericName, T_Exit,
                   T_MAX };
        static const char* s_tagName[];

        Ob::RowCol d_loc;

        enum { MAX_SLOT = 1048575 };
        uint d_slot : 20; // slots from 0..250, table index from 0..max
        uint d_slotValid : 1;
        uint d_slotAllocated : 1;
        uint d_visited : 1;
        uint d_unsafe : 1;  // used by Pointer, Record (CSTRUCT, CUNION) and Array, or fields of cstruct/cunion

    #ifdef _DEBUG
        static QSet<Thing*> insts;
        Thing();
        virtual ~Thing();
    #else
        Thing():d_slot(0),d_slotValid(false),d_slotAllocated(false),d_visited(false),d_unsafe(false) {}
        virtual ~Thing() {}
    #endif
        virtual bool isScope() const { return false; }
        virtual bool isNamed() const { return false; }
        virtual int getTag() const { return T_Thing; }
        virtual void accept(AstVisitor* v){}
        const char* getTagName() const { return s_tagName[getTag()]; }
        void setSlot( quint32 );
        void dump( QIODevice* = 0 );
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
    struct SysAttr;

    typedef QList< Ref<Statement> > StatSeq;

    struct AstVisitor
    {
        virtual void visit( BaseType* ) {}
        virtual void visit( Pointer* ) {}
        virtual void visit( Array* ) {}
        virtual void visit( Record* ) {}
        virtual void visit( ProcType* ) {}
        virtual void visit( QualiType* ) {}
        virtual void visit( Enumeration* ) {}
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
        virtual void visit( GenericName* ) {}
        virtual void visit( Exit* ) {}
    };

    typedef QList< Ref<Type> > MetaActuals; // actually only NamedTypes, but not resolvable before validation
                                            // for comparison and naming Type::derefed::findDecl is used
    typedef QList< Ref<GenericName> > MetaParams;

    typedef QHash<QByteArray, Ref<SysAttr> > SysAttrs;

    struct Type : public Thing
    {
        enum { NONE, ANY, CVOID, NIL, BYTEARRAY, STRING, WSTRING, BOOLEAN, CHAR, WCHAR, BYTE, SHORTINT,
               INTEGER, LONGINT, REAL, LONGREAL, SET, ENUMINT }; // BaseType

        Named* d_decl; // a reference to the corresponding declaration (type, var, etc.) or null if type is anonymous
        Type* d_binding; // points back to pointer or array type in case of anonymous type

        uint d_baseType : 5;    // used by BaseType
        uint d_union : 1;   // used by Record (CUNION)
        uint d_typeBound : 1; // used by ProcType
        uint d_varargs : 1; // used by ProcType
        uint d_byValue : 1;   // used to mark a Record which can be represented as CLI struct
        uint d_selfRef : 1;  // true if legally referencing self (i.e. via pointer)
        uint d_metaActual : 1;  // true if this is an actual type replacing ANY in instantiation
        uint d_usedByVal : 1; // true if record is used by value as a formal parameter, return, field or variable
        uint d_usedByRef : 1; // true if record is used as pointer base type in a formal param, return, field or variable or VAR param

        // Ref<Expression> d_flag; // optional system flag, no longer used, see Scope::d_sysAttrs

        Type():d_decl(0),d_binding(0),d_baseType(0),d_union(false),
            d_typeBound(false),d_varargs(false),d_byValue(false),d_selfRef(false),d_metaActual(false),
            d_usedByVal(false), d_usedByRef(false) {}
        typedef QList< Ref<Type> > List;
        virtual bool isStructured(bool withPtrAndProcType = false) const { return false; }
        virtual bool isPointer() const { return false; }
        virtual Type* derefed() { return this; }
        virtual QString pretty() const { return QString(); }
        virtual bool hasByteSize() const { return true; }
        virtual quint32 getByteSize() const { return 0; }
        virtual quint32 getAlignment() const { return getByteSize(); }
        Named* findDecl(bool recursive = false) const;
        int getBaseType() const { return d_baseType; }
        bool isInteger() const { return d_baseType >= BYTE && d_baseType <= LONGINT; }
        bool isReal() const { return d_baseType == REAL || d_baseType == LONGREAL; }
        bool isNumeric() const { return isInteger() || isReal(); }
        bool isString(bool* wide = 0) const;
        bool isChar(bool* wide = 0) const;
        bool isSet() const { return d_baseType == SET; }
        bool isText(bool* wide = 0, bool resolvePtr = false) const;
        bool isByteArray(bool resolvePtr = false) const;
        Record* toRecord(bool* isPtr = 0) const;
        Module* declaredIn();
    };

    struct BaseType : public Type
    {
        // this is a simple type like INTEGER, REAL etc.
        // actual data is in Type::d_baseType
        static const char* s_typeName[];

        BaseType(quint8 t = NIL ) { d_baseType = t; }
        QVariant maxVal() const;
        QVariant minVal() const;
        quint32 getByteSize() const;
        int getTag() const { return T_BaseType; }
        void accept(AstVisitor* v) { v->visit(this); }
        const char* getTypeName() const { return s_typeName[d_baseType]; }
        QString pretty() const { return getTypeName(); }
    };

    struct Pointer : public Type
    {
        Ref<Type> d_to; // only to Record or Array
        int getTag() const { return T_Pointer; }
        bool isStructured(bool withPtrAndProcType = false) const { return withPtrAndProcType; }
        void accept(AstVisitor* v) { v->visit(this); }
        quint32 getByteSize() const { return s_pointerByteSize; }
        QString pretty() const;
        bool isPointer() const { return true; }
        static quint8 s_pointerByteSize;
    };

    struct Array : public Type
    {
        qint32 d_len;  // zero if dynamic, -1 if error
        Ref<Expression> d_lenExpr; // null for open arrays
        Ref<Type> d_type;
        Array():d_len(0) {}
        int getTag() const { return T_Array; }
        void accept(AstVisitor* v) { v->visit(this); }
        bool isStructured(bool withPtrAndProcType = false) const { return true; }
        bool hasByteSize() const;
        quint32 getByteSize() const;
        quint32 getAlignment() const;
        Type* getTypeDim(int& dims ) const;
        QString pretty() const;
        QList<Array*> getDims();
    };

    struct Record : public Type
    {
        Ref<QualiType> d_base; // super type - a quali to a Record or Pointer or null
        Record* d_baseRec;
        QList<Record*> d_subRecs;

        typedef QHash<const char*,Named*> Names;
        Names d_names;
        QList< Ref<Field> > d_fields;
        QList< Ref<Procedure> > d_methods;
        quint16 d_fieldCount, d_methCount;
        uint d_alignment : 4;
        uint d_byteSize : 28;

        Record():d_baseRec(0),d_fieldCount(0),d_methCount(0),d_byteSize(0),d_alignment(0) {}
        int getTag() const { return T_Record; }
        void accept(AstVisitor* v) { v->visit(this); }
        bool isStructured(bool withPtrAndProcType = false) const { return true; }
        Named* find(const QByteArray& name , bool recursive) const;
        QString pretty() const { return d_unsafe ? ( d_union ? "CUNION" : "CSTRUCT" ) : "RECORD"; }
        QList<Field*> getOrderedFields() const;
        QList<Procedure*> getOrderedMethods() const;
        Record* findBySlot(int) const;
        quint32 getByteSize() const;
        quint32 getAlignment() const { return d_alignment; }
        Field* nextField(Field*) const;
        static QSet<Record*> calcDependencyOrder(QList<Record*>& inout);
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
        bool isStructured(bool withPtrAndProcType = false) const { return withPtrAndProcType; }
        bool isPointer() const { return true; }
        Parameter* find( const QByteArray& ) const;
        void accept(AstVisitor* v) { v->visit(this); }
        bool isBuiltIn() const;
        QString pretty() const { return d_typeBound ? "PROC(^)" : "PROC"; }
        quint32 getByteSize() const { return Pointer::s_pointerByteSize; }
    };

    struct QualiType : public Type
    {
        typedef QPair<Named*,Named*> ModItem; // Module or 0 -> Item

        Ref<Expression> d_quali;

        QualiType(){}
        ModItem getQuali() const;
        QByteArrayList getQualiString() const;
        int getTag() const { return T_QualiType; }
        bool hasByteSize() const;
        quint32 getByteSize() const;
        virtual quint32 getAlignment() const;
        void accept(AstVisitor* v) { v->visit(this); }
        Type* derefed();
        QString pretty() const;
        bool isDotted() const;
    };

    struct Named : public Thing
    {
        enum Visibility { NotApplicable, Private, ReadWrite, ReadOnly, LocalAccess };
        QByteArray d_name;
        Ref<Type> d_type;
        Scope* d_scope; // owning scope up to module (whose scope is nil)

        uint d_liveFrom : 20;
        uint d_upvalSource : 1; // the scope from which locals are used; the local used as upval
        uint d_upvalIntermediate: 1; // the scopes between source and user
        uint d_upvalSink : 1; // the scope from which locals from the outer scope are used
        uint d_visibility : 3; // Visibility enum
        uint d_synthetic: 1;
        uint d_hasErrors : 1;
        uint d_liveTo : 20;
        uint d_noBody : 1; // Procedure

        Named(const QByteArray& n = QByteArray(), Type* t = 0, Scope* s = 0):d_scope(s),d_type(t),d_name(n),
            d_visibility(NotApplicable),d_synthetic(false),d_liveFrom(0),d_liveTo(0),
            d_upvalSource(0),d_upvalIntermediate(0),d_upvalSink(0),
            d_hasErrors(0),d_noBody(0) {}
        virtual QByteArray getName() const { return d_name; }
        bool isNamed() const { return true; }
        virtual bool isVarParam() const { return false; }
        Module* getModule() const;
        QByteArrayList getQualifiedName() const;
        bool isPublic() const;
        const char* visibilitySymbol() const;
    };

    struct GenericName : public Named
    {
        void accept(AstVisitor* v) { v->visit(this); }
        int getTag() const { return T_GenericName; }
    };

    struct Field : public Named // Record field
    {
        Record* d_owner; // the record owning the field
        Field* d_super; // the field of the super class this field overrides, or zero
        // slots are used for byte offsets of the field in the record
        Field():d_super(0),d_owner(0){}
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
        bool d_receiver;
        Parameter():d_var(false),d_const(false),d_receiver(false) {}
        int getTag() const { return T_Parameter; }
        void accept(AstVisitor* v) { v->visit(this); }
        bool isVarParam() const { return ( d_var || d_const ); }
    };

    struct Const : public Named
    {
        QVariant d_val;
        uint d_vtype : 8;
        uint d_strLen : 22;
        uint d_wide : 1; // mark WSTRING and WCHAR, double vs float, longint vs integer
        uint d_minInt : 1;
        Ref<Expression> d_constExpr;
        Const():d_vtype(0),d_wide(false),d_strLen(0),d_minInt(false){}
        Const(const QByteArray& name, Literal* lit );
        int getTag() const { return T_Const; }
        void accept(AstVisitor* v) { v->visit(this); }
    };

    struct Enumeration : public Type
    {
        QList< Ref<Const> > d_items;
        int getTag() const { return T_Enumeration; }
        void accept(AstVisitor* v) { v->visit(this); }
        QString pretty() const { return "enumeration"; }
        quint32 getByteSize() const;
    };

    struct Import : public Named
    {
        VirtualPath d_path;
        Ob::RowCol d_aliasPos; // invalid if no alias present
        Ref<Module> d_mod;
        MetaActuals d_metaActuals;
        int getTag() const { return T_Import; }
        void accept(AstVisitor* v) { v->visit(this); }
    };

    struct BuiltIn : public Named
    {
        enum { // Oberon-07
               ABS, ODD, LEN, LSL, ASR, ROR, FLOOR, FLT, ORD, CHR, INC, DEC, INCL, EXCL,
               NEW, ASSERT, PACK, UNPK,
               LED, // LED not global proc in Oberon report, but used as such in Project Oberon
               // IDE
               TRAP, TRAPIF, TRACE, NOP, LDMOD, LDCMD,
               // SYSTEM
               SYS_ADR, SYS_BIT, SYS_GET, SYS_H, SYS_LDREG, SYS_PUT, SYS_REG, SYS_VAL, SYS_COPY, // obsolete
               // Oberon-2
               MAX, CAP, LONG, SHORT, HALT, COPY, ASH, MIN, BYTESIZE, ENTIER, // BYTESIZE=SIZE
               // Blackbox
               BITS,
               // Oberon-2 SYSTEM
               SYS_MOVE, SYS_NEW, SYS_ROT, SYS_LSH, SYS_GETREG, SYS_PUTREG, // obsolete
               // Blackbox SYSTEM
               SYS_TYP,
               // Oberon+
               CAST, STRLEN, WCHR, PRINTLN, DEFAULT, BITAND, BITNOT, BITOR, BITXOR,
               BITSHL, BITSHR, BITASR,
               ADR, // obsolete
               MAXBUILTIN
             };
        static const char* s_typeName[];
        quint8 d_func;
        BuiltIn(quint8 f, ProcType* = 0 );
        int getTag() const { return T_BuiltIn; }
        void accept(AstVisitor* v) { v->visit(this); }
        static QByteArrayList getValidNames();
    };

    struct Scope : public Named
    {
        typedef QHash<const char*, Named*> Names;
        Names d_names;
        QList< Ref<Named> > d_order;
        QList< Ref<IdentLeaf> > d_helper; // filled with helper decls when fillXref
        SysAttrs d_sysAttrs;

        StatSeq d_body;
        Ob::RowCol d_end;
        quint16 d_varCount; // Variable, LocalVar
        quint8 d_parCount; // Parameter; incl. receiver

        Scope():d_varCount(0),d_parCount(0){}
        bool isScope() const { return true; }
        int getTag() const { return T_Scope; }

        Named* find( const QByteArray&, bool recursive = true ) const;
        bool add( Named* );
    };

    struct Procedure : public Scope
    {
        Ref<Parameter> d_receiver;
        Record* d_receiverRec; // the record to which this procedure is bound
        Procedure* d_super; // the procedure of the super class this procedure overrides, or zero
        QList<Procedure*> d_subs; // the procedures of the subclasses which override this procedure
        // Ref<Expression> d_imp; // the number or string after PROC+, no longer supported, see d_sysAttrs
        Procedure():d_receiverRec(0),d_super(0) {}
        void accept(AstVisitor* v) { v->visit(this); }
        int getTag() const { return T_Procedure; }
        ProcType* getProcType() const;
    };

    struct Module : public Scope
    {
        QList<Import*> d_imports;
        QList<Module*> d_usedBy;
        QString d_file;
        VirtualPath d_fullName;  // package path (if present) + module name
        MetaParams d_metaParams;
        MetaActuals d_metaActuals; // set if this is an instance of a generic module
        Ob::RowCol d_begin;
        bool d_isValidated;
        bool d_isDef; // DEFINITION module
        bool d_isExt;
        bool d_externC;
        QList< Ref<Type> > d_helper2; // filled with pointers because of ADDROF

        Module():d_isDef(false),d_isValidated(false),d_isExt(false),d_externC(false) {}
        int getTag() const { return T_Module; }
        void accept(AstVisitor* v) { v->visit(this); }
        QByteArray getName() const;
        QByteArray getFullName() const;
        QByteArray formatMetaActuals() const;
        bool isFullyInstantiated() const;
        Import* findImport(Module*) const;
        void findAllInstances(QList<Module*>&) const;
      };

    struct NamedType : public Named
    {
        NamedType( const QByteArray& n, Type* t ) { d_name = n; d_type = t; t->d_decl = this; } // only used for base types
        NamedType() {}
        int getTag() const { return T_NamedType; }
        void accept(AstVisitor* v) { v->visit(this); }
    };

    struct Statement : public Thing
    {
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
        enum { IF, WHILE, REPEAT, WITH, LOOP };
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

    enum IdentRole { NoRole, DeclRole, LhsRole, VarRole, RhsRole, SuperRole, SubRole, CallRole,
                     ImportRole, ThisRole, MethRole, StringRole };
                     // used by Xref as additional information for IdentLeaf/Sel

    struct Expression : public Thing
    {
        NoRef<Type> d_type; // this must be NoRef, otherwise there are refcount cycles!
        virtual Named* getIdent(bool first=false) const { return 0; }
        virtual Module* getModule() const { return 0; }
        virtual quint8 visibilityFor(Module*) const { return Named::NotApplicable; }
        virtual Expression* getSub() const { return 0; }
        QList<Expression*> getSubList() const;
        virtual quint8 getUnOp() const { return 0; }
        virtual quint8 getBinOp() const { return 0; }
        virtual IdentRole getIdentRole() const { return NoRole; }
    };

    struct Literal : public Expression
    {
        enum { SET_BIT_LEN = 32 };
        typedef std::bitset<SET_BIT_LEN> SET;

        enum ValueType { Invalid, Integer, Real, Boolean, String /* bytearray utf8 */, Bytes /* bytearray */,
                         Char /* quint16 */, Nil, Set, Enum };
        QVariant d_val;
        uint d_vtype : 8;
        uint d_strLen : 22;
        uint d_wide : 1; // mark WSTRING and WCHAR, or double precision float, or LONGINT
        uint d_minInt : 1; // integer type is at least INTEGER
        Literal( ValueType t = Invalid, Ob::RowCol l = Ob::RowCol(),
                 const QVariant& v = QVariant(), Type* typ = 0 ):
                        d_val(v),d_vtype(t),d_strLen(0),d_wide(0),d_minInt(0){ d_loc = l; d_type = typ; }
        int getTag() const { return T_Literal; }
        void accept(AstVisitor* v) { v->visit(this); }
        static bool isWide( const QString& );
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
        IdentRole d_role;
        IdentLeaf():d_mod(0),d_role(NoRole) {}
        IdentLeaf( Named* id, const Ob::RowCol&, Module* mod, Type* t, IdentRole r );
        Module* d_mod; // we need this to find out when xref from which module the ident is coming
        Named* getIdent(bool first=false) const { return d_ident.data(); }
        Module* getModule() const { return d_mod; }
        int getTag() const { return T_IdentLeaf; }
        void accept(AstVisitor* v) { v->visit(this); }
        quint8 visibilityFor(Module*) const;
        IdentRole getIdentRole() const { return d_role; }
    };

    struct UnExpr : public Expression
    {
        enum Op { Invalid,
                  NEG, NOT, DEREF, ADDROF, // implemented in UnExpr
                  CAST, SEL, CALL, IDX // implemented in subclasses
                };
        static const char* s_opName[];
        quint8 d_op;
        Ref<Expression> d_sub;
        UnExpr(quint8 op = Invalid, Expression* e = 0 ):d_op(op),d_sub(e){}
        int getTag() const { return T_UnExpr; }
        void accept(AstVisitor* v) { v->visit(this); }
        quint8 visibilityFor(Module* m) const;
        Module* getModule() const { return d_sub.isNull() ? 0 : d_sub->getModule(); }
        Expression* getSub() const { return d_sub.data(); }
        quint8 getUnOp() const { return d_op; }
        Named* getIdent(bool first=false) const { return first && d_sub ? d_sub->getIdent(first) : 0; }
    };

    struct IdentSel : public UnExpr // SEL
    {
        NoRef<Named> d_ident;
        QByteArray d_name; // name to be resolved with result written to d_ident
        IdentRole d_role;
        IdentSel():UnExpr(SEL),d_role(NoRole) {}
        Named* getIdent(bool first=false) const { return first && d_sub ? d_sub->getIdent(first) : d_ident.data(); }
        int getTag() const { return T_IdentSel; }
        void accept(AstVisitor* v) { v->visit(this); }
        quint8 visibilityFor(Module* m) const;
        IdentRole getIdentRole() const { return d_role; }
    };

    struct ArgExpr : public UnExpr // CALL, IDX, or CAST
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
        quint8 d_inclType; // the including type in case of comparison relations (whose type is bool otherwise)
        Ref<Expression> d_lhs, d_rhs;
        BinExpr():d_op(Invalid),d_inclType(0){}
        int getTag() const { return T_BinExpr; }
        void accept(AstVisitor* v) { v->visit(this); }
        bool isRelation() const;
        bool isArithOp() const;
        Module* getModule() const { return !d_lhs.isNull() ? d_lhs->getModule() : !d_rhs.isNull() ? d_rhs->getModule() : 0 ; }
        quint8 getBinOp() const { return d_op; }
    };

    struct SysAttr : public Named
    {
        ExpList d_valExpr;
        QVariantList d_values;
    };

    class Instantiator // interface
    {
    public:
        virtual Module* instantiate( Module* generic, const MetaActuals& actuals ) = 0;
        virtual QList<Module*> instances( Module* generic ) = 0;
    };
}

#ifdef OBX_AST_DECLARE_SET_METATYPE_IN_HEADER
Q_DECLARE_METATYPE( Obx::Literal::SET )
#endif

#endif // OBXAST_H
