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

#include "ObLuaGen2.h"
#include "ObAst.h"
#include "ObErrors.h"
#include "ObAstEval.h"
#include <QTextStream>
#include <QtDebug>
#include <QCoreApplication>
#include <QDateTime>
#include <QDir>
using namespace Ob;
using namespace Ob::Ast;

struct TransformIndexFunctionToStats : public AstVisitor
{
    // Rules:
    // A function call cannot be the actual arg of a VAR param
    // Lua cannot take the address of a slot; thus the slot is passed by value to the param and returned
    //   as an additional return value of the procedure, which might already have a regular return value or not.
    // If the desig is an Index we need two slots as a substitute of the address: the table and the index.
    //   So if we have A[B] as an actual argument of VAR param, we need to remember both A and B to be able
    //   to assign the changed value to the right element; otherwise doing something like "A[B] = func(A[B])"
    //   could go to the wrong element if B renders a different value when applied twice.
    // If we have A.B (IdentSel) as an actual argument of VAR param, then "A.B = func(A.B)" hits the same element ever.
    // If we have A[B].C, we again have to remember the table referenced by A[B] and the index "C".
    // To conclude: whenever the desig includes an index (and only then), we need two slots to remember the table and the index.
    // The index can contain other function calls, which by themselves have indexed arguments,
    //   e.g. "F1( A[ F2( B[C] ) ] )"; here we need to memorize the table referenced by A, the index "F2( B[C] )",
    //   the table referenced by B and the index C, if both F1 and F2 have VAR params. This call translates to
    //   "T1 = B; T2 = C; T3, T1[T2] = F2(T1[T2]); T4 = A; T4[T3] = F1( T4[T3] );" T4 can reuse T1 or T2, B and A no
    //   copy required, so more efficient "T2 = C; T3, B[T2] = F2(B[T2]); A[T3] = F1(A[T3]);"
    // Therefore the main goal is to move function calls in VAR param actuals to preceding, isolated assign statements.
    //   Adding return assignments for VAR params is then simply a local operation of the assignment or call

    TransformIndexFunctionToStats():curScope(0){}

    Module* curModule;
    Scope* curScope;
    int allocatedVarTemps;
    bool allocatedTemp1;
    bool allocatedTemp2;
    QList<StatSeq> bodies;
    QList<bool> inVar;

    void assureTemp1()
    {
        if( allocatedTemp1 )
            return;
        Ref<Variable> tmp = new Variable();
        tmp->d_scope = curScope;
        tmp->d_name = QByteArray("__tmp1");
        tmp->d_synthetic = true;
        curScope->d_names.insert( tmp->d_name,tmp.data() );
        curScope->d_order.append(tmp.data());
        allocatedTemp1 = true;
    }

    void prepareBody( Scope* m )
    {
        curScope = m;
        allocatedVarTemps = 0;
        allocatedTemp1 = false;
        allocatedTemp2 = false;
    }

    void iterateBody( StatSeq& body )
    {
        bodies.push_back( StatSeq() );
        for( int i = 0; i < body.size(); i++ )
        {
            body[i]->accept(this);
            bodies.back().append( body[i] );
        }
        body = bodies.back();
        bodies.pop_back();
    }

    void visit( Procedure* m)
    {
        for( int i = 0; i < m->d_order.size(); i++ )
            m->d_order[i]->accept(this); // subprocs

        prepareBody(m);
        iterateBody( m->d_body );
        curScope = 0;
    }

    void visit( Module* m )
    {
        for( int i = 0; i < m->d_order.size(); i++ )
            m->d_order[i]->accept(this); // subprocs

        prepareBody(m);
        iterateBody( m->d_body );
        curScope = 0;
    }

    void handleCall( Ref<Expression>& e )
    {
        if( e->getTag() != Thing::T_CallExpr )
            return;

        CallExpr* c = static_cast<CallExpr*>( e.data() );
        ProcType* pt = c->getProcType();
        if( pt->isBuiltIn() )
            return; // RISK

        // if this call is part of a designator expression which is passed to a VAR param, it could have a side effect
        // and thus must be taken out of the expression
        const bool isInVar = !inVar.isEmpty() && inVar.back();

        // if this calls a procedure with var params, we need to account for the additional returns
        // we therefore need to transform function calls to assign statements; the function assigns its regular value
        // to a temporary variable which is then used at the original place of the function
        bool hasVar = false;
        int numOfIndexVar = 0;
        for( int i = 0; i < pt->d_formals.size(); i++ )
        {
            if( pt->d_formals[i]->d_var )
            {
                hasVar = true;
                if( i < c->d_actuals.size() && getLastIndexOp(c->d_actuals[i].data()).first != 0 )
                    numOfIndexVar++;
            }
        }

        if( !hasVar && !isInVar )
            return;

        // The function assigns to a new local variable which then replaces the function call at it's original
        // place; the function call is moved to a preceding assignment statement.
        // We only take care of the regular function return here which we assign to a temorary. The additional return
        // values required - one for each VAR param - are handled in the code generator directly.
        // It's no issue to add new temporary variables ad libidum, because the Lua compiler will recognize their scope
        // and reuse the slots.
        Ref<Variable> tmp = new Variable();
        tmp->d_scope = curScope;
        tmp->d_name = QByteArray("__c") + QByteArray::number( curScope->d_order.size() );
        tmp->d_synthetic = true;
        curScope->d_names.insert( tmp->d_name,tmp.data() );
        curScope->d_order.append(tmp.data());
        tmp->d_type = e->d_type;
        tmp->d_liveFrom = 1;
        if( !bodies.back().isEmpty() )
            tmp->d_liveFrom = bodies.back().back()->d_loc.d_row;
        tmp->d_liveTo = e->d_loc.d_row;

        Ref<Assign> a = new Assign();
        Ref<IdentLeaf> id = new IdentLeaf();
        id->d_ident = tmp.data();
        a->d_lhs = id.data();
        a->d_rhs = c;

        Q_ASSERT( !bodies.isEmpty() );
        bodies.back().append( a.data() );

        e = id.data();

        const int diff = numOfIndexVar - allocatedVarTemps;
        if( diff > 0 )
        {
            // not all requred vars are allocated yet
            for( int i = 0; i < diff; i++ )
            {
                Ref<Variable> tmp = new Variable();
                tmp->d_scope = curScope;
                tmp->d_name = QByteArray("__t") + QByteArray::number( allocatedVarTemps );
                tmp->d_synthetic = true;
                curScope->d_names.insert( tmp->d_name,tmp.data() );
                curScope->d_order.append(tmp.data());
                // type is left empty here; just a placehoder
                tmp = new Variable();
                tmp->d_scope = curScope;
                tmp->d_name = QByteArray("__i") + QByteArray::number( allocatedVarTemps );
                tmp->d_synthetic = true;
                curScope->d_names.insert( tmp->d_name,tmp.data() );
                curScope->d_order.append(tmp.data());
                // type is left empty here; just a placehoder
            }
            allocatedVarTemps += diff;
        }

    }

    void visit( Call* c )
    {
        // this is a procedure call, but when VAR params are present it turns to an implicit function call
        c->d_what->accept(this);
    }

    void visit( Return* r )
    {
        r->d_what->accept(this);
        handleCall( r->d_what );
    }

    void visit( Assign* a )
    {
        // lhs cannot be a procedure call
        a->d_lhs->accept(this);
        a->d_rhs->accept(this);
        handleCall( a->d_rhs );
    }

    void visit( IfLoop* l )
    {
        Q_ASSERT( l->d_if.size() == l->d_then.size() );
        for( int i = 0; i < l->d_if.size(); i++ )
        {
            Q_ASSERT( !bodies.isEmpty() );
            const int pre = bodies.back().size();
            l->d_if[i]->accept(this);
            handleCall(l->d_if[i]);
            const StatSeq added = bodies.back().mid(pre);
            if( l->d_op == IfLoop::REPEAT )
                bodies.back() = bodies.back().mid(0,pre);
            iterateBody( l->d_then[i] );
            // replicate statements generated from d_if to end of body for WHILE and REPEAT
            // in case of REPEAT the added statements only occur in the loop, not before.
            if( !added.isEmpty() && ( l->d_op == IfLoop::REPEAT || l->d_op == IfLoop::WHILE ) )
                l->d_then[i] += added;
        }
        iterateBody( l->d_else );
    }

    void visit( ForLoop* l )
    {
        l->d_from->accept(this);
        handleCall( l->d_from );
        Q_ASSERT( !bodies.isEmpty() );
        const int pre = bodies.back().size();
        l->d_to->accept(this);
        handleCall( l->d_to );
        const StatSeq added = bodies.back().mid(pre);
        l->d_by->accept(this); // const expr cannot contain procedure call
        iterateBody( l->d_do );
        // replicate statements generated from d_to to end of body
        if( !added.isEmpty() )
            l->d_do += added;
    }

    void visit( CaseStmt* c )
    {
        c->d_exp->accept(this);
        handleCall( c->d_exp );
        for( int i = 0; i < c->d_cases.size(); i++ )
        {
            for( int j = 0; j < c->d_cases[i].d_labels.size(); j++ )
            {
                c->d_cases[i].d_labels[j]->accept(this);
                handleCall(c->d_cases[i].d_labels[j]); // can case labels be funcion calls?
            }
            iterateBody( c->d_cases[i].d_block );
        }
    }

    void visit( SetExpr* s )
    {
        for( int i = 0; i < s->d_parts.size(); i++ )
        {
            s->d_parts[i]->accept(this);
            handleCall(s->d_parts[i]);
        }
    }

    void visit( UnExpr* e )
    {
        e->d_sub->accept(this);
        handleCall(e->d_sub);
    }

    void visit( IdentSel* e )
    {
        e->d_sub->accept(this);
        handleCall(e->d_sub);
    }

    void visit( CallExpr* c )
    {
        Q_ASSERT( curScope != 0 );

        Q_ASSERT( !c->d_sub.isNull() && !c->d_sub->d_type.isNull() && c->d_sub->d_type->derefed()->getTag() == Thing::T_ProcType );
        ProcType* pt = static_cast<ProcType*>( c->d_sub->d_type->derefed() );

        // CallExpr is always the top Expr in a desig
        c->d_sub->accept(this);

        if( pt->isBuiltIn() )
        {
            // built-in procedures which need special treatment or are not supported
            BuiltIn* bi = static_cast<BuiltIn*>( pt->d_ident );
            switch(bi->d_func)
            {
            case BuiltIn::INC:
            case BuiltIn::DEC:
                return; // no VAR
            case BuiltIn::NEW:
                assureTemp1();
                return; // inline anyway
            case BuiltIn::GET:
            case BuiltIn::PUT:
                return; // not supported
            }
            // else
        }

        Q_ASSERT( pt->d_formals.size() == c->d_actuals.size() );
        for( int i = 0; i < c->d_actuals.size(); i++ )
        {
            inVar.push_back(pt->d_formals[i]->d_var);
            c->d_actuals[i]->accept(this);
            inVar.pop_back();
            /* unnecessary; moving the calls to a separate statement each is sufficient; the additional
             * return assignments to the actuals of VAR params is directly done by the code generator
            if( pt->d_formals[i]->d_var )
            {
                // check wheter actual contains an index

                // we already assured that actual is a desig, not an arbitrary expr
                Expression* e = getLastIndexOp( c->d_actuals[i].data() );
                if( e )
                    qDebug() << "VAR param with actual index desig" << e->d_loc.d_row << e->d_loc.d_col << curModule->d_file
                             << "nest depth" << calls.size();
            }
            */
            handleCall(c->d_actuals[i]); // actual of a value param may be function call; the latter function might have VAR params.
        }
    }

    void visit( BinExpr* e )
    {
        e->d_lhs->accept(this);
        handleCall(e->d_lhs);
        e->d_rhs->accept(this);
        handleCall(e->d_rhs);
    }

    typedef QPair<BinExpr*,Expression*> IndexPred;
    static IndexPred getLastIndexOp( Expression* e, Expression* pred = 0 )
    {
        // TODO: review this concept and printRightPart. Only the case where a
        // desig contains [] has to be handled, and then only the last table/index
        if( e == 0 )
            return IndexPred();
        switch( e->getTag() )
        {
        case Thing::T_BinExpr:
            {
                BinExpr* bi = static_cast<BinExpr*>(e);
                if( bi->d_op == BinExpr::Index )
                    return qMakePair(bi,pred);
                else
                    return getLastIndexOp(bi->d_lhs.data(),bi); // RISK: only look in lhs
            }
            break;
        case Thing::T_UnExpr:
            return getLastIndexOp( static_cast<UnExpr*>(e)->d_sub.data(), e );
        case Thing::T_IdentSel:
            return getLastIndexOp( static_cast<IdentSel*>(e)->d_sub.data(), e );
        case Thing::T_CallExpr:
            return getLastIndexOp( static_cast<CallExpr*>(e)->d_sub.data(), e );
        }
        return IndexPred();
    }
};

static QSet<QByteArray> s_lkw;

static bool isLuaKeyword( const QByteArray& str )
{
    if( s_lkw.isEmpty() )
        s_lkw << "and" <<       "break" <<     "do" <<        "else" <<      "elseif"
              << "end" <<       "false" <<     "for" <<       "function" <<  "if"
              << "in" <<        "local" <<     "nil" <<       "not" <<       "or"
              << "repeat" <<    "return" <<    "then" <<      "true" <<      "until" <<     "while"
                 // built-in LuaJIT libraries
              << "math" << "bit" << "obnlj" << "module";
    return s_lkw.contains(str);
}

static bool startsWith2Underscores( const QByteArray& str )
{
    if( !str.startsWith("__") )
        return false;
    if( str.size() == 2 )
        return true;
    if( str[3] == '_' )
        return false;
    else
        return true;
}

static QByteArray luaStringEscape( QByteArray str )
{
    QByteArray out;
    out.reserve(str.size()*2);
    char buf[10];
    for( int i = 0; i < str.size(); i++ )
    {
        const quint8 c = quint8(str[i]);
        if( QChar::fromLatin1(str[i]).isPrint() && c != 255 )
        {
            switch( quint8(str[i]))
            {
            case '\\':
                out += "\\\\";
                break;
            case '"':
                out += "\\\"";
                break;
            default:
                out += str[i];
                break;
            }
        }else
        {
            switch( c )
            {
            case '\n':
                out += "\\n";
                break;
            case '\t':
                out += "\\t";
                break;
            case '\a':
                out += "\\a";
                break;
            case '\b':
                out += "\\b";
            case '\f':
                out += "\\f";
                break;
            case '\v':
                out += "\\v";
                break;
            case 0:
                out += "\\0";
                break;
            default:
                ::sprintf(buf,"\\%03d", c );
                out += buf;
                break;
            }
        }
    }
    return out;
}

static QByteArray toName( Named* id)
{
    if( id == 0 )
        return QByteArray();
    if( id->d_synthetic )
        return id->d_name;
    QByteArray res = id->d_name;
    if( isLuaKeyword(res) || startsWith2Underscores(res) )
        return res + "_";
    else
        return res;
}

struct LuaGen2Imp : public AstVisitor
{
    QTextStream out;
    Errors* err;
    Module* mod;
    quint16 level;
    Scope* curScope;
    bool ownsErr;

    QString ws() const
    {
        return QString(level,QChar('\t'));
    }

    static QualiType::ModItem findClass( Type* t )
    {
        // 1:1 from LjbcGen
        QualiType::ModItem res;


        if( t->getTag() == Thing::T_Pointer )
        {
            Pointer* p = static_cast<Pointer*>(t);
            if( p->d_to->getTag() == Thing::T_QualiType )
                return findClass( p->d_to.data() );
            Q_ASSERT( p->d_to->getTag() == Thing::T_Record &&
                      p->d_to->d_ident == 0 );
            // since QualiType type aliasses are no longer shortcutted, but there is an instance of
            // QualiType wherever there is no inplace type declaration after TO. But the latter has
            // no d_ident by definition.
        }else if( t->getTag() == Thing::T_QualiType )
        {
            QualiType* q = static_cast<QualiType*>( t );
            res = q->getQuali();
            if( res.first )
                return res; // quali points to an import, that's where we find a class for sure
            Q_ASSERT( res.second );
            return findClass( res.second->d_type.data() );
        }

        Q_ASSERT( t->getTag() != Thing::T_QualiType && t == t->derefed() );

        res.second = t->d_ident;
        if( res.second == 0 )
        {
            if( t->getTag() == Thing::T_Record )
            {
                Record* r = static_cast<Record*>( t );
                if( r->d_binding )
                    res.second = r->d_binding->d_ident;
            }
        }
        return res;
    }

    static QByteArray className( Type* t )
    {
        Q_ASSERT( t );
        QualiType::ModItem mi = findClass( t );
        if( mi.first && mi.second )
            return toName(mi.first) + "." + toName(mi.second);
        else if( mi.second )
            return toName(mi.second);
        else
            return "???";
    }

    void inline initHelper( Array* a, int curDim, const QByteArray& name, const QByteArray& rec = QByteArray() )
    {
        // TODO: higher dims via local temporary instead of n-dim index (only relevant for dim > 2)
        out << " = obnlj.Arr(" << a->d_len << ")" << endl;
        out << ws() << "for __" << curDim << "=1," << a->d_len << " do" << endl;
        level++;
        if( !rec.isEmpty() )
            out << ws() << "local " << rec << " = {}" << endl;
        out << ws() << name;
        for( int j = 0; j <= curDim; j++ )
            out << "[__" << j << "]";
        if( !rec.isEmpty())
            out << " = " << rec << endl;
        level--;
    }

    void initMatrix( const QList<Array*>& dims, const QByteArray& name, int curDim )
    {
        // We need to create the arrays for each matrix dimension besides the highest one, unless it is of record value.
        // If a matrix has only one dimension (i.e. it is an array), no initialization is required, unless it is of record value
        // Each matrix dimension is created in a recursive call of this method.
        // Examples:
        // ARRAY n OF INTEGER
        //      -> obnlj.Arr(n)
        // ARRAY n OF CHAR
        //      -> obnlj.Str(n)
        // ARRAY n, m OF INTEGER
        //      -> for i=1,n do A[i] = obnlj.Arr(m) end
        // ARRAY n, m OF RECORD END
        //      -> for i=1,n do A[i] = obnlj.Arr(m)
        //          for j=1,m do A[i][j] = initRecord() end end


        const int oldLevel = level;
        level += curDim;

        if( curDim == dims.size() - 1 )
        {
            // we're at the highest dimension
            if( isString( dims[curDim] ) )
                out << " = obnlj.Str(" << dims[curDim]->d_len << ")";
            else if( dims[curDim]->d_type->derefed()->getTag() == Thing::T_Record )
            {
                const QByteArray rec = "__r" + QByteArray::number(curDim);
                initHelper( dims[curDim], curDim, name, rec );
                level++;
                initRecord( dims[curDim]->d_type.data(), rec );
                level--;
                out << ws() << "end";
            }
            else
                out << " = obnlj.Arr(" << dims[curDim]->d_len << ")";
        }else
        {
            // we're at a lower dimension
            initHelper( dims[curDim], curDim, name );
            initMatrix( dims, name, curDim + 1 );
            out << ws() << "end";
        }
        level = oldLevel;
        out << endl;
    }

    static bool isString( Type* t )
    {
        if( t && t->getTag() == Thing::T_Array )
        {
            Array* a = static_cast<Array*>(t);
            Type* td = a->d_type->derefed();
            if( td->getTag() == Thing::T_BaseType &&
                    static_cast<BaseType*>( td )->d_type == BaseType::CHAR )
                return true;
        }
        return false;
    }

    void initArray(Array* arr, const QByteArray& name )
    {
        // out << name << " = "; was already done by the caller
        // provide the code right of the '=' with all the necessary initialization
        Array* curDim = arr;
        QList<Array*> dims;
        dims << curDim;
        Type* td = curDim->d_type->derefed();
        while( td->getTag() == Thing::T_Array )
        {
            curDim = static_cast<Array*>( td );
            dims << curDim;
            td = curDim->d_type->derefed();
        }
        initMatrix( dims, name, 0 );
    }

    void initRecord( Type* rt, const QByteArray& name )
    {
        // out << " = {}" << endl; was already done by the caller

        Q_ASSERT( rt != 0 );

        if( findClass(rt).second != 0 )
            out << ws() << "setmetatable(" << name << "," << className( rt ) << ")" << endl;

        Record* r = toRecord(rt);
        QList<Record*> topDown;
        topDown << r;

        Record* base = r->getBaseRecord();
        while( base )
        {
            topDown.prepend(base);
            base = base->getBaseRecord();
        }

        for( int j = 0; j < topDown.size(); j++ )
        {
            // go from top to bottom through inheritance hierarchy and initialize members
            Record* rec = topDown[j];
            for( int i = 0; i < rec->d_fields.size(); i++ )
            {
                Type* t = rec->d_fields[i]->d_type.data();
                Type* td = t->derefed();
                const QByteArray field = name + "." + rec->d_fields[i]->d_name;
                level++;
                if( td->getTag() == Thing::T_Record )
                {
                    out << ws() << field << " = {}" << endl;
                    initRecord( t, field );
                }else if( td->getTag() == Thing::T_Array )
                {
                    out << ws() << field;
                    initArray( static_cast<Array*>(td), field);
                }
                level--;
            }
        }
    }

    void emitVariable( Named* v )
    {
        const QByteArray name = toName(v);
        out << ws() << "local ";
        // v->d_type can be null here because of the allocatedVarTemps in TransformIndexFunctionToStats
        Type* td = v->d_type.isNull() ? 0 : v->d_type->derefed();
        if( td && td->getTag() == Thing::T_Record )
        {
            level++;
            out << name << " = {}" << endl;
            initRecord( v->d_type.data(), name );
            level--;
        }else if( td && td->getTag() == Thing::T_Array )
        {
            level++;
            out << name;
            initArray( static_cast<Array*>(td), name );
            level--;
        }else
            out << name << endl;

        if( v->d_scope == mod && v->d_public )
            out << "module" << "." << name << " = function() return " << name << " end" << endl;
    }

    void visit( Variable* v )
    {
        emitVariable(v);
    }

    void visit( LocalVar* v )
    {
        emitVariable(v);
    }

    void visit( Parameter* p )
    {
        out << toName( p );
    }

    void visit( ProcType* p )
    {
        out << "(";
        for( int i = 0; i < p->d_formals.size(); i++ )
        {
            if( i != 0 )
                out << ",";
            out << toName(p->d_formals[i].data());
        }
        out << ")";
    }

    static bool inline isNamedTypeWithLocal( Named* nt )
    {
        // adapted from LjbcGen

        if( nt->getTag() != Thing::T_NamedType )
            return false;

        // we only consider original named type declarations here, i.e. no aliasses.
        if( nt->d_type->d_ident != nt )
            return false;

        const int tag = nt->d_type->getTag();

        // In case of type aliasses which point to record types (wheter in this or another module), we at least
        // put a copy of the original named type table to the module table of the present module,
        // but we don't allocate a new slot.
        if( tag == Thing::T_QualiType )
            return false;

        // We need a table for a named record type so it can be used as metatable for the
        // instance of the record and a base for subrecords.
        if( tag == Thing::T_Record )
            return true;

        if( tag == Thing::T_Pointer )
        {
            Pointer* p = static_cast<Pointer*>(nt->d_type.data());
            Q_ASSERT( p->d_to->derefed()->getTag() == Thing::T_Record );
            Record* r = static_cast<Record*>(p->d_to->derefed());

            // A pointer to an anonymous record declared for the pointer is treatet the
            // same way as a named Record declaration
            if( r->d_binding == p )
                return true;
        }

        return false;
    }

    void visit( NamedType* t )
    {
        if( isNamedTypeWithLocal( t ) )
        {
            // this is an original (i.e. non-alias) declaration of a recrod
            Record* r = toRecord(t->d_type.data());
            const QByteArray name = toName(t);
            out << ws() << "local " << name << " = ";
            out << "{}" << endl;
            if( !r->d_base.isNull() )
            {
                Q_ASSERT( r->d_base->d_quali->getIdent() != 0 );
                level++;
                out << ws() << "setmetatable(" << name << "," << className(r->d_base.data()) << ")" << endl;
                level--;
            }
            if( t->d_scope == mod && t->d_public )
                out << "module" << "." << name << " = " << name << endl;
        }else if( t->d_type->getTag() == Thing::T_QualiType && Model::toRecord(t->d_type.data()) != 0 )
        {
            QualiType* q = static_cast<QualiType*>(t->d_type.data());
            const QByteArray name = toName(t);
            out << ws() << "local " << name << " = ";
            q->d_quali->accept(this);
            out << endl;
            // << quali( tr->d_quali->d_type->d_ident ) << endl;
            if( t->d_scope == mod && t->d_public )
                out << "module" << "." << name << " = " << name << endl;
        }
    }

    void print( const QVariant& v )
    {
        if( v.canConvert<Ast::Set>() )
            out << "obnlj.SET(" << QByteArray::number( quint32(v.value<Ast::Set>().to_ulong()) ) << ")";
        else if( v.type() == QVariant::ByteArray )
            out << "obnlj.Str(\"" << luaStringEscape(v.toByteArray() ) << "\")";
        else if( v.isNull() )
            out << "null";
        else
            out << v.toByteArray();
    }

    void visit( Const*  c)
    {
        out << ws() << "local " + toName(c) << " = ";

        print( c->d_val );
        out << endl;
        if( c->d_scope == mod && c->d_public )
            out << "module" << "." << toName(c) << " = " << toName(c) << endl;
    }

    void visit( Import* i )
    {
        out << "local " << toName(i) << " = require '" << toName(i->d_mod.data()) << "'" << endl;
    }

    void visit( Procedure* m)
    {
        out << ws() << "local function " << toName(m);

        m->d_type->accept(this);
        out << endl;

        level++;
        for( int i = 0; i < m->d_order.size(); i++ )
        {
            if( m->d_order[i]->getTag() != Thing::T_Parameter )
                m->d_order[i]->accept(this);
        }

        if( !m->d_body.isEmpty() )
            out << ws() << "-- BEGIN" << endl;
        curScope = m;
        for( int i = 0; i < m->d_body.size(); i++ )
            m->d_body[i]->accept(this);

        Type* td = m->d_type->derefed();
        Q_ASSERT( td->getTag() == Thing::T_ProcType );
        ProcType* p = static_cast<ProcType*>( td );
        if( p->d_return.isNull() )
        {
            int numOfVar = 0;
            // add return of VAR params to proper procedure
            for( int i = 0; i < p->d_formals.size(); i++ )
            {
                if( p->d_formals[i]->d_var )
                {
                    numOfVar++;
                    if( numOfVar == 1 )
                        out << ws() << "return " << toName(p->d_formals[i].data());
                    else if( numOfVar > 1 )
                        out << "," << toName(p->d_formals[i].data());
                }
            }
            out << endl;
        }

        curScope = 0;
        level--;
        out << ws() << "end" << endl;

        if( m->d_scope == mod && m->d_public )
            out << "module" << "." << toName(m) << " = " << toName(m) << endl;
    }

    void visit( Module* m )
    {
        curScope = 0;
        out << "-- Generated by " << qApp->applicationName() << " " << qApp->applicationVersion() <<
                " on " << QDateTime::currentDateTime().toString(Qt::ISODate) << endl << endl;

        out << "---------- MODULE " << m->d_name << " ----------" << endl;
        out << "local module = {}" << endl << endl;

        out << "local obnlj = require 'obnlj'" << endl;

        for( int i = 0; i < m->d_order.size(); i++ )
            m->d_order[i]->accept(this);

        curScope = m;
        for( int i = 0; i < m->d_body.size(); i++ )
            m->d_body[i]->accept(this);
        curScope = 0;

        out << "return module" << endl;
        out << "---------- END MODULE " << m->d_name << " ----------" << endl;
        out.flush();
    }

    void printRightPart( const TransformIndexFunctionToStats::IndexPred& ip, Expression* e )
    {
        if( ip.second )
        {
            Q_ASSERT( ip.second->getTag() == Thing::T_UnExpr || ip.second->getTag() == Thing::T_IdentSel
                      || ip.second->getTag() == Thing::T_CallExpr );
            UnExpr* u = static_cast<UnExpr*>( ip.second );
            Q_ASSERT( u->d_sub.data() == ip.first );
            Ref<Expression> holder = u->d_sub; // otherwise first is deleted when splitting
            u->d_sub = new IdentLeaf(); // temp value without contents
            e->accept(this);
            u->d_sub = holder;
        }
    }

    void printVarReturns( Expression* e )
    {
        if( e->getTag() != Thing::T_CallExpr )
            return;
        CallExpr* c = static_cast<CallExpr*>(e);
        ProcType* pt = c->getProcType();
        if( pt->isBuiltIn() )
            return;

        int count1 = 0, count2 = 0;
        for( int i = 0; i < c->d_actuals.size(); i++ )
        {
            if( pt->d_formals[i]->d_var )
            {
                TransformIndexFunctionToStats::IndexPred ip = TransformIndexFunctionToStats::getLastIndexOp(
                            c->d_actuals[i].data() );
                if( count1 > 0 || count2 > 0 )
                    out << ",";
                if( ip.first )
                {
                    out << "__t" << count1 << "[__i" << count1 << "+1]";
                    printRightPart( ip, c->d_actuals[i].data() );
                    count1++;
                }else
                {
                    c->d_actuals[i]->accept(this);
                    count2++;
                }
            }
        }
    }

    bool generateIndexVarStatements( Expression* e )
    {
        if( e->getTag() != Thing::T_CallExpr )
            return false;
        CallExpr* c = static_cast<CallExpr*>(e);
        ProcType* pt = c->getProcType();
        if( pt->isBuiltIn() )
            return false;

        int cur = 0;
        bool hasVar = false;
        for( int i = 0; i < c->d_actuals.size(); i++ )
        {
            if( pt->d_formals[i]->d_var )
            {
                TransformIndexFunctionToStats::IndexPred ip = TransformIndexFunctionToStats::getLastIndexOp(
                            c->d_actuals[i].data() );
                if( ip.first )
                {
                    out << ws() << "__t" << cur << " = ";
                    Ref<Expression> table = ip.first->d_lhs;
                    ip.first->d_lhs = 0;
                    table->accept(this);
                    ip.first->d_lhs = table;
                    out << endl;
                    out << ws() << "__i" << cur << " = ";
                    ip.first->d_rhs->accept(this);
                    out << endl;
                    cur++;
                }
                hasVar = true;
            }
        }
        return hasVar;
    }

    void visit( Call* c )
    {
        CallExpr* ce = c->getCallExpr();
        if( ce->getProcType()->isBuiltIn() && renderBuiltIn( ce ) )
            return;

        const bool hasVar = generateIndexVarStatements(c->d_what.data());
        out << ws();
        if( hasVar )
        {
            printVarReturns(c->d_what.data());
            out << " = ";
        }
        c->d_what->accept(this);
        out << endl;
    }

    void visit( Return* r )
    {
        out << ws() << "return ";
        r->d_what->accept(this);
        Q_ASSERT( curScope->getTag() == Thing::T_Procedure && curScope->d_type->getTag() == Thing::T_ProcType );
        ProcType* p = static_cast<ProcType*>( curScope->d_type.data() );
        for( int i = 0; i < p->d_formals.size(); i++ )
            if( p->d_formals[i]->d_var )
                out << "," << toName(p->d_formals[i].data());
        out << endl;
    }

    static inline bool isArrayOfChar( Type* t )
    {
        Type* td = t ? t->derefed() : 0;
        if( td && td->getTag() == Thing::T_Array )
        {
            Array* a = static_cast<Array*>(td);
            Type* at = a->d_type->derefed();
            if( at->getTag() == Thing::T_BaseType &&
                    static_cast<BaseType*>( at )->d_type == BaseType::CHAR )
                return true;
        }
        return false;
    }

    void visit( Assign* a )
    {
        out << ws();
        const bool hasVar = generateIndexVarStatements(a->d_rhs.data());

        a->d_lhs->accept(this);
        if( hasVar )
        {
            out << ",";
            printVarReturns(a->d_rhs.data());
        }
        if( isArrayOfChar( a->d_lhs->d_type.data() ) )
        {
            Q_ASSERT( !hasVar );
            out << ":assig( ";
            a->d_rhs->accept(this);
            out << " )";
        }else
        {
            out << " = ";
            a->d_rhs->accept(this);
        }

        out << endl;
    }

    void renderIf( IfLoop* l )
    {
        out << ws() << "if ";
        l->d_if.first()->accept(this);
        out << " then " << endl;
        level++;
        for( int i = 0; i < l->d_then.first().size(); i++ )
            l->d_then.first()[i]->accept(this);
        level--;

        for( int i = 1; i < l->d_if.size(); i++ )
        {
            out << ws() << "elseif ";
            l->d_if[i]->accept(this);
            out << " then" << endl;
            level++;
            for( int j = 0; j < l->d_then[i].size(); j++ )
                l->d_then[i][j]->accept(this);
            level--;
        }

        if( !l->d_else.isEmpty() )
        {
            out << ws() << "else" << endl;
            level++;
            for( int j = 0; j < l->d_else.size(); j++ )
                l->d_else[j]->accept(this);
            level--;
        }
        out << ws() << "end" << endl;
    }

    void renderWhile( IfLoop* l )
    {
        out << ws() << "while true do" << endl;
        level++;
        out << ws() << "if ";
        l->d_if.first()->accept(this);
        out << " then" << endl;
        level++;
        for( int i = 0; i < l->d_then.first().size(); i++ )
            l->d_then.first()[i]->accept(this);
        level--;
        for( int i = 1; i < l->d_if.size(); i++ )
        {
            out << ws() << "elseif ";
            l->d_if[i]->accept(this);
            out << " then" << endl;
            level++;
            for( int j = 0; j < l->d_then[i].size(); j++ )
                l->d_then[i][j]->accept(this);
            level--;
        }

        out << ws() << "else" << endl;
        level++;
        out << ws() << "break" << endl;
        level--;
        out << ws() << "end" << endl;
        level--;
        out << ws() << "end" << endl;
    }

    void renderRepeat( IfLoop* l )
    {
        out << ws() << "repeat " << endl;
        level++;
        for( int i = 0; i < l->d_then.first().size(); i++ )
            l->d_then.first()[i]->accept(this);
        level--;
        out << ws() << "until ";
        l->d_if.first()->accept(this);
        out << endl;
    }

    void visit( IfLoop* l )
    {
        Q_ASSERT( !l->d_if.isEmpty() && l->d_then.size() == l->d_if.size() );
        switch( l->d_op )
        {
        case IfLoop::IF:
            renderIf(l);
            break;
        case IfLoop::WHILE:
            renderWhile(l);
            break;
        case IfLoop::REPEAT:
            renderRepeat(l);
            break;
        }
    }

    void visit( ForLoop* l )
    {
        // the same as while because in Lua the TO expression is only executed once
        out << ws() << toName(l->d_id.data()) << " = ";
        l->d_from->accept(this);
        out << endl;

        const int inc = l->d_byVal.toInt();
        out << ws() << "while " << toName(l->d_id.data());
        if( inc > 0 )
            out << " <= ";
        else
            out << " >= ";
        l->d_to->accept(this);
        out << " do" << endl;
        level++;
        for( int i = 0; i < l->d_do.size(); i++ )
            l->d_do[i]->accept(this);
        out << ws() << toName(l->d_id.data()) << " = " <<
               toName(l->d_id.data()) << " + (" << inc << ")" << endl;
        level--;
        out << ws() << "end" << endl;

    }

    void renderTypeCase( CaseStmt* cs )
    {
        int n = 0;
        for( int i = 0; i < cs->d_cases.size(); i++ )
        {
            const CaseStmt::Case& c = cs->d_cases[i];

            out << ws() << ( n == 0 ? "if " : "elseif ");
            out << "obnlj.is_a( ";
            cs->d_exp->accept(this);
            out << ", ";

            Q_ASSERT( c.d_labels.size() == 1 );

            out << className(c.d_labels.first()->d_type.data());

            out << " ) then" << endl;

            level++;
            for( int j = 0; j < c.d_block.size(); j++ )
                c.d_block[j]->accept(this);
            level--;
            n++;
        }
        if( n )
            out << ws() << "end" << endl;
    }

    void visit( CaseStmt* cs )
    {
        if( cs->d_typeCase )
        {
            renderTypeCase(cs);
            return;
        }

        int n = 0;
        for( int i = 0; i < cs->d_cases.size(); i++ )
        {
            const CaseStmt::Case& c = cs->d_cases[i];

            out << ws() << ( n == 0 ? "if " : "elseif ");

            for( int j = 0; j < c.d_labels.size(); j++ )
            {
                if( j != 0 )
                    out << " or ";
                Expression* l = c.d_labels[j].data();
                // TODO: avoid calling cs->d_exp more than once by using temp var
                if( l->getTag() == Thing::T_BinExpr )
                {
                    BinExpr* bi = static_cast<BinExpr*>( l );
                    Q_ASSERT( bi->d_op == BinExpr::Range );

                    cs->d_exp->accept(this);
                    out << " >= ";
                    bi->d_lhs->accept(this);
                    out << " and ";
                    cs->d_exp->accept(this);
                    out << " <= ";
                    bi->d_rhs->accept(this);
                }else
                {
                    cs->d_exp->accept(this);
                    out << " == ";
                    l->accept(this);
                }
            }
            out << " then" << endl;
            level++;
            for( int j = 0; j < c.d_block.size(); j++ )
                c.d_block[j]->accept(this);
            level--;
            n++;
        }
        if( n )
            out << ws() << "end" << endl;
    }

    void visit( Literal* l)
    {
        print( l->d_val );
    }

    void visit( SetExpr* s )
    {
        out << "obnlj.SET(";
        for( int i = 0; i < s->d_parts.size(); i++ )
        {
            if( i != 0 )
                out << ",";
            if( s->d_parts[i]->getTag() == Thing::T_BinExpr &&
                    static_cast<BinExpr*>( s->d_parts[i].data() )->d_op == BinExpr::Range )
            {
                BinExpr* bi = static_cast<BinExpr*>( s->d_parts[i].data() );
                bi->d_lhs->accept(this);
                out << ",";
                bi->d_rhs->accept(this);
            }else
            {
                s->d_parts[i]->accept(this);
                out << ",-1";
            }
        }
        out << ")";
    }

    void visit( IdentLeaf* id )
    {
        // may be null because of printRightPart which temporarily adds a dummy leaf
        if( !id->d_ident.isNull() )
            out << toName(id->d_ident.data());
    }

    void visit( UnExpr* e )
    {
        bool par = false;
        switch( e->d_op )
        {
        case UnExpr::NOT:
            out << "not ";
            par = true;
            break;
        case UnExpr::NEG:
            out << "-";
            break;
        }
        if( par )
            out << "( ";
        e->d_sub->accept(this);
        if( par )
            out << " )";
    }

    void visit( IdentSel* e )
    {
        e->d_sub->accept(this);
        Q_ASSERT( !e->d_ident.isNull() );
        out << "." << toName( e->d_ident.data() );
        if( e->d_ident->getTag() == Thing::T_Variable && e->d_ident->d_scope != mod )
            out << "()";
    }

    static Record* toRecord( Type* t )
    {
        return Model::toRecord(t);
    }

    bool renderBuiltIn( CallExpr* c )
    {
        ProcType* pt = c->getProcType();
        Q_ASSERT( pt->d_ident && pt->d_ident->getTag() == Thing::T_BuiltIn );
        BuiltIn* bi = static_cast<BuiltIn*>( pt->d_ident );

        switch( bi->d_func )
        {
        // Statements:
        case BuiltIn::NEW:
            Q_ASSERT( c->d_actuals.size() == 1 );
            out << ws() << "__tmp1 = {}" << endl;
            out << ws();
            c->d_actuals.first()->accept(this);
            out << " = __tmp1" << endl;
            initRecord( c->d_actuals.first()->d_type.data(), "__tmp1" );
            return true;
        case BuiltIn::INC:
        case BuiltIn::DEC:
            out << ws();
            if( c->d_actuals.size() == 1 )
            {
                c->d_actuals.first()->accept(this);
                out << " = "; // TODO indexed VAR param handling, mittels __tmp
                c->d_actuals.first()->accept(this);
                if( bi->d_func == BuiltIn::INC )
                    out << " + 1";
                else
                    out << " - 1";
            }else
            {
                Q_ASSERT( c->d_actuals.size() == 2 );
                c->d_actuals.first()->accept(this);
                out << " = "; // TODO indexed VAR param handling
                c->d_actuals.first()->accept(this);
                if( bi->d_func == BuiltIn::INC )
                    out << " + ";
                else
                    out << " - ";
                c->d_actuals.last()->accept(this);
            }
            out << endl;
            return true;
        case BuiltIn::ASSERT:
            Q_ASSERT( c->d_actuals.size() == 1 );
            out << ws() << "obnlj.ASSERT(";
            c->d_actuals.first()->accept(this);
            out << ",\"" << mod->d_name << "\"," << c->d_loc.d_row << ")" << endl;
            return true;
        case BuiltIn::INCL:
            Q_ASSERT( c->d_actuals.size() == 2 );
            out << ws() << "obnlj.INCL(";
            c->d_actuals.first()->accept(this);
            out << ",";
            c->d_actuals.last()->accept(this);
            out << ")" << endl;
            return true;
        case BuiltIn::EXCL:
            Q_ASSERT( c->d_actuals.size() == 2 );
            out << ws() << "obnlj.EXCL(";
            c->d_actuals.first()->accept(this);
            out << ",";
            c->d_actuals.last()->accept(this);
            out << ")" << endl;
            return true;
        case BuiltIn::PACK:
            Q_ASSERT( c->d_actuals.size() == 2 );
            out << ws();
            c->d_actuals.first()->accept(this);
            out << "= obnljlib.PACK( "; // TODO indexed VAR param handling
            c->d_actuals.first()->accept(this);
            out << ", ";
            c->d_actuals.last()->accept(this);
            out << " )";
            out << endl;
            return true;
        case BuiltIn::UNPK:
            Q_ASSERT( c->d_actuals.size() == 2 );
            out << ws();
            c->d_actuals.first()->accept(this);
            out << ",";
            c->d_actuals.last()->accept(this);
            out << "= obnljlib.UNPK( "; // TODO indexed VAR param handling
            c->d_actuals.first()->accept(this);
            out << ", ";
            c->d_actuals.last()->accept(this);
            out << " )";
            out << endl;
            return true;
        case BuiltIn::WriteChar:
            Q_ASSERT( c->d_actuals.size() == 1 );
            out << "Out.Char(";
            c->d_actuals.first()->accept(this);
            out << ")";
            return true;
        case BuiltIn::WriteInt:
            Q_ASSERT( c->d_actuals.size() == 1 );
            out << "Out.Int(";
            c->d_actuals.first()->accept(this);
            out << ",4)";
            return true;
        case BuiltIn::WriteReal:
            Q_ASSERT( c->d_actuals.size() == 1 );
            out << "Out.Real(";
            c->d_actuals.first()->accept(this);
            out << ",0)";
            return true;
        case BuiltIn::WriteLn:
            Q_ASSERT( c->d_actuals.size() == 0 );
            out << "Out.Ln()";
            return true;

        // Expressions:
        case BuiltIn::ORD:
            Q_ASSERT( c->d_actuals.size() == 1 );
            out << "obnlj.ORD( ";
            c->d_actuals.first()->accept(this);
            out << " )";
            return true;
        case BuiltIn::CHR:
            Q_ASSERT( c->d_actuals.size() == 1 );
            out << "obnlj.Char( ";
            c->d_actuals.first()->accept(this);
            out << " )";
            return true;
        case BuiltIn::ODD:
            Q_ASSERT( c->d_actuals.size() == 1 );
            c->d_actuals.first()->accept(this);
            out << " % 2 == 1";
            return true;
        case BuiltIn::ABS:
            Q_ASSERT( c->d_actuals.size() == 1 );
            out << "math.abs( ";
            c->d_actuals.first()->accept(this);
            out << " )";
            return true;
        case BuiltIn::LEN:
            Q_ASSERT( c->d_actuals.size() == 1 );
            c->d_actuals.first()->accept(this);
            out << ".n ";
            return true;
        case BuiltIn::LSL:
            Q_ASSERT( c->d_actuals.size() == 2 );
            out << "bit.lshift(";
            c->d_actuals.first()->accept(this);
            out << ",";
            c->d_actuals.last()->accept(this);
            out << ")";
            return true;
        case BuiltIn::ASR:
            Q_ASSERT( c->d_actuals.size() == 2 );
            out << "bit.arshift(";
            c->d_actuals.first()->accept(this);
            out << ",";
            c->d_actuals.last()->accept(this);
            out << ")";
            return true;
        case BuiltIn::ROR:
            Q_ASSERT( c->d_actuals.size() == 2 );
            out << "bit.ror(";
            c->d_actuals.first()->accept(this);
            out << ",";
            c->d_actuals.last()->accept(this);
            out << ")";
            return true;
        case BuiltIn::FLOOR:
            Q_ASSERT( c->d_actuals.size() == 1 );
            out << "math.floor(";
            c->d_actuals.first()->accept(this);
            out << ")";
            return true;
        case BuiltIn::FLT:
            Q_ASSERT( c->d_actuals.size() == 1 );
            c->d_actuals.first()->accept(this);
            return true;

        // Not supported:
        case BuiltIn::LED:
        case BuiltIn::ADR:
        case BuiltIn::BIT:
        case BuiltIn::GET:
        case BuiltIn::H:
        case BuiltIn::LDREG:
        case BuiltIn::PUT:
        case BuiltIn::REG:
        case BuiltIn::VAL:
        case BuiltIn::COPY:
            qWarning() << "SYSTEM." << BuiltIn::s_typeName[bi->d_func] << "not supported by code generator";
            return false;
        }

        return false;
    }

    void visit( CallExpr* c )
    {
        ProcType* pt = c->getProcType();

        if( pt->isBuiltIn() && renderBuiltIn( c ) )
            return;

        c->d_sub->accept(this);
        out << "(";
        int cur = 0;
        for( int i = 0; i < c->d_actuals.size(); i++ )
        {
            if( i != 0 )
                out << ",";

            if( i < pt->d_formals.size() && pt->d_formals[i]->d_var )
            {
                TransformIndexFunctionToStats::IndexPred ip = TransformIndexFunctionToStats::getLastIndexOp(
                            c->d_actuals[i].data() );
                if( ip.first )
                {
                    out << "__t" << cur << "[__i" << cur << "+1]";
                    printRightPart( ip, c->d_actuals[i].data() );
                    cur++;
                }else
                    c->d_actuals[i]->accept(this);
            }else
                c->d_actuals[i]->accept(this);
        }
        out << ")";
    }

    void visit( BinExpr* e )
    {
        if( e->d_op == BinExpr::IN )
        {
            out << "obnlj.IN(";
            e->d_lhs->accept(this);
            out << ", ";
            e->d_rhs->accept(this);
            out << " )";
        }else if( e->d_op == BinExpr::IS )
        {
            out << "obnlj.is_a( ";
            e->d_lhs->accept(this);
            out << ", ";
            out << className(e->d_rhs->d_type.data());
            out << " ) ";
        }else if( e->d_op == BinExpr::MOD )
        {
            out << "obnlj.MOD(";
            e->d_lhs->accept(this);
            out << ",";
            e->d_rhs->accept(this);
            out << ")";
        }else if( e->d_op == BinExpr::DIV )
        {
            out << "obnlj.DIV(";
            e->d_lhs->accept(this);
            out << ",";
            e->d_rhs->accept(this);
            out << ")";
        }else if( e->d_op == BinExpr::Index )
        {
            e->d_lhs->accept(this);
            out << "[";
            e->d_rhs->accept(this);
            out << "+1]"; // index is alwas integer
        }else
        {
            e->d_lhs->accept(this);
            switch( e->d_op )
            {
            case BinExpr::ADD:
                out << " + ";
                break;
            case BinExpr::SUB:
                out << " - ";
                break;
            case BinExpr::OR:
                out << " or ";
                break;
            case BinExpr::MUL:
                out << " * ";
                break;
            case BinExpr::FDIV:
                out << " / ";
                break;
            case BinExpr::AND:
                out << " and ";
                break;
            case BinExpr::EQ:
                out << " == ";
                break;
            case BinExpr::NEQ:
                out << " ~= ";
                break;
            case BinExpr::LT:
                out << " < ";
                break;
            case BinExpr::LEQ:
                out << " <= ";
                break;
            case BinExpr::GT:
                out << " > ";
                break;
            case BinExpr::GEQ:
                out << " >= ";
                break;
            default:
                out << " ??? ";
                break;
            }
            e->d_rhs->accept(this);
        }
    }
};

QByteArray LuaGen2::toName(Named* id)
{
    return ::toName(id);
}

bool LuaGen2::translate(Ast::Model* mdl, const QString& outdir, const QString& mod, Errors* err)
{
    Q_ASSERT( mdl );

    QDir dir(outdir);
    if( !mod.isEmpty() )
    {
        dir.mkpath( mod );
        dir.cd( mod );
    }

    int errs = 0;
    Ob::Ast::Model::Modules mods = mdl->getModules();
    for( int i = 0; i < mods.size(); i++ )
    {
        if( mods[i]->d_isDef )
            continue;

        QFile out( dir.absoluteFilePath( toName(mods[i].data()) + ".lua" ) );
        if( !out.open(QIODevice::WriteOnly) )
        {
            errs++;
            if( err )
                err->error(Errors::Generator,toName(mods[i].data()), 0,0,QString("cannot open file '%1' for writing").
                       arg(out.fileName()) );
        }else
        {
            if( !Ob::LuaGen2::translate(mods[i].data(),&out,err) )
                errs++;
        }
    }
    return errs == 0;
}

bool LuaGen2::translate(Module* m, QIODevice* out, Errors* errs )
{
    Q_ASSERT( m != 0 && out != 0 );

    if( m->d_hasErrors )
        return false;

    if( m->d_isDef )
        return true;

    TransformIndexFunctionToStats trans;
    trans.curModule = m;
    m->accept(&trans);

    /*
    QTextStream ts(stdout);
    Eval::render( ts, m );
    ts.flush();
    */

    LuaGen2Imp imp;

    imp.level = 0;
    imp.mod = m;

    if( errs == 0 )
    {
        imp.err = new Errors();
        imp.err->setReportToConsole(true);
        imp.ownsErr = true;
    }else
    {
        imp.err = errs;
        imp.ownsErr = false;
    }
    const quint32 errCount = imp.err->getErrCount();

    imp.out.setDevice(out);

    m->accept(&imp);

    const bool hasErrs = ( imp.err->getErrCount() - errCount ) != 0;

    if( imp.ownsErr )
        delete imp.err;
    return hasErrs;
}
