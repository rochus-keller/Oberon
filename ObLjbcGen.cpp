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

#include "ObLjbcGen.h"
#include "ObAst.h"
#include "ObErrors.h"
#include "ObAstEval.h"
#include "ObLuaGen2.h"
#include "ObLexer.h"
#include <LjTools/LuaJitComposer.h>
#include <QBitArray>
#include <QDir>
#include <QFile>
#include <QtDebug>
using namespace Ob;
using namespace Ob::Ast;
using namespace Lua;

struct LjbcGenImp : public AstVisitor
{
    struct NoMoreFreeSlots {};

    struct Ctx
    {
        Scope* scope;
        JitComposer::SlotPool pool;
        typedef QHash<Named*,quint16> Upvals;
        Upvals upvals;
        quint8 frameSize;
        bool returnFound;
        Ctx(Scope* s = 0):frameSize(0),returnFound(false),scope(s) { }
        QList<quint8> callSlots; // stack of call bases

        int startFrom() const
        {
            if( callSlots.isEmpty() )
                return -1;
            else
                return callSlots.back();
        }

        int buySlots(int len = 1, bool call = false )
        {
            const int tmp = JitComposer::nextFreeSlot(pool,len, startFrom() );
            if( tmp < 0 )
                throw NoMoreFreeSlots();
            // qDebug() << "buy" << tmp << len;
            const int max = tmp + len;
            if( max > int(frameSize) )
                frameSize = max;
            if( call )
                // in case of a call take care that slots used to evaluate the arguments are higher than
                // the allocated call base; otherwise it could happen that an argument executes yet another
                // call to which a base lower than the waiting one is allocated resulting in random modifications
                // of the waiting call base;
                callSlots.push_back(tmp);
            return tmp;
        }
        void sellSlots(quint8 base, int len = 1 )
        {
            // qDebug() << "sell" << base << len;
            JitComposer::releaseSlot(pool,base, len );
            callSlots.removeAll(base);
        }
        quint16 resolveUpval(Named* n)
        {
            Upvals::const_iterator i = upvals.find(n);
            if( i != upvals.end() )
                return i.value();
            const int nr = upvals.size();
            upvals[ n ] = nr;
            return nr;
        }
    };
    QList<Ctx> ctx;

    JitComposer bc;
    Errors* err;
    Module* mod;
    quint8 modSlot; // NOTE: never accessed from sub proc, so no uv management needed
    bool ownsErr;
    typedef QHash<Scope*,Import*> Imps;
    Imps d_imps;
    Ref<Import> obnlj;

    struct Value
    {
        enum { None, Ref, Tmp, Uv, Ref2, Tmp2, Ref2v, Tmp2v, Pre, Val, Jump, Sold };

        uint d_slot : 16;  // Ref, Tmp, Pre, Uv, Ref2, Tmp2
        uint d_idx : 8;   // Ref2, Tmp2
        uint d_kind : 4;
        uint d_isVarParam : 1;
        uint d_isParam : 1;
        uint notUsed : 2;
        QVariant d_val; // Val, Tmp2v, Ref2v
        QList<quint32> trueList, falseList; // Jump
        Type* d_type;  // optional, for special assignments
        int d_line;

        Value():d_slot(0),d_idx(0),d_kind(None),d_line(-1),d_type(0),d_isVarParam(0),d_isParam(0),notUsed(0){}
        bool isVar() const { return d_kind > None && d_kind < Val; }
        bool isConst() const { return d_kind == Val; }

        Value( const Value& rhs ) { *this = rhs; }
        Value& operator=( const Value& )
        {
            qWarning() << "dont copy Value";
            return *this;
        }
    };

    void sell( Value& v )
    {
        switch( v.d_kind )
        {
        case Value::Sold:
            qWarning() << "try to double sell slot" << v.d_slot;
            break;
        case Value::Tmp:
        case Value::Tmp2v:
            ctx.back().sellSlots( v.d_slot );
            v.d_kind = Value::Sold;
            break;
        case Value::Tmp2:
            ctx.back().sellSlots( v.d_slot );
            ctx.back().sellSlots( v.d_idx );
            v.d_kind = Value::Sold;
            break;
        }
    }

    Named* findClass( Type* t )
    {
        if( t->getTag() == Thing::T_Pointer )
        {
            Pointer* p = static_cast<Pointer*>(t);
            if( p->d_to->d_ident != 0)
                t = p->d_to.data();
        }

        Named* ident = t->d_ident;
        if( ident == 0 )
        {
            Type* td = t->derefed();
            if( td->getTag() == Thing::T_Record )
            {
                Record* r = static_cast<Record*>( td );
                if( r->d_binding )
                    ident = r->d_binding->d_ident;
            }
        }
        return ident;
    }

    void fetchClass( Type* t, Value& out )
    {
        quint8 to;
        if( out.d_kind == Value::Pre )
            to = out.d_slot;
        else
        {
            out.d_slot = ctx.back().buySlots(1);
            out.d_kind = Value::Tmp;
            to = out.d_slot;
        }

        // find
        if( t->getTag() == Thing::T_Pointer )
        {
            Pointer* p = static_cast<Pointer*>(t);
            if( p->d_to->d_ident != 0)
                t = p->d_to.data();
        }

        Named* ident = findClass(t);
        Q_ASSERT( ident != 0 );

        if( Model::isXInModule(mod,ident->d_scope) )
        {
            if( t->getTag() == Thing::T_TypeRef )
            {
                // the TypeRef is local in this module and points to a name which we expect in the
                // imported modules table
                TypeRef* tr = static_cast<TypeRef*>( t );
                Q_ASSERT( tr->d_ref->d_ident != 0 );
                Import* i = d_imps.value( tr->d_ref->d_ident->d_scope );
                Q_ASSERT( i != 0 && i->d_slotValid );
                bc.TGET(to,i->d_slot, QVariant::fromValue(tr->d_ref->d_ident->d_name), out.d_line );
            }else
            {
                // just any locally defined type
                Q_ASSERT( ident && ident->d_slotValid );

                if( ctx.back().scope != ident->d_scope )
                {
                    // resolve upvalue
                    bc.UGET( to, resolveUpval( ident ), out.d_line );
                }else
                {
                    // Module level
                    bc.MOV(to,ident->d_slot,out.d_line);
                }

            }
        }else
        {
            // the type is declared in an imported module; we expect the module to be present in
            // our import table (otherwise the validator would have complained)
            Import* i = d_imps.value( ident->d_scope );
            Q_ASSERT( i != 0 && i->d_slotValid );
            bc.TGET(to,i->d_slot, QVariant::fromValue(ident->d_name), out.d_line );
        }
    }

    void visit( NamedType* t )
    {
        if( isNamedTypeWithSlot(t) && ( isLive(t) || isDurable(t) ) )
        {
            Q_ASSERT( t->d_slotValid );
            bc.TNEW( t->d_slot, 0, 0, t->d_loc.d_row );

            // export it if public by copy to module table
            if( t->d_scope == mod && t->d_public )
                bc.TSET( t->d_slot, modSlot, QVariant::fromValue(t->d_name), t->d_loc.d_row );

            Record* r = Model::toRecord( t->d_type.data() );
            if( r->d_base )
            {
                quint8 base = ctx.back().buySlots(3,true);
                bc.GGET(base, "setmetatable", t->d_loc.d_row );
                bc.MOV(base+1,t->d_slot,t->d_loc.d_row );
                Value v;
                v.d_slot = base+2;
                v.d_kind = Value::Pre;
                v.d_line = t->d_loc.d_row;
                fetchClass(r->d_base,v);
                bc.CALL(base,0, 2, v.d_line);
                ctx.back().sellSlots(base,3);
            }
        }else if( Model::toRecord(t->d_type.data()) != 0 && t->d_scope == mod && t->d_public )
        {
            Value v;
            v.d_line = t->d_loc.d_row;
            fetchClass(t->d_type.data(),v);
            bc.TSET( v.d_slot, modSlot, QVariant::fromValue(t->d_name), t->d_loc.d_row );
            sell(v);
        }
    }

    quint16 resolveUpval( Named* n )
    {
        Q_ASSERT( n->d_scope != ctx.back().scope );
        // get the upval id from the present context
        const quint16 res = ctx.back().resolveUpval(n);
        bool foundHome = false;
        // now register the upval in all upper contexts too up to but not including the
        // one where the symbol was defined
        for( int i = ctx.size() - 2; i >= 0; i-- )
        {
            if( n->d_scope == ctx[i].scope )
            {
                foundHome = true;
                break;
            }else
                ctx[i].resolveUpval(n);
        }
        Q_ASSERT( foundHome );
        return res;
    }

    void genArray( int len, quint8 out, int line )
    {
        quint8 tmp = ctx.back().buySlots(2,true);
        fetchObnljMember(tmp,"Arr",line);
        bc.KSET(tmp+1, len,line );
        bc.CALL(tmp,1,1,line);
        bc.MOV(out,tmp,line);
        ctx.back().sellSlots(tmp,2);
    }

    void initMatrix( const QList<Array*>& dims, quint8 table, int curDim, int line )
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

        if( curDim == dims.size() - 1 )
        {
            // we're at the highest dimension
            if( isString( dims[curDim] ) )
            {
                // out << " = obnlj.Str(" << dims[curDim]->d_len << ")";
                quint8 tmp = ctx.back().buySlots(2,true);
                fetchObnljMember(tmp,"Str",line);
                bc.KSET(tmp+1, dims[curDim]->d_len, line );
                bc.CALL(tmp,1,1,line);
                bc.MOV(table,tmp,line);
                ctx.back().sellSlots(tmp,2);
            }else if( dims[curDim]->d_type->derefed()->getTag() == Thing::T_Record )
            {
                // initHelper( dims[curDim], curDim, name, rec );
                genArray( dims[curDim]->d_len, table, line );

                quint8 base = ctx.back().buySlots(4);
                quint8 elem = ctx.back().buySlots(1);
                bc.KSET(base,1,line);
                bc.KSET(base+1,dims[curDim]->d_len,line);
                bc.KSET(base+2,1,line);
                bc.FORI(base,0,line);
                const quint32 pc = bc.getCurPc();

                bc.TNEW( elem, 0, 0, line );
                initRecord( dims[curDim]->d_type.data(), elem, line );
                bc.TSET(elem,table,base+3, line);

                bc.FORL(base, pc - bc.getCurPc() - 1,line);
                bc.patch(pc,bc.getCurPc() - pc);

                ctx.back().sellSlots(elem);
                ctx.back().sellSlots(base,4);

            }else
                genArray( dims[curDim]->d_len, table, line );
                // out << " = obnlj.Arr(" << dims[curDim]->d_len << ")";
        }else
        {
            // we're at a lower dimension
            //initHelper( dims[curDim], curDim, table, line );
            genArray( dims[curDim]->d_len, table, line );

            quint8 base = ctx.back().buySlots(4);
            quint8 elem = ctx.back().buySlots(1);
            bc.KSET(base,1,line);
            bc.KSET(base+1,dims[curDim]->d_len,line);
            bc.KSET(base+2,1,line);
            bc.FORI(base,0,line);
            const quint32 pc = bc.getCurPc();

            initMatrix( dims, elem, curDim + 1, line );
            bc.TSET(elem,table,base+3, line);

            bc.FORL(base, pc - bc.getCurPc() - 1,line);
            bc.patch(pc,bc.getCurPc() - pc);

            ctx.back().sellSlots(elem);
            ctx.back().sellSlots(base,4);
        }
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

    void initArray(Array* arr, quint8 table, int line )
    {
        // table is the slot where the new array will be stored

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
        initMatrix( dims, table, 0, line );
    }

    void initRecord( Type* rt, quint8 table, int line )
    {
        Q_ASSERT( rt != 0 );
        // out << " = {}" << endl; was already done by the caller

        if( findClass(rt) )
        {
            quint8 base = ctx.back().buySlots(3,true);
            bc.GGET(base, "setmetatable", line );
            bc.MOV(base+1,table,line );
            Value v;
            v.d_slot = base+2;
            v.d_kind = Value::Pre;
            v.d_line = line;
            fetchClass(rt,v);
            bc.CALL(base,0, 2, v.d_line);
            ctx.back().sellSlots(base,3);
        }

        if( rt->getTag() == Thing::T_Pointer )
            rt = static_cast<Pointer*>(rt)->d_to.data();

        Q_ASSERT( rt->derefed()->getTag() == Thing::T_Record );

        Record* r = static_cast<Record*>( rt->derefed() );
        QList<Record*> topDown;
        topDown << r;

        Record* base = r->d_base ? static_cast<Record*>(r->d_base->derefed()) : 0;
        while( base )
        {
            topDown.prepend(base);
            base = base->d_base ? static_cast<Record*>(base->d_base->derefed()) : 0;
        }

        for( int j = 0; j < topDown.size(); j++ )
        {
            // go from top to bottom through inheritance hierarchy and initialize members
            Record* rec = topDown[j];
            for( int i = 0; i < rec->d_fields.size(); i++ )
            {
                Type* t = rec->d_fields[i]->d_type.data();
                Type* td = t->derefed();
                quint8 field = ctx.back().buySlots(1);
                if( td->getTag() == Thing::T_Record )
                {
                    // out << ws() << field << " = {}" << endl;
                    bc.TNEW( field, 0, 0, line );
                    bc.TSET(field, table, QVariant::fromValue(rec->d_fields[i]->d_name), line );
                    initRecord( t, field, line );
                }
                else if( td->getTag() == Thing::T_Array )
                {
                    // out << ws() << field;
                    initArray( static_cast<Array*>(td), field, line );
                    bc.TSET(field, table, QVariant::fromValue(rec->d_fields[i]->d_name), line );
                }
                ctx.back().sellSlots(field);
            }
        }
    }

    void emitVariable( Named* v )
    {
        if( !v->d_slotValid )
            return; // Var not used
        Q_ASSERT( v->d_slotValid );

        if( v->d_type->getTag() == Thing::T_Record )
        {
            // name = {}
            bc.TNEW( v->d_slot, 0, 0, v->d_loc.d_row );
            initRecord( v->d_type.data(), v->d_slot, v->d_loc.d_row );
        }else if( v->d_type->getTag() == Thing::T_Array )
        {
            initArray( static_cast<Array*>( v->d_type.data() ), v->d_slot, v->d_loc.d_row );
        }else
        {
            // unneccessary, already nil: bc.KNIL(v->d_slot, 1, v->d_loc.d_row );
        }

        if( v->d_scope == mod && v->d_public )
        {
            if( v->d_type->isStructured() )
            {
                bc.TSET( v->d_slot, modSlot, QVariant::fromValue(v->d_name), v->d_loc.d_row );
            }else
            {
                //    out << "module" << "." << name << " = function() return " << name << " end" << endl;
                const int tmp = ctx.back().buySlots(1);
                const int func = bc.openFunction(0,v->d_name,v->d_loc.d_row, v->d_loc.d_row );
                bc.UGET(0,0, v->d_loc.d_row);
                bc.RET(0,1, v->d_loc.d_row);
                JitComposer::Upval uv;
                uv.d_isLocal = true;
                uv.d_isRo = true;
                uv.d_name = v->d_name;
                Q_ASSERT( v->d_slotValid );
                uv.d_uv = v->d_slot;
                bc.setUpvals(JitComposer::UpvalList() << uv);
                bc.closeFunction(1);
                bc.FNEW(tmp,func,v->d_loc.d_row);
                bc.TSET( tmp,modSlot,QVariant::fromValue(v->d_name), v->d_loc.d_row );
                ctx.back().sellSlots(tmp);
            }
        }
    }

    void visit( Variable* v )
    {
        emitVariable(v);
    }

    void visit( LocalVar* v )
    {
        emitVariable(v);
    }

    int emitImport( const QByteArray& modName, int line )
    {
        int tmp = ctx.back().buySlots(2,true);
        bc.GGET( tmp, "require", line );
        bc.KSET( tmp+1, modName, line );
        bc.CALL( tmp, 1, 1, line );
        ctx.back().sellSlots(tmp+1); // keep tmp+0 as fixed import slot, release the other
        return tmp;
    }

    void visit( Import* i )
    {
        // local imported = require 'module'
        i->d_slot = emitImport( i->d_mod.isNull() ? i->d_name : i->d_mod->d_name, i->d_loc.d_row );
        i->d_slotValid = true;
        d_imps.insert(i->d_mod.data(), i );
    }

    bool inline isDurable( Named* n )
    {
        return n->d_usedFromSubs || n->d_usedFromLive || ( n->d_scope == mod && n->d_public );
    }

    bool inline isLive( Named* n )
    {
        return n->d_liveFrom > 0;
    }

    bool inline isNamedTypeWithSlot( Named* nt )
    {
        if( nt->getTag() != Thing::T_NamedType )
            return false;

        // we only consider original named type declarations here, i.e. no aliasses
        if( nt->d_type->d_ident != nt )
            return false;

        const int tag = nt->d_type->getTag();

        // In case of type aliasses which point to record types (wheter in this or another module), we at least
        // put a copy of the original named type table to the module table of the present module,
        // but we don't allocate a new slot.
        if( tag == Thing::T_TypeRef )
            return false;

        // We need a table for a named record type so it can be used as metatable for the
        // instance of the record and a base for subrecords.
        if( tag == Thing::T_Record )
            return true;

        if( tag == Thing::T_Pointer )
        {
            Pointer* p = static_cast<Pointer*>(nt->d_type.data());
            Q_ASSERT( p->d_to->getTag() == Thing::T_Record );
            Record* r = static_cast<Record*>(p->d_to.data());

            // A pointer to an anonymous record declared for the pointer is treatet the
            // same way as a named Record declaration
            if( r->d_binding == p )
                return true;
        }

        return false;
    }

    bool allocateLocals( Scope* s )
    {
        JitComposer::Intervals vals;
        Ctx& c = ctx.back();
        for( int i = 0; i < s->d_order.size(); i++ )
        {
            const int tag = s->d_order[i]->getTag();
            if( tag == Thing::T_Parameter )
            {
                Parameter* p = static_cast<Parameter*>( s->d_order[i] );
                p->d_slot = c.buySlots();
                p->d_slotValid = true;
            }else if( tag == Thing::T_Variable || tag == Thing::T_LocalVar || tag == Thing::T_Procedure ||
                      isNamedTypeWithSlot( s->d_order[i] ) )
            {
                Named* n = s->d_order[i];
                const bool durable = isDurable(n);
                if( isLive(n) || durable )
                {
                    // public module vars need a slot even if not used in module statement sequence
                    if( true ) // if( durable )
                        // NOTE: we don't reuse slots at the moment because it has a significant impact
                        // on initialization complexity which requires further investigation.
                        // So currently there is a dedicated slot for each symbol which is either
                        // effectively in use in the current scope or (potentially) used in other scopes.
                    {
                        const int slot = c.buySlots();
                        if( slot < 0 )
                            return error( s->d_loc, QString("out of free slots") );
                        n->d_slot = slot;
                        n->d_slotValid = true;
                    }else
                        vals << JitComposer::Interval( n->d_liveFrom, n->d_liveTo, n );
                }
            }
        }
        if( JitComposer::allocateWithLinearScan(c.pool,vals,1) )
        {
            foreach( const JitComposer::Interval& val, vals )
            {
                Named* rb = static_cast<Named*>( val.d_payload );
                rb->d_slot = val.d_slot;
                rb->d_slotValid = true;
            }
            const int max = JitComposer::highestUsedSlot(c.pool) + 1;
            if( !vals.isEmpty() && max > c.frameSize )
                c.frameSize = max;
        }else
            return error( s->d_loc, QString("out of free slots") );
        return true;
    }

    bool inline error( const Loc& l, const QString& msg )
    {
        err->error(Errors::Semantics, mod->d_file, l.d_row, l.d_col, msg );
        return false;
    }

    void visit( Procedure* p)
    {
        if( !p->d_slotValid )
            return; // nobody seems to use this function

        ctx.push_back( Ctx(p) );
        const int id = bc.openFunction(static_cast<ProcType*>(p->d_type.data())->d_formals.size(),
                        p->d_name,p->d_loc.d_row, p->d_end.d_row );
        Q_ASSERT( id >= 0 );

        allocateLocals(p);

        if( p->d_type->d_ident == 0 )
            p->d_type->accept(this);

        for( int i = 0; i < p->d_order.size(); i++ )
            p->d_order[i]->accept(this);

        for( int i = 0; i < p->d_body.size(); i++ )
            p->d_body[i]->accept(this);

        if( !ctx.back().returnFound )
        {
            Q_ASSERT( p->d_type->getTag() == Thing::T_ProcType );
            ProcType* pt = static_cast<ProcType*>( p->d_type.data() );

            QList<quint8> vars;
            for( int i = 0; i < pt->d_formals.size(); i++ )
            {
                if( pt->d_formals[i]->d_var )
                    vars.append(i);
            }
            if( vars.isEmpty() )
                bc.RET(p->d_end.d_row);
            else
            {
                quint8 tmp = ctx.back().buySlots(vars.size());
                for( int i = 0; i < vars.size(); i++ )
                    bc.MOV(tmp+i,vars[i],p->d_end.d_row);
                bc.RET(tmp,vars.size(),p->d_end.d_row);
                ctx.back().sellSlots(tmp);
            }
        }

        JitComposer::VarNameList sn = getSlotNames(p);
        bc.setVarNames( sn );
        bc.setUpvals( getUpvals() );
        bc.closeFunction(ctx.back().frameSize);
        ctx.pop_back();
        Q_ASSERT( p->d_slotValid );
        bc.FNEW( p->d_slot, id, p->d_end.d_row );
        if( p->d_scope == mod && p->d_public )
            bc.TSET( p->d_slot, modSlot, QVariant::fromValue(p->d_name), p->d_end.d_row );
    }

    JitComposer::VarNameList getSlotNames( Scope* m )
    {
        JitComposer::VarNameList vnl(ctx.back().frameSize);
        for( int i = 0; i < m->d_order.size(); i++ )
        {
            if( m->d_order[i]->d_slotValid )
            {
                Named* n = m->d_order[i];
                JitComposer::VarName& vn = vnl[n->d_slot];
                Q_ASSERT( vn.d_name.isEmpty() );
                vn.d_name = n->d_name;
                // we currently don't reuse slots, so they're valid over the whole body
                vn.d_from = 0; // n->d_liveFrom;
                vn.d_to = bc.getCurPc(); // n->d_liveTo;
            }
        }
        for( int i = 0; i < vnl.size(); i++ )
        {
            if( vnl[i].d_name.isEmpty() )
                vnl[i].d_name = "";
        }
        return vnl;
    }

    JitComposer::UpvalList getUpvals()
    {
        JitComposer::UpvalList uvl(ctx.back().upvals.size());
        Ctx::Upvals::const_iterator i;
        for( i = ctx.back().upvals.begin(); i != ctx.back().upvals.end(); ++i )
        {
            JitComposer::Upval u;
            u.d_name = i.key()->d_name;
            // if( i.key()->d_uvRo )
            //    u.d_isRo = true; // TODO
            if( i.key()->d_scope == ctx.back().scope->d_scope )
            {
                u.d_uv = i.key()->d_slot;
                u.d_isLocal = true;
            }else
            {
                Q_ASSERT( ctx.size() > 1 );
                u.d_uv = ctx[ ctx.size() - 2 ].resolveUpval(i.key());
            }
            uvl[i.value()] = u;
        }

        return uvl;
    }

    void visit( Module* m )
    {
        ctx.push_back( Ctx(m) );
        bc.openFunction(0,m->d_file.toUtf8(),m->d_loc.d_row, m->d_end.d_row );

        modSlot = ctx.back().buySlots(1);
        // NOTE: if not modSlot is at slot 0 but a module var instead, upvalue access to the var at
        // slot 0 doesn't work, i.e. renders a constant floating point number instead of the true value.

        allocateLocals(m);

        bc.TNEW( modSlot, 0, 0, m->d_loc.d_row ); // local module = {}

        // local obnlj = require 'obnlj'
        Named* lib = m->find("obnlj");
        if( lib == 0 || lib->getTag() != Thing::T_Import )
        {
            obnlj = new Import();
            obnlj->d_name = Lexer::getSymbol("obnlj");
            obnlj->d_synthetic = true;
            obnlj->d_loc = m->d_loc;
            obnlj->d_slot = emitImport( obnlj->d_name, m->d_loc.d_row );
            obnlj->d_slotValid = true;
            obnlj->d_scope = mod;
        }else
            obnlj = static_cast<Import*>( lib );


        for( int i = 0; i < m->d_order.size(); i++ )
            m->d_order[i]->accept(this);

        for( int i = 0; i < m->d_body.size(); i++ )
            m->d_body[i]->accept(this);

        bc.UCLO( modSlot, 0, m->d_end.d_row );
        // bc.GSET( modSlot, m->d_name, m->d_end.d_row );
        bc.RET( modSlot, 1, m->d_end.d_row ); // return module

        JitComposer::VarNameList sn = getSlotNames(m);
        sn[modSlot].d_name = "_mod_";
        sn[obnlj->d_slot].d_name = "obnlj";
        bc.setVarNames( sn );
        bc.setUpvals( getUpvals() );
        bc.closeFunction(ctx.back().frameSize);
        ctx.pop_back();
    }

    void visit( Call* c )
    {
        Value v;
        process( c->d_what.data(), v );
        sell(v);
    }

    void visit( Return* r )
    {
        Q_ASSERT( ctx.back().scope->getTag() == Thing::T_Procedure &&
                  ctx.back().scope->d_type->getTag() == Thing::T_ProcType );
        ProcType* pt = static_cast<ProcType*>( ctx.back().scope->d_type.data() );

        QList<quint8> vars;
        for( int i = 0; i < pt->d_formals.size(); i++ )
        {
            if( pt->d_formals[i]->d_var )
                vars.append(i);
        }

        ctx.back().returnFound = true;
        Value ret;
        ret.d_slot = ctx.back().buySlots( 1 + vars.size() );
        ret.d_kind = Value::Tmp;
        ret.d_type = pt->d_return.data();
        assignExpr( ret, r->d_what.data(), r->d_loc.d_row );

        for( int i = 0; i < vars.size(); i++ )
            bc.MOV( ret.d_slot + 1, vars[i], r->d_loc.d_row );

        bc.RET(ret.d_slot, 1 + vars.size(), r->d_loc.d_row);
        ctx.back().sellSlots( ret.d_slot, 1 + vars.size() );
    }

    static bool isStructuredAssigByValue( const Value& v )
    {
        if( v.d_type == 0 || v.d_isVarParam || v.d_isParam )
            return false;
        const int tag = v.d_type->derefed()->getTag();
        if( tag == Thing::T_Record || tag == Thing::T_Array )
            return true;
        else
            return false;
    }

    void assignExpr( Value& lhs, Expression* ex, int line = -1 )
    {
        Q_ASSERT( lhs.d_kind == Value::Ref || lhs.d_kind == Value::Tmp || lhs.d_kind == Value::Uv
                  || lhs.d_kind == Value::Ref2 || lhs.d_kind == Value::Tmp2
                  || lhs.d_kind == Value::Ref2v || lhs.d_kind ==  Value::Tmp2v );

        if( line == -1 )
            line = ex->d_loc.d_row;

        if( ( lhs.d_kind == Value::Ref || lhs.d_kind == Value::Tmp ) && !isStructuredAssigByValue(lhs) )
        {
            // directly assign to lhs register to avoid additional MOV if possible
            Value rhs;
            rhs.d_slot = lhs.d_slot;
            rhs.d_kind = Value::Pre;
            process( ex, rhs );
            rhs.d_line = line;
            rhs.d_type = ex->d_type.data();
            assign(lhs,rhs);
        }else
        {
            // all other assignment cases
            Value rhs;
            process( ex, rhs );
            rhs.d_line = line;
            rhs.d_type = ex->d_type.data();
            assign(lhs,rhs);
        }
    }

    void copyImpl( Value& lhs, Value& rhs, int line )
    {
        Q_ASSERT( lhs.d_type );

        Type* t = lhs.d_type->derefed();
        const int tag = t->getTag();

        Q_ASSERT( tag == Thing::T_Record || tag == Thing::T_Array );

        derefIndexed(rhs);
        storeConst(rhs);
        Q_ASSERT( rhs.d_kind == Value::Ref || rhs.d_kind == Value::Tmp );

        derefIndexed(lhs);
        Q_ASSERT( lhs.d_kind == Value::Ref || lhs.d_kind == Value::Tmp );

        if( tag == Thing::T_Array )
        {
            Array* a = static_cast<Array*>(t);
            Type* at = a->d_type->derefed();
            if( at->getTag() == Thing::T_BaseType && static_cast<BaseType*>( at )->d_type == BaseType::CHAR )
            {
                quint8 tmp = ctx.back().buySlots(3,true);
                bc.TGET(tmp, lhs.d_slot, QVariant::fromValue(QByteArray("assig")), line );
                bc.MOV(tmp+1, lhs.d_slot, line );
                bc.MOV(tmp+2,rhs.d_slot, line );
                bc.CALL(tmp,0,2,line);
                ctx.back().sellSlots(tmp,3);
            }else
                qWarning() << "array deep copy not implemented" << line; // TODO

        }else
        {
            // TODO
            qWarning() << "record deep copy not implemented" << line;
        }
    }

    void assignImpl( const Value& lhs, Value& rhs, int line )
    {
        if( lhs.d_kind == Value::Ref || lhs.d_kind == Value::Tmp )
        {
            switch( rhs.d_kind )
            {
            case Value::Val:
                bc.KSET( lhs.d_slot, rhs.d_val, line );
                break;
            case Value::Tmp2:
            case Value::Ref2:
                bc.TGET( lhs.d_slot, rhs.d_slot, rhs.d_idx, line );
                break;
            case Value::Tmp2v:
            case Value::Ref2v:
                bc.TGET( lhs.d_slot, rhs.d_slot, rhs.d_val, line );
                break;
            case Value::Ref:
            case Value::Tmp:
                bc.MOV( lhs.d_slot, rhs.d_slot, line );
                break;
            case Value::Uv:
                bc.UGET( lhs.d_slot, rhs.d_slot, line );
                break;
            default:
                Q_ASSERT( false );
                break;
            }
        }else if( lhs.d_kind == Value::Uv )
        {
            derefIndexed(rhs);
            Q_ASSERT( rhs.d_kind == Value::Ref || rhs.d_kind == Value::Tmp || rhs.d_kind == Value::Val );
            if( rhs.d_kind == Value::Val )
                bc.USET( lhs.d_slot, rhs.d_val, line );
            else
                bc.USET( lhs.d_slot, rhs.d_slot, line );
        }else if( lhs.d_kind == Value::Ref2 || lhs.d_kind == Value::Tmp2 )
        {
            derefIndexed(rhs);
            storeConst(rhs);
            Q_ASSERT( rhs.d_kind == Value::Ref || rhs.d_kind == Value::Tmp );
            bc.TSET( rhs.d_slot, lhs.d_slot, lhs.d_idx, line );
        }else if( lhs.d_kind == Value::Ref2v || lhs.d_kind == Value::Tmp2v )
        {
            derefIndexed(rhs);
            storeConst(rhs);
            Q_ASSERT( rhs.d_kind == Value::Ref || rhs.d_kind == Value::Tmp );
            bc.TSET( rhs.d_slot, lhs.d_slot, lhs.d_val, line );
        }else
            Q_ASSERT( false );

    }

    static inline bool isArrayOfChar( Type* t )
    {
        // TEMP
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

    void jumpToBooleanValue( Value& out )
    {
        if( out.d_kind == Value::Jump )
        {
            out.d_slot = ctx.back().buySlots(1);
            out.d_kind = Value::Tmp;
            bc.KSET(out.d_slot, false, out.d_line );
            backpatch( out.falseList, bc.getCurPc() );
            bc.JMP( ctx.back().frameSize, 1, out.d_line );
            bc.KSET(out.d_slot, true, out.d_line );
            backpatch( out.trueList, bc.getCurPc() );
        }
    }

    void assign( Value& lhs, Value& rhs, bool keepRhs = false )
    {
        Q_ASSERT( lhs.d_kind == Value::Ref || lhs.d_kind == Value::Tmp || lhs.d_kind == Value::Uv
                  || lhs.d_kind == Value::Ref2 || lhs.d_kind == Value::Tmp2
                  || lhs.d_kind == Value::Ref2v || lhs.d_kind ==  Value::Tmp2v );

        const int line = rhs.d_line;

        if( ( lhs.d_kind == Value::Ref || lhs.d_kind == Value::Tmp ) &&
               (  rhs.d_kind == Value::Jump || rhs.d_kind == Value::Pre ) )
        {
            if( rhs.d_kind == Value::Jump )
            {
                bc.KSET(lhs.d_slot, false, rhs.d_line );
                backpatch( rhs.falseList, bc.getCurPc() );
                bc.JMP( ctx.back().frameSize, 1, rhs.d_line );
                bc.KSET(lhs.d_slot, true, rhs.d_line );
                backpatch( rhs.trueList, bc.getCurPc() );
            }// else assignment implicitly happened by evaluating rhs expr to lhs slot
        }else
        {
            Q_ASSERT( rhs.d_kind != Value::Pre );

            jumpToBooleanValue( rhs );
            if( isArrayOfChar(lhs.d_type) && isStructuredAssigByValue(lhs) ) // TODO for all records and arrays
                copyImpl( lhs, rhs, line );
            else
                assignImpl( lhs, rhs, line );
        }

        if( !keepRhs )
            sell(rhs);

        sell(lhs);
    }

    void visit( Assign* a )
    {
        // lhs is a plain designator (not a CallExpr)
        // either a local slot or an element of a table stored in a local slot.

        Value lhs;
        processExpr( a->d_lhs.data(), lhs );
        // top lhs expr: ident | '[' ExpList ']' | '^' | '(' qualident ')'

        lhs.d_type = a->d_lhs->d_type.data();
        assignExpr(lhs, a->d_rhs.data(), a->d_loc.d_row);
    }

    void renderIf( IfLoop* l )
    {
        Q_ASSERT( !l->d_if.isEmpty() && !l->d_then.isEmpty() );

        Value if0;
        process( l->d_if[0].data(), if0 );
        derefIndexed(if0);
        assureJump(if0);

        Q_ASSERT( if0.d_kind == Value::Jump );

        backpatch( if0.trueList, bc.getCurPc() + 1 );

        QList<quint32> after;

        for( int i = 0; i < l->d_then[0].size(); i++ )
            l->d_then[0][i]->accept(this);

        bc.JMP(ctx.back().frameSize, 0, l->d_loc.d_row );
        after << bc.getCurPc();

        backpatch( if0.falseList, bc.getCurPc() + 1 );

        for( int i = 1; i < l->d_if.size(); i++ )
        {
            Value ifn;
            process( l->d_if[i].data(), ifn );
            derefIndexed(ifn);
            assureJump(ifn);

            backpatch( ifn.trueList, bc.getCurPc() + 1 );

            for( int j = 0; j < l->d_then[i].size(); j++ )
                l->d_then[i][j]->accept(this);

            bc.JMP(ctx.back().frameSize, 0, l->d_if[i]->d_loc.d_row );
            after << bc.getCurPc();

            backpatch( ifn.falseList, bc.getCurPc() + 1 );
        }

        if( !l->d_else.isEmpty() )
        {
            for( int j = 0; j < l->d_else.size(); j++ )
                l->d_else[j]->accept(this);
        }

        backpatch( after, bc.getCurPc() + 1 );
    }

    void renderWhile( IfLoop* l )
    {
        QList<quint32> loop, after;

        bc.LOOP( ctx.back().frameSize, 0, l->d_loc.d_row ); // while true do
        const quint32 start = bc.getCurPc();
        loop << start;

        Value if0;
        process( l->d_if[0].data(), if0 ); // if cond
        derefIndexed(if0);
        assureJump(if0);

        backpatch( if0.trueList, bc.getCurPc() + 1 );

        for( int i = 0; i < l->d_then[0].size(); i++ ) // then start
            l->d_then[0][i]->accept(this);

        bc.JMP(ctx.back().frameSize, 0, l->d_loc.d_row ); // then complete, stay in loop
        loop << bc.getCurPc();

        backpatch( if0.falseList, bc.getCurPc() + 1 );

        for( int i = 1; i < l->d_if.size(); i++ )
        {
            Value ifn;
            process( l->d_if[i].data(), ifn ); // elseif cond
            derefIndexed(ifn);
            assureJump(ifn);

            backpatch( ifn.trueList, bc.getCurPc() + 1 );

            for( int j = 0; j < l->d_then[i].size(); j++ ) // then start
                l->d_then[i][j]->accept(this);

            bc.JMP( ctx.back().frameSize, 0, l->d_if[i]->d_loc.d_row ); // then complete, stay in loop
            loop << bc.getCurPc();

            backpatch( ifn.falseList, bc.getCurPc() + 1 );
        }

        bc.JMP(ctx.back().frameSize, 0, l->d_loc.d_row ); // else quit loop
        after << bc.getCurPc();

        bc.JMP( ctx.back().frameSize, start - bc.getCurPc() - 2, l->d_loc.d_row ); // end while true
        backpatch( loop, bc.getCurPc() );

        backpatch( after, bc.getCurPc() + 1 );
    }

    void renderRepeat( IfLoop* l )
    {
        QList<quint32> loop;

        bc.LOOP( ctx.back().frameSize, 0, l->d_loc.d_row ); // repeat
        const quint32 start = bc.getCurPc();
        loop << start;

        for( int i = 0; i < l->d_then.first().size(); i++ )
            l->d_then.first()[i]->accept(this);

        Value cond;
        process( l->d_if.first().data(), cond ); // until cond
        derefIndexed(cond);
        assureJump(cond);

        bc.JMP( ctx.back().frameSize, start - bc.getCurPc() - 2, l->d_loc.d_row ); // end while true
        backpatch( loop, bc.getCurPc() );
        backpatch( cond.falseList, bc.getCurPc() );

        backpatch( cond.trueList, bc.getCurPc() + 1 ); // jump after loop
    }

    void visit( IfLoop* l )
    {
        Q_ASSERT( l->d_then.size() == l->d_if.size() );
        if( l->d_then.isEmpty() )
            return;
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

        Q_ASSERT( l->d_id->d_slotValid );

        // Consider to first rewrite the AST instead of the following complex code

        Value id;
        id.d_slot = l->d_id->d_slot;
        id.d_kind = Value::Ref;
        assignExpr( id, l->d_from.data(), l->d_loc.d_row );

        Value by;
        by.d_kind = Value::Val;
        by.d_val = l->d_byVal;
        by.d_line = l->d_loc.d_row;
        storeConst(by);

        QList<quint32> loop, after;

        bc.LOOP( ctx.back().frameSize, 0, l->d_loc.d_row ); // while true do
        const quint32 start = bc.getCurPc();
        loop << start;

        Value to;
        process( l->d_to.data(), to );
        derefIndexed(to);
        storeConst(to);

        Value if0;
        const int inc = l->d_byVal.toInt();
        if( inc > 0 )
            bc.ISLE(id.d_slot,to.d_slot,l->d_loc.d_row); // id <= to
        else
            bc.ISGE(id.d_slot,to.d_slot,l->d_loc.d_row); // id >= to

        bc.JMP( ctx.back().frameSize, 0, l->d_loc.d_row );
        if0.trueList << bc.getCurPc();
        bc.JMP( ctx.back().frameSize, 0, l->d_loc.d_row );
        if0.falseList << bc.getCurPc();
        if0.d_kind = Value::Jump;

        backpatch( if0.trueList, bc.getCurPc() + 1 );

        for( int i = 0; i < l->d_do.size(); i++ ) // do start
            l->d_do[i]->accept(this);
        bc.ADD(id.d_slot,id.d_slot,l->d_byVal, l->d_loc.d_row ); // id += inc

        bc.JMP(ctx.back().frameSize, 0, l->d_loc.d_row ); // do complete, stay in loop
        loop << bc.getCurPc();

        backpatch( if0.falseList, bc.getCurPc() + 1 );

        bc.JMP(ctx.back().frameSize, 0, l->d_loc.d_row ); // else quit loop
        after << bc.getCurPc();

        bc.JMP( ctx.back().frameSize, start - bc.getCurPc() - 2, l->d_loc.d_row ); // end while true
        backpatch( loop, bc.getCurPc() );

        backpatch( after, bc.getCurPc() + 1 );

        sell(to);
        sell(by);
    }

    void emitTypeCase( CaseStmt* cs )
    {
        // first rewrite the AST with 'if' instead of complex 'case'

        Ref<IfLoop> ifl = new IfLoop();
        ifl->d_op = IfLoop::IF;
        ifl->d_loc = cs->d_loc;

        for( int i = 0; i < cs->d_cases.size(); i++ )
        {
            const CaseStmt::Case& c = cs->d_cases[i];

            Q_ASSERT( c.d_labels.size() == 1 );

            Ref<BinExpr> eq = new BinExpr();
            eq->d_op = BinExpr::IS;
            eq->d_lhs = cs->d_exp;
            eq->d_rhs = c.d_labels.first();
            eq->d_loc = cs->d_exp->d_loc;

            ifl->d_if.append(eq.data());
            ifl->d_then.append( c.d_block );
        }

        // and now generate code for the if
        ifl->accept(this);
    }

    void visit( CaseStmt* cs )
    {
        if( cs->d_typeCase )
        {
            emitTypeCase(cs);
            return;
        }

        // first rewrite the AST with 'if' instead of complex 'case'

        Ref<IfLoop> ifl = new IfLoop();
        ifl->d_op = IfLoop::IF;
        ifl->d_loc = cs->d_loc;

        for( int i = 0; i < cs->d_cases.size(); i++ )
        {
            const CaseStmt::Case& c = cs->d_cases[i];

            QList< Ref<Expression> > ors;
            for( int j = 0; j < c.d_labels.size(); j++ )
            {
                Expression* l = c.d_labels[j].data();
                // TODO: avoid calling cs->d_exp more than once by using temp var
                if( l->getTag() == Thing::T_BinExpr )
                {
                    BinExpr* bi = static_cast<BinExpr*>( l );
                    Q_ASSERT( bi->d_op == BinExpr::Range );

                    Ref<BinExpr> _and = new BinExpr();
                    _and->d_op = BinExpr::AND;
                    _and->d_loc = l->d_loc;

                    Ref<BinExpr> lhs = new BinExpr();
                    lhs->d_op = BinExpr::GEQ;
                    lhs->d_lhs = cs->d_exp;
                    lhs->d_rhs = bi->d_lhs;
                    lhs->d_loc = l->d_loc;

                    Ref<BinExpr> rhs = new BinExpr();
                    rhs->d_op = BinExpr::LEQ;
                    rhs->d_lhs = cs->d_exp;
                    rhs->d_rhs = bi->d_rhs;
                    rhs->d_loc = l->d_loc;

                    _and->d_lhs = lhs.data();
                    _and->d_rhs = rhs.data();

                    ors << _and.data();
                }else
                {
                    Ref<BinExpr> eq = new BinExpr();
                    eq->d_op = BinExpr::EQ;
                    eq->d_lhs = cs->d_exp;
                    eq->d_rhs = l;
                    eq->d_loc = l->d_loc;

                    ors << eq.data();
                }
            }
            Q_ASSERT( !ors.isEmpty() );
            if( ors.size() == 1 )
                ifl->d_if.append( ors.first() );
            else
            {
                Q_ASSERT( ors.size() > 1 );
                Ref<BinExpr> bi = new BinExpr();
                bi->d_op = BinExpr::OR;
                bi->d_lhs = ors[0];
                bi->d_rhs = ors[1];
                bi->d_loc = ors[1]->d_loc;
                for( int i = 2; i < ors.size(); i++ )
                {
                    Ref<BinExpr> tmp = new BinExpr();
                    tmp->d_op = BinExpr::OR;
                    tmp->d_lhs = bi.data();
                    bi = tmp;
                    bi->d_rhs = ors[i];
                    bi->d_loc = ors[i]->d_loc;
                }
                ifl->d_if.append( bi.data() );
            }

            ifl->d_then.append( c.d_block );
        }

        // and now generate code for the if
        ifl->accept(this);
    }

    // go top down, generate bottom up
    // code is generated by all non-leaf (unexpr and subs, binexpr, setexpr)
    // each expr returns either a (temp) slot or a const code

    void process( Expression* ex, Value& out )
    {
        // top expression call, starts a new expression

        // boolean expression level 0
        //   1 relation
        //   1 value (true/false)
        //   1 bool variable
        //   n or
        //   n and
        //   1 not

        // Examples:

        // Oberon: ~( ( y > 1 ) & c OR d & e & f & g )
        // AST: ( NOT ( OR ( AND ( GT y '1' ) c ) ( AND ( AND ( AND d e ) f ) g ) ) )

        // ~( ( y > 1 ) & c OR d & e OR f & g )
        // ( NOT ( OR ( OR ( AND ( GT y '1' ) c ) ( AND d e ) ) ( AND f g ) ) )

        // ~( ( y > 1 ) & c OR d & ( e OR f ) & g )
        // ( NOT ( OR ( AND ( GT y '1' ) c ) ( AND ( AND d ( OR e f ) ) g ) ) )

        // NOTE: the current boolean expression implementation simply follows Aho, Lam, Sethi, Ullmann
        // Compilers 2nd edition chapters 6.6 and 6.7; the LuaJIT compiler does more fancy stuff which
        // I'll save for the future.

        processExpr(ex,out);
    }

    void processLiteral( Literal* l, Value& out )
    {
        if( l->d_val.canConvert<Ast::Set>() )
        {
            // out << "obnlj.SET(" << QByteArray::number( quint32(v.value<Ast::Set>().to_ulong()) ) << ")";
            quint8 tmp = ctx.back().buySlots(2,true);
            fetchObnljMember( tmp, "SET", l->d_loc.d_row );
            bc.KSET(tmp+1, qlonglong(l->d_val.value<Ast::Set>().to_ulong()), l->d_loc.d_row );
            bc.CALL(tmp,1,1, l->d_loc.d_row );
            emitEndOfCall(tmp,2,out,l->d_loc.d_row);
        }else if( l->d_val.type() == QVariant::ByteArray )
        {
            // out << "obnlj.Str(\"" << luaStringEscape(v.toByteArray() ) << "\")"; // TODO: string escape?
            quint8 tmp = ctx.back().buySlots(2,true);
            fetchObnljMember( tmp, "Str", l->d_loc.d_row );
            bc.KSET(tmp+1, l->d_val, l->d_loc.d_row );
            bc.CALL(tmp,1,1, l->d_loc.d_row );
            emitEndOfCall(tmp,2,out,l->d_loc.d_row);
        }else
        {
            out.d_val = l->d_val;
            out.d_kind = Value::Val;
            out.d_line = l->d_loc.d_row;
        }
    }

    void processExpr( Expression* ex, Value& out )
    {
        out.d_line = ex->d_loc.d_row;
        switch( ex->getTag() )
        {
        case Thing::T_Literal:
            processLiteral( static_cast<Literal*>(ex), out );
            break;
        case Thing::T_IdentLeaf:
            processIdentLeaf( static_cast<IdentLeaf*>( ex ), out );
            break;
        case Thing::T_BinExpr:
            processBinExpr( static_cast<BinExpr*>( ex ), out );
            break;
        case Thing::T_UnExpr:
            processUnExpr( static_cast<UnExpr*>( ex ), out );
            break;
        case Thing::T_SetExpr:
            processSetExpr( static_cast<SetExpr*>( ex ), out );
            break;
        case Thing::T_IdentSel:
            processIdentSel( static_cast<IdentSel*>( ex ), out );
            break;
        case Thing::T_CallExpr:
            processCallExpr( static_cast<CallExpr*>( ex ), out );
            break;
        default:
            Q_ASSERT( false );
            break;
        }
    }

    void processIdentLeaf( IdentLeaf* id, Value& out )
    {
        Q_ASSERT( !id->d_ident.isNull() );
        const int tag = id->d_ident->getTag();

        if( tag == Thing::T_LocalVar || tag == Thing::T_Variable || tag == Thing::T_Parameter ||
                  tag == Thing::T_Procedure || ( tag == Thing::T_NamedType && id->d_ident->d_slotValid ) ||
                  tag == Thing::T_Import )
        {
            if( id->d_ident->d_scope != ctx.back().scope )
            {
                // Upvalue
                out.d_slot = resolveUpval(id->d_ident.data());
                out.d_kind = Value::Uv;
                out.d_line = id->d_loc.d_row;
            }else
            {
                Q_ASSERT( id->d_ident->d_slotValid );
                out.d_slot = id->d_ident->d_slot;
                out.d_kind = Value::Ref;
                out.d_line = id->d_loc.d_row;
            }
        }else if( tag == Thing::T_Const )
        {
            Const* c = static_cast<Const*>( id->d_ident.data() );
            out.d_val = c->d_val;
            out.d_kind = Value::Val;
            out.d_line = c->d_loc.d_row;
        }
    }

    void processBinExpr( BinExpr* e, Value& out )
    {
        Value lhs;
        Value rhs;

        processExpr( e->d_lhs.data(), lhs );
        if( e->d_op == BinExpr::Index )
        {
            // Modify expression, add 1 to each index
            Ref<BinExpr> add = new BinExpr();
            add->d_op = BinExpr::ADD;
            add->d_type = e->d_rhs->d_type;
            add->d_loc = e->d_rhs->d_loc;
            add->d_lhs = e->d_rhs;
            add->d_rhs = new Literal(e->d_rhs->d_type.data(),e->d_rhs->d_loc, qlonglong(1) );
            //e->d_rhs = add.data(); // no need for persistence
            // NOTE: apparently on bytecode level also zero based indices seem to work (even -1 works)
            process( add.data(), rhs );
        }else if( e->d_op == BinExpr::IS || e->d_op == BinExpr::AND || e->d_op == BinExpr::OR )
        {
            // NOP rhs IS, AND, OR
        }else
            processExpr( e->d_rhs.data(), rhs );

        if( lhs.d_kind == Value::Val && rhs.d_kind == Value::Val )
        {
            // if both val then compile time eval
            out.d_kind = Value::Val;
            QString err;
            out.d_val = Eval::binOp( e->d_op, lhs.d_val, rhs.d_val, &err );
            if( !out.d_val.isValid() )
                error( e->d_loc, err );
            return;
        }

        switch( e->d_op )
        {
        case BinExpr::ADD:
        case BinExpr::SUB:
        case BinExpr::MUL:
        case BinExpr::MOD:
        case BinExpr::DIV:
        case BinExpr::FDIV:
            // in num or slot, out slot
            processArithOp( e, out, lhs, rhs );
            break;
        case BinExpr::Index:
            processIndexOp( e, out, lhs, rhs );
            break;
        case BinExpr::Range:
            Q_ASSERT( false );
            break;
        case BinExpr::AND:
        case BinExpr::OR:
            processAndOrOp( e, out, lhs );
            break;
        case BinExpr::IS:
        case BinExpr::IN:
            processRelationIsIn(e,out,lhs,rhs);
            break;
        default:
            // EQ, NEQ, LE, LEQ, GT, GEQ
            processRelationOp( e, out, lhs, rhs );
            break;
        }
    }

    void assureJump( Value& out )
    {
        if( out.d_kind != Value::Jump )
        {
            derefIndexed(out);
            storeConst(out);
            bc.IST(out.d_slot,out.d_line);
            sell(out);
            out.d_kind = Value::Jump;
            bc.JMP( ctx.back().frameSize, 0, out.d_line );
            out.trueList << bc.getCurPc();
            bc.JMP( ctx.back().frameSize, 0, out.d_line );
            out.falseList << bc.getCurPc();
        }
    }

    void backpatch( const QList<quint32>& l, quint32 pc2 )
    {
        foreach( quint32 pc1, l )
            bc.patch( pc1, pc2 - pc1 - 1 );
    }

    void processAndOrOp( BinExpr* e, Value& out, Value& lhs )
    {
        // if( a and b and c ) then x -> a, b and c have the same outcome targets
        // if( a[b] and c ) then x -> b cannot be boolean but must be integer
        // if( a(b and d( x or y ) ) and c ) then x -> args of a and d can be boolean, so during evaluation of one expression another can happen
        // if( a and ( b or c ) and d ) then x

        assureJump(lhs);

        const int M = bc.getCurPc() + 1;
        Value rhs;
        processExpr( e->d_rhs.data(), rhs );
        assureJump(rhs);

        switch( e->d_op )
        {
        case BinExpr::AND:
            backpatch( lhs.trueList, M );
            out.trueList = rhs.trueList;
            out.falseList = lhs.falseList + rhs.falseList;
            break;
        case BinExpr::OR:
            backpatch( lhs.falseList, M );
            out.trueList = lhs.trueList + rhs.trueList;
            out.falseList = rhs.falseList;
            break;
        }

        out.d_kind = Value::Jump;
    }

    void fetchObnljMember( quint8 to, const QByteArray& index, int line )
    {
        if( ctx.back().scope != mod ) // obnlj->d_scope == mod
        {
            // resolve upvalue
            bc.UGET( to, resolveUpval( obnlj.data() ), line );
            bc.TGET(to, to, QVariant::fromValue(index), line );
        }else
        {
            // Module level
            bc.TGET(to, obnlj->d_slot, QVariant::fromValue(index), line );
        }
    }

    void processRelationIsIn( BinExpr* e, Value& out, Value& lhs, Value& rhs )
    {
        derefIndexed(lhs);
        derefIndexed(rhs);

        Q_ASSERT( lhs.d_kind == Value::Ref || lhs.d_kind == Value::Tmp || lhs.d_kind == Value::Val );

        quint8 tmp = ctx.back().buySlots(3,true);

        if( e->d_op == BinExpr::IS )
        {
            Q_ASSERT( rhs.d_kind == Value::None );
            fetchObnljMember(tmp,"is_a", e->d_loc.d_row);
        }else if( e->d_op == BinExpr::IN )
            fetchObnljMember(tmp,"IN", e->d_loc.d_row);
        else
            Q_ASSERT( false );

        Value v;
        v.d_slot = tmp + 1;
        v.d_kind = Value::Ref;
        assign(v, lhs, true );

        v.d_slot = tmp+2;
        if( e->d_op == BinExpr::IS )
        {
            v.d_line = e->d_loc.d_row;
            v.d_kind = Value::Pre;
            fetchClass(e->d_rhs->d_type.data(),v);
        }else
        {
            v.d_kind = Value::Ref;
            assign(v, rhs, true );
        }

        bc.CALL(tmp,1,2, e->d_loc.d_row);
        emitEndOfCall(tmp,3,out,e->d_loc.d_row);

        sell(lhs);
        sell(rhs);
    }

    void processRelationOp( BinExpr* e, Value& out, Value& lhs, Value& rhs )
    {
        derefIndexed(lhs);
        derefIndexed(rhs);

        Q_ASSERT( lhs.d_kind == Value::Ref || lhs.d_kind == Value::Tmp || lhs.d_kind == Value::Val );
        Q_ASSERT( rhs.d_kind == Value::Ref || rhs.d_kind == Value::Tmp || rhs.d_kind == Value::Val );

        const bool inv = false;
        int op = e->d_op;
        if( inv && op == BinExpr::EQ )
            op = BinExpr::NEQ;
        else if( inv && op == BinExpr::NEQ )
            op = BinExpr::EQ;

        switch( op )
        {
        case BinExpr::EQ:
            if( rhs.d_kind == Value::Val )
                bc.ISEQ(lhs.d_slot,rhs.d_val,e->d_loc.d_row);
            else if( lhs.d_kind == Value::Val )
                bc.ISEQ(rhs.d_slot,lhs.d_val,e->d_loc.d_row);
            else
                bc.ISEQ(lhs.d_slot,rhs.d_slot,e->d_loc.d_row);
            break;
        case BinExpr::NEQ:
            if( rhs.d_kind == Value::Val )
                bc.ISNE(lhs.d_slot,rhs.d_val,e->d_loc.d_row);
            else if( lhs.d_kind == Value::Val )
                bc.ISNE(rhs.d_slot,lhs.d_val,e->d_loc.d_row);
            else
                bc.ISNE(lhs.d_slot,rhs.d_slot,e->d_loc.d_row);
            break;

        case BinExpr::LT:
            storeConst(lhs);
            storeConst(rhs);
            if( inv )
                bc.ISGE(lhs.d_slot,rhs.d_slot,e->d_loc.d_row); // see http://wiki.luajit.org/Bytecode-2.0
            else
                bc.ISLT(lhs.d_slot,rhs.d_slot,e->d_loc.d_row);
            break;
        case BinExpr::LEQ:
            storeConst(lhs);
            storeConst(rhs);
            if( inv )
                bc.ISGT(lhs.d_slot,rhs.d_slot,e->d_loc.d_row);
            else
                bc.ISLE(lhs.d_slot,rhs.d_slot,e->d_loc.d_row);
            break;
        case BinExpr::GT:
            storeConst(lhs);
            storeConst(rhs);
            if( inv )
                bc.ISGE(rhs.d_slot,lhs.d_slot,e->d_loc.d_row);
            else
                bc.ISGT(lhs.d_slot,rhs.d_slot,e->d_loc.d_row);
            break;
       case BinExpr::GEQ:
            storeConst(lhs);
            storeConst(rhs);
            if( inv )
                bc.ISGT(rhs.d_slot,lhs.d_slot,e->d_loc.d_row);
            else
                bc.ISGE(lhs.d_slot,rhs.d_slot,e->d_loc.d_row);
            break;

        default:
            Q_ASSERT( false );
            break;
        }

        bc.JMP( ctx.back().frameSize, 0, e->d_loc.d_row );
        out.trueList << bc.getCurPc();
        bc.JMP( ctx.back().frameSize, 0, e->d_loc.d_row );
        out.falseList << bc.getCurPc();
        out.d_kind = Value::Jump;

        sell(lhs);
        sell(rhs);
    }

    void derefIndexed( Value& v )
    {
        switch( v.d_kind )
        {
        case Value::Tmp2:
            bc.TGET( v.d_slot, v.d_slot, v.d_idx, v.d_line );
            ctx.back().sellSlots( v.d_idx );
            v.d_kind = Value::Tmp;
            break;
        case Value::Ref2:
            {
                quint8 slot = ctx.back().buySlots(1);
                bc.TGET( slot, v.d_slot, v.d_idx, v.d_line );
                v.d_slot = slot;
                v.d_kind = Value::Tmp;
            }
            break;
        case Value::Tmp2v:
            bc.TGET( v.d_slot, v.d_slot, v.d_val, v.d_line );
            v.d_kind = Value::Tmp;
            break;
        case Value::Ref2v:
            {
                quint8 slot = ctx.back().buySlots(1);
                bc.TGET( slot, v.d_slot, v.d_val, v.d_line );
                v.d_slot = slot;
                v.d_kind = Value::Tmp;
            }
            break;
        case Value::Uv:
            {
                quint8 slot = ctx.back().buySlots(1);
                bc.UGET(slot, v.d_slot, v.d_line );
                v.d_slot = slot;
                v.d_kind = Value::Tmp;
            }
            break;
        }
    }

    void storeConst( Value& v )
    {
        if( v.d_kind == Value::Val )
        {
            v.d_slot = ctx.back().buySlots(1);
            bc.KSET( v.d_slot, v.d_val, v.d_line );
            v.d_kind = Value::Tmp;
        }
    }

    void processArithOp( BinExpr* e, Value& out, Value& lhs, Value& rhs )
    {
        derefIndexed(lhs);
        derefIndexed(rhs);

        // no tmp2 or ref2 left here
        Q_ASSERT( lhs.d_kind == Value::Ref || lhs.d_kind == Value::Tmp || lhs.d_kind == Value::Val );
        Q_ASSERT( rhs.d_kind == Value::Ref || rhs.d_kind == Value::Tmp || rhs.d_kind == Value::Val );
        Q_ASSERT( !( lhs.d_kind == Value::Val && rhs.d_kind == Value::Val ) );

        quint8 resSlot = 0;
        if( out.d_kind == Value::Pre )
            resSlot = out.d_slot;
        else if( lhs.d_kind == Value::Tmp )
        {
            out.d_kind = Value::Tmp;
            resSlot = lhs.d_slot;
            out.d_slot = resSlot;
        }else if( rhs.d_kind == Value::Tmp )
        {
            out.d_kind = Value::Tmp;
            resSlot = rhs.d_slot;
            out.d_slot = resSlot;
        }else
        {
            out.d_kind = Value::Tmp;
            resSlot = ctx.back().buySlots(1);
            out.d_slot = resSlot;
        }

        switch( e->d_op )
        {
        case BinExpr::ADD:
            if( lhs.d_kind == Value::Val )
                bc.ADD( resSlot, lhs.d_val, rhs.d_slot, e->d_loc.d_row );
            else if( rhs.d_kind == Value::Val )
                bc.ADD( resSlot, lhs.d_slot, rhs.d_val, e->d_loc.d_row );
            else
                bc.ADD( resSlot, lhs.d_slot, rhs.d_slot, e->d_loc.d_row );
            break;
        case BinExpr::SUB:
            if( lhs.d_kind == Value::Val )
                bc.SUB( resSlot, lhs.d_val, rhs.d_slot, e->d_loc.d_row );
            else if( rhs.d_kind == Value::Val )
                bc.SUB( resSlot, lhs.d_slot, rhs.d_val, e->d_loc.d_row );
            else
                bc.SUB( resSlot, lhs.d_slot, rhs.d_slot, e->d_loc.d_row );
            break;
        case BinExpr::MUL:
            if( lhs.d_kind == Value::Val )
                bc.MUL( resSlot, lhs.d_val, rhs.d_slot, e->d_loc.d_row );
            else if( rhs.d_kind == Value::Val )
                bc.MUL( resSlot, lhs.d_slot, rhs.d_val, e->d_loc.d_row );
            else
                bc.MUL( resSlot, lhs.d_slot, rhs.d_slot, e->d_loc.d_row );
            break;
        case BinExpr::FDIV:
            if( lhs.d_kind == Value::Val )
                bc.DIV( resSlot, lhs.d_val, rhs.d_slot, e->d_loc.d_row );
            else if( rhs.d_kind == Value::Val )
                bc.DIV( resSlot, lhs.d_slot, rhs.d_val, e->d_loc.d_row );
            else
                bc.DIV( resSlot, lhs.d_slot, rhs.d_slot, e->d_loc.d_row );
            break;
        case BinExpr::MOD:
#if 0
            if( lhs.d_kind == Value::Val )
                bc.MOD( resSlot, lhs.d_val, rhs.d_slot, e->d_loc.d_row );
            else if( rhs.d_kind == Value::Val )
                bc.MOD( resSlot, lhs.d_slot, rhs.d_val, e->d_loc.d_row );
            else
                bc.MOD( resSlot, lhs.d_slot, rhs.d_slot, e->d_loc.d_row );
#endif
            {
                quint8 tmp = ctx.back().buySlots(3,true);
                fetchObnljMember(tmp,"MOD",e->d_loc.d_row);
                Value arg;
                arg.d_slot = tmp + 1;
                arg.d_kind = Value::Ref;
                assign(arg, lhs, true );
                arg.d_slot = tmp + 2;
                arg.d_kind = Value::Ref;
                assign(arg, rhs, true );
                bc.CALL(tmp,1,2,e->d_loc.d_row);
                bc.MOV(resSlot, tmp, e->d_loc.d_row ); // TODO: move MOD/DIV in separate function to avoid this MOV
                ctx.back().sellSlots(tmp,3);
            }
            break;
        case BinExpr::DIV:
            {
                quint8 tmp = ctx.back().buySlots(3,true);
                fetchObnljMember(tmp,"DIV",e->d_loc.d_row);
                Value arg;
                arg.d_slot = tmp + 1;
                arg.d_kind = Value::Ref;
                assign(arg, lhs, true );
                arg.d_slot = tmp + 2;
                arg.d_kind = Value::Ref;
                assign(arg, rhs, true );
                bc.CALL(tmp,1,2,e->d_loc.d_row);
                bc.MOV(resSlot, tmp, e->d_loc.d_row );
                ctx.back().sellSlots(tmp,3);
            }
            break;
        default:
            Q_ASSERT( false );
            break;
        }
        if( lhs.d_slot != resSlot )
            sell(lhs);
        if( rhs.d_slot != resSlot )
            sell(rhs);
    }

    void processIndexOp( BinExpr* e, Value& out, Value& lhs, Value& rhs )
    {
        Q_ASSERT( e->d_op == BinExpr::Index );

        derefIndexed(lhs);
        derefIndexed(rhs);

        Q_ASSERT( lhs.d_kind == Value::Ref || lhs.d_kind == Value::Tmp );
        Q_ASSERT( rhs.d_kind == Value::Ref || rhs.d_kind == Value::Tmp || rhs.d_kind == Value::Val );

        // ignore Pre if there
        if( lhs.d_kind == Value::Ref && rhs.d_kind == Value::Ref )
        {
            out.d_kind = Value::Ref2;
            out.d_slot = lhs.d_slot;
            out.d_idx = rhs.d_slot;
        }else if( lhs.d_kind == Value::Ref && rhs.d_kind == Value::Val )
        {
            out.d_kind = Value::Ref2v;
            out.d_slot = lhs.d_slot;
            out.d_val = rhs.d_val;
        }else if( lhs.d_kind == Value::Ref && rhs.d_kind == Value::Tmp )
        {
            out.d_kind = Value::Tmp2;
            out.d_slot = ctx.back().buySlots(1);
            bc.MOV( out.d_slot, lhs.d_slot, e->d_loc.d_row );
            out.d_idx = rhs.d_slot;
        }else if( lhs.d_kind == Value::Tmp && rhs.d_kind == Value::Ref )
        {
            out.d_kind = Value::Tmp2;
            out.d_slot = lhs.d_slot;
            out.d_idx = ctx.back().buySlots(1);
            bc.MOV( out.d_idx, rhs.d_slot, e->d_loc.d_row );
        }else if( lhs.d_kind == Value::Tmp && rhs.d_kind == Value::Tmp )
        {
            out.d_kind = Value::Tmp2;
            out.d_slot = lhs.d_slot;
            out.d_idx = rhs.d_slot;
        }else if( lhs.d_kind == Value::Tmp && rhs.d_kind == Value::Val )
        {
            out.d_kind = Value::Tmp2v;
            out.d_slot = lhs.d_slot;
            out.d_val = rhs.d_val;
        }
    }

    void processUnExpr( UnExpr* e, Value& out )
    {
        if( e->d_op == UnExpr::DEREF || e->d_op == UnExpr::CAST )
        {
            processExpr( e->d_sub.data(), out );
            return;
        }
        // else

        Value rhs;
        processExpr( e->d_sub.data(), rhs );

        if( rhs.d_kind == Value::Val )
        {
            out.d_kind = Value::Val;
            QString err;
            out.d_val = Eval::unOp( e->d_op, rhs.d_val, &err );
            if( !out.d_val.isValid() )
                error( e->d_loc, err );
            return;
        }else if( rhs.d_kind == Value::Jump )
        {
            out.d_kind = Value::Jump;
            out.trueList = rhs.falseList;
            out.falseList = rhs.trueList;
            return;
        }

        derefIndexed(rhs);
        Q_ASSERT( rhs.d_kind == Value::Ref || rhs.d_kind == Value::Tmp );

        quint8 resSlot = 0;
        if( out.d_kind == Value::Pre )
            resSlot = out.d_slot;
        else if( rhs.d_kind == Value::Tmp )
        {
            out.d_kind = Value::Tmp;
            resSlot = rhs.d_slot;
            out.d_slot = resSlot;
        }else
        {
            out.d_kind = Value::Tmp;
            resSlot = ctx.back().buySlots(1);
            out.d_slot = resSlot;
        }

        switch( e->d_op )
        {
        case UnExpr::NOT:
            bc.NOT( resSlot, rhs.d_slot, e->d_loc.d_row );
            break;
        case UnExpr::NEG:
            bc.UNM( resSlot, rhs.d_slot, e->d_loc.d_row );
            break;
        }

        if( rhs.d_slot != resSlot )
            sell(rhs);
    }

    void processSetExpr( SetExpr* s, Value& out )
    {
        const int count = s->d_parts.size() * 2 + 1;
        quint8 tmp = ctx.back().buySlots(count,true);
        fetchObnljMember(tmp,"SET",s->d_loc.d_row);

        int n = 1;
        for( int i = 0; i < s->d_parts.size(); i++ )
        {
            if( s->d_parts[i]->getTag() == Thing::T_BinExpr &&
                    static_cast<BinExpr*>( s->d_parts[i].data() )->d_op == BinExpr::Range )
            {
                BinExpr* bi = static_cast<BinExpr*>( s->d_parts[i].data() );
                Value lhs;
                lhs.d_slot = tmp + n++;
                lhs.d_kind = Value::Ref;
                assignExpr(lhs, bi->d_lhs.data() );
                lhs.d_slot = tmp + n++;
                assignExpr(lhs, bi->d_rhs.data() );
            }else
            {
                Value lhs;
                lhs.d_slot = tmp + n++;
                lhs.d_kind = Value::Ref;
                assignExpr(lhs, s->d_parts[i].data() );
                Value rhs;
                rhs.d_kind = Value::Val;
                rhs.d_val = -1;
                rhs.d_line = s->d_parts[i]->d_loc.d_row;
                lhs.d_slot = tmp + n++;
                assign(lhs, rhs );
            }
        }
        bc.CALL( tmp, 1, count - 1, s->d_loc.d_row );
        emitEndOfCall(tmp,count,out,s->d_loc.d_row);
    }

    void processIdentSel( IdentSel* e, Value& out )
    {
        Q_ASSERT( !e->d_ident.isNull() );

        Value lhs;
        processExpr( e->d_sub.data(), lhs );

        derefIndexed(lhs);

        Q_ASSERT( lhs.d_kind == Value::Ref || lhs.d_kind == Value::Tmp );

        if( e->d_sub->getTag() == Thing::T_Import &&
                e->d_ident->getTag() != Thing::T_Procedure && !e->d_ident->d_type->isStructured() &&
                e->d_ident->getTag() != Thing::T_NamedType )
        {
            // value access to scalar variable in other module by function call
            quint8 tmp = ctx.back().buySlots(1,true);
            Q_ASSERT( lhs.d_kind == Value::Ref );
            bc.TGET( tmp, lhs.d_slot, QVariant::fromValue(e->d_ident->d_name), e->d_loc.d_row );
            bc.CALL( tmp, 1, 0, e->d_loc.d_row );
            emitEndOfCall(tmp,1,out,e->d_loc.d_row);
        }else
        {
            // ignore Pre if there
            // NOTE: caller doesn't lose Pre slot and checks whether Value kind changed
            out.d_slot = lhs.d_slot;
            if( lhs.d_kind == Value::Ref )
                out.d_kind = Value::Ref2v;
            else
                out.d_kind = Value::Tmp2v;
            out.d_val = QVariant::fromValue(e->d_ident->d_name);
        }
    }

    bool genBuiltIn( CallExpr* c, Value& out )
    {
        ProcType* pt = c->getProcType();
        Q_ASSERT( pt->d_ident && pt->d_ident->getTag() == Thing::T_BuiltIn );
        BuiltIn* bi = static_cast<BuiltIn*>( pt->d_ident );

        switch( bi->d_func )
        {
        // Statements:
        case BuiltIn::NEW:
            {
                Q_ASSERT( c->d_actuals.size() == 1 );
                Value lhs;
                processExpr( c->d_actuals.first().data(), lhs );
                Q_ASSERT( lhs.d_kind != Value::Tmp );
                Value rhs;
                if( lhs.d_kind == Value::Ref || lhs.d_kind == Value::Tmp )
                {
                    rhs.d_slot = lhs.d_slot;
                    rhs.d_kind = Value::Pre;
                }else
                {
                    rhs.d_slot = ctx.back().buySlots(1);
                    rhs.d_kind = Value::Tmp;
                }
                rhs.d_line = c->d_loc.d_row;
                bc.TNEW( rhs.d_slot, 0, 0, rhs.d_line );
                initRecord( c->d_actuals.first()->d_type.data(), rhs.d_slot, rhs.d_line );

                assign(lhs,rhs);
            }
            return true;
        case BuiltIn::INC:
        case BuiltIn::DEC:
            {
                Value lhs;
                processExpr( c->d_actuals.first().data(), lhs );
                Q_ASSERT( lhs.d_kind != Value::Tmp );
                Ref<BinExpr> add = new BinExpr();
                add->d_lhs = c->d_actuals.first();
                add->d_type = c->d_actuals.first()->d_type;
                add->d_loc = c->d_actuals.first()->d_loc;
                if( bi->d_func == BuiltIn::INC )
                    add->d_op = BinExpr::ADD;
                else
                    add->d_op = BinExpr::SUB;
                if( c->d_actuals.size() == 1 )
                    add->d_rhs = new Literal(add->d_type.data(),add->d_loc,qlonglong(1));
                else
                {
                    Q_ASSERT( c->d_actuals.size() == 2 );
                    add->d_rhs = c->d_actuals.last();
                }
                assignExpr(lhs,add.data());
            }
            return true;
        case BuiltIn::ASSERT:
            {
                quint8 tmp = ctx.back().buySlots(4,true);
                fetchObnljMember(tmp,"ASSERT",c->d_loc.d_row);
                Value lhs;
                lhs.d_slot = tmp + 1;
                lhs.d_kind = Value::Ref;
                assignExpr(lhs, c->d_actuals.first().data() );
                bc.KSET(tmp + 2, QVariant::fromValue(mod->d_name), c->d_loc.d_row );
                bc.KSET(tmp + 3, c->d_loc.d_row, c->d_loc.d_row );
                bc.CALL(tmp,0,3,c->d_loc.d_row);
                ctx.back().sellSlots(tmp,4);
            }
            return true;
        case BuiltIn::INCL:
        case BuiltIn::EXCL:
            {
                Q_ASSERT( c->d_actuals.size() == 2 );
                quint8 tmp = ctx.back().buySlots(3, true);
                if( bi->d_func == BuiltIn::INCL )
                    fetchObnljMember(tmp,"INCL",c->d_loc.d_row);
                else
                    fetchObnljMember(tmp,"EXCL",c->d_loc.d_row);
                Value lhs;
                lhs.d_slot = tmp + 1;
                lhs.d_kind = Value::Ref;
                assignExpr(lhs, c->d_actuals.first().data() );
                lhs.d_slot = tmp + 2;
                lhs.d_kind = Value::Ref;
                assignExpr(lhs, c->d_actuals.last().data() );
                bc.CALL(tmp,0,2,c->d_loc.d_row);
                ctx.back().sellSlots(tmp,3);
            }
            return true;
        case BuiltIn::PACK:
            {
                Q_ASSERT( c->d_actuals.size() == 2 );
                VarParams vp(2);
                vp[0].d_isVarParam = true;
                prepareVarParams(c->d_actuals,vp);
                quint8 tmp = ctx.back().buySlots(3,true);
                fetchObnljMember(tmp,"PACK_NT",c->d_loc.d_row); // use non-thunk version
                emitCall(tmp,false,out,c->d_actuals, vp, c->d_loc.d_row );
            }
            return true;
        case BuiltIn::UNPK:
            {
                Q_ASSERT( c->d_actuals.size() == 2 );
                VarParams vp(2);
                vp[0].d_isVarParam = true;
                vp[1].d_isVarParam = true;
                prepareVarParams(c->d_actuals,vp);
                quint8 tmp = ctx.back().buySlots(3,true);
                fetchObnljMember(tmp,"UNPK_NT",c->d_loc.d_row);
                emitCall(tmp,false,out,c->d_actuals, vp, c->d_loc.d_row );
            }
            return true;
        case BuiltIn::WriteChar:
            {
                Q_ASSERT( c->d_actuals.size() == 1 );
                quint8 tmp = ctx.back().buySlots(2,true);
                bc.GGET(tmp, "Out", c->d_loc.d_row );
                bc.TGET(tmp, tmp,"Char", c->d_loc.d_row );
                emitCall(tmp,false,out,c->d_actuals, c->d_loc.d_row );
            }
            return true;
        case BuiltIn::WriteInt:
            {
                Q_ASSERT( c->d_actuals.size() == 1 );
                quint8 tmp = ctx.back().buySlots(3,true);
                bc.GGET(tmp, "Out", c->d_loc.d_row );
                bc.TGET(tmp, tmp,"Int", c->d_loc.d_row );
                CallExpr::Actuals acts = c->d_actuals;
                acts.append( new Literal(0,c->d_loc, qlonglong(4) ) );
                emitCall(tmp,false,out,acts, c->d_loc.d_row );
            }
            return true;
        case BuiltIn::WriteReal:
            {
                Q_ASSERT( c->d_actuals.size() == 1 );
                quint8 tmp = ctx.back().buySlots(3,true);
                bc.GGET(tmp, "Out", c->d_loc.d_row );
                bc.TGET(tmp, tmp,"Real", c->d_loc.d_row );
                CallExpr::Actuals acts = c->d_actuals;
                acts.append( new Literal(0,c->d_loc, qlonglong(4) ) );
                emitCall(tmp,false,out,acts, c->d_loc.d_row );
            }
            return true;
        case BuiltIn::WriteLn:
            {
                Q_ASSERT( c->d_actuals.size() == 0 );
                quint8 tmp = ctx.back().buySlots(1,true);
                bc.GGET(tmp, "Out", c->d_loc.d_row );
                bc.TGET(tmp, tmp,"Ln", c->d_loc.d_row );
                emitCall(tmp,false,out,c->d_actuals, c->d_loc.d_row );
            }
            return true;

        // Expressions:
        case BuiltIn::ORD:
            {
                Q_ASSERT( c->d_actuals.size() == 1 );
                quint8 tmp = ctx.back().buySlots(2,true);
                fetchObnljMember(tmp,"ORD",c->d_loc.d_row);
                emitCall(tmp,true,out,c->d_actuals, c->d_loc.d_row );
            }
            return true;
        case BuiltIn::CHR:
            {
                Q_ASSERT( c->d_actuals.size() == 1 );
                quint8 tmp = ctx.back().buySlots(2,true);
                fetchObnljMember(tmp,"Char",c->d_loc.d_row);
                emitCall(tmp,true,out,c->d_actuals, c->d_loc.d_row );
            }
            return true;
        case BuiltIn::ODD:
            {
                Q_ASSERT( c->d_actuals.size() == 1 );
                // out << " % 2 == 1";
                Ref<BinExpr> mod = new BinExpr();
                mod->d_lhs = c->d_actuals.first();
                mod->d_loc = c->d_actuals.first()->d_loc;
                mod->d_op = BinExpr::MOD;
                mod->d_rhs = new Literal(0,mod->d_loc,qlonglong(2));
                Ref<BinExpr> eq = new BinExpr();
                eq->d_lhs = mod.data();
                eq->d_loc = mod->d_loc;
                eq->d_op = BinExpr::EQ;
                eq->d_rhs = new Literal(0,mod->d_loc,qlonglong(1));
                processExpr(eq.data(),out);
            }
            return true;
        case BuiltIn::ABS:
            {
                Q_ASSERT( c->d_actuals.size() == 1 );
                quint8 tmp = ctx.back().buySlots(2,true);
                bc.GGET(tmp, "math", c->d_loc.d_row );
                bc.TGET(tmp, tmp,"abs", c->d_loc.d_row );
                emitCall(tmp,true,out,c->d_actuals, c->d_loc.d_row );
            }
            return true;
        case BuiltIn::LEN:
            {
                Q_ASSERT( c->d_actuals.size() == 1 );
                Value id;
                processExpr(c->d_actuals.first().data(),id);
                derefIndexed(id);
                Q_ASSERT( id.d_kind == Value::Tmp || id.d_kind == Value::Ref );
                if( out.d_kind == Value::Pre )
                {
                    bc.TGET( out.d_slot, id.d_slot, QVariant::fromValue(QByteArray("n")), c->d_loc.d_row );
                    sell(id);
                }else if( id.d_kind == Value::Tmp )
                {
                    out.d_slot = id.d_slot;
                    out.d_kind = Value::Tmp;
                    bc.TGET( out.d_slot, id.d_slot, QVariant::fromValue(QByteArray("n")), c->d_loc.d_row );
                }else
                {
                    out.d_slot = ctx.back().buySlots(1);
                    out.d_kind = Value::Tmp;
                    bc.TGET( out.d_slot, id.d_slot, QVariant::fromValue(QByteArray("n")), c->d_loc.d_row );
                }
            }
            return true;
        case BuiltIn::LSL:
            {
                Q_ASSERT( c->d_actuals.size() == 2 );
                quint8 tmp = ctx.back().buySlots(3,true);
                bc.GGET(tmp, "bit", c->d_loc.d_row );
                bc.TGET(tmp, tmp,"lshift", c->d_loc.d_row );
                emitCall(tmp,true,out,c->d_actuals, c->d_loc.d_row );
            }
            return true;
        case BuiltIn::ASR:
            {
                Q_ASSERT( c->d_actuals.size() == 2 );
                quint8 tmp = ctx.back().buySlots(3,true);
                bc.GGET(tmp, "bit", c->d_loc.d_row );
                bc.TGET(tmp, tmp,"arshift", c->d_loc.d_row );
                emitCall(tmp,true,out,c->d_actuals, c->d_loc.d_row );
            }
            return true;
        case BuiltIn::ROR:
            {
                Q_ASSERT( c->d_actuals.size() == 2 );
                quint8 tmp = ctx.back().buySlots(3,true);
                bc.GGET(tmp, "bit", c->d_loc.d_row );
                bc.TGET(tmp, tmp,"ror", c->d_loc.d_row );
                emitCall(tmp,true,out,c->d_actuals, c->d_loc.d_row );
            }
            return true;
        case BuiltIn::FLOOR:
            {
                Q_ASSERT( c->d_actuals.size() == 1 );
                quint8 tmp = ctx.back().buySlots(2,true);
                bc.GGET(tmp, "math", c->d_loc.d_row );
                bc.TGET(tmp, tmp,"floor", c->d_loc.d_row );
                emitCall(tmp,true,out,c->d_actuals, c->d_loc.d_row );
            }
            return true;
        case BuiltIn::FLT:
            Q_ASSERT( c->d_actuals.size() == 1 );
            processExpr(c->d_actuals.first().data(), out);
            if( out.d_kind == Value::Val )
                out.d_val = double(out.d_val.toInt() );
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

    void emitEndOfCall(quint8 base, int baseLen, Value& out, int line )
    {
        if( out.d_kind == Value::Pre )
        {
            bc.MOV( out.d_slot, base, line );
            ctx.back().sellSlots( base, baseLen );
        }else
        {
            out.d_slot = base;
            out.d_kind = Value::Tmp;
            ctx.back().sellSlots( base + 1, baseLen - 1 );
        }
    }

    typedef QVector<Value> VarParams;

    void prepareVarParams( const CallExpr::Actuals& actuals, VarParams& vp )
    {
        // we need to allocate temp slots for VAR params before the call because
        // if the temp slots would come after the call slots the would be modified
        // by the call
        Q_ASSERT( vp.size() == actuals.size() );

        for( int i = 0; i < actuals.size(); i++ )
        {
            if( vp[i].d_isVarParam )
            {
                Value& rhs = vp[i];
                process( actuals[i].data(), rhs );
                Q_ASSERT( rhs.d_kind == Value::Tmp || rhs.d_kind == Value::Ref ||
                            rhs.d_kind == Value::Tmp2 || rhs.d_kind == Value::Tmp2v ||
                          rhs.d_kind == Value::Ref2 || rhs.d_kind == Value::Ref2v || rhs.d_kind == Value::Uv );
            }
        }
    }

    void emitCall( quint8 base, bool hasReturn, Value& out,
                   const CallExpr::Actuals& actuals, int line )
    {
        VarParams vp;
        emitCall(base, hasReturn, out, actuals, vp, line );
    }

    void emitCall( quint8 base, bool hasReturn, Value& out,
                   const CallExpr::Actuals& actuals, VarParams& vp, int line )
    {
        Q_ASSERT( vp.isEmpty() || vp.size() == actuals.size() );

        int vpCount = 0;
        for( int i = 0; i < actuals.size(); i++ )
        {
            Value lhs;
            lhs.d_slot = base + i + 1;
            lhs.d_kind = Value::Ref;
            lhs.d_isParam = true;

            if( i < vp.size() )
            {
                lhs.d_type = vp[i].d_type;
                lhs.d_isVarParam = vp[i].d_isVarParam;
            }

            if( i < vp.size() && vp[i].d_isVarParam )
            {
                vpCount++;
                assign( lhs, vp[i], true );
            }else
                assignExpr(lhs, actuals[i].data() );
        }

        const int off = hasReturn ? 1 : 0;
        bc.CALL( base, off+vpCount, actuals.size(), line );

        for( int i = 0; i < vp.size(); i++ )
        {
            Value& v = vp[i];
            switch( v.d_kind )
            {
            case Value::Tmp2:
            case Value::Ref2:
                bc.TSET( base + off + i, v.d_slot, v.d_idx, v.d_line );
                break;
            case Value::Tmp2v:
            case Value::Ref2v:
                bc.TSET( base + off + i, v.d_slot, v.d_val, v.d_line );
                break;
            case Value::Tmp:
            case Value::Ref:
                bc.MOV(v.d_slot, base + off + i, v.d_line );
                break;
            }

            sell(v);
        }

        if( hasReturn )
            emitEndOfCall(base, actuals.size() + 1, out, line );
        else
            ctx.back().sellSlots( base, actuals.size() + 1 );
    }

    void processCallExpr( CallExpr* e, Value& out )
    {

        ProcType* pt = e->getProcType();

        if( pt->isBuiltIn() && genBuiltIn( e, out ) )
            return;

        VarParams vp(pt->d_formals.size());
        for( int i = 0; i < pt->d_formals.size(); i++ )
        {
            vp[i].d_type = pt->d_formals[i]->d_type.data();
            vp[i].d_isVarParam = pt->d_formals[i]->d_var;
            vp[i].d_isParam = true;
        }
        prepareVarParams(e->d_actuals,vp);
        // NOTE: slots for multi-return helpers (VAR params) must be allocated before call slots
        // are allocated because the call modifies slots after the call slots.

        quint8 tmp = ctx.back().buySlots(pt->d_formals.size() + 1,true);

        Value lhs;
        lhs.d_slot = tmp;
        lhs.d_kind = Value::Pre;
        processExpr( e->d_sub.data(), lhs );

        derefIndexed(lhs);
        Q_ASSERT( lhs.d_kind == Value::Ref || lhs.d_kind == Value::Tmp || lhs.d_kind == Value::Pre );

        if( lhs.d_kind != Value::Pre )
        {
            bc.MOV( tmp, lhs.d_slot, e->d_loc.d_row );
            sell(lhs);
        }

        emitCall(tmp,!pt->d_return.isNull(),out,e->d_actuals, vp, e->d_loc.d_row );
    }
};

bool LjbcGen::translate(Ast::Model* mdl, const QString& outdir, const QString& mod, Errors* err)
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

        QFile out( dir.absoluteFilePath( LuaGen2::toName(mods[i].data()) + ".lua" ) );
        if( !out.open(QIODevice::WriteOnly) )
        {
            errs++;
            if( err )
                err->error(Errors::Generator,LuaGen2::toName(mods[i].data()), 0,0,QString("cannot open file '%1' for writing").
                       arg(out.fileName()) );
        }else
        {
            if( !translate(mods[i].data(),&out,err) )
                errs++;
        }
    }
    return errs == 0;
}

bool LjbcGen::translate(Ast::Module* m, QIODevice* out, Errors* errs)
{

    Q_ASSERT( m != 0 && out != 0 );

    if( m->d_hasErrors )
        return false;

    if( m->d_isDef )
        return true;

    LjbcGenImp imp;
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


    try
    {
        m->accept(&imp);
    }catch( const LjbcGenImp::NoMoreFreeSlots& )
    {
        Q_ASSERT( !imp.ctx.isEmpty() );
        imp.err->error(Errors::Generator, m->d_file, imp.ctx.back().scope->d_loc.d_row, 1,
                       QString("run out of slots in scope from line %1 to %2")
                       .arg(imp.ctx.back().scope->d_loc.d_row).arg(imp.ctx.back().scope->d_end.d_row) );
    }

    const bool hasErrs = ( imp.err->getErrCount() - errCount ) != 0;

    //imp.bc.setStripped(true);
    if( !hasErrs )
        imp.bc.write(out);

    if( imp.ownsErr )
        delete imp.err;
    return hasErrs;

}
