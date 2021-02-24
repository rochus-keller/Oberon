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

#include "ObxLjbcGen.h"
#include "ObRowCol.h"
#include "ObErrors.h"
#include "ObxModel.h"
#include <LjTools/LuaJitComposer.h>
#include <QDir>
#include <QtDebug>
using namespace Obx;
using namespace Ob;
using namespace Lua;

#ifndef OBX_AST_DECLARE_SET_METATYPE_IN_HEADER
Q_DECLARE_METATYPE( Obx::Literal::SET )
#endif

struct ObxLjbcGenImp : public AstVisitor
{
    // This version of the code generator (in contrast to ObLjbcGen) uses a register for each expression evaluation to
    // transfer the result back to caller; I assume that the tracer/optimizer is able to get rid of the redundant moves;
    // the code generator/allocator is much simpler to implement on the other hand.
    // This implementation also tries to replace names by indexes (magic numbers) which are ~30% faster than hash
    // lookup; the magic numbers are in sync with obxlj.lua.

    struct NoMoreFreeSlots {};
    enum { MAX_PROC_SLOTS = 230 };

    struct ProcsCollector : public AstVisitor
    {
        QList<Procedure*> allProcs;
        QList<Record*> allRecords;

        void checkRecord(Type* t)
        {
            if( t->getTag() == Thing::T_Pointer )
                t = cast<Pointer*>(t)->d_to.data(); // no deref, intentionally
            if( t && t->getTag() == Thing::T_Record )
                allRecords.append( cast<Record*>(t) );
        }

        void checkRecord(Named* n)
        {
            NamedType* nt = cast<NamedType*>(n);
            Q_ASSERT( nt->d_type );
            // we're only interested in non-alias types here, so no derefed()
            checkRecord(nt->d_type.data());
        }

        void visit( Module* me )
        {
            foreach( const Ref<Named>& n, me->d_order )
            {
                const int tag = n->getTag();
                if( tag == Thing::T_Procedure )
                {
                    Procedure* p = cast<Procedure*>(n.data());
                    if( p->d_receiver.isNull() )
                        allProcs.append(cast<Procedure*>(n.data()));
                    n->accept(this);
                }else if( tag == Thing::T_NamedType )
                    checkRecord(n.data());
                // TODO: local types in var, typedefs und params
            }
        }

        void visit( Procedure* me)
        {
            foreach( const Ref<Named>& n, me->d_order )
            {
                const int tag = n->getTag();
                if( tag == Thing::T_Procedure )
                {
                    Procedure* p = cast<Procedure*>(n.data());
                    Q_ASSERT( p->d_receiver.isNull() );
                    allProcs.append(p);
                    n->accept(this); // all procs go to toplevel
                }else if( tag == Thing::T_NamedType )
                    checkRecord(n.data());
            }
        }
    };

    struct Ctx
    {
        Scope* scope;
        JitComposer::SlotPool pool;
        // module is the top level proc and each other proc is a sub-proc of module; there are no sub-sub-procs
        typedef QHash<quint8,QPair<quint16,QByteArray> > Upvals; // slot -> upval; only for sub-procs; slot is in module and read-only
        Upvals upvals;
        Ctx(Scope* s = 0):scope(s) { }

        int buySlots(int len = 1, bool call = false )
        {
            const int tmp = JitComposer::nextFreeSlot(pool,len, call );
            if( tmp < 0 )
                throw NoMoreFreeSlots();
            return tmp;
        }
        void sellSlots(quint8 base, int len = 1 )
        {
            // qDebug() << "sell" << base << len;
            JitComposer::releaseSlot(pool,base, len );
        }
        quint16 resolveUpval(Named* n)
        {
            return resolveUpval(n->d_slot,n->d_name);
        }
        quint16 resolveUpval(quint8 slot, const QByteArray& name)
        {
            Upvals::const_iterator i = upvals.find(slot);
            if( i != upvals.end() )
                return i.value().first;
            const int nr = upvals.size();
            QPair<quint16,QByteArray>& uv = upvals[ slot ];
            uv.first = nr;
            uv.second = name;
            return nr;
        }
    };

    QList<Ctx> ctx;
    JitComposer bc;
    Errors* err;
    Module* mod;
    bool ownsErr;
    quint8 modSlot, obxlj, curClass;
    QList<quint8> slotStack;
    QList <quint32> exitJumps;


    void visit( Module* me)
    {
        ctx.push_back( Ctx(me) );
        bc.openFunction(0,me->d_file.toUtf8(),me->d_loc.packed(), me->d_end.packed() );

        QHash<quint8,QByteArray> names;
        modSlot = ctx.back().buySlots(1);
        names[modSlot] = "@mod";
        obxlj = ctx.back().buySlots(1);
        names[obxlj] = "@obxlj";

        foreach( Import* imp, me->d_imports )
        {
            if(imp->d_mod->d_synthetic )
                continue; // ignore SYSTEM
            imp->setSlot( ctx.back().buySlots(1) ); // procs are stored as indices in the module table as well as in the slots
            names[imp->d_slot] = imp->d_name;
        }

        ProcsCollector pc;
        me->accept(&pc);
        bool slotsFinished = false;
        quint32 slotNr = 0;
        foreach( Procedure* p, pc.allProcs )
        {
            Q_ASSERT( p->d_receiver.isNull() );
            // bound procs are stored in the class objects instead

            if( !slotsFinished )
            {
                p->setSlot( ctx.back().buySlots(1) ); // procs are stored as indices in the module table
                                                    // as well as in the slots up to MAX_PROC_SLOTS
                names[p->d_slot] = p->d_name; // more than one proc/slot can have the same name
                if( p->d_slot >= MAX_PROC_SLOTS )
                {
                    slotsFinished = true;
                    slotNr = ctx.back().pool.d_frameSize + 1;
                }
            }else
            {
                p->setSlot( slotNr++ );
                // vars are not stored in slots; their slot nr can grow > 255
            }
            allocateLocals(p);
        }

        if( !slotsFinished )
            slotNr = ctx.back().pool.d_frameSize + 1; // var and proc slots don't overlap
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Variable )
            {
                n->setSlot(slotNr++);
                // vars are not stored in slots; their slot nr can grow > 255
            }
        }

        foreach( Record* r, pc.allRecords )
            allocateClasses(r,slotNr);

        Q_ASSERT( modSlot == 0 );
        bc.TNEW( modSlot, ctx.back().pool.d_frameSize, 0, me->d_loc.packed() );

        emitImport( "obxlj", obxlj, me->d_loc );

        foreach( Import* imp, me->d_imports )
            imp->accept(this);

        foreach( Procedure* p, pc.allProcs )
            p->accept(this);

        curClass = ctx.back().buySlots(1);
        foreach( Record* r, pc.allRecords )
            allocateClassTables(r,me->d_loc);
        ctx.back().sellSlots(curClass);

        bc.UCLO( modSlot, 0, me->d_end.packed() );

        // the module body is in a synthetic procedure so that upvalue access is uniform
        Procedure modBody;
        modBody.d_type = new ProcType();
        modBody.d_body = me->d_body;
        modBody.d_scope = me;
        modBody.d_loc = me->d_loc;
        modBody.d_end = me->d_end;
        modBody.accept(this);
        int tmp = ctx.back().buySlots(1,true);
        bc.FNEW( tmp, modBody.d_slot, me->d_end.packed() );
        bc.CALL(tmp,0,0,me->d_end.packed());
        ctx.back().sellSlots(tmp);

        // make Module table a global variable
        bc.GSET( modSlot, me->d_name, me->d_end.packed() );
        bc.RET( modSlot, 1, me->d_end.packed() ); // return module

        JitComposer::VarNameList sn(ctx.back().pool.d_frameSize);
        QHash<quint8,QByteArray>::const_iterator vi;
        for( vi = names.begin(); vi != names.end(); ++vi )
        {
            JitComposer::VarName& n = sn[vi.key()];
            n.d_name = vi.value();
            n.d_to = bc.getCurPc();
        }
        bc.setVarNames( sn );
        bc.closeFunction(ctx.back().pool.d_frameSize);
        ctx.pop_back();
    }

    void visit( Procedure* me)
    {
        ctx.push_back( Ctx(me) );
        const int id = bc.openFunction(me->getProcType()->d_formals.size(),
                                       me->d_name,me->d_loc.packed(), me->d_end.packed() );
        Q_ASSERT( id >= 0 );

        QHash<quint8,QByteArray> names;
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Parameter )
            {
                int slot = ctx.back().buySlots(1);
                Q_ASSERT( n->d_slotValid && n->d_slot == slot );
                names[n->d_slot] = n->d_name;
            }
        }

        foreach( const Ref<Named>& n, me->d_order )
        {
            const int tag = n->getTag();
            if( tag == Thing::T_LocalVar )
            {
                int slot = ctx.back().buySlots(1);
                Q_ASSERT( n->d_slotValid && n->d_slot == slot );
                names[n->d_slot] = n->d_name;
            }
        }

        foreach( const Ref<Statement>& s, me->d_body )
            s->accept(this);

        bc.RET( 0, 1, me->d_end.packed() ); // return module

        JitComposer::VarNameList sn(ctx.back().pool.d_frameSize);
        QHash<quint8,QByteArray>::const_iterator vi;
        for( vi = names.begin(); vi != names.end(); ++vi )
        {
            JitComposer::VarName& n = sn[vi.key()];
            n.d_name = vi.value();
            n.d_to = bc.getCurPc();
        }
        bc.setVarNames( sn );
        JitComposer::UpvalList uv(ctx.back().upvals.size());
        Ctx::Upvals::const_iterator uvi;
        for( uvi = ctx.back().upvals.begin(); uvi != ctx.back().upvals.end(); ++uvi )
        {
            JitComposer::Upval u;
            u.d_name = uvi.value().second;
            u.d_isRo = true;
            u.d_isLocal = true;
            u.d_uv = uvi.key();
            uv[uvi.value().first] = u;
        }
        bc.setUpvals(uv);
        bc.closeFunction(ctx.back().pool.d_frameSize);
        ctx.pop_back();

        if( me->d_slotValid )
        {
            if( me->d_receiver.isNull() )
            {
                if( me->d_slot > MAX_PROC_SLOTS )
                {
                    int tmp = ctx.back().buySlots(1);
                    bc.FNEW( tmp, id, me->d_end.packed() );
                    bc.TSET( tmp, modSlot, me->d_name, me->d_end.packed() );
                    ctx.back().sellSlots(tmp);
                }else
                {
                    bc.FNEW( me->d_slot, id, me->d_end.packed() );
                    bc.TSET( me->d_slot, modSlot, me->d_name, me->d_end.packed() );
                }
            }else
            {
                int tmp = ctx.back().buySlots(1);
                bc.FNEW( tmp, id, me->d_end.packed() );
                emitSetTableByIndex( tmp, curClass, me->d_slot, me->d_end );
                ctx.back().sellSlots(tmp);
            }
        }else
            me->d_slot = id; // in case of module body
    }

    void visit( Import* me)
    {
        emitImport( me->d_name, me->d_slot, me->d_aliasPos.isValid() ? me->d_aliasPos : me->d_loc );
    }

    void visit( Variable* me)
    {
        emitInitializer(me);
    }

    void visit( LocalVar* me)
    {
        emitInitializer(me);
    }

    void visit( Call* me)
    {
        Q_ASSERT( me->d_what );
        me->d_what->accept(this);
    }

    void visit( Return* me )
    {
        // TODO: handle multiple returns for var params
        if( me->d_what )
        {
            me->d_what->accept(this);
            bc.RET(slotStack.back(),1,me->d_loc.packed());
            releaseSlot();
        }else
            bc.RET(me->d_loc.packed());
    }

    void visit( Assign* me )
    {
        Q_ASSERT( me->d_rhs );
        me->d_rhs->accept(this);
        Q_ASSERT( slotStack.size() >= 1 );

        Q_ASSERT( me->d_lhs );

        // TODO: copy for structured assignment by value

        const int unop = me->d_lhs->getUnOp();
        if( unop == UnExpr::SEL )
        {
            Q_ASSERT( me->d_lhs->getTag() == Thing::T_IdentSel );
            IdentSel* sel = cast<IdentSel*>(me->d_lhs.data());
            Named* id = sel->getIdent();
            if( !checkValidSlot(id,me->d_loc) )
                return;
            Q_ASSERT( id && id->d_slotValid );
            sel->d_sub->accept(this);
            Q_ASSERT( slotStack.size() >= 2 );
            emitSetTableByIndex( slotStack[slotStack.size()-2], slotStack.back(), id->d_slot, me->d_loc );
            releaseSlot(); // lhs
        }else if( unop == UnExpr::IDX )
        {
            Q_ASSERT( me->d_lhs->getTag() == Thing::T_ArgExpr );
            ArgExpr* args = cast<ArgExpr*>( me->d_lhs.data() );
            Q_ASSERT( args->d_args.size() == 1 );
            args->d_sub->accept(this);
            args->d_args.first()->accept(this);
            Q_ASSERT( slotStack.size() >= 3 ); // ...rhs, table, index
            bc.TSET( slotStack[slotStack.size()-3], slotStack[slotStack.size()-2], slotStack[slotStack.size()-1],
                    me->d_loc.packed() );
            releaseSlot(); // index
            releaseSlot(); // table
        }else if( me->d_lhs->getTag() == Thing::T_IdentLeaf )
        {
            Named* n = me->d_lhs->getIdent();
            if( !checkValidSlot( n, me->d_loc ) )
                return;
            switch( n->getTag() )
            {
            case Thing::T_Variable:
                {
                    Q_ASSERT( ctx.back().scope != mod );
                    int tmp = ctx.back().buySlots(1);
                    fetchModule(tmp,me->d_loc);
                    emitSetTableByIndex(slotStack.back(), tmp, n->d_slot, me->d_loc );
                    ctx.back().sellSlots(tmp);
                }
                break;
            case Thing::T_Parameter:
            case Thing::T_LocalVar:
                if( n->d_scope == ctx.back().scope )
                    bc.MOV( n->d_slot, slotStack.back(), me->d_loc.packed() );
                else
                    qWarning() << "non-local access not yet implemented";
                break;
            }
        }else
        {
            qDebug() << "ERR" << mod->d_name << me->d_loc.d_row << me->d_loc.d_col;
            Q_ASSERT( false );
        }

        releaseSlot(); // rhs
    }

    void visit( ForLoop* me)
    {
        // i := from;
        // WHILE i <= to DO statements; i := i + by END
        // WHILE i >= to DO statements; i := i + by END

        Q_ASSERT( me->d_id );
        Named* i = me->d_id->getIdent();
        Q_ASSERT( i && i->d_slotValid );

        Q_ASSERT( me->d_from );
        me->d_from->accept(this);
        Q_ASSERT( slotStack.size() >= 1 );
        bc.MOV( i->d_slot, slotStack.back(), me->d_loc.packed() ); // i := from
        releaseSlot(); // from

        bc.LOOP( ctx.back().pool.d_frameSize, 0, me->d_loc.packed() ); // while
        const quint32 loopStart = bc.getCurPc();

        Q_ASSERT( me->d_to );
        me->d_to->accept(this);
        Q_ASSERT( slotStack.size() >= 1 );

        const int inc = me->d_byVal.toInt();
        if( inc > 0 )
            bc.ISGT(i->d_slot,slotStack.back(),me->d_loc.packed()); // i <= to, inverted so that jump to end if true
        else
            bc.ISLT(i->d_slot,slotStack.back(),me->d_loc.packed()); // i >= to, dito
        releaseSlot(); // to

        bc.JMP( ctx.back().pool.d_frameSize, 0, me->d_loc.packed() );
        const quint32 jumpToEnd = bc.getCurPc();

        // do
        foreach( const Ref<Statement>& s, me->d_do ) // statements
            s->accept(this);
        bc.ADD( i->d_slot, i->d_slot, me->d_byVal, me->d_loc.packed() ); // i := i + by

        bc.JMP( ctx.back().pool.d_frameSize, loopStart - bc.getCurPc() - 2, me->d_loc.packed() ); // end while
        bc.patch(loopStart);
        bc.patch(jumpToEnd);
    }

    void visit( IfLoop* me)
    {
        switch( me->d_op )
        {
        case IfLoop::IF:
            emitIf(me);
            break;
        case IfLoop::WHILE:
            emitWhile(me);
            break;
        case IfLoop::REPEAT:
            emitRepeat(me);
            break;
        case IfLoop::WITH:
            emitWith(me);
            break;
        case IfLoop::LOOP:
            emitLoop(me);
            break;
        }
    }

    void visit( Exit* me)
    {
        bc.JMP( ctx.back().pool.d_frameSize, 0, me->d_loc.packed() );
        exitJumps << bc.getCurPc();
    }

    void visit( CaseStmt* me)
    {
        if( me->d_typeCase )
            emitTypeCase(me);
        else
            emitPlainCase(me);
    }

    void visit( Literal* me)
    {
        const int res = ctx.back().buySlots(1);
        emitConst( res, me->d_vtype, me->d_val, me->d_loc );
        slotStack.push_back(res);
    }

    void visit( SetExpr* me)
    {
        const int res = ctx.back().buySlots(1);

        bc.KSET(res, 0, me->d_loc.packed() );

        quint8 addElemToSet = ctx.back().buySlots(1);
        fetchObxlibMember(addElemToSet,9,me->d_loc);
        quint8 addRangeToSet = ctx.back().buySlots(1);
        fetchObxlibMember(addRangeToSet,10,me->d_loc);

        for( int i = 0; i < me->d_parts.size(); i++ )
        {
            BinExpr* bi = me->d_parts[i]->getTag() == Thing::T_BinExpr ? cast<BinExpr*>( me->d_parts[i].data() ) : 0;
            if( bi && bi->d_op == BinExpr::Range )
            {
                int tmp = ctx.back().buySlots(4,true);
                bc.MOV(tmp, addRangeToSet, me->d_loc.packed() );
                bc.MOV(tmp+1, res, me->d_loc.packed() );
                if( bi->d_lhs )
                {
                    bi->d_lhs->accept(this);
                    bc.MOV(tmp+2, slotStack.back(), me->d_loc.packed() );
                    releaseSlot();
                }
                if( bi->d_rhs )
                {
                    bi->d_rhs->accept(this);
                    bc.MOV(tmp+3, slotStack.back(), me->d_loc.packed() );
                    releaseSlot();
                }
                bc.CALL(tmp,1,3, me->d_loc.packed() );
                bc.MOV(res, tmp, me->d_loc.packed() );
                ctx.back().sellSlots(tmp,4);
            }else
            {
                int tmp = ctx.back().buySlots(3,true);
                bc.MOV(tmp, addElemToSet, me->d_loc.packed() );
                bc.MOV(tmp+1, res, me->d_loc.packed() );
                me->d_parts[i]->accept(this);
                bc.MOV(tmp+2, slotStack.back(), me->d_loc.packed() );
                releaseSlot();
                bc.CALL(tmp,1,2, me->d_loc.packed() );
                bc.MOV(res, tmp, me->d_loc.packed() );
                ctx.back().sellSlots(tmp,3);
            }
        }
        ctx.back().sellSlots(addElemToSet);
        ctx.back().sellSlots(addRangeToSet);
        slotStack.push_back(res);
    }

    void visit( UnExpr* me)
    {
        Q_ASSERT( !me->d_sub.isNull() );

        me->d_sub->accept(this);

        Q_ASSERT( !me->d_sub->d_type.isNull() );
        // prev must be a pointer or a record
        Type* prevT = me->d_sub->d_type->derefed();
        Q_ASSERT( prevT );

        const int res = ctx.back().buySlots(1);

        switch( me->d_op )
        {
        case UnExpr::NEG:
            if( prevT->getBaseType() == BaseType::SET )
            {
                int tmp = ctx.back().buySlots(2,true);
                fetchObxlibMember(tmp,11,me->d_loc); // bit.bnot
                bc.MOV(tmp+1,slotStack.back(),me->d_loc.packed());
                bc.CALL(tmp,1,1,me->d_loc.packed());
                bc.MOV(res,tmp,me->d_loc.packed());
                ctx.back().sellSlots(tmp,2);
            }else
            {
                Q_ASSERT( prevT->isNumeric() );
                bc.UNM(res,slotStack.back(),me->d_loc.packed());
            }
            break;
        case UnExpr::NOT:
            bc.NOT(res,slotStack.back(),me->d_loc.packed());
            break;
        case UnExpr::DEREF:
            // NOP
            bc.MOV(res,slotStack.back(),me->d_loc.packed());
            break;
        default:
            qDebug() << "ERR" << mod->d_name << me->d_loc.d_row << me->d_loc.d_col;
            Q_ASSERT( false );
            break;
        }
        releaseSlot();
        slotStack.push_back(res);
    }

    void visit( BinExpr* me)
    {
        Q_ASSERT( !me->d_lhs.isNull() && !me->d_rhs.isNull() &&
                  !me->d_lhs->d_type.isNull() && !me->d_rhs->d_type.isNull() );
        me->d_lhs->accept(this);

        if( me->d_op != BinExpr::AND && me->d_op != BinExpr::OR )
        {
            // AND and OR are special in that rhs might not be executed
            me->d_rhs->accept(this);
            Q_ASSERT( slotStack.size() >= 2 ); // running lhs and rhs should have produced two results/registers
        }else
            Q_ASSERT( slotStack.size() >= 1 );

        Type* lhsT = me->d_lhs->d_type.isNull() ? 0 : me->d_lhs->d_type->derefed();
        Type* rhsT = me->d_rhs->d_type.isNull() ? 0 : me->d_rhs->d_type->derefed();
        Q_ASSERT( lhsT && rhsT );
        const int ltag = lhsT->getTag();
        const int rtag = rhsT->getTag();
        bool lwide, rwide;

        const int res = ctx.back().buySlots(1);

        switch( me->d_op )
        {
        case BinExpr::IN:
            if( lhsT->isInteger() && rhsT->getBaseType() == Type::SET )
                setOp( res, 21, me->d_loc ); // module.setTest
            else
                Q_ASSERT(false);
            break;
        case BinExpr::IS:
            {
                int tmp = ctx.back().buySlots(3,true);
                fetchObxlibMember(tmp,23,me->d_loc); // module.is_a
                bc.MOV(tmp+1,slotStack[slotStack.size()-2],me->d_loc.packed());
                bc.MOV(tmp+2, slotStack.back(), me->d_loc.packed() );
                bc.CALL(tmp,1,2,me->d_loc.packed());
                bc.MOV(res,tmp,me->d_loc.packed());
                ctx.back().sellSlots(tmp,3);
            }
            break;
        case BinExpr::ADD:
            if( lhsT->isNumeric() && rhsT->isNumeric() )
                bc.ADD(res, slotStack[slotStack.size()-2], slotStack.back(), me->d_loc.packed() );
            else if( lhsT->isSet() && rhsT->isSet() )
                setOp( res, 12, me->d_loc ); // bit.bor
            else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                checkHandleChar(lhsT,rhsT,lwide,rwide,me->d_loc);
                int tmp = ctx.back().buySlots(4,true);
                fetchObxlibMember(tmp,13,me->d_loc); // module.joinStrings
                bc.MOV(tmp+1,slotStack[slotStack.size()-2],me->d_loc.packed());
                bc.MOV(tmp+2, slotStack.back(), me->d_loc.packed() );
                bc.KSET(tmp+3, lwide || rwide );
                bc.CALL(tmp,1,3,me->d_loc.packed());
                bc.MOV(res,tmp,me->d_loc.packed());
                ctx.back().sellSlots(tmp,4);
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::SUB:
            if( lhsT->isNumeric() && rhsT->isNumeric() )
                bc.SUB(res, slotStack[slotStack.size()-2], slotStack.back(), me->d_loc.packed() );
            else if( lhsT->isSet() && rhsT->isSet() )
                setOp( res, 18, me->d_loc ); // module.setSub
            else
                Q_ASSERT(false);
            break;
        case BinExpr::FDIV:
            if( lhsT->isNumeric() && rhsT->isNumeric() )
                bc.DIV(res, slotStack[slotStack.size()-2], slotStack.back(), me->d_loc.packed() );
            else if( lhsT->isSet() && rhsT->isSet() )
                setOp( res, 20, me->d_loc ); // module.setDiv
            break;
        case BinExpr::MUL:
            if( lhsT->isNumeric() && rhsT->isNumeric() )
                bc.MUL(res, slotStack[slotStack.size()-2], slotStack.back(), me->d_loc.packed() );
            else if( lhsT->isSet() && rhsT->isSet() )
                setOp( res, 19, me->d_loc ); // bit.band
            else
                Q_ASSERT(false);
            break;
        case BinExpr::DIV:
            if( lhsT->isNumeric() && rhsT->isNumeric() )
            {
                int tmp = ctx.back().buySlots(3,true);
                fetchObxlibMember(tmp,14,me->d_loc); // ObxFfi_DIV
                bc.MOV(tmp+1, slotStack[slotStack.size()-2], me->d_loc.packed() );
                bc.MOV(tmp+2, slotStack.back(), me->d_loc.packed() );
                bc.CALL(tmp,1,2,me->d_loc.packed());
                bc.MOV(res,tmp,me->d_loc.packed());
                ctx.back().sellSlots(tmp,3);
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::MOD:
            if( lhsT->isNumeric() && rhsT->isNumeric() )
            {
                int tmp = ctx.back().buySlots(3,true);
                fetchObxlibMember(tmp,15,me->d_loc); // ObxFfi_MOD
                bc.MOV(tmp+1, slotStack[slotStack.size()-2], me->d_loc.packed() );
                bc.MOV(tmp+2, slotStack.back(), me->d_loc.packed() );
                bc.CALL(tmp,1,2,me->d_loc.packed());
                bc.MOV(res,tmp,me->d_loc.packed());
                ctx.back().sellSlots(tmp,3);
            }
            break;
        case BinExpr::AND:
            if( lhsT->getBaseType() == Type::BOOLEAN && rhsT->getBaseType() == Type::BOOLEAN )
            {
                // lhs was run and slot has a bool result
                bc.ISF(slotStack.back(),me->d_loc.packed());
                bc.JMP(ctx.back().pool.d_frameSize,0,me->d_loc.packed());
                const quint32 pc1 = bc.getCurPc();
                me->d_rhs->accept(this);
                Q_ASSERT( slotStack.size() >= 2 );
                bc.ISF(slotStack.back(),me->d_loc.packed());
                releaseSlot();
                bc.JMP(ctx.back().pool.d_frameSize,0,me->d_loc.packed());
                const quint32 pc2 = bc.getCurPc();
                bc.KSET(res, true, me->d_loc.packed() );
                bc.JMP(ctx.back().pool.d_frameSize,0,me->d_loc.packed());
                const quint32 pc3 = bc.getCurPc();
                bc.patch(pc1);
                bc.patch(pc2);
                bc.KSET(res, false, me->d_loc.packed() );
                bc.patch(pc3);
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::OR:
            if( lhsT->getBaseType() == Type::BOOLEAN && rhsT->getBaseType() == Type::BOOLEAN )
            {
                bc.IST(slotStack.back(),me->d_loc.packed());
                bc.JMP(ctx.back().pool.d_frameSize,0,me->d_loc.packed());
                const quint32 pc1 = bc.getCurPc();
                me->d_rhs->accept(this);
                Q_ASSERT( slotStack.size() >= 2 );
                bc.IST(slotStack.back(),me->d_loc.packed());
                releaseSlot();
                bc.JMP(ctx.back().pool.d_frameSize,0,me->d_loc.packed());
                const quint32 pc2 = bc.getCurPc();
                bc.KSET(res, false, me->d_loc.packed() );
                bc.JMP(ctx.back().pool.d_frameSize,0,me->d_loc.packed());
                const quint32 pc3 = bc.getCurPc();
                bc.patch(pc1);
                bc.patch(pc2);
                bc.KSET(res, true, me->d_loc.packed() );
                bc.patch(pc3);
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::EQ:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( lhsT->getBaseType() == Type::BOOLEAN && rhsT->getBaseType() == Type::BOOLEAN ) ||
                    ( lhsT->getBaseType() == Type::SET && rhsT->getBaseType() == Type::SET) ||
                    ( lhsT->isChar() && rhsT->isChar() ) ||
                    ( ( lhsT->getBaseType() == Type::NIL || ltag == Thing::T_Pointer || ltag == Thing::T_ProcType ) &&
                    ( rhsT->getBaseType() == Type::NIL || rtag == Thing::T_Pointer || rtag == Thing::T_ProcType ) ) )
            {
                bc.ISEQ(slotStack[slotStack.size()-2], slotStack.back(), me->d_loc.packed() );
                jumpTrueFalse( res, me->d_loc );
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                checkHandleChar(lhsT,rhsT,lwide,rwide,me->d_loc);
                stringOp(res, lwide, rwide, 1, me->d_loc);
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::NEQ:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( lhsT->getBaseType() == Type::BOOLEAN && rhsT->getBaseType() == Type::BOOLEAN ) ||
                    ( lhsT->getBaseType() == Type::SET && rhsT->getBaseType() == Type::SET) ||
                    ( lhsT->isChar() && rhsT->isChar() ) ||
                    ( ( lhsT->getBaseType() == Type::NIL || ltag == Thing::T_Pointer || ltag == Thing::T_ProcType ) &&
                    ( rhsT->getBaseType() == Type::NIL || rtag == Thing::T_Pointer || rtag == Thing::T_ProcType ) ) )
            {
                bc.ISNE(slotStack[slotStack.size()-2], slotStack.back(), me->d_loc.packed() );
                jumpTrueFalse( res, me->d_loc );
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                checkHandleChar(lhsT,rhsT,lwide,rwide,me->d_loc);
                stringOp(res, lwide, rwide, 2, me->d_loc);
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::LT:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) || ( lhsT->isChar() && rhsT->isChar() ) )
            {
                bc.ISLT(slotStack[slotStack.size()-2], slotStack.back(), me->d_loc.packed() );
                jumpTrueFalse( res, me->d_loc );
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                checkHandleChar(lhsT,rhsT,lwide,rwide,me->d_loc);
                stringOp(res, lwide, rwide, 3, me->d_loc);
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::LEQ:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) || ( lhsT->isChar() && rhsT->isChar() ) )
            {
                bc.ISLE(slotStack[slotStack.size()-2], slotStack.back(), me->d_loc.packed() );
                jumpTrueFalse( res, me->d_loc );
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                checkHandleChar(lhsT,rhsT,lwide,rwide,me->d_loc);
                stringOp(res, lwide, rwide, 4, me->d_loc);
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::GT:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) || ( lhsT->isChar() && rhsT->isChar() ) )
            {
                bc.ISGT(slotStack[slotStack.size()-2], slotStack.back(), me->d_loc.packed() );
                jumpTrueFalse( res, me->d_loc );
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                checkHandleChar(lhsT,rhsT,lwide,rwide,me->d_loc);
                stringOp(res, lwide, rwide, 5, me->d_loc);
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::GEQ:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) || ( lhsT->isChar() && rhsT->isChar() ) )
            {
                bc.ISGE(slotStack[slotStack.size()-2], slotStack.back(), me->d_loc.packed() );
                jumpTrueFalse( res, me->d_loc );
            }else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                checkHandleChar(lhsT,rhsT,lwide,rwide,me->d_loc);
                stringOp(res, lwide, rwide, 6, me->d_loc);
            }else
                Q_ASSERT(false);
            break;
        default:
            Q_ASSERT(false);
        }
        releaseSlot();
        if( me->d_op != BinExpr::AND && me->d_op != BinExpr::OR )
            releaseSlot();
        slotStack.push_back(res);
    }

    void visit( IdentLeaf* me)
    {
        Named* id = me->getIdent();
        Q_ASSERT( id );
        const int tag = id->getTag();

        if( tag == Thing::T_BuiltIn )
            return; // no slot used

        const int res = ctx.back().buySlots(1);

        switch( tag )
        {
        case Thing::T_Const:
            {
                Const* c = cast<Const*>(id);
                emitConst( res, c->d_vtype, c->d_val, me->d_loc );
            }
            break;
        case Thing::T_Import:
            if( !checkValidSlot(id,me->d_loc))
                return;
            Q_ASSERT( id->d_slotValid );
            Q_ASSERT( id->d_scope != ctx.back().scope );
//                if( id->d_scope == ctx.back().scope )
//                    bc.MOV(res,id->d_slot,me->d_loc.packed());
//                else
            bc.UGET(res, ctx.back().resolveUpval(id), me->d_loc.packed() );
            break;
        case Thing::T_Variable:
            {
                if( !checkValidSlot(id,me->d_loc))
                    return;
                Q_ASSERT( id->d_slotValid );
                fetchModule( res, me->d_loc );
                emitGetTableByIndex(res, res, id->d_slot, me->d_loc );
            }
            break;
        case Thing::T_Procedure:
            if( !checkValidSlot(id,me->d_loc) )
                return;
            Q_ASSERT( id->d_slotValid );
            if( id->d_slot > MAX_PROC_SLOTS )
            {
                fetchModule( res, me->d_loc );
                emitGetTableByIndex(res, res, id->d_slot, me->d_loc );
            }else
                bc.UGET(res, ctx.back().resolveUpval(id), me->d_loc.packed() );
            break;
        case Thing::T_LocalVar:
        case Thing::T_Parameter:
            if( id->d_scope == ctx.back().scope )
            {
                if( !checkValidSlot(id,me->d_loc) )
                    return;
                Q_ASSERT( id->d_slotValid );
                bc.MOV(res,id->d_slot,me->d_loc.packed());
            }else
                qWarning() << "non local access not yet implemented"; // TODO
            break;
        default:
            qDebug() << "ERR" << mod->d_name << me->d_loc.d_row << me->d_loc.d_col;
            Q_ASSERT( false );
            break;
        }

        slotStack.push_back(res);
    }

    void visit( IdentSel* me)
    {
        Q_ASSERT( !me->d_sub.isNull() );

        // shortcut SYSTEM.x
        Named* subId = me->d_sub->getIdent();
        if( subId && subId->getTag() == Thing::T_Import && subId->d_name == "SYSTEM" )
            return; // no slot used

        me->d_sub->accept(this);

        Named* id = me->getIdent();
        Q_ASSERT( id );
        const int tag = id->getTag();
        const int res = ctx.back().buySlots(1);

        switch( tag )
        {
        case Thing::T_Procedure:
            if( !checkValidSlot(id,me->d_loc))
                return;
            Q_ASSERT( id->d_slotValid );
            emitGetTableByIndex(res, slotStack.back(), id->d_slot, me->d_loc );
            // TODO: bound procedures require this and fetch from module, not from record
            // don't release if we use slotStack.back() as this
            releaseSlot();
            break;
        case Thing::T_Field: // TODO: assign slot ids to fields somewhere
        case Thing::T_Variable:
            if( !checkValidSlot(id,me->d_loc) )
                return;
            Q_ASSERT( id->d_slotValid );
            emitGetTableByIndex(res, slotStack.back(), id->d_slot, me->d_loc );
            releaseSlot();
            break;
        case Thing::T_Const:
            {
                Const* c = cast<Const*>(id);
                emitConst( res, c->d_vtype, c->d_val, me->d_loc );
            }
            releaseSlot();
            break;
        case Thing::T_BuiltIn:
            bc.MOV(res, slotStack.back(), me->d_loc.packed() );
            releaseSlot();
            break;
        default:
            qDebug() << "ERR" << mod->d_name << tag << me->d_loc.d_row << me->d_loc.d_col;
            Q_ASSERT( false );
            break;
        }

        slotStack.push_back(res);
    }

    void visit( ArgExpr* me )
    {
        switch( me->d_op )
        {
        case ArgExpr::IDX:
            emitIndex(me);
            break;
        case ArgExpr::CALL:
            emitCall(me);
            break;
        case ArgExpr::CAST:
            // NOP
            break;
        }
    }

    ///// Not used

    void visit( BaseType* ) {}
    void visit( Pointer* ) {}
    void visit( Array* ) {}
    void visit( Record* ) {}
    void visit( ProcType* ) {}
    void visit( QualiType* ) {}
    void visit( Enumeration* ) {}
    void visit( GenericName* ) {}
    void visit( BuiltIn* ) {}
    void visit( Field* ) {}
    void visit( Parameter* ) {}
    void visit( NamedType* ) {}
    void visit( Const* ) {}

    ///// Helper

    void emitImport( const QByteArray& modName, quint8 toSlot, const RowCol& loc )
    {
        int tmp = ctx.back().buySlots(2,true);
        bc.GGET( tmp, "require", loc.packed() );
        bc.KSET( tmp+1, modName, loc.packed() );
        bc.CALL( tmp, 1, 1, loc.packed() );
        bc.MOV(toSlot,tmp,loc.packed() );
        ctx.back().sellSlots(tmp,2);
    }

    void fetchObxlib( quint8 to, const RowCol& loc )
    {
        bc.UGET(to, ctx.back().resolveUpval(obxlj,"@obxlj"), loc.packed() );
    }

    void fetchObxlibMember( quint8 to, quint8 what, const RowCol& loc )
    {
        if( ctx.back().scope == mod )
        {
            bc.TGETi(to,obxlj,what,loc.packed());
        }else
        {
            fetchObxlib(to,loc);
            bc.TGETi(to,to,what,loc.packed());
        }
    }

    void fetchModule( quint8 to, const RowCol& loc )
    {
        bc.UGET(to, ctx.back().resolveUpval(modSlot,"@mod"), loc.packed() );
    }

    void emitInitializer( quint8 to, Type* t, const RowCol& loc )
    {
        Q_ASSERT( t );
        t = t->derefed();
        switch( t->getTag() )
        {
        case Thing::T_Record:
            {
                Record* r = cast<Record*>(t);
                bc.TNEW( to, r->d_fieldCount, 0, loc.packed() );

                int tmp = ctx.back().buySlots(1);
                if( ctx.back().scope == mod ) // emitInitializer is called both from module and proc level
                    tmp = modSlot;
                else
                    fetchModule( tmp, loc );

                // call setmetatable
                emitGetTableByIndex( tmp, tmp, r->d_slot, loc );
                int base = ctx.back().buySlots(3,true);
                bc.TGETi( base, obxlj, 22, loc.packed() ); // setmetatable
                bc.MOV( base+1, to, loc.packed() );
                bc.MOV(base+2, tmp, loc.packed() );
                bc.CALL( base, 0, 2, loc.packed() );
                ctx.back().sellSlots(base,3);

                // initialize structured fields
                QList<Field*> fields = r->getOrderedFields();
                for( int i = 0; i < fields.size(); i++ )
                {
                    Type* t = fields[i]->d_type.data();
                    Q_ASSERT( t );
                    t = t->derefed();
                    const int tag = t->getTag();
                    if( tag == Thing::T_Record || tag == Thing::T_Array )
                    {
                        emitInitializer(tmp, t, loc );
                        emitSetTableByIndex(tmp,to,fields[i]->d_slot,loc);
                    }
                }
                ctx.back().sellSlots(tmp);
            }
            break;
        case Thing::T_Array:
            {
                Array* a = cast<Array*>(t);
                Q_ASSERT( a->d_len > 0 && !a->d_type.isNull() );
                Type* t = a->d_type->derefed();
                Q_ASSERT( t );

                if( t->getBaseType() > 0 )
                {
                    int tmp = ctx.back().buySlots(2,true);
                    switch( t->getBaseType() )
                    {
                    case Type::BOOLEAN:
                    case Type::CHAR:
                    case Type::BYTE:
                        fetchObxlibMember(tmp,8,loc); // module.createByteArray
                        break;
                    case Type::SHORTINT:
                        fetchObxlibMember(tmp,3,loc); // module.createShortArray
                        break;
                    case Type::WCHAR:
                        fetchObxlibMember(tmp,2,loc); // module.createWcharArray
                        break;
                    case Type::INTEGER:
                        fetchObxlibMember(tmp,4,loc); // module.createIntArray
                        break;
                    case Type::SET:
                        fetchObxlibMember(tmp,24,loc); // module.createSetArray
                        break;
                    case Type::LONGINT:
                        fetchObxlibMember(tmp,5,loc); // module.createLongArray
                        break;
                    case Type::REAL:
                        fetchObxlibMember(tmp,6,loc); // module.createFloatArray
                        break;
                    case Type::LONGREAL:
                        fetchObxlibMember(tmp,7,loc); // module.createDoubleArray
                        break;
                    default:
                        Q_ASSERT( false );
                        break;
                    }
                    bc.KSET(tmp+2,a->d_len, loc.packed() );
                    bc.CALL(tmp,1,1,loc.packed());
                    bc.MOV(to,tmp,loc.packed());
                    ctx.back().sellSlots(tmp,2);
                }else
                {
                    bc.TNEW( to, a->d_len, 0, loc.packed() );
                    const int tmp = ctx.back().buySlots(1);
                    bc.KSET( tmp, a->d_len, loc.packed() );
                    bc.TSET(tmp,to,"count",loc.packed());

                    const int tag = t->getTag();
                    if( tag == Thing::T_Record || tag == Thing::T_Array )
                    {
                        for( quint32 i = 0; i < a->d_len; i++ )
                        {
                            emitInitializer(tmp, t, loc );
                            emitSetTableByIndex(tmp,to,i,loc);
                        }
                    }

                    ctx.back().sellSlots(tmp);
                }
            }
            break;
        }
    }

    void emitInitializer( Named* me )
    {
        emitInitializer( me->d_slot, me->d_type.data(), me->d_loc );
    }

    void releaseSlot()
    {
        Q_ASSERT( !slotStack.isEmpty() );
        ctx.back().sellSlots(slotStack.back());
        slotStack.pop_back();
    }

    void jumpTrueFalse( quint8 res, const RowCol& loc )
    {
        // if true
        bc.JMP(ctx.back().pool.d_frameSize,0,loc.packed());
        const quint32 pc1 = bc.getCurPc();
        bc.KSET(res, false, loc.packed() );
        bc.JMP(ctx.back().pool.d_frameSize,0,loc.packed());
        const quint32 pc2 = bc.getCurPc();
        bc.patch(pc1);
        bc.KSET(res, true, loc.packed() );
        bc.patch(pc2);
    }

    void stringOp(quint8 res, bool lwide, bool rwide, int op, const RowCol& loc)
    {
        int tmp = ctx.back().buySlots(6,true);
        fetchObxlibMember(tmp,17, loc); // stringRelOp
        bc.MOV(tmp+1, slotStack[slotStack.size()-2], loc.packed() );
        bc.KSET(tmp+2, lwide, loc.packed() );
        bc.MOV(tmp+3, slotStack.back(), loc.packed() );
        bc.KSET(tmp+4, rwide, loc.packed() );
        bc.KSET(tmp+5, op, loc.packed() );
        bc.CALL(tmp,1,5, loc.packed());
        bc.MOV(res,tmp, loc.packed());
        ctx.back().sellSlots(tmp,6);
    }

    void charToString( quint8 res, quint8 in, bool forceWide, const RowCol& loc  )
    {
        int tmp = ctx.back().buySlots(3,true);
        fetchObxlibMember(tmp,16,loc); // module.charToString
        bc.MOV(tmp+1,in,loc.packed());
        bc.KSET(tmp+2,forceWide,loc.packed());
        bc.CALL(tmp,1,2,loc.packed());
        bc.MOV(res,tmp,loc.packed());
        ctx.back().sellSlots(tmp,3);
    }

    inline void checkHandleChar(Type* lhsT, Type* rhsT, bool lwide, bool rwide, const RowCol& loc )
    {
        if( lhsT->isChar() )
            charToString(slotStack[slotStack.size()-2],slotStack[slotStack.size()-2],lwide || rwide, loc );
        if( rhsT->isChar() )
            charToString(slotStack.back(),slotStack.back(),lwide || rwide, loc );
    }

    void setOp( quint8 res, int op, const RowCol& loc )
    {
        int tmp = ctx.back().buySlots(3,true);
        fetchObxlibMember(tmp,op,loc);
        bc.MOV(tmp+1, slotStack[slotStack.size()-2], loc.packed() );
        bc.MOV(tmp+2, slotStack.back(), loc.packed() );
        bc.CALL(tmp,1,2,loc.packed());
        bc.MOV(res,tmp,loc.packed());
        ctx.back().sellSlots(tmp,3);
    }

    void emitConst(quint8 res, quint8 vtype, const QVariant& val, const RowCol& loc )
    {
        switch( vtype )
        {
        case Literal::Integer:
        case Literal::Real:
        case Literal::Boolean:
        case Literal::Nil:
            bc.KSET(res, val, loc.packed() );
            break;
        case Literal::String:
            {
                int tmp = ctx.back().buySlots(3,true);
                fetchObxlib(tmp,loc);
                const QString str = QString::fromUtf8(val.toByteArray());
                bc.KSET(tmp+1,str.size()+1,loc.packed());
                bc.KSET(tmp+2,val,loc.packed());
                if( Literal::isWide(str) )
                {
                    bc.TGETi(tmp,tmp,2,loc.packed()); // createWcharArray
                    bc.CALL(tmp,1,2,loc.packed());
                }else
                {
                    bc.TGETi(tmp,tmp,1,loc.packed()); // createCharArray
                    bc.CALL(tmp,1,2,loc.packed());
                }
                bc.MOV(res,tmp,loc.packed());
                ctx.back().sellSlots(tmp,3);
            }
            break;
        case Literal::Bytes:
            {
                int tmp = ctx.back().buySlots(3,true);
                fetchObxlibMember(tmp,8,loc); // createByteArray
                bc.KSET(tmp+1,val.toByteArray().size()+1,loc.packed());
                bc.KSET(tmp+2,val,loc.packed());
                bc.CALL(tmp,1,2,loc.packed());
                bc.MOV(res,tmp,loc.packed());
                ctx.back().sellSlots(tmp,3);
            }
            break;
        case Literal::Char:
#if 1
            bc.KSET(res,val,loc.packed());
#else
            {
                // directly converting a char to a string; not useful because there are still char variables
                int tmp = ctx.back().buySlots(2,true);
                fetchObxlibMember(tmp,16,me->d_loc); // module.charToString
                bc.KSET(tmp+1,me->d_val,loc.packed());
                bc.CALL(tmp,1,1,loc.packed());
                bc.MOV(res,tmp,loc.packed());
                ctx.back().sellSlots(tmp,2);
            }
#endif
            break;
        case Literal::Set:
            {
                Literal::SET s = val.value<Literal::SET>();
                bc.KSET(res,quint32(s.to_ulong()),loc.packed());
            }
            break;
        }
    }

    void emitGetTableByIndex( quint8 res, quint8 table, quint32 index, const RowCol& loc)
    {
        if( index > 255 )
        {
            int tmp = ctx.back().buySlots(1);
            bc.KSET(tmp,index,loc.packed());
            bc.TGET(res,table,tmp,loc.packed());
            ctx.back().sellSlots(tmp);
        }else
            bc.TGETi( res, table, index, loc.packed() );
    }

    void emitSetTableByIndex( quint8 value, quint8 table, quint32 index, const RowCol& loc)
    {
        if( index > 255 )
        {
            int tmp = ctx.back().buySlots(1);
            bc.KSET(tmp,index,loc.packed());
            bc.TSET(value,table,tmp,loc.packed());
            ctx.back().sellSlots(tmp);
        }else
            bc.TSETi( value, table, index, loc.packed() );
    }

    void emitIndex( ArgExpr* me )
    {
        Q_ASSERT( me->d_sub );
        me->d_sub->accept(this);
        quint8 table = slotStack.back();

        for( int i = 0; i < me->d_args.size(); i++ )
        {
            me->d_args[i]->accept(this);
            bc.TGET( table, table, slotStack.back(), me->d_args[i]->d_loc.packed() );
            releaseSlot();
        }
        const int res = ctx.back().buySlots(1);
        bc.MOV( res, table, me->d_loc.packed() );
        releaseSlot(); // table
        slotStack.push_back(res);
    }

    void emitCall( ArgExpr* me )
    {
        Q_ASSERT( me->d_sub );
        me->d_sub->accept(this);

        Named* func = me->d_sub->getIdent();
        if( func && func->getTag() == Thing::T_BuiltIn )
        {
            // TODO handle built-in
            return;
        }

        int tmp = ctx.back().buySlots( me->d_args.size() + 1, true );
        bc.MOV( tmp, slotStack.back(), me->d_loc.packed() );
        releaseSlot();

        // TODO: bound procs
        for( int i = 0; i < me->d_args.size(); i++ )
        {
            me->d_args[i]->accept(this);
            // TODO: copy by value structured types
            Q_ASSERT( !slotStack.isEmpty() );
            bc.MOV(tmp+1+i, slotStack.back(), me->d_args[i]->d_loc.packed() );
            // TODO var params
            releaseSlot();
        }

        bc.CALL( tmp, 1, me->d_args.size(), me->d_loc.packed() );

        const int res = ctx.back().buySlots(1);
        bc.MOV(res, tmp, me->d_loc.packed() );
        slotStack.push_back(res);

        ctx.back().sellSlots( tmp, me->d_args.size() + 1 );
    }

    static void allocateClasses( Record* r, quint32& slotNr )
    {
        if( r->d_baseRec && !r->d_baseRec->d_slotValid )
            allocateClasses( r->d_baseRec, slotNr );
        if( !r->d_slotValid )
        {
            r->setSlot(slotNr++);
            quint32 nr = 0;
            if( r->d_baseRec )
                nr = r->d_baseRec->d_fieldCount;
            for( int i = 0; i < r->d_fields.size(); i++ )
            {
                if( r->d_fields[i]->d_super )
                {
                    Q_ASSERT( r->d_fields[i]->d_super->d_slotValid );
                    r->d_fields[i]->setSlot( r->d_fields[i]->d_super->d_slot );
                }else
                {
                    r->d_fields[i]->setSlot( nr++ );
                }
            }
            r->d_fieldCount = nr;
            nr = 0;
            if( r->d_baseRec )
                nr = r->d_baseRec->d_methCount;
            for( int i = 0; i < r->d_methods.size(); i++ )
            {
                if( r->d_methods[i]->d_super )
                {
                    Q_ASSERT( r->d_methods[i]->d_super->d_slotValid );
                    r->d_methods[i]->setSlot( r->d_methods[i]->d_super->d_slot );
                }else
                {
                    r->d_methods[i]->setSlot( nr++ );
                }
                allocateLocals(r->d_methods[i].data());
            }
            r->d_methCount = nr;
        }
    }

    void allocateClassTables( Record* r, const RowCol& loc )
    {
        if( r->d_baseRec && !r->d_baseRec->d_slotAllocated )
            allocateClassTables( r->d_baseRec, loc );
        if( !r->d_slotAllocated )
        {
            bc.TNEW( curClass, r->d_methCount, 0, loc.packed() );
            if( !checkValidSlot( r, loc ) )
                return;
            Q_ASSERT( r->d_slotValid );
            emitSetTableByIndex( curClass, modSlot, r->d_slot, loc );
            if( r->d_baseRec )
            {
                if( !checkValidSlot( r->d_baseRec, loc ) )
                    return;
                Q_ASSERT( r->d_baseRec->d_slotValid );
                int baseClass = ctx.back().buySlots(1);

                // call setmetatable
                emitGetTableByIndex( baseClass, modSlot, r->d_baseRec->d_slot, loc );
                int tmp = ctx.back().buySlots(3,true);
                bc.TGETi( tmp, obxlj, 22, loc.packed() ); // setmetatable
                bc.MOV( tmp+1, curClass, loc.packed() );
                bc.MOV(tmp+2,baseClass, loc.packed() );
                bc.CALL( tmp, 0, 2, loc.packed() );
                ctx.back().sellSlots(tmp,3);

                // copy down all methods
                tmp = ctx.back().buySlots(1);
                for( int i = 0; i < r->d_baseRec->d_methCount; i++ )
                {
                    emitGetTableByIndex(tmp,baseClass,i,loc);
                    emitSetTableByIndex(tmp,curClass,i,loc);
                }
                ctx.back().sellSlots(tmp);
            }
            for( int i = 0; i < r->d_methods.size(); i++ )
                r->d_methods[i]->accept(this); // generate code for curMethod and store it in curClass

            r->d_slotAllocated = true;
        }
    }

    static void allocateLocals( Procedure* me )
    {
        int slot = 0;
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Parameter )
                n->setSlot(slot++);
        }
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_LocalVar )
                n->setSlot(slot++);
        }
    }

    bool checkValidSlot( Thing* t, const RowCol& loc )
    {
        if( t == 0 || !t->d_slotValid )
        {
            qCritical() << "ERROR" << mod->d_name << loc.d_row << loc.d_col;
            return false;
        }else
            return true;
    }

    void emitIf( IfLoop* me)
    {
        me->d_if[0]->accept(this); // IF
        Q_ASSERT( !slotStack.isEmpty() );
        bc.ISF( slotStack.back(), me->d_if[0]->d_loc.packed());
        releaseSlot();

        bc.JMP( ctx.back().pool.d_frameSize, 0, me->d_loc.packed() );
        const quint32 afterFirst = bc.getCurPc();

        for( int i = 0; i < me->d_then[0].size(); i++ )
            me->d_then[0][i]->accept(this);

        QList<quint32> afterEnd;
        bc.JMP(ctx.back().pool.d_frameSize, 0, me->d_loc.packed() );
        afterEnd << bc.getCurPc();

        bc.patch(afterFirst);
        for( int i = 1; i < me->d_if.size(); i++ ) // ELSIF
        {
            me->d_if[i]->accept(this);
            Q_ASSERT( !slotStack.isEmpty() );
            bc.ISF( slotStack.back(), me->d_if[i]->d_loc.packed());
            releaseSlot();

            bc.JMP( ctx.back().pool.d_frameSize, 0, me->d_loc.packed() );
            const quint32 afterNext = bc.getCurPc();

            for( int j = 0; j < me->d_then[i].size(); j++ )
                me->d_then[i][j]->accept(this);

            bc.JMP(ctx.back().pool.d_frameSize, 0, me->d_if[i]->d_loc.packed() );
            afterEnd << bc.getCurPc();

            bc.patch(afterNext);
        }

        if( !me->d_else.isEmpty() ) // ELSE
        {
            for( int j = 0; j < me->d_else.size(); j++ )
                me->d_else[j]->accept(this);
        }

        foreach( quint32 pc, afterEnd )
            bc.patch(pc);
    }

    void emitWhile(IfLoop* me)
    {
        bc.LOOP( ctx.back().pool.d_frameSize, 0, me->d_loc.packed() ); // while
        const quint32 loopStart = bc.getCurPc();

        me->d_if[0]->accept(this); // condition
        Q_ASSERT( !slotStack.isEmpty() );
        bc.ISF( slotStack.back(), me->d_if[0]->d_loc.packed());
        releaseSlot();

        bc.JMP( ctx.back().pool.d_frameSize, 0, me->d_loc.packed() );
        const quint32 afterFirst = bc.getCurPc();

        for( int i = 0; i < me->d_then[0].size(); i++ )
            me->d_then[0][i]->accept(this);

        bc.JMP(ctx.back().pool.d_frameSize, loopStart - bc.getCurPc() - 2, me->d_loc.packed() );

        bc.patch(afterFirst);

        QList<quint32> afterEnd;
        for( int i = 1; i < me->d_if.size(); i++ ) // ELSIF
        {
            me->d_if[i]->accept(this);
            Q_ASSERT( !slotStack.isEmpty() );
            bc.ISF( slotStack.back(), me->d_if[i]->d_loc.packed());
            releaseSlot();

            bc.JMP( ctx.back().pool.d_frameSize, 0, me->d_loc.packed() );
            const quint32 afterNext = bc.getCurPc();

            for( int j = 0; j < me->d_then[i].size(); j++ )
                me->d_then[i][j]->accept(this);

            bc.JMP(ctx.back().pool.d_frameSize, 0, me->d_if[i]->d_loc.packed() );
            afterEnd << bc.getCurPc();

            bc.patch(afterNext);
        }

        foreach( quint32 pc, afterEnd )
            bc.patch(pc);
    }

    void emitRepeat(IfLoop* me)
    {
        bc.LOOP( ctx.back().pool.d_frameSize, 0, me->d_loc.packed() ); // repeat
        const quint32 loopStart = bc.getCurPc();

        for( int i = 0; i < me->d_then.first().size(); i++ )
            me->d_then.first()[i]->accept(this);

        me->d_if[0]->accept(this); // until condition
        Q_ASSERT( !slotStack.isEmpty() );
        bc.IST( slotStack.back(), me->d_if[0]->d_loc.packed());
        releaseSlot();

        bc.JMP( ctx.back().pool.d_frameSize, 0, me->d_loc.packed() ); // if true jump to afterEnd
        const quint32 afterEnd = bc.getCurPc();

        bc.JMP( ctx.back().pool.d_frameSize, loopStart - bc.getCurPc() - 2, me->d_loc.packed() ); // if false jump to loopStart

        bc.patch( afterEnd );
    }

    void emitLoop(IfLoop* me)
    {
        bc.LOOP( ctx.back().pool.d_frameSize, 0, me->d_loc.packed() ); // loop
        const quint32 loopStart = bc.getCurPc();

        for( int i = 0; i < me->d_then.first().size(); i++ )
            me->d_then.first()[i]->accept(this);

        bc.JMP( ctx.back().pool.d_frameSize, loopStart - bc.getCurPc() - 2, me->d_loc.packed() ); // jump to loopStart

        foreach( quint32 pc, exitJumps )
            bc.patch(pc);
        exitJumps.clear();
    }

    void emitWith(IfLoop* me)
    {
        // if guard then statseq elsif guard then statseq else statseq end
        // guard ::= lhs IS rhs
        emitIf(me);
    }

    void emitTypeCase( CaseStmt* me )
    {
        // first rewrite the AST with 'if' instead of complex 'case'

        Ref<IfLoop> ifl = new IfLoop();
        ifl->d_op = IfLoop::IF;
        ifl->d_loc = me->d_loc;

        for( int i = 0; i < me->d_cases.size(); i++ )
        {
            const CaseStmt::Case& c = me->d_cases[i];

            Q_ASSERT( c.d_labels.size() == 1 );

            Ref<BinExpr> eq = new BinExpr();
            eq->d_op = BinExpr::IS;
            eq->d_lhs = me->d_exp;
            eq->d_rhs = c.d_labels.first();
            eq->d_loc = me->d_exp->d_loc;

            ifl->d_if.append(eq.data());
            ifl->d_then.append( c.d_block );
        }

        // and now generate code for the if
        ifl->accept(this);
    }

    void emitPlainCase( CaseStmt* me )
    {
        // first rewrite the AST with 'if' instead of complex 'case'

        Ref<IfLoop> ifl = new IfLoop();
        ifl->d_op = IfLoop::IF;
        ifl->d_loc = me->d_loc;

        for( int i = 0; i < me->d_cases.size(); i++ )
        {
            const CaseStmt::Case& c = me->d_cases[i];

            QList< Ref<Expression> > ors;
            for( int j = 0; j < c.d_labels.size(); j++ )
            {
                Expression* l = c.d_labels[j].data();
                // TODO: avoid redundant evaluations using temp vars
                if( l->getTag() == Thing::T_BinExpr )
                {
                    BinExpr* bi = cast<BinExpr*>( l );
                    Q_ASSERT( bi->d_op == BinExpr::Range );

                    Ref<BinExpr> _and = new BinExpr();
                    _and->d_op = BinExpr::AND;
                    _and->d_loc = l->d_loc;

                    Ref<BinExpr> lhs = new BinExpr();
                    lhs->d_op = BinExpr::GEQ;
                    lhs->d_lhs = me->d_exp;
                    lhs->d_rhs = bi->d_lhs;
                    lhs->d_loc = l->d_loc;

                    Ref<BinExpr> rhs = new BinExpr();
                    rhs->d_op = BinExpr::LEQ;
                    rhs->d_lhs = me->d_exp;
                    rhs->d_rhs = bi->d_rhs;
                    rhs->d_loc = l->d_loc;

                    _and->d_lhs = lhs.data();
                    _and->d_rhs = rhs.data();

                    ors << _and.data();
                }else
                {
                    Ref<BinExpr> eq = new BinExpr();
                    eq->d_op = BinExpr::EQ;
                    eq->d_lhs = me->d_exp;
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
};


bool LjbcGen::translate(Model* mdl, const QString& outdir, const QString& mod, Ob::Errors* err)
{
    Q_ASSERT( mdl );
    Q_ASSERT( RowCol::COL_BIT_LEN == JitComposer::COL_BIT_LEN && RowCol::ROW_BIT_LEN == JitComposer::ROW_BIT_LEN );

    QDir dir(outdir);
    if( !mod.isEmpty() )
    {
        dir.mkpath( mod );
        dir.cd( mod );
    }

    int errs = 0;
    foreach( Module* m, mdl->getDepOrder() )
    {
        if( m->d_isDef )
            continue;

        QFile out( dir.absoluteFilePath( m->d_name + ".lua" ) );
        if( !out.open(QIODevice::WriteOnly) )
        {
            errs++;
            if( err )
                err->error(Errors::Generator,m->d_name, 0,0,QString("cannot open file '%1' for writing").
                       arg(out.fileName()) );
        }else
        {
            if( m->d_isDef )
                allocateDef(m,&out,err);
            else if( !translate(m,&out,err) )
                errs++;
        }
    }
    return errs == 0;
}

bool LjbcGen::translate(Module* m, QIODevice* out, Ob::Errors* errs)
{
    Q_ASSERT( m != 0 && out != 0 );

    if( m->d_hasErrors )
        return false;

    if( m->d_isDef )
        return true;

    ObxLjbcGenImp imp;
    imp.bc.setUseRowColFormat(true);
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
    }catch( const ObxLjbcGenImp::NoMoreFreeSlots& )
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

bool LjbcGen::allocateDef(Module* m, QIODevice* out, Errors* errs)
{
    ObxLjbcGenImp::ProcsCollector pc;
    m->accept(&pc);

    quint32 slotNr = 0;

    foreach( const Ref<Named>& n, m->d_order )
    {
        if( n->getTag() == Thing::T_Variable )
            n->setSlot(slotNr++);
    }

    foreach( Procedure* p, pc.allProcs )
    {
        Q_ASSERT( p->d_receiver.isNull() );
        // bound procs are stored in the class objects instead
        p->setSlot( slotNr++ );
    }

    foreach( Record* r, pc.allRecords )
        ObxLjbcGenImp::allocateClasses(r,slotNr);

    return true;
}


