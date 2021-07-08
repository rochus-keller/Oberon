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

// #define _HAVE_OUTER_LOCAL_ACCESS

struct ObxLjbcGenImp : public AstVisitor
{
    // This version of the code generator (in contrast to ObLjbcGen) uses a register for each expression evaluation to
    // transfer the result back to caller; I assume that the tracer/optimizer is able to get rid of the redundant moves;
    // the code generator/allocator is much simpler to implement on the other hand.
    // This implementation also tries to replace names by indexes (magic numbers) which are ~30% faster than hash
    // lookup; the magic numbers are in sync with obxlj.lua.

    struct NoMoreFreeSlots {};
    enum { MAX_PROC_SLOTS = 230 };

    struct Collector : public AstVisitor
    {
        QList<Procedure*> allProcs;
        QList<Record*> allRecords;
        QList<QualiType*> allAliasses;
        Module* thisMod;

        void collect(Type* t, bool isTypedef )
        {
            int tag = t->getTag();
            if( tag == Thing::T_Array )
            {
                collect(cast<Array*>(t)->d_type.data(), false);
                return;
            }
            if( isTypedef && tag == Thing::T_QualiType )
            {
                QualiType* q = cast<QualiType*>(t);
                if( t->toRecord() && q->isDotted() )
                {
                    // t is B.C in "type A = B.C"
                    //qDebug() << "*** ALIAS dotted quali" << thisMod->getName() << t->d_loc.d_row << t->d_loc.d_col;
                    allAliasses.append( q );
                }
                return;
            }
            if( tag == Thing::T_Pointer )
            {
                t = cast<Pointer*>(t)->d_to.data(); // no deref, intentionally
                tag = t->getTag();
                if( isTypedef && tag == Thing::T_QualiType )
                {
                    QualiType* q = cast<QualiType*>(t);
                    if( t->toRecord() && q->isDotted() )
                    {
                        // t is B.C in "type A = pointer to B.C"
                        //qDebug() << "*** ALIAS ptr to dotted quali" << thisMod->getName() << t->d_loc.d_row << t->d_loc.d_col;
                        allAliasses.append( q );
                    }
                    return;
                }
            }
            if( tag == Thing::T_Record )
            {
                Record* r = cast<Record*>(t);
                allRecords.append( r );
                foreach( const Ref<Field>& f, r->d_fields )
                    collect(f->d_type.data(), false);
            }
        }

        void collect( Named* n )
        {
            switch( n->getTag() )
            {
            case Thing::T_Procedure:
                {
                    Procedure* p = cast<Procedure*>(n);
                    if( p->d_receiver.isNull() )
                        allProcs.append(cast<Procedure*>(n));
                    n->accept(this);
                }
                break;
            case Thing::T_NamedType:
                collect(n->d_type.data(), true);
                break;
            case Thing::T_Variable:
            case Thing::T_Parameter:
            case Thing::T_LocalVar:
                collect(n->d_type.data(), false);
                break;
            }
        }

        void visit( Module* me )
        {
            thisMod = me;
            foreach( const Ref<Named>& n, me->d_order )
                collect(n.data());
        }

        void visit( Procedure* me)
        {
            foreach( const Ref<Named>& n, me->d_order )
                collect(n.data());
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
    Module* thisMod;
    bool ownsErr, stripped;
    quint8 modSlot, obxlj, curClass;
    QList<quint8> slotStack;
    QList <quint32> exitJumps;
    QByteArray system;

    ObxLjbcGenImp():err(0),thisMod(0),ownsErr(false),modSlot(0),obxlj(0),curClass(0),stripped(0){}

    struct Accessor
    {
        enum Kind { Invalid, TableIdxSlot, TableIdx, Slot };
        uint kind : 3;
        uint disposeSlot : 1;
        uint disposeIndex : 1;
        uint slot : 8;
        uint index : 16;
        Accessor():kind(Invalid),disposeSlot(0),disposeIndex(0),slot(0),index(0){}
    };

    void visit( Module* me )
    {
        // this method now assumes that all slots are already allocated!
        ctx.push_back( Ctx(me) );
        bc.openFunction(0,me->getName(),me->d_loc.packed(), me->d_end.packed() );

        QHash<quint8,QByteArray> names;
        modSlot = ctx.back().buySlots(1);
        names[modSlot] = "@mod";
        obxlj = ctx.back().buySlots(1);
        names[obxlj] = "@obxlj";

        foreach( Import* imp, me->d_imports )
        {
            if(imp->d_mod->d_synthetic )
                continue; // ignore SYSTEM
            const int slot = ctx.back().buySlots(1);
            Q_ASSERT( imp->d_slotValid && imp->d_slot == slot );
            // imports store a reference to the imported module table in a local slot and the module table
            names[imp->d_slot] = imp->d_name;
        }

        Collector pc;
        me->accept(&pc);
        bool slotsFinished = false;
        foreach( Procedure* p, pc.allProcs )
        {
            Q_ASSERT( p->d_receiver.isNull() );
            // bound procs are stored in the class objects instead

            if( !slotsFinished )
            {
                const int slot = ctx.back().buySlots(1); // procs are stored as indices in the module table
                                                    // as well as in the module function slots up to MAX_PROC_SLOTS
                Q_ASSERT( p->d_slotValid && p->d_slot == slot );
                names[p->d_slot] = p->d_name; // more than one proc/slot can have the same name
                if( p->d_slot >= MAX_PROC_SLOTS )
                    slotsFinished = true;
            }
            allocateLocals(p);
        }

        foreach( Record* r, pc.allRecords )
        {
            quint32 slotNr = 0;
            ObxLjbcGenImp::allocateClasses(r,slotNr,me, true);
        }

        Q_ASSERT( modSlot == 0 );
        bc.TNEW( modSlot, ctx.back().pool.d_frameSize, 0, me->d_loc.packed() );

        emitImport( "obxlj", obxlj, me->d_loc );

        foreach( Import* imp, me->d_imports )
            imp->accept(this);
#if 0 // TEST, usually 0
        // qDebug() << "**** dump of" << me->getName();
        QFile out("/home/me/gendump.txt");
        bool res = out.open(QIODevice::Append);
        me->dump(&out); // TEST
#endif
        curClass = ctx.back().buySlots(1);
        foreach( Record* r, pc.allRecords )
            allocateClassTables(r);
        foreach( QualiType* q, pc.allAliasses )
        {
            if( !q->d_slotAllocated )
            {
                fetchClass( curClass, q, q->d_loc );
                emitSetTableByIndex(curClass,modSlot,q->d_slot,q->d_loc);
                q->d_slotAllocated = true;
            }
        }
        ctx.back().sellSlots(curClass);

        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Variable )
                emitInitializer(n.data());
        }

        foreach( Procedure* p, pc.allProcs )
            p->accept(this);

        // make Module table a global variable
        bc.GSET( modSlot, me->getName(), me->d_end.packed() );
        const int tmp = ctx.back().buySlots(1);
        // save the module path in the module table
        bc.KSET(tmp, me->getName(), me->d_end.packed() );
        bc.TSET(tmp,modSlot,"@mod", me->d_end.packed() );
        ctx.back().sellSlots(tmp);

        // the module body is in a synthetic procedure so that upvalue access is uniform
        if( !me->d_body.isEmpty() )
        {
            Procedure modBody;
            modBody.d_type = new ProcType();
            modBody.d_type->d_loc = me->d_begin;
            modBody.d_type->d_decl = me;
            modBody.d_name = "begin";
            modBody.d_body = me->d_body;
            modBody.d_scope = me;
            modBody.d_loc = me->d_begin;
            modBody.d_end = me->d_end;
            modBody.accept(this);
            const int tmp = ctx.back().buySlots(1,true);
            bc.FNEW( tmp, modBody.d_slot, me->d_end.packed() );
            bc.UCLO( modSlot, 0, me->d_end.packed() );
            bc.CALL(tmp,0,0,me->d_end.packed()); // no var, no return arg
            ctx.back().sellSlots(tmp);
        }else
            bc.UCLO( modSlot, 0, me->d_end.packed() );

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
        bc.closeFunction(ctx.back().pool.d_frameSize); // module function
        ctx.pop_back();
    }

    void visit( Procedure* me)
    {
        ctx.push_back( Ctx(me) );

        const int parCount =
#ifdef _HAVE_OUTER_LOCAL_ACCESS
                ( me->d_upvalIntermediate || me->d_upvalSink ? 1 : 0 ) +
#endif
                me->d_parCount;

        const int id = bc.openFunction( parCount, me->d_name,me->d_loc.packed(), me->d_end.packed() );
        Q_ASSERT( id >= 0 );

        QHash<quint8,QByteArray> names;
#ifdef _HAVE_OUTER_LOCAL_ACCESS
        if( me->d_upvalSource )
        {
            // this trick can also be used for procs where we run out of local slots
            const int localSlotCount = parCount + me->d_varCount;
            const int reserve = ctx.back().buySlots(localSlotCount);
            const int tmp = ctx.back().buySlots(1);
            bc.TNEW(tmp,localSlotCount, 0, me->d_loc.packed() );
            foreach( const Ref<Named>& n, me->d_order )
            {
                const int tag = n->getTag();
                if( tag == Thing::T_Parameter )
                {
                    Q_ASSERT( n->d_slotValid );
                    Q_ASSERT( n->d_slot < localSlotCount );
                    emitSetTableByIndex( n->d_slot, tmp, n->d_slot, n->d_loc );
                }else if( tag == Thing::T_LocalVar )
                {
                    Q_ASSERT( n->d_slotValid );
                    Q_ASSERT( n->d_slot < localSlotCount );
                    emitInitializer( n->d_slot, n->d_type.data(), n->d_loc );
                    emitSetTableByIndex(n->d_slot ,tmp,n->d_slot, n->d_loc);
                }
            }
            if( me->d_upvalIntermediate || me->d_upvalSink )
                emitSetTableByIndex( parCount, tmp, parCount, me->d_loc.packed() ); // transfer outer frame
            ctx.back().sellSlots(tmp);
            ctx.back().sellSlots(reserve);
            const int zero = ctx.back().buySlots(1);
            Q_ASSERT( zero == 0 );
            names[zero] = "@frame";
            bc.MOV(zero,tmp,me->d_loc.packed());
        }else
#endif
        {
            foreach( const Ref<Named>& n, me->d_order )
            {
                // in d_order there is also the receiver, at 0 position as expected
                if( n->getTag() == Thing::T_Parameter )
                {
                    const int slot = ctx.back().buySlots(1);
                    Q_ASSERT( n->d_slotValid && n->d_slot == slot );
                    names[n->d_slot] = n->d_name;
                }
            }
#ifdef _HAVE_OUTER_LOCAL_ACCESS
            if( me->d_upvalIntermediate || me->d_upvalSink )
                ctx.back().buySlots(1); // reserve slot for outer frame at the end of param list
#endif

            foreach( const Ref<Named>& n, me->d_order )
            {
                const int tag = n->getTag();
                if( tag == Thing::T_LocalVar )
                {
                    const int slot = ctx.back().buySlots(1);
                    Q_ASSERT( n->d_slotValid && n->d_slot == slot );
                    names[n->d_slot] = n->d_name;
                    emitInitializer(n.data());
                }
            }
        }

#ifdef _INSERT_DGBTRACE
        {
            const quint8 tmp = ctx.back().buySlots(2, true);
            fetchObxlibMember(tmp,45,me->d_loc); // ObxFfi_DBGTRACE
            bc.KSET(tmp+1, QByteArray("> ") + thisMod->getName() + QByteArray(":") + me->getName(),me->d_loc.packed());
            bc.CALL(tmp,0,1,me->d_loc.packed());
            ctx.back().sellSlots(tmp,2);
        }
#endif

        foreach( const Ref<Statement>& s, me->d_body )
            s->accept(this);

        if( me->d_body.isEmpty() || me->d_body.last()->getTag() != Thing::T_Return )
            emitReturn( me->getProcType(), 0, me->d_end );
            // we need the full emitReturn here instead of only bc.RET(me->d_end.packed()), because there
            // are proper procs with var params

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
                // store top-level module procs also by name in module (because of Obs Modules.ThisCommand)
                if( me->d_slot > MAX_PROC_SLOTS )
                {
                    const int tmp = ctx.back().buySlots(1);
                    bc.FNEW( tmp, id, me->d_end.packed() );
                    emitSetTableByIndex( tmp, modSlot, me->d_slot, me->d_end );
                    if( me->d_scope == thisMod )
                        bc.TSET( tmp, modSlot, me->d_name, me->d_end.packed() );
                    ctx.back().sellSlots(tmp);
                }else
                {
                    bc.FNEW( me->d_slot, id, me->d_end.packed() );
                    emitSetTableByIndex( me->d_slot, modSlot, me->d_slot, me->d_end );
                    if( me->d_scope == thisMod )
                        bc.TSET( me->d_slot, modSlot, me->d_name, me->d_end.packed() );
                }
            }else
            {
                const int tmp = ctx.back().buySlots(1);
                bc.FNEW( tmp, id, me->d_end.packed() );
                emitSetTableByIndex( tmp, curClass, me->d_slot, me->d_end );
                ctx.back().sellSlots(tmp);
            }
        }else
            me->d_slot = id; // in case of module body
    }

    void visit( Import* me)
    {
        if( me->d_mod->d_name.constData() == system.constData() )
            return;
        emitImport( me->d_mod->getName(), me->d_slot, me->d_aliasPos.isValid() ? me->d_aliasPos : me->d_loc );
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
        Q_ASSERT( ctx.back().scope->getTag() == Thing::T_Procedure );
        emitReturn( cast<Procedure*>(ctx.back().scope)->getProcType(), me->d_what.data(), me->d_loc );
    }

    void visit( Assign* me )
    {
        Q_ASSERT( me->d_rhs );
        me->d_rhs->accept(this);
        if( slotStack.isEmpty() )
            return; // error already reported

        Q_ASSERT( me->d_lhs );

        Q_ASSERT( !me->d_lhs->d_type.isNull() );
        Q_ASSERT( !me->d_rhs->d_type.isNull() );

        Type* lhsT = derefed(me->d_lhs->d_type.data());
        Type* rhsT = derefed(me->d_rhs->d_type.data());
        Q_ASSERT( lhsT != 0 && rhsT != 0 );

        if( lhsT->isStructured() )
        {
            // copy by value in structured assignment
            me->d_lhs->accept(this);
            if( slotStack.size() < 2 )
                return; // already reported
            Q_ASSERT( slotStack.size() >= 2 ); // ..., rhs value or table, lhs table
            emitCopy( false, me->d_lhs->d_type.data(), slotStack.back(),
                      me->d_rhs->d_type.data(), slotStack[ slotStack.size() - 2 ], me->d_loc );
            releaseSlot(); // lhs
        }else
        {
            if( lhsT->isChar() && !rhsT->isChar() )
            {
                // len-1-string to char
                Q_ASSERT( rhsT->isString() || rhsT->isStructured() );
                bc.TGETi( slotStack.back(), slotStack.back(), 0, me->d_loc.packed() );
            }
            emitAssig( me->d_lhs.data(), slotStack.back(), me->d_loc );
        }
        releaseSlot(); // rhs
    }

    void visit( ForLoop* me)
    {
        // i := from;
        // WHILE i <= to DO statements; i := i + by END
        // WHILE i >= to DO statements; i := i + by END

        Ref<Assign> a = new Assign();
        a->d_loc = me->d_loc;
        a->d_lhs = me->d_id;
        a->d_rhs = me->d_from;

        Ref<IfLoop> loop = new IfLoop();
        loop->d_loc = me->d_loc;
        loop->d_op = IfLoop::WHILE;

        Ref<BinExpr> cond = new BinExpr();
        cond->d_loc = me->d_loc;
        if( me->d_byVal.toInt() > 0 )
            cond->d_op = BinExpr::LEQ;
        else
            cond->d_op = BinExpr::GEQ;
        cond->d_lhs = me->d_id;
        cond->d_rhs = me->d_to;
        cond->d_type = me->d_id->d_type.data();
        loop->d_if.append( cond.data() );

        loop->d_then.append( me->d_do );

        Ref<BinExpr> add = new BinExpr();
        add->d_loc = me->d_loc;
        add->d_op = BinExpr::ADD;
        add->d_lhs = me->d_id;
        add->d_rhs = me->d_by;
        add->d_type = me->d_by->d_type;

        Ref<Assign> a2 = new Assign();
        a2->d_loc = me->d_loc;
        a2->d_lhs = me->d_id;
        a2->d_rhs = add.data();

        loop->d_then.back().append( a2.data() );

        a->accept(this);
        loop->accept(this);
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

        const quint8 addElemToSet = ctx.back().buySlots(1);
        fetchObxlibMember(addElemToSet,9,me->d_loc);
        const quint8 addRangeToSet = ctx.back().buySlots(1);
        fetchObxlibMember(addRangeToSet,10,me->d_loc);

        for( int i = 0; i < me->d_parts.size(); i++ )
        {
            BinExpr* bi = me->d_parts[i]->getTag() == Thing::T_BinExpr ? cast<BinExpr*>( me->d_parts[i].data() ) : 0;
            if( bi && bi->d_op == BinExpr::Range )
            {
                const int tmp = ctx.back().buySlots(4,true);
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
                const int tmp = ctx.back().buySlots(3,true);
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
        if( slotStack.isEmpty() )
            return; // error already reported

        // prev must be a pointer or a record
        Type* prevT = derefed(me->d_sub->d_type.data());
        Q_ASSERT( prevT );

        // const int res = ctx.back().buySlots(1);
        // reuse slot instead
        const int res = slotStack.back();

        switch( me->d_op )
        {
        case UnExpr::NEG:
            if( prevT->getBaseType() == BaseType::SET )
            {
                const int tmp = ctx.back().buySlots(2,true);
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
            if( prevT->getTag() == Thing::T_ProcType )
            {
                Named* id = me->d_sub->getIdent();
                if( id->getTag() == Thing::T_Procedure )
                {
                    Procedure* p = cast<Procedure*>(id);
                    if( !p->d_receiver.isNull() )
                    {
                        const int proc = ctx.back().buySlots(1);
                        fetchObxlibMember( proc, 40, me->d_loc ); // getmetatable
                        const int tmp = ctx.back().buySlots(2,true);
                        bc.MOV(tmp,proc,me->d_loc.packed());
                        bc.MOV( tmp+1, slotStack[ slotStack.size() - 2 ], me->d_loc.packed() ); // this
                        bc.CALL( tmp, 1, 1, me->d_loc.packed() );
                        bc.MOV( tmp+1, tmp, me->d_loc.packed() ); // class of this
                        bc.MOV(tmp,proc,me->d_loc.packed());
                        bc.CALL( tmp, 1, 1, me->d_loc.packed() );
                        emitGetTableByIndex( slotStack.back(), tmp, p->d_slot, me->d_loc );
                        ctx.back().sellSlots(tmp,2);
                        ctx.back().sellSlots(proc);
                    }
                }
            }else
            {
                // NOP
                // bc.MOV(res,slotStack.back(),me->d_loc.packed());
            }
            break;
        case UnExpr::ADDROF:
            // NOP
            break;
        default:
            qDebug() << "ERR" << me->d_op << thisMod->d_name << me->d_loc.d_row << me->d_loc.d_col;
            Q_ASSERT( false );
            break;
        }
        //releaseSlot();
        //slotStack.push_back(res);
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
            if( slotStack.size() < 2)
                return; // error already reported
            Q_ASSERT( slotStack.size() >= 2 ); // running lhs and rhs should have produced two results/registers
        }else
        {
            if( slotStack.size() < 1 )
                return; // error already reported
            Q_ASSERT( slotStack.size() >= 1 );
        }

        Type* lhsT = derefed(me->d_lhs->d_type.data());
        Type* rhsT = derefed(me->d_rhs->d_type.data());
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
                const int tmp = ctx.back().buySlots(3,true);
                fetchObxlibMember(tmp,23,me->d_loc); // module.is_a
                bc.MOV(tmp+1,slotStack[slotStack.size()-2],me->d_loc.packed());
                bc.MOV(tmp+2, slotStack.back(), me->d_loc.packed() );
                bc.CALL(tmp,1,2,me->d_loc.packed());
                bc.MOV(res,tmp,me->d_loc.packed());
                ctx.back().sellSlots(tmp,3);
            }
            break;
        case BinExpr::ADD:
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) )
                bc.ADD(res, slotStack[slotStack.size()-2], slotStack.back(), me->d_loc.packed() );
            else if( lhsT->isSet() && rhsT->isSet() )
                setOp( res, 12, me->d_loc ); // bit.bor
            else if( lhsT->isText(&lwide) && rhsT->isText(&rwide) )
            {
                checkHandleChar(lhsT,rhsT,lwide,rwide,me->d_loc);
                const int tmp = ctx.back().buySlots(5,true);
                fetchObxlibMember(tmp,13,me->d_loc); // module.joinStrings
                bc.MOV(tmp+1,slotStack[slotStack.size()-2],me->d_loc.packed());
                bc.MOV(tmp+2, slotStack.back(), me->d_loc.packed() );
                bc.KSET(tmp+3, lwide, me->d_loc.packed() );
                bc.KSET(tmp+4, rwide, me->d_loc.packed() );
                bc.CALL(tmp,1,4,me->d_loc.packed());
                bc.MOV(res,tmp,me->d_loc.packed());
                ctx.back().sellSlots(tmp,5);
            }else if( ltag == Thing::T_Pointer || rtag == Thing::T_Pointer )
            {
                qWarning() << "adding of pointer to array of char/wchar not yet supported";
            }else
                Q_ASSERT(false);
            break;
        case BinExpr::SUB:
            if( lhsT->isNumeric() && rhsT->isNumeric() ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) )
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
                const int tmp = ctx.back().buySlots(3,true);
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
                const int tmp = ctx.back().buySlots(3,true);
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
                if( slotStack.size() < 2 )
                    return; // already reported
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
                if( slotStack.size() < 2 )
                    return; // already reported
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
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
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
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
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
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
                    ( lhsT->isChar() && rhsT->isChar() ) )
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
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
                    ( lhsT->isChar() && rhsT->isChar() ) )
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
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
                    ( lhsT->isChar() && rhsT->isChar() ) )
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
            if( ( lhsT->isNumeric() && rhsT->isNumeric() ) ||
                    ( ltag == Thing::T_Enumeration && rtag == Thing::T_Enumeration ) ||
                    ( lhsT->isChar() && rhsT->isChar() ) )
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
        if( id == 0 )
            return; // already reported
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
        case Thing::T_NamedType:
            {
                // happens e.g. in typecase
#if 0
                Type* t = derefed(id->d_type.data());
                if( t && t->getTag() == Thing::T_Pointer )
                    t = derefed(cast<Pointer*>(t)->d_to.data());
                if( !checkValidSlot(t,me->d_loc))
                    return;
                fetchModule( res, me->d_loc );
                emitGetTableByIndex(res, res, t->d_slot, me->d_loc );
#else
                fetchModule( res, me->d_loc );
                fetchClassImp2( res, res, id->d_type.data(), false, me->d_loc );
#endif
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
#ifdef _HAVE_OUTER_LOCAL_ACCESS
                if( ctx.back().scope->d_upvalSource )
                    emitGetTableByIndex(res,0,id->d_slot,me->d_loc);
                else
#endif
                    bc.MOV(res,id->d_slot,me->d_loc.packed());
            }else
                qWarning() << "non local access not yet implemented"; // TODO
            break;
        default:
            qDebug() << "ERR" << tag << thisMod->d_name << me->d_loc.d_row << me->d_loc.d_col;
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
        const bool derefImport = subId && subId->getTag() == Thing::T_Import;
        if( derefImport && subId->d_name.constData() == system.constData() )
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
            else
            {
                Procedure* p = cast<Procedure*>(id);
                Q_ASSERT( id->d_slotValid );
                if( !p->d_receiver.isNull() )
                {
                    // bound procedures require this and fetch from class, not from record
                    // don't release if we use slotStack.back() as this
                    const int tmp = ctx.back().buySlots(2,true);
                    fetchObxlibMember( tmp, 40, me->d_loc ); // getmetatable
                    bc.MOV( tmp+1, slotStack.back(), me->d_loc.packed() );
                    bc.CALL( tmp, 1, 1, me->d_loc.packed() );
                    emitGetTableByIndex( res, tmp, id->d_slot, me->d_loc );
                    ctx.back().sellSlots(tmp,2);
                }else
                {
                    emitGetTableByIndex(res, slotStack.back(), id->d_slot, me->d_loc );
                    releaseSlot();
                }
            }
            break;
        case Thing::T_Field:
        case Thing::T_Variable:
            if( !checkValidSlot(id,me->d_loc) )
                return;
            Q_ASSERT( id->d_slotValid );
            emitGetTableByIndex(res, slotStack.back(), id->d_slot, me->d_loc );
            releaseSlot();
            break;
        case Thing::T_NamedType:
            {
                Q_ASSERT( derefImport );
                Q_ASSERT( id->d_type->toRecord() );
#if 1
                fetchClassImp2( res, slotStack.back(), id->d_type.data(), false, me->d_loc );
#else
                Thing* slotDonor = 0;
                const int tag = id->d_type->getTag();
                if( tag == Thing::T_QualiType )
                {
                    // this is a type alias which eventually points to a record
                    slotDonor = id;
                }else
                {
                    // this is a local type
                    if( tag == Thing::T_Pointer )
                    {
                        Pointer* p = cast<Pointer*>(id->d_type.data());
                        // TODO could point to record or local or remote quali
                    }else
                    {
                        Q_ASSERT( tag == Thing::T_Record );
                        slotDonor = id->d_type.data();
                    }
                }

                if( !checkValidSlot(slotDonor,me->d_loc) )
                    return;
                emitGetTableByIndex(res, slotStack.back(), slotDonor->d_slot, me->d_loc );
#endif
                releaseSlot();
            }
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
            qDebug() << "ERR" << thisMod->d_name << tag << me->d_loc.d_row << me->d_loc.d_col;
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
            me->d_sub->accept(this);
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
        Q_ASSERT( !modName.isEmpty() );
        const int tmp = ctx.back().buySlots(2,true);
        bc.GGET( tmp, "require", loc.packed() );
        bc.KSET( tmp+1, modName, loc.packed() );
        bc.CALL( tmp, 1, 1, loc.packed() );
        bc.MOV(toSlot,tmp,loc.packed() );
        Q_ASSERT( thisMod == ctx.back().scope );
        bc.TSETi(tmp,modSlot,toSlot,loc.packed()); // store a copy also in the module table
        ctx.back().sellSlots(tmp,2);
    }

    void fetchObxlib( quint8 to, const RowCol& loc )
    {
        bc.UGET(to, ctx.back().resolveUpval(obxlj,"@obxlj"), loc.packed() );
    }

    void fetchObxlibMember( quint8 to, quint8 what, const RowCol& loc )
    {
        if( ctx.back().scope == thisMod )
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
        if( ctx.back().scope == thisMod )
            bc.MOV( to, modSlot, loc.packed() );
        else
            bc.UGET(to, ctx.back().resolveUpval(modSlot,"@mod"), loc.packed() );
    }

    void emitCreateRecord( quint8 to, Type* t, const RowCol& loc )
    {
        Record* r = t->toRecord();
        Q_ASSERT( r );
        bc.TNEW( to, r->d_fieldCount, 0, loc.packed() );

        const int tmp = ctx.back().buySlots(4,true);
        fetchClass(tmp, t, loc );

        // call setmetatable
        int base = tmp+1;
        fetchObxlibMember(base, 22, loc ); // setmetatable

        bc.MOV(base+1, to, loc.packed() );
        bc.MOV(base+2, tmp, loc.packed() );
        bc.CALL( base, 0, 2, loc.packed() );
        ctx.back().sellSlots(tmp,4);
    }

    void emitCreateArray( quint8 to, Type* baseType, quint32 constLen, quint8 lenSlot, const RowCol& loc )
    {
        baseType = derefed(baseType);
        Q_ASSERT( baseType );
        if( baseType->getBaseType() > 0 )
        {
            const int tmp = ctx.back().buySlots(2,true);
            switch( baseType->getBaseType() )
            {
            case Type::BOOLEAN:
                fetchObxlibMember(tmp,43,loc); // module.createBoolArray
                break;
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
            if( constLen > 0 )
                bc.KSET(tmp+1, constLen, loc.packed() );
            else
                bc.MOV(tmp+1, lenSlot, loc.packed() );
            bc.CALL(tmp,1,1,loc.packed());
            bc.MOV(to,tmp,loc.packed());
            ctx.back().sellSlots(tmp,2);
        }else
        {
            bc.TNEW( to, constLen, 0, loc.packed() );
            if( constLen > 0 )
            {
                const int tmp = ctx.back().buySlots(1);
                bc.KSET( tmp, constLen, loc.packed() );
                bc.TSET(tmp,to,"count",loc.packed());
                ctx.back().sellSlots(tmp);
            }else
            {
                bc.TSET(lenSlot,to,"count",loc.packed());
            }
        }
    }

    bool emitInitializer( quint8 to, Type* t, bool resolvePtr, bool isLocalVar, const RowCol& loc,
                          const QList<Expression*>& lengths = QList<Expression*>() )
    {
        // note that this proc is also called if t is a pointer

        // expects non-derefed t!
        Type* td = derefed(t);
        Q_ASSERT( td );

        if( resolvePtr && td->getTag() == Thing::T_Pointer )
            td = derefed(cast<Pointer*>(td)->d_to.data());

        switch( td->getTag() )
        {
        case Thing::T_BaseType:
            // at least the oberon system assumes initialized module variables
            // nil is the default value of luajit tables and not suited as a default BaseType
            if( td->getBaseType() == Type::BOOLEAN )
                bc.KSET(to,false,loc.packed());
            else
                bc.KSET(to,0.0,loc.packed());
            return true;
        case Thing::T_Enumeration:
            bc.KSET(to,0,loc.packed());
            return true;
        case Thing::T_Pointer:
            if( isLocalVar )
                bc.KNIL(to,1,loc.packed() ); // luajit apparently doesn't initialize local vars
            break;
        case Thing::T_Record:
            {
                Record* r = cast<Record*>(td);
                emitCreateRecord( to, t, loc );

                // initialize fields
                const int tmp = ctx.back().buySlots(1);
                QList<Field*> fields = r->getOrderedFields();
                for( int i = 0; i < fields.size(); i++ )
                {
                    Type* t2 = fields[i]->d_type.data();
                    Type* t2d = derefed(t2);
                    Q_ASSERT( t2d );
                    // oberon system expects all vars to be initialized
                    if( emitInitializer(tmp, t2, false, false, loc ) )
                            emitSetTableByIndex(tmp,to,fields[i]->d_slot,loc);
                }
                ctx.back().sellSlots(tmp);
            }
            return true;
        case Thing::T_Array:
            {
                Array* a = cast<Array*>(td);
                Type* td = derefed(a->d_type.data());

                int lenSlot = -1;
                if( !lengths.isEmpty() )
                {
                    // len comes from NEW( ptr, expression ) instead of compile time constant
                    Q_ASSERT( a->d_lenExpr.isNull() );
                    lengths.first()->accept(this);
                    Q_ASSERT( !slotStack.isEmpty() );
                    lenSlot = slotStack.back();
                    emitCreateArray(to, td, 0, lenSlot, loc );
                    slotStack.pop_back(); // keep lenSlot for now
                }else
                {
                    Q_ASSERT( !a->d_lenExpr.isNull() );
                    emitCreateArray(to, td, a->d_len, 0, loc );
                }

                if( td->isStructured() )
                {
                    const quint8 base = ctx.back().buySlots(4);
                    const int tmp = ctx.back().buySlots(1);
                    bc.KSET(base,0,loc.packed());
                    if( !lengths.isEmpty() )
                    {
                        bc.SUB(lenSlot, lenSlot, QVariant(1), loc.packed() );
                        bc.MOV(base+1, lenSlot, loc.packed() );
                    }else
                        bc.KSET(base+1, a->d_len-1,loc.packed());

                    if( lenSlot >= 0 )
                    {
                        ctx.back().sellSlots(lenSlot); // free as soon as possible, i.e. before recursion
                        lenSlot = -1;
                    }

                    bc.KSET(base+2,1,loc.packed());
                    bc.FORI(base,0,loc.packed());
                    const quint32 pc = bc.getCurPc();

                    if( lengths.size() > 1 )
                        emitInitializer(tmp, a->d_type.data(), false, false, loc, lengths.mid(1) );
                    else
                        emitInitializer(tmp, a->d_type.data(), false, false, loc );
                    bc.TSET(tmp,to,base+3, loc.packed() );

                    bc.FORL(base, pc - bc.getCurPc() - 1,loc.packed());
                    bc.patch(pc);

                    ctx.back().sellSlots(tmp);
                    ctx.back().sellSlots(base,4);
                }
                if( lenSlot >= 0 )
                    ctx.back().sellSlots(lenSlot);
            }
            return true;
        }
        return false;
    }

    void emitInitializer( Named* me )
    {
        switch( me->getTag() )
        {
        case Thing::T_Variable:
            {
                const int tmp = ctx.back().buySlots(1);
                if( emitInitializer( tmp, me->d_type.data(), false, false, me->d_loc ) )
                {
                    Q_ASSERT( ctx.back().scope == thisMod );
                    emitSetTableByIndex( tmp, modSlot, me->d_slot, me->d_loc );
                }
                ctx.back().sellSlots(tmp);
            }
            break;
        case Thing::T_LocalVar:
#ifdef _HAVE_OUTER_LOCAL_ACCESS
            if( ctx.back().scope->d_upvalSource )
            {
                const int tmp = ctx.back().buySlots(1);
                if( emitInitializer( tmp, me->d_type.data(), false, me->d_loc ) )
                    emitSetTableByIndex(tmp,0,me->d_slot, me->d_loc);
                ctx.back().sellSlots(tmp);
            }else
#endif
                emitInitializer( me->d_slot, me->d_type.data(), false, true, me->d_loc );
            break;
        case Thing::T_Parameter:
            Q_ASSERT( false );
        }
    }

    void releaseSlot()
    {
        if( slotStack.isEmpty() )
            return; // error already reported
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
        const int tmp = ctx.back().buySlots(6,true);
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
        const int tmp = ctx.back().buySlots(3,true);
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
        const int tmp = ctx.back().buySlots(3,true);
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
        case Literal::Enum:
            bc.KSET(res, val, loc.packed() );
            break;
        case Literal::String:
            {
                const int tmp = ctx.back().buySlots(3,true);
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
                    bc.TGETi(tmp,tmp,1,loc.packed()); // charToStringArray
                    bc.CALL(tmp,1,2,loc.packed());
                }
                bc.MOV(res,tmp,loc.packed());
                ctx.back().sellSlots(tmp,3);
            }
            break;
        case Literal::Bytes:
            {
                const int tmp = ctx.back().buySlots(3,true);
                fetchObxlibMember(tmp,8,loc); // createByteArray
                bc.KSET(tmp+1,val.toByteArray().size(),loc.packed());
                bc.KSET(tmp+2,val,loc.packed());
                bc.CALL(tmp,1,2,loc.packed());
                bc.MOV(res,tmp,loc.packed());
                ctx.back().sellSlots(tmp,3);
            }
            break;
        case Literal::Char:
            bc.KSET(res,val,loc.packed());
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
            const int tmp = ctx.back().buySlots(1);
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
            const int tmp = ctx.back().buySlots(1);
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
        if( slotStack.isEmpty() )
            return; // error already reported
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

    void emitBuiltIn( BuiltIn* bi, ArgExpr* ae )
    {
        const int res = ctx.back().buySlots(1);
        switch( bi->d_func )
        {
        case BuiltIn::MAX:
        case BuiltIn::MIN:
            if( ae->d_args.size() == 1 )
            {
                Type* t = derefed(ae->d_args.first()->d_type.data());
                switch( t->getTag() )
                {
                case Thing::T_BaseType:
                    {
                    BaseType* bt = cast<BaseType*>(t);
                    if( bi->d_func == BuiltIn::MAX )
                        bc.KSET(res, bt->maxVal(), ae->d_args.first()->d_loc.packed() );
                    else
                        bc.KSET(res, bt->minVal(), ae->d_args.first()->d_loc.packed() );
                    }
                    break;
                default:
                    Q_ASSERT( false );
                }
            }else if( ae->d_args.size() == 2 )
            {
                ae->d_args.first()->accept(this);
                ae->d_args.last()->accept(this);
                Q_ASSERT( slotStack.size() >= 2 );
                if( bi->d_func == BuiltIn::MAX )
                    bc.ISGE( slotStack[ slotStack.size()-2 ], slotStack[ slotStack.size()-1 ], ae->d_loc.packed() );
                else
                    bc.ISLE( slotStack[ slotStack.size()-2 ], slotStack[ slotStack.size()-1 ], ae->d_loc.packed() );
                bc.JMP(ctx.back().pool.d_frameSize,0, ae->d_loc.packed());
                const quint32 pc1 = bc.getCurPc();
                bc.MOV(res, slotStack[ slotStack.size()-1 ], ae->d_loc.packed() );
                bc.JMP(ctx.back().pool.d_frameSize,0,ae->d_loc.packed());
                const quint32 pc2 = bc.getCurPc();
                bc.patch(pc1);
                bc.MOV(res, slotStack[ slotStack.size()-2 ], ae->d_loc.packed() );
                bc.patch(pc2);
                releaseSlot();
                releaseSlot();
            }else
                Q_ASSERT( false );
            break;
        case BuiltIn::DEFAULT:
            {
                Q_ASSERT( !ae->d_args.isEmpty() && !ae->d_args.first()->d_type.isNull() );
                Expression* e = ae->d_args.first().data();
                emitDefault(res,e->d_type.data(),e->d_loc);
            }
            break;
        case BuiltIn::PRINTLN:
            {
                Q_ASSERT( ae->d_args.size() == 1 );
                ae->d_args.first()->accept(this);
                Q_ASSERT( !slotStack.isEmpty() );
                const int tmp = ctx.back().buySlots(2,true);
                fetchObxlibMember(tmp,25,ae->d_loc); // module.println
                bc.MOV(tmp+1,slotStack.back(),ae->d_loc.packed() );
                bc.CALL(tmp,0,1,ae->d_loc.packed());
                releaseSlot();
                ctx.back().sellSlots(tmp,2);
            }
            break;
        case BuiltIn::TRACE:
            {
                Q_ASSERT( ae->d_args.size() == 1 && ae->d_args.first()->d_type->isString() );
                ae->d_args.first()->accept(this);
                Q_ASSERT( !slotStack.isEmpty() );
                const int tmp = ctx.back().buySlots(2,true);
                fetchObxlibMember(tmp,47,ae->d_loc); // module.trace
                bc.MOV(tmp+1,slotStack.back(),ae->d_loc.packed() );
                bc.CALL(tmp,0,1,ae->d_loc.packed());
                releaseSlot();
                ctx.back().sellSlots(tmp,2);
            }
            break;
        case BuiltIn::NOP:
            {
                Q_ASSERT( ae->d_args.isEmpty() );
                const int tmp = ctx.back().buySlots(1,true);
                fetchObxlibMember(tmp,48,ae->d_loc); // module.nop
                bc.CALL(tmp,0,0,ae->d_loc.packed());
            }
            break;
        case BuiltIn::INC:
        case BuiltIn::DEC:
            {
                Ref<BinExpr> add = new BinExpr();
                add->d_lhs = ae->d_args.first();
                add->d_type = ae->d_args.first()->d_type;
                add->d_loc = ae->d_args.first()->d_loc;
                if( bi->d_func == BuiltIn::INC )
                    add->d_op = BinExpr::ADD;
                else
                    add->d_op = BinExpr::SUB;
                if( ae->d_args.size() == 1 )
                {
                    add->d_rhs = new Literal( Literal::Integer,add->d_loc,qlonglong(1));
                    add->d_rhs->d_type = ae->d_args.first()->d_type;
                }else
                {
                    Q_ASSERT( ae->d_args.size() == 2 );
                    add->d_rhs = ae->d_args.last();
                }
                Ref<Assign> ass = new Assign();
                ass->d_lhs = ae->d_args.first();
                ass->d_loc = ae->d_loc;
                ass->d_rhs = add.data();
                ass->accept(this);
            }
            break;
        case BuiltIn::TRAP:
            {
                const quint8 tmp = ctx.back().buySlots(1,true);
                fetchObxlibMember(tmp,28,ae->d_loc); // TRAP
                bc.CALL(tmp,0,0,ae->d_loc.packed());
                ctx.back().sellSlots(tmp,1);
            }
            break;
        case BuiltIn::TRAPIF:
            {
                Q_ASSERT( ae->d_args.size() == 1 );
                ae->d_args.first()->accept(this);
                Q_ASSERT( !slotStack.isEmpty() );

                const quint8 tmp = ctx.back().buySlots(2,true);
                fetchObxlibMember(tmp,28,ae->d_loc); // TRAP
                bc.MOV(tmp+1,slotStack.back(),ae->d_loc.packed());
                bc.CALL(tmp,0,1,ae->d_loc.packed());
                ctx.back().sellSlots(tmp,2);
                releaseSlot();
            }
            break;
        case BuiltIn::HALT:
            {
                Q_ASSERT( ae->d_args.size() == 1 );
                ae->d_args.first()->accept(this);
                Q_ASSERT( !slotStack.isEmpty() );

                const quint8 tmp = ctx.back().buySlots(2,true);
                fetchObxlibMember(tmp,44,ae->d_loc); // ABORT 44, ObxFfi_CRASH 46
                bc.MOV(tmp+1,slotStack.back(),ae->d_loc.packed());
                bc.CALL(tmp,0,1,ae->d_loc.packed());
                ctx.back().sellSlots(tmp,2);
                releaseSlot();
            }
            break;
        case BuiltIn::ASSERT:
            {
                Q_ASSERT( !ae->d_args.isEmpty() ); // TODO: by now second optional arg ignored!
                ae->d_args.first()->accept(this);
                Q_ASSERT( !slotStack.isEmpty() );

                const quint8 tmp = ctx.back().buySlots(4,true);
                fetchObxlibMember(tmp,29,ae->d_loc); // ASSERT
                bc.MOV(tmp + 1, slotStack.back(), ae->d_loc.packed() );
                bc.KSET(tmp + 2, QVariant::fromValue(thisMod->getName()), ae->d_loc.packed() );
                bc.KSET(tmp + 3, ae->d_loc.d_row, ae->d_loc.packed() );
                bc.CALL(tmp,0,3, ae->d_loc.packed());
                ctx.back().sellSlots(tmp,4);
                releaseSlot();
            }
            break;
        case BuiltIn::NEW:
            {
                Q_ASSERT( !ae->d_args.isEmpty() );

                Type* t = ae->d_args.first()->d_type.data();
                Type* td = derefed(t);
                Q_ASSERT( td && td->getTag() == Thing::T_Pointer );
                //Pointer* ptr = cast<Pointer*>(td);

                const int tmp = ctx.back().buySlots(1);
                QList<Expression*> dims;
                for( int i = 1; i < ae->d_args.size(); i++ )
                    dims << ae->d_args[i].data();

                // we must pass t here (not ptr->d_to) because the pointer could be a named type defined in another module;
                // if we deref the pointer we lose the module information
                emitInitializer(tmp, t, true, false, ae->d_loc, dims );
                emitAssig(ae->d_args.first().data(), tmp, ae->d_loc );
                ctx.back().sellSlots(tmp);
            }
            break;
        case BuiltIn::LEN:
            {
                // TODO: len with two args
                Q_ASSERT( !ae->d_args.isEmpty() );
                Type* t = derefed(ae->d_args.first()->d_type.data() );
                if( t && t->getTag() == Thing::T_Pointer )
                    t = derefed( cast<Pointer*>(t)->d_to.data() );

                if( t->isString() )
                {
                    ae->d_args.first()->accept(this);
                    Q_ASSERT( !slotStack.isEmpty() );
                    emitCalcDynLen( res, slotStack.back(), t->getBaseType(), ae->d_loc );
                    releaseSlot();
                }else
                {
                    Q_ASSERT( t->getTag() == Thing::T_Array );
                    Array* a = cast<Array*>(t);
                    Type* at = derefed( a->d_type.data() );
                    Q_ASSERT( at );
                    if( a->d_len > 0 )
                    {
                        bc.KSET( res, a->d_len, ae->d_loc.packed() );
                    }else
                    {
                        ae->d_args.first()->accept(this);
                        Q_ASSERT( !slotStack.isEmpty() );
                        emitCalcDynLen( res, slotStack.back(), at->getBaseType(), ae->d_loc );
                        releaseSlot();
                    }
                }
            }
            break;
        case BuiltIn::INCL:
            emitCallBuiltIn2var1(res,9,ae); // addElemToSet
            break;
        case BuiltIn::EXCL:
            emitCallBuiltIn2var1(res,30,ae); // module.removeElemFromSet
            break;
        case BuiltIn::PACK:
            emitCallBuiltIn2var1(res,31,ae); // math.ldexp
            break;
        case BuiltIn::UNPK:
            {
                Q_ASSERT( ae->d_args.size() == 2 );
                Accessor x;
                accessor( ae->d_args.first().data(), x );
                Accessor n;
                accessor( ae->d_args.last().data(), n );

                const quint8 tmp = ctx.back().buySlots(3, true);
                fetchObxlibMember(tmp,32,ae->d_loc); // module.UNPACK

                emitAccToSlot( tmp+1, x, ae->d_loc );

                emitAccToSlot( tmp+2, n, ae->d_loc );

                bc.CALL(tmp,2,2,ae->d_loc.packed());
                emitSlotToAcc(x,tmp,ae->d_loc);
                emitSlotToAcc(n,tmp+1,ae->d_loc);
                ctx.back().sellSlots(tmp,3);
                releaseAcc(x);
                releaseAcc(n);
            }
            break;
        case BuiltIn::ORD:
        case BuiltIn::CHR:
        case BuiltIn::FLT:
            {
                Q_ASSERT( ae->d_args.size() == 1 );
                ae->d_args.first()->accept(this);
                if( slotStack.isEmpty() )
                    return; // error already reported
                Type* t = derefed(ae->d_args.first()->d_type.data() );
                if( bi->d_func == BuiltIn::ORD && t && t->getTag() == Thing::T_Pointer )
                {   // undocumented oberon feature
                    const int tmp = ctx.back().buySlots(2,true);
                    fetchObxlibMember(tmp,42,ae->d_loc); // module.ADDRESSOF
                    bc.MOV(tmp+1,slotStack.back(),ae->d_loc.packed() );
                    bc.CALL(tmp,1,1,ae->d_loc.packed());
                    bc.MOV(res,tmp,ae->d_loc.packed());
                    ctx.back().sellSlots(tmp,2);
                }else if( t && t->getBaseType() == Type::BOOLEAN )
                {
                    const int tmp = ctx.back().buySlots(2,true);
                    fetchObxlibMember(tmp,39,ae->d_loc); // module.bool_to_number
                    bc.MOV(tmp+1,slotStack.back(),ae->d_loc.packed() );
                    bc.CALL(tmp,1,1,ae->d_loc.packed());
                    bc.MOV(res,tmp,ae->d_loc.packed());
                    ctx.back().sellSlots(tmp,2);
                }else
                {
                    if( t && ( t->isString() || t->isStructured() ) )
                        bc.TGETi( slotStack.back(), slotStack.back(), 0, ae->d_loc.packed() );
                    bc.MOV(res, slotStack.back(), ae->d_loc.packed() );
                }
                releaseSlot();
            }
            break;
        case BuiltIn::ODD:
            emitCallBuiltIn1(res,33,ae); // module.ODD
            break;
        case BuiltIn::ABS:
            emitCallBuiltIn1(res,34,ae); // module.abs
            break;
        case BuiltIn::FLOOR:
            emitCallBuiltIn1(res,38,ae); // module.floor
            break;
        case BuiltIn::LSL:
            emitCallBuiltIn2(res, 35, ae ); // bit.lshift
            break;
        case BuiltIn::ASR:
            emitCallBuiltIn2(res, 36, ae ); // bit.arshift
            break;
        case BuiltIn::ROR:
            emitCallBuiltIn2(res, 37, ae ); // bit.ror
            break;
        case BuiltIn::BITAND:
            emitCallBuiltIn2(res, 19, ae ); // bit.band
            break;
        case BuiltIn::BITOR:
            emitCallBuiltIn2(res, 12, ae ); // bit.bor
            break;
        case BuiltIn::BITXOR:
            emitCallBuiltIn2(res, 41, ae ); // bit.bxor
            break;
        case BuiltIn::BITNOT:
            emitCallBuiltIn1(res, 11, ae ); // bit.bnot
            break;
        case BuiltIn::BYTESIZE:
            {
                Q_ASSERT( !ae->d_args.isEmpty() && !ae->d_args.first()->d_type.isNull() );
                Expression* e = ae->d_args.first().data();
                Type* t = derefed(e->d_type.data());
                switch( t->getBaseType() )
                {
                case Type::BOOLEAN:
                case Type::CHAR:
                case Type::BYTE:
                    bc.KSET(res,1,e->d_loc.packed());
                    break;
                case Type::WCHAR:
                case Type::SHORTINT:
                    bc.KSET(res,2,e->d_loc.packed());
                    break;
                case Type::INTEGER:
                case Type::REAL:
                case Type::SET:
                    bc.KSET(res,4,e->d_loc.packed());
                    break;
                case Type::LONGINT:
                case Type::LONGREAL:
                    bc.KSET(res,8,e->d_loc.packed());
                    break;
                default:
                    switch( t->getTag() )
                    {
                    case Thing::T_Pointer:
                        bc.KSET(res,4,e->d_loc.packed());
                        break;
                    case Thing::T_Record:
                    case Thing::T_Array:
                        bc.KSET(res, 0,e->d_loc.packed()); // TODO
                        break;
                    default:
                        Q_ASSERT( false ); // TODO
                        break;
                    }
                    break;
                }
            }
            break;
        case BuiltIn::SHORT:
        case BuiltIn::LONG:
            // TODO: other versions of short/long
            ae->d_args.first()->accept(this);
            Q_ASSERT( !slotStack.isEmpty() );
            bc.MOV(res,slotStack.back(),ae->d_loc.packed());
            releaseSlot();
            break;
        default:
            // TODO
            qWarning() << "missing generator implementation of" << BuiltIn::s_typeName[bi->d_func];
            break;
        }
        slotStack.push_back(res);
    }

    void emitCalcDynLen(quint8 res, quint8 table, quint8 baseType, const RowCol& loc )
    {
        if( baseType > 0 )
        {
            const int tmp = ctx.back().buySlots(2,true);
            fetchObxlibMember(tmp, 26, loc ); // bytesize
            bc.MOV( tmp+1, table, loc.packed() );

            bc.CALL( tmp, 1, 1, loc.packed() );
            bc.MOV(res, tmp, loc.packed() );
            ctx.back().sellSlots(tmp,2);
            int divisor = 1;

            switch( baseType )
            {
            case Type::BOOLEAN:
            case Type::CHAR:
            case Type::BYTE:
            case Type::STRING: // STRING happens e.g. in LEN("test")
                // NOP
                break;
            case Type::SHORTINT:
            case Type::WCHAR:
            case Type::WSTRING:
                divisor = 2;
                break;
            case Type::INTEGER:
            case Type::SET:
            case Type::REAL:
                divisor = 4;
                break;
            case Type::LONGINT:
            case Type::LONGREAL:
                divisor = 8;
                break;
            default:
                Q_ASSERT( false );
                break;
            }
            if( divisor > 1 )
                bc.DIV(res,res,QVariant(divisor), loc.packed());
        }else
        {
            bc.TGET( res, table, "count", loc.packed() );
        }
    }

    void emitCallBuiltIn2(quint8 res, int bi, ArgExpr* ae)
    {
        Q_ASSERT( ae->d_args.size() == 2 );
        const quint8 tmp = ctx.back().buySlots(3, true);
        fetchObxlibMember(tmp,bi,ae->d_loc);

        ae->d_args.first()->accept(this);
        if( slotStack.isEmpty() )
            return; // error already reported
        bc.MOV(tmp+1, slotStack.back(), ae->d_loc.packed() );
        releaseSlot();

        ae->d_args.last()->accept(this);
        if( slotStack.isEmpty() )
            return; // error already reported
        bc.MOV(tmp+2, slotStack.back(), ae->d_loc.packed() );
        releaseSlot();

        bc.CALL(tmp,1,2,ae->d_loc.packed());
        bc.MOV(res,tmp,ae->d_loc.packed());
        ctx.back().sellSlots(tmp,3);
    }

    void emitCallBuiltIn2var1(quint8 res, int bi, ArgExpr* ae)
    {
        Q_ASSERT( ae->d_args.size() == 2 );

        Accessor acc;
        accessor( ae->d_args.first().data(), acc ); // NOTE: acc has to be allocated before tmp!

        const quint8 tmp = ctx.back().buySlots(3, true);
        fetchObxlibMember(tmp,bi,ae->d_loc);

        emitAccToSlot( tmp+1, acc, ae->d_loc );

        ae->d_args.last()->accept(this);
        Q_ASSERT( !slotStack.isEmpty() );
        bc.MOV(tmp+2, slotStack.back(), ae->d_loc.packed() );
        releaseSlot();

        bc.CALL(tmp,1,2,ae->d_loc.packed());
        emitSlotToAcc(acc,tmp,ae->d_loc);
        ctx.back().sellSlots(tmp,3);
        releaseAcc(acc);
    }

    void emitCallBuiltIn1(quint8 res, int bi, ArgExpr* ae )
    {
        Q_ASSERT( ae->d_args.size() == 1 );
        const quint8 tmp = ctx.back().buySlots(2, true);
        fetchObxlibMember(tmp,bi,ae->d_loc);

        ae->d_args.last()->accept(this);
        if( slotStack.isEmpty() )
            return; // error already reported
        bc.MOV(tmp+1, slotStack.back(), ae->d_loc.packed() );
        releaseSlot();

        bc.CALL(tmp,1,1,ae->d_loc.packed());
        bc.MOV(res,tmp,ae->d_loc.packed());
        ctx.back().sellSlots(tmp,2);
    }

    bool trueVarParam( Parameter* p )
    {
        if( !p->d_var || p->d_const )
            return false;
        Q_ASSERT( p->d_var && !p->d_const );
#if 1
        return true;
        // NOTE: we also need true var for pointers, otherwise new(p) in a procedure with var param doesn't work.
        // therefore the following is obsolete:
#else
        // var params of structured types don't need special care since LJ all structured types are accessed by ref
        // a true var param is therefore only a basic type, pointer, enum or proc type (i.e. !isStructured)
        Type* t = derefed(p->d_type.data());
        Q_ASSERT( t );
        return !t->isStructured();
#endif
    }

    void emitCall( ArgExpr* me )
    {
        const int res = ctx.back().buySlots(1);

        Q_ASSERT( me->d_sub );
        me->d_sub->accept(this);

        Named* func = 0;
        if( me->d_sub->getUnOp() == UnExpr::DEREF )
        {
            // call to superclass method
            UnExpr* ue = cast<UnExpr*>(me->d_sub.data());
            func = ue->d_sub->getIdent();
        }else
            func = me->d_sub->getIdent();

        if( func && func->getTag() == Thing::T_BuiltIn )
        {
            emitBuiltIn( cast<BuiltIn*>(func), me );
            return;
        }

        Type* subT = derefed( me->d_sub->d_type.data() );
        Q_ASSERT( subT && subT->getTag() == Thing::T_ProcType );
        ProcType* pt = cast<ProcType*>( subT );
        Q_ASSERT( pt->d_formals.size() == me->d_args.size() );

#ifdef _HAVE_OUTER_LOCAL_ACCESS
        bool passFrame = false;
        if( pt->d_ident && ( pt->d_ident->d_upvalIntermediate || pt->d_ident->d_upvalSink ) )
            passFrame = true;
#endif

        QVector<Accessor> accs(me->d_args.size());
        int varCount = 0; // number of true var params which have to be returned by the function
        for( int i = 0; i < me->d_args.size(); i++ )
        {
            if( trueVarParam( pt->d_formals[i].data() ) )
            {
                Q_ASSERT( pt->d_formals[i]->d_var && !pt->d_formals[i]->d_const );
                // here is a VAR but not IN param
                varCount++;
                accessor( me->d_args[i].data(), accs[i] );
            }
        }

        bool isBound = false;
        if( func && func->getTag() == Thing::T_Procedure )
        {
            Procedure* p = cast<Procedure*>(func);
            isBound = !p->d_receiver.isNull();
        }

        const int funcCount = 1;
        // we always assume a return value if there are vars to be returned even if there isn't one
        const int retCount = pt->d_return.isNull() && varCount == 0 ? 0 : 1;

        const int thisCount = ( isBound ? 1 : 0 );
        const int argCount = me->d_args.size()
#ifdef _HAVE_OUTER_LOCAL_ACCESS
                + ( passFrame ? 1 : 0 )
#endif
                ;

        const int slot = ctx.back().buySlots( funcCount + thisCount + argCount, true ); // Allocate the slots for the call

        if( slotStack.isEmpty() )
            return; // error already reported
        bc.MOV( slot, slotStack.back(), me->d_loc.packed() );
        releaseSlot();

        if( isBound )
        {
            if( slotStack.size() < 1 )
                return; // already reported
            Q_ASSERT( slotStack.size() >= 1 );
            bc.MOV(slot+funcCount, slotStack.back(), me->d_loc.packed() ); // 'this' is always the first param
            releaseSlot();
        }

        for( int i = 0; i < me->d_args.size(); i++ )
        {
            const int off = funcCount + thisCount + i;
            if( accs[i].kind != Accessor::Invalid )
            {
                Q_ASSERT( pt->d_formals[i]->d_var && !pt->d_formals[i]->d_const );
                emitAccToSlot(slot+off, accs[i], me->d_args[i]->d_loc );
            }else
            {
                // here all by val (i.e. !d_var) or IN (i.e. d_var && d_const)
                me->d_args[i]->accept(this);
                if( slotStack.isEmpty() )
                    return; // error already reported

                Type* lhsT = derefed(pt->d_formals[i]->d_type.data());
                Q_ASSERT( lhsT != 0 );
                if( !pt->d_formals[i]->d_var && lhsT->isStructured() )
                {
                    // a structured arg (record, array) passed by val
                    emitCopy( true, pt->d_formals[i]->d_type.data(), slot+off,
                              me->d_args[i]->d_type.data(), slotStack.back(), me->d_loc );
                }else
                {
                    // a structured arg passed to IN, i.e. just pass the reference
                    // or a non-structured arg passed by IN or by val, just pass the value in both cases
                    Type* rhsT = derefed(me->d_args[i]->d_type.data());
                    Q_ASSERT( rhsT != 0 );
                    // the same code here as in visit(Assign*) if one-char-string to char:
                    if( lhsT->isChar() && !rhsT->isChar() )
                    {
                        Q_ASSERT( rhsT->isString() || rhsT->isStructured() );
                        bc.TGETi( slotStack.back(), slotStack.back(), 0, me->d_loc.packed() );
                    }
                    bc.MOV(slot+off, slotStack.back(), me->d_args[i]->d_loc.packed() );
                }
                releaseSlot();
            }
        }
#ifdef _HAVE_OUTER_LOCAL_ACCESS
        if( passFrame )
        {
            Q_ASSERT( pt->d_ident );
            // TODO: go up the frame chain corresponding to the position of pt in the chain
            // outer frame is allocated at end of param list; subsequent local vars consider it in slot allocation
            if( ctx.back().scope->d_upvalSource )
                bc.MOV(tmp + 1 + me->d_args.size(), 0, me->d_loc.packed() );
            else
            {
                Q_ASSERT( ctx.back().scope->getTag() == Thing::T_Procedure );
                Procedure* p = cast<Procedure*>(ctx.back().scope);
                bc.MOV(tmp + 1 + me->d_args.size(), p->d_parCount, me->d_loc.packed() );
            }
        }
#endif

        bc.CALL( slot, retCount + varCount, thisCount + argCount, me->d_loc.packed() );
        if( retCount )
            bc.MOV(res, slot, me->d_loc.packed() );

        // handle returned vars
        int off = retCount;
        foreach( const Accessor& acc, accs )
        {
            Q_ASSERT( retCount != 0 );
            if( acc.kind == Accessor::Invalid )
                continue;
            emitSlotToAcc(acc,slot+off,me->d_loc);
            off++;
            releaseAcc(acc);
        }

        ctx.back().sellSlots( slot, funcCount + thisCount + argCount );
        slotStack.push_back(res);
    }

    static void allocateClasses( Record* r, quint32& slotNr, Module* thisMod, bool resolveSuper )
    {
        if( r->d_baseRec && !r->d_baseRec->d_slotValid
                && r->d_base->d_quali->getTag() == Thing::T_IdentLeaf ) // treat only local records here
            allocateClasses( r->d_baseRec, slotNr, thisMod, resolveSuper );

        if( !r->d_slotValid )
        {
           r->setSlot(slotNr++);
           quint32 nr = 0;
           if( r->d_baseRec )
               nr = r->d_baseRec->d_fieldCount;
           for( int i = 0; i < r->d_fields.size(); i++ )
           {
               if( r->d_fields[i]->d_super == 0 )
                   r->d_fields[i]->setSlot( nr++ );
           }
           r->d_fieldCount = nr;
           nr = 0;
           if( r->d_baseRec )
               nr = r->d_baseRec->d_methCount;
           for( int i = 0; i < r->d_methods.size(); i++ )
           {
               if( r->d_methods[i]->d_super == 0 )
                   r->d_methods[i]->setSlot( nr++ );
               allocateLocals(r->d_methods[i].data());
           }
           r->d_methCount = nr;
        }else if( resolveSuper )
        {
            // slots of super fields and methods are only available when the super is validated
            // with generic modules it can happen that this is not the case when slots of the module importing
            // the generic instances are allocated; see e.g. Are-we-fast-yet CD.handleNewFrame.apply
            Q_ASSERT( r->d_slotValid );
            for( int i = 0; i < r->d_fields.size(); i++ )
            {
                if( r->d_fields[i]->d_super )
                {
                    if( !r->d_fields[i]->d_super->d_slotValid )
                    {
                        qCritical() << "ERROR invalid slot in" << r->d_fields[i]->getQualifiedName().join('.');
                        // Q_ASSERT( false );
                    }else
                        r->d_fields[i]->setSlot( r->d_fields[i]->d_super->d_slot );
                }
            }
            for( int i = 0; i < r->d_methods.size(); i++ )
            {
                if( r->d_methods[i]->d_super )
                {
                    if( !r->d_methods[i]->d_super->d_slotValid )
                    {
                        qCritical() << "ERROR invalid slot in" << r->d_methods[i]->getQualifiedName().join('.');
                        // TODO Q_ASSERT( false );
                    }else
                        r->d_methods[i]->setSlot( r->d_methods[i]->d_super->d_slot );
                }
            }
        }
    }

    void fetchClass( quint8 to, Type* t, const RowCol& loc )
    {
        // t is a record base (qualitype), a local alias (qualitype) or a variable/param/field type (quali, ptr or record)
        // a record base starts with a quali which is no alias and could point to local or imported module (named type, ptr or record)
        //     this quali when it leads to a pointer could lead to another quali refering to yet another module
        //     so also pointers pointing to qualis should have a slot
        if( ctx.back().scope == thisMod )
            fetchClassImp2( to, modSlot, t, false, loc );
        else
        {
            fetchModule(to,loc);
            fetchClassImp2( to, to, t, false, loc );
       }
    }

    void fetchClassImp2( quint8 to, quint8 mod, Type* t, bool, const RowCol& loc )
    {
        // t is a
        //     record or a pointer
        //     named type in this module
        //     named type in an imported module

        // Cases to be handled when finding class tables:
        // ptr rec (direct)
        // A ptr rec (same mod)
        // A ptr B rec (same mod)
        // A ptr B.C rec (ptr and rec different mod)
        // A.B ptr rec
        // A.B ptr C rec (C in module A)
        // A.B ptr C.D rec (C might not be in orig import list, and no alias involved!)
        // rec
        // A rec
        // A.B rec

        // mind qualis can be arbitrary len chains leading to any module along the dependency tree:
        // e.g. A B.C D E.F rec
        // only a dotted qualitype can lead out of the module

        // each record has a slot in the module table where it is declared
        // a dotted qualitype finally leading to a record has a slot in the module table where
        //     it is declared and stores a reference to the record class table if
        //     - it belongs to a NamedType
        //     - it belongs to a pointer which belongs to a named type

        switch( t->getTag() )
        {
        case Thing::T_QualiType:
            {
                QualiType* q = cast<QualiType*>(t);
                if( q->d_slotAllocated )
                    emitGetTableByIndex( to, mod, q->d_slot, loc );
                else if( q->isDotted() )
                {
                    // switch to other module
                    Expression* qe = q->d_quali.data();
                    Q_ASSERT( qe->getTag() == Thing::T_IdentSel );
                    IdentSel* sel = cast<IdentSel*>(qe);
                    Q_ASSERT( sel->d_sub && sel->d_sub->getTag() == Thing::T_IdentLeaf );
                    IdentLeaf* leaf = cast<IdentLeaf*>( sel->d_sub.data() );
                    Q_ASSERT( !leaf->d_ident.isNull() &&
                              leaf->d_ident->getTag() == Thing::T_Import );
                    Q_ASSERT( qe->d_type.data() == sel->d_type.data() );
                    Module* leafMod = leaf->d_ident->getModule();
                    Q_ASSERT( leafMod != 0 );
                    // Q_ASSERT( leaf->d_ident->d_slotValid );
                    if( !leaf->d_ident->d_slotValid )
                        qCritical() << "no slot allocated at" << thisMod->getName() << leaf->d_loc.d_row << leaf->d_loc.d_col;

                    if( leafMod == thisMod )
                    {
                        // leaf->d_ident->d_slot is the local slot where the reference to the imported module table is stored
                        if( ctx.back().scope == thisMod )
                            fetchClassImp2( to, leaf->d_ident->d_slot, q->d_quali->d_type.data(), false, loc );
                        else
                        {
                            // provide this function can be called from procedure level too
                            bc.UGET(to, ctx.back().resolveUpval(leaf->d_ident->d_slot,leaf->d_name), loc.packed() );
                            fetchClassImp2( to, to, q->d_quali->d_type.data(), false, loc );
                        }
                    }else
                    {
                        // there is no continuous path, but leaf should at least be in a imported module, as e.g.
                        // in T7Module NEW(y.f), where type(f) is B.U in the context of module A, which again
                        // is an import of T7Module; import B is also a known name in A (pointing to module table of B)

                        Import* imp = thisMod->findImport(leafMod);
                        if( imp != 0 )
                        {
                            fetchModule(to,loc);
                            // requires that imports are both stored in local slots and in the module table
                            bc.TGETi(to, to, imp->d_slot, loc.packed() );
                            // to is now module A, same effect as GGET im->getName()
                        }else
                        {
                            qWarning() << "unexpected case that" << leafMod->getName() << "is not found in imports of" << thisMod->getName();
                            bc.GGET(to, leafMod->getName(), loc.packed() ); // universal, but expensive
                        }
                        bc.TGETi(to, to, leaf->d_ident->d_slot, loc.packed() ); // to is now module B
                        fetchClassImp2( to, to, q->d_quali->d_type.data(), false, loc );
                    }
                }else
                    // name in same module
                    fetchClassImp2( to, mod, q->d_quali->d_type.data(), false, loc );
            }
            break;
        case Thing::T_Pointer:
            {
                Pointer* p = cast<Pointer*>(t);
                fetchClassImp2( to, mod, p->d_to.data(), false, loc );
            }
            break;
        case Thing::T_Record:
            {
#if 0
                // not necessary since the hand crafted Lua implementations of definition files are compatible
                if( !t->d_slotAllocated )
                {
                    Q_ASSERT( t->d_slotValid );

                    Named* n = t->findDecl();
                    Module* m = 0;
                    if( n )
                        m = n->getModule();
                    if( m )
                    {
                        if( !m->d_isDef )
                        {
                            QString name;
                            if( m )
                                name = m->getName();
                            else
                                name = thisMod->getName();
                            qWarning() << "slot not valid for record" << name << t->d_loc.d_row << t->d_loc.d_col;
                        }else
                        {
                            bc.GGET(to, m->getName(), loc.packed() );
                            emitGetTableByIndex(to, to, t->d_slot, loc );
                        }
                    }
                    bc.KNIL( to, 1, loc.packed() );
                }else
#else
                Q_ASSERT( t->d_slotValid );
#endif
                    emitGetTableByIndex( to, mod, t->d_slot, loc );
            }
            break;
        default:
            Q_ASSERT( false );
        }
    }

#if 0
    void fetchClassImp1( quint8 to, quint8 mod, Type* t, bool isAlias, const RowCol& loc )
    {
        // TODO isAlias should not be presupposed, but determined by d_decl which is NamedType

        switch( t->getTag() )
        {
        case Thing::T_QualiType:
            if( !isAlias )
            {
                QualiType* q = cast<QualiType*>(t);

                int m = -1;
                QByteArray name = "@mod";
                Expression* qe = q->d_quali.data();
                if( qe->getTag() == Thing::T_IdentLeaf )
                {
                    m = modSlot; // target is in this module
                }else
                {
                    // target is in another module
                    Q_ASSERT( qe->getTag() == Thing::T_IdentSel );
                    IdentSel* sel = cast<IdentSel*>(qe);
                    Q_ASSERT( sel->d_sub && sel->d_sub->getTag() == Thing::T_IdentLeaf );
                    IdentLeaf* leaf = cast<IdentLeaf*>( sel->d_sub.data() );

                    Q_ASSERT( leaf->getIdent()->getTag() == Thing::T_Import && leaf->getIdent()->d_slotValid );
                    Q_ASSERT( qe->d_type.data() == sel->d_type.data() );

                    m = leaf->getIdent()->d_slot;
                    name = leaf->d_name;
                }
                // TODO: check, doesn't look right
                if( ctx.back().scope == thisMod )
                {
                    // qDebug() << "fetchClass local" << m << name << thisMod->d_name << loc.d_row;
                    fetchClassImp1( to, m, qe->d_type.data(), true, loc );
                }else
                {
                    // qDebug() << "fetchClass upval" << m << name << thisMod->d_name << loc.d_row;
                    bc.UGET(to, ctx.back().resolveUpval(m,name), loc.packed() );
                    fetchClassImp1( to, to, qe->d_type.data(), true, loc );
                }
            }else
            {
                // if a quali points to another quali, then this one is the one with the slot
                Q_ASSERT( t->d_slotAllocated );
                emitGetTableByIndex( to, mod, t->d_slot, loc );
            }
            break;
        case Thing::T_Pointer:
            {
                Pointer* p = cast<Pointer*>(t);
                fetchClassImp1( to, mod, p->d_to.data(), false, loc );
            }
            break;
        case Thing::T_Record:
            {
                // this is a local decl
                if( !t->d_slotAllocated )
                    return; // error already reported
                emitGetTableByIndex( to, mod, t->d_slot, loc );
            }
            break;
        default:
            Q_ASSERT( false );
        }
    }
#endif

    void allocateClassTables( Record* r )
    {
        if( r->d_baseRec && !r->d_baseRec->d_slotAllocated
            && r->d_base->d_quali->getTag() == Thing::T_IdentLeaf ) // treat only local records here
            allocateClassTables( r->d_baseRec );
        if( !r->d_slotAllocated )
        {
            bc.TNEW( curClass, r->d_methCount, 2, r->d_loc.packed() );
            if( !checkValidSlot( r, r->d_loc ) )
                return;
            Q_ASSERT( r->d_slotValid );

            // store the record slot number and module reference in each class table
            const int tmp = ctx.back().buySlots(1);
            bc.KSET(tmp,r->d_slot, r->d_loc.packed() );
            bc.TSET(tmp, curClass, "@cls", r->d_loc.packed() );
            bc.TSET(modSlot, curClass, "@mod", r->d_loc.packed() );
            ctx.back().sellSlots(tmp);

            emitSetTableByIndex( curClass, modSlot, r->d_slot, r->d_loc );
            if( r->d_baseRec )
            {
                if( !checkValidSlot( r->d_baseRec, r->d_loc ) )
                    return;
#if 0 // TEST
                if( !r->d_base->d_quali->getIdent()->getModule()->d_isDef && !r->d_baseRec->d_slotAllocated )
                {
                    QualiType::ModItem baseq = r->d_base->getQuali();
                    QString what;
                    if( baseq.first )
                        what = baseq.first->d_name;
                    if( baseq.second )
                        what += "." + baseq.second->d_name;
                    qCritical() << "ERROR base record not allocated at" << thisMod->getName() <<
                                   r->d_base->d_loc.d_row << r->d_base->d_loc.d_col << what;
                }
#endif
                const int baseClass = ctx.back().buySlots(1);

                // call setmetatable
                fetchClass( baseClass, r->d_base.data(), r->d_base->d_loc );
                const int tmp = ctx.back().buySlots(3,true);
                Q_ASSERT( ctx.back().scope == thisMod );
                bc.TGETi( tmp, obxlj, 22, r->d_loc.packed() ); // setmetatable
                bc.MOV( tmp+1, curClass, r->d_loc.packed() );
                bc.MOV(tmp+2,baseClass, r->d_loc.packed() );
                bc.CALL( tmp, 0, 2, r->d_loc.packed() );
                ctx.back().sellSlots(tmp,3);

                // copy down all methods
                const int tmp2 = ctx.back().buySlots(1);
                for( int i = 0; i < r->d_baseRec->d_methCount; i++ )
                {
                    emitGetTableByIndex(tmp2,baseClass,i,r->d_loc);
                    emitSetTableByIndex(tmp2,curClass,i,r->d_loc);
                }
                ctx.back().sellSlots(tmp2);
            }
            for( int i = 0; i < r->d_methods.size(); i++ )
                r->d_methods[i]->accept(this); // generate code for curMethod and store it in curClass

#if 0
            // not necessary!
            if( !stripped )
            {
                int tmp = ctx.back().buySlots(1);
                bc.KSET(tmp, thisMod->getName(), r->d_loc.packed() );
                bc.TSET(tmp,curClass,"@mod",r->d_loc.packed() );
                bc.KSET(tmp, r->d_loc.packed(), r->d_loc.packed() );
                bc.TSET(tmp,curClass,"@loc",r->d_loc.packed() );
                ctx.back().sellSlots(tmp);
            }
#endif
            r->d_slotAllocated = true;
        }
    }

    static void allocateLocals( Procedure* me )
    {
        int slot = 0;
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Parameter && !n->d_slotValid )
                n->setSlot(slot++);
        }
#ifdef _HAVE_OUTER_LOCAL_ACCESS
        if( me->d_upvalIntermediate || me->d_upvalSink )
            slot++; // reserve one slot for the frame of the outer proc passed as last param
#endif
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
            qCritical() << "ERROR invalid slot in" << thisMod->getName() << loc.d_row << loc.d_col;
            return false;
        }else
            return true;
    }

    void emitIf( IfLoop* me)
    {
        me->d_if[0]->accept(this); // IF
        if( slotStack.isEmpty() )
            return; // error already reported
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
            if( slotStack.isEmpty() )
                return; // error already reported
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
        // substitute by primitive statements
        Ref<IfLoop> loop = new IfLoop();
        loop->d_op = IfLoop::LOOP;
        loop->d_loc = me->d_loc;

        Ref<IfLoop> conds = new IfLoop();
        conds->d_op = IfLoop::IF;
        conds->d_loc = me->d_loc;

        conds->d_if = me->d_if;
        conds->d_then = me->d_then;

        Q_ASSERT( me->d_else.isEmpty() );
        Ref<Exit> ex = new Exit();
        ex->d_loc = me->d_loc;
        conds->d_else << ex.data();

        loop->d_then << ( StatSeq() << conds.data() );

        loop->accept(this); // now render
    }

    void emitRepeat(IfLoop* me)
    {
        // could be substituted by primitive LOOP and IF statement
        bc.LOOP( ctx.back().pool.d_frameSize, 0, me->d_loc.packed() ); // repeat
        const quint32 loopStart = bc.getCurPc();

        for( int i = 0; i < me->d_then.first().size(); i++ )
            me->d_then.first()[i]->accept(this);

        me->d_if[0]->accept(this); // until condition
        if( slotStack.isEmpty() )
            return; // error already reported
        bc.IST( slotStack.back(), me->d_if[0]->d_loc.packed());
        releaseSlot();

        bc.JMP( ctx.back().pool.d_frameSize, 0, me->d_loc.packed() ); // if true jump to afterEnd
        const quint32 afterEnd = bc.getCurPc();

        bc.patch(loopStart);
        bc.JMP( ctx.back().pool.d_frameSize, loopStart - bc.getCurPc() - 2, me->d_loc.packed() ); // if false jump to loopStart

        bc.patch( afterEnd );
    }

    void emitLoop(IfLoop* me)
    {
        bc.LOOP( ctx.back().pool.d_frameSize, 0, me->d_loc.packed() ); // loop
        const quint32 loopStart = bc.getCurPc();

        for( int i = 0; i < me->d_then.first().size(); i++ )
            me->d_then.first()[i]->accept(this);

        bc.patch(loopStart);
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

        if( me->d_cases.isEmpty() )
            return;

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
            eq->d_type = new BaseType(Type::BOOLEAN);

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

        Ref<BaseType> boolean = new BaseType(Type::BOOLEAN);

        for( int i = 0; i < me->d_cases.size(); i++ )
        {
            const CaseStmt::Case& c = me->d_cases[i];

            QList< Ref<Expression> > ors;
            for( int j = 0; j < c.d_labels.size(); j++ )
            {
                Expression* l = c.d_labels[j].data();
                // TODO: avoid redundant evaluations using temp vars
                bool done = false;
                if( l->getTag() == Thing::T_BinExpr )
                {
                    BinExpr* bi = cast<BinExpr*>( l );
                    if( bi->d_op == BinExpr::Range )
                    {
                        Ref<BinExpr> _and = new BinExpr();
                        _and->d_op = BinExpr::AND;
                        _and->d_loc = l->d_loc;
                        _and->d_type = boolean.data();

                        Ref<BinExpr> lhs = new BinExpr();
                        lhs->d_op = BinExpr::GEQ;
                        lhs->d_lhs = me->d_exp;
                        lhs->d_rhs = bi->d_lhs;
                        lhs->d_loc = l->d_loc;
                        lhs->d_type = boolean.data();

                        Ref<BinExpr> rhs = new BinExpr();
                        rhs->d_op = BinExpr::LEQ;
                        rhs->d_lhs = me->d_exp;
                        rhs->d_rhs = bi->d_rhs;
                        rhs->d_loc = l->d_loc;
                        rhs->d_type = boolean.data();

                        _and->d_lhs = lhs.data();
                        _and->d_rhs = rhs.data();

                        ors << _and.data();
                        done = true;
                    }
                }
                if( !done )
                {
                    Ref<BinExpr> eq = new BinExpr();
                    eq->d_op = BinExpr::EQ;
                    eq->d_lhs = me->d_exp;
                    eq->d_rhs = l;
                    eq->d_loc = l->d_loc;
                    eq->d_type = boolean.data();

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
                bi->d_type = boolean.data();
                for( int i = 2; i < ors.size(); i++ )
                {
                    Ref<BinExpr> tmp = new BinExpr();
                    tmp->d_op = BinExpr::OR;
                    tmp->d_lhs = bi.data();
                    tmp->d_type = boolean.data();
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

    void accessor(Expression* desig, Accessor& acc )
    {
        const int unop = desig->getUnOp();
        const int tag = desig->getTag();
        if( unop == UnExpr::SEL )
        {
            Q_ASSERT( desig->getTag() == Thing::T_IdentSel );
            IdentSel* sel = cast<IdentSel*>(desig);
            Named* id = sel->getIdent();
            // NOTE: id could come from another module, but d_sub evaluates to the proper table anyway
            if( !checkValidSlot(id,desig->d_loc) )
                return;
            Q_ASSERT( id && id->d_slotValid );
            sel->d_sub->accept(this);
            Q_ASSERT( !slotStack.isEmpty() ); // ... table
            acc.slot = slotStack.back();
            acc.disposeSlot = 1;
            slotStack.pop_back();
            acc.index = id->d_slot;
            acc.kind = Accessor::TableIdx;
        }else if( unop == UnExpr::IDX )
        {
            Q_ASSERT( desig->getTag() == Thing::T_ArgExpr );
            ArgExpr* args = cast<ArgExpr*>( desig );
            Q_ASSERT( args->d_args.size() == 1 );
            args->d_sub->accept(this);
            args->d_args.first()->accept(this);
            if( slotStack.size() < 2 )
                return; // already reported
            Q_ASSERT( slotStack.size() >= 2 ); // ... table, index
            acc.slot = slotStack[slotStack.size()-2];
            acc.disposeSlot = 1;
            acc.index = slotStack[slotStack.size()-1];
            acc.disposeIndex = 1;
            slotStack.pop_back();
            slotStack.pop_back();
            acc.kind = Accessor::TableIdxSlot;
        }else if( unop == UnExpr::CAST )
        {
            Q_ASSERT( desig->getTag() == Thing::T_ArgExpr );
            ArgExpr* args = cast<ArgExpr*>( desig );
            accessor( args->d_sub.data(), acc );
        }else if( unop == UnExpr::DEREF )
        {
            Q_ASSERT( desig->getTag() == Thing::T_UnExpr );
            UnExpr* ue = cast<UnExpr*>( desig );
            accessor( ue->d_sub.data(), acc );
        // TODO: what about CALL
        }else if( tag == Thing::T_IdentLeaf )
        {
            Named* n = desig->getIdent();
            if( !checkValidSlot( n, desig->d_loc ) )
                return;
            switch( n->getTag() )
            {
            case Thing::T_Variable:
                {
                    Q_ASSERT( ctx.back().scope != thisMod );
                    const int tmp = ctx.back().buySlots(1);
                    fetchModule(tmp,desig->d_loc); // it's identleaf, it's local
                    acc.slot = tmp;
                    acc.disposeSlot = 1;
                    acc.index = n->d_slot;
                    acc.kind = Accessor::TableIdx;
                }
                break;
            case Thing::T_Parameter:
            case Thing::T_LocalVar:
                if( n->d_scope == ctx.back().scope )
                {
                    acc.slot = n->d_slot;
                    acc.kind = Accessor::Slot;
                }else
                    qWarning() << "non-local access not yet implemented";
                break;
            }
        }else if( tag == Thing::T_Literal )
        {
            Q_ASSERT( cast<Literal*>(desig)->d_vtype == Literal::Nil );
            // this happens in BB when calling the Win32 API
            acc.kind = Accessor::Invalid;
        }else if( tag == Thing::T_ArgExpr )
        {
            ArgExpr* ae = cast<ArgExpr*>(desig);
            Q_ASSERT( ae->d_sub && ae->d_sub->getIdent() && ae->d_sub->getIdent()->getTag() == Thing::T_BuiltIn &&
                      cast<BuiltIn*>(ae->d_sub->getIdent())->d_func == BuiltIn::SYS_VAL);
            acc.kind = Accessor::Invalid;
        }else
        {
            qDebug() << "ERR" << desig->getUnOp() << desig->getTag() << thisMod->getName() << desig->d_loc.d_row << desig->d_loc.d_col;
            Q_ASSERT( false );
        }
    }

#ifdef _INSERT_DGBTRACE
    void emitTraceEnd(const RowCol& loc)
    {
        quint8 tmp = ctx.back().buySlots(2, true);
        fetchObxlibMember(tmp,45,loc); // ObxFfi_DBGTRACE
        bc.KSET(tmp+1, QByteArray("< ") + thisMod->getName() + QByteArray(":") + ctx.back().scope->getName(),loc.packed());
        bc.CALL(tmp,0,1,loc.packed());
        ctx.back().sellSlots(tmp,2);
    }
#endif

    void emitReturn(ProcType* pt, Expression* what, const RowCol& loc)
    {
        Q_ASSERT( pt );

        QVector<bool> accs(pt->d_formals.size());
        int varcount = 0;
        for( int i = 0; i < pt->d_formals.size(); i++)
        {
            accs[i] = trueVarParam( pt->d_formals[i].data() );
            if( accs[i] )
                varcount++;
        }
        if( varcount )
        {
            const int tmp = ctx.back().buySlots( 1 + varcount );
            if( what )
            {
                what->accept(this);
                if( slotStack.isEmpty() )
                    return; // error already reported
                bc.MOV(tmp,slotStack.back(),loc.packed());
                releaseSlot();
            }else if( !pt->d_return.isNull() )
            {
                // a function with no body
                emitDefault(tmp,pt->d_return.data(),loc);
            }
            int pos = 1;
            for( int i = 0; i < pt->d_formals.size(); i++ )
            {
                if( accs[i] )
                {
                    Q_ASSERT( pt->d_formals[i]->d_slotValid );
#ifdef _HAVE_OUTER_LOCAL_ACCESS
                    if( ctx.back().scope->d_upvalSource )
                        emitGetTableByIndex(tmp+pos,0,pt->d_formals[i]->d_slot,loc);
                    else
#endif
                        bc.MOV(tmp+pos, pt->d_formals[i]->d_slot, loc.packed() );
                    pos++;
                }
            }
#ifdef _INSERT_DGBTRACE
            emitTraceEnd(loc);
#endif
            bc.RET(tmp,1+varcount,loc.packed());
            ctx.back().sellSlots( tmp, 1 + varcount );
        }else
        {
            if( what )
            {
                what->accept(this);
                if( slotStack.isEmpty() )
                    return; // error already reported

                Type* lhsT = derefed(pt->d_return.data());
                Type* rhsT = derefed(what->d_type.data());
                if( lhsT && rhsT && lhsT->isChar() && !rhsT->isChar() )
                {
                    // len-1-string to char
                    Q_ASSERT( rhsT->isString() || rhsT->isStructured() );
                    bc.TGETi( slotStack.back(), slotStack.back(), 0, loc.packed() );
                }
                if( lhsT && lhsT->isStructured() )
                {
                    // TODO: do we really need to copy structures returned by value?
                    const int tmp = ctx.back().buySlots(1);
                    emitCopy( true, pt->d_return.data(), tmp, what->d_type.data(), slotStack.back(), loc );
                    bc.MOV(slotStack.back(),tmp, loc.packed());
                    ctx.back().sellSlots(tmp);
                }

#ifdef _INSERT_DGBTRACE
                emitTraceEnd(loc);
#endif
                bc.RET(slotStack.back(),1,loc.packed());
                releaseSlot();
            }else if( !pt->d_return.isNull() )
            {
                // a function with no body
                const int tmp = ctx.back().buySlots(1);
                emitDefault(tmp,pt->d_return.data(),loc);
#ifdef _INSERT_DGBTRACE
                emitTraceEnd(loc);
#endif
                bc.RET(tmp,1,loc.packed()); // procedures always return one slot
                ctx.back().sellSlots(tmp);
            }else
            {
#ifdef _INSERT_DGBTRACE
                emitTraceEnd(loc);
#endif
                bc.RET(loc.packed()); // if we combine this with the !pt->d_return.isNull() part and
                                        // always return an arg then OberonSystem crashes randomly
            }
        }
    }

    void emitCopy( bool create, Type* lhsT, quint8 lhs, Type* rhsT, quint8 rhs, const RowCol& loc )
    {
        Q_ASSERT( lhsT != 0 && rhsT != 0 );
        Type* lhsTd = derefed(lhsT);
        Q_ASSERT( lhsTd && lhsTd->isStructured() );

        Type* rhsTd = derefed(rhsT);
        Q_ASSERT( rhsT );

        switch( lhsTd->getTag() )
        {
        case Thing::T_Record:
            {
                //qDebug() << "copy record used in" << thisMod->getName() << loc.d_row << loc.d_col;
                Record* r = cast<Record*>(lhsTd);
                if( create )
                    emitCreateRecord(lhs,rhsT,loc); // use original type, not derefed
                    // use the rhsT here because in call the actual parameter has local type information (lhsT might
                    // refer to non-local types which are not even imported instead)
                QList<Field*> ff = r->getOrderedFields();
                foreach( Field* f, ff )
                {
                    Type* ft = derefed(f->d_type.data());
                    Q_ASSERT( ft );

                    if( ft->isStructured() )
                    {
                        const int lhs2 = ctx.back().buySlots(1);
                        const int rhs2 = ctx.back().buySlots(1);
                        if( !create ) // if lhs table is newly created it is empty; no need to fetch.
                            emitGetTableByIndex(lhs2,lhs,f->d_slot,loc);
                        emitGetTableByIndex(rhs2,rhs,f->d_slot,loc);
                        emitCopy(create, f->d_type.data(),lhs2, f->d_type.data(), rhs2, loc );
                        if( create )
                            emitSetTableByIndex(lhs2,lhs,f->d_slot,loc);
                        ctx.back().sellSlots(lhs2);
                        ctx.back().sellSlots(rhs2);
                    }else
                    {
                        Q_ASSERT( f->d_slotValid );
                        const int tmp = ctx.back().buySlots(1);
                        emitGetTableByIndex(tmp,rhs,f->d_slot,loc);
                        emitSetTableByIndex(tmp,lhs,f->d_slot,loc);
                        ctx.back().sellSlots(tmp);
                    }
                }
            }
            break;
        case Thing::T_Array:
            {
                //qDebug() << "copy array used in" << thisMod->getName() << loc.d_row << loc.d_col;
                // cases: normal array, char array (strlen, wide/normal), literal rhs (char or string)
                Array* lhsA = cast<Array*>(lhsTd);
                Type* laT = derefed(lhsA->d_type.data());
                Q_ASSERT( laT );
                Array* rhsA = rhsTd->getTag() == Thing::T_Array ? cast<Array*>(rhsTd) : 0;
                Type* raT = rhsA ? derefed(rhsA->d_type.data()) : 0;

                int lenSlot = -1;
                quint32 constLen = 0;
                if( rhsA && !rhsA->d_lenExpr.isNull() )
                    constLen = rhsA->d_len;
                else if( !lhsA->d_lenExpr.isNull() )
                    constLen = lhsA->d_len;
                else if( create || !laT->isChar() )
                {
                    lenSlot = ctx.back().buySlots(1);
                    int baseType = 0;
                    if( raT )
                        baseType = raT->getBaseType();
                    else if( rhsTd->getBaseType() == Type::STRING )
                        baseType = Type::CHAR;
                    else if( rhsTd->getBaseType() == Type::WSTRING )
                        baseType = Type::WCHAR;
                    emitCalcDynLen(lenSlot,rhs, baseType,loc);
                }
                // can lhsT be an open array? yes
                // can rhsT be an open array? yes, if it's a param of the proc the code is in

                if( create )
                    emitCreateArray(lhs,laT, constLen, lenSlot >= 0 ? lenSlot : 0, loc );

                if( laT->isChar() ) // copy to array of char/wchar
                {
                    if( rhsTd->isChar() )
                    {
                        bc.TSETi(rhs,lhs,0,loc.packed());
                        const int tmp = ctx.back().buySlots(1);
                        bc.KSET(tmp,0,loc.packed());
                        bc.TSETi(tmp,lhs,1,loc.packed());
                        ctx.back().sellSlots(tmp);
                    }else
                    {
                        const int tmp = ctx.back().buySlots(3,true);
                        fetchObxlibMember(tmp,27,loc); // module.strcpy
                        bc.MOV(tmp+1,lhs,loc.packed());
                        bc.MOV(tmp+2,rhs,loc.packed());
                        bc.CALL(tmp,0,2,loc.packed());
                        ctx.back().sellSlots(tmp,3);
                    }
                }else if( laT->isStructured())
                {
                    const quint8 base = ctx.back().buySlots(4);
                    const quint8 lhs2 = ctx.back().buySlots(1);
                    const quint8 rhs2 = ctx.back().buySlots(1);
                    bc.KSET(base,0,loc.packed());
                    if( constLen > 0 )
                        bc.KSET(base+1,constLen-1,loc.packed());
                    else
                    {
                        bc.SUB(lenSlot, lenSlot, QVariant(1), loc.packed() );
                        bc.MOV(base+1, lenSlot, loc.packed() );
                        ctx.back().sellSlots(lenSlot);
                        lenSlot = -1;
                    }
                    bc.KSET(base+2,1,loc.packed());
                    bc.FORI(base,0,loc.packed());
                    const quint32 pc = bc.getCurPc();

                    bc.TGET(rhs2,rhs,base+3,loc.packed());
                    if( !create )
                        bc.TGET(lhs2,lhs,base+3,loc.packed());
                    emitCopy(create,lhsA->d_type.data(),lhs2, rhsA->d_type.data(), rhs2, loc );
                    if( create )
                        bc.TSET(lhs2,lhs,base+3,loc.packed());

                    bc.FORL(base, pc - bc.getCurPc() - 1,loc.packed());
                    bc.patch(pc);

                    ctx.back().sellSlots(lhs2);
                    ctx.back().sellSlots(rhs2);
                    ctx.back().sellSlots(base,4);
                }else
                {
                    const quint8 base = ctx.back().buySlots(4);
                    const quint8 tmp = ctx.back().buySlots(1);
                    bc.KSET(base,0,loc.packed());
                    if( constLen > 0 )
                        bc.KSET(base+1,constLen-1,loc.packed());
                    else
                    {
                        bc.SUB(lenSlot, lenSlot, QVariant(1), loc.packed() );
                        bc.MOV(base+1, lenSlot, loc.packed() );
                        ctx.back().sellSlots(lenSlot);
                        lenSlot = -1;
                    }
                    bc.KSET(base+2,1,loc.packed());
                    bc.FORI(base,0,loc.packed());
                    const quint32 pc = bc.getCurPc();

                    bc.TGET(tmp,rhs,base+3,loc.packed());
                    bc.TSET(tmp,lhs,base+3,loc.packed());

                    bc.FORL(base, pc - bc.getCurPc() - 1,loc.packed());
                    bc.patch(pc);

                    ctx.back().sellSlots(tmp);
                    ctx.back().sellSlots(base,4);
                }
                if( lenSlot >= 0 )
                    ctx.back().sellSlots(lenSlot);
            }
            break;
        default:
            Q_ASSERT( false );
        }
    }

    static inline Type* derefed( Type* t )
    {
        if( t )
            return t->derefed();
        else
            return 0;
    }

    void emitAssig( Expression* lhs, quint8 rhs, const RowCol& loc )
    {
        Accessor acc;
        accessor( lhs, acc );
        emitSlotToAcc(acc,rhs, loc);
        releaseAcc(acc);
    }

    void emitAccToSlot( quint8 slot, const Accessor& acc, const RowCol& loc )
    {
        switch( acc.kind )
        {
        case Accessor::Slot:
#ifdef _HAVE_OUTER_LOCAL_ACCESS
            if( ctx.back().scope->d_upvalSource )
                emitGetTableByIndex( slot, 0, acc.slot, loc );
            else
#endif
                bc.MOV(slot, acc.slot, loc.packed() );
            break;
        case Accessor::TableIdx:
            emitGetTableByIndex(slot, acc.slot, acc.index, loc );
            break;
        case Accessor::TableIdxSlot:
            bc.TGET(slot, acc.slot, acc.index, loc.packed() );
            break;
        default:
            Q_ASSERT( false );
        }
    }

    void emitDefault( quint8 to, Type* t, const RowCol& loc )
    {
        emitInitializer(to,t,false,true,loc);
    }

    void emitSlotToAcc( const Accessor& acc, quint8 slot, const RowCol& loc )
    {
        switch( acc.kind )
        {
        case Accessor::Slot:
#ifdef _HAVE_OUTER_LOCAL_ACCESS
            if( ctx.back().scope->d_upvalSource )
                emitSetTableByIndex( slot, 0, acc.slot, loc );
            else
#endif
                bc.MOV( acc.slot, slot, loc.packed() );
            break;
        case Accessor::TableIdx:
            emitSetTableByIndex( slot, acc.slot, acc.index, loc );
            break;
        case Accessor::TableIdxSlot:
            bc.TSET( slot, acc.slot, acc.index, loc.packed() );
            break;
        default:
            break;
        }
    }

    void releaseAcc( const Accessor& acc )
    {
        if( acc.disposeSlot )
            ctx.back().sellSlots(acc.slot);
        if( acc.disposeIndex )
            ctx.back().sellSlots(acc.index);
    }

};

#if 0 // no longer useful
bool LjbcGen::translate(Model* mdl, const QString& outdir, const QString& mod, bool strip, Ob::Errors* err)
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
        {
            LjbcGen::allocateDef(m, 0, err );
        }else
        {
            QFile out( dir.absoluteFilePath( m->getName() + ".lua" ) );
            if( !out.open(QIODevice::WriteOnly) )
            {
                errs++;
                if( err )
                    err->error(Errors::Generator,m->getName(), 0,0,QString("cannot open file '%1' for writing").
                           arg(out.fileName()) );
            }else
            {
                if( m->d_isDef )
                    allocateDef(m,&out,err);
                else if( !translate(m,&out, strip, err) )
                    errs++;
            }
        }
    }
    return errs == 0;
}
#endif

bool LjbcGen::allocateSlots(Module* me)
{
    Q_ASSERT( me );
    quint32 slotNr = me->d_isDef ? 0 : 2;
    foreach( Import* imp, me->d_imports )
    {
        if(imp->d_mod->d_synthetic )
            continue; // ignore SYSTEM
        imp->setSlot( slotNr++ );
    }

    ObxLjbcGenImp::Collector pc;
    me->accept(&pc);
    foreach( Procedure* p, pc.allProcs )
    {
        Q_ASSERT( p->d_receiver.isNull() );
        // bound procs are stored in the class objects instead

        p->setSlot( slotNr++ );
    }
    // var and proc slots don't overlap
    foreach( const Ref<Named>& n, me->d_order )
    {
        if( n->getTag() == Thing::T_Variable )
        {
            n->setSlot(slotNr++);
            // vars are not stored in slots; their slot nr can grow > 255
        }
    }

    foreach( Record* r, pc.allRecords )
        ObxLjbcGenImp::allocateClasses(r,slotNr,me, false);

    foreach( QualiType* q, pc.allAliasses )
    {
        if( !q->d_slotValid )
            q->setSlot(slotNr++);
    }
    me->d_slotValid = true;
    return true;
}

bool LjbcGen::translate(Module* m, QIODevice* out, bool strip, Ob::Errors* errs)
{
    Q_ASSERT( m != 0 && out != 0 );

    if( m->d_hasErrors || !m->d_isValidated ) //  not validated can happen if imports cannot be resolved
        return false;

    if( !m->d_slotValid )
    {
        qWarning() << "generating code for module where slots are not allocated" << m->getName();
        allocateSlots(m);
    }

    if( m->d_isDef )
        return true;

    ObxLjbcGenImp imp;
    imp.bc.setUseRowColFormat(true);
    imp.bc.setStripped(strip);
    imp.thisMod = m;
    imp.stripped = strip;
    imp.system = Lexer::getSymbol("SYSTEM");

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
    // TODO replace this by LjbcGen::allocateSlots

    ObxLjbcGenImp::Collector pc;
    m->accept(&pc);

    quint32 slotNr = 0;
    QTextStream ts(out);

    foreach( const Ref<Named>& n, m->d_order )
    {
        if( n->getTag() == Thing::T_Variable )
        {
            n->setSlot(slotNr++);
            if( out )
                ts << "-- module[" << n->d_slot << "] = " << n->getName() << endl;
        }
    }

    foreach( Procedure* p, pc.allProcs )
    {
        Q_ASSERT( p->d_receiver.isNull() );
        // bound procs are stored in the class objects instead
        p->setSlot( slotNr++ );
        if( out )
            ts << "module[" << p->d_slot << "] = module." << p->getName() << endl;
    }

    foreach( Record* r, pc.allRecords )
    {
        ObxLjbcGenImp::allocateClasses(r,slotNr,m,true);
        if( out )
        {
            Named* name = r->d_decl;
            if( name == 0 && r->d_binding )
                name = r->d_binding->d_decl;
            if( name )
                ts << "-- module[" << r->d_slot << "] = record " << name->getName() << endl;
            else
                qWarning() << "no name for record slot" << r->d_slot;
        }
    }

    foreach( QualiType* q, pc.allAliasses )
    {
        q->setSlot(slotNr++);
        if( out )
        {
            ts << "-- module[" << q->d_slot << "] = alias ";
            Named* name = q->d_decl;
            Q_ASSERT( name );
            ts << name->getName() << endl;
        }
    }

    return true;
}


