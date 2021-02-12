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
    // The magic numbers are in sync with obxlj.lua.

    struct NoMoreFreeSlots {};

    struct ProcsCollector : public AstVisitor
    {
        QList<Procedure*> allProcs;

        void visit( Module* me )
        {
            foreach( const Ref<Named>& n, me->d_order )
            {
                if( n->getTag() == Thing::T_Procedure )
                {
                    allProcs.append(cast<Procedure*>(n.data()));
                    n->accept(this);
                }
            }
        }

        void visit( Procedure* me)
        {
            foreach( const Ref<Named>& n, me->d_order )
            {
                if( n->getTag() == Thing::T_Procedure )
                {
                    allProcs.append(cast<Procedure*>(n.data()));
                    n->accept(this); // all procs go to toplevel
                }
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
        bool returnFound;
        Ctx(Scope* s = 0):returnFound(false),scope(s) { }

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
    quint8 modSlot, obxlj;
    QList<quint8> slotStack;


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
            imp->d_slot = ctx.back().buySlots(1); // procs are stored as indices in the module table as well as in the slots
            imp->d_slotValid = true;
            names[imp->d_slot] = imp->d_name;
        }

        ProcsCollector pc;
        me->accept(&pc);
        foreach( Procedure* p, pc.allProcs )
        {
            p->d_slot = ctx.back().buySlots(1); // procs are stored as indices in the module table as well as in the slots
            p->d_slotValid = true;
            names[p->d_slot] = p->d_name; // more than one proc/slot can have the same name
        }

        quint32 slot = ctx.back().pool.d_frameSize + 1;
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Variable )
            {
                n->d_slot = slot++;
                n->d_slotValid = true; // vars are not stored in slots; their slot nr can grow > 255
            }
        }

        Q_ASSERT( modSlot == 0 );
        bc.TNEW( modSlot, ctx.back().pool.d_frameSize, 0, me->d_loc.packed() );

        emitImport( "obxlj", obxlj, me->d_loc );

        foreach( Import* imp, me->d_imports )
            imp->accept(this);

        foreach( Procedure* p, pc.allProcs )
            p->accept(this);

        foreach( const Ref<Statement>& s, me->d_body )
            s->accept(this); // TODO: body in a sub-proc to save slots and unify access to module elements?

        bc.UCLO( modSlot, 0, me->d_end.packed() );
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
        const int id = bc.openFunction(me->getProcType()->d_formals.size(), me->d_name,me->d_loc.packed(), me->d_end.packed() );
        Q_ASSERT( id >= 0 );

        QHash<quint8,QByteArray> names;
        foreach( const Ref<Named>& n, me->d_order )
        {
            if( n->getTag() == Thing::T_Parameter )
            {
                n->d_slot = ctx.back().buySlots(1);
                n->d_slotValid = true;
                names[n->d_slot] = n->d_name;
            }
        }

        foreach( const Ref<Named>& n, me->d_order )
        {
            const int tag = n->getTag();
            if( tag == Thing::T_LocalVar )
            {
                n->d_slot = ctx.back().buySlots(1);
                n->d_slotValid = true;
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

        Q_ASSERT( me->d_slotValid );
        bc.FNEW( me->d_slot, id, me->d_end.packed() );
        bc.TSET( me->d_slot, modSlot, me->d_name, me->d_end.packed() );
    }

    void visit( Import* me)
    {
        emitImport( me->d_name, me->d_slot, me->d_aliasPos.isValid() ? me->d_aliasPos : me->d_loc );
    }

    void emitInitializer( Named* me )
    {
        // TODO: initializations where needed
    }

    void visit( Variable* me)
    {
        emitInitializer(me);
    }

    void visit( LocalVar* me)
    {
        emitInitializer(me);
    }

    // TODO:
    void visit( Call* ) {}
    void visit( Return* ) {}
    void visit( Assign* ) {}
    void visit( IfLoop* ) {}
    void visit( ForLoop* ) {}
    void visit( CaseStmt* ) {}
    void visit( Exit* ) {}

    void visit( Literal* me)
    {
        const int res = ctx.back().buySlots(1);
        switch( me->d_vtype )
        {
        case Literal::Integer:
        case Literal::Real:
        case Literal::Boolean:
        case Literal::Nil:
            bc.KSET(res, me->d_val, me->d_loc.packed() );
            break;
        case Literal::String:
            {
                int tmp = ctx.back().buySlots(3,true);
                fetchObxlib(tmp,me->d_loc);
                bc.KSET(tmp+1,me->d_strLen+1,me->d_loc.packed());
                bc.KSET(tmp+2,me->d_val,me->d_loc.packed());
                if( me->d_wide )
                {
                    bc.TGET(tmp,tmp,2,me->d_loc.packed()); // createWcharArray
                    bc.CALL(tmp,1,2,me->d_loc.packed());
                }else
                {
                    bc.TGET(tmp,tmp,1,me->d_loc.packed()); // createCharArray
                    bc.CALL(tmp,1,2,me->d_loc.packed());
                }
                bc.MOV(res,tmp,me->d_loc.packed());
                ctx.back().sellSlots(tmp,3);
            }
            break;
        case Literal::Bytes:
            {
                int tmp = ctx.back().buySlots(3,true);
                fetchObxlibMember(tmp,8,me->d_loc); // createByteArray
                bc.KSET(tmp+1,me->d_val.toByteArray().size()+1,me->d_loc.packed());
                bc.KSET(tmp+2,me->d_val,me->d_loc.packed());
                bc.CALL(tmp,1,2,me->d_loc.packed());
                bc.MOV(res,tmp,me->d_loc.packed());
                ctx.back().sellSlots(tmp,3);
            }
            break;
        case Literal::Char:
#if 1
            bc.KSET(res,me->d_val,me->d_loc.packed());
#else
            {
                // directly converting a char to a string
                int tmp = ctx.back().buySlots(2,true);
                fetchObxlibMember(tmp,16,me->d_loc); // module.charToString
                bc.KSET(tmp+1,me->d_val,me->d_loc.packed());
                bc.CALL(tmp,1,1,me->d_loc.packed());
                bc.MOV(res,tmp,me->d_loc.packed());
                ctx.back().sellSlots(tmp,2);
            }
#endif
            break;
        case Literal::Set:
            {
                Literal::SET s = me->d_val.value<Literal::SET>();
                bc.KSET(res,quint32(s.to_ulong()),me->d_loc.packed());
            }
            break;
        }
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
            Q_ASSERT( false );
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
            // TODO
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

    // TODO:
    void visit( IdentLeaf* ) {}
    void visit( IdentSel* ) {}
    void visit( ArgExpr* ) {}

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
        bc.UGET(to, ctx.back().resolveUpval(obxlj,"obxlj"), loc.packed() );
    }

    void fetchObxlibMember( quint8 to, int what, const RowCol& loc )
    {
        fetchObxlib(to,loc);
        bc.TGET(to,to,what,loc.packed());
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
        bc.JMP(ctx.back().pool.d_frameSize,loc.packed());
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
            if( !translate(m,&out,err) )
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


