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

#include "ObxPelibGen.h"
#include "ObRowCol.h"
#include "ObErrors.h"
#include "ObxModel.h"
#include <PeLib/SimpleApi.h>
#include <QDir>
#include <QtDebug>
using namespace Obx;
using namespace Ob;

struct ObxPelibGenImp : public AstVisitor
{
    struct Collector : public AstVisitor
    {
        QList<Procedure*> allProcs; // only static procs, i.e. not bound to a record
        QList<Record*> allRecords;
        QList<QualiType*> allAliasses;

        void collectRecord(Type* t, bool isTypedef )
        {
            const int tag = t->getTag();
            if( tag == Thing::T_Array )
            {
                collectRecord(cast<Array*>(t)->d_type.data(), false);
                return;
            }
            if( isTypedef && tag == Thing::T_QualiType )
            {
                // this is B in "type A = B"
                // QualiTypes pointing to a record or pointer to record get their own slot in the module table
                if( t->toRecord() )
                    allAliasses.append( cast<QualiType*>(t) );
                return;
            }
            if( tag == Thing::T_Pointer )
                t = cast<Pointer*>(t)->d_to.data(); // no deref, intentionally
            if( t && t->getTag() == Thing::T_Record )
            {
                Record* r = cast<Record*>(t);
                allRecords.append( r );
                foreach( const Ref<Field>& f, r->d_fields )
                    collectRecord(f->d_type.data(), false);
            }
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
                    collectRecord(n->d_type.data(), true);
                else if( tag == Thing::T_Variable )
                    collectRecord(n->d_type.data(), false);
            }
        }

        void visit( Procedure* me)
        {
            ProcType* pt = me->getProcType();
            foreach( const Ref<Parameter>& p, pt->d_formals )
                collectRecord(p->d_type.data(), false);
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
                    collectRecord(n->d_type.data(), true);
                else if( tag == Thing::T_LocalVar )
                    collectRecord(n->d_type.data(), false);
            }
        }
    };

    DotNetPELib::SimpleApi out;
    quint32 anonRecord;
    Ob::Errors* err;
    Module* curMod;
    QSet<Module*> imports;
    bool ownsErr;

    ObxPelibGenImp():err(0),ownsErr(false),anonRecord(0),curMod(0) {}

    // TODO: arbitrary order of declarations might collide with dependencies in PeLib
    void visit( Module* me)
    {
        curMod = me;

        // A Module is a top-level class with all record types as nested classes; otherwise we could not merge more
        // than one OBX module in an assembly.
        for( int i = 0; i < me->d_fullName.size() - 1; i++ )
            out.beginNamespace(me->d_fullName[i]);
        out.beginClass( me->d_name, true ); // this is the module

        Collector coll;
        coll.visit(me);

        // go through all record types and generate .net classes without members first (doing class augmentation later)
        for( int i = 0; i < coll.allRecords.size(); i++ )
        {
            // for this version each OBX record inherits from .net object; later versions will sort out the ones suitable as values
            out.beginClass( recordName(coll.allRecords[i] ) );
            out.endClass();
        }

        // go through all record types and generate fields and method headers
        for( int i = 0; i < coll.allRecords.size(); i++ )
        {
            Record* r = coll.allRecords[i];
            out.openClass( recordName(r) );
            if( r->d_baseRec )
                out.setSuperClass( recordName( r->d_baseRec ) );
            for( int j = 0; j < r->d_fields.size(); j++ )
                r->d_fields[j]->accept(this);
            for( int j = 0; j < r->d_methods.size(); j++ )
            {
                Procedure* p = r->d_methods[j].data();
                while( p->d_super )
                    p = p->d_super; // use the original signature since dotnet doesn't directly support covariance
                out.beginMethod( p->d_name );
                ProcType* pt = p->getProcType();
                if( pt->d_return )
                    out.setReturnType( typeQuali(pt->d_return.data()) );
                for( int k = 0; k < pt->d_formals.size(); k++ )
                    out.addArgument( typeQuali( pt->d_formals[k]->d_type.data() ) +
                                     ( pt->d_formals[k]->isVarParam() ? "&" : "" ),
                                     pt->d_formals[k]->d_name );
                out.endMethod();
            }
            out.endClass();
        }

        // go through all static procedures and generate headers
        for( int i = 0; i < coll.allProcs.size(); i++ )
        {
            Procedure* p = coll.allProcs[i];
            out.beginMethod( p->d_name );
            ProcType* pt = p->getProcType();
            if( pt->d_return )
                out.setReturnType( typeQuali(pt->d_return.data()) );
            if( p->d_loc.d_row == 989 )
                qDebug() << "hit";
            for( int k = 0; k < pt->d_formals.size(); k++ )
            {
                QByteArray type = typeQuali( pt->d_formals[k]->d_type.data() );
                if( type.isEmpty() )
                    type = typeQuali( pt->d_formals[k]->d_type.data() ); // TEST
                out.addArgument( type + ( pt->d_formals[k]->isVarParam() ? "&" : "" ),
                                 pt->d_formals[k]->d_name );
            }
            out.endMethod();
        }

        out.endClass(); // this ends the module
        for( int i = 0; i < me->d_fullName.size() - 1; i++ )
            out.endNamespace();
    }

    void visit( Field* me)
    {

    }


    void visit( BaseType* ) {}
    void visit( Pointer* ) {}
    void visit( Array* ) {}
    void visit( Record* ) {}
    void visit( ProcType* ) {}
    void visit( QualiType* ) {}
    void visit( Variable* ) {}
    void visit( LocalVar* ) {}
    void visit( Parameter* ) {}
    void visit( NamedType* ) {}
    void visit( Const* ) {}
    void visit( Import* ) {}
    void visit( Procedure* ) {}
    void visit( BuiltIn* ) {}
    void visit( Call* ) {}
    void visit( Return* ) {}
    void visit( Assign* ) {}
    void visit( IfLoop* ) {}
    void visit( ForLoop* ) {}
    void visit( CaseStmt* ) {}
    void visit( Literal* ) {}
    void visit( SetExpr* ) {}
    void visit( IdentLeaf* ) {}
    void visit( UnExpr* ) {}
    void visit( IdentSel* ) {}
    void visit( ArgExpr* ) {}
    void visit( BinExpr* ) {}
    void visit( Enumeration* ) {}
    void visit( GenericName* ) {}
    void visit( Exit* ) {}

    ///// Utilities

    QByteArray recordName( Record* r, bool asRef = false )
    {
        Named* n = r->findDecl(false);
        if( n && n->getTag() == Thing::T_NamedType )
        {
            QByteArray name = n->d_name;
            Scope* s = n->d_scope;
            while( s && s->getTag() == Thing::T_Procedure )
            {
                name = s->d_name + "$" + name;
                s = s->d_scope;
            }
            Q_ASSERT( s->getTag() == Thing::T_Module );
            if( asRef )
            {
                if( s != curMod  )
                    imports.insert( cast<Module*>(s) );
                name = s->getName() + "." + name;
            }
            return name;
        }else
        {
            if( r->d_slot == 0 )
                r->d_slot = ++anonRecord;
            return "$" + QByteArray::number(r->d_slot);
        }
    }

    QByteArray typeQuali( Type* t )
    {
        t = derefed(t);
        switch( t->getTag() )
        {
        case Thing::T_Record:
            return recordName(cast<Record*>(t), true );
        case Thing::T_Pointer:
            return typeQuali( cast<Pointer*>(t)->d_to.data() );
        case Thing::T_Array:
            {
                Array* a = cast<Array*>(t);
                const QList<Array*> dims = a->getDims();
                return typeQuali(dims.last()->d_type.data()) + "[" + QByteArray( dims.size() - 1, ',' ) + "]";
            }
        case Thing::T_BaseType:
            switch( t->d_baseType )
            {
            case Type::BOOLEAN:
                return "bool";
            case Type::CHAR:
                return "uint8";
            case Type::WCHAR:
                return "char";
            case Type::BYTE:
                return "uint8";
            case Type::SHORTINT:
                return "int16";
            case Type::INTEGER:
                return "int32";
            case Type::LONGINT:
                return "int64";
            case Type::REAL:
                return "float32";
            case Type::LONGREAL:
                return "float64";
            case Type::SET:
                return "uint32";
            default:
                Q_ASSERT( false );
            }
            break;
        case Thing::T_ProcType:
        case Thing::T_Enumeration:
            // TODO
            qWarning() << "typeQuali procType and Enum not implemented";
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
};

bool PelibGen::translate(Model* mdl, const QString& outdir, bool strip, Ob::Errors* err)
{
    Q_ASSERT( mdl );
    //Q_ASSERT( RowCol::COL_BIT_LEN == JitComposer::COL_BIT_LEN && RowCol::ROW_BIT_LEN == JitComposer::ROW_BIT_LEN );

    int errs = 0;
    foreach( Module* m, mdl->getDepOrder() )
    {
        if( m->d_isDef )
            ; // NOP
        else if( !translate(m, outdir, false, strip, err) ) // TODO isPrimary
            errs++;
    }
    return errs == 0;
}

bool PelibGen::translate(Module* m, const QString& outdir, bool isPrimary, bool strip, Ob::Errors* errs)
{
    Q_ASSERT( m != 0 && !outdir.isEmpty() );

    if( m->d_hasErrors )
        return false;

    if( m->d_isDef )
        return true;

    ObxPelibGenImp imp;
    /*
    imp.bc.setUseRowColFormat(true);
    imp.bc.setStripped(strip);
    imp.thisMod = m;
    imp.stripped = strip;
    */

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


    const QByteArray modName = m->getName();
    const QString fileName( QDir(outdir).absoluteFilePath( modName + ( isPrimary ? ".exe" : ".dll" ) ) );

    bool hasErrs = false;
    try
    {
        imp.out.beginModule( m->d_name );
        m->accept(&imp);

        hasErrs = ( imp.err->getErrCount() - errCount ) != 0;

        //imp.bc.setStripped(true);
        if( !hasErrs )
        {
            imp.out.writeByteCode( fileName.toUtf8() );
            qDebug() << "wrote bytecode to" << fileName;
        }

        imp.out.endModule();
#if 1
    }catch( const std::runtime_error& e )
    {
        imp.err->error(Errors::Generator, m->d_file, 1, 1, e.what() );
        hasErrs = true;
    }
#endif

    if( imp.ownsErr )
        delete imp.err;
    return hasErrs;
}
