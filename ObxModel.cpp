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

#include "ObxModel.h"
#include "ObErrors.h"
#include "ObFileCache.h"
#include "ObxValidator.h"
#include <QBuffer>
#include <QFile>
#include <QDir>
#include <QFileInfo>
#include <QtDebug>
#include <qhash.h>
#include <math.h>
using namespace Obx;
using namespace Ob;

static uint qHash( const QByteArrayList& ba, uint seed )
{
    return qHash(ba.last(),seed);
}

struct Model::CrossReferencer : public AstVisitor
{
    Module* d_mod;
    Model* d_mdl;
    QList<Scope*> stack;
    QSet<Type*> visited;

    CrossReferencer(Model* mdl, Module* mod)
    {
        d_mod = mod;
        d_mdl = mdl;
        d_mod->accept(this);
    }

    void visit( Module* me )
    {
        stack.push_back(me);

        me->d_helper << new IdentLeaf( me, me->d_loc,me, 0);
        d_mdl->d_xref[me].append( me->d_helper.back().data() );

        foreach( const Ref<Named>& n, me->d_order )
        {
            if( !n.isNull() )
                n->accept(this);
        }

        foreach( const Ref<Statement>& s, me->d_body )
        {
            if( !s.isNull() )
                s->accept(this);
        }
        stack.pop_back();
    }

    void visit( Import* me )
    {
        Q_ASSERT( !d_mod->d_helper.isEmpty() && d_mod->d_helper.first()->getIdent()->getTag() == Thing::T_Module );

        if( !me->d_mod.isNull() )
        {
            d_mdl->d_xref[me->d_mod.data()].append( d_mod->d_helper.first().data() );

            IdentLeaf* e2 = new IdentLeaf( me->d_mod.data(), me->d_loc, d_mod, 0 );
            d_mod->d_helper.append( e2 );
            d_mdl->d_xref[me->d_mod.data()].append( e2 );
        }

        IdentLeaf* e1 = new IdentLeaf( me, me->d_aliasPos.isValid() ? me->d_aliasPos : me->d_loc, d_mod, 0 );
        d_mod->d_helper.append( e1 );
        d_mdl->d_xref[me].append( e1 );
    }

    void visit( Procedure* me )
    {
        stack.push_back(me);

        me->d_helper << new IdentLeaf( me, me->d_loc,d_mod, 0);
        d_mdl->d_xref[me].append( me->d_helper.back().data() );

        //if( me->d_receiver ) // receiver Param is part of d_order
        //    me->d_receiver->accept(this);

        ProcType* pt = me->getProcType();
        // ProcType formals are part of d_order
        if( pt->d_return )
            pt->d_return->accept(this);

        foreach( const Ref<Named>& n, me->d_order )
        {
            if( !n.isNull() )
                n->accept(this);
        }
        // d_metaParams were already processed in me->d_order

        foreach( const Ref<Statement>& s, me->d_body )
        {
            if( !s.isNull() )
                s->accept(this);
        }
        stack.pop_back();
    }

    void visit( NamedType* me )
    {
        Scope* s = stack.back();
        s->d_helper << new IdentLeaf(me, me->d_loc, d_mod );
        d_mdl->d_xref[me].append( s->d_helper.back().data() );

        if( me->d_type )
            me->d_type->accept(this);
    }

    void visit( Pointer* me)
    {
        if( me->d_flag )
            me->d_flag->accept(this);
        if( me->d_to )
            me->d_to->accept(this);
    }

    void visit( Array* me )
    {
        if( me->d_flag )
            me->d_flag->accept(this);
        if( me->d_type )
            me->d_type->accept(this);
        if( me->d_lenExpr )
            me->d_lenExpr->accept(this);
    }

    void visit( Record* me )
    {
        if( me->d_base )
            me->d_base->accept(this);
        if( me->d_flag )
            me->d_flag->accept(this);
        foreach( const Ref<Field>& f, me->d_fields )
            f->accept(this);
        // d_methods is handled from Procedure
    }

    void visit( ProcType* me )
    {
        if( me->d_return )
            me->d_return->accept(this);
        foreach( const Ref<Parameter>& p, me->d_formals )
            p->accept(this);
    }

    void visit( QualiType* me )
    {
        if( me->d_quali )
            me->d_quali->accept(this);
        foreach( const Ref<Thing>& t, me->d_metaActuals )
            t->accept(this);
    }
    void visitVar( Named* me )
    {
        Scope* s = stack.back();
        s->d_helper << new IdentLeaf( me, me->d_loc, d_mod );
        d_mdl->d_xref[me].append( s->d_helper.back().data() );
        // we need the visited set here because the same type can be assigned to more than one Named
        if( me->d_type && !visited.contains(me->d_type.data()) )
        {
            visited.insert(me->d_type.data());
            me->d_type->accept(this);
        }
    }

    void visit( Field* me )
    {
        visitVar(me);
    }

    void visit( Variable* me )
    {
        visitVar(me);
    }

    void visit( LocalVar* me )
    {
        visitVar(me);
    }

    void visit( Parameter* me )
    {
        visitVar(me);
    }

    void visit( GenericName* me )
    {
        visitVar(me);
    }

    void visit( Const* me )
    {
        Scope* s = stack.back();
        s->d_helper << new IdentLeaf( me, me->d_loc, d_mod );
        d_mdl->d_xref[me].append( s->d_helper.back().data() );
        if( me->d_constExpr )
            me->d_constExpr->accept(this);
    }

    void visit( Enumeration* me )
    {
        foreach( const Ref<Const>& c, me->d_items )
            c->accept(this);
    }

    void visit( Call* me )
    {
        if( me->d_what )
            me->d_what->accept(this);
    }

    void visit( Return* me )
    {
        if( me->d_what )
            me->d_what->accept(this);
    }

    void visit( Assign* me )
    {
        if( me->d_lhs )
            me->d_lhs->accept(this);
        if( me->d_rhs )
            me->d_rhs->accept(this);
    }

    void visit( IfLoop* me )
    {
        foreach( const Ref<Expression>& e, me->d_if )
        {
            if( e )
                e->accept(this);
        }
        foreach( const StatSeq& ss, me->d_then )
        {
            foreach( const Ref<Statement>& s, ss )
            {
                if( !s.isNull() )
                    s->accept(this);
            }
        }
        foreach( const Ref<Statement>& s, me->d_else )
        {
            if( !s.isNull() )
                s->accept(this);
        }
    }

    void visit( ForLoop* me )
    {
        if( me->d_id )
            me->d_id->accept(this);
        if( me->d_from )
            me->d_from->accept(this);
        if( me->d_to )
            me->d_to->accept(this);
        if( me->d_by )
            me->d_by->accept(this);
        foreach( const Ref<Statement>& s, me->d_do )
        {
            if( !s.isNull() )
                s->accept(this);
        }
    }

    void visit( CaseStmt* me )
    {
        if( me->d_exp )
            me->d_exp->accept(this);
        foreach( const CaseStmt::Case& c, me->d_cases )
        {
            foreach( const Ref<Expression>& e, c.d_labels )
            {
                if( e )
                    e->accept(this);
            }
            foreach( const Ref<Statement>& s, c.d_block )
            {
                if( !s.isNull() )
                    s->accept(this);
            }
        }
        foreach( const Ref<Statement>& s, me->d_else )
        {
            if( !s.isNull() )
                s->accept(this);
        }
    }

    void visit( SetExpr* me )
    {
        foreach( const Ref<Expression>& e, me->d_parts )
        {
            if( e )
                e->accept(this);
        }
    }

    void visit( IdentLeaf* me )
    {
        if( !me->d_ident.isNull() )
            d_mdl->d_xref[me->d_ident.data()].append( me );
    }

    void visit( UnExpr* me )
    {
        if( me->d_sub )
            me->d_sub->accept(this);
    }

    void visit( IdentSel* me )
    {
        if( me->d_sub )
            me->d_sub->accept(this);
        if( !me->d_ident.isNull() )
            d_mdl->d_xref[me->d_ident.data()].append( me );
    }

    void visit( ArgExpr* me )
    {
        if( me->d_sub )
            me->d_sub->accept(this);
        foreach( const Ref<Expression>& e, me->d_args )
        {
            if( e )
                e->accept(this);
        }
    }

    void visit( BinExpr* me )
    {
        if( me->d_lhs )
            me->d_lhs->accept(this);
        if( me->d_rhs )
            me->d_rhs->accept(this);
    }

    // NOP
    void visit( BaseType* ) {}
    void visit( Exit* ) {}
    void visit( BuiltIn* ) {}
    void visit( Literal* ) {}

};

Model::Model(QObject *parent) : QObject(parent),d_fillXref(false)
{
    d_errs = new Errors(this);
    d_fc = new FileCache(this);

    d_globals = new Scope();
    d_globalsLower = new Scope();
    d_boolType = new BaseType(BaseType::BOOLEAN);
    d_charType = new BaseType(BaseType::CHAR);
    d_byteType = new BaseType(BaseType::BYTE);
    d_intType = new BaseType(BaseType::INTEGER);
    d_shortType = new BaseType(BaseType::SHORTINT);
    d_longType = new BaseType(BaseType::LONGINT);
    d_realType = new BaseType(BaseType::REAL);
    d_doubleType = new BaseType(BaseType::LONGREAL);
    d_setType = new BaseType(BaseType::SET);
    d_stringType = new BaseType(BaseType::STRING);
    d_nilType = new BaseType(BaseType::NIL);
    d_anyType = new BaseType(BaseType::ANY);
    d_anyNum = new BaseType(BaseType::ANYNUM);
    d_anyRec = new Record();

    fillGlobals();
}

void Model::clear()
{
    d_errs->clear();

    d_depOrder.clear();
    unbindFromGlobal();
    d_modules.clear();
    d_others.clear();
    d_xref.clear();
    d_sloc = 0;
}

bool Model::parseFiles(const FileGroups& files)
{
    if( files.isEmpty() )
    {
        qDebug() << "nothing to parse";
        return false;
    }
    clear();

    const quint32 before = d_errs->getErrCount();
    foreach( const FileGroup& fg, files )
    {
        const QString old = QDir::currentPath();
        if( !fg.d_root.isEmpty() )
            QDir::setCurrent(fg.d_root);
        foreach( const QString& file, fg.d_files )
        {
            if( !fg.d_root.isEmpty() && !QFileInfo(file).isRelative() )
            {
                error( file, tr("file not relative to file group '%1'").arg(fg.d_root));
                continue;
            }else if( fg.d_root.isEmpty() && QFileInfo(file).isRelative() )
            {
                error( file, tr("with empty root file '%1' must have an absolute path").arg(file));
                continue;
            }

            const QString path = QDir::current().absoluteFilePath(file);
            qDebug() << "parsing" << path;
            Ref<Module> m = parseFile(path);
            if( m.isNull() )
                error( path, tr("cannot open file") );
            else
            {
                m->d_fullName = FileGroup::toFullName(file);
#if 0
                // ETH Oberon V3 and V4 violate this rule
                if( m->d_fullName.isEmpty() || m->d_fullName.last() != m->d_name )
                    error( path, tr("file name must correspond to module name '%1'").arg(m->d_name.constData() ) );
                else
#else
                m->d_fullName.last() = m->d_name;
#endif
                if( d_modules.contains( m->d_fullName ) )
                    error( path,tr("full name of module is not unique in file groups: %1").
                           arg(m->d_fullName.join('/').constData()));
                else
                {
                    if( m->d_isExt )
                        m->d_scope = d_globalsLower.data();
                    else
                        m->d_scope = d_globals.data();
                    d_modules.insert( m->d_fullName, m );
                }
            }
        }
        QDir::setCurrent(old);
    }

    if( before != d_errs->getErrCount() )
        return false; // stop on parsing errors

    resolveImports();
    if( !findProcessingOrder() )
        return false;

    Validator::BaseTypes bt;
    bt.d_boolType = d_boolType.data();
    bt.d_charType = d_charType.data();
    bt.d_byteType = d_byteType.data();
    bt.d_intType = d_intType.data();
    bt.d_shortType = d_shortType.data();
    bt.d_longType = d_longType.data();
    bt.d_realType = d_realType.data();
    bt.d_doubleType = d_doubleType.data();
    bt.d_setType = d_setType.data();
    bt.d_stringType = d_stringType.data();
    bt.d_nilType = d_nilType.data();
    bt.d_anyType = d_anyType.data();
    bt.d_anyNum = d_anyNum.data();
    bt.d_anyRec = d_anyRec.data();

    foreach( Module* m, d_depOrder )
    {
        if( m == d_systemModule.data())
            continue;
        qDebug() << "analyzing" << m->d_file;

        Validator::check(m, bt, d_errs );
        if( d_fillXref )
            CrossReferencer(this,m);
    }

    return true;
}

Ref<Module> Model::parseFile(const QString& path)
{
    bool found;
    FileCache::Entry content = d_fc->getFile(path, &found );
    if( found )
    {
        QBuffer buf;
        buf.setData( content.d_code );
        buf.open(QIODevice::ReadOnly);
        return parseFile( &buf, path );
    }else
    {
        QFile file(path);
        if( !file.open(QIODevice::ReadOnly) )
            return 0;
        return parseFile( &file, path );
    }
}

Ref<Module> Model::parseFile(QIODevice* in, const QString& path)
{
    Ob::Lexer lex;
    lex.setErrors(d_errs);
    lex.setCache(d_fc);
    lex.setIgnoreComments(true);
    lex.setPackComments(true);
    lex.setSensExt(true);
    lex.setStream( in, path );
    Obx::Parser p(&lex,d_errs);
    Ref<Module> res = p.parse();
    d_sloc += lex.getSloc();
    // qDebug() << path << "with" << lex.getSloc() << "LOC";
    return res;
}

void Model::unbindFromGlobal()
{
    if( d_globals.isNull() )
        return;
    foreach( Module* m, d_depOrder )
        m->d_scope = 0; // if Modules continue to live they don't have dangling ptr
}

void Model::fillGlobals()
{
    Ref<NamedType> t;

    // Built-in types
    t = new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_boolType->d_type]),d_boolType.data() );
    d_globals->add( t.data() );

    t = new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_charType->d_type]),d_charType.data() );
    d_globals->add( t.data() );

    Ref<NamedType> byteType = new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_byteType->d_type]),d_byteType.data() );
    d_globals->add( byteType.data() );

    t = new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_intType->d_type]),d_intType.data() );
    d_globals->add( t.data() );

    t = new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_realType->d_type]),d_realType.data() );
    d_globals->add( t.data() );

    t = new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_setType->d_type]),d_setType.data() );
    d_globals->add( t.data() );

    t = new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_stringType->d_type]),d_stringType.data() );
    d_globals->add( t.data() );

    t = new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_nilType->d_type]),d_nilType.data() );
    d_globals->add( t.data() );

    t = new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_longType->d_type]),d_longType.data() );
    d_globals->add( t.data() );

    t = new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_shortType->d_type]),d_shortType.data() );
    d_globals->add( t.data() );

    t = new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_doubleType->d_type]),d_doubleType.data() );
    d_globals->add( t.data() );

    Ref<BuiltIn> bi;

    // MODULE System
    Ref<Module> sys = new Module();
    d_systemModule = sys;
    sys->d_name = Lexer::getSymbol("SYSTEM");
    sys->d_isDef = true;
    sys->d_synthetic = true;

    sys->add( byteType.data() );

    bi = new BuiltIn(BuiltIn::SYS_ADR, new ProcType( Type::List() << d_anyType.data(), d_intType.data() ) );
    sys->add( bi.data());

    bi = new BuiltIn(BuiltIn::SYS_BIT, new ProcType( Type::List() << d_intType.data() << d_intType.data(), d_boolType.data() ) );
    sys->add( bi.data());

    bi = new BuiltIn(BuiltIn::SYS_GET, new ProcType( Type::List() << d_intType.data() << d_anyType.data(),
                                              ProcType::Vars() << false << true   ) );
    sys->add( bi.data());

    bi = new BuiltIn(BuiltIn::SYS_H, new ProcType( Type::List() << d_intType.data(), d_intType.data() ) );
    sys->add( bi.data());

    bi = new BuiltIn(BuiltIn::SYS_LDREG, new ProcType( Type::List() << d_intType.data() << d_intType.data() ) );
    sys->add( bi.data());

    bi = new BuiltIn(BuiltIn::SYS_PUT, new ProcType( Type::List() << d_intType.data() << d_anyType.data() ) );
    sys->add( bi.data());

    bi = new BuiltIn(BuiltIn::SYS_REG, new ProcType( Type::List() << d_intType.data(), d_intType.data() ) );
    sys->add( bi.data());

    bi = new BuiltIn(BuiltIn::SYS_VAL, new ProcType( Type::List() << d_anyType.data() << d_anyType.data(), d_anyType.data() ) );
    sys->add( bi.data());

    bi = new BuiltIn(BuiltIn::SYS_COPY, new ProcType( Type::List() << d_intType.data() << d_intType.data() << d_intType.data(),
                                                  ProcType::Vars() << false << false << false) ); // all three INTEGER representing address
    sys->add( bi.data());

    // Oberon-2
    sys->add( new BuiltIn(BuiltIn::SYS_MOVE, new ProcType( Type::List() << d_longType.data() << d_longType.data()
                                                           << d_anyNum.data() ) ) );
    sys->add( new BuiltIn(BuiltIn::SYS_NEW, new ProcType( Type::List() << d_anyType.data() << d_anyNum.data() ) ) );
    sys->add( new BuiltIn(BuiltIn::SYS_ROT, new ProcType( Type::List() << d_anyType.data() << d_anyNum.data(),
                                                          d_anyType.data() ) ) );
    sys->add( new BuiltIn(BuiltIn::SYS_LSH, new ProcType( Type::List() << d_anyType.data() << d_anyNum.data(),
                                                          d_anyType.data() ) ) );


    // Built-in procedures
    bi = new BuiltIn(BuiltIn::ABS, new ProcType( Type::List() << d_anyNum.data(), d_anyNum.data() ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::ODD, new ProcType( Type::List() << d_intType.data(), d_boolType.data() ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::LEN, new ProcType( Type::List() << d_anyType.data(), d_intType.data() ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::LSL, new ProcType( Type::List() << d_intType.data() << d_intType.data(), d_intType.data() ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::ASR, new ProcType( Type::List() << d_intType.data() << d_intType.data(), d_intType.data() ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::ROR, new ProcType( Type::List() << d_anyType.data() // integer type or SET in Oberon System
                                                 << d_intType.data(), d_intType.data() ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::FLOOR, new ProcType( Type::List() << d_realType.data(), d_intType.data() ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::FLT, new ProcType( Type::List() << d_intType.data(), d_realType.data() ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::ORD, new ProcType( Type::List() << d_anyType.data(), d_intType.data() ) ); // char, bool, set
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::CHR, new ProcType( Type::List() << d_intType.data(), d_charType.data() ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::INC, new ProcType( Type::List() << d_intType.data() << d_intType.data(),
                         ProcType::Vars() << true << false ) ); // optional second param
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::DEC, new ProcType( Type::List() << d_intType.data() << d_intType.data(),
                         ProcType::Vars() << true << false ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::INCL, new ProcType( Type::List() << d_setType.data() << d_intType.data(),
                                                  ProcType::Vars() << true << false ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::EXCL, new ProcType( Type::List() << d_setType.data() << d_intType.data(),
                                                  ProcType::Vars() << true << false ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::NEW, new ProcType( Type::List() << d_anyType.data(), ProcType::Vars() << true ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::ASSERT, new ProcType( Type::List() << d_boolType.data() ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::PACK, new ProcType( Type::List() << d_realType.data() << d_intType.data(),
                                                  ProcType::Vars() << true << false ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::UNPK, new ProcType( Type::List() << d_realType.data() << d_intType.data(),
                                                  ProcType::Vars() << true << true ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::LED, new ProcType( Type::List() << d_intType.data() ) );
    d_globals->add( bi.data());

    // Oberon IDE
    bi = new BuiltIn(BuiltIn::TRAP, new ProcType());
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::TRAPIF, new ProcType( Type::List() << d_boolType.data() ) );
    d_globals->add( bi.data());

    // lboasso oberonc
    bi = new BuiltIn(BuiltIn::WriteInt, new ProcType( Type::List() << d_intType.data() ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::WriteReal, new ProcType( Type::List() << d_realType.data() ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::WriteChar, new ProcType( Type::List() << d_charType.data() ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::WriteLn );
    d_globals->add( bi.data());

    // Oberon-2
    d_globals->add( new BuiltIn(BuiltIn::MAX, new ProcType( Type::List() << d_anyType.data(), d_anyType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::MIN, new ProcType( Type::List() << d_anyType.data(), d_anyType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::CAP, new ProcType( Type::List() << d_charType.data(), d_charType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::LONG, new ProcType( Type::List() << d_anyNum.data(), d_anyNum.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::SHORT, new ProcType( Type::List() << d_anyNum.data(), d_anyNum.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::HALT, new ProcType( Type::List() << d_anyNum.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::COPY, new ProcType( Type::List() << d_anyType.data() << d_anyType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::ASH, new ProcType( Type::List() << d_anyNum.data() << d_anyNum.data(), d_intType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::SIZE, new ProcType( Type::List() << d_anyType.data(), d_intType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::ENTIER, new ProcType( Type::List() << d_anyNum.data(), d_longType.data() ) ) );

    // Oberon+
    d_globals->add( new BuiltIn(BuiltIn::VAL, new ProcType( Type::List() << d_anyType.data() << d_anyType.data(), d_anyType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::STRLEN, new ProcType( Type::List() << d_anyType.data(), d_intType.data() ) ) );

    // Blackbox
#ifdef OBX_BBOX
    d_globals->add( new Const( Lexer::getSymbol("INF"),
                               new Literal( Literal::Real, RowCol(), INFINITY, d_realType.data() ) ) );
    Ref<NamedType> anyrec = new NamedType(Lexer::getSymbol("ANYREC"),d_anyRec.data() );
    d_globals->add( anyrec.data() );
    Ref<Pointer> anyptr = new Pointer();
    anyptr->d_to = anyrec->d_type.data();
    d_globals->add( new NamedType(Lexer::getSymbol("ANYPTR"), anyptr.data() ) );

    d_globals->add( new BuiltIn(BuiltIn::BITS, new ProcType( Type::List() << d_intType.data(), d_setType.data() ) ) );

    sys->add( new BuiltIn(BuiltIn::SYS_TYP, new ProcType( Type::List() << d_anyRec.data(), d_intType.data() ) ) );
    sys->add( new BuiltIn(BuiltIn::SYS_GETREG, new ProcType( Type::List() << d_intType.data() << d_anyType.data(),
                                              ProcType::Vars() << false << true   ) ) );
    sys->add( new BuiltIn(BuiltIn::SYS_PUTREG, new ProcType( Type::List() << d_intType.data() << d_anyType.data() ) ) );
    sys->add( new NamedType(Lexer::getSymbol("PTR"), anyptr.data() ) );

    d_globals->add( new Const( Lexer::getSymbol("untagged"), 0 ) );
    d_globals->add( new Const( Lexer::getSymbol("union"), 0 ) );
    d_globals->add( new Const( Lexer::getSymbol("align8"), 0 ) );
    d_globals->add( new Const( Lexer::getSymbol("noalign"), 0 ) );
    d_globals->add( new Const( Lexer::getSymbol("align2"), 0 ) );

    // TODO THISRECORD
#endif


    // lower case
    d_globalsLower->d_names = d_globals->d_names;
    Scope::Names::const_iterator i;
    for( i = d_globals->d_names.begin(); i != d_globals->d_names.end(); ++i )
        d_globalsLower->d_names.insert( Lexer::getSymbol( QByteArray(i.key()).toLower() ), i.value() );
}

bool Model::resolveImports()
{
    bool hasErrors = false;
    Modules::const_iterator i;
    for( i = d_modules.begin(); i != d_modules.end(); ++i )
    {
        Module* m = i.value().data();
        foreach( Import* i, m->d_imports )
        {
            i->d_mod = d_modules.value(i->d_path);
            if( i->d_mod.isNull() )
            {
                if( i->d_path.size() == 1 && i->d_path.last() == d_systemModule->d_name )
                    i->d_mod = d_systemModule;
                else
                {
                    i->d_mod = d_others.value( i->d_path );
                    if( i->d_mod.isNull() )
                    {
                        i->d_mod = parseFile( i->d_path.join('/') );
                        if( i->d_mod.isNull() )
                        {
                            error( Loc( i->d_loc, m->d_file ), tr("cannot find module '%1'").
                                   arg( i->d_path.join('/').constData() ) );
                            hasErrors = true;
                        }else
                        {
                            if( i->d_mod->d_isExt )
                                i->d_mod->d_scope = d_globalsLower.data();
                            else
                                i->d_mod->d_scope = d_globals.data();
                            d_others.insert(i->d_path, i->d_mod );
                        }
                    }
                }
            }
        }
    }
    return hasErrors;
}

static bool DFS( Module* m, QSet<Module*>& mods, QList<Module*>& trace )
{
    trace.append(m);
    mods.remove(m);
    foreach( Import* i, m->d_imports )
    {
        Module* mm = i->d_mod.data();
        const int pos = trace.indexOf(mm);
        if( pos != -1 )
        {
            trace = trace.mid(pos);
            return true;
        }
        if( !mods.contains(mm) )
        {
            continue;
        }
        else if( DFS( mm, mods, trace ) )
            return true;
    }
    return false;
}

bool Model::findProcessingOrder()
{
    // Mods nicht const da sonst COW eine neue Kopie macht die SynTree löscht

    QSet<Module*> mods, all;
    Modules::const_iterator i;
    for( i = d_modules.begin(); i != d_modules.end(); ++i )
        mods.insert(i.value().data());
    for( i = d_others.begin(); i != d_others.end(); ++i )
        mods.insert(i.value().data());
    mods.insert(d_systemModule.data());
    all = mods;

    QSet<Module*> used;
    foreach( Module* m, mods )
    {
        if( m == d_systemModule.data() )
            continue;

        // Find all leafs
        if( m->d_imports.isEmpty() )
        {
            d_depOrder.append(m);
            used.insert(m);
        }
#if 0
        else if( m->d_isDef )
        {
            // definitions must be leafs and may not reference modules, but can reference each other
            defs.insert(m);
            d_depOrder.append(m);
            used.insert(m);
            foreach( Import* imp, m->d_imports )
            {
                if( imp->d_mod.isNull() && !imp->d_mod->d_isDef )
                    error( m->d_file, tr("definition '%1' is referencing module '%1'")
                           .arg(m->d_name.constData()).arg( imp->d_mod->d_name.constData() ) );
            }
        }
#endif
    }
    mods -= used;

    while( !mods.isEmpty() )
    {
        foreach( Module* m, mods )
        {
            bool allUsed = true;
            foreach( Import* imp, m->d_imports )
            {
                if( !imp->d_mod.isNull() && !used.contains(imp->d_mod.data()) )
                {
                    allUsed = false;
                    break;
                }
            }
            if( allUsed )
            {
                used.insert(m);
                d_depOrder.append(m);
            }
        }
        const int count = mods.size();
        mods -= used;
        if( count == mods.size() )
            break;
    }
#if 0
    if( !mods.isEmpty() )
    {
        foreach( Module* m, mods )
        {
            error( m->d_file, tr("module '%1' has circular import dependencies").arg(m->d_name.constData()) );
        }
        return false;
    }
#else
    while( !mods.isEmpty() )
    {
        Module* m = *mods.begin();
        QList<Module*> trace;
        if( DFS( m, mods, trace ) )
        {
            Q_ASSERT( !trace.isEmpty() );
            QStringList names;
            foreach( Module* mm, trace )
                names << mm->d_name;
            error( trace.first()->d_file, tr("there are circular import dependencies among: %1")
                   .arg( names.join(" ") ) );
            return false;
        }
    }

#endif

#if 0
    foreach( Module* m, d_depOrder )
        qDebug() << m->d_name;
#endif
    return true;
}

bool Model::error(const QString& file, const QString& msg)
{
    d_errs->error( Errors::Semantics, file, 1,1, msg );
    return false;
}

bool Model::error(const Loc& loc, const QString& msg)
{
    d_errs->error( Errors::Semantics, loc, msg );
    return false;
}

QByteArrayList Model::FileGroup::toFullName(const QString& filePath)
{
    QFileInfo info(filePath);
    if( !info.isRelative() )
    {
        //qCritical() << "filename not relative" << relativeFileName;
        return QByteArrayList() << info.completeBaseName().toUtf8();
    }
    QByteArrayList res;
    const QStringList segments = info.path().split( '/' );
    foreach( const QString& seg, segments )
    {
        if( !seg.startsWith('.') )
            res.append( seg.toUtf8() );
    }
    res.append( info.completeBaseName().toUtf8() );
    return res;
}

QStringList Model::FileGroup::absolutePaths() const
{
    QStringList res;
    QDir root(d_root);
    foreach( const QString& f, d_files )
    {
        if( QFileInfo(f).isRelative() )
            res << root.absoluteFilePath(f);
        else
            res << f;
    }
    return res;
}

Model::FileGroup Model::FileGroup::fromPaths(const QString& root, const QStringList& files)
{
    QFileInfo ri(root);

    QDir rd;
    if( ri.isFile() )
        rd = ri.canonicalPath();
    else
        rd = ri.canonicalFilePath();

    FileGroup res;
    res.d_root = rd.canonicalPath();

    foreach( const QString& f, files )
    {
        QFileInfo fi(f);
        if( fi.isRelative() )
            res.d_files << f;
        else
            res.d_files << rd.relativeFilePath(f);
    }
    return res;
}
