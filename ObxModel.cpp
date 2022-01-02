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
    uint res = 0;
    for( int i = 0; i < ba.size(); i++ )
        res ^= qHash(ba[i],seed);
    return res;
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
        if(mod)
            d_mod->accept(this);
    }

    void visit( Module* me )
    {
        stack.push_back(me);

        me->d_helper << new IdentLeaf( me, me->d_loc,me, 0, DeclRole );
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

        // causes begin to highlight when module name is hit; not useful
        //me->d_helper << new IdentLeaf( me, me->d_begin ,d_mod, 0, DeclRole);
        //d_mdl->d_xref[me].append( me->d_helper.back().data() );

        stack.pop_back();
    }

    void visit( Import* me )
    {
        Q_ASSERT( !d_mod->d_helper.isEmpty() && d_mod->d_helper.first()->getIdent()->getTag() == Thing::T_Module );

        IdentLeaf* e1 = new IdentLeaf( me, me->d_aliasPos.isValid() ? me->d_aliasPos : me->d_loc, d_mod, 0, ImportRole );
        d_mod->d_helper.append( e1 );
        d_mdl->d_xref[me].append( e1 );

        if( !me->d_mod.isNull() )
        {
            IdentLeaf* e2 = new IdentLeaf( me->d_mod.data(), me->d_loc, d_mod, 0, ImportRole );
            d_mod->d_helper.append( e2 );
            d_mdl->d_xref[me->d_mod.data()].append( e2 );
        }

        foreach( const Ref<Type>& a, me->d_metaActuals )
            a->accept(this);
    }

    void visit( Procedure* me )
    {
        stack.push_back(me);

        me->d_helper << new IdentLeaf( me, me->d_loc,d_mod, 0, DeclRole);
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
        s->d_helper << new IdentLeaf(me, me->d_loc, d_mod, 0, DeclRole );
        d_mdl->d_xref[me].append( s->d_helper.back().data() );

        if( me->d_type )
            me->d_type->accept(this);
    }

    void visit( Pointer* me)
    {
#if 0
        if( me->d_flag )
            me->d_flag->accept(this);
#endif
        if( me->d_to )
            me->d_to->accept(this);
    }

    void visit( Array* me )
    {
#if 0
        if( me->d_flag )
            me->d_flag->accept(this);
#endif
        if( me->d_type )
            me->d_type->accept(this);
        if( me->d_lenExpr )
            me->d_lenExpr->accept(this);
    }

    void visit( Record* me )
    {
        if( me->d_base )
            me->d_base->accept(this);
#if 0
        if( me->d_flag )
            me->d_flag->accept(this);
#endif
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
    }
    void visitVar( Named* me, bool receiver = false )
    {
        Scope* s = stack.back();
        s->d_helper << new IdentLeaf( me, me->d_loc, d_mod, 0, receiver ? ThisRole : DeclRole );
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
        visitVar(me, me->d_receiver );
    }

    void visit( GenericName* me )
    {
        visitVar(me);
    }

    void visit( Const* me )
    {
        Scope* s = stack.back();
        s->d_helper << new IdentLeaf( me, me->d_loc, d_mod, 0, DeclRole );
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

    void visit( Literal* l)
    {
#if 0 // obsolete; tries to interpret a string a a module/command
        if( l->d_vtype == Literal::String && !l->d_wide )
        {
            QByteArray str = l->d_val.toByteArray();
            if( str.isEmpty() || !::isalpha(str[0]) || str.endsWith(".dll") )
                return;
            for( int i = 1; i < str.size(); i++ )
            {
                if( str[i] != '_' && str[i] != '.' && !::isalnum(str[i]) )
                    return;
            }
            QByteArrayList quali = str.split('.');
            Module* m = d_mdl->findModule(quali.first());
            if( m )
            {
                RowCol rc = l->d_loc;
                rc.d_col += 1;
                IdentLeaf* e1 = new IdentLeaf( m, rc, d_mod, 0, StringRole );
                d_mod->d_helper.append( e1 );
                d_mdl->d_xref[m].append( e1 );

                // qDebug() << "CallByString" << d_mod->d_file << l->d_loc.d_row << l->d_loc.d_col << str;
                Named* n = quali.size() > 1 ? m->find( Lexer::getSymbol(quali.last()) ) : 0;
                if( n )
                {
                    rc.d_col += quali.first().size() + 1;
                    IdentLeaf* e2 = new IdentLeaf( n, rc, d_mod, 0, StringRole );
                    d_mod->d_helper.append( e2 );
                    d_mdl->d_xref[n].append( e2 );
                }
            }
        }
#endif
    }


    // NOP
    void visit( BaseType* ) {}
    void visit( Exit* ) {}
    void visit( BuiltIn* ) {}

};

Model::Model(QObject *parent) : QObject(parent),d_fillXref(false)
{
    d_errs = new Errors(this);
    d_fc = new FileCache(this);

    d_globals = new Scope();
    d_globalsLower = new Scope();
    d_noType = new BaseType(BaseType::NONE);
    d_boolType = new BaseType(BaseType::BOOLEAN);
    d_charType = new BaseType(BaseType::CHAR);
    d_wcharType = new BaseType(BaseType::WCHAR);
    d_byteType = new BaseType(BaseType::BYTE);
    d_intType = new BaseType(BaseType::INTEGER);
    d_shortType = new BaseType(BaseType::SHORTINT);
    d_longType = new BaseType(BaseType::LONGINT);
    d_realType = new BaseType(BaseType::REAL);
    d_longrealType = new BaseType(BaseType::LONGREAL);
    d_setType = new BaseType(BaseType::SET);
    d_stringType = new BaseType(BaseType::STRING);
    d_wstringType = new BaseType(BaseType::WSTRING);
    d_byteArrayType = new BaseType(BaseType::BYTEARRAY);
    d_nilType = new BaseType(BaseType::NIL);
    d_anyType = new BaseType(BaseType::ANY);
    d_voidType = new BaseType(BaseType::CVOID);
    d_anyRec = new Record();
    d_cmdType = new ProcType();

    fillGlobals();
}

void Model::clear()
{
    d_errs->clear();

    d_depOrder.clear();
    unbindFromGlobal();
    d_insts.clear();
    d_modules.clear();
    d_packages.clear();
    d_others.clear();
    d_xref.clear();
    d_sloc = 0;
}

bool Model::parseFiles(const PackageList& files)
{
    if( files.isEmpty() )
    {
        qDebug() << "nothing to parse";
        return false;
    }

    clear();

    const quint32 before = d_errs->getErrCount();
    foreach( const Package& package, files )
    {
        foreach( const QString& filePath, package.d_files )
        {
            qDebug() << "parsing" << filePath;
            Ref<Module> m = parseFile(filePath);
            if( m.isNull() )
                error( filePath, tr("cannot open file") );
            else
            {
                m->d_fullName = package.d_path;
                m->d_fullName << m->d_name;

                if( d_modules.contains( m->d_fullName ) )
                    error( filePath,tr("Module name is not unique in package: %1").
                           arg(m->d_fullName.join('.').constData()));
                else
                {
                    if( m->d_isExt )
                        m->d_scope = d_globalsLower.data();
                    else
                        m->d_scope = d_globals.data();
                    d_modules.insert( m->d_fullName, m );
                    d_packages[package.d_path].append(m.data());
                }
            }
        }
    }

    resolveImports();
    if( !findProcessingOrder() )
        return false;

    if( before != d_errs->getErrCount() )
        return false; // stop on parsing errors, but only after we found the dependency order

    Validator::BaseTypes bt;
    bt.d_noType = d_noType.data();
    bt.d_boolType = d_boolType.data();
    bt.d_charType = d_charType.data();
    bt.d_byteType = d_byteType.data();
    bt.d_intType = d_intType.data();
    bt.d_shortType = d_shortType.data();
    bt.d_longType = d_longType.data();
    bt.d_realType = d_realType.data();
    bt.d_longrealType = d_longrealType.data();
    bt.d_setType = d_setType.data();
    bt.d_stringType = d_stringType.data();
    bt.d_byteArrayType = d_byteArrayType.data();
    bt.d_nilType = d_nilType.data();
    bt.d_voidType = d_voidType.data();
    bt.d_anyType = d_anyType.data();
    bt.d_anyRec = d_anyRec.data();
    bt.d_wcharType = d_wcharType.data();
    bt.d_wstringType = d_wstringType.data();

    foreach( Module* m, d_depOrder )
    {
        if( m == d_systemModule.data())
            continue;

        Q_ASSERT( m->d_metaActuals.isEmpty() );
                      // generic module instances are not validated here,
                      // but are validated in Validator::visit(Import*) for locality

        qDebug() << "analyzing" << m->getName();

        Validator::check(m, bt, d_errs, this );

        //m->dump(); // TEST
        if( d_fillXref )
        {
            CrossReferencer(this,m);
            for( int i = 0; i < m->d_imports.size(); i++ )
            {
                if( !m->d_imports[i]->d_metaActuals.isEmpty() )
                    CrossReferencer(this,m->d_imports[i]->d_mod.data());
            }
        }
    }

#if 0 // TEST
    qDebug() << "**** generic module instances:";
    ModInsts::const_iterator i;
    for( i = d_insts.begin(); i != d_insts.end(); ++i )
    {
        foreach( const Ref<Module>& inst, i.value() )
            qDebug() << inst->getName() << inst.data();
    }
    qDebug() << "**** end ";
#endif

    return true;
}

Ref<Module> Model::parseFile(const QString& filePath)
{
    bool found;
    FileCache::Entry content = d_fc->getFile(filePath, &found );
    if( found )
    {
        QBuffer buf;
        buf.setData( content.d_code );
        buf.open(QIODevice::ReadOnly);
        return parseFile( &buf, filePath );
    }else
    {
        QFile file(filePath);
        if( !file.open(QIODevice::ReadOnly) )
            return 0;
        return parseFile( &file, filePath );
    }
}

Ref<Module> Model::parseFile(QIODevice* in, const QString& filePath)
{
    Ob::Lexer lex;
    lex.setErrors(d_errs);
    lex.setCache(d_fc);
    lex.setIgnoreComments(true);
    lex.setPackComments(true);
    lex.setSensExt(true);
    lex.setStream( in, filePath );
    Obx::Parser p(&lex,d_errs);
    Ref<Module> res = p.parse();
    d_sloc += lex.getSloc();
    // qDebug() << path << "with" << lex.getSloc() << "LOC";
    return res;
}

struct TreeShaker : public AstVisitor
{
    QSet<Named*>& used;
    Module* mod;
    TreeShaker( QSet<Named*>& u, Module* m ):mod(m),used(u){}
    void visit( Pointer* me)
    {
        if( me->d_to )
            me->d_to->accept(this);
    }
    void visit( Array* me)
    {
        if( me->d_lenExpr )
            me->d_lenExpr->accept(this);
        if( me->d_type )
            me->d_type->accept(this);
    }
    void visit( Record* me)
    {
        if( me->d_base )
            me->d_base->accept(this);
        foreach( const Ref<Field>& f, me->d_fields )
            f->accept(this);
    }
    void visit( ProcType* me)
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
    }
    void rememberUsed( Named* me )
    {
        const bool alreadySeen = used.contains(me);
        if( !alreadySeen )
        {
            used.insert(me);
            me->accept(this);
        }
    }
    void visitNamed(Named* me)
    {
        rememberUsed(me);
        if( me->d_type )
            me->d_type->accept(this);
    }
    void visit( Field* me)
    {
        visitNamed(me);
    }
    void visit( Variable* me)
    {
        visitNamed(me);
    }
    void visit( LocalVar* me)
    {
        visitNamed(me);
    }
    void visit( Parameter* me)
    {
        visitNamed(me);
    }
    void visit( NamedType* me)
    {
        visitNamed(me);
    }
    void visit( Const* me)
    {
        if( me->d_constExpr )
            me->d_constExpr->accept(this);
    }
    void visit( Procedure* me)
    {
        if( me->d_receiver )
            me->d_receiver->accept(this);
        ProcType* t = me->getProcType();
        t->accept(this);
    }
    void visit( SetExpr* me)
    {
        foreach( const Ref<Expression>& e, me->d_parts )
            e->accept(this);
    }
    void visit( IdentLeaf* me )
    {
        if( !me->d_ident.isNull() && me->d_ident->getModule() == mod )
            rememberUsed(me->d_ident.data());
    }
    void visit( IdentSel* me)
    {
        if( me->d_sub )
            me->d_sub->accept(this);
        if( !me->d_ident.isNull() && me->d_ident->getModule() == mod )
            rememberUsed(me->d_ident.data());
    }
    void visit( UnExpr* me)
    {
        if( me->d_sub )
            me->d_sub->accept(this);
    }
    void visit( ArgExpr* me)
    {
        foreach( const Ref<Expression>& e, me->d_args )
            e->accept(this);
    }
    void visit( BinExpr* me)
    {
        if( me->d_lhs )
            me->d_lhs->accept(this);
        if( me->d_rhs )
            me->d_rhs->accept(this);
    }

    void visit( Literal* ) {}
    void visit( Import* ) {}
    void visit( BuiltIn* ) {}
    void visit( Module* ) {}
    void visit( Call* ) {}
    void visit( Return* ) {}
    void visit( Assign* ) {}
    void visit( IfLoop* ) {}
    void visit( ForLoop* ) {}
    void visit( CaseStmt* ) {}
    void visit( Enumeration* ) {}
    void visit( GenericName* ) {}
    void visit( Exit* ) {}
};

Ref<Module> Model::treeShaken(Module* m) const
{
    QSet<Named*> used;
    foreach( const Ref<Named>& n, m->d_order )
    {
        foreach( const Ref<Expression>& e, d_xref.value(n.data()) )
        {
            // if there is at least one e outside of m referencing n
            if( e->getModule() != m )
                used.insert(n.data());
        }
    }

#if 0
    // doesn't work because d_xref has no relation between e.g. a named pointer and its type
    bool change = false;
    do
    {
        const int before = used.size();
        foreach( const Ref<Named>& n, m->d_order )
        {
            foreach( const Ref<Expression>& e, d_xref.value(n.data()) )
            {
                if( used.contains(e->getIdent()) )
                    used.insert( n.data() );
            }
        }
        change = used.size() != before;
    }while( change );
#else
    QSet<Named*> tmp = used;
    foreach( Named* n, tmp )
    {
        TreeShaker ts(used,m);
        n->accept(&ts);
    }

#endif


    Ref<Module> nm = new Module();
    nm->d_file = m->d_file;
    nm->d_name = m->d_name;
    nm->d_isDef = m->d_isDef;
    nm->d_isExt = m->d_isExt;
    nm->d_fullName = m->d_fullName;
    nm->d_imports = m->d_imports;
    foreach( const Ref<Named>& n, m->d_order )
    {
        if( used.contains(n.data()) )
        {
            nm->d_names[n->d_name.constData()] = n.data();
            nm->d_order.append(n);
        }
    }
    return nm;
}

void Model::addPreload(const QByteArray& name, const QByteArray& source)
{
    d_fc->addFile( name, source, true );
}

static inline bool match( const MetaActuals& lhs, const MetaActuals& rhs )
{
    if( lhs.size() != rhs.size() )
        return false;
    for( int i = 0; i < lhs.size(); i++ )
    {
        if( lhs[i]->derefed() != rhs[i]->derefed() )
            return false;
    }
    return true;
}

Module* Model::instantiate(Module* generic, const MetaActuals& actuals)
{
    Q_ASSERT( generic && generic->d_metaActuals.isEmpty() && !generic->d_metaParams.isEmpty() &&
              generic->d_metaParams.size() == actuals.size() );

    ModList& insts = d_insts[generic];
    Ref<Module> inst;
    for( int i = 0; i < insts.size(); i++ )
    {
        if( match(insts[i]->d_metaActuals,actuals) )
        {
            inst = insts[i];
            break;
        }
    }
    if( inst.isNull() )
    {
        inst = parseFile( generic->d_file );
        if( inst.isNull() || inst->d_hasErrors )
            return 0; // already reported
        inst->d_metaActuals = actuals;
        inst->d_fullName = generic->d_fullName;
        inst->d_scope = generic->d_scope;
        if( resolveImport(inst.data()) )
            inst->d_hasErrors = true;
        insts.append(inst);
    }
    return inst.data();
}

QList<Module*> Model::instances(Module* generic)
{
    ModInsts::const_iterator i = d_insts.find(generic);
    QList<Module*> res;
    if( i != d_insts.end() )
    {
        foreach( const Ref<Module>& m, i.value() )
            res.append(m.data());
    }
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
    // Built-in types
    d_globals->add( new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_boolType->d_baseType]),d_boolType.data() ) );
    d_globals->add( new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_charType->d_baseType]),d_charType.data() ) );
    d_globals->add( new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_wcharType->d_baseType]),d_wcharType.data() ) );
    d_globals->add( new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_intType->d_baseType]),d_intType.data() ) );
    d_globals->add( new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_realType->d_baseType]),d_realType.data() ) );
    d_globals->add( new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_setType->d_baseType]),d_setType.data() ) );
    d_globals->add( new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_nilType->d_baseType]),d_nilType.data() ) );
    d_globals->add( new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_longType->d_baseType]),d_longType.data() ) );
    d_globals->add( new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_shortType->d_baseType]),d_shortType.data() ) );
    d_globals->add( new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_longrealType->d_baseType]),d_longrealType.data() ) );

    Ref<NamedType> byteType = new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_byteType->d_baseType]),d_byteType.data() );
    d_globals->add( byteType.data() );

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
                                                           << d_anyType.data() ) ) );
    sys->add( new BuiltIn(BuiltIn::SYS_NEW, new ProcType( Type::List() << d_anyType.data() << d_anyType.data() ) ) );
    sys->add( new BuiltIn(BuiltIn::SYS_ROT, new ProcType( Type::List() << d_anyType.data() << d_anyType.data(),
                                                          d_anyType.data() ) ) );
    sys->add( new BuiltIn(BuiltIn::SYS_LSH, new ProcType( Type::List() << d_anyType.data() << d_anyType.data(),
                                                          d_anyType.data() ) ) );


    // Built-in procedures
    bi = new BuiltIn(BuiltIn::ABS, new ProcType( Type::List() << d_anyType.data(), d_anyType.data() ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::ODD, new ProcType( Type::List() << d_intType.data(), d_boolType.data() ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::LEN, new ProcType( Type::List() << d_anyType.data(), d_intType.data() ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::LSL, new ProcType( Type::List() << d_intType.data() << d_intType.data(), d_intType.data() ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::ASR, new ProcType( Type::List() << d_intType.data() << d_intType.data(), d_intType.data() ) );
    d_globals->add( bi.data());

    bi = new BuiltIn(BuiltIn::ROR, new ProcType( Type::List() << d_intType.data() // integer type or SET in Oberon System
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
    bi = new BuiltIn(BuiltIn::TRACE, new ProcType( Type::List() << d_stringType.data() ) );
    d_globals->add( bi.data());
    bi = new BuiltIn(BuiltIn::NOP, new ProcType() );
    d_globals->add( bi.data());
    bi = new BuiltIn(BuiltIn::LDMOD, new ProcType( Type::List() << d_stringType.data(), d_boolType.data() ) );
    d_globals->add( bi.data());
    bi = new BuiltIn(BuiltIn::LDCMD, new ProcType( Type::List() << d_stringType.data() << d_stringType.data(), d_cmdType.data() ) );
    d_globals->add( bi.data());

    // Oberon-2
    d_globals->add( new BuiltIn(BuiltIn::MAX, new ProcType( Type::List() << d_anyType.data(), d_anyType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::MIN, new ProcType( Type::List() << d_anyType.data(), d_anyType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::CAP, new ProcType( Type::List() << d_charType.data(), d_charType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::LONG, new ProcType( Type::List() << d_anyType.data(), d_anyType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::SHORT, new ProcType( Type::List() << d_anyType.data(), d_anyType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::HALT, new ProcType( Type::List() << d_intType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::COPY, new ProcType( Type::List() << d_anyType.data() << d_anyType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::ASH, new ProcType( Type::List() << d_intType.data() << d_intType.data(), d_intType.data() ) ) );
    bi = new BuiltIn(BuiltIn::BYTESIZE, new ProcType( Type::List() << d_anyType.data(), d_intType.data() ) );
    d_globals->add( bi.data() );
    d_globals->d_names[Lexer::getSymbol("SIZE").constData()] = bi.data(); // backward compatibility for BB and OBN2
    d_globals->add( new BuiltIn(BuiltIn::ENTIER, new ProcType( Type::List() << d_longrealType.data(), d_longType.data() ) ) );

    // Oberon+
    d_globals->add( new BuiltIn(BuiltIn::VAL, new ProcType( Type::List() << d_anyType.data() << d_anyType.data(), d_anyType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::STRLEN, new ProcType( Type::List() << d_anyType.data(), d_intType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::WCHR, new ProcType( Type::List() << d_intType.data(), d_wcharType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::PRINTLN, new ProcType( Type::List() << d_anyType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::DEFAULT, new ProcType( Type::List() << d_anyType.data(), d_anyType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::BITAND, new ProcType( Type::List() << d_intType.data() << d_intType.data(), d_intType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::BITNOT, new ProcType( Type::List() << d_intType.data(), d_intType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::BITOR, new ProcType( Type::List() << d_intType.data() << d_intType.data(), d_intType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::BITXOR, new ProcType( Type::List() << d_intType.data() << d_intType.data(), d_intType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::BITSHL, new ProcType( Type::List() << d_intType.data() << d_intType.data(), d_intType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::BITSHR, new ProcType( Type::List() << d_intType.data() << d_intType.data(), d_intType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::BITASR, new ProcType( Type::List() << d_intType.data() << d_intType.data(), d_intType.data() ) ) );
    d_globals->add( new BuiltIn(BuiltIn::ADR, new ProcType( Type::List() << d_anyType.data(), d_anyType.data() ) ) );
    Ref<NamedType> anyrec = new NamedType(Lexer::getSymbol("ANYREC"),d_anyRec.data() );
    d_globals->add( anyrec.data() );
    d_globals->add( new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_voidType->d_baseType]),d_voidType.data() ) );
    d_globals->add( new BuiltIn(BuiltIn::BITS, new ProcType( Type::List() << d_intType.data(), d_setType.data() ) ) );

    // Blackbox
#ifdef OBX_BBOX
    d_globals->add( new Const( Lexer::getSymbol("INF"),
                               new Literal( Literal::Real, RowCol(), INFINITY, d_realType.data() ) ) );
#if 0 // no longer used; use ^ANYREC instead
    Ref<Pointer> anyptr = new Pointer();
    anyptr->d_to = anyrec->d_type.data();
    d_globals->add( new NamedType(Lexer::getSymbol("ANYPTR"), anyptr.data() ) );
#endif

    sys->add( new BuiltIn(BuiltIn::SYS_TYP, new ProcType( Type::List() << d_anyRec.data(), d_intType.data() ) ) );
    sys->add( new BuiltIn(BuiltIn::SYS_GETREG, new ProcType( Type::List() << d_intType.data() << d_anyType.data(),
                                              ProcType::Vars() << false << true   ) ) );
    sys->add( new BuiltIn(BuiltIn::SYS_PUTREG, new ProcType( Type::List() << d_intType.data() << d_anyType.data() ) ) );

#if 0 // no longer used; use *void instead
    Ref<Pointer> ptr = new Pointer();
    ptr->d_to = d_anyType.data();
    sys->add( new NamedType(Lexer::getSymbol("PTR"), ptr.data() ) );
#endif

#if 0
    d_globals->add( new Const( Lexer::getSymbol("untagged"), 0 ) );
    d_globals->add( new Const( Lexer::getSymbol("union"), 0 ) );
    d_globals->add( new Const( Lexer::getSymbol("align8"), 0 ) );
    d_globals->add( new Const( Lexer::getSymbol("noalign"), 0 ) );
    d_globals->add( new Const( Lexer::getSymbol("align2"), 0 ) );
#endif

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
        if( resolveImport(m) )
            hasErrors = true;
    }
    return hasErrors;
}

bool Model::resolveImport(Module* m)
{
    bool hasErrors = false;
    foreach( Import* i, m->d_imports )
    {
        QPair<Module*,Module*> res = findModule(m->d_fullName.mid(0,m->d_fullName.size()-1), i->d_path );
        if( res.first && res.second )
        {
            warning( Loc(i->d_loc,m->d_file), tr("ambigous imports: prefer %1 in same package instead of top-level %2")
                     .arg( res.first->getName().constData() ).arg( res.second->getName().constData() ) );
            i->d_mod = res.first;
        }else
            i->d_mod = res.first;
        if( i->d_mod.isNull() )
        {
            // mechanism to resolve built-in modules and preloads (SYSTEM, oakwook, etc.)
            // which are not explicitly part of the project
            if( i->d_path.size() == 1 && i->d_path.last() == d_systemModule->d_name )
                i->d_mod = d_systemModule;
            else
            {
                i->d_mod = d_others.value( i->d_path );
                if( i->d_mod.isNull() )
                {
                    i->d_mod = parseFile( i->d_path.join('.') ); // works because of FileCache which includes both file and virtual paths
                    if( i->d_mod.isNull() )
                    {
                        error( Loc( i->d_loc, m->d_file ), tr("cannot find module '%1'").
                               arg( i->d_path.join('.').constData() ) );
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
        if( !i->d_mod.isNull() )
            i->d_mod->d_usedBy.append(m);
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
    d_depOrder.clear();
    d_insts.clear();

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
    int circularErrs = 0;
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
            circularErrs++;
        }
    }

#endif

#if 0
    qDebug() << "**** dependency order:";
    foreach( Module* m, d_depOrder )
        qDebug() << m->d_name;
#endif
    if( circularErrs )
        return false;
    else
        return true;
}

QPair<Module*,Module*> Model::findModule(const VirtualPath& package, const VirtualPath& module)
{
    Module* top = d_modules.value(module).data();
    // module could be an absolute or a relative path
    // first check in the same package then in the global module path list
    if( module.size() == 1 )
    {
        const QByteArray name = module.first();
        QList<Module*> mods = d_packages.value(package);
        foreach( Module* m, mods )
        {
            if( m->d_name == name )
            {
                if( top != m )
                {
                    return  qMakePair(m,top); // ambiguous
                }
            }
        }
    }
    return qMakePair(top,(Module*)0);
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

bool Model::warning(const Loc& loc, const QString& msg)
{
    d_errs->warning( Errors::Semantics, loc, msg );
    return false;
}

