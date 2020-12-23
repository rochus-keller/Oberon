/*
* Copyright 2019, 2020 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include "ObAst.h"
#include "ObErrors.h"
#include "ObFileCache.h"
#include "ObLexer.h"
#include "ObParser.h"
#include "ObAstEval.h"
#include "ObAstValidator.h"
#include <QtDebug>
#include <QFile>
#include <QBuffer>
#include <typeinfo>
#include <limits.h>
using namespace Ob;

//#define _DUMP_AST
#ifdef _DUMP_AST
#include "ObCodeModel.h"
#endif

const char* Ast::Thing::s_tagName[] =
{
    "Thing", "Module", "Import", "Pointer", "Record", "BaseType", "Array", "ProcType", "NamedType",
    "CallExpr", "Literal", "SetExpr", "IdentLeaf", "UnExpr", "IdentSel", "BinExpr", "Field",
    "Const", "BuiltIn", "Parameter", "Return", "Procedure", "Variable", "LocalVar", "TypeRef",
    "QualiType", "Call", "Assign", "IfLoop", "ForLoop", "CaseStmt", "Scope"
};

const char* Ast::BaseType::s_typeName[] =
{
    "ANY", "ANYNUM", "NIL", "STRING", "BOOLEAN", "CHAR", "INTEGER", "REAL", "BYTE", "SET"
};

const char* Ast::BuiltIn::s_typeName[] =
{
    "ABS", "ODD", "LEN", "LSL", "ASR", "ROR", "FLOOR", "FLT", "ORD",
    "CHR", "INC", "DEC", "INCL", "EXCL", "NEW", "ASSERT", "PACK", "UNPK",
    "WriteInt", "WriteReal", "WriteChar", "WriteLn",
    "LED", "TRAP", "TRAPIF",
    "ADR", "BIT", "GET", "H", "LDREG", "PUT", "REG", "VAL", "COPY"
};

const char* Ast::UnExpr::s_opName[] =
{
    "???",
    "NEG", "NOT", "DEREF", "CAST", "SEL", "CALL"
};

const char* Ast::BinExpr::s_opName[] =
{
    "???",
    "Index", "Range",
    "EQ", "NEQ", "LE", "LEQ", "GT", "GEQ", "IN", "IS",
    "ADD", "SUB", "OR",
    "MUL", "FDIV", "DIV", "MOD", "AND"
};

static inline RowCol toRowCol(SynTree* st)
{
    Q_ASSERT( st != 0 );
    RowCol res;
    if( !res.setRowCol( st->d_tok.d_lineNr, st->d_tok.d_colNr ) )
        qWarning() << "exceeding maximum row or column number at" << st->d_tok.d_sourcePath << st->d_tok.d_lineNr;
    return res;
}

static SynTree* findFirstChild(const SynTree* st, int type , int startWith = 0)
{
    if( st == 0 )
        return 0;
    for( int i = startWith; i < st->d_children.size(); i++ )
    {
        SynTree* sub = st->d_children[i];
        if( sub->d_tok.d_type == type )
            return sub;
    }
    if( st->d_tok.d_type == type )
        return const_cast<SynTree*>(st);
    return 0;
}

static SynTree* flatten(SynTree* st, int stopAt = 0)
{
    if( st == 0 )
        return 0;
    while( st->d_children.size() == 1 && ( stopAt == 0 || st->d_tok.d_type != stopAt ) )
        st = st->d_children.first();
    return st;
}

static void markUsed( Ast::Type* t )
{
    if( t == 0 )
        return;

    if( t->d_ident )
    {
        if( t->d_ident->d_usedFromLive )
            return; // already visited
        t->d_ident->d_usedFromLive = true;
    }

    const int tag = t->getTag();
    switch( tag )
    {
    case Ast::Thing::T_Pointer:
        markUsed( Ast::thing_cast<Ast::Pointer*>(t)->d_to.data() );
        break;
    case Ast::Thing::T_Array:
        markUsed( Ast::thing_cast<Ast::Array*>(t)->d_type.data() );
        break;
    case Ast::Thing::T_Record:
        {
            Ast::Record* r = Ast::thing_cast<Ast::Record*>(t);
            if( !r->d_base.isNull() )
                markUsed( r->d_base.data() );
            if( r->d_binding && r->d_binding->d_ident )
                r->d_binding->d_ident->d_usedFromLive = true;
        }
        break;
    case Ast::Thing::T_QualiType:
        markUsed( Ast::thing_cast<Ast::QualiType*>(t)->d_quali->d_type.data() );
        break;
    }

    // if d_ident is in another module then it must be public, otherwise it could not be
    // used as type here; with d_usedFromLive we cover also local use for records
    // not public and with no direct live range
}

static void extendLiveRange( Ast::Scope* s, Ast::Named* n, SynTree* st )
{
    // called only for the first ident of a desig
    if( s == n->d_scope )
    {
        // range
        if( n->d_liveFrom == 0 )
            n->d_liveTo = n->d_liveFrom = st->d_tok.d_lineNr;
        else if( st->d_tok.d_lineNr > n->d_liveTo )
            n->d_liveTo = st->d_tok.d_lineNr;
    }else
        n->d_usedFromSubs = true;

    if( !n->d_type.isNull() )
        markUsed( n->d_type.data() );
}

Ast::Model::Model(QObject*p):QObject(p),d_enableExt(false),d_senseExt(false),d_curModule(0),
    d_curTypeDecl(0),d_fillXref(false)
{
    d_errs = new Errors(this);
    d_fc = new FileCache(this);

    d_global = new Scope();
    d_globalLc = new Scope();
    d_boolType = new BaseType(BaseType::BOOLEAN);
    d_charType = new BaseType(BaseType::CHAR);
    d_byteType = new BaseType(BaseType::BYTE); // The type BYTE is compatible with the type INTEGER, and vice-versa.
    d_intType = new BaseType(BaseType::INTEGER);
    d_realType = new BaseType(BaseType::REAL);
    d_setType = new BaseType(BaseType::SET);
    d_stringType = new BaseType(BaseType::STRING);
    d_nilType = new BaseType(BaseType::NIL);
    d_anyType = new BaseType(BaseType::ANY);
    d_anyNum = new BaseType(BaseType::ANYNUM);
}

Ast::Model::~Model()
{
    unbindFromGlobal();
}

void Ast::Model::clearclear()
{
    d_errs->clear();

    d_depOrder.clear();
    d_xref.clear();
    d_global = 0;
    d_globalLc = 0;
    d_depOrder.clear();
    d_boolType = 0;
    d_charType = 0;
    d_byteType = 0;
    d_intType = 0;
    d_realType = 0;
    d_setType = 0;
    d_stringType = 0;
    d_nilType = 0;
    d_anyType = 0;
    d_anyNum = 0;
}

void Ast::Model::clear()
{
    d_errs->clear();

    d_depOrder.clear();
    d_xref.clear();

    unbindFromGlobal();
    d_global->d_names.clear();
    d_globalLc->d_names.clear();

    Ref<NamedType> t;

    // Built-in types
    t = new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_boolType->d_type]),d_boolType.data(), d_global.data());
    d_global->d_names.insert( t->d_name.constData(), t.data() );

    t = new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_charType->d_type]),d_charType.data(), d_global.data());
    d_global->d_names.insert( t->d_name.constData(), t.data() );

    t = new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_byteType->d_type]),d_byteType.data(), d_global.data());
    d_global->d_names.insert( t->d_name.constData(), t.data() );

    t = new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_intType->d_type]),d_intType.data(), d_global.data());
    d_global->d_names.insert( t->d_name.constData(), t.data() );

    t = new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_realType->d_type]),d_realType.data(), d_global.data());
    d_global->d_names.insert( t->d_name.constData(), t.data() );

    t = new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_setType->d_type]),d_setType.data(), d_global.data());
    d_global->d_names.insert( t->d_name.constData(), t.data() );

    t = new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_stringType->d_type]),d_stringType.data(),d_global.data());
    d_global->d_names.insert( t->d_name.constData(), t.data() );

    t = new NamedType(Lexer::getSymbol(BaseType::s_typeName[d_nilType->d_type]),d_nilType.data(),d_global.data());
    d_global->d_names.insert( t->d_name.constData(), t.data() );

    // redundant types because of existing source code compatibility
    t = new NamedType(Lexer::getSymbol("LONGINT"),d_intType.data(),d_global.data());
    d_global->d_names.insert( t->d_name.constData(), t.data() );

    t = new NamedType(Lexer::getSymbol("SHORTINT"),d_intType.data(),d_global.data());
    d_global->d_names.insert( t->d_name.constData(), t.data() );

    t = new NamedType(Lexer::getSymbol("LONGREAL"),d_realType.data(),d_global.data());
    d_global->d_names.insert( t->d_name.constData(), t.data() );

    Ref<BuiltIn> bi;

    // MODULE System
    d_system = Lexer::getSymbol("SYSTEM");
    Ref<Module> sys = new Module();
    sys->d_name = d_system;
    sys->d_isDef = true;
    sys->d_synthetic = true;

    bi = new BuiltIn(BuiltIn::ADR, new ProcType( Type::List() << d_anyType.data(), d_intType.data() ) );
    sys->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::BIT, new ProcType( Type::List() << d_intType.data() << d_intType.data(), d_boolType.data() ) );
    sys->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::GET, new ProcType( Type::List() << d_intType.data() << d_anyType.data(),
                                              ProcType::Vars() << false << true   ) );
    sys->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::H, new ProcType( Type::List() << d_intType.data(), d_intType.data() ) );
    sys->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::LDREG, new ProcType( Type::List() << d_intType.data() << d_intType.data() ) );
    sys->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::PUT, new ProcType( Type::List() << d_intType.data() << d_anyType.data() ) );
    sys->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::REG, new ProcType( Type::List() << d_intType.data(), d_intType.data() ) );
    sys->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::VAL, new ProcType( Type::List() << d_anyType.data() << d_anyType.data(), d_anyType.data() ) );
    sys->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::COPY, new ProcType( Type::List() << d_intType.data() << d_intType.data() << d_intType.data(),
                                                  ProcType::Vars() << false << false << false) ); // all three INTEGER representing address
    sys->d_names.insert(bi->d_name.constData(),bi.data());

    d_global->d_names.insert( sys->d_name.constData(), sys.data() );

    // Built-in procedures
    bi = new BuiltIn(BuiltIn::ABS, new ProcType( Type::List() << d_anyNum.data(), d_anyNum.data() ) );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::ODD, new ProcType( Type::List() << d_intType.data(), d_boolType.data() ) );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::LEN, new ProcType( Type::List() << d_anyType.data(), d_intType.data() ) );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::LSL, new ProcType( Type::List() << d_intType.data() << d_intType.data(), d_intType.data() ) );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::ASR, new ProcType( Type::List() << d_intType.data() << d_intType.data(), d_intType.data() ) );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::ROR, new ProcType( Type::List() << d_anyType.data() // integer type or SET in Oberon System
                                                 << d_intType.data(), d_intType.data() ) );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::FLOOR, new ProcType( Type::List() << d_realType.data(), d_intType.data() ) );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::FLT, new ProcType( Type::List() << d_intType.data(), d_realType.data() ) );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::ORD, new ProcType( Type::List() << d_anyType.data(), d_intType.data() ) ); // char, bool, set
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::CHR, new ProcType( Type::List() << d_intType.data(), d_charType.data() ) );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::INC, new ProcType( Type::List() << d_intType.data() << d_intType.data(),
                         ProcType::Vars() << true << false ) ); // optional second param
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::DEC, new ProcType( Type::List() << d_intType.data() << d_intType.data(),
                         ProcType::Vars() << true << false ) );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::INCL, new ProcType( Type::List() << d_setType.data() << d_intType.data(),
                                                  ProcType::Vars() << true << false ) );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::EXCL, new ProcType( Type::List() << d_setType.data() << d_intType.data(),
                                                  ProcType::Vars() << true << false ) );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::NEW, new ProcType( Type::List() << d_anyType.data(), ProcType::Vars() << true ) );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::ASSERT, new ProcType( Type::List() << d_boolType.data() ) );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::PACK, new ProcType( Type::List() << d_realType.data() << d_intType.data(),
                                                  ProcType::Vars() << true << false ) );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::UNPK, new ProcType( Type::List() << d_realType.data() << d_intType.data(),
                                                  ProcType::Vars() << true << true ) );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::LED, new ProcType( Type::List() << d_intType.data() ) );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::TRAP, new ProcType());
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::TRAPIF, new ProcType( Type::List() << d_boolType.data() ) );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::WriteInt, new ProcType( Type::List() << d_intType.data() ) );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::WriteReal, new ProcType( Type::List() << d_realType.data() ) );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::WriteChar, new ProcType( Type::List() << d_charType.data() ) );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());

    bi = new BuiltIn(BuiltIn::WriteLn );
    d_global->d_names.insert(bi->d_name.constData(),bi.data());


    d_globalLc->d_names = d_global->d_names;
    Scope::Names::const_iterator i;
    for( i = d_global->d_names.begin(); i != d_global->d_names.end(); ++i )
        d_globalLc->d_names.insert( Lexer::getSymbol( QByteArray(i.key()).toLower() ), i.value() );

}

void Ast::Model::addPreload(const QByteArray& name, const QByteArray& source)
{
    d_fc->addFile( name, source, true );
}

static QList<Ast::Module*> getImportedModules( Ast::Module* m )
{
    QList<Ast::Module*> res;
    foreach( Ast::Named* n, m->d_order )
    {
        if( n->getTag() == Ast::Thing::T_Import )
        {
            Ast::Import* imp = Ast::thing_cast<Ast::Import*>(n);
            res << imp->d_mod.data();
        }
    }
    return res;
}

static bool anyWithError( const QList<Ast::Module*>& mods )
{
    foreach( Ast::Module* m, mods )
    {
        if( m->d_hasErrors )
            return true;
    }
    return false;
}

bool Ast::Model::parseFiles(const QStringList& files)
{
    if( files.isEmpty() )
    {
        qDebug() << "nothing to parse";
        return false;
    }

    clear();

    ParseResultList pr;
    foreach( const QString& path, files )
    {
        qDebug() << "parsing" << path;
        if( !parseFile(path,pr) )
            qCritical() << "cannot open file" << path;
    }

    //if( d_errs->getErrCount() != 0 )
    //    return false;

    qDebug() << "checking dependencies...";

    Mods mods;
    createModules(mods,pr);

    QList<Module*> order = findProcessingOrder(mods);

    // if( d_errs->getErrCount() != 0 )
    //    return false;

    foreach( Module* m, order )
    {
        if( mods[m->d_name.constData()].d_st == 0 )
        {
            Q_ASSERT( m->d_name.constData() == d_system.constData() );
            continue;
        }
        qDebug() << "analyzing" << m->d_file;
        module( m, mods[m->d_name.constData()].d_st );

        if( !m->d_hasErrors )
        {
            if( anyWithError( getImportedModules(m) ) )
            {
                d_errs->error(Errors::Semantics, m->d_file, m->d_loc.d_row, m->d_loc.d_col,
                              tr("There are errors in imported modules") );
                m->d_hasErrors = true;
            }else
                Validator::validate( this, m, d_errs );
        }
    }

    return d_errs->getErrCount() == 0;
}

bool Ast::Model::parseFile(const QString& path, ParseResultList& res) const
{
    bool found;
    FileCache::Entry content = d_fc->getFile(path, &found );
    if( found )
    {
        QBuffer buf;
        buf.setData( content.d_code );
        buf.open(QIODevice::ReadOnly);
        parseFile( &buf, path, res );
    }else
    {
        QFile file(path);
        if( !file.open(QIODevice::ReadOnly) )
            return false;
        parseFile( &file, path, res );
    }
    return true;
}

void Ast::Model::parseFile(QIODevice* in, const QString& path, ParseResultList& res) const
{
    const quint32 before = d_errs->getErrCount();
    Ob::Lexer lex;
    lex.setErrors(d_errs);
    lex.setCache(d_fc);
    lex.setIgnoreComments(false);
    lex.setPackComments(true);
    if( d_senseExt )
        lex.setSensExt(d_senseExt);
    else
        lex.setEnableExt(d_enableExt);
    lex.setStream( in, path );
    Ob::Parser p(&lex,d_errs);
    p.RunParser();

    if( before != d_errs->getErrCount() )
        return;

    QList<SynTree*> toDelete;
    foreach( SynTree* st, p.d_root.d_children )
    {
        if( st->d_tok.d_type == SynTree::R_module || st->d_tok.d_type == SynTree::R_definition )
        {
#ifdef _DUMP_AST
            QFile out("dump.txt");
            out.open(QIODevice::WriteOnly);
            QTextStream ts(&out);
            Ob::CodeModel::dump(ts,st);
#endif
            SynTree* id = findFirstChild(st,Tok_ident);
            Q_ASSERT( id != 0 );
            ParseResult& pr = res[ id->d_tok.d_val.constData() ];
            if( pr.d_modName != 0 )
            {
                error(id,tr("duplicate module name '%1'").arg(id->d_tok.d_val.constData()));
                toDelete << st;
            }else
            {
                pr.d_modName = id;
                pr.d_isExt = lex.isEnabledExt();
                pr.d_modRoot = st;
            }
        }else
            toDelete << st;
    }
    p.d_root.d_children.clear();
    foreach( SynTree* st, toDelete )
        delete st;
}

Ast::Model::Modules Ast::Model::getModules() const
{
    Modules res;
    Scope::Names::const_iterator i;
    for( i = d_global->d_names.begin(); i != d_global->d_names.end(); ++i )
    {
        if( i.value()->getTag() == Thing::T_Module )
            res << Ast::thing_cast<Module*>(i.value().data());
    }
    return res;
}

Ast::Model::BaseTypes Ast::Model::getBaseTypes() const
{
    BaseTypes res;
    res.d_anyNum = d_anyNum.data();
    res.d_anyType = d_anyType.data();
    res.d_boolType = d_boolType.data();
    res.d_byteType = d_byteType.data();
    res.d_charType = d_charType.data();
    res.d_intType = d_intType.data();
    res.d_realType = d_realType.data();
    res.d_nilType = d_nilType.data();
    res.d_setType = d_setType.data();
    res.d_stringType = d_stringType.data();
    return res;
}

bool Ast::Model::module(Ast::Module* m, SynTree* st)
{
    const quint32 errs = d_errs->getErrCount();
    d_curModule = m;
    Q_ASSERT( d_deferBody.isEmpty() );
    for( int i = 2; i < st->d_children.size(); i++ )
    {
        SynTree* sub = st->d_children[i];
        switch( sub->d_tok.d_type )
        {
        case SynTree::R_ImportList:
            importList(m, sub);
            break;
        case SynTree::R_DeclarationSequence:
            Q_ASSERT(!m->d_isDef);
            declarationSequence(m,sub,false);
            deferredBody();
            break;
        case SynTree::R_DeclarationSequence2:
            Q_ASSERT(m->d_isDef);
            declarationSequence(m,sub,true);
            break;
        case Tok_ident:
            if( sub->d_tok.d_val != m->d_name )
                return error(sub,tr("ident after END is supposed to be equal to module name") );
            break;
        case SynTree::R_StatementSequence:
            statementSequence(m, m->d_body, sub );
            break;
        case Tok_END:
            m->d_end = toRowCol(sub);
            break;
        case Tok_BEGIN:
            break;
        default:
            Q_ASSERT( false );
            break;
        }
    }
    d_curModule->d_hasErrors = ( d_errs->getErrCount() - errs ) != 0;
    d_curModule = 0;
    return true;
}

bool Ast::Model::importList(Ast::Module* m, SynTree* st)
{
    Q_ASSERT( st->d_children.size() >= 2 && st->d_children.first()->d_tok.d_type == Tok_IMPORT );
    for( int i = 1; i < st->d_children.size(); i++ )
    {
        SynTree* imp = st->d_children[i];
        Q_ASSERT( !imp->d_children.isEmpty() );
        SynTree* moduleName = imp->d_children.first();
        SynTree* nickname = moduleName;
        if( imp->d_children.size() > 1 )
        {
            Q_ASSERT( imp->d_children.size() == 3 );
            moduleName = imp->d_children.last();
        }
        Q_ASSERT( m->d_scope );
        Named* n = m->d_scope->find(moduleName->d_tok.d_val);
        if( n && n->getTag() == Thing::T_Module )
        {
            Module* mi = Ast::thing_cast<Module*>(n);
            Ref<Import> ii = new Import();
            ii->d_mod = mi;
            ii->d_loc = toRowCol(imp);
            ii->d_name = nickname->d_tok.d_val;
            ii->d_scope = m;
            if( d_fillXref )
            {
                Q_ASSERT( !m->d_helper.isEmpty() && m->d_helper.first()->getIdent()->getTag() == Thing::T_Module );
                d_xref[mi].append( m->d_helper.first().data() );

                IdentLeaf* e1 = new IdentLeaf( ii.data(), nickname, d_curModule, 0 );
                m->d_helper.append( e1 );
                d_xref[ii.data()].append( e1 );
                if( imp->d_children.size() > 1 )
                {
                    IdentLeaf* e2 = new IdentLeaf( mi, moduleName, d_curModule, 0 );
                    m->d_helper.append( e2 );
                    d_xref[mi].append( e2 );
                }
            }

            addToScope( m, ii.data() );
        }// else already reported
    }
    return true;
}

bool Ast::Model::declarationSequence(Scope* m, SynTree* st, bool definition)
{
    QList<Named*> typeDecls;
    for( int i = 0; i < st->d_children.size(); i++ )
    {
        SynTree* sub = st->d_children[i];
        switch( sub->d_tok.d_type )
        {
        case SynTree::R_ConstDeclaration:
            constDeclaration(m,sub);
            break;
        case SynTree::R_TypeDeclaration:
            typeDecls << typeDeclaration(m,sub);
            break;
        case SynTree::R_VariableDeclaration:
            fixTypes();
            variableDeclaration(m,sub);
            break;
        case SynTree::R_ProcedureDeclaration:
            fixTypes();
            procedureDeclaration(m,sub,definition);
            break;
        case SynTree::R_ProcedureHeading:
            fixTypes();
            procedureDeclaration(m,sub,definition);
            break;
        case Tok_CONST:
        case Tok_TYPE:
        case Tok_VAR:
            break;
        default:
            qCritical() << "declarationSequence not supported" << SynTree::rToStr(sub->d_tok.d_type);
            Q_ASSERT( false );
            break;
        }
    }
    fixTypes();
    foreach( Named* n, typeDecls )
        checkSelfRefs(n,n->d_type.data(),true,false,false);
    return true;
}

bool Ast::Model::procedureDeclaration(Ast::Scope* m, SynTree* st, bool headingOnly)
{
    Q_ASSERT( st->d_children.size() >= 1 );

    Ref<Procedure> p = new Procedure();
    p->d_loc = toRowCol(st);
    p->d_scope = m;
    SynTree* id = procedureHeading(p.data(), headingOnly ? st : st->d_children.first() );
    addToScope(m,p.data()); // body needs to reference this procedure if recursively called
    if( !headingOnly )
    {
        Q_ASSERT( st->d_children.size() == 3 );
        procedureBody(p.data(),st->d_children[1]);
        if( st->d_children.last()->d_tok.d_val != p->d_name )
            error( st->d_children.last(), tr("final ident doesn't correspond to procedure name") );
    }

    if( d_fillXref )
    {
        m->d_helper << new IdentLeaf(p.data(),id,d_curModule, p->d_type.data());
        d_xref[p.data()].append( m->d_helper.back().data() );
    }

    return true;
}

SynTree* Ast::Model::procedureHeading(Ast::Procedure* p, SynTree* st)
{
    Q_ASSERT( st->d_children.size() >= 2 && st->d_children[1]->d_tok.d_type == SynTree::R_identdef );
    SynTree* idef = st->d_children[1];
    Q_ASSERT( !idef->d_children.isEmpty() && idef->d_children.first()->d_tok.d_type == Tok_ident );
    p->d_name = idef->d_children.first()->d_tok.d_val;
    if( idef->d_children.size() > 1 )
    {
        Q_ASSERT( idef->d_children.size() == 2 &&  idef->d_children.last()->d_tok.d_type == Tok_Star );
        p->d_public = true;
    }
    p->d_isDef = d_curModule->d_isDef;

    if( st->d_children.size() > 2 )
    {
        Q_ASSERT( st->d_children.size() == 3 );
        Ref<ProcType> pt = formalParameters( p, st->d_children.last() );
        p->d_type = pt.data();
        for( int i = 0; i < pt->d_formals.size(); i++ )
        {
            addToScope( p, pt->d_formals[i].data() );
        }
    }else
        p->d_type = new ProcType();

    publicWarning(p);

    return idef->d_children.first();
}

bool Ast::Model::procedureBody(Ast::Procedure* p, SynTree* st)
{
    Q_ASSERT( !st->d_children.isEmpty() );
    declarationSequence(p,st->d_children.first(),false);
    bool returnFound = false;
    for( int i = 1; i < st->d_children.size(); i++ )
    {
        switch( st->d_children[i]->d_tok.d_type )
        {
        case SynTree::R_StatementSequence:
            // defer evaluation because a procedure can call others declared later
            d_deferBody << DeferBody(st->d_children[i],p);
            break;
        case SynTree::R_ReturnStatement:
            d_deferBody << DeferBody(st->d_children[i],p);
            returnFound = true;
            break;
        case Tok_BEGIN:
            break;
        case Tok_END:
            p->d_end = toRowCol(st->d_children[i]);
            break;
        default:
            Q_ASSERT( false );
            break;
        }
    }
    Q_ASSERT( p->d_type->getTag() == Thing::T_ProcType );
    ProcType* pt = Ast::thing_cast<ProcType*>(p->d_type.data() );
    if( !pt->d_return.isNull() )
    {
        if( !returnFound )
            error( st, tr("RETURN statement expected in function") );
    }

    return true;
}

void Ast::Model::deferredBody(Ast::Procedure* p, SynTree* st)
{
    switch( st->d_tok.d_type )
    {
    case SynTree::R_StatementSequence:
        statementSequence(p,p->d_body,st);
        break;
    case SynTree::R_ReturnStatement:
        {
            SynTree* rs = st;
            Q_ASSERT( rs->d_children.size() == 2 );
            Ref<Return> r = new Return();
            r->d_loc = toRowCol(rs);
            r->d_what = expression(p,rs->d_children.last());
            p->d_body << r.data();
        }
        break;
    default:
        Q_ASSERT( false );
        break;
    }
}

void Ast::Model::deferredBody()
{
    foreach( const DeferBody& d, d_deferBody )
        deferredBody(d.d_p,d.d_st);
    d_deferBody.clear();
}

Ast::Named* Ast::Model::typeDeclaration(Scope* m, SynTree* st)
{
    Q_ASSERT( st->d_children.size() == 3 && st->d_children.first()->d_tok.d_type == SynTree::R_identdef );
    SynTree* idef = st->d_children.first();
    Q_ASSERT( !idef->d_children.isEmpty() && idef->d_children.first()->d_tok.d_type == Tok_ident );
    const QByteArray name = idef->d_children.first()->d_tok.d_val;
    bool pub = false;
    if( idef->d_children.size() > 1 )
    {
        Q_ASSERT( idef->d_children.size() == 2 &&  idef->d_children.last()->d_tok.d_type == Tok_Star );
        pub = true;
    }
    Ref<NamedType> nt = new NamedType();
    d_curTypeDecl = nt.data();
    nt->d_name = name;
    nt->d_loc = toRowCol(idef);
    nt->d_public = pub;
    nt->d_isDef = d_curModule->d_isDef;
    nt->d_scope = m;
    publicWarning(nt.data());
    addToScope(m,nt.data()); // add to scope before type() to support recursive declarations
    nt->d_type = type(m,nt.data(),st->d_children.last());
    if( !nt->d_type.isNull() )
    {
        if( nt->d_type->isSelfRef() )
            error(st->d_children.last(),tr("recursive type definition"));

        // NOTE: check SelfRefs when type is set. If type is pointer or procedure then there is only
        // an issue when X = POINTER TO X, but not X = POINTER TO RECORD x: X; END.
        // If type is not a pointer, then X = RECORD x: X; END is an error.
        // The following cases are already checked: X = X or X = RECORD(X)
    }
    if( d_fillXref )
    {
        m->d_helper << new IdentLeaf(nt.data(),idef->d_children.first(),d_curModule, nt->d_type.data() );
        d_xref[nt.data()].append( m->d_helper.back().data() );
    }

    d_curTypeDecl = 0;
    return nt.data();
}

Ast::Ref<Ast::Type> Ast::Model::type(Scope* s, Named* id, SynTree* st, Pointer* binding )
{
    Q_ASSERT( st->d_children.size() == 1 );
    SynTree* sub = st->d_children.first();
    switch( sub->d_tok.d_type )
    {
    case SynTree::R_NamedType:
        return namedType(s,id,sub,binding);
    case SynTree::R_ArrayType:
        {
            Ref<Type> t = arrayType(s, sub );
            Q_ASSERT( t->d_ident == 0 );
            t->d_ident = id;
            return t;
        }
    case SynTree::R_RecordType:
        {
            Ref<Type> t = recordType(s,sub, binding);
            Q_ASSERT( t->d_ident == 0 );
            t->d_ident = id;
            return t;
        }
    case SynTree::R_PointerType:
        {
            Ref<Type> t = pointerType(s,sub);
            Q_ASSERT( t->d_ident == 0 );
            t->d_ident = id;
            return t;
        }
    case SynTree::R_ProcedureType:
        if( sub->d_children.size() == 1 )
        {
            Ref<Type> t = new ProcType();
            Q_ASSERT( t->d_ident == 0 );
            t->d_ident = id;
            return t;
        }
        else if( sub->d_children.size() == 2 )
        {
            Ref<Type> t = formalParameters( s, sub->d_children.last() ).data();
            Q_ASSERT( t->d_ident == 0 );
            t->d_ident = id;
            return t;
        }
        else
            Q_ASSERT( false );
        break;
    default:
        qCritical() << "Model::type invalid type" << SynTree::rToStr(sub->d_tok.d_type);
        Q_ASSERT(false);
        break;
    }
    return Ref<Ast::Type>();
}

Ast::Ref<Ast::Type> Ast::Model::namedType(Ast::Scope* s, Ast::Named* id, SynTree* st, Ast::Pointer* binding)
{
    Q_ASSERT( st->d_children.size() == 1 && st->d_children.first()->d_tok.d_type == SynTree::R_qualident );
    SynTree* sub = st->d_children.first();
    Ref<Type> t;
    const bool isPointerToType = binding != 0;
    if( isPointerToType ) // only Pointer may reference subsequent declarations
    {
        if( sub->d_children.size() == 2 )
        {
            // Resolve qualidents to imported modules immediately
            t = getTypeFromQuali(s,sub).data();
        }else
        {
            // Only resolve qualidents after all type declarations, otherwhise a POINTER TO T where
            // T exists in outer scope and will be declared later is wrongly assigned!
            // See T2TypeDeclarations.obn
            d_fixType.append( FixType(binding, s, st) );
        }
    }else
        t = getTypeFromQuali(s,sub).data();
    if( !t.isNull() )
    {
        Q_ASSERT( t->d_ident == 0 );
        t->d_ident = id;
    }
    return t;
}

static quint32 evalLen( Errors* errs, Ast::Expression* e, SynTree* st )
{
    QString msg;
    const QVariant v = Ast::Eval::evalConstExpr( e, &msg );
    if( !v.isValid() )
    {
        errs->error(Errors::Semantics, st->d_tok.toLoc(), msg);
        return 0;
    }
    qint64 len = v.toLongLong();
    if( v.type() != QVariant::LongLong || len <= 0 )
        errs->error(Errors::Semantics, st->d_tok.toLoc(), Ast::Model::tr("expecting positive integer") );
    else if( len > UINT_MAX )
        errs->error(Errors::Semantics, st->d_tok.toLoc(), Ast::Model::tr("maximum supported array lenght is 2^32") );
    else
        return len;
    return 0;
}

Ast::Ref<Ast::Type> Ast::Model::arrayType(Ast::Scope* ds, SynTree* t)
{
    SynTree* ll = findFirstChild( t, SynTree::R_LengthList );
#ifndef OB_OBN2
    Q_ASSERT( ll != 0 && !ll->d_children.isEmpty() && ll->d_children.first()->d_tok.d_type == SynTree::R_expression );
#endif
    Q_ASSERT( !t->d_children.isEmpty() && t->d_children.last()->d_tok.d_type == SynTree::R_type );

    Ref<Type> tp = type(ds,0,t->d_children.last());

    // Not a good check. X = POINTER TO RECORD Y: ARRAY OF X; END; is legal!
    // if( tp->getTag() == Thing::T_SelfRef )
    //    error(t->d_children.last(),tr("recursive declaration"));

    Ref<Array> res = new Array();
    res->d_type = tp;

    if( ll )
    {
        res->d_lenExpr = expression(ds, ll->d_children.first() );
        res->d_len = evalLen( d_errs, res->d_lenExpr.data(), ll->d_children.first() );

        Ref<Array> last = res;
        for( int i = 1; i < ll->d_children.size(); i++ )
        {
            Q_ASSERT( ll->d_children[i]->d_tok.d_type == SynTree::R_expression );
            Ref<Array> cur = new Array();
            last->d_type = cur.data();
            cur->d_type = tp;

            cur->d_lenExpr = expression(ds, ll->d_children[i] );
            cur->d_len = evalLen( d_errs, cur->d_lenExpr.data(), ll->d_children[i] );

            last = cur;
        }
    }
    return res.data();
}

Ast::Ref<Ast::Type> Ast::Model::pointerType(Ast::Scope* ds, SynTree* t)
{
    Q_ASSERT( t->d_children.size() > 1 && t->d_children[2]->d_tok.d_type == SynTree::R_type );
    Ref<Pointer> res = new Pointer();
    res->d_to = type(ds, 0, t->d_children[2], res.data() );
    if( !res->d_to.isNull() && res->d_to->derefed()->getTag() != Thing::T_Record )
        error(t,tr("not pointing to a record"));
    return res.data();
}

Ast::Ref<Ast::ProcType> Ast::Model::formalParameters(Ast::Scope* s, SynTree* st)
{
    Q_ASSERT( st->d_tok.d_type == SynTree::R_FormalParameters );
    Ref<ProcType> p = new ProcType();
    for( int i = 0; i < st->d_children.size(); i++ )
    {
        switch( st->d_children[i]->d_tok.d_type )
        {
        case SynTree::R_FPSection:
            fpSection( s, p.data(), st->d_children[i] );
            break;
        case SynTree::R_NamedType:
            Q_ASSERT( st->d_children[i]->d_children.size() == 1 );
            p->d_return = getTypeFromQuali(s, st->d_children[i]->d_children.first()).data();
            break;
        case Tok_Lpar:
        case Tok_Rpar:
            break;
        default:
            Q_ASSERT(false );
            break;
        }
    }
    return p;
}

Ast::Ref<Ast::Type> Ast::Model::recordType(Ast::Scope* s, SynTree* st, Pointer* binding)
{
    Ref<Record> rec = new Record();
    rec->d_binding = binding;
    SynTree* baseSt = findFirstChild(st,SynTree::R_BaseType, 1 );
    if( baseSt )
    {
        Q_ASSERT( baseSt->d_children.size() == 1 ); // NamedType
        baseSt = baseSt->d_children.first();
        Q_ASSERT( baseSt->d_children.size() == 1 ); // quelident
        baseSt = baseSt->d_children.first();
        Ref<QualiType> base = getTypeFromQuali(s, baseSt);
        if( !base.isNull() )
        {
            if( base->isSelfRef() )
                error(baseSt,tr("record cannot be the base of itself"));
            else
                rec->d_base = base.data();
        }
    }
    SynTree* fls = findFirstChild(st,SynTree::R_FieldListSequence, 1 );
    if( fls )
    {
        for( int i = 0; i < fls->d_children.size(); i++ )
        {
            if( fls->d_children[i]->d_tok.d_type == SynTree::R_FieldList )
                fieldList( s, rec.data(), fls->d_children[i] );
        }
    }

    return rec.data();
}

bool Ast::Model::variableDeclaration(Ast::Scope* ds, SynTree* t)
{
    Q_ASSERT( t->d_children.size() > 1 && t->d_children.first()->d_tok.d_type == SynTree::R_IdentList &&
              t->d_children.last()->d_tok.d_type == SynTree::R_type );
    Ref<Type> tp = type(ds, 0, t->d_children.last());
    foreach( SynTree* i, t->d_children.first()->d_children )
    {
        SynTree* id = i->d_children.first();
        const bool pub = i->d_children.size() > 1;

        Ref<Named> v = ( ds->getTag() == Thing::T_Module ? (Named*)new Variable() : (Named*)new LocalVar() );
        v->d_name = id->d_tok.d_val;
        v->d_public = pub;
        v->d_isDef = d_curModule->d_isDef;
        v->d_type = tp;
        v->d_loc = toRowCol(id);
        v->d_scope = ds;

        if( d_fillXref )
        {
            ds->d_helper << new IdentLeaf(v.data(),id,d_curModule, v->d_type.data() );
            d_xref[v.data()].append( ds->d_helper.back().data() );
        }

        publicWarning(v.data());
        addToScope( ds, v.data() );
    }
    return true;
}

bool Ast::Model::constDeclaration(Ast::Scope* m, SynTree* st)
{
    Q_ASSERT( st->d_tok.d_type == SynTree::R_ConstDeclaration );
    Q_ASSERT( st->d_children.size() == 3 && st->d_children.first()->d_tok.d_type == SynTree::R_identdef
              && !st->d_children.first()->d_children.isEmpty()
              && st->d_children.last()->d_tok.d_type == SynTree::R_expression );

    SynTree* id = st->d_children.first()->d_children.first();
    const bool pub = st->d_children.first()->d_children.size() > 1;

    Ref<Const> c = new Const();
    c->d_name = id->d_tok.d_val;
    c->d_public = pub;
    c->d_isDef = d_curModule->d_isDef;
    c->d_scope = m;
    // A ConstExpression is an expression containing constants only. More precisely, its evaluation must
    // be possible by a mere textual scan without execution of the program
    // Wirt's compiler seems not even to support name resolution for constants

    c->d_constExpr = expression(m,st->d_children.last());
    if( !c->d_constExpr.isNull() ) // prev errors
        c->d_type = c->d_constExpr->d_type.data();
    QString msg;
    c->d_val = Eval::evalConstExpr(c->d_constExpr.data(), &msg);
    c->d_loc = toRowCol(st);
    if( !c->d_val.isValid() )
        error(st->d_children.last(),msg);

    if( d_fillXref )
    {
        m->d_helper << new IdentLeaf(c.data(),id,d_curModule, c->d_type.data() );
        d_xref[c.data()].append( m->d_helper.back().data() );
    }

    addToScope( m, c.data() );
    publicWarning(c.data());

    return true;
}

bool Ast::Model::fpSection(Scope* s, Ast::ProcType* p, SynTree* st)
{
    Q_ASSERT( st->d_tok.d_type == SynTree::R_FPSection &&
              st->d_children.size() >= 2 && st->d_children.last()->d_tok.d_type == SynTree::R_FormalType );

    bool var = false;
    if( st->d_children.first()->d_tok.d_type == Tok_VAR )
        var = true;

    quint16 arrayOfCount = 0;
    SynTree* formalType = st->d_children.last();
    for( int i = 0; i < formalType->d_children.size(); i++ )
    {
        if( formalType->d_children[i]->d_tok.d_type == Tok_ARRAY )
            arrayOfCount++;
    }

#ifdef OB_OBN2
    Q_ASSERT( formalType->d_children.last()->d_tok.d_type == SynTree::R_type &&
              formalType->d_children.last()->d_children.size() == 1 );
    Ref<Type> t = type(s, 0, formalType->d_children.last()).data();
#else
    Q_ASSERT( formalType->d_children.last()->d_tok.d_type == SynTree::R_NamedType &&
              formalType->d_children.last()->d_children.size() == 1 );
    Ref<Type> t = getTypeFromQuali(s,formalType->d_children.last()->d_children.last()).data();
#endif
    if( arrayOfCount )
    {
        QList< Ref<Array> > tmp;
        while( arrayOfCount-- )
        {
           Ref<Array> a  = new Array();
           if( !tmp.isEmpty() )
               tmp.back()->d_type = a.data();
           tmp.push_back(a);
        }
        tmp.back()->d_type = t;
        t = tmp.front().data();
    }

    for( int i = 0; i < st->d_children.size(); i++ )
    {
        if( st->d_children[i]->d_tok.d_type == Tok_ident )
        {
            Ref<Parameter> v = new Parameter();
            v->d_type = t;
            v->d_name = st->d_children[i]->d_tok.d_val;
            v->d_loc = toRowCol(st->d_children[i]);
            v->d_scope = s;
            v->d_var = var;
            if( p->find( v->d_name ) != 0 )
                error(st->d_children[i],tr("parameter name not unique") );
            p->d_formals.append(v);
            if( d_fillXref )
            {
                s->d_helper << new IdentLeaf(v.data(),st->d_children[i],d_curModule, t.data() );
                d_xref[v.data()].append( s->d_helper.back().data() );
            }

        }
    }
    return true;
}

void Ast::Model::publicWarning( Ast::Named* n)
{
    Q_ASSERT( n && n->d_scope );
    if( n->d_public && n->d_scope->getTag() != Ast::Thing::T_Module )
        d_errs->warning(Errors::Semantics, d_curModule->d_file, n->d_loc.d_row, n->d_loc.d_col,
                        tr("declaring local symbol public") );
    else if( n->d_public && n->d_scope->getTag() == Ast::Thing::T_Module
             && Ast::thing_cast<Module*>(n->d_scope)->d_isDef )
        d_errs->warning(Errors::Semantics, d_curModule->d_file, n->d_loc.d_row, n->d_loc.d_col,
                        tr("in a DEFINITION all symbols are public") );
}

bool Ast::Model::fieldList(Scope* s, Ast::Record* rec, SynTree* st )
{
    Q_ASSERT( st->d_children.size() == 2 );
    Ref<Type> t = type(s, 0, st->d_children.last() );
    SynTree* fl = st->d_children.first();
    for( int i = 0; i < fl->d_children.size(); i++ )
    {
        if( fl->d_children[i]->d_tok.d_type == SynTree::R_identdef )
        {
            SynTree* idef = fl->d_children[i];
            Q_ASSERT( !idef->d_children.isEmpty() && idef->d_children.first()->d_tok.d_type == Tok_ident );
            const QByteArray name = idef->d_children.first()->d_tok.d_val;
            bool pub = false;
            if( idef->d_children.size() > 1 )
            {
                Q_ASSERT( idef->d_children.size() == 2 &&  idef->d_children.last()->d_tok.d_type == Tok_Star );
                pub = true;
            }
            if( rec->find(name, true) != 0 )
            {
                error( idef, tr("field name not unique") );
                continue;
            }
            Ref<Field> f = new Field();
            f->d_loc = toRowCol(idef);
            f->d_name = name;
            f->d_public = pub;
            f->d_isDef = d_curModule->d_isDef;
            f->d_scope = s; // CHECK what goes here?
            publicWarning(f.data());
            f->d_type = t;
            rec->d_fields.append(f);
            if( d_fillXref )
            {
                s->d_helper << new IdentLeaf(f.data(),idef->d_children.first(),d_curModule, t.data() );
                d_xref[f.data()].append( s->d_helper.back().data() );
            }

        }
    }
    return true;
}

bool Ast::Model::statementSequence(Ast::Scope* s, Ast::StatSeq& ss, SynTree* st)
{
    ss.clear();
    Q_ASSERT( st->d_tok.d_type == SynTree::R_StatementSequence );
    for( int i = 0; i < st->d_children.size(); i++ )
    {
        Ast::Ref<Ast::Statement> stmt = statement(s, st->d_children[i] );
        if( !stmt.isNull() )
            ss << stmt;
    }
    return true;
}

Ast::Ref<Ast::Statement> Ast::Model::statement(Ast::Scope* s, SynTree* st)
{
    Q_ASSERT( st->d_tok.d_type == SynTree::R_statement );
    if( st->d_children.isEmpty() )
        return 0;

    SynTree* stmt = st->d_children.first();
    switch( stmt->d_tok.d_type )
    {
    case SynTree::R_assignmentOrProcedureCall:
        return assignmentOrProcedureCall(s,stmt);
    case SynTree::R_IfStatement:
        return ifStatement(s,stmt);
    case SynTree::R_CaseStatement:
        return caseStatement(s,stmt);
    case SynTree::R_WhileStatement:
        return whileStatement(s,stmt);
    case SynTree::R_RepeatStatement:
        return repeatStatement(s,stmt);
    case SynTree::R_ForStatement:
        return forStatement(s,stmt);
    default:
        Q_ASSERT(false);
    }

    return 0;
}

Ast::Ref<Ast::Statement> Ast::Model::assignmentOrProcedureCall(Ast::Scope* s, SynTree* st)
{
    Q_ASSERT( !st->d_children.isEmpty() );
    Ref<Expression> d = designator(s,st->d_children.first() );
    if( d.isNull() )
        return 0;
    if( st->d_children.size() == 1 )
    {
        // procedure call

        if( !d->d_type.isNull() && d->d_type->derefed()->getTag() == Thing::T_ProcType &&
                d->getTag() != Thing::T_CallExpr )
        {
            ProcType* p = Ast::thing_cast<ProcType*>(d->d_type->derefed());
            if( !p->d_formals.isEmpty() )
                error( st, tr("cannot call procedure without actual arguments") );
            Ref<CallExpr> call = new CallExpr();
            call->d_loc = toRowCol(st);
            call->d_sub = d.data();
            d = call.data();
        }

        if( d->getTag() == Thing::T_CallExpr )
        {
            CallExpr* e = Ast::thing_cast<CallExpr*>(d.data());
            Q_ASSERT( !e->d_sub.isNull() && !e->d_sub->d_type.isNull() &&
                      e->d_sub->d_type->derefed()->getTag() == Thing::T_ProcType );
            ProcType* p = Ast::thing_cast<ProcType*>(e->d_sub->d_type->derefed());
            if( !p->d_return.isNull() )
                error(st, tr("cannot use a function in a procedure call"));
            Ref<Call> c = new Call();
            c->d_what = d;
            return c.data();
        }else
            error(st,tr("expecting procedure") );
    }else if( st->d_children.size() == 3 )
    {
        // assignment
        if( d->getTag() == Thing::T_CallExpr )
            error( st->d_children.first(), tr("left side of assignment cannot be a procedure call") );
        Ref<Expression> rhs = expression(s,st->d_children.last());
        Ref<Assign> a = new Assign();
        a->d_loc = toRowCol(st);
        a->d_lhs = d;
        a->d_rhs = rhs;
        return a.data();
        // TODO check
        // PROCEDURE: If a procedure P is assigned to
        // a procedure variable of type T, the (types of the) formal parameters of P must be the same as those
        // indicated in the formal parameters of T. The same holds for the result type in the case of a function
        // procedure (see 10.1). P must not be declared local to another procedure, and neither can it be a
        // standard procedure.
    }else
        Q_ASSERT( false );

    return 0;
}

Ast::Ref<Ast::Statement> Ast::Model::ifStatement(Ast::Scope* s, SynTree* st)
{
    Q_ASSERT( st->d_tok.d_type == SynTree::R_IfStatement && st->d_children.size() >= 5 );
    Ref<IfLoop> c = new IfLoop();
    c->d_op = IfLoop::IF;
    c->d_loc = toRowCol(st);
    c->d_if << expression(s, st->d_children[1]);
    StatSeq seq;
    statementSequence(s,seq,st->d_children[3]);
    c->d_then.append(seq);
    for( int i = 4; i < st->d_children.size(); i++ )
    {
        SynTree* sub = st->d_children[i];
        switch( sub->d_tok.d_type )
        {
        case SynTree::R_ElsifStatement:
            Q_ASSERT( sub->d_children.size() == 4 );
            c->d_if << expression(s, sub->d_children[1]);
            statementSequence(s,seq,sub->d_children[3]);
            c->d_then.append(seq);
            break;
        case SynTree::R_ElseStatement:
            Q_ASSERT( sub->d_children.size() == 2 );
            statementSequence(s,seq,sub->d_children.last());
            c->d_else = seq;
            break;
        case Tok_END:
            break;
        default:
            Q_ASSERT( false );
            break;
        }
    }
    return c.data();
}

Ast::Ref<Ast::Statement> Ast::Model::whileStatement(Ast::Scope* s, SynTree* st)
{
    Q_ASSERT( st->d_tok.d_type == SynTree::R_WhileStatement && st->d_children.size() >= 5 );
    Ref<IfLoop> c = new IfLoop();
    c->d_op = IfLoop::WHILE;
    c->d_loc = toRowCol(st);
    c->d_if << expression(s, st->d_children[1]);
    StatSeq seq;
    statementSequence(s,seq,st->d_children[3]);
    c->d_then.append(seq);
    for( int i = 4; i < st->d_children.size(); i++ )
    {
        SynTree* sub = st->d_children[i];
        switch( sub->d_tok.d_type )
        {
        case SynTree::R_ElsifStatement2:
            Q_ASSERT( sub->d_children.size() == 4 );
            c->d_if << expression(s, sub->d_children[1]);
            statementSequence(s,seq,sub->d_children[3]);
            c->d_then.append(seq);
            break;
        case Tok_END:
            break;
        default:
            Q_ASSERT( false );
            break;
        }
    }
    return c.data();
}

Ast::Ref<Ast::Statement> Ast::Model::repeatStatement(Ast::Scope* s, SynTree* st)
{
    Q_ASSERT( st->d_tok.d_type == SynTree::R_RepeatStatement && st->d_children.size() == 4 );
    Ref<IfLoop> c = new IfLoop();
    c->d_op = IfLoop::REPEAT;
    c->d_loc = toRowCol(st);
    c->d_if << expression(s, st->d_children[3]);
    StatSeq seq;
    statementSequence(s,seq,st->d_children[1]);
    c->d_then.append(seq);
    return c.data();
}

Ast::Ref<Ast::Statement> Ast::Model::forStatement(Ast::Scope* s, SynTree* st)
{
    Q_ASSERT( st->d_tok.d_type == SynTree::R_ForStatement && st->d_children.size() >= 9 );

    Ref<ForLoop> f = new ForLoop();
    f->d_loc = toRowCol(st);
    Q_ASSERT( st->d_children[1]->d_tok.d_type == Tok_ident );
    Named* id = s->find(st->d_children[1]->d_tok.d_val );
    if( id == 0 )
        error(st->d_children[1],tr("identifier not declared") );
    else
    {
        extendLiveRange( s, id, st->d_children[1] );
        f->d_id = new IdentLeaf( id, st->d_children[1], d_curModule, id->d_type.data() );
        if( d_fillXref )
            d_xref[id] << f->d_id.data();
    }
    f->d_from = expression(s, st->d_children[3]);
    f->d_to = expression(s, st->d_children[5]);
    if( st->d_children[6]->d_tok.d_type == Tok_BY )
    {
        f->d_by = expression(s, st->d_children[7]);
        QString err;
        f->d_byVal = Eval::evalConstExpr( f->d_by.data(), &err );
        if( !f->d_byVal.isValid() )
            error(st->d_children[7],err);
    }else
    {
        f->d_by = new Literal(d_intType.data(), toRowCol(st), 1);
        f->d_byVal = 1;
        f->d_loc = toRowCol(st);
    }

    SynTree* ss = findFirstChild( st, SynTree::R_StatementSequence, 6 );
    Q_ASSERT( ss != 0 );
    statementSequence(s,f->d_do,ss);
    return f.data();
}

Ast::Ref<Ast::Statement> Ast::Model::caseStatement(Ast::Scope* s, SynTree* st)
{
    Q_ASSERT( st->d_tok.d_type == SynTree::R_CaseStatement && st->d_children.size() >= 5 );

    Ref<Expression> caseExpr = expression(s,st->d_children[1]);
    if( caseExpr.isNull() || caseExpr->d_type.isNull() )
        return 0;

    Named* caseId = 0;
    Ref<Type> orig;
    Type* cextd = caseExpr->d_type->derefed();
    if( cextd->getTag() == Thing::T_Pointer || cextd->getTag() == Thing::T_Record )
    {
        // type case statement
        caseId = caseExpr->getIdent();
        orig = caseId->d_type;
    } // else
        // normal case statement

    Ref<CaseStmt> cs = new CaseStmt();
    cs->d_loc = toRowCol(st);
    cs->d_exp = caseExpr;
    cs->d_typeCase = caseId != 0;
    for( int i = 3; i < st->d_children.size(); i++ )
    {
        if( st->d_children[i]->d_tok.d_type == SynTree::R_Case )
        {
            SynTree* cst = st->d_children[i];
            if( cst->d_children.isEmpty() )
                continue;
            Q_ASSERT( cst->d_children.size() == 2 ); // : is suppressed
            SynTree* cll = cst->d_children.first();
            CaseStmt::Case c;
            for( int j = 0; j < cll->d_children.size(); j++ )
            {
                Ref<Ast::Expression> lr = labelRange(s, cll->d_children[j] );
                if( !lr.isNull() ) // error already reported
                    c.d_labels << lr;
            }
            if( caseId != 0 )
            {
                if( c.d_labels.size() != 1 || c.d_labels.first()->getIdent() == 0 )
                {
                    error(cll,tr("expecting a qualident case label in a type case statement"));
                    continue;
                }else if( !isSubType( c.d_labels.first()->getIdent()->d_type.data(), orig.data() ) )
                {
                    error(cll,tr("case label must be a subtype of the case variable in a type case statement"));
                    continue;
                }else
                    caseId->d_type = c.d_labels.first()->getIdent()->d_type;
            }
            statementSequence(s, c.d_block, cst->d_children.last() );
            cs->d_cases << c;
        }
    }
    if( caseId != 0 )
        caseId->d_type = orig;
    return cs.data();
}

static inline quint8 relationToBinOp( quint16 t )
{
    switch( t )
    {
    case Tok_Eq:
        return Ast::BinExpr::EQ;
    case Tok_Hash:
        return Ast::BinExpr::NEQ;
    case Tok_Lt:
        return Ast::BinExpr::LT;
    case Tok_Leq:
        return Ast::BinExpr::LEQ;
    case Tok_Gt:
        return Ast::BinExpr::GT;
    case Tok_Geq:
        return Ast::BinExpr::GEQ;
    case Tok_IN:
        return Ast::BinExpr::IN;
    case Tok_IS:
        return Ast::BinExpr::IS;
    default:
        qCritical() << "relationToBinOp missing" << SynTree::rToStr(t);
        Q_ASSERT( false );
    }
    return 0;
}

static inline quint8 addOpToBinOp( quint16 t )
{
    switch( t )
    {
    case Tok_Plus:
        return Ast::BinExpr::ADD;
    case Tok_Minus:
        return Ast::BinExpr::SUB;
    case Tok_OR:
        return Ast::BinExpr::OR;
    default:
        Q_ASSERT( false );
    }
    return 0;
}

static inline quint8 mulOpToBinOp( quint16 t )
{
    switch( t )
    {
    case Tok_Star:
        return Ast::BinExpr::MUL;
    case Tok_Slash:
        return Ast::BinExpr::FDIV;
    case Tok_DIV:
        return Ast::BinExpr::DIV;
    case Tok_MOD:
        return Ast::BinExpr::MOD;
    case Tok_Amp:
        return Ast::BinExpr::AND;
    default:
        Q_ASSERT( false );
    }
    return 0;
}

Ast::Ref<Ast::Expression> Ast::Model::expression(Ast::Scope* s, SynTree* st)
{
    Q_ASSERT( st->d_tok.d_type == SynTree::R_expression );
    Q_ASSERT( st->d_children.size() == 1 || st->d_children.size() == 3 );
    Ref<Expression> lhs = simpleExpression( s, st->d_children.first() );
    if( st->d_children.size() == 3 )
    {
        Ref<Expression> rhs = simpleExpression( s, st->d_children.last() );
        Ref<BinExpr> res = new BinExpr();
        res->d_lhs = lhs;
        res->d_rhs = rhs;
        Q_ASSERT(st->d_children[1]->d_tok.d_type == SynTree::R_relation && !st->d_children[1]->d_children.isEmpty() );
        res->d_op = relationToBinOp(st->d_children[1]->d_children.first()->d_tok.d_type);
        res->d_loc = toRowCol(st->d_children[1]->d_children.first());
        res->d_type = d_boolType.data();
        lhs = res.data();
    }
    return lhs;
}

Ast::Ref<Ast::Expression> Ast::Model::simpleExpression(Ast::Scope* s, SynTree* st)
{
    Q_ASSERT( st->d_tok.d_type == SynTree::R_SimpleExpression && !st->d_children.isEmpty() );

    bool minus = false;
    int i = 0;
    if( st->d_children.first()->d_tok.d_type == Tok_Plus )
        i++;
    else if( st->d_children.first()->d_tok.d_type == Tok_Minus )
    {
        minus = true;
        i++;
    }
    Q_ASSERT( i < st->d_children.size() );

    // a * b * c * d
    // ( ( ( a * b ) * c ) * d )
    Ref<Expression> lhs = term( s, st->d_children[i++] );
    if( lhs.isNull() )
        return 0;

    if( minus ) // minus associates with the first lhs! -(5 + 6) = 11
    {
        // TODO: simplify if const
        Ref<UnExpr> u = new UnExpr();
        u->d_op = UnExpr::NEG;
        u->d_loc = toRowCol(st->d_children.first());
        u->d_sub = lhs;
        u->d_type = lhs->d_type;
        lhs = u.data();
    }

    if( i < st->d_children.size() )
    {
        // TODO: simplify if const

        Q_ASSERT( ( st->d_children.size() - i ) % 2 == 0 );
        for( ; i < st->d_children.size(); i += 2 )
        {
            Ref<Expression> rhs = term( s, st->d_children[i+1] );
            Ref<BinExpr> res = new BinExpr();
            res->d_lhs = lhs;
            res->d_rhs = rhs;
            Q_ASSERT( st->d_children[i]->d_tok.d_type == SynTree::R_AddOperator && !st->d_children[i]->d_children.isEmpty() );
            res->d_op = addOpToBinOp( st->d_children[i]->d_children.first()->d_tok.d_type );
            res->d_loc = toRowCol(st->d_children[i]->d_children.first());
            if( !rhs.isNull() )
                res->d_type = rhs->d_type;
            lhs = res.data();
        }
    }

    return lhs;
}

Ast::Ref<Ast::Expression> Ast::Model::term(Ast::Scope* s, SynTree* st)
{
    Q_ASSERT( st->d_tok.d_type == SynTree::R_term && !st->d_children.isEmpty() );
    Ref<Expression> lhs = factor( s, st->d_children.first() );
    if( st->d_children.size() > 1 )
    {
        // TODO: simplify if const

        Q_ASSERT( ( st->d_children.size() - 1 ) % 2 == 0 );
        for( int i = 1; i < st->d_children.size(); i += 2 )
        {
            Ref<Expression> rhs = factor( s, st->d_children[i+1] );
            Ref<BinExpr> res = new BinExpr();
            res->d_lhs = lhs;
            res->d_rhs = rhs;
            Q_ASSERT( st->d_children[i]->d_tok.d_type == SynTree::R_MulOperator &&
                      !st->d_children[i]->d_children.isEmpty() );
            res->d_op = mulOpToBinOp( st->d_children[i]->d_children.first()->d_tok.d_type );
            res->d_loc = toRowCol(st->d_children[i]->d_children.first());
            if( !rhs.isNull() )
                res->d_type = rhs->d_type;
            lhs = res.data();
        }
    }
    return lhs;
}

Ast::Ref<Ast::Expression> Ast::Model::factor(Ast::Scope* s, SynTree* st)
{
    Q_ASSERT( st->d_tok.d_type == SynTree::R_factor && !st->d_children.isEmpty() );

    SynTree* first = st->d_children.first();
    switch( first->d_tok.d_type )
    {
    case SynTree::R_literal:
        return literal(s,st->d_children[0]);
    case Tok_Lpar:
        return expression(s,st->d_children[1]);
    case Tok_Tilde:
        {
            Ref<Expression> f = factor(s,st->d_children[1]);
            // TODO: simplify if const
            if( f.isNull() )
                return 0;
            Ref<Expression> res = new UnExpr(UnExpr::NOT,f.data() );
            res->d_loc = toRowCol(first);
            res->d_type = f->d_type;
            return res;
        }
        break;
    case SynTree::R_variableOrFunctionCall:
        Q_ASSERT( !st->d_children.first()->d_children.isEmpty() );
        return designator(s,st->d_children.first()->d_children.first());
    default:
        Q_ASSERT(false);
    }
    return 0;
}

Ast::Ref<Ast::Expression> Ast::Model::literal(Ast::Scope* s, SynTree* st)
{
    Q_ASSERT( st->d_tok.d_type == SynTree::R_literal && !st->d_children.isEmpty() );

    QVariant val;
    SynTree* first = st->d_children.first();
    switch( first->d_tok.d_type )
    {
    case SynTree::R_number:
        Q_ASSERT( !st->d_children.first()->d_children.isEmpty() );
        switch( st->d_children.first()->d_children.first()->d_tok.d_type)
        {
        case Tok_real:
            val = first->d_children.first()->d_tok.d_val.toDouble();
            return new Literal(d_realType.data(), toRowCol(first->d_children.first()),val);
        case Tok_integer:
            if( first->d_children.first()->d_tok.d_val.endsWith('H') )
                val = first->d_children.first()->d_tok.d_val.
                                           left(first->d_children.first()->d_tok.d_val.size()-1).toLongLong(0,16);
            else
                val = first->d_children.first()->d_tok.d_val.toLongLong();
            return new Literal(d_intType.data(), toRowCol(first->d_children.first()),val);
        default:
            Q_ASSERT(false);
            break;
        }
        break;
    case Tok_string:
        return new Literal(d_stringType.data(), toRowCol(first),first->d_tok.d_val.mid(1,first->d_tok.d_val.size()-2));
    case Tok_hexstring:
        return new Literal(d_stringType.data(), toRowCol(first), QByteArray::fromHex(
                               first->d_tok.d_val.mid(1, first->d_tok.d_val.size() - 2)));
    case Tok_hexchar:
        return new Literal(d_charType.data(), toRowCol(first), QByteArray::fromHex(
                               first->d_tok.d_val.left( first->d_tok.d_val.size() - 1 ) ));
    case Tok_NIL:
        return new Literal(d_nilType.data(), toRowCol(first));
    case Tok_TRUE:
        return new Literal(d_boolType.data(), toRowCol(first), true);
    case Tok_FALSE:
        return new Literal(d_boolType.data(), toRowCol(first), false);
    case SynTree::R_set:
        return set(s,st->d_children.first());
    default:
        Q_ASSERT(false);
    }
    return 0;
}

Ast::Ref<Ast::Expression> Ast::Model::designator(Ast::Scope* s, SynTree* st)
{
    Q_ASSERT( st->d_tok.d_type == SynTree::R_designator && !st->d_children.isEmpty() );
    Q_ASSERT( st->d_children.first()->d_tok.d_type == SynTree::R_qualident );

    SynTree* quali = st->d_children.first();

    Ref<Expression> cur;
    Type* type = 0;
    Module* sourceMod = d_curModule;

    bool imported = false;

    // deref first ident of quali
    {
        Named* n = s->find(quali->d_children.first()->d_tok.d_val);
        if( n == 0 )
        {
            error(quali->d_children.first(),tr("cannot resolve identifier") );
            return 0;
        }
        Ref<IdentLeaf> id = new IdentLeaf();
        id->d_ident = n;
        id->d_loc = toRowCol(quali->d_children.first());
        id->d_mod = d_curModule;
        cur = id.data();
        extendLiveRange( s, n, quali->d_children.first() );
        if( d_fillXref )
            d_xref[n].append(id.data());
        if( n->getTag() == Thing::T_Import )
        {
            imported = true;
            type = d_nilType.data(); // Just a trick to avoid regular zero type
            Import* i = Ast::thing_cast<Import*>(n);
            sourceMod = i->d_mod.data();
        }else
            type = n->d_type.data();
        cur->d_type = type;
        if( type == 0 )
            return 0; // reaction to a previous error
    }

    QList<SynTree*> desig = st->d_children.mid(1);
    SynTree tmp;
    if( quali->d_children.size() == 2 )
    {
        // delegate handling of the quali to the regular selector handler below with this trick
        tmp.d_tok = quali->d_tok;
        tmp.d_tok.d_type = SynTree::R_selector;
        tmp.d_children.append( new SynTree( quali->d_children.last()->d_tok ) );
        desig.prepend(&tmp);
    }

    for(int i = 0; i < desig.size(); i++ )
    {
        Type* td = type->derefed();
        SynTree* sel = desig[i];
        Q_ASSERT( sel->d_tok.d_type == SynTree::R_selector && !sel->d_children.isEmpty() );
        SynTree* first = sel->d_children.first();
        switch( first->d_tok.d_type )
        {
        case Tok_ident: // instead of dot
            if( i == 0 && imported )
            {
                Q_ASSERT( cur->getIdent() != 0 && cur->getIdent()->getTag() == Thing::T_Import );
                Import* imp = Ast::thing_cast<Import*>( cur->getIdent() );
                Named* v = imp->d_mod->find(first->d_tok.d_val, false );
                if( v == 0 )
                {
                    error(first,tr("cannot resolve identifier in '%1'").arg(imp->d_mod->d_name.constData() ) );
                    return 0;
                }
                Ref<IdentSel> id = new IdentSel();
                id->d_sub = cur.data();
                id->d_ident = v;
                id->d_loc = toRowCol(first);
                cur = id.data();
                type = v->d_type.data();
                cur->d_type = type;
                if( d_fillXref )
                    d_xref[v].append(id.data());
                if( type == 0 )
                    return 0;
                if( sourceMod != d_curModule && !sourceMod->d_isDef && !( v->d_public || v->d_isDef ) )
                    error(first,tr("element is not public") );
                break; // jump out here to avoid interference with pointer and record idents below
            }
            if( td->getTag() == Thing::T_Pointer )
            {
                // the dot implies dereferencing and p.f stands for p^.f.
                Ref<UnExpr> deref = new UnExpr();
                deref->d_op = UnExpr::DEREF;
                deref->d_sub = cur.data();
                deref->d_loc = toRowCol(first);
                cur = deref.data();
                Pointer* p = Ast::thing_cast<Pointer*>(td);
                type = p->d_to.data();
                cur->d_type = type;
                if( type == 0 )
                    return 0;
                td = type->derefed();
            }
            // this is intentionally not elsif so derefed pointer can fall through to record
            if( td->getTag() == Thing::T_Record )
            {
                Record* r = Ast::thing_cast<Record*>(td);
                Named* f = r->find(first->d_tok.d_val, true);
                if( f == 0 )
                {
                    error(first,tr("record field doesn't exist") );
                    return 0;
                }
                if( sourceMod != d_curModule && !sourceMod->d_isDef && !( f->d_public || f->d_isDef ) )
                    error(first,tr("element is not public") );

                Ref<IdentSel> id = new IdentSel();
                id->d_sub = cur.data();
                id->d_ident = f;
                id->d_loc = toRowCol(first);
                cur = id.data();
                type = f->d_type.data();
                cur->d_type = type;
                if( d_fillXref )
                    d_xref[f].append(id.data());
                if( type == 0 )
                    return 0;
            }else
            {
                error(first,tr("the designated object is not a record") );
                return 0;
            }
            break;
        case Tok_Lbrack:
            {
                Q_ASSERT( sel->d_children.size() == 3 );
                CallExpr::Actuals args;
                QList<SynTree*> argsSt;
                SynTree* el = sel->d_children[1];
                Q_ASSERT( el->d_tok.d_type == SynTree::R_ExpList && !el->d_children.isEmpty() );
                foreach( SynTree* e, el->d_children )
                {
                    Ref<Expression> ex = expression(s,e);
                    if( ex.isNull() )
                        continue;
                    args << ex;
                    argsSt << e;
                }
                for( int j = 0; j < args.size() ; j++ )
                {
                    if( td->getTag() == Thing::T_Array )
                    {
                        Array* a = Ast::thing_cast<Array*>(td);
                        Ref<BinExpr> ex = new BinExpr();
                        ex->d_op = BinExpr::Index;
                        ex->d_loc = toRowCol(argsSt[j]);
                        ex->d_rhs = args[j];
                        ex->d_lhs = cur.data();
                        cur = ex.data();
                        type = a->d_type.data();
                        cur->d_type = type;
                        if( type == 0 )
                            return 0;
                        td = type->derefed();
                    }else
                    {
                        error(sel,tr("this expression cannot be indexed") );
                        return 0;
                    }
                }
            }
            break;
        case Tok_Hat:
            if( td->getTag() == Thing::T_Pointer )
            {
                Ref<UnExpr> deref = new UnExpr();
                deref->d_op = UnExpr::DEREF;
                deref->d_sub = cur.data();
                deref->d_loc = toRowCol(first);
                cur = deref.data();
                Pointer* p = Ast::thing_cast<Pointer*>(td);
                type = p->d_to.data();
                cur->d_type = type;
                if( type == 0 )
                    return 0;
            }else
            {
                error(first,tr("can only dereference pointer types") );
                return 0;
            }
            break;
        case Tok_Lpar:
            {
                Q_ASSERT( sel->d_children.size() == 2 || sel->d_children.size() == 3 );
                CallExpr::Actuals args;
                QList<SynTree*> sts;
                if( sel->d_children.size() == 3 )
                {
                    // (ExpList)
                    SynTree* el = sel->d_children[1];
                    Q_ASSERT( el->d_tok.d_type == SynTree::R_ExpList && !el->d_children.isEmpty() );
                    foreach( SynTree* e, el->d_children )
                    {
                        Ref<Expression> res = expression(s,e);
                        if( res.isNull() )
                            return 0;
                        args << res;
                        sts << e;
                    }
                }// else () no args
                Named* id = args.isEmpty() ? 0 : args.first()->getIdent();
                if( args.size() == 1 && flatten(sts.first(), SynTree::R_qualident)->d_tok.d_type == SynTree::R_qualident
                        && id != 0 && id->getTag() == Thing::T_NamedType )
                {
                    // Type guard
                    Ref<UnExpr> tg = new UnExpr();
                    tg->d_op = UnExpr::CAST;
                    tg->d_sub = cur.data();
                    tg->d_loc = toRowCol(sts.first());
                    cur = tg.data();
                    type = id->d_type.data();
                    cur->d_type = type;
                    if( type == 0 )
                        return 0;
                }else if( td->getTag() == Thing::T_ProcType )
                {
                    Ref<CallExpr> call = new CallExpr();
                    call->d_actuals = args;
                    call->d_sub = cur.data();
                    call->d_loc = toRowCol(first);
                    ProcType* p = Ast::thing_cast<ProcType*>(td);
                    if( cur->getIdent() && cur->getIdent()->getTag() == Thing::T_BuiltIn )
                    {
                        BuiltIn* bi = Ast::thing_cast<BuiltIn*>(cur->getIdent());
                        if( ( bi->d_func == BuiltIn::VAL || bi->d_func == BuiltIn::ABS ) && !args.isEmpty() )
                            type = args.first()->d_type.data();
                        else
                            type = p->d_return.data();
                    }else
                        type = p->d_return.data();
                    cur = call.data();
                    cur->d_type = type;
                    if( type == 0 && i != desig.size() - 1 )
                    {
                        error(sel,tr("procedure has no return value"));
                        return 0;
                    }
                }else
                {
                    error(sel,tr("this expression cannot be called") );
                    return 0;
                }
            }
            break;
        default:
            Q_ASSERT(false);
            break;
        }
    }

    return cur.data();
}

Ast::Ref<Ast::Expression> Ast::Model::qualident(Ast::Scope* s, SynTree* quali)
{
    Q_ASSERT( quali->d_tok.d_type == SynTree::R_qualident &&
              !quali->d_children.isEmpty() && quali->d_children.size() <= 2 );

    Ref<Expression> cur;
    Module* sourceMod = d_curModule;
    Import* imp = 0;
    Named* ident = 0;

    // deref first ident of quali (nearly identical to designator)
    {
        ident = s->find(quali->d_children.first()->d_tok.d_val);
        if( ident == 0 )
        {
            error(quali->d_children.first(),tr("cannot resolve identifier") );
            return 0;
        }
        Ref<IdentLeaf> id = new IdentLeaf();
        id->d_ident = ident;
        id->d_loc = toRowCol(quali->d_children.first());
        id->d_mod = d_curModule;
        cur = id.data();
        extendLiveRange( s, ident, quali->d_children.first() );
        if( d_fillXref )
            d_xref[ident].append(id.data());
        if( ident->getTag() == Thing::T_Import )
        {
            imp = Ast::thing_cast<Import*>(ident);
            sourceMod = imp->d_mod.data();
        }
        cur->d_type = ident->d_type.data();
    }

    if( quali->d_children.size() == 2 )
    {
        // similar to ident i==0 in designator
        if( imp == 0 )
        {
            error(quali,tr("qualident doesn't reference an imported module") );
            return 0;
        }
        Q_ASSERT( imp != 0 );
        SynTree* st = quali->d_children.last();
        ident = imp->d_mod->find(st->d_tok.d_val, false );
        if( ident == 0 )
        {
            error(st,tr("cannot resolve identifier in '%1'").arg(imp->d_mod->d_name.constData() ) );
            return 0;
        }
        Ref<IdentSel> id = new IdentSel();
        id->d_sub = cur.data();
        id->d_ident = ident;
        id->d_loc = toRowCol(st);
        cur = id.data();
        cur->d_type = ident->d_type.data();
        if( d_fillXref )
            d_xref[ident].append(id.data());
        if( sourceMod != d_curModule && !sourceMod->d_isDef && !( ident->d_public || ident->d_isDef ) )
            error(st,tr("element is not public") );
    }

    Q_ASSERT( ident != 0 );
    Q_ASSERT( ident->d_type == cur->d_type.data() );

    return cur.data();
}

Ast::Ref<Ast::Expression> Ast::Model::set(Ast::Scope* s, SynTree* st)
{
    Q_ASSERT( st->d_tok.d_type == SynTree::R_set );
    Ref<SetExpr> set = new SetExpr();
    set->d_loc = toRowCol(st);
    set->d_type = d_setType.data();
    for( int i = 1; i < st->d_children.size(); i++ )
    {
        SynTree* elem = st->d_children[i];
        if( elem->d_tok.d_type == SynTree::R_element )
        {
            Q_ASSERT( !elem->d_children.isEmpty() );
            Ref<Expression> lhs = expression(s, elem->d_children.first());
            if( elem->d_children.size() == 3 )
            {
                Ref<Expression> rhs = expression(s, elem->d_children.last());
                Ref<BinExpr> range = new BinExpr();
                range->d_op = BinExpr::Range;
                range->d_loc = toRowCol(elem->d_children[1]);
                if( !rhs.isNull() )
                    range->d_type = rhs->d_type;
                range->d_lhs = lhs;
                range->d_rhs = rhs;
                set->d_parts << range.data();
            }else
                set->d_parts << lhs.data();
        }
    }
    return set.data();
}

Ast::Ref<Ast::Expression> Ast::Model::labelRange(Ast::Scope* s, SynTree* st)
{
    Q_ASSERT( st->d_tok.d_type == SynTree::R_LabelRange && !st->d_children.isEmpty() );
    Ref<Expression> lhs = label(s,st->d_children.first());
    if( !lhs.isNull() && st->d_children.size() > 1 )
    {
        Q_ASSERT( st->d_children.size() == 3 );
        Ref<Expression> rhs = label(s,st->d_children.last());
        Ref<BinExpr> bin = new BinExpr();
        bin->d_op = BinExpr::Range;
        bin->d_loc = toRowCol(st->d_children[1]);
        bin->d_lhs = lhs;
        bin->d_rhs = rhs;
        if( !rhs.isNull() )
            bin->d_type = rhs->d_type;
        lhs = bin.data();
    }
    return lhs;
}

Ast::Ref<Ast::Expression> Ast::Model::label(Ast::Scope* s, SynTree* st)
{
    Q_ASSERT( st->d_tok.d_type == SynTree::R_label && !st->d_children.isEmpty() );

    QVariant val;
    SynTree* first = st->d_children.first();
    switch( first->d_tok.d_type )
    {
    case Tok_integer:
        if( first->d_tok.d_val.endsWith('H') )
            val = first->d_tok.d_val.left(first->d_tok.d_val.size()-1).toLongLong(0,16);
        else
            val = first->d_tok.d_val.toLongLong();
        return new Literal(d_intType.data(),toRowCol(first),val);
        break;
    case Tok_string:
        return new Literal(d_stringType.data(),toRowCol(first),first->d_tok.d_val.mid(1,first->d_tok.d_val.size()-2));
    case Tok_hexstring:
        return new Literal(d_stringType.data(),toRowCol(first),QByteArray::fromHex(
                               first->d_tok.d_val.mid(1, first->d_tok.d_val.size() - 2)));
    case Tok_hexchar:
        return new Literal(d_charType.data(),toRowCol(first), QByteArray::fromHex(
                               first->d_tok.d_val.left( first->d_tok.d_val.size() - 1 ) ));
    case SynTree::R_qualident:
        {
            return qualident( s, first ).data();
        }
        break;
    default:
        Q_ASSERT(false);
    }
    return 0;
}

bool Ast::Model::error(SynTree* st, const QString& msg) const
{
    d_errs->error(Errors::Semantics, st->d_tok.toLoc(), msg );
    return false;
}

static QString moduleName( Ast::Named* n )
{
    if( n == 0 )
        return QString();
    if( n->getTag() == Ast::Thing::T_Module )
        return n->d_name;
    else
        return moduleName(n->d_scope);
}

bool Ast::Model::error(Ast::Named* n, const QString& str) const
{
    // it's ok to use d_curModule because function is only used for local module symbols
    d_errs->error(Errors::Semantics, d_curModule ? d_curModule->d_file : moduleName(n),
                  n->d_loc.d_row, n->d_loc.d_col, str );
    return false;
}

void Ast::Model::unbindFromGlobal()
{
    if( d_global.isNull() )
        return;
    Scope::Names::const_iterator i;
    for( i = d_global->d_names.begin(); i != d_global->d_names.end(); ++i )
    {
        if( i.value()->isNamed() )
            i.value().data()->d_scope = 0; // if Modules continue to live they don't have dangling ptr
    }
}

const QList<SynTree*> Ast::Model::getImports(const SynTree* st)
{
    QList<SynTree*> res;
    SynTree* il = findFirstChild(st,SynTree::R_ImportList);
    if( il )
    {
        foreach( SynTree* i, il->d_children )
        {
            if( i->d_tok.d_type == SynTree::R_import )
            {
                Q_ASSERT( !i->d_children.isEmpty() && i->d_children.first()->d_tok.d_type == Tok_ident );
                if( i->d_children.size() > 1 )
                {
                    Q_ASSERT( i->d_children[1]->d_tok.d_type == Tok_ColonEq &&
                            i->d_children[2]->d_tok.d_type == Tok_ident );
                    res << i->d_children[2];
                }else
                    res << i->d_children.first();
            }
        }
    }
    return res;
}

void Ast::Model::createModules(Mods& mods, ParseResultList& prl)
{
    ParseResultList::iterator i;
    for( i = prl.begin(); i != prl.end(); ++i )
    {
        ParseResult& pr = i.value();
        SynTree* modRoot = pr.d_modRoot;
        Ref<Module> m = new Module();
        if( m->d_useExt )
            m->d_scope = d_globalLc.data();
        else
            m->d_scope = d_global.data();
        if( d_fillXref )
        {
            m->d_helper << new IdentLeaf(m.data(),pr.d_modName,m.data(), 0);
            d_xref[m.data()].append( m->d_helper.back().data() );
        }
        m->d_name = pr.d_modName->d_tok.d_val;
        m->d_loc = toRowCol(pr.d_modName);
        m->d_file = pr.d_modName->d_tok.d_sourcePath;
        m->d_useExt = pr.d_isExt;
        m->d_isDef = modRoot->d_tok.d_type == SynTree::R_definition;

        if( m->d_scope->d_names.contains(pr.d_modName->d_tok.d_val.constData()) )
            error(modRoot,tr("invalid module name '%1'").arg(pr.d_modName->d_tok.d_val.constData()));
        else
        {
            Usage& u = mods[pr.d_modName->d_tok.d_val.constData()];
            Q_ASSERT( u.d_st == 0 );
            u.d_st = modRoot;
            pr.d_modRoot = 0; // avoid deleting modRoot
        }

        if( !d_global->d_names.contains(pr.d_modName->d_tok.d_val.constData()) )
            d_global->d_names.insert(pr.d_modName->d_tok.d_val.constData(), m.data());
        if( !d_globalLc->d_names.contains(pr.d_modName->d_tok.d_val.constData()) )
            d_globalLc->d_names.insert(pr.d_modName->d_tok.d_val.constData(), m.data());

        const QList<SynTree*> imports = getImports(modRoot);
        foreach( SynTree* imp, imports )
        {
            if( imp->d_tok.d_val.constData() == d_system.constData() )
            {
                // NOP, importing SYSTEM
                if( false ) // d_fillXref )
                {
                    // not needed, leads to double entries
                    Named* system = d_global->d_names.value( imp->d_tok.d_val.constData() ).data();
                    d_xref[system].append( m->d_helper.back().data() );
                }
            }else if( !mods.contains(imp->d_tok.d_val.constData()) && !prl.contains(imp->d_tok.d_val.constData()) )
            {
                // the required import is not in the parsed files, either because it references a
                // library outside of the parsed files or a parsed file is missing due to syntax errors
                if( Named* test = d_global->d_names.value( imp->d_tok.d_val.constData() ).data() )
                {
                    if( !test->isScope() )
                        error(imp,tr("import is not a module '%1'").arg(imp->d_tok.d_val.constData()));
                }else if( !resolveImport( mods, imp->d_tok.d_val ) )
                    error(imp,tr("cannot resolve import '%1'").arg(imp->d_tok.d_val.constData()));
            }
            mods[ m->d_name.constData() ].d_uses.insert( imp->d_tok.d_val.constData() );
            mods[ imp->d_tok.d_val.constData() ].d_usedBy.insert( m->d_name.constData() );
        }
    }
}

bool Ast::Model::resolveImport(Mods& mods, const QByteArray& imp)
{
    ParseResultList pr;
    if( !parseFile( imp, pr ) )
        return false;
    if( pr.isEmpty() )
        return false;
    else
        createModules(mods,pr);
    return true;
}

QList<Ast::Module*> Ast::Model::findProcessingOrder(Mods& in)
{
    // Mods nicht const da sonst COW eine neue Kopie macht die SynTree löscht
    QList<const char*> order;

    QSet<const char*> mods = in.keys().toSet();

    QSet<const char*> used;
    foreach( const char* m, mods )
    {
        // Find all leafs
        if( in[m].d_uses.isEmpty() )
        {
            order.append(m);
            used.insert(m);
        }
    }
    mods -= used;

    while( !mods.isEmpty() )
    {
        foreach( const char* m, mods )
        {
            bool allUsed = true;
            foreach( const char* o, in[m].d_uses )
            {
                if( !used.contains(o) )
                {
                    allUsed = false;
                    break;
                }
            }
            if( allUsed )
            {
                used.insert(m);
                order.append(m);
            }
        }
        const int count = mods.size();
        mods -= used;
        if( count == mods.size() )
            break;
    }
    d_depOrder.clear();
    if( !mods.isEmpty() )
    {
        foreach( const char* m, mods )
        {
            Named* mod = d_global->d_names.value(m).data();
            if( mod != 0 )
                error(mod, tr("module '%1' has circular import dependencies").arg(m) );
        }
        return d_depOrder;
    }

    foreach( const char* m, order )
    {
        Named* mod = d_global->d_names.value(m).data();
        if( mod && mod->isScope() )
            d_depOrder.append( Ast::thing_cast<Module*>(mod) );
    }

    return d_depOrder;
}

void Ast::Model::fixTypes()
{
    if( d_fixType.isEmpty() )
        return;

    foreach( const FixType& ft, d_fixType )
    {
        ft.d_ptr->d_to = namedType(ft.d_scope,0,ft.d_st);
        // NOTE: sync with pointerType
        if( ft.d_ptr->d_to.isNull() || ft.d_ptr->d_to->derefed()->getTag() != Thing::T_Record )
            error(ft.d_st,tr("not pointing to a record"));
    }

    d_fixType.clear();
}

Ast::Ref<Ast::QualiType> Ast::Model::getTypeFromQuali(Ast::Scope* s, SynTree* st)
{
    Ref<Expression> e = qualident( s, st );
    if( e.isNull() )
        return 0; // error was already reported
    Named* ident = e->getIdent();
    Q_ASSERT( ident != 0 );
    if( ident->getTag() != Thing::T_NamedType )
        error(st,tr("qualident must reference a named type") );

    Ref<QualiType> q = new QualiType();
    q->d_quali = e;
    q->d_selfRef = e->d_type.isNull() && ident == d_curTypeDecl;

    if( e->d_type.isNull() && !q->d_selfRef )
        error(st,tr("type not available")); // is this an expected outcome?

    return q;
}

bool Ast::Model::checkSelfRefs(Named* n, Ast::Type* t, bool top, bool startsWithPointer, bool inRecord )
{
    if( startsWithPointer )
        return true;

    if( t->getTag() == Thing::T_Pointer )
    {
        // X = POINTER TO X;
        // X = POINTER TO RECORD Y: X; END;
        // X = RECORD Y: POINTER TO X; END;
        Pointer* p = Ast::thing_cast<Pointer*>(t);
        if( p->d_to.isNull() )
            return false;
        if( p->d_to->isSelfRef() )
        {
            if( top )
                return error(n,tr("recursive type declaration") );
            return checkSelfRefs(n,p->d_to.data(), false, top, inRecord );
        }
        return true; // as soon as a pointer is seen selfref is no issue
        // return checkSelfRefs(n,p->d_to.data(), false, startsWithPointer, inRecord, inProc );
    }
    if( t->getTag() == Thing::T_Array )
    {
        // POINTER TO ARRAY illegal
        // X = ARRAY OF POINTER ok
        Array* a = Ast::thing_cast<Array*>(t);
        if( a->d_type.isNull() )
            return false;
        if( a->d_type->isSelfRef() )
        {
            if( top )
                return error(n,tr("recursive type declaration") );
        }
        return checkSelfRefs(n,a->d_type.data(), false, false, inRecord );
    }
    if( t->getTag() == Thing::T_Record )
    {
        Record* r = Ast::thing_cast<Record*>(t);
        for( int i = 0; i < r->d_fields.size(); i++ )
        {
            Field* f = r->d_fields[i].data();
            if( f->d_type.isNull() )
                continue;
            if( f->d_type->isSelfRef() )
                return error(n,tr("recursive type declaration") );
            checkSelfRefs(f,f->d_type.data(),false,false,true);
        }
    }
    // else
    return true;
}

Ast::Record* Ast::Model::toRecord( Ast::Type* t )
{
    // If a type T is an extension of T0 and P is a pointer type bound to T,
    // then P is also an extension of P0

    if( t == 0 )
        return 0;
    if( t->getTag() == Thing::T_QualiType || t->getTag() == Thing::T_TypeRef )
        t = t->derefed();
    Q_ASSERT( t != 0 );
    if( t->getTag() == Ast::Thing::T_Pointer )
        t = Ast::thing_cast<Ast::Pointer*>(t)->d_to.data();
    if( t == 0 )
        return 0;
    if( t->getTag() == Thing::T_QualiType || t->getTag() == Thing::T_TypeRef )
        t = t->derefed();
    Q_ASSERT( t != 0 );
    if( t->getTag() != Ast::Thing::T_Record )
        return 0;
    else
        return Ast::thing_cast<Record*>(t);
}

bool Ast::Model::isXInModule(Ast::Module* mod, Ast::Scope* x)
{
    if( x == 0 )
        return false;
    if( x == mod )
        return true;
    else if( x->d_scope )
        return isXInModule( mod, x->d_scope );
    else
        return false;

}

bool Ast::Model::isSubType(Ast::Type* sub, Ast::Type* super)
{
    sub = toRecord(sub);
    super = toRecord(super);
    if( sub == 0 || super == 0 )
        return false;

    while( sub )
    {
        // A type T extends a type T0, if it equals T0, or if it directly extends an extension of T0
        if( sub == super )
            return true;
        Q_ASSERT( sub->getTag() == Ast::Thing::T_Record );
        sub = toRecord( Ast::thing_cast<Ast::Record*>(sub)->d_base.data() );
    }
    return false;
}

bool Ast::Model::addToScope(Ast::Scope* s, Ast::Named* n)
{
    Q_ASSERT( s != 0 && n != 0 );
    if( s->d_names.contains(n->d_name.constData()) )
        return error(n,tr("name already used '%1'").arg(n->d_name.constData()) );
    // else
    s->d_names[n->d_name.constData()] = n;
    s->d_order.append(n);
    return true;
}

Ast::Model::ParseResult::~ParseResult()
{
    if( d_modRoot )
        delete d_modRoot;
}

Ast::Model::Usage::~Usage()
{
    if( d_st )
        delete d_st;
}

Ast::Named*Ast::Scope::find(const QByteArray& name, bool recursive) const
{
    Names::const_iterator i = d_names.find( name.constData() );
    if( i != d_names.end() )
        return i.value().data();
    if( recursive && d_scope )
        return d_scope->find(name);
    else
        return 0;
}

Ast::Field*Ast::Record::find(const QByteArray& name, bool recursive) const
{
    for( int i = 0; i < d_fields.size(); i++ )
    {
        if( d_fields[i]->d_name.constData() == name.constData() )
            return d_fields[i].data();
    }
    if( recursive )
    {
        Record* r = getBaseRecord();
        if( r )
            return r->find(name,recursive);
    }
    return 0;
}

Ast::Record*Ast::Record::getBaseRecord() const
{
    Q_ASSERT( d_base.isNull() || !d_base->d_quali.isNull() );

    if( d_base.isNull() || d_base->d_quali->d_type.isNull() )
        return 0;

    return Model::toRecord( d_base.data() );
}

Ast::BuiltIn::BuiltIn(quint8 f, ProcType* pt):d_func(f)
{
    d_name = Lexer::getSymbol(s_typeName[f]);
    if( pt )
        d_type = pt;
    else
        d_type = new ProcType();
    Q_ASSERT( d_type->d_ident == 0 );
    d_type->d_ident = this;
}

Ast::ProcType::ProcType(const Ast::Type::List& f, Ast::Type* r):d_return(r)
{
    for( int i = 0; i < f.size(); i++ )
    {
        Ref<Parameter> p = new Parameter();
        p->d_type = f[i];
        d_formals.append(p);
    }
}

Ast::ProcType::ProcType(const Ast::Type::List& f, const Vars& var, Ast::Type* r)
{
    Q_ASSERT( f.size() == var.size() );
    for( int i = 0; i < f.size(); i++ )
    {
        Ref<Parameter> p = new Parameter();
        p->d_type = f[i];
        p->d_var = var[i];
        d_formals.append(p);
    }
}

Ast::Parameter*Ast::ProcType::find(const QByteArray& name) const
{
    if( name.isEmpty() )
        return 0;
    for( int i = 0; i < d_formals.size(); i++ )
    {
        if( d_formals[i]->d_name.constData() == name.constData() )
            return d_formals[i].data();
    }
    return 0;
}

bool Ast::ProcType::isBuiltIn() const
{
    return d_ident && d_ident->getTag() == Thing::T_BuiltIn;
}

Ast::ProcType*Ast::CallExpr::getProcType() const
{
    Q_ASSERT( !d_sub.isNull() && !d_sub->d_type.isNull() && d_sub->d_type->derefed()->getTag() == Thing::T_ProcType );
    return Ast::thing_cast<ProcType*>( d_sub->d_type->derefed() );
}

Ast::CallExpr*Ast::Call::getCallExpr() const
{
    Q_ASSERT( !d_what.isNull() && d_what->getTag() == Thing::T_CallExpr );
    return Ast::thing_cast<CallExpr*>( d_what.data() );
}

Ast::Module* Ast::Named::getModule()
{
    if( getTag() == Thing::T_Module )
        return Ast::thing_cast<Module*>(this);
    else if( d_scope )
        return d_scope->getModule();
    else
        return 0;
}

Ast::QualiType::ModItem Ast::QualiType::getQuali() const
{
    Q_ASSERT( !d_quali.isNull() );
    ModItem res;
    res.second = d_quali->getIdent();

    const int tag = d_quali->getTag();
    switch( tag )
    {
    case Thing::T_IdentLeaf:
        break; // NOP
    case Thing::T_IdentSel:
        {
            IdentSel* i = Ast::thing_cast<IdentSel*>( d_quali.data() );
            Q_ASSERT( !i->d_sub.isNull() && i->d_sub->getTag() == Thing::T_IdentLeaf );
            res.first = i->d_sub->getIdent();
        }
        break;
    default:
        Q_ASSERT( false );
        break;
    }

    return res;
}

Ast::Type*Ast::QualiType::derefed()
{
    Q_ASSERT( !d_quali.isNull() );
    if( d_quali->d_type.isNull() )
        return this; // never return 0 with derefed
    else
        return d_quali->d_type->derefed();
}


Ast::IdentLeaf::IdentLeaf(Ast::Named* id, SynTree* loc, Ast::Module* mod, Ast::Type* t):d_mod(mod),d_ident(id)
{
    d_loc = toRowCol(loc);
    d_type = t;
}

#ifdef _DEBUG

QSet<Ast::Thing*> Ast::Thing::insts;

Ast::Thing::Thing()
{
    insts.insert(this);
}

Ast::Thing::~Thing()
{
    insts.remove(this);
}

#endif
