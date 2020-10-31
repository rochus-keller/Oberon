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

#include "ObCodeModel.h"
#include "ObLexer.h"
#include "ObErrors.h"
#include "ObParser.h"
#include "ObFileCache.h"
#include <QtDebug>
#include <QFile>
#include <QFileInfo>
#include <typeinfo>
#include <QBuffer>
using namespace Ob;

Q_DECLARE_METATYPE( Ob::CodeModel::Set )

static inline const CodeModel::Type* derefed( const CodeModel::Type* t )
{
    return ( t != 0 ? t->deref() : 0 );
}

CodeModel::CodeModel(QObject *parent) : QObject(parent),d_synthesize(false),d_trackIds(false),
    d_enableExt(false),d_fc(0), d_senseExt(false)
{
    d_errs = new Errors(this);
    d_errs->setReportToConsole(true);
    d_fc = new FileCache(this);

}

void CodeModel::clear()
{
    d_errs->clear();

    d_dir.clear();
    d_revDir.clear();

    d_scope = GlobalScope();

    // Add Basic Types
    d_scope.d_types.append( new Type( Type::BOOLEAN ) );
    d_scope.d_boolType = d_scope.d_types.back();
    d_scope.d_types.append( new Type( Type::CHAR ) );
    d_scope.d_charType = d_scope.d_types.back();
    d_scope.d_types.append( new Type( Type::INTEGER ) );
    d_scope.d_intType = d_scope.d_types.back();
    d_scope.d_types.append( new Type( Type::REAL ) );
    d_scope.d_realType = d_scope.d_types.back();
    d_scope.d_types.append( new Type( Type::BYTE ) );
    d_scope.d_types.append( new Type( Type::SET ) );
    d_scope.d_setType = d_scope.d_types.back();
    foreach( Type* t, d_scope.d_types )
        d_scope.addToScope(t);
    d_scope.d_names.insert( Lexer::getSymbol("LONGINT"), d_scope.d_intType );
    d_scope.d_names.insert( Lexer::getSymbol("LONGREAL"), d_scope.d_realType );
    d_scope.d_types.append( new Type( Type::STRING ) );
    d_scope.d_stringType = d_scope.d_types.back();
    d_scope.d_types.append( new Type( Type::NIL ) );
    d_scope.d_nilType = d_scope.d_types.back();

    // Add global procs
    d_scope.d_procs.append( new Element( Element::ABS ) );
    d_scope.d_procs.append( new Element( Element::ODD ) );
    d_scope.d_procs.append( new Element( Element::LEN ) );
    d_scope.d_procs.append( new Element( Element::LSL ) );
    d_scope.d_procs.append( new Element( Element::ASR ) );
    d_scope.d_procs.append( new Element( Element::ROR ) );
    d_scope.d_procs.append( new Element( Element::FLOOR ) );
    d_scope.d_procs.append( new Element( Element::FLT ) );
    d_scope.d_procs.append( new Element( Element::ORD ) );
    d_scope.d_procs.append( new Element( Element::CHR ) );
    d_scope.d_procs.append( new Element( Element::INC ) );
    d_scope.d_procs.append( new Element( Element::DEC ) );
    d_scope.d_procs.append( new Element( Element::INCL ) );
    d_scope.d_procs.append( new Element( Element::EXCL ) );
    d_scope.d_procs.append( new Element( Element::NEW ) );
    d_scope.d_procs.append( new Element( Element::ASSERT ) );
    d_scope.d_procs.append( new Element( Element::PACK ) );
    d_scope.d_procs.append( new Element( Element::UNPK ) );
    d_scope.d_procs.append( new Element( Element::LED ) );
    d_scope.d_procs.append( new Element( Element::WriteInt ) );
    d_scope.d_procs.append( new Element( Element::WriteReal ) );
    d_scope.d_procs.append( new Element( Element::WriteChar ) );
    d_scope.d_procs.append( new Element( Element::WriteLn ) );
    foreach( Element* t, d_scope.d_procs )
        d_scope.addToScope( t );

    if( d_enableExt ) // additional lower case builtins
        addLowerCaseGlobals();

}

void CodeModel::addPreload(const QByteArray& name, const QByteArray& source)
{
    d_preload[name] = source;
}

QByteArrayList CodeModel::getBuitinIdents()
{
    QByteArrayList res;
    for( int i = Element::ABS; i <= Element::LED; i++ )
        res << Element::s_kindName[i];
    for( int i = Type::BOOLEAN; i <= Type::SET; i++ )
        res << Type::s_kindName[i];
    return res;
}

void CodeModel::parseFile(const QString& path)
{
    QFile file(path);
    if( !file.open(QIODevice::ReadOnly) )
    {
        d_errs->error(Errors::Lexer, path, 0, 0,
                         tr("cannot open file from path %1").arg(path) );
        return;
    }
    parseFile( &file, path );
}

static bool IdenUseLessThan( const CodeModel::IdentUse& lhs, const CodeModel::IdentUse& rhs )
{
    return lhs.first->d_tok.d_lineNr < rhs.first->d_tok.d_lineNr ||
            (!(rhs.first->d_tok.d_lineNr < lhs.first->d_tok.d_lineNr) &&
             lhs.first->d_tok.d_colNr < rhs.first->d_tok.d_colNr );
}

bool CodeModel::parseFiles(const QStringList& files)
{
    clear();

    if( files.isEmpty() )
    {
        qDebug() << "nothing to parse";
        return false;
    }

    foreach( const QString& path, files )
    {
        qDebug() << "parsing" << path;
        parseFile(path);
    }

    if( d_errs->getSyntaxErrCount() != 0 )
    {
        qCritical() << "terminating because of syntax errors";
        return false;
    }

    qDebug() << "checking dependencies...";
    checkModuleDependencies();

    QList<Module*> order = findProcessingOrder();

    foreach( Module* m, order )
    {
        if( m->d_def != 0 )
        {
            qDebug() << "analyzing" << m->d_def->d_tok.d_sourcePath;
            processDeclSeq(m,m->d_def);
            resolveTypeRefs(m);
            checkTypeRules(m);
            checkNames(m);

            if( d_trackIds )
            {
                IdentUseList& l = d_dir[m->d_def->d_tok.d_sourcePath];
                std::sort( l.begin(), l.end(), IdenUseLessThan );
            }
        }
    }

    return d_errs->getErrCount() == 0;
}

void CodeModel::dump(QTextStream& out, const SynTree* node, int level)
{
    QByteArray str;
    if( node->d_tok.d_type == Ob::Tok_Invalid )
        level--;
    else if( node->d_tok.d_type < Ob::SynTree::R_First )
    {
        if( Ob::tokenTypeIsKeyword( node->d_tok.d_type ) )
            str = Ob::tokenTypeString(node->d_tok.d_type);
        else if( node->d_tok.d_type > Ob::TT_Specials )
            str = QByteArray("\"") + node->d_tok.d_val + QByteArray("\"");
        else
            str = QByteArray("\"") + node->d_tok.getString() + QByteArray("\"");

    }else
        str = Ob::SynTree::rToStr( node->d_tok.d_type );
    if( !str.isEmpty() )
    {
        str += QByteArray("\t") + QFileInfo(node->d_tok.d_sourcePath).baseName().toUtf8() +
                ":" + QByteArray::number(node->d_tok.d_lineNr) +
                ":" + QByteArray::number(node->d_tok.d_colNr);
        QByteArray ws;
        for( int i = 0; i < level; i++ )
            ws += "|  ";
        str = ws + str;
        out << str.data() << endl;
    }
    foreach( Ob::SynTree* sub, node->d_children )
        dump( out, sub, level + 1 );
}

static bool IdentUseListCompare( const CodeModel::IdentUse& lhs, const QPair<quint32,quint16>& rhs )
{
    return lhs.first->d_tok.d_lineNr < rhs.first ||
            (!(rhs.first < lhs.first->d_tok.d_lineNr) &&
             lhs.first->d_tok.d_colNr < rhs.second );
}

static inline bool isHit( const CodeModel::IdentUse& res, quint32 line, quint16 col )
{
    return res.first->d_tok.d_lineNr == line && res.first->d_tok.d_colNr <= col &&
                col <= ( res.first->d_tok.d_colNr + res.first->d_tok.d_len );
}

CodeModel::IdentUse CodeModel::findSymbolBySourcePos(const QString& file, quint32 line, quint16 col) const
{
    if( !d_trackIds )
        return IdentUse();
    const IdentUseList& l = d_dir[file];
    /* funktioniert nicht
    IdentUseList::const_iterator i = std::lower_bound(l.begin(),l.end(), qMakePair(line,col), IdentUseListCompare );
    if( i == l.end() )
        return IdentUse();
    IdentUse res = (*i);

    if( isHit( res, line, col ) )
        return res;
    */

    foreach( const IdentUse& res, l )
        if( isHit( res, line, col ) )
            return res;
    // else
    return IdentUse();
}

QList<const SynTree*> CodeModel::findReferencingSymbols(const CodeModel::NamedThing* sym, const QString& file)
{
    QList<const SynTree*> tmp = d_revDir.values( sym );
    if( file.isEmpty() )
        return tmp;
    QList<const SynTree*> res;
    foreach( const SynTree* s, tmp )
    {
        if( s->d_tok.d_sourcePath == file )
            res.append( s );
    }
    return res;
}

QList<Token> CodeModel::getComments(QString file) const
{
    return d_comments.value(file);
}

void CodeModel::parseFile(QIODevice* in, const QString& path)
{
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

    if( d_senseExt && !d_enableExt && lex.isEnabledExt() )
    {
        d_enableExt = true;
        addLowerCaseGlobals();
    }

    QList<SynTree*> toDelete;
    foreach( SynTree* st, p.d_root.d_children )
    {
        if( st->d_tok.d_type == SynTree::R_module || st->d_tok.d_type == SynTree::R_definition )
        {
            SynTree* id = findFirstChild(st,Tok_ident);
            if( id == 0 || !checkNameNotInScope(&d_scope,id) )
            {
                toDelete << st;
            }else
            {
                const QByteArray name = id->d_tok.d_val;
                Module* m = new Module();
                m->d_outer = &d_scope;
                m->d_name = name;
                m->d_def = st;
                m->d_id = id;
                m->d_isExt = lex.isEnabledExt();
                m->d_isDef = st->d_tok.d_type == SynTree::R_definition;
                d_scope.d_mods.append( m );
                d_scope.addToScope( m );
                index(id,m);
            }
        }else
            toDelete << st;
    }
    p.d_root.d_children.clear();
    foreach( SynTree* st, toDelete )
        delete st;
    foreach( const Token& t, p.d_comments )
        d_comments[t.d_sourcePath].append(t);
}

void CodeModel::checkModuleDependencies()
{
    QList<Module*> mods = d_scope.d_mods; // d_mods ändert während loop
    foreach( Module* m, mods )
    {
        SynTree* il = findFirstChild(m->d_def,SynTree::R_ImportList);
        if( il )
        {
            foreach( SynTree* i, il->d_children )
            {
                if( i->d_tok.d_type == SynTree::R_import )
                {
                    Q_ASSERT( !i->d_children.isEmpty() && i->d_children.first()->d_tok.d_type == Tok_ident );
                    const QByteArray localName = i->d_children.first()->d_tok.d_val;
                    QByteArray globalName = localName;
                    if( i->d_children.size() > 1 )
                    {
                        Q_ASSERT( i->d_children[1]->d_tok.d_type == Tok_ColonEq &&
                                i->d_children[2]->d_tok.d_type == Tok_ident );
                        globalName = i->d_children[2]->d_tok.d_val;
                    }
                    NamedThing* nt = d_scope.d_names.value(globalName);
                    Module* other = dynamic_cast<Module*>( nt );
                    if( other == 0 )
                    {
                        if( nt != 0 )
                        {
                            d_errs->error( Errors::Semantics, i, tr("'%1' is not a module").arg(globalName.data()));
                            continue;
                        }

                        if( d_preload.contains(globalName) )
                        {
                            QByteArray src = d_preload.value(globalName);
                            QBuffer buf( &src );
                            buf.open(QIODevice::ReadOnly);
                            parseFile( &buf, globalName );
                            other = dynamic_cast<Module*>( d_scope.d_names.value(globalName) );
                            if( other == 0 )
                                qCritical() << "failed preload module" << globalName;
                            else
                                qDebug() << "preloaded module" << globalName;
                        }else if( d_synthesize )
                        {
                            qDebug() << "synthesizing module" << globalName;
                            other = new Module();
                            other->d_outer = &d_scope;
                            other->d_name = globalName;
                            d_scope.d_mods.append( other );
                            d_scope.addToScope( other );
                        }else
                            d_errs->error( Errors::Semantics, i,
                                           tr("imported module '%1' not found").arg(globalName.data()));
                    }
                    if( other != 0 )
                    {
                        m->d_using.append(other);
                        other->d_usedBy.append(m);
                        m->d_names.insert(localName,other);
                        index(i->d_children.first(),other);
                    }
                }
            }
        }
    }
}

QList<CodeModel::Module*> CodeModel::findProcessingOrder()
{
    QList<Module*> res;
    QSet<Module*> mods = d_scope.d_mods.toSet();

    QSet<Module*> used;
    foreach( Module* m, mods )
    {
        // Find all leafs
        if( m->d_using.isEmpty() )
        {
            res.append(m);
            used.insert(m);
        }
    }
    mods -= used;

    while( !mods.isEmpty() )
    {
        foreach( Module* m, mods )
        {
            bool allUsed = true;
            foreach( Module* o, m->d_using )
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
                res.append(m);
            }
        }
        const int count = mods.size();
        mods -= used;
        if( count == mods.size() )
            break;
    }
    if( !mods.isEmpty() )
    {
        foreach( Module* m, mods )
        {
            if( m->d_def )
                d_errs->error(Errors::Semantics, m->d_def->d_tok.d_sourcePath, 0, 0,
                              tr("module '%1' has circular import dependencies").arg(m->d_name.data() ) );
        }
    }
    return res;
}

void CodeModel::processDeclSeq(Unit* m, SynTree* t)
{
    Q_ASSERT( t != 0 );
    SynTree* ds = findFirstChild( t, SynTree::R_DeclarationSequence ); // MODULE
    if( ds == 0 )
        ds = findFirstChild( t, SynTree::R_DeclarationSequence2 ); // DEFINITION
    if( ds )
    {
        foreach( SynTree* d, ds->d_children )
        {
            switch( d->d_tok.d_type )
            {
            case SynTree::R_ConstDeclaration:
                processConstDeclaration(m,d);
                break;
            case SynTree::R_TypeDeclaration:
                processTypeDeclaration(m,d);
                break;
            case SynTree::R_VariableDeclaration:
                processVariableDeclaration(m,d);
                break;
            case SynTree::R_ProcedureDeclaration: // MODULE
                processProcedureDeclaration(m,d);
                break;
            case SynTree::R_ProcedureHeading: // DEFINITION
                processProcedureDeclaration(m,d);
                break;
            }
        }
    }

    SynTree* ss = findFirstChild( t, SynTree::R_StatementSequence );
    if( ss )
    {
        foreach( SynTree* s, ss->d_children )
        {
            Q_ASSERT( s->d_tok.d_type == SynTree::R_statement );
            if( !s->d_children.isEmpty() )
                m->d_body.append(s->d_children.first());
        }
    }
}

void CodeModel::processConstDeclaration(Unit* m, SynTree* d)
{
    Q_ASSERT( d != 0 && d->d_tok.d_type == SynTree::R_ConstDeclaration );
    Q_ASSERT( d->d_children.size() > 1 && d->d_children.last()->d_tok.d_type == SynTree::R_expression );

    QPair<SynTree*,bool> id = getIdentFromIdentDef(d->d_children.first());
    if( checkNameNotInScope(m,id.first) )
    {
        Element* c = new Element();
        c->d_kind = Element::Constant;
        c->d_name = id.first->d_tok.d_val;
        c->d_public = id.second;
        c->d_def = d;
        c->d_id = id.first;
        c->d_st = d->d_children.last();
        // A ConstExpression is an expression containing constants only. More precisely, its evaluation must
        // be possible by a mere textual scan without execution of the program
        // Wirt's compiler seems not even to support name resolution for constants
        c->d_const = evalExpression(m,c->d_st);
        m->d_elems.append(c);
        m->addToScope( c );
        index(id.first,c);
    }
}

void CodeModel::processTypeDeclaration(Unit* m, SynTree* t)
{
    Q_ASSERT( t->d_children.size() > 1 );
    QPair<SynTree*,bool> id = getIdentFromIdentDef(t->d_children.first());
    if( checkNameNotInScope(m,id.first) )
    {
        Type* tp = parseType(m, t->d_children.last() );
        if( tp == 0 )
            return;
        tp->d_def = t;
        tp->d_name = id.first->d_tok.d_val;
        tp->d_id = id.first;
        tp->d_public = id.second;
        m->addToScope( tp );
        index(id.first,tp);
    }

}

void CodeModel::processVariableDeclaration(Unit* m, SynTree* t)
{
    Q_ASSERT( t->d_children.size() > 1 && t->d_children.first()->d_tok.d_type == SynTree::R_IdentList &&
              t->d_children.last()->d_tok.d_type == SynTree::R_type );
    Type* tp = parseType(m,t->d_children.last());
    //if( tp == 0 )
    //    return;
    foreach( SynTree* i, t->d_children.first()->d_children )
    {
        QPair<SynTree*,bool> id = getIdentFromIdentDef(i);
        if( checkNameNotInScope(m,id.first) )
        {
            Element* c = new Element();
            c->d_kind = Element::Variable;
            c->d_name = id.first->d_tok.d_val;
            c->d_public = id.second;
            c->d_def = t;
            c->d_type = tp;
            c->d_id = id.first;
            m->d_elems.append(c);
            m->addToScope( c );
            index(id.first,c);
        }
    }
}

void CodeModel::processProcedureDeclaration(Unit* ds, SynTree* t)
{
    SynTree* ph = findFirstChild( t, SynTree::R_ProcedureHeading );
    SynTree* pb = findFirstChild( t, SynTree::R_ProcedureBody );
    Q_ASSERT( ph != 0 );
    SynTree* idef = findFirstChild(ph,SynTree::R_identdef );
    Q_ASSERT( idef != 0 );

    QPair<SynTree*,bool> id = getIdentFromIdentDef(idef);
    if( !checkNameNotInScope(ds,id.first) )
        return;

    Procedure* res = new Procedure();
    res->d_outer = ds;
    res->d_def = t;
    res->d_name = id.first->d_tok.d_val;
    res->d_id = id.first;
    res->d_public = id.second;
    ds->d_procs.append(res);
    ds->addToScope( res );
    index(id.first,res);

    SynTree* fp = findFirstChild(ph,SynTree::R_FormalParameters );
    res->d_type = parseFormalParams(res,fp,res->d_vals);
    foreach( Element* p, res->d_vals )
        res->addToScope( p );

    Module* m = dynamic_cast<Module*>( ds );
    if( m )
    {
        if( m->d_isDef )
        {
            if( pb != 0 )
                d_errs->error(Errors::Semantics, t, tr("procedure body not allowed in definitions") );
            return;
        }else if( pb == 0 )
        {
            d_errs->error(Errors::Semantics, t, tr("procedure body is missing") );
            return;
        }
    }
    Q_ASSERT( pb != 0 );

    processDeclSeq(res,pb);
    SynTree* rs = findFirstChild(pb,SynTree::R_ReturnStatement );
    if( rs )
        res->d_body.append(rs);
}

bool CodeModel::checkNameNotInScope(Scope* scope, SynTree* id)
{
    Q_ASSERT( id != 0 );
    const QByteArray name = id->d_tok.d_val;
    if( scope->d_names.contains(name) )
    {
        d_errs->error( Errors::Semantics, id, tr("duplicate name: '%1'").arg(name.data()));
        return false;
    }else
        return true;
}

bool CodeModel::checkNameNotInRecord(Type* scope, SynTree* id)
{
    Q_ASSERT( id != 0 && scope->d_kind == Type::Record );
    const QByteArray name = id->d_tok.d_val;
    if( scope->d_vals.contains(name) )
    {
        d_errs->error( Errors::Semantics, id, tr("duplicate name: '%1'").arg(name.data()));
        return false;
    }else
        return true;
}

void CodeModel::checkNames(CodeModel::Unit* ds)
{
    // TODO: check procedure type assignments:
    // P must not be declared local to another procedure, and neither can it be a standard procedure.

    foreach( Element* d, ds->d_elems)
        checkNames(ds,d->d_st);
    foreach( Type* t, ds->d_types)
        checkNames(ds,t->getExpr());
    foreach( SynTree* s, ds->d_body )
        checkNames(ds,s);
    foreach( Procedure* p, ds->d_procs )
        checkNames(p);
}

void CodeModel::checkTypeRules(CodeModel::Unit* ds)
{
    foreach( Type* r, ds->d_types)
    {
        if( r->d_kind == Type::Record )
        {
            const Type* tp = derefed( r->d_type );
            if( tp != 0 && tp->d_kind != Type::Record )
            {
                SynTree* st = r->d_def;
                Q_ASSERT(st!=0);
                d_errs->error( Errors::Semantics, st, tr("base type not a RECORD type"));
            }
        }else if( r->d_kind == Type::Pointer )
        {
            const Type* tp = derefed( r->d_type );
            if( tp != 0 && tp->d_kind != Type::Record )
            {
                SynTree* st = r->d_def;
                Q_ASSERT(st!=0);
                d_errs->error( Errors::Semantics, st, tr("POINTER type not pointing to RECORD"));
            }
        }
    }
    foreach( Procedure* p, ds->d_procs )
        checkTypeRules(p);
}

static QString notNull( const CodeModel::NamedThing* ptr )
{
    return ( ptr != 0 ? ( dynamic_cast<const CodeModel::Type*>(ptr) ? "!" : "+" ) : "-" );
}

static QString toString( const CodeModel::DesigOpList& l, bool mid = false, bool sym = false )
{
    QString res;
    for( int i = 0; i < l.size(); i++ )
    {
        switch( l[i].d_op )
        {
        case CodeModel::UnknownOp:
        default:
            res += "?";
            if( sym ) res += notNull(l[i].d_sym);
            break;
        case CodeModel::IdentOp:
            if( mid || i != 0 )
                res += ".";
            res += l[i].d_arg->d_tok.d_val;
            if( sym ) res += notNull(l[i].d_sym);
            break;
        case CodeModel::PointerOp:
            res += l[i].d_arg->d_tok.d_val;
            if( sym ) res += notNull(l[i].d_sym);
            break;
        case CodeModel::TypeOp:
            res += "(T)";
            if( sym ) res += notNull(l[i].d_sym);
            break;
        case CodeModel::ProcedureOp:
            if( l[i].d_arg->d_children.isEmpty() )
                res += "()";
            else
                res += "(,)";
            if( sym ) res += notNull(l[i].d_sym);
            break;
        case CodeModel::ArrayOp:
            res += "[,]";
            if( sym ) res += notNull(l[i].d_sym);
            break;
        }
    }
    return res;
}

void CodeModel::checkNames(CodeModel::Unit* ds, SynTree* st, const CodeModel::Type* expected )
{
    if( st == 0 )
        return;

    if( st->d_tok.d_type == SynTree::R_expression )
    {
        if( st->d_children.size() == 1 )
            checkNames(ds, st->d_children.first(), expected );
        else
        {
            Q_ASSERT( st->d_children.size() > 1 );
            if( st->d_children[1]->d_tok.d_type == Tok_IN )
            {
                checkNames(ds, st->d_children.first(), 0 );
                checkNames(ds, st->d_children.last(), d_scope.d_setType );
            }else
            {
                checkNames(ds, st->d_children.first(), 0 );
                checkNames(ds, st->d_children.last(), 0 );
                /* TODO: noch nicht optimal
                const Type* t = typeOfExpression( ds, st->d_children.first() );
                if( t )
                    checkNames(ds, st->d_children.last(), t );
                else
                {
                    t = typeOfExpression( ds, st->d_children.last() );
                    if( t )
                        checkNames(ds, st->d_children.first(), t );
                }
                */
            }
        }
        // TODO analog mit SimpleExpression, term und factor
    }else if( st->d_tok.d_type == SynTree::R_assignmentOrProcedureCall )
    {
        Q_ASSERT( !st->d_children.isEmpty() && st->d_children.first()->d_tok.d_type == SynTree::R_designator );
        for( int i = 1; i < st->d_children.first()->d_children.size(); i++ )
        {
            // first go through all selectors of the designator for checking
            SynTree* sel = st->d_children.first()->d_children[i];
            Q_ASSERT( sel->d_tok.d_type == SynTree::R_selector );
            checkNames( ds, sel );
        }
        DesigOpList lhs = derefDesignator( ds, st->d_children.first(), true, d_synthesize);
        if( st->d_children.size() > 1 )
        {
            Q_ASSERT( st->d_children[1]->d_tok.d_type == Tok_ColonEq && st->d_children.size() == 3 &&
                    st->d_children[2]->d_tok.d_type == SynTree::R_expression );
            st->d_tok.d_type = SynTree::R_assignment_;
            checkAssig( ds, lhs, st->d_children[2] );
        }else
        {
            //qDebug() << "ProcCall" << toString(dopl,false,true);
            st->d_tok.d_type = SynTree::R_ProcedureCall_;
        }
    }else if( st->d_tok.d_type == SynTree::R_IfStatement || st->d_tok.d_type == SynTree::R_ElsifStatement ||
              st->d_tok.d_type == SynTree::R_WhileStatement )
    {
        Q_ASSERT( st->d_children.size() > 1 );
        checkNames(ds, st->d_children[1], d_scope.d_boolType ); // if/elsif/while-expression
        for( int i = 3; i < st->d_children.size(); i++ )
            checkNames(ds, st->d_children[i]);
    }else if( st->d_tok.d_type == SynTree::R_RepeatStatement )
    {
        Q_ASSERT( st->d_children.size() == 4 );
        checkNames(ds, st->d_children[3], d_scope.d_boolType ); // until-expression
        checkNames(ds, st->d_children[1]);
    }else if( st->d_tok.d_type == SynTree::R_ForStatement )
    {
        Q_ASSERT( st->d_children.size() >= 6 );
        checkNames(ds, st->d_children[3], d_scope.d_intType ); // for-expression
        checkNames(ds, st->d_children[5], d_scope.d_intType ); // to-expression
        SynTree* by = CodeModel::findFirstChild(st,SynTree::R_expression, 6 );
        if( by )
            checkNames(ds, by, d_scope.d_intType ); // by-expression
        for( int i = ( by == 0 ? 6: 8 ); i < st->d_children.size(); i++ )
            checkNames(ds, st->d_children[i]);
    }else if( st->d_tok.d_type == SynTree::R_CaseStatement )
    {
        checkCaseStatement(ds,st);
    }else if( st->d_tok.d_type == SynTree::R_designator )
    {
        for( int i = 1; i < st->d_children.size(); i++ )
        {
            SynTree* sel = st->d_children[i];
            Q_ASSERT( sel->d_tok.d_type == SynTree::R_selector );
            checkNames( ds, sel );
        }
        derefDesignator( ds, st, true, d_synthesize, expected );
    }else if( st->d_tok.d_type == SynTree::R_qualident )
    {
        derefQualident( ds, st, true, false );
    }else
    {
        foreach( SynTree* sub, st->d_children )
            checkNames(ds,sub, expected);
    }
}

void CodeModel::checkAssig(CodeModel::Unit* ds, const CodeModel::DesigOpList& lhs, SynTree* expr)
{
    Q_ASSERT( !lhs.isEmpty() && lhs.first().d_arg != 0 );
    if( lhs.last().d_sym == 0 )
    {
        d_errs->error(Errors::Semantics, lhs.first().d_arg, tr("cannot assign to '%1'").arg(toString(lhs) ) );
        return;
    }

    for( int i = 0; i < lhs.size(); i++ )
    {
        if( lhs[i].d_op == ProcedureOp )
        {
            d_errs->error(Errors::Semantics, lhs[i].d_arg, tr("procedure call cannot be left of assignment" ) );
            return;
        }
    }

    if( lhs.last().d_sym->isStub() )
    {
        // lhs type maybe not known
        if( lhs.last().d_op == IdentOp && lhs.last().d_sym )
        {
            if( Element* s = dynamic_cast<Element*>( const_cast<NamedThing*>(lhs.last().d_sym) ) )
            {
                if( s->d_kind != Element::Unknown && s->d_kind != Element::Variable )
                    qWarning() << "stubed member" << s->d_name << "of module" << s->d_scope->d_name <<
                                  "first seen as" << Element::s_kindName[s->d_kind] <<
                                  "redeclaring to" << Element::s_kindName[Element::Variable];
                s->d_kind = Element::Variable;
                if( s->d_type == 0 )
                {
                    s->d_type = typeOfExpression(ds,expr);
                    if( s->d_type == 0 || s->d_type->deref() == 0 )
                        qWarning() << "unknown type of expression in" << expr->d_tok.d_sourcePath
                                   << expr->d_tok.d_lineNr << ":" << expr->d_tok.d_colNr;
                }
            }
        }else if( lhs.last().d_op == ArrayOp && lhs.last().d_sym )
        {
            Q_ASSERT( lhs.size() > 1 );
            if( Element* s = dynamic_cast<Element*>( const_cast<NamedThing*>(lhs[lhs.size()-2].d_sym) ) )
            {
                if( s->d_kind != Element::Unknown && s->d_kind != Element::Variable )
                    qWarning() << "stubed member" << s->d_name << "of module" << s->d_scope->d_name <<
                                  "first seen as" << Element::s_kindName[s->d_kind] <<
                                  "redeclaring to" << Element::s_kindName[Element::Variable];
                s->d_kind = Element::Variable;
                if( s->d_type == 0 )
                    s->d_type = typeOfExpression(ds,expr);
                // TODO: check and update type of StubVal
            }
        }
    }else
    {
        // rhs type maybe not known
        const CodeModel::Type* t = lhs.last().d_sym->getType();
        // first check the rhs expression
        checkNames(ds,expr, t );
        // now check for type compatibility

    }
}

void CodeModel::checkCaseStatement(CodeModel::Unit* ds, SynTree* st)
{
    Q_ASSERT( st->d_tok.d_type == SynTree::R_CaseStatement && st->d_children.size() >= 4 );

    const CodeModel::Type* t = derefed(typeOfExpression(ds, st->d_children[1]));
    if( t->d_kind == CodeModel::Type::Pointer || t->d_kind == CodeModel::Type::Record )
    {
        QList< QPair< QPair<const Type*, SynTree*>,SynTree*> > cases;
        const NamedThing* var = 0;
        SynTree* id = flatten(st->d_children[1]);
        if( id->d_tok.d_type == Tok_ident )
            var = ds->findByName( id->d_tok.d_val ) ;
        if( var == 0 )
        {
            d_errs->error(Errors::Semantics,st,tr("only simple type case variables supported"));
            goto NormalCaseStatement;
        }
        for( int i = 3; i < st->d_children.size(); i++ )
        {
            SynTree* c = st->d_children[i];
            if( c->d_tok.d_type == SynTree::R_Case && !c->d_children.isEmpty() )
            {
                Q_ASSERT( c->d_children.first()->d_tok.d_type == SynTree::R_CaseLabelList );
                SynTree* q = flatten(c->d_children.first(),SynTree::R_qualident);
                if( q->d_tok.d_type != SynTree::R_qualident )
                    goto NormalCaseStatement;
                Quali quali = derefQualident(ds,q,false,false);
                if( const Type* t = dynamic_cast<const Type*>( quali.second.first ) )
                {
                    Q_ASSERT( c->d_children.last()->d_tok.d_type == SynTree::R_StatementSequence );
                    cases << qMakePair( qMakePair( t, c->d_children.first()), c->d_children.last() );
                }else
                    goto NormalCaseStatement;
            }
        }
        // qDebug() << "typecase at" << st->d_tok.d_sourcePath << st->d_tok.d_lineNr;
        for( int i = 0; i < cases.size(); i++ )
        {
            checkNames(ds,cases[i].first.second);
            Unit scope;
            scope.d_outer = ds;
            TypeAlias alias;
            alias.d_name = id->d_tok.d_val;
            alias.d_newType = cases[i].first.first;
            alias.d_alias = const_cast<NamedThing*>(var);
            scope.addToScope(&alias);
            checkNames( &scope, cases[i].second );
        }
        checkNames(ds,st->d_children[1]);
        return;
    }
NormalCaseStatement:
    foreach( SynTree* sub, st->d_children )
        checkNames(ds,sub);
}

CodeModel::Type*CodeModel::parseType(CodeModel::Unit* ds, SynTree* t)
{
    Q_ASSERT( t->d_tok.d_type == SynTree::R_type && !t->d_children.isEmpty() );
    switch( t->d_children.first()->d_tok.d_type )
    {
    case SynTree::R_qualident:
        return parseTypeRef(ds,t->d_children.first());
    case SynTree::R_ArrayType:
        return parseArrayType(ds,t->d_children.first());
    case SynTree::R_RecordType:
        return parseRecordType(ds,t->d_children.first());
    case SynTree::R_PointerType:
        return parsePointerType(ds,t->d_children.first());
    case SynTree::R_ProcedureType:
        return parseProcType(ds,t->d_children.first());
    }
    Q_ASSERT_X( false, "CodeModel::parseType", "unsupported type" );
    return 0;
}

CodeModel::Type*CodeModel::parseTypeRef(CodeModel::Unit* ds, SynTree* t)
{
    Q_ASSERT( t->d_tok.d_type == SynTree::R_qualident );
    Type* res = new Type();
    // in der ersten Runde werden qualidents noch nicht aufgelöst da sonst Reihenfolge in TYPE massgebend wird.
    res->d_def = t;
    res->d_kind = Type::TypeRef;
    res->d_st = t;
    ds->d_types.append(res);
    return res;
}

CodeModel::Type*CodeModel::parsePointerType(CodeModel::Unit* ds, SynTree* t)
{
    Q_ASSERT( t->d_children.size() > 1 && t->d_children[2]->d_tok.d_type == SynTree::R_type );
    Type* tp = parseType(ds, t->d_children[2] );
    Type* res = new Type();
    res->d_kind = Type::Pointer;
    res->d_type = tp;
    res->d_def = t;
    ds->d_types.append(res);
    return res;
}

CodeModel::Type*CodeModel::parseRecordType(CodeModel::Unit* ds, SynTree* t)
{
    SynTree* st = findFirstChild( t, SynTree::R_BaseType );
    Type* bt = 0;
    if( st )
    {
        Q_ASSERT( !st->d_children.isEmpty() && st->d_children.first()->d_tok.d_type == SynTree::R_qualident );
        bt = parseTypeRef(ds,st->d_children.first());
    }

    Type* res = new Type();
    res->d_kind = Type::Record;
    res->d_type = bt;
    res->d_def = t;
    ds->d_types.append(res);

    st = findFirstChild( t, SynTree::R_FieldListSequence );
    if( st )
    {
        foreach( SynTree* fl, st->d_children )
        {
            Q_ASSERT( fl->d_tok.d_type == SynTree::R_FieldList && fl->d_children.size() > 1 );
            Type* tp = parseType( ds, fl->d_children.last() );
            if( tp == 0 )
                continue;
            Q_ASSERT( fl->d_children.first()->d_tok.d_type == SynTree::R_IdentList );
            foreach( SynTree* i, fl->d_children.first()->d_children )
            {
                QPair<SynTree*,bool> id = getIdentFromIdentDef(i);
                if( checkNameNotInRecord(res,id.first) )
                {
                    const QByteArray name = id.first->d_tok.d_val;
                    Element* f = new Element();
                    f->d_kind = Element::Variable;
                    f->d_name = name;
                    f->d_public = id.second;
                    f->d_def = i;
                    f->d_id = id.first;
                    f->d_type = tp;
                    res->d_vals.insert( name, f );
                    index(id.first,f);
                }
            }
        }
    }
    return res;
}

CodeModel::Type*CodeModel::parseArrayType(CodeModel::Unit* ds, SynTree* t)
{
    SynTree* ll = findFirstChild( t, SynTree::R_LengthList );
    Q_ASSERT( ll != 0 && !ll->d_children.isEmpty() && ll->d_children.first()->d_tok.d_type == SynTree::R_expression );
    Q_ASSERT( !t->d_children.isEmpty() && t->d_children.last()->d_tok.d_type == SynTree::R_type );

    Type* tp = parseType(ds,t->d_children.last());
    Type* res = new Type();
    res->d_kind = Type::Array;
    res->d_def = t;
    res->d_type = tp;
    res->d_st = ll->d_children.first();
    bool ok;
    res->d_len = evalExpression( ds, res->d_st ).toUInt(&ok);
    if( !ok || res->d_len == 0 )
        d_errs->error(Errors::Semantics, res->d_st, tr("invalid array size") );
    ds->d_types.append(res);
    Type* last = res;
    for( int i = 1; i < ll->d_children.size(); i++ )
    {
        Q_ASSERT( ll->d_children[i]->d_tok.d_type == SynTree::R_expression );
        Type* cur = new Type();
        last->d_type = cur;
        cur->d_kind = Type::Array;
        cur->d_def = t;
        cur->d_type = tp;
        cur->d_st = ll->d_children[i];
        cur->d_len = evalExpression( ds, cur->d_st ).toUInt(&ok);
        if( !ok || cur->d_len == 0 )
            d_errs->error(Errors::Semantics, cur->d_st, tr("invalid array size") );
        ds->d_types.append(cur);
        last = cur;
    }
    return res;
}

CodeModel::Type*CodeModel::parseProcType(CodeModel::Unit* ds, SynTree* t)
{
    Type* res = new Type();
    res->d_kind = Type::ProcRef;
    res->d_def = t;
    ds->d_types.append(res);

    SynTree* fp = findFirstChild( t, SynTree::R_FormalParameters );
    QList<Element*> params;
    res->d_type = parseFormalParams(ds,fp,params);
    for( int i = 0; i < params.size(); i++ )
        res->d_vals.insert( QString("_%1").arg(i,2,10,QChar('0')).toUtf8(), params[i] );

    return res;
}

CodeModel::Type*CodeModel::parseFormalParams(CodeModel::Unit* ds, SynTree* fp, QList<Element*>& params)
{
    Type* res = 0;
    if( fp != 0 )
    {
        Q_ASSERT( fp->d_tok.d_type == SynTree::R_FormalParameters && !fp->d_children.isEmpty() );
        if( fp->d_children.last()->d_tok.d_type == SynTree::R_qualident )
            res = parseTypeRef(ds,fp->d_children.last());

        foreach( SynTree* sec, fp->d_children )
        {
            if( sec->d_tok.d_type == SynTree::R_FPSection )
            {
                Q_ASSERT( !sec->d_children.isEmpty() && sec->d_children.last()->d_tok.d_type == SynTree::R_FormalType &&
                          !sec->d_children.last()->d_children.isEmpty() &&
                          sec->d_children.last()->d_children.last()->d_tok.d_type == SynTree::R_qualident );
                Type* tp = parseTypeRef(ds,sec->d_children.last()->d_children.last());

                const bool var = sec->d_children.first()->d_tok.d_type == Tok_VAR;

                if( sec->d_children.last()->d_children.size() > 1 )
                {
                    Q_ASSERT( sec->d_children.last()->d_children.first()->d_tok.d_type == Tok_ARRAY );
                    Type* arr = new Type();
                    arr->d_kind = Type::Array; // Open Array has d_st==0, d_def!=0
                    arr->d_def = sec->d_children.last()->d_children.first();
                    arr->d_type = tp;
                    ds->d_types.append(arr);
                    tp = arr;
                }

                foreach( SynTree* id, sec->d_children )
                {
                    if( id->d_tok.d_type == Tok_ident )
                    {
                        Element* p = new Element();
                        p->d_kind = Element::Variable;
                        p->d_type = tp;
                        p->d_var = var;
                        p->d_name = id->d_tok.d_val;
                        p->d_def = sec;
                        p->d_id = id;
                        params.append(p);
                        index(id,p);
                    }
                }
            }
        }
    }
    return res;
}

void CodeModel::resolveTypeRefs(CodeModel::Unit* ds)
{
    // hier werden nun alle qualidents aufgelöst; im Fehlerfall bleibt d_type jedoch 0!
    foreach( Type* r, ds->d_types )
    {
        if( r->d_kind != Type::TypeRef )
            continue;
        //const NamedThing* nt = derefQualident(ds,r->d_typeSt);
        Quali q = derefQualident( ds, r->d_st, true, d_synthesize );

        if( q.second.first == 0 )
            continue; // ID wurde nicht gefunden

        const Type* tp = dynamic_cast<const Type*>(q.second.first);
        if( tp == 0 )
        {
            d_errs->error( Errors::Semantics, r->d_st, tr("qualident doesn't reference a type") );
        }else
            r->d_type = tp;
    }
    foreach( Procedure* p, ds->d_procs )
        resolveTypeRefs(p);
}

CodeModel::Quali CodeModel::derefQualident(CodeModel::Unit* ds, SynTree* t, bool report, bool synthesize)
{
    Q_ASSERT( t && t->d_tok.d_type == SynTree::R_qualident );
    if( t->d_children.isEmpty() )
        return Quali();
    Q_ASSERT( t->d_children.first()->d_tok.d_type == Tok_ident );

    SynTree* id1 = 0;
    SynTree* id2 = 0;
    const NamedThing* nt = 0;
    const Module* m = 0;
    if( t->d_children.size() > 1 )
    {
        id1 = t->d_children.first();
        nt = ds->findByName(id1->d_tok.d_val);
        if( nt == 0 )
        {
            if( report )
                d_errs->error( Errors::Semantics, id1, tr("module '%1' not imported").arg(id1->d_tok.d_val.data()) );
            Q_ASSERT( m == 0 && id1 != 0 && nt == 0 && id2 == 0 );
            return Quali(qMakePair(m,id1),qMakePair(nt,id2));
        }
        Q_ASSERT( t->d_children.last()->d_tok.d_type == Tok_ident );
        m = dynamic_cast<const Module*>(nt);
        if( m == 0 )
        {
            if( report )
                d_errs->error( Errors::Semantics, id1, tr("referenced '%1' is not a module").arg(id1->d_tok.d_val.data()) );
            nt = 0;
            Q_ASSERT( m == 0 && id1 != 0 && nt == 0 && id2 == 0 );
            return Quali(qMakePair(m,id1),qMakePair(nt,id2));
        }
        id2 = t->d_children.last();
        nt = m->findByName(id2->d_tok.d_val);
        if( synthesize && nt == 0 && m->d_def == 0 )
        {
            Module* mm = const_cast<Module*>(m);
            // Stub Module wurde gefunden, aber ident darin nicht
            qDebug() << "synthesizing type" << id2->d_tok.d_val << "in module" << id1->d_tok.d_val;
            Type* s = new Type();
            mm->d_types.append(s);
            s->d_name = id2->d_tok.d_val;
            s->d_public = true;
            mm->addToScope( s );
            nt = s;
        }else if( nt == 0 && report )
        {
            d_errs->error( Errors::Semantics, id1, tr("ident '%2' not found in module '%1'").arg(id1->d_tok.d_val.data())
                           .arg(id2->d_tok.d_val.data()) );
        }
        Q_ASSERT( m != 0 && id1 != 0 && id2 != 0 );
        if( report )
        {
            index(id1,m);
            if( nt )
                index(id2,nt);
        }
        return Quali(qMakePair(m,id1),qMakePair(nt,id2));
    }else
    {
        id2 = t->d_children.first();
        nt = ds->findByName(id2->d_tok.d_val);
        if( nt == 0 && report )
        {
            d_errs->error( Errors::Semantics, id2, tr("local ident '%1' not found").arg(id2->d_tok.d_val.data()) );
        }
        Q_ASSERT( m == 0 && id1 == 0 && id2 != 0 );
        if( report && nt != 0 )
            index(id2,nt);
        return Quali(qMakePair(m,id1),qMakePair(nt,id2));
    }
}

CodeModel::DesigOpList CodeModel::derefDesignator(CodeModel::Unit* ds, SynTree* t,
                                                               bool report, bool synthesize, const Type* expected)
{
    DesigOpList desig; // flattended designator

    Q_ASSERT( t != 0 && t->d_tok.d_type == SynTree::R_designator && !t->d_children.isEmpty() &&
              t->d_children.first()->d_tok.d_type == SynTree::R_qualident );

    for( int i = 0; i < t->d_children.first()->d_children.size(); i++ )
    {
        desig += getSelectorOp(t->d_children.first()->d_children[i]);
    }
    for( int i = 1; i < t->d_children.size(); i++ )
    {
        desig += getSelectorOp(t->d_children[i]);
        if( desig.back().d_op == TypeOp )
        {
            SynTree* flat = flatten(desig.back().d_arg, SynTree::R_qualident);
            Quali q = derefQualident( ds, flat, false );
            desig.back().d_sym = dynamic_cast<const Type*>( q.second.first );
            if( desig.back().d_sym == 0 )
                desig.back().d_op = ProcedureOp;
            else
                desig.back().d_arg = flat;
        }
    }

    desig.first().d_sym = ds->findByName(desig.first().d_arg->d_tok.d_val);
    if( desig.first().d_sym == 0 )
    {
        SynTree* dP = desig[0].d_arg;
        if( report )
            d_errs->error( Errors::Semantics, dP, tr("ident '%1' not found").arg(toString(desig.mid(0,1))) );
        return desig;
    }
    Q_ASSERT( desig.first().d_sym != 0 );
    if( report )
        index(desig.first().d_arg,desig.first().d_sym);
    for( int i = 1; i < desig.size(); i++ )
    {
        if( desig.size() < i )
            break; // sollte nicht vorkommen
        SynTree* dP = desig[i-1].d_arg;
        if( dynamic_cast<const Module*>( desig[i-1].d_sym ) && i-1 > 0 )
        {
            if( report )
                d_errs->error( Errors::Semantics, dP,
                               tr("modules cannot be idirectly designated '%1'").arg(toString(desig.mid(0,i))) );
            break;
        }
        DesigOpErr err;
        const NamedThing* next = applyDesigOp( ds, desig[i-1].d_sym, desig[i], &err, synthesize,
                i == desig.size() - 1 ? expected : 0 );
        if( err == InvalidOperation )
        {
            if( report )
                d_errs->error( Errors::Semantics, dP, tr("invalid operation '%1' on designator '%2'")
                           .arg(toString(desig.mid(i,1),true)).arg(toString(desig.mid(0,i))) );
            break;
        }else if( err == MissingType )
        {
            break; // fehlender Type wurde bereits in resolveTypeRefs gemeldet
        }else if( err == NotFound )
        {
            if( report )
                d_errs->error( Errors::Semantics, dP, tr("ident '%1' not found").arg(toString(desig.mid(0,i+1))) );
            break;
        }else
        {
            desig[i].d_sym = next;
            if( desig[i].d_op == IdentOp && report )
                index(desig[i].d_arg,desig[i].d_sym);
        }
    }

    if( report )
    {
        // ProcedureOp darf nur einmal im DesigOpList vorkommen und nur ganz am Schluss
        int n = 0;
        int hit = -1;
        for( int i = 0; i < desig.size(); i++ )
        {
            if( desig[i].d_op == ProcedureOp )
            {
                n++;
                hit = i;
            }
        }
        if( n > 1 || ( n > 0 && hit != ( desig.size() -1 ) ) )
            d_errs->error( Errors::Semantics, t, tr("invalid procedure call embedded in designator" ) );
    }
    return desig;
}

const CodeModel::NamedThing*CodeModel::applyDesigOp(Unit* ds, const CodeModel::NamedThing* input,
                                                    const DesigOp& dop, DesigOpErr* errOut,
                                                    bool synthesize, const CodeModel::Type* expected )
{
    DesigOpErr err = NoError;
    const CodeModel::NamedThing* res = 0;

    if( const Module* m = dynamic_cast<const Module*>( input ) )
    {
        if( dop.d_op == IdentOp )
        {
            res = m->findByName(dop.d_arg->d_tok.d_val);
            if( res == 0 )
            {
                if( m->d_def == 0 && synthesize )
                {
                    Module* mm = const_cast<Module*>( m );
                    qDebug() << "synthesizing member" << dop.d_arg->d_tok.d_val << "in module" << m->d_name;
                    Element* c = new Element();
                    c->d_name = dop.d_arg->d_tok.d_val;
                    c->d_public = true;
                    c->d_scope = mm;
                    mm->d_elems.append(c);
                    mm->addToScope( c );
                    res = c;
                }else
                    err = NotFound;
            }
        }else
            err = InvalidOperation;
    }else if( const TypeAlias* ac = dynamic_cast<const TypeAlias*>( input ) )
    {
        if( Element* v = dynamic_cast<Element*>( ac->d_alias ) )
        {
            const Type* oldType = v->d_type;
            v->d_type = const_cast<Type*>(ac->d_newType);
            res = applyDesigOp( ds, v, dop, &err, synthesize, expected );
            v->d_type = oldType;
        }else
            Q_ASSERT( false );

    }else if( const Element* vc = dynamic_cast<const Element*>( input ) )
    {
        if( vc->isStub() && !vc->isPredefProc() )
        {
            Element* v = const_cast<Element*>( vc );
            if( dop.d_op == ProcedureOp )
            {
                if( v->d_kind != Element::StubProc && v->d_kind != Element::Unknown )
                    qWarning() << "stubed member" << v->d_name << "of module" << v->d_scope->d_name <<
                                  "first seen as" << Element::s_kindName[v->d_kind] <<
                                  "redeclaring to" << Element::s_kindName[Element::StubProc];
                v->d_kind = Element::StubProc;
                if( v->d_type == 0 )
                    v->d_type = expected;
                if( dop.d_arg != 0 && v->d_vals.isEmpty() )
                {
                    Q_ASSERT( dop.d_arg->d_tok.d_type == SynTree::R_ExpList );
                    for( int i = 0; i < dop.d_arg->d_children.size(); i++ )
                    {
                        // Generate Procedure Params
                        Element* s = new Element();
                        s->d_kind = Element::Variable;
                        s->d_name = "_" + QByteArray::number(i);
                        s->d_public = true;
                        s->d_type = typeOfExpression(ds,dop.d_arg->d_children[i]);
                        s->d_st = dop.d_arg->d_children[i];
                        v->d_vals.append(s);
                    }
                    v->d_st = dop.d_arg;
                }
            }else if( dop.d_op == IdentOp )
            {
                if( v->d_kind != Element::Variable && v->d_kind != Element::Unknown )
                    qWarning() << "stubed member" << v->d_name << "of module" << v->d_scope->d_name <<
                                  "first seen as" << Element::s_kindName[v->d_kind] <<
                                  "redeclaring to" << Element::s_kindName[Element::Variable];
                v->d_kind = Element::Variable;
                if( vc->d_type == 0 )
                {
                    Type* st = new Type();
                    st->d_kind = Type::Record;
                    st->d_scope = v->d_scope;
                    v->d_type = st;
                    Unit* m = dynamic_cast<Unit*>(v->d_scope);
                    Q_ASSERT( m != 0 );
                    m->d_types.append(st);
                }
                res = applyDesigOp( ds, v->d_type, dop, &err, synthesize, expected );
            }else if( dop.d_op == ArrayOp )
            {
                if( v->d_kind != Element::Variable && v->d_kind != Element::Unknown )
                    qWarning() << "stubed member" << v->d_name << "of module" << v->d_scope->d_name <<
                                  "first seen as" << Element::s_kindName[v->d_kind] <<
                                  "redeclaring to" << Element::s_kindName[Element::Variable];
                v->d_kind = Element::Variable;
                if( vc->d_type == 0 )
                {
                    Type* st = new Type();
                    st->d_kind = Type::Array;
                    st->d_scope = v->d_scope;
                    v->d_type = st;
                    Unit* m = dynamic_cast<Unit*>(v->d_scope);
                    Q_ASSERT( m != 0 );
                    m->d_types.append(st);
                }
                res = applyDesigOp( ds, v->d_type, dop, &err, synthesize, expected );
            }else
                err = InvalidOperation;
        }else
        {
            if( vc->d_kind == Element::Constant )
                err = InvalidOperation;
            else if( vc->d_kind == Element::Variable )
            {
                // Für Modul-Variablen und Record-Felder
                if( dop.d_op == TypeOp )
                {
                    res = dop.d_sym;
                    if( res == 0 )
                        err = MissingType;
                }else
                {
                    const Type* vt = const_cast<Type*>( vc->d_type );
                    if( vt == 0 )
                        err = MissingType;
                    else
                        res = applyDesigOp( ds, derefed(vt), dop, &err, synthesize, expected );
                }
            }else if( vc->isPredefProc() )
            {
                if( dop.d_op == ProcedureOp )
                {
                    res = typeOfExpression(ds,vc->d_kind,dop.d_arg);
                }else
                {
                    err = InvalidOperation;
                }
            }
            else
                Q_ASSERT( false );
        }
    }else if( const Type* t = dynamic_cast<const Type*>( input ) )
    {
        if( t->isStub() )
        {
            if( dop.d_op == IdentOp )
            {
                res = t->findByName(dop.d_arg->d_tok.d_val);
                if( res == 0 && synthesize )
                {
                    Type* t2 = const_cast<Type*>(t);
                    if( t2->d_kind != Type::Record && t2->d_kind != Type::Unknown )
                        qWarning() << "stubed" << t2->d_name << "first seen as" << Type::s_kindName[t2->d_kind] <<
                                      "redeclaring to" << Type::s_kindName[Type::Record];
                    qDebug() << "synthesizing field" << dop.d_arg->d_tok.d_val << "in record" << t2->d_name <<
                                "of module" << t2->d_scope->d_name;
                    t2->d_kind = Type::Record;
                    Element* c = new Element();
                    c->d_kind = Element::Variable;
                    c->d_name = dop.d_arg->d_tok.d_val;
                    c->d_public = true;
                    c->d_type = expected;
                    c->d_scope = t->d_scope;
                    t2->d_vals.insert( c->d_name, c );
                    res = c;
                }
                if( res == 0 )
                    err = NotFound;
            }else if( dop.d_op == ArrayOp )
            {
                if( t->d_type == 0 && synthesize )
                {
                    Type* t2 = const_cast<Type*>(t);
                    t2->d_type = expected;
                }
                res = t->d_type; // don't deref
            }
            else
                err = InvalidOperation;
        }else if( t->isBasicType() )
            err = InvalidOperation;
        else if( t->d_kind == Type::TypeRef )
        {
            t = t->deref();
            if( t != input )
                res = applyDesigOp( ds, t->deref(), dop, &err, synthesize, expected );
        }else if( t->d_kind == Type::Array )
        {
            if( dop.d_op == ArrayOp )
                res = t->d_type; // don't deref
            else
                err = InvalidOperation;
            if( res == 0 )
                err = MissingType;
        }
        else if( t->d_kind == Type::Record )
        {
            if( dop.d_op == IdentOp )
            {
                res = t->findByName(dop.d_arg->d_tok.d_val);
                if( res == 0 )
                    err = NotFound;
            }else
                err = InvalidOperation;
        }
        else if( t->d_kind == Type::Pointer )
        {
            const Type* td = derefed(t->d_type);
            if( dop.d_op == PointerOp )
                res = t->d_type;
            else if( dop.d_op == IdentOp && td != 0 && td->d_kind == Type::Record )
                res = applyDesigOp( ds, td, dop, &err, synthesize, expected );
            else
                err = InvalidOperation;
        }
        else if( t->d_kind == Type::ProcRef )
        {
            if( dop.d_op == ProcedureOp )
                res = t->d_type;
            else
                err = InvalidOperation;
        }
        else
            Q_ASSERT( false );
    }else if( const Procedure* v = dynamic_cast<const Procedure*>( input ) )
    {
        if( dop.d_op == ProcedureOp )
        {
            res = v->d_type;
        }else
        {
            err = InvalidOperation;
        }
    }else
    {
        err = NotFound;
    }
    if( errOut )
        *errOut = err;
    return res;
}

QPair<SynTree*, bool> CodeModel::getIdentFromIdentDef(SynTree* d)
{
    Q_ASSERT( d != 0 && d->d_tok.d_type == SynTree::R_identdef );
    SynTree* id = d->d_children.first();
    return qMakePair( id, d->d_children.size() > 1 );
}

SynTree*CodeModel::findFirstChild(const SynTree* st, int type, int startWith )
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

SynTree*CodeModel::flatten(SynTree* st, int stopAt)
{
    if( st == 0 )
        return 0;
    while( st->d_children.size() == 1 && ( stopAt == 0 || st->d_tok.d_type != stopAt ) )
        st = st->d_children.first();
    return st;
}

CodeModel::DesigOpList CodeModel::derefDesignator( const CodeModel::Unit* ds, const SynTree* st) const
{
    return const_cast<CodeModel*>(this)->derefDesignator(const_cast<Unit*>(ds),const_cast<SynTree*>(st),false,false);
}

CodeModel::Quali CodeModel::derefQualident( const CodeModel::Unit* ds, const SynTree* st)
{
    return derefQualident(const_cast<Unit*>(ds), const_cast<SynTree*>(st), false, false );
}

CodeModel::DesigOp CodeModel::getSelectorOp(SynTree* st)
{
    Q_ASSERT( st != 0 );
    if( st->d_tok.d_type == Tok_ident )
        return DesigOp(IdentOp,st);
    else if( st->d_tok.d_type == SynTree::R_selector )
    {
        Q_ASSERT( !st->d_children.isEmpty() );
        if( st->d_children.size() == 1 )
        {
            SynTree* first = st->d_children.first();
            if( first->d_tok.d_type == Tok_ident )
                return DesigOp(IdentOp,first);
            else if( first->d_tok.d_type == Tok_Hat )
                return DesigOp(PointerOp,first);
        }else if( st->d_children.size() == 2 )
        {
            Q_ASSERT( st->d_children.first()->d_tok.d_type == Tok_Lpar &&
                      st->d_children.last()->d_tok.d_type == Tok_Rpar);
            return DesigOp(ProcedureOp, (SynTree*)0);
        }else if( st->d_children.size() == 3 )
        {
            if( st->d_children.first()->d_tok.d_type == Tok_Lbrack )
            {
                Q_ASSERT( st->d_children.last()->d_tok.d_type == Tok_Rbrack );
                return DesigOp(ArrayOp,st->d_children[1]);
            }else if( st->d_children.first()->d_tok.d_type == Tok_Lpar )
            {
                Q_ASSERT( st->d_children.last()->d_tok.d_type == Tok_Rpar );
                SynTree* flat = flatten(st->d_children[1], SynTree::R_qualident);
                if( flat->d_tok.d_type == SynTree::R_qualident )
                    return DesigOp(TypeOp,st->d_children[1]);
                else
                    return DesigOp(ProcedureOp,st->d_children[1]);
            }
        }else
            Q_ASSERT(false);
    }
    return DesigOp(UnknownOp,st);
}

const CodeModel::Type*CodeModel::typeOfExpression( const Unit* ds, SynTree* st) const
{
    if( st == 0 )
        return 0;

    switch( st->d_tok.d_type )
    {
    case Tok_hexchar:
        return d_scope.d_charType;
    case Tok_integer:
        return d_scope.d_intType;
    case Tok_real:
        return d_scope.d_realType;
    case Tok_string:
    case Tok_hexstring:
        return d_scope.d_stringType;
    case Tok_TRUE:
    case Tok_FALSE:
        return d_scope.d_boolType;
    case Tok_NIL:
        return d_scope.d_nilType;
    case SynTree::R_set:
        return d_scope.d_setType;
    case SynTree::R_designator:
        {
            DesigOpList nt = const_cast<CodeModel*>(this)->
                    derefDesignator( const_cast<Unit*>(ds), st, false, false );
            if( nt.isEmpty() )
                return 0;
            else if( const Element* c = dynamic_cast<const Element*>(nt.last().d_sym) )
            {
                if( c->d_kind == Element::Constant )
                {
                    if( const Module* m = dynamic_cast<const Module*>( nt.first().d_sym ) )
                        ds = const_cast<Module*>(m);
                    return typeOfExpression( ds, c->d_st );
                }else if( c->d_kind == Element::Variable )
                    return c->d_type;
            }
            else if( nt.last().d_sym != 0 )
                return dynamic_cast<const Type*>(nt.last().d_sym);
            //else
            return 0;
        }
    }

    if( st->d_children.size() == 1 )
        return typeOfExpression( ds, st->d_children.first() );
    Q_ASSERT( st->d_children.size() > 1 );

    switch( st->d_tok.d_type )
    {
    case SynTree::R_expression:
        return d_scope.d_boolType;
    case SynTree::R_SimpleExpression:
        {
            SynTree* sub = findFirstChild(st,SynTree::R_AddOperator);
            if( sub && sub->d_tok.d_type == Tok_OR )
                return d_scope.d_boolType;
            sub = findFirstChild(st,SynTree::R_term);
            Q_ASSERT( sub != 0 );
            return typeOfExpression( ds, sub );
        }
        break;
    case SynTree::R_term:
        {
            SynTree* sub = findFirstChild(st,SynTree::R_MulOperator);
            if( sub && sub->d_tok.d_type == Tok_Amp )
                return d_scope.d_boolType;
            sub = findFirstChild(st,SynTree::R_factor);
            Q_ASSERT( sub != 0 );
            return typeOfExpression( ds, sub );
        }
        break;
    case SynTree::R_factor:
        if( st->d_children.first()->d_tok.d_type == Tok_Lpar )
            return typeOfExpression( ds, st->d_children[1] );
        if( st->d_children.first()->d_tok.d_type == Tok_Tilde )
            return d_scope.d_boolType;
        Q_ASSERT( false );
        break;
    default:
        return 0;
    }
    return 0;
}

bool CodeModel::isArrayOfChar(const NamedThing* nt) const
{
    if( nt == 0 )
        return false;
    const Type* t = 0;
    if( const Element* e = dynamic_cast<const Element*>(nt) )
    {
        if( e->d_kind == Element::Variable )
            t = e->d_type;
    }else
        t = dynamic_cast<const Type*>(nt);
    if( t && t->d_kind == Type::Array )
    {
        t = derefed(t->d_type);
        return t == d_scope.d_charType;
    }
    return false;
}

const CodeModel::Type*CodeModel::typeOfExpression(Unit* ds, Element::Kind m, SynTree* args) const
{
    switch( m )
    {
    case Element::ABS:
        return typeOfExpression(ds,args);
    case Element::ODD:
        return d_scope.d_boolType;
    case Element::LEN:
    case Element::LSL:
    case Element::ASR:
    case Element::ROR:
    case Element::FLOOR:
        return d_scope.d_intType;
    case Element::FLT:
        return d_scope.d_realType;
    case Element::CHR:
        return d_scope.d_charType;
    default:
        return 0;
    }
}

QVariant CodeModel::evalSimpleExpression(const CodeModel::Unit* u, SynTree* expr) const
{
    Q_ASSERT( expr->d_tok.d_type == SynTree::R_SimpleExpression );
    Q_ASSERT( !expr->d_children.isEmpty() );

    int op = Tok_Invalid;
    int termIdx = 0;
    switch( expr->d_children.first()->d_tok.d_type )
    {
    case Tok_Plus:
    case Tok_Minus:
        op = expr->d_children.first()->d_tok.d_type;
        termIdx++;
        break;
    }
    Q_ASSERT( expr->d_children.size() > termIdx );
    QVariant lhs = evalTerm(u,expr->d_children[termIdx++]);

    // Vorzeichen anwenden
    if( op != Tok_Invalid )
    {
        if( lhs.type() == QVariant::LongLong )
            lhs = lhs.toLongLong() * ( op == Tok_Plus ? 1 : -1 );
        else if( lhs.type() == QVariant::Double )
            lhs = lhs.toDouble() * ( op == Tok_Plus ? 1.0 : -1.0 );
        else if( lhs.canConvert<Set>() )
            lhs = QVariant::fromValue( ~lhs.value<Set>());
        else
            d_errs->error(Errors::Semantics, expr, tr("invalid sign for expression type"));
    }

    if( expr->d_children.size() == termIdx )
        return lhs;
    // else
    Q_ASSERT( ( expr->d_children.size() - termIdx ) % 2 == 0 );

    QVariant res = lhs;

    for( int i = termIdx; i < expr->d_children.size() - termIdx; i += 2 )
    {
        const QVariant rhs = evalTerm(u,expr->d_children[i+1]);

        Q_ASSERT( expr->d_children[i]->d_tok.d_type == SynTree::R_AddOperator &&
                  !expr->d_children[i]->d_children.isEmpty());
        switch( expr->d_children[i]->d_children.first()->d_tok.d_type )
        {
        case Tok_Plus:
            if( res.type() == QVariant::Double && rhs.type() == QVariant::Double )
                res = res.toDouble() + rhs.toDouble();
            else if( res.type() == QVariant::LongLong && rhs.type() == QVariant::LongLong )
                res = res.toLongLong() + rhs.toLongLong();
            else if( lhs.canConvert<Set>() && rhs.canConvert<Set>() )
                return QVariant::fromValue( lhs.value<Set>() | rhs.value<Set>() );
            else
                d_errs->error(Errors::Semantics, expr->d_children[i], "operator not compatible with value types");
            break;
        case Tok_Minus:
            if( res.type() == QVariant::Double && rhs.type() == QVariant::Double )
                res = res.toDouble() - rhs.toDouble();
            else if( res.type() == QVariant::LongLong && rhs.type() == QVariant::LongLong )
                res = res.toLongLong() - rhs.toLongLong();
            else if( lhs.canConvert<Set>() && rhs.canConvert<Set>() )
            {
                const Set a = lhs.value<Set>();
                const Set b = rhs.value<Set>();
                Set res;
                for( int j = 0; j < a.size(); j++ )
                    res.set( a.test(j) && !b.test(j) );
                return QVariant::fromValue( res );
            }else
                d_errs->error(Errors::Semantics, expr->d_children[i], "operator not compatible with value types");
            break;
        case Tok_OR:
            if( res.type() == QVariant::Bool && rhs.type() == QVariant::Bool )
                res = res.toBool() || rhs.toBool();
            else
                d_errs->error(Errors::Semantics, expr->d_children[i], "operator not compatible with value types");
            break;
        default:
            Q_ASSERT(false);
            break;
        }
    }
    return res;
}

QVariant CodeModel::evalTerm(const CodeModel::Unit* u, SynTree* expr) const
{
    Q_ASSERT( expr->d_tok.d_type == SynTree::R_term );
    Q_ASSERT( !expr->d_children.isEmpty() );
    const QVariant lhs = evalFactor( u, expr->d_children.first() );
    if( expr->d_children.size() == 1 )
        return lhs;
    Q_ASSERT( ( expr->d_children.size() - 1 ) % 2 == 0 );

    QVariant res = lhs;

    for( int i = 1; i < expr->d_children.size() - 1; i += 2 )
    {
        const QVariant rhs = evalFactor( u, expr->d_children[i+1] );

        Q_ASSERT( expr->d_children[i]->d_tok.d_type == SynTree::R_MulOperator &&
                  !expr->d_children[i]->d_children.isEmpty());
        switch( expr->d_children[i]->d_children.first()->d_tok.d_type )
        {
        case Tok_Star:
            if( res.type() == QVariant::Double && rhs.type() == QVariant::Double )
                res = res.toDouble() * rhs.toDouble();
            else if( res.type() == QVariant::LongLong && rhs.type() == QVariant::LongLong )
                res = res.toLongLong() * rhs.toLongLong();
            else if( lhs.canConvert<Set>() && rhs.canConvert<Set>() )
                return QVariant::fromValue( lhs.value<Set>() & rhs.value<Set>() );
            else
                d_errs->error(Errors::Semantics, expr->d_children[i], "operator not compatible with value types");
            break;
        case Tok_Slash:
            if( res.type() == QVariant::Double && rhs.type() == QVariant::Double )
                res = res.toDouble() / rhs.toDouble();
            else if( res.type() == QVariant::LongLong && rhs.type() == QVariant::LongLong )
                res = res.toLongLong() / rhs.toLongLong();
            else if( lhs.canConvert<Set>() && rhs.canConvert<Set>() )
            {
                const Set a = lhs.value<Set>();
                const Set b = rhs.value<Set>();
                Set res;
                for( int j = 0; j < a.size(); j++ )
                    res.set( a.test(j) || b.test(j) && !( a.test(j) && b.test(j) ) );
                return QVariant::fromValue( res );
            }else
                d_errs->error(Errors::Semantics, expr->d_children[i], "operator not compatible with value types");
            break;
        case Tok_DIV:
            if( res.type() == QVariant::LongLong && rhs.type() == QVariant::LongLong )
            {
                const qint64 a = res.toLongLong();
                const qint64 b = rhs.toLongLong();
                // res = ( a - ( ( a % b + b ) % b ) ) / b;
                // source: http://lists.inf.ethz.ch/pipermail/oberon/2019/013353.html
                if (a < 0)
                    res = (a - b + 1) / b;
                else
                    res = a / b;
            }
            else
                d_errs->error(Errors::Semantics, expr->d_children[i], "operator not compatible with value types");
            break;
        case Tok_MOD:
            if( res.type() == QVariant::LongLong && rhs.type() == QVariant::LongLong )
            {
                const qint64 a = res.toLongLong();
                const qint64 b = rhs.toLongLong();
                // res = ( a % b + b ) % b;
                // source: http://lists.inf.ethz.ch/pipermail/oberon/2019/013353.html
                if (a < 0)
                    res = (b - 1) + ((a - b + 1)) % b;
                else
                    res = a % b;
            }
            else
                d_errs->error(Errors::Semantics, expr->d_children[i], "operator not compatible with value types");
            break;
        case Tok_Amp:
            if( res.type() == QVariant::Bool && rhs.type() == QVariant::Bool )
                res = res.toBool() && rhs.toBool();
            else
                d_errs->error(Errors::Semantics, expr->d_children[i], "operator not compatible with value types");
            break;
        default:
            qCritical() << "unexpected operator" << SynTree::rToStr(expr->d_children[i]->d_tok.d_type);
            Q_ASSERT(false);
            break;
        }
    }
    return res;
}

QVariant CodeModel::evalFactor(const CodeModel::Unit* u, SynTree* expr) const
{
    Q_ASSERT( expr->d_tok.d_type == SynTree::R_factor );
    Q_ASSERT( !expr->d_children.isEmpty() );
    SynTree* first = expr->d_children.first();
    switch( first->d_tok.d_type )
    {
    case SynTree::R_literal:
        return evalLiteral( u, expr->d_children[0]);
    case SynTree::R_variableOrFunctionCall:
        {
            Q_ASSERT( first->d_children.size() == 1 && first->d_children.first()->d_tok.d_type
                      == SynTree::R_designator );
            DesigOpList dopl = derefDesignator(u,first->d_children.first());
            if( dopl.isEmpty() || dopl.size() > 2 || dopl[0].d_op != CodeModel::IdentOp ||
                    ( dopl.size() == 2 && dopl[1].d_op != CodeModel::IdentOp ) )
            {
                if( dopl.size() == 2 && dopl[1].d_op == CodeModel::ProcedureOp )
                    d_errs->error(Errors::Semantics, first->d_children.first(), tr("procedure calls not supported in const") );
                else
                    d_errs->error(Errors::Semantics, first->d_children.first(), tr("invalid designator") );
                break;
            }
            const CodeModel::Element* e;
            if( dopl.size() == 2 )
                e = dynamic_cast<const CodeModel::Element*>( dopl[1].d_sym );
            else
                e = dynamic_cast<const CodeModel::Element*>( dopl[0].d_sym );
            if( e == 0 || e->d_kind != Element::Constant )
            {
                d_errs->error(Errors::Semantics, first->d_children.first(), tr("designator must reference a constant") );
                break;
            }
            return e->d_const;
        }
        break;
    case Tok_Lpar: // '(' expression ')'
        Q_ASSERT( expr->d_children.size() == 3 );
        return evalExpression( u, expr->d_children[1]);
    case Tok_Tilde: // '~' factor
        {
            Q_ASSERT( expr->d_children.size() == 2 );
            QVariant v = evalFactor( u, expr->d_children.last() );
            if( v.type() == QVariant::Bool )
                return !v.toBool();
            else
                d_errs->error(Errors::Semantics, first, tr("operator ~ can only be applied to boolean") );
        }
        break;
    default:
        d_errs->error(Errors::Semantics, first, tr("invalid constant expression") );
        break;
    }

    return QVariant();
}

QVariant CodeModel::evalLiteral(const CodeModel::Unit* u, SynTree* expr) const
{
    Q_ASSERT( expr->d_tok.d_type == SynTree::R_literal );
    Q_ASSERT( !expr->d_children.isEmpty() );
    SynTree* first = expr->d_children.first();
    switch( first->d_tok.d_type )
    {
    case SynTree::R_number:
        Q_ASSERT( expr->d_children.size() == 1 );
        if( first->d_children.first()->d_tok.d_type == Tok_real )
            return first->d_children.first()->d_tok.d_val.toDouble();
        else if( first->d_children.first()->d_tok.d_type == Tok_integer )
        {
            if( first->d_children.first()->d_tok.d_val.endsWith('H') )
                return first->d_children.first()->d_tok.d_val.
                                           left(first->d_children.first()->d_tok.d_val.size()-1).toLongLong(0,16);
            else
                return first->d_children.first()->d_tok.d_val.toLongLong();
        }else
            Q_ASSERT(false);
        break;
    case Tok_string:
        return first->d_tok.d_val.mid(1,first->d_tok.d_val.size()-2);
    case Tok_hexstring:
        return QByteArray::fromHex(first->d_tok.d_val.mid(1, first->d_tok.d_val.size() - 2));
    case Tok_hexchar:
        return QByteArray::fromHex( first->d_tok.d_val.left( first->d_tok.d_val.size() - 1 ) );
    case Tok_NIL:
        d_errs->error(Errors::Semantics, first, tr("NIL not allowed as a constant value") );
        break;
    case Tok_TRUE:
        return true;
    case Tok_FALSE:
        return false;
    case SynTree::R_set:
        {
           Q_ASSERT( first->d_children.size() >= 2 && first->d_children.first()->d_tok.d_type == Tok_Lbrace &&
                    first->d_children.last()->d_tok.d_type == Tok_Rbrace );
           Set res;
           for( int i = 1; i < first->d_children.size() - 1; i++ )
           {
               SynTree* e = first->d_children[i];
               Q_ASSERT( e->d_tok.d_type == SynTree::R_element && !e->d_children.isEmpty() );
               const QVariant from = evalExpression(u,e->d_children.first() );
               if( from.type() != QVariant::LongLong || from.toLongLong() >= res.size() )
               {
                   d_errs->error(Errors::Semantics, e->d_children.first(), tr("invalid set element") );
                   continue;
               }
               if( e->d_children.size() > 1 )
               {
                   Q_ASSERT( e->d_children.size() == 3 );
                   const QVariant to = evalExpression(u,e->d_children.last() );
                   if( to.type() != QVariant::LongLong || to.toLongLong() >= res.size() ||
                           to.toLongLong() <= from.toLongLong() )
                   {
                       d_errs->error(Errors::Semantics, e->d_children.last(), tr("invalid set element range") );
                       continue;
                   }
                   for( int j = from.toLongLong(); j <= to.toLongLong(); j++ )
                       res.set(j);
               }else
                   res.set( from.toULongLong() );
           }
           return QVariant::fromValue(res);
        }
        break;
    default:
        d_errs->error(Errors::Semantics, first, tr("invalid constant expression") );
        break;
    }

    return QVariant();
}

QVariant CodeModel::evalExpression(const CodeModel::Unit* u, SynTree* expr) const
{
    Q_ASSERT( expr->d_tok.d_type == SynTree::R_expression );
    Q_ASSERT( !expr->d_children.isEmpty() );
    const QVariant lhs = evalSimpleExpression( u, expr->d_children.first() );
    if( expr->d_children.size() == 1 )
        return lhs;
    // else
    Q_ASSERT( expr->d_children.size() == 3 );
    const QVariant rhs = evalSimpleExpression( u, expr->d_children.last() );

    switch( expr->d_children[1]->d_tok.d_type )
    {
    case Tok_Eq:
        if( lhs.type() == QVariant::Double && rhs.type() == QVariant::Double )
            return lhs.toDouble() == rhs.toDouble();
        else if( lhs.type() == QVariant::LongLong && rhs.type() == QVariant::LongLong )
            return lhs.toLongLong() == rhs.toLongLong();
        else if( lhs.type() == QVariant::ByteArray && rhs.type() == QVariant::ByteArray )
            return lhs.toByteArray() == rhs.toByteArray();
        else if( lhs.type() == QVariant::Bool && rhs.type() == QVariant::Bool )
            return lhs.toByteArray() == rhs.toByteArray();
        else if( lhs.canConvert<Set>() && rhs.canConvert<Set>() )
            return lhs.value<Set>() == rhs.value<Set>();
        else
            d_errs->error(Errors::Semantics, expr->d_children[1], "relation not compatible with value types");
        break;
    case Tok_Hash:
        if( lhs.type() == QVariant::Double && rhs.type() == QVariant::Double )
            return lhs.toDouble() != rhs.toDouble();
        else if( lhs.type() == QVariant::LongLong && rhs.type() == QVariant::LongLong )
            return lhs.toLongLong() != rhs.toLongLong();
        else if( lhs.type() == QVariant::ByteArray && rhs.type() == QVariant::ByteArray )
            return lhs.toByteArray() != rhs.toByteArray();
        else if( lhs.type() == QVariant::Bool && rhs.type() == QVariant::Bool )
            return lhs.toByteArray() != rhs.toByteArray();
        else if( lhs.canConvert<Set>() && rhs.canConvert<Set>() )
            return lhs.value<Set>() != rhs.value<Set>();
        else
            d_errs->error(Errors::Semantics, expr->d_children[1], "relation not compatible with value types");
        break;
    case Tok_Lt:
        if( lhs.type() == QVariant::Double && rhs.type() == QVariant::Double )
            return lhs.toDouble() < rhs.toDouble();
        else if( lhs.type() == QVariant::LongLong && rhs.type() == QVariant::LongLong )
            return lhs.toLongLong() < rhs.toLongLong();
        else if( lhs.type() == QVariant::ByteArray && rhs.type() == QVariant::ByteArray )
            return lhs.toByteArray() < rhs.toByteArray();
        else
            d_errs->error(Errors::Semantics, expr->d_children[1], "relation not compatible with value types");
        break;
    case Tok_Leq:
        if( lhs.type() == QVariant::Double && rhs.type() == QVariant::Double )
            return lhs.toDouble() <= rhs.toDouble();
        else if( lhs.type() == QVariant::LongLong && rhs.type() == QVariant::LongLong )
            return lhs.toLongLong() <= rhs.toLongLong();
        else if( lhs.type() == QVariant::ByteArray && rhs.type() == QVariant::ByteArray )
            return lhs.toByteArray() <= rhs.toByteArray();
        else
            d_errs->error(Errors::Semantics, expr->d_children[1], "relation not compatible with value types");
        break;
    case Tok_Geq:
        if( lhs.type() == QVariant::Double && rhs.type() == QVariant::Double )
            return lhs.toDouble() >= rhs.toDouble();
        else if( lhs.type() == QVariant::LongLong && rhs.type() == QVariant::LongLong )
            return lhs.toLongLong() >= rhs.toLongLong();
        else if( lhs.type() == QVariant::ByteArray && rhs.type() == QVariant::ByteArray )
            return lhs.toByteArray() >= rhs.toByteArray();
        else
            d_errs->error(Errors::Semantics, expr->d_children[1], "relation not compatible with value types");
        break;
    case Tok_Gt:
        if( lhs.type() == QVariant::Double && rhs.type() == QVariant::Double )
            return lhs.toDouble() > rhs.toDouble();
        else if( lhs.type() == QVariant::LongLong && rhs.type() == QVariant::LongLong )
            return lhs.toLongLong() > rhs.toLongLong();
        else if( lhs.type() == QVariant::ByteArray && rhs.type() == QVariant::ByteArray )
            return lhs.toByteArray() > rhs.toByteArray();
        else
            d_errs->error(Errors::Semantics, expr->d_children[1], "relation not compatible with value types");
        break;
    case Tok_IN:
        if( lhs.type() == QVariant::LongLong && rhs.canConvert<Set>() )
        {
            const qint64 i = lhs.toLongLong();
            const Set s = rhs.value<Set>();
            if( i >= 0 && i < s.size() )
                return s.test(i);
            else
                return false;
        }else
            d_errs->error(Errors::Semantics, expr->d_children[1], "relation not compatible with value types");
        break;
    case Tok_IS:
        d_errs->error(Errors::Semantics, expr->d_children[1], tr("relation not supported in a constant") );
        break;
    default:
        Q_ASSERT(false);
        break;
    }

    return QVariant();
}

void CodeModel::index(const SynTree* idUse, const CodeModel::NamedThing* decl)
{
    if( !d_trackIds )
        return;
    Q_ASSERT( idUse != 0 && decl != 0 );
    d_dir[idUse->d_tok.d_sourcePath].append( IdentUse(idUse,decl) );
    d_revDir.insert(decl,idUse);
}

void CodeModel::addLowerCaseGlobals()
{
    Scope::Names names = d_scope.d_names;
    Scope::Names::const_iterator i;
    for( i = names.begin(); i != names.end(); ++i )
        d_scope.d_names.insert( Lexer::getSymbol( i.key() ).toLower(), i.value() );
}

CodeModel::GlobalScope::~GlobalScope()
{
    foreach( Element* p, d_procs )
        delete p;
    foreach( Module* m, d_mods )
        delete m;
    foreach( Type* t, d_types )
        delete t;
}

CodeModel::Unit::~Unit()
{
    foreach( Procedure* p, d_procs )
        delete p;
    foreach( Element* c, d_elems )
        delete c;
    foreach( Type* t, d_types )
        delete t;
}

const CodeModel::Module* CodeModel::Scope::getModule() const
{
    if( const Module* m = dynamic_cast<const Module*>(this) )
        return m;
    else
        return d_outer->getModule();
}

void CodeModel::Scope::addToScope(CodeModel::NamedThing* nt)
{
    nt->d_scope = this;
    d_names.insert( nt->d_name, nt );
}

QList<CodeModel::Type*> CodeModel::Unit::getNamedTypes() const
{
    QList<Type*> res;
    foreach( Type* t, d_types )
        if( !t->d_name.isEmpty() )
            res << t;
    return res;
}

QList<CodeModel::Element*> CodeModel::Unit::getConsts() const
{
    QList<Element*> res;
    foreach( Element* e, d_elems )
        if( e->d_kind == Element::Constant )
            res << e;
    return res;
}

QList<CodeModel::Element*> CodeModel::Unit::getVars() const
{
    QList<Element*> res;
    foreach( Element* e, d_elems )
        if( e->d_kind == Element::Variable )
            res << e;
    return res;
}

QList<CodeModel::Element*> CodeModel::Unit::getStubProcs() const
{
    QList<Element*> res;
    foreach( Element* e, d_elems )
        if( e->d_kind == Element::StubProc )
            res << e;
    return res;
}

QList<CodeModel::Element*> CodeModel::Unit::getUnknowns() const
{
    QList<Element*> res;
    foreach( Element* e, d_elems )
        if( e->d_kind == Element::Unknown )
            res << e;
    return res;
}

CodeModel::Procedure::~Procedure()
{
    foreach( Element* v, d_vals )
        delete v;
}

const CodeModel::Element* CodeModel::Type::findByName(const QByteArray& n) const
{
    const Element* res = d_vals.value(n);
    if( res == 0 && d_type != 0 )
    {
        const Type* parent = d_type->deref();
        if( parent && parent->d_kind == Type::Record )
            res = parent->findByName(n);
    }

    return res;
}

QList<CodeModel::Element*> CodeModel::Type::getParams() const
{
    QList<Element*> res;
    Vals::const_iterator i;
    for( i = d_vals.begin(); i != d_vals.end(); ++i )
        res << i.value();
    return res;
}

const CodeModel::NamedThing*CodeModel::Scope::findByName(const QByteArray& name) const
{
    const CodeModel::NamedThing* thing = d_names.value(name);
    if( thing == 0 && d_outer )
        thing = d_outer->findByName(name);
    return thing;
}

const char* CodeModel::Type::s_kindName[] =
{
    "?",
    "BOOLEAN",
    "CHAR",
    "INTEGER",
    "REAL",
    "BYTE",
    "SET",
    "STRING",
    "NIL",
    "TypeRef",
    "Array",
    "Record",
    "Pointer",
    "ProcRef",
};

CodeModel::Type::Type(CodeModel::Type::Kind k):d_kind(k),d_type(0),d_st(0),d_len(0)
{
    if( k >= BOOLEAN && k <= SET )
        d_name = Lexer::getSymbol( s_kindName[k] );
}

CodeModel::Type::~Type()
{
    Vals::iterator i;
    for( i = d_vals.begin(); i != d_vals.end(); ++i )
        delete i.value();
}

const CodeModel::Type*CodeModel::Type::deref() const
{
    if( d_kind == TypeRef && d_type != 0 )
        return d_type->deref();
    else
        return this;
}

QByteArray CodeModel::Type::typeName() const
{
    if( isBasicType() )
        return "BasicType";
    switch( d_kind )
    {
    case TypeRef:
        return "TypeRef";
    case Array:
        return "Array";
    case Record:
        return "Record";
    case Pointer:
        return "Pointer";
    case ProcRef:
        return "ProcRef";
    default:
        break;
    }
    return QByteArray();
}

const char* CodeModel::Element::s_kindName[] =
{
    "?",
    "ABS",
    "ODD",
    "LEN",
    "LSL",
    "ASR",
    "ROR",
    "FLOOR",
    "FLT",
    "ORD",
    "CHR",
    "INC",
    "DEC",
    "INCL",
    "EXCL",
    "NEW",
    "ASSERT",
    "PACK",
    "UNPK",
    "WriteInt",
    "WriteReal",
    "WriteChar",
    "WriteLn",
    "LED",
    "Constant",
    "Variable",
    "StubProc",
};

CodeModel::Element::Element(CodeModel::Element::Kind k):d_kind(k),d_st(0),d_type(0)
{
    if( k > Unknown && k <= LED )
        d_name = Lexer::getSymbol(s_kindName[k]);
}

CodeModel::Element::~Element()
{
    foreach( Element* e, d_vals )
        delete e;
}

QByteArray CodeModel::Element::typeName() const
{
    if( isPredefProc() )
        return "Predefined";
    switch( d_kind )
    {
    case Constant:
        return "Constant";
    case Variable:
        return "Variable";
    case StubProc:
        return "StubProc";
    default:
        return QByteArray();
    }
}

QList<CodeModel::Element*> CodeModel::Element::getParams() const
{
     if( d_type && d_type->d_kind == Type::ProcRef && d_kind != StubProc )
     {
         QList<Element*> res;
         Type::Vals::const_iterator i;
         for( i = d_type->d_vals.begin(); i != d_type->d_vals.end(); ++i )
             res << i.value();
         return res;
     }else
         return d_vals;
}
