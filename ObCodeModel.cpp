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
#include <QtDebug>
#include <QFile>
#include <QFileInfo>
#include <typeinfo>
using namespace Ob;

static inline const CodeModel::Type* derefed( const CodeModel::Type* t )
{
    return ( t != 0 ? t->deref() : 0 );
}

CodeModel::CodeModel(QObject *parent) : QObject(parent),d_synthesize(false),d_trackIds(false)
{
    d_errs = new Errors(this);
    d_errs->setReportToConsole(true);

}

void CodeModel::clear()
{
    d_errs->clear();

    d_dir.clear();
    d_revDir.clear();

    d_scope = GlobalScope();

    // Add Basic Types
    d_scope.d_types.append( new BasicType( BasicType::BOOLEAN ) );
    d_scope.d_boolType = d_scope.d_types.back();
    d_scope.d_types.append( new BasicType( BasicType::CHAR ) );
    d_scope.d_charType = d_scope.d_types.back();
    d_scope.d_types.append( new BasicType( BasicType::INTEGER ) );
    d_scope.d_intType = d_scope.d_types.back();
    d_scope.d_types.append( new BasicType( BasicType::REAL ) );
    d_scope.d_realType = d_scope.d_types.back();
    d_scope.d_types.append( new BasicType( BasicType::BYTE ) );
    d_scope.d_types.append( new BasicType( BasicType::SET ) );
    d_scope.d_setType = d_scope.d_types.back();
    foreach( Type* t, d_scope.d_types )
        d_scope.d_names.insert( t->d_name, t );
    d_scope.d_names.insert( Lexer::getSymbol("LONGINT"), d_scope.d_intType );
    d_scope.d_names.insert( Lexer::getSymbol("LONGREAL"), d_scope.d_realType );
    d_scope.d_types.append( new BasicType( BasicType::STRING ) );
    d_scope.d_stringType = d_scope.d_types.back();
    d_scope.d_types.append( new BasicType( BasicType::NIL ) );
    d_scope.d_nilType = d_scope.d_types.back();

    // Add Basic Types
    d_scope.d_procs.append( new PredefProc( PredefProc::ABS ) );
    d_scope.d_procs.append( new PredefProc( PredefProc::ODD ) );
    d_scope.d_procs.append( new PredefProc( PredefProc::LEN ) );
    d_scope.d_procs.append( new PredefProc( PredefProc::LSL ) );
    d_scope.d_procs.append( new PredefProc( PredefProc::ASR ) );
    d_scope.d_procs.append( new PredefProc( PredefProc::ROR ) );
    d_scope.d_procs.append( new PredefProc( PredefProc::FLOOR ) );
    d_scope.d_procs.append( new PredefProc( PredefProc::FLT ) );
    d_scope.d_procs.append( new PredefProc( PredefProc::ORD ) );
    d_scope.d_procs.append( new PredefProc( PredefProc::CHR ) );
    d_scope.d_procs.append( new PredefProc( PredefProc::INC ) );
    d_scope.d_procs.append( new PredefProc( PredefProc::DEC ) );
    d_scope.d_procs.append( new PredefProc( PredefProc::INCL ) );
    d_scope.d_procs.append( new PredefProc( PredefProc::EXCL ) );
    d_scope.d_procs.append( new PredefProc( PredefProc::NEW ) );
    d_scope.d_procs.append( new PredefProc( PredefProc::ASSERT ) );
    d_scope.d_procs.append( new PredefProc( PredefProc::PACK ) );
    d_scope.d_procs.append( new PredefProc( PredefProc::UNPK ) );
    d_scope.d_procs.append( new PredefProc( PredefProc::LED ) );
    foreach( PredefProc* t, d_scope.d_procs )
        d_scope.d_names.insert( t->d_name, t );
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

void CodeModel::parseFile(const QString& path)
{
    QFile in( path );
    if( !in.open(QIODevice::ReadOnly) )
    {
        d_errs->warning( Errors::Lexer, path, 0, 0, tr("cannot open file for reading") );
        return;
    }
    Ob::Lexer lex;
    lex.setStream( &in, path );
    lex.setErrors(d_errs);
    Ob::Parser p(&lex,d_errs);
    p.RunParser();

    QList<SynTree*> toDelete;
    foreach( SynTree* st, p.d_root.d_children )
    {
        if( st->d_tok.d_type == SynTree::R_module )
        {
            SynTree* id = findFirstChild(st,Tok_ident);
            if( !checkNameNotInScope(&d_scope,id) )
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
                d_scope.d_mods.append( m );
                d_scope.d_names.insert( name, m );
                index(id,m);
            }
        }else
            toDelete << st;
    }
    p.d_root.d_children.clear();
    foreach( SynTree* st, toDelete )
        delete st;
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
                            d_errs->error( Errors::Semantics, i->d_tok.d_sourcePath, i->d_tok.d_lineNr, i->d_tok.d_colNr,
                                             tr("'%1' is not a module").arg(globalName.data()));
                            continue;
                        }
                        if( d_synthesize )
                        {
                            qDebug() << "synthesizing module" << globalName;
                            other = new Module();
                            other->d_outer = &d_scope;
                            other->d_name = globalName;
                            d_scope.d_mods.append( other );
                            d_scope.d_names.insert( globalName, other );
                        }else
                            d_errs->error( Errors::Semantics, i->d_tok.d_sourcePath, i->d_tok.d_lineNr, i->d_tok.d_colNr,
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

void CodeModel::processDeclSeq(DeclarationSequence* m, SynTree* t)
{
    Q_ASSERT( t != 0 );
    SynTree* ds = findFirstChild( t, SynTree::R_DeclarationSequence );
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
            case SynTree::R_ProcedureDeclaration:
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
            m->d_body.append(s);
        }
    }
}

void CodeModel::processConstDeclaration(DeclarationSequence* m, SynTree* d)
{
    Q_ASSERT( d != 0 && d->d_tok.d_type == SynTree::R_ConstDeclaration );
    Q_ASSERT( d->d_children.size() > 1 && d->d_children.last()->d_tok.d_type == SynTree::R_expression );

    QPair<SynTree*,bool> id = getIdentFromIdentDef(d->d_children.first());
    if( checkNameNotInScope(m,id.first) )
    {
        const QByteArray name = id.first->d_tok.d_val;
        Constant* c = new Constant();
        c->d_name = name;
        c->d_public = id.second;
        c->d_def = d;
        c->d_id = id.first;
        c->d_expr = d->d_children.last();
        m->d_consts.append(c);
        m->d_names.insert( name, c );
        index(id.first,c);
    }
}

void CodeModel::processTypeDeclaration(DeclarationSequence* m, SynTree* t)
{
    Q_ASSERT( t->d_children.size() > 1 );
    QPair<SynTree*,bool> id = getIdentFromIdentDef(t->d_children.first());
    if( checkNameNotInScope(m,id.first) )
    {
        Type* tp = parseType(m, t->d_children.last() );
        if( tp == 0 )
            return;
        tp->d_def = t;
        const QByteArray name = id.first->d_tok.d_val;
        tp->d_name = name;
        tp->d_id = id.first;
        tp->d_public = id.second;
        m->d_names.insert( name, tp );
        index(id.first,tp);
    }

}

void CodeModel::processVariableDeclaration(DeclarationSequence* m, SynTree* t)
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
            const QByteArray name = id.first->d_tok.d_val;
            Variable* c = new Variable();
            c->d_name = name;
            c->d_public = id.second;
            c->d_def = t;
            c->d_type = tp;
            c->d_id = id.first;
            m->d_vars.append(c);
            m->d_names.insert( name, c );
            index(id.first,c);
        }
    }
}

void CodeModel::processProcedureDeclaration(DeclarationSequence* ds, SynTree* t)
{
    SynTree* ph = findFirstChild( t, SynTree::R_ProcedureHeading );
    SynTree* pb = findFirstChild( t, SynTree::R_ProcedureBody );
    Q_ASSERT( ph != 0 && pb != 0 );
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
    ds->d_names.insert( res->d_name, res );
    index(id.first,res);

    SynTree* fp = findFirstChild(ph,SynTree::R_FormalParameters );
    res->d_returns = parseFormalParams(res,fp,res->d_params);
    foreach( FormalParam* p, res->d_params )
        res->d_names.insert( p->d_name, p );

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
        d_errs->error( Errors::Semantics, id->d_tok.d_sourcePath, id->d_tok.d_lineNr, id->d_tok.d_colNr,
                       tr("duplicate name: '%1'").arg(name.data()));
        return false;
    }else
        return true;
}

bool CodeModel::checkNameNotInRecord(CodeModel::RecordType* scope, SynTree* id)
{
    Q_ASSERT( id != 0 );
    const QByteArray name = id->d_tok.d_val;
    if( scope->d_fields.contains(name) )
    {
        d_errs->error( Errors::Semantics, id->d_tok.d_sourcePath, id->d_tok.d_lineNr, id->d_tok.d_colNr,
                       tr("duplicate name: '%1'").arg(name.data()));
        return false;
    }else
        return true;
}

void CodeModel::checkNames(CodeModel::DeclarationSequence* ds)
{
    // TODO: check procedure type assignments:
    // P must not be declared local to another procedure, and neither can it be a standard procedure.

    foreach( Constant* d, ds->d_consts)
        checkNames(ds,d->d_expr);
    foreach( Type* t, ds->d_types)
        checkNames(ds,t->getExpr());
    foreach( SynTree* s, ds->d_body )
        checkNames(ds,s);
    foreach( Procedure* p, ds->d_procs )
        checkNames(p);
}

void CodeModel::checkTypeRules(CodeModel::DeclarationSequence* ds)
{
    foreach( Type* t, ds->d_types)
    {
        if( PointerType* p = dynamic_cast<PointerType*>(t) )
        {
            const Type* tp = derefed( p->d_type );
            if( tp )
            {
                if( dynamic_cast<const RecordType*>( tp ) == 0 )
                {
                    SynTree* st = p->d_def;
                    Q_ASSERT(st!=0);
                    d_errs->error( Errors::Semantics, st->d_tok.d_sourcePath, st->d_tok.d_lineNr, st->d_tok.d_colNr,
                                   tr("POINTER type not pointing to RECORD"));
                }
            }
        }else if( RecordType* r = dynamic_cast<RecordType*>(t) )
        {
            const Type* tp = derefed( r->d_base );
            if( tp )
            {
                if( dynamic_cast<const RecordType*>( tp ) == 0 )
                {
                    SynTree* st = r->d_def;
                    Q_ASSERT(st!=0);
                    d_errs->error( Errors::Semantics, st->d_tok.d_sourcePath, st->d_tok.d_lineNr, st->d_tok.d_colNr,
                                   tr("base type not a RECORD type"));
                }
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

void CodeModel::checkNames(CodeModel::DeclarationSequence* ds, SynTree* st)
{
    if( st == 0 )
        return;
    if( st->d_tok.d_type == SynTree::R_assignmentOrProcedureCall )
    {
        Q_ASSERT( !st->d_children.isEmpty() && st->d_children.first()->d_tok.d_type == SynTree::R_designator );
        for( int i = 1; i < st->d_children.first()->d_children.size(); i++ )
        {
            SynTree* sel = st->d_children.first()->d_children[i];
            Q_ASSERT( sel->d_tok.d_type == SynTree::R_selector );
            checkNames( ds, sel );
        }
        DesigOpList dopl = derefDesignator( ds, st->d_children.first(), true, d_synthesize);
        if( st->d_children.size() > 1 )
        {
            Q_ASSERT( st->d_children[1]->d_tok.d_type == Tok_ColonEq && st->d_children.size() == 3 &&
                    st->d_children[2]->d_tok.d_type == SynTree::R_expression );
            st->d_tok.d_type = SynTree::R_assignment;
            checkNames(ds,st->d_children[2]);
            checkAssig( ds, dopl, st->d_children[2] );
        }else
        {
            //qDebug() << "ProcCall" << toString(dopl,false,true);
            st->d_tok.d_type = SynTree::R_ProcedureCall;
        }
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
        derefDesignator( ds, st, true, d_synthesize);
    }else if( st->d_tok.d_type == SynTree::R_qualident )
    {
        derefQualident( ds, st, true, false );
    }else
    {
        foreach( SynTree* sub, st->d_children )
            checkNames(ds,sub);
    }
}

void CodeModel::checkAssig(CodeModel::DeclarationSequence* ds, const CodeModel::DesigOpList& dopl, SynTree* expr)
{
    Q_ASSERT( !dopl.isEmpty() && dopl.first().d_arg != 0 );
    if( dopl.last().d_sym == 0 )
    {
        d_errs->error(Errors::Semantics, dopl.first().d_arg->d_tok.d_sourcePath, dopl.first().d_arg->d_tok.d_lineNr,
                      dopl.first().d_arg->d_tok.d_colNr, tr("cannot assign to '%1'").arg(toString(dopl) ) );
        return;
    }

    if( dopl.last().d_op == IdentOp )
    {
        if( StubVal* s = dynamic_cast<StubVal*>( const_cast<NamedThing*>(dopl.last().d_sym) ) )
        {
            if( s->d_kind != StubVal::Unknown && s->d_kind != StubVal::Variable )
                qWarning() << "stubed member" << s->d_name << "of module" << s->d_mod->d_name <<
                              "first seen as" << StubVal::s_kindName[s->d_kind] <<
                              "redeclaring to" << StubVal::s_kindName[StubVal::Variable];
            s->d_kind = StubVal::Variable;
            if( s->d_type == 0 )
                s->d_type = typeOfExpression(ds,expr);

        }
    }else if( dopl.last().d_op == ArrayOp )
    {
        Q_ASSERT( dopl.size() > 1 );
        if( StubVal* s = dynamic_cast<StubVal*>( const_cast<NamedThing*>(dopl[dopl.size()-2].d_sym) ) )
        {
            if( s->d_kind != StubVal::Unknown && s->d_kind != StubVal::Variable )
                qWarning() << "stubed member" << s->d_name << "of module" << s->d_mod->d_name <<
                              "first seen as" << StubVal::s_kindName[s->d_kind] <<
                              "redeclaring to" << StubVal::s_kindName[StubVal::Variable];
            s->d_kind = StubVal::Variable;
            if( s->d_type == 0 )
                s->d_type = typeOfExpression(ds,expr);
            // TODO: check and update type of StubVal
        }
    }
}

void CodeModel::checkCaseStatement(CodeModel::DeclarationSequence* ds, SynTree* st)
{
    Q_ASSERT( st->d_tok.d_type == SynTree::R_CaseStatement && st->d_children.size() >= 4 );

    QList< QPair< QPair<const Type*, SynTree*>,SynTree*> > cases;
    const NamedThing* var = 0;
    SynTree* id = flatten(st->d_children[1]);
    if( id->d_tok.d_type == Tok_ident )
        var = ds->findByName( id->d_tok.d_val ) ;
    if( var == 0 )
        goto NormalCaseStatement;
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
    for( int i = 0; i < cases.size(); i++ )
    {
        checkNames(ds,cases[i].first.second);
        DeclarationSequence scope;
        scope.d_outer = ds;
        TypeAlias alias;
        alias.d_newType = cases[i].first.first;
        alias.d_alias = const_cast<NamedThing*>(var);
        scope.d_names.insert(id->d_tok.d_val,&alias);
        checkNames( &scope, cases[i].second );
    }
    checkNames(ds,st->d_children[1]);
    return;
NormalCaseStatement:
    foreach( SynTree* sub, st->d_children )
        checkNames(ds,sub);
}

CodeModel::Type*CodeModel::parseType(CodeModel::DeclarationSequence* ds, SynTree* t)
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

CodeModel::Type*CodeModel::parseTypeRef(CodeModel::DeclarationSequence* ds, SynTree* t)
{
    Q_ASSERT( t->d_tok.d_type == SynTree::R_qualident );
    TypeRef* res = new TypeRef();
    // in der ersten Runde werden qualidents noch nicht aufgelöst da sonst Reihenfolge in TYPE massgebend wird.
    res->d_def = t;
    res->d_typeSt = t;
    ds->d_types.append(res);
    return res;
}

CodeModel::Type*CodeModel::parsePointerType(CodeModel::DeclarationSequence* ds, SynTree* t)
{
    Q_ASSERT( t->d_children.size() > 1 && t->d_children[2]->d_tok.d_type == SynTree::R_type );
    Type* tp = parseType(ds, t->d_children[2] );
    PointerType* res = new PointerType(tp);
    res->d_def = t;
    ds->d_types.append(res);
    return res;
}

CodeModel::Type*CodeModel::parseRecordType(CodeModel::DeclarationSequence* ds, SynTree* t)
{
    SynTree* st = findFirstChild( t, SynTree::R_BaseType );
    Type* bt = 0;
    if( st )
    {
        Q_ASSERT( !st->d_children.isEmpty() && st->d_children.first()->d_tok.d_type == SynTree::R_qualident );
        bt = parseTypeRef(ds,st->d_children.first());
    }

    RecordType* res = new RecordType();
    res->d_base = bt;
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
                    Variable* f = new Variable();
                    f->d_name = name;
                    f->d_public = id.second;
                    f->d_def = i;
                    f->d_id = id.first;
                    f->d_type = tp;
                    res->d_fields.insert( name, f );
                    index(id.first,f);
                }
            }
        }
    }
    return res;
}

CodeModel::Type*CodeModel::parseArrayType(CodeModel::DeclarationSequence* ds, SynTree* t)
{
    SynTree* ll = findFirstChild( t, SynTree::R_LengthList );
    Q_ASSERT( ll != 0 && !ll->d_children.isEmpty() && ll->d_children.first()->d_tok.d_type == SynTree::R_expression );
    Q_ASSERT( !t->d_children.isEmpty() && t->d_children.last()->d_tok.d_type == SynTree::R_type );

    Type* tp = parseType(ds,t->d_children.last());
    ArrayType* res = new ArrayType();
    res->d_def = t;
    res->d_type = tp;
    res->d_dim = ll->d_children.first();
    ds->d_types.append(res);
    ArrayType* last = res;
    for( int i = 1; i < ll->d_children.size(); i++ )
    {
        Q_ASSERT( ll->d_children[i]->d_tok.d_type == SynTree::R_expression );
        ArrayType* cur = new ArrayType();
        last->d_type = cur;
        cur->d_def = t;
        cur->d_type = tp;
        cur->d_dim = ll->d_children[i];
        ds->d_types.append(cur);
        last = cur;
    }
    return res;
}

CodeModel::Type*CodeModel::parseProcType(CodeModel::DeclarationSequence* ds, SynTree* t)
{
    ProcType* res = new ProcType();
    res->d_def = t;
    ds->d_types.append(res);

    SynTree* fp = findFirstChild( t, SynTree::R_FormalParameters );
    res->d_returns = parseFormalParams(ds,fp,res->d_params);

    return res;
}

CodeModel::Type*CodeModel::parseFormalParams(CodeModel::DeclarationSequence* ds, SynTree* fp, QList<FormalParam*>& params)
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
                if( sec->d_children.last()->d_children.size() > 1 )
                {
                    Q_ASSERT( sec->d_children.last()->d_children.first()->d_tok.d_type == Tok_ARRAY );
                    ArrayType* arr = new ArrayType();
                    arr->d_def = sec->d_children.last()->d_children.first();
                    arr->d_type = tp;
                    ds->d_types.append(arr);
                    tp = arr;
                }
                bool var = sec->d_children.first()->d_tok.d_type == Tok_VAR;
                foreach( SynTree* id, sec->d_children )
                {
                    if( id->d_tok.d_type == Tok_ident )
                    {
                        FormalParam* p = new FormalParam(tp,var);
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

void CodeModel::resolveTypeRefs(CodeModel::DeclarationSequence* ds)
{
    // hier werden nun alle qualidents aufgelöst; im Fehlerfall bleibt d_type jedoch 0!
    foreach( Type* t, ds->d_types )
    {
        if( TypeRef* r = dynamic_cast<TypeRef*>(t) )
        {
            //const NamedThing* nt = derefQualident(ds,r->d_typeSt);
            Quali q = derefQualident( ds, r->d_typeSt, true, d_synthesize );

            if( q.second.first == 0 )
                continue; // ID wurde nicht gefunden

            const Type* tp = dynamic_cast<const Type*>(q.second.first);
            if( tp == 0 )
            {
                d_errs->error( Errors::Semantics, r->d_typeSt->d_tok.d_sourcePath, r->d_typeSt->d_tok.d_lineNr,
                               r->d_typeSt->d_tok.d_colNr, tr("qualident doesn't reference a type") );
            }else
                r->d_type = tp;
        }
    }
    foreach( Procedure* p, ds->d_procs )
        resolveTypeRefs(p);
}

#if 0
const CodeModel::NamedThing*CodeModel::derefQualident(CodeModel::DeclarationSequence* ds, SynTree* t)
{
    Q_ASSERT( false ); // obsolet, use derefQualident2
    Q_ASSERT( t->d_tok.d_type == SynTree::R_qualident );
    Q_ASSERT( !t->d_children.isEmpty() && t->d_children.first()->d_tok.d_type == Tok_ident );

    SynTree* id1 = t->d_children.first();
    const NamedThing* nt = ds->findByName(id1->d_tok.d_val);
    if( nt == 0 )
    {
        d_errs->error( Errors::Semantics, id1->d_tok.d_sourcePath, id1->d_tok.d_lineNr, id1->d_tok.d_colNr,
                       tr("ident '%1' not found").arg(id1->d_tok.d_val.data()) );
        return 0;
    }
    if( t->d_children.size() > 1 )
    {
        Q_ASSERT( t->d_children.last()->d_tok.d_type == Tok_ident );
        SynTree* id2 = t->d_children.last();
        if( const Module* m = dynamic_cast<const Module*>(nt) )
        {
            nt = m->findByName(id2->d_tok.d_val);
        }else
        {
            d_errs->error( Errors::Semantics, id1->d_tok.d_sourcePath, id1->d_tok.d_lineNr, id1->d_tok.d_colNr,
                           tr("referenced '%1' is not a module").arg(id1->d_tok.d_val.data()) );
            return 0;
        }
        if( nt == 0 )
        {
            d_errs->error( Errors::Semantics, id1->d_tok.d_sourcePath, id1->d_tok.d_lineNr, id1->d_tok.d_colNr,
                           tr("ident '%1.%2' not found").arg(id1->d_tok.d_val.data())
                           .arg(id2->d_tok.d_val.data()) );
            return 0;
        }
    }
    Q_ASSERT( nt != 0 );
    return nt;
}
#endif

CodeModel::Quali CodeModel::derefQualident(CodeModel::DeclarationSequence* ds, SynTree* t, bool report, bool synthesize)
{
    Q_ASSERT( t->d_tok.d_type == SynTree::R_qualident );
    Q_ASSERT( !t->d_children.isEmpty() && t->d_children.first()->d_tok.d_type == Tok_ident );

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
                d_errs->error( Errors::Semantics, id1->d_tok.d_sourcePath, id1->d_tok.d_lineNr, id1->d_tok.d_colNr,
                           tr("module '%1' not imported").arg(id1->d_tok.d_val.data()) );
            Q_ASSERT( m == 0 && id1 != 0 && nt == 0 && id2 == 0 );
            return Quali(qMakePair(m,id1),qMakePair(nt,id2));
        }
        Q_ASSERT( t->d_children.last()->d_tok.d_type == Tok_ident );
        m = dynamic_cast<const Module*>(nt);
        if( m == 0 )
        {
            if( report )
                d_errs->error( Errors::Semantics, id1->d_tok.d_sourcePath, id1->d_tok.d_lineNr, id1->d_tok.d_colNr,
                           tr("referenced '%1' is not a module").arg(id1->d_tok.d_val.data()) );
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
            StubType* s = new StubType();
            mm->d_types.append(s);
            s->d_name = id2->d_tok.d_val;
            s->d_public = true;
            s->d_mod = mm;
            mm->d_names.insert( s->d_name, s );
            nt = s;
        }else if( nt == 0 && report )
        {
            d_errs->error( Errors::Semantics, id1->d_tok.d_sourcePath, id1->d_tok.d_lineNr, id1->d_tok.d_colNr,
                           tr("ident '%2' not found in module '%1'").arg(id1->d_tok.d_val.data())
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
            d_errs->error( Errors::Semantics, id2->d_tok.d_sourcePath, id2->d_tok.d_lineNr, id2->d_tok.d_colNr,
                           tr("local ident '%1' not found").arg(id2->d_tok.d_val.data()) );
        }
        Q_ASSERT( m == 0 && id1 == 0 && id2 != 0 );
        if( report )
            index(id2,nt);
        return Quali(qMakePair(m,id1),qMakePair(nt,id2));
    }
}

CodeModel::DesigOpList CodeModel::derefDesignator(CodeModel::DeclarationSequence* ds, SynTree* t,
                                                               bool report, bool synthesize)
{
    DesigOpList desig; // flattended designator

    Q_ASSERT( t->d_tok.d_type == SynTree::R_designator && !t->d_children.isEmpty() &&
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
            d_errs->error( Errors::Semantics, dP->d_tok.d_sourcePath, dP->d_tok.d_lineNr, dP->d_tok.d_colNr,
                           tr("ident '%1' not found").arg(toString(desig.mid(0,1))) );
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
                d_errs->error( Errors::Semantics, dP->d_tok.d_sourcePath, dP->d_tok.d_lineNr, dP->d_tok.d_colNr,
                               tr("modules cannot be idirectly designated '%1'").arg(toString(desig.mid(0,i))) );
            break;
        }
        DesigOpErr err;
        const NamedThing* next = applyDesigOp( ds, desig[i-1].d_sym, desig[i], &err, synthesize );
        if( err == InvalidOperation )
        {
            if( report )
                d_errs->error( Errors::Semantics, dP->d_tok.d_sourcePath, dP->d_tok.d_lineNr, dP->d_tok.d_colNr,
                               tr("invalid operation '%1' on designator '%2'")
                           .arg(toString(desig.mid(i,1),true)).arg(toString(desig.mid(0,i))) );
            break;
        }else if( err == MissingType )
        {
            break; // fehlender Type wurde bereits in resolveTypeRefs gemeldet
        }else if( err == NotFound )
        {
            if( report )
                d_errs->error( Errors::Semantics, dP->d_tok.d_sourcePath, dP->d_tok.d_lineNr, dP->d_tok.d_colNr,
                               tr("ident '%1' not found").arg(toString(desig.mid(0,i+1))) );
            break;
        }else
        {
            desig[i].d_sym = next;
            if( desig[i].d_op == IdentOp && report )
                index(desig[i].d_arg,desig[i].d_sym);
        }
    }
    return desig;
}

const CodeModel::NamedThing*CodeModel::applyDesigOp(DeclarationSequence* ds, const CodeModel::NamedThing* input,
                                                    const DesigOp& dop, DesigOpErr* errOut, bool synthesize)
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
                    StubVal* c = new StubVal();
                    c->d_name = dop.d_arg->d_tok.d_val;
                    c->d_public = true;
                    c->d_mod = mm;
                    mm->d_stubs.append(c);
                    mm->d_names.insert( c->d_name, c );
                    res = c;
                }else
                    err = NotFound;
            }
        }else
            err = InvalidOperation;
    }else if( const TypeAlias* ac = dynamic_cast<const TypeAlias*>( input ) )
    {
        if( Variable* v = dynamic_cast<Variable*>( ac->d_alias ) )
        {
            Type* oldType = v->d_type;
            v->d_type = const_cast<Type*>(ac->d_newType);
            res = applyDesigOp( ds, v, dop, &err, synthesize );
            v->d_type = oldType;
        }else
            Q_ASSERT( false );

    }else if( const Variable* v = dynamic_cast<const Variable*>( input ) )
    {
        // Für Modul-Variablen und Record-Felder
        if( dop.d_op == TypeOp )
        {
            res = dop.d_sym;
            if( res == 0 )
                err = MissingType;
        }else
        {
            const Type* vt = const_cast<Type*>( v->d_type );
            if( vt == 0 )
                err = MissingType;
            else
                res = applyDesigOp( ds, derefed(vt), dop, &err, synthesize );
        }
    }else if( const FormalParam* v = dynamic_cast<const FormalParam*>( input ) )
    {
        Type* vt = const_cast<Type*>( v->d_type );
        if( vt == 0 )
            err = MissingType;
        else
            res = applyDesigOp( ds, derefed(vt), dop, &err, synthesize );
    }else if( const Constant* v = dynamic_cast<const Constant*>( input ) )
    {
        err = InvalidOperation;
    }else if( const ArrayType* v = dynamic_cast<const ArrayType*>( input ) )
    {
        if( dop.d_op == ArrayOp )
            res = v->d_type; // don't deref
        else
            err = InvalidOperation;
        if( res == 0 )
            err = MissingType;
    }else if( const RecordType* v = dynamic_cast<const RecordType*>( input ) )
    {
        if( dop.d_op == IdentOp )
        {
            res = v->findByName(dop.d_arg->d_tok.d_val);
            if( res == 0 )
                err = NotFound;
        }else
            err = InvalidOperation;
    }else if( const PointerType* v = dynamic_cast<const PointerType*>( input ) )
    {
        const Type* t = derefed(v->d_type);
        if( dop.d_op == PointerOp )
            res = v->d_type;
        else if( dop.d_op == IdentOp && dynamic_cast<const RecordType*>(t) )
            res = applyDesigOp( ds, t, dop, &err, synthesize );
        else
            err = InvalidOperation;
    }else if( const ProcType* v = dynamic_cast<const ProcType*>( input ) )
    {
        if( dop.d_op == ProcedureOp )
            res = v->d_returns;
        else
            err = InvalidOperation;
    }else if( const TypeRef* v = dynamic_cast<const TypeRef*>( input ) )
    {
        res = applyDesigOp( ds, v->deref(), dop, &err, synthesize );
    }else if( dynamic_cast<const BasicType*>( input ) )
    {
        err = InvalidOperation;
    }else if( const Procedure* v = dynamic_cast<const Procedure*>( input ) )
    {
        if( dop.d_op == ProcedureOp )
        {
            res = v->d_returns;
        }else
        {
            err = InvalidOperation;
        }
    }else if( const PredefProc* p = dynamic_cast<const PredefProc*>( input ) )
    {
        if( dop.d_op == ProcedureOp )
        {
            res = typeOfExpression(ds,p->d_proc,dop.d_arg);
        }else
        {
            err = InvalidOperation;
        }
    }else if( const BasicType* b = dynamic_cast<const BasicType*>( input ) )
    {
        err = InvalidOperation;
    }else if( const StubType* tc = dynamic_cast<const StubType*>( input ) )
    {
        if( dop.d_op == IdentOp )
        {
            res = tc->findByName(dop.d_arg->d_tok.d_val);
            if( res == 0 && synthesize )
            {
                StubType* t = const_cast<StubType*>(tc);
                if( t->d_kind != StubType::Record && t->d_kind != StubType::Unknown )
                    qWarning() << "stubed" << t->d_name << "first seen as" << StubType::s_kindName[t->d_kind] <<
                                  "redeclaring to" << StubType::s_kindName[StubType::Record];
                qDebug() << "synthesizing field" << dop.d_arg->d_tok.d_val << "in record" << t->d_name <<
                            "of module" << t->d_mod->d_name;
                t->d_kind = StubType::Record;
                StubVal* c = new StubVal();
                c->d_kind = StubVal::Variable;
                c->d_name = dop.d_arg->d_tok.d_val;
                c->d_public = true;
                c->d_type = 0;
                c->d_mod = tc->d_mod;
                t->d_fields.insert( c->d_name, c );
                res = c;
            }
            if( res == 0 )
                err = NotFound;
        }else
            err = InvalidOperation;
    }else if( const StubVal* vc = dynamic_cast<const StubVal*>( input ) )
    {
        StubVal* v = const_cast<StubVal*>(vc);
        if( dop.d_op == ProcedureOp )
        {
            if( vc->d_kind != StubVal::Procedure && vc->d_kind != StubVal::Unknown )
                qWarning() << "stubed member" << vc->d_name << "of module" << vc->d_mod->d_name <<
                              "first seen as" << StubVal::s_kindName[vc->d_kind] <<
                              "redeclaring to" << StubVal::s_kindName[StubVal::Procedure];
            v->d_kind = StubVal::Procedure;
            if( dop.d_arg != 0 && v->d_params.isEmpty() )
            {
                Q_ASSERT( dop.d_arg->d_tok.d_type == SynTree::R_ExpList );
                for( int i = 0; i < dop.d_arg->d_children.size(); i++ )
                {
                    StubVal* s = new StubVal();
                    s->d_kind = StubVal::Param;
                    s->d_name = QByteArray::number(i);
                    s->d_public = true;
                    s->d_type = typeOfExpression(ds,dop.d_arg->d_children[i]);
                    s->d_expr = dop.d_arg->d_children[i];
                    v->d_params.insert(s->d_name,s);
                }
                v->d_expr = dop.d_arg;
            }
        }else if( dop.d_op == IdentOp )
        {
            if( vc->d_kind != StubVal::Procedure && vc->d_kind != StubVal::Unknown )
                qWarning() << "stubed member" << vc->d_name << "of module" << vc->d_mod->d_name <<
                              "first seen as" << StubVal::s_kindName[vc->d_kind] <<
                              "redeclaring to" << StubVal::s_kindName[StubVal::Variable];
            if( vc->d_type == 0 )
            {
                StubType* st = new StubType();
                st->d_kind = StubType::Record;
                st->d_mod = v->d_mod;
                v->d_type = st;
                v->d_mod->d_types.append(st);
            }
            res = applyDesigOp( ds, vc->d_type, dop, &err, synthesize );
        }else
            err = InvalidOperation;
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

SynTree*CodeModel::findFirstChild(SynTree* st, int type)
{
    if( st == 0 )
        return 0;
    foreach( SynTree* sub, st->d_children )
    {
        if( sub->d_tok.d_type == type )
            return sub;
    }
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

CodeModel::DesigOpList CodeModel::derefDesignator(CodeModel::DeclarationSequence* ds, const SynTree* st)
{
    return derefDesignator(ds,const_cast<SynTree*>(st),false,false);
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

const CodeModel::Type*CodeModel::typeOfExpression(DeclarationSequence* ds, SynTree* st) const
{
    if( st == 0 )
        return 0;

    switch( st->d_tok.d_type )
    {
    case Tok_integer:
        return d_scope.d_intType;
    case Tok_real:
        return d_scope.d_realType;
    case Tok_string:
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
            DesigOpList nt = const_cast<CodeModel*>(this)->derefDesignator( ds, st, false, false );
            if( nt.isEmpty() )
                return 0;
            else if( const Constant* c = dynamic_cast<const Constant*>(nt.last().d_sym) )
            {
                if( const Module* m = dynamic_cast<const Module*>( nt.first().d_sym ) )
                    ds = const_cast<Module*>(m);
                return typeOfExpression( ds, c->d_expr );
            }
            else if( const Variable* v = dynamic_cast<const Variable*>(nt.last().d_sym) )
                return v->d_type;
            else if( nt.last().d_sym != 0 )
                return dynamic_cast<const Type*>(nt.last().d_sym);
            else
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

const CodeModel::Type*CodeModel::typeOfExpression(DeclarationSequence* ds,
                                                  PredefProc::Meta m, SynTree* args) const
{
    switch( m )
    {
    case PredefProc::ABS:
        return typeOfExpression(ds,args);
    case PredefProc::ODD:
        return d_scope.d_boolType;
    case PredefProc::LEN:
    case PredefProc::LSL:
    case PredefProc::ASR:
    case PredefProc::ROR:
    case PredefProc::FLOOR:
        return d_scope.d_intType;
    case PredefProc::FLT:
        return d_scope.d_realType;
    case PredefProc::CHR:
        return d_scope.d_charType;
    default:
        return 0;
    }
}

void CodeModel::index(const SynTree* idUse, const CodeModel::NamedThing* decl)
{
    if( !d_trackIds )
        return;
    Q_ASSERT( idUse != 0 && decl != 0 );
    d_dir[idUse->d_tok.d_sourcePath].append( IdentUse(idUse,decl) );
    d_revDir.insert(decl,idUse);
}

CodeModel::GlobalScope::~GlobalScope()
{
    foreach( PredefProc* p, d_procs )
        delete p;
    foreach( Module* m, d_mods )
        delete m;
    foreach( Type* t, d_types )
        delete t;
}

CodeModel::DeclarationSequence::~DeclarationSequence()
{
    foreach( Procedure* p, d_procs )
        delete p;
    foreach( Constant* c, d_consts )
        delete c;
    foreach( Type* t, d_types )
        delete t;
    foreach( Variable* v, d_vars )
        delete v;
    foreach( StubVal* v, d_stubs )
        delete v;
}

CodeModel::BasicType::BasicType(CodeModel::BasicType::Meta t):d_type(t)
{
    switch( t )
    {
    case BOOLEAN:
        d_name = Lexer::getSymbol("BOOLEAN");
        break;
    case CHAR:
        d_name = Lexer::getSymbol("CHAR");
        break;
    case INTEGER:
        d_name = Lexer::getSymbol("INTEGER");
        break;
    case REAL:
        d_name = Lexer::getSymbol("REAL");
        break;
    case BYTE:
        d_name = Lexer::getSymbol("BYTE");
        break;
    case SET:
        d_name = Lexer::getSymbol("SET");
        break;
    default:
        break;
    }
}

CodeModel::Procedure::~Procedure()
{
    foreach( FormalParam* v, d_params )
        delete v;
}

CodeModel::PredefProc::PredefProc(CodeModel::PredefProc::Meta t):d_proc(t)
{
    switch(t)
    {
    case ABS:
        d_name = Lexer::getSymbol("ABS");
        break;
    case ODD:
        d_name = Lexer::getSymbol("ODD");
        break;
    case LEN:
        d_name = Lexer::getSymbol("LEN");
        break;
    case LSL:
        d_name = Lexer::getSymbol("LSL");
        break;
    case ASR:
        d_name = Lexer::getSymbol("ASR");
        break;
    case ROR:
        d_name = Lexer::getSymbol("ROR");
        break;
    case FLOOR:
        d_name = Lexer::getSymbol("FLOOR");
        break;
    case FLT:
        d_name = Lexer::getSymbol("FLT");
        break;
    case ORD:
        d_name = Lexer::getSymbol("ORD");
        break;
    case CHR:
        d_name = Lexer::getSymbol("CHR");
        break;
    case INC:
        d_name = Lexer::getSymbol("INC");
        break;
    case DEC:
        d_name = Lexer::getSymbol("DEC");
        break;
    case INCL:
        d_name = Lexer::getSymbol("INCL");
        break;
    case EXCL:
        d_name = Lexer::getSymbol("EXCL");
        break;
    case NEW:
        d_name = Lexer::getSymbol("NEW");
        break;
    case ASSERT:
        d_name = Lexer::getSymbol("ASSERT");
        break;
    case PACK:
        d_name = Lexer::getSymbol("PACK");
        break;
    case UNPK:
        d_name = Lexer::getSymbol("UNPK");
        break;
    case LED:
        d_name = Lexer::getSymbol("LED");
        break;
    }
}

CodeModel::RecordType::~RecordType()
{
    Fields::iterator i;
    for( i = d_fields.begin(); i != d_fields.end(); ++i )
        delete (*i);
}

const CodeModel::Variable*CodeModel::RecordType::findByName(const QByteArray& n) const
{
    const Variable* res = d_fields.value(n);
    if( res == 0 && d_base != 0 )
    {
        const RecordType* parent = dynamic_cast<const RecordType*>( d_base->deref() );
        if( parent )
            res = parent->findByName(n);
    }

    return res;
}

CodeModel::ProcType::~ProcType()
{
    foreach( FormalParam* v, d_params )
        delete v;
}

const CodeModel::NamedThing*CodeModel::Scope::findByName(const QByteArray& name) const
{
    const CodeModel::NamedThing* thing = d_names.value(name);
    if( thing == 0 && d_outer )
        thing = d_outer->findByName(name);
    return thing;
}

const char* CodeModel::StubType::s_kindName[] =
{
    "?",
    "RECORD",
    "ARRAY",
    "POINTER",
    "PROC",
};

const char* CodeModel::StubVal::s_kindName[] =
{
    "?",
    "CONST",
    "VAR",
    "PROC",
    "PARAM"
};

CodeModel::StubType::~StubType()
{
    QMap<QByteArray,StubVal*>::iterator i;
    for( i = d_fields.begin(); i != d_fields.end(); ++i )
        delete (*i);
}


CodeModel::StubVal::~StubVal()
{
    QMap<QByteArray,StubVal*>::iterator i;
    for( i = d_params.begin(); i != d_params.end(); ++i )
        delete i.value();
}
