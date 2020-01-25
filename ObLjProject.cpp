/*
* Copyright 2020 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon parser/compiler library.
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

#include "ObLjProject.h"
#include "ObAst.h"
#include "ObErrors.h"
#include "ObLjbcGen.h"
#include <QBuffer>
#include <QDir>
#include <QtDebug>
#include <QSettings>
using namespace Ob;
using namespace Ast;

struct HitTest : public AstVisitor
{
    quint32 line; quint16 col;

    void test(Expression* e)
    {
        if( e == 0 )
            return;
        if( e->d_loc.d_row > line )
            return;
        Named* n = e->getIdent();
        if( n == 0 )
            return;
        if( line == e->d_loc.d_row && col >= e->d_loc.d_col && col <= e->d_loc.d_col + n->d_name.size() )
            throw e;
    }

    void visit( Pointer* p )
    {
        if( p->d_to->d_ident == 0 )
            p->d_to->accept(this); // look for qualis
    }

    void visit( Array* a )
    {
        if( a->d_type->d_ident == 0 )
            a->d_type->accept(this); // look for qualis
    }

    void visit( Record* r )
    {
        if( !r->d_base.isNull() )
            r->d_base->accept(this); // look for qualis
        for( int i = 0; i < r->d_fields.size(); i++ )
        {
            r->d_fields[i]->accept(this); // look for qualis
        }
    }

    void visit( ProcType* p )
    {
        if( p->d_return && p->d_return->d_ident == 0 )
            p->d_return->accept(this); // look for qualis
        for( int i = 0; i < p->d_formals.size(); i++ )
            p->d_formals[i]->accept(this); // look for qualis
    }

    void visit( QualiType* q )
    {
        q->d_quali->accept(this);
    }

    void visit( Field* f )
    {
        if( f->d_type->d_ident == 0 )
            f->d_type->accept(this);
        // TODO test(f);
    }

    void visit( Variable* v )
    {
        if( v->d_type->d_ident == 0 )
            v->d_type->accept(this);
    }

    void visit( LocalVar* v )
    {
        if( v->d_type->d_ident == 0 )
            v->d_type->accept(this);
    }

    void visit( Parameter* p )
    {
        if( p->d_type->d_ident == 0 )
            p->d_type->accept(this);
    }

    void visit( NamedType* t )
    {
        if( t->d_type->d_ident == t )
            t->d_type->accept(this);
    }

    void visit( Const* c )
    {
        c->d_constExpr->accept(this);
    }

    void visit( Import* i )
    {
    }

    void visit( Procedure* m)
    {
        if( m->d_type->d_ident == 0 )
            m->d_type->accept(this);

        for( int i = 0; i < m->d_order.size(); i++ )
            m->d_order[i]->accept(this);
        for( int i = 0; i < m->d_body.size(); i++ )
            m->d_body[i]->accept(this);
        for( int i = 0; i < m->d_helper.size(); i++ )
            m->d_helper[i]->accept(this);
    }

    void visit( Module* m )
    {
        for( int i = 0; i < m->d_order.size(); i++ )
            m->d_order[i]->accept(this);
        for( int i = 0; i < m->d_body.size(); i++ )
            m->d_body[i]->accept(this);
        for( int i = 0; i < m->d_helper.size(); i++ )
            m->d_helper[i]->accept(this);
    }

    void visit( Call* c )
    {
        c->d_what->accept(this);
    }

    void visit( Return* r )
    {
        r->d_what->accept(this);
    }

    void visit( Assign* a )
    {
        a->d_lhs->accept(this);
        a->d_rhs->accept(this);
    }

    void visit( IfLoop* l )
    {
        for( int i = 0; i < l->d_if.size(); i++ )
            l->d_if[i]->accept(this);
        for( int i = 0; i < l->d_then.size(); i++ )
        {
            const StatSeq& then = l->d_then[i];
            for( int j = 0; j < then.size(); j++ )
                then[j]->accept(this);
        }
        for( int i = 0; i < l->d_else.size(); i++ )
            l->d_else[i]->accept(this);
    }

    void visit( ForLoop* l )
    {
        l->d_id->accept(this);
        l->d_from->accept(this);
        l->d_to->accept(this);
        l->d_by->accept(this);
        for( int i = 0; i < l->d_do.size(); i++ )
            l->d_do[i]->accept(this);
    }

    void visit( CaseStmt* c )
    {
        c->d_exp->accept(this);
        for( int i = 0; i < c->d_cases.size(); i++ )
        {
            for( int j = 0; j < c->d_cases[i].d_labels.size(); j++ )
                c->d_cases[i].d_labels[j]->accept(this);
            for( int j = 0; j < c->d_cases[i].d_block.size(); j++ )
                c->d_cases[i].d_block[j]->accept(this);
        }
    }

    void visit( Literal* ) {}

    void visit( SetExpr* s )
    {
        for( int i = 0; i < s->d_parts.size(); i++ )
            s->d_parts[i]->accept(this);
    }

    void visit( IdentLeaf* id )
    {
        test(id);
    }

    void visit( UnExpr* e )
    {
        e->d_sub->accept(this);
    }

    void visit( IdentSel* e )
    {
        e->d_sub->accept(this);
        test(e);
    }

    void visit( CallExpr* c )
    {
        c->d_sub->accept(this);
        for( int i = 0; i < c->d_actuals.size(); i++ )
            c->d_actuals[i]->accept(this);
    }

    void visit( BinExpr* e )
    {
        e->d_lhs->accept(this);
        e->d_rhs->accept(this);
    }
};

Project::Project(QObject *parent) : QObject(parent),d_dirty(false),d_useBuiltInOakwood(false)
{
    d_mdl = new Ast::Model(this);
    d_mdl->setSenseExt(true);
    d_mdl->getErrs()->setRecord(true);
    d_mdl->setFillXref(true);

    d_suffixes << ".Mod" << ".obn";
}

void Project::clear()
{
    d_mdl->clear();
    d_filePath.clear();
    d_files.clear();
}

void Project::createNew()
{
    clear();
    d_filePath.clear();
    d_dirty = false;
    emit sigModified(d_dirty);
    emit sigRenamed();
}

bool Project::initializeFromDir(const QDir& dir, bool recursive)
{
    clear();
    d_dirty = false;

    QStringList files = findFiles(dir, recursive);
    foreach( const QString& f, files )
        d_files.insert(f,File(f));
    emit sigRenamed();
    return true;
}

void Project::setSuffixes(const QStringList& s)
{
    d_suffixes = s;
    touch();
}

void Project::setMain(const Project::ModProc& mp)
{
    d_main = mp;
    touch();
}

void Project::setUseBuiltInOakwood(bool on)
{
    d_useBuiltInOakwood = on;
    touch();
}

bool Project::addFile(const QString& path)
{
    if( d_files.contains(path) )
        return false;
    d_files.insert(path,File(path));
    touch();
    return true;
}

bool Project::removeFile(const QString& path)
{
    if( !d_files.contains(path) )
        return false;
    d_files.remove(path);
    touch();
    return true;
}

Project::FileList Project::getFilesInExecOrder() const
{
    const QList<Ast::Module*>& order = d_mdl->getProcessingOrder();
    FileList res;
    foreach( Ast::Module* m, order )
    {
        Q_ASSERT( m );
        FileHash::const_iterator i = d_files.find( m->d_file );
        if( i == d_files.end() || i.value().d_bc.isEmpty() || i.value().d_mod.isNull() )
            continue;
        res.append( i.value() );
    }
    return res;
}

Ast::Expression* Project::findSymbolBySourcePos(const QString& file, quint32 line, quint16 col) const
{
    FileHash::const_iterator i = d_files.find(file);
    if( i == d_files.end() || i.value().d_mod->d_hasErrors )
        return 0;
    try
    {
        HitTest hit;
        hit.col = col;
        hit.line = line;
        i.value().d_mod->accept(&hit);
    }catch( Expression* e )
    {
        return e;
    }catch(...)
    {

    }
    return 0;
}

Ast::Model::ExpList Project::getUsage(Named* n) const
{
    const Model::XRef& xref = d_mdl->getXref();
    return xref.value(n);
}

Errors* Project::getErrs() const
{
    return d_mdl->getErrs();
}

FileCache* Project::getFc() const
{
    return d_mdl->getFc();
}

QStringList Project::findFiles(const QDir& dir, bool recursive)
{
    QStringList res;
    QStringList files;

    if( recursive )
    {
        files = dir.entryList( QDir::Dirs | QDir::NoDotAndDotDot, QDir::Name );

        foreach( const QString& f, files )
            res += findFiles( QDir( dir.absoluteFilePath(f) ), recursive );
    }

    QStringList suff = d_suffixes;
    for(int i = 0; i < suff.size(); i++ )
        suff[i] = "*" + suff[i];
    files = dir.entryList( suff, QDir::Files, QDir::Name );
    foreach( const QString& f, files )
    {
        res.append( dir.absoluteFilePath(f) );
    }
    return res;
}

void Project::touch()
{
    if( !d_dirty )
    {
        d_dirty = true;
        emit sigModified(d_dirty);
    }
}

bool Project::recompile()
{
    const bool res = d_mdl->parseFiles( d_files.keys() );
    Ast::Model::Modules mods = d_mdl->getModules();
    foreach( const Ast::Ref<Ast::Module>& m, mods )
    {
        if( m->d_file.isEmpty() || m->d_isDef )
            continue; // e.g. SYSTEM
        FileHash::iterator i = d_files.find(m->d_file);
        if( i != d_files.end() )
            i.value().d_mod = m;
        else
        {
            qDebug() << "missing" << m->d_name;
            Q_ASSERT( false );
        }
    }
    emit sigRecompiled();
    return res;
}

bool Project::generate()
{
    const quint32 errs = d_mdl->getErrs()->getErrCount();
    FileHash::iterator i;
    for( i = d_files.begin(); i != d_files.end(); ++i )
    {
        if( i.value().d_mod.isNull() || i.value().d_mod->d_hasErrors || i.value().d_mod->d_isDef )
            i.value().d_bc.clear();
        else
        {
            qDebug() << "generating" << i.value().d_mod->d_name;
            QBuffer buf;
            buf.open(QIODevice::WriteOnly);
            LjbcGen::translate(i.value().d_mod.data(), &buf, d_mdl->getErrs() );
            buf.close();
            i.value().d_bc = buf.buffer();
        }
    }
    return errs == d_mdl->getErrs()->getErrCount();
}

bool Project::save()
{
    if( d_filePath.isEmpty() )
        return false;

    QDir dir = QFileInfo(d_filePath).dir();

    QSettings out(d_filePath,QSettings::IniFormat);
    if( !out.isWritable() )
        return false;

    out.setValue("Suffixes", d_suffixes );
    out.setValue("BuiltInOakwood", d_useBuiltInOakwood );
    out.setValue("MainModule", d_main.first );
    out.setValue("MainProc", d_main.second );

    out.beginWriteArray("Modules", d_files.size() );
    FileHash::const_iterator i;
    int n = 0;
    for( i = d_files.begin(); i != d_files.end(); ++i )
    {
        const QString absPath = i.key();
        const QString relPath = dir.relativeFilePath( absPath );
        out.setArrayIndex(n++);
        out.setValue("AbsPath", absPath );
        out.setValue("RelPath", relPath );
    }
    out.endArray();

    d_dirty = false;
    emit sigModified(d_dirty);
    return true;
}

bool Project::loadFrom(const QString& filePath)
{
    clear();

    d_filePath = filePath;

    QDir dir = QFileInfo(d_filePath).dir();

    QSettings in(d_filePath,QSettings::IniFormat);

    d_suffixes = in.value("Suffixes").toStringList();
    d_useBuiltInOakwood = in.value("BuiltInOakwood").toBool();
    d_main.first = in.value("MainModule").toByteArray();
    d_main.second = in.value("MainProc").toByteArray();

    const int count = in.beginReadArray("Modules");

    for( int i = 0; i < count; i++ )
    {
        in.setArrayIndex(i);
        QString absPath = in.value("AbsPath").toString();
        const QString relPath = in.value("RelPath").toString();
        if( QFileInfo(absPath).exists() )
            d_files.insert( absPath, File(absPath) );
        else
        {
            absPath = dir.absoluteFilePath(relPath);
            if( QFileInfo(absPath).exists() )
                d_files.insert( absPath, File(absPath) );
            else
                qCritical() << "Could not open module" << relPath;
        }
    }

    in.endArray();

    d_dirty = false;
    emit sigModified(d_dirty);
    emit sigRenamed();
    return true;
}

bool Project::saveTo(const QString& filePath)
{
    d_filePath = filePath;
    const bool res = save();
    emit sigRenamed();
    return res;
}

