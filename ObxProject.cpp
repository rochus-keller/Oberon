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

#include "ObxProject.h"
#include "ObxAst.h"
#include "ObxModel.h"
#include "ObErrors.h"
#include "ObxLjbcGen.h"
#include <QBuffer>
#include <QDir>
#include <QtDebug>
#include <QSettings>
#include <QCoreApplication>
#include <qdatetime.h>
using namespace Obx;
using namespace Ob;

struct ObxHitTest : public AstVisitor
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
        //if( p->d_to->d_ident == 0 )
        if( p->d_flag )
            p->d_flag->accept(this);
        if( p->d_to )
            p->d_to->accept(this); // look for qualis
    }

    void visit( Array* a )
    {
        //if( a->d_type->d_ident == 0 )
        if( a->d_flag )
            a->d_flag->accept(this);
        if( a->d_type )
            a->d_type->accept(this); // look for qualis
        if( a->d_lenExpr )
            a->d_lenExpr->accept(this);
    }

    void visit( Record* r )
    {
        if( r->d_flag )
            r->d_flag->accept(this);
        if( !r->d_base.isNull() )
            r->d_base->accept(this); // look for qualis
        for( int i = 0; i < r->d_fields.size(); i++ )
            r->d_fields[i]->accept(this); // look for qualis
    }

    void visit( ProcType* p )
    {
        if( p->d_return ) // && p->d_return->d_ident == 0 )
            p->d_return->accept(this); // look for qualis
        for( int i = 0; i < p->d_formals.size(); i++ )
            p->d_formals[i]->accept(this); // look for qualis
    }

    void visit( QualiType* q )
    {
        if( q->d_quali )
            q->d_quali->accept(this);
    }

    void visit( Field* f )
    {
        //if( f->d_type->d_ident == 0 )
        if( f->d_type )
            f->d_type->accept(this);
        // TODO test(f);
    }

    void visit( Variable* v )
    {
        //if( v->d_type->d_ident == 0 )
        if( v->d_type )
            v->d_type->accept(this);
    }

    void visit( LocalVar* v )
    {
        //if( v->d_type->d_ident == 0 )
        if( v->d_type )
            v->d_type->accept(this);
    }

    void visit( Parameter* p )
    {
        //if( p->d_type->d_ident == 0 )
        if( p->d_type )
            p->d_type->accept(this);
    }

    void visit( NamedType* t )
    {
        if( t->d_type->d_decl == t )
            t->d_type->accept(this);
    }

    void visit( Const* c )
    {
        if( c->d_constExpr )
            c->d_constExpr->accept(this);
    }

    void visit( Import* i )
    {
        foreach( const Ref<Type>& a, i->d_metaActuals )
            a->accept(this);
    }

    void visit( Procedure* m)
    {
        //if( m->d_type->d_ident == 0 )
        if( m->d_type )
            m->d_type->accept(this);

        if( m->d_receiver )
            m->d_receiver->accept(this);

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
        if( c->d_what )
            c->d_what->accept(this);
    }

    void visit( Return* r )
    {
        if( r->d_what )
            r->d_what->accept(this);
    }

    void visit( Assign* a )
    {
        if( a->d_lhs)
            a->d_lhs->accept(this);
        if( a->d_rhs )
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
        if( l->d_id )
            l->d_id->accept(this);
        if( l->d_from )
            l->d_from->accept(this);
        if( l->d_to )
            l->d_to->accept(this);
        if( l->d_by )
            l->d_by->accept(this);
        for( int i = 0; i < l->d_do.size(); i++ )
            l->d_do[i]->accept(this);
    }

    void visit( CaseStmt* c )
    {
        if( c->d_exp )
            c->d_exp->accept(this);
        for( int i = 0; i < c->d_cases.size(); i++ )
        {
            for( int j = 0; j < c->d_cases[i].d_labels.size(); j++ )
                c->d_cases[i].d_labels[j]->accept(this);
            for( int j = 0; j < c->d_cases[i].d_block.size(); j++ )
                c->d_cases[i].d_block[j]->accept(this);
        }
    }

    void visit( Literal* ) {} // NOP

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
        if( e->d_sub )
            e->d_sub->accept(this);
    }

    void visit( IdentSel* e )
    {
        if( e->d_sub )
            e->d_sub->accept(this);
        test(e);
    }

    void visit( ArgExpr* c )
    {
        if( c->d_sub )
            c->d_sub->accept(this);
        for( int i = 0; i < c->d_args.size(); i++ )
            c->d_args[i]->accept(this);
    }

    void visit( BinExpr* e )
    {
        if( e->d_lhs )
            e->d_lhs->accept(this);
        if( e->d_rhs )
            e->d_rhs->accept(this);
    }

    virtual void visit( Enumeration* me )
    {
        foreach( const Ref<Const>& c, me->d_items )
            c->accept(this);
    }

    virtual void visit( GenericName* v)
    {
        //if( v->d_type->d_ident == 0 )
        if( v->d_type )
            v->d_type->accept(this);
    }

};

struct ObxModuleDump : public AstVisitor
{
    QTextStream out;
    int level;
    RowCol last;
    Module* mod;

    ObxModuleDump():level(0),mod(0){}

    QByteArray ws() const
    {
        return QByteArray( level, '\t' );
    }

    void checkNl( Thing* me )
    {
        if( last.isValid() && me->d_loc.d_row > last.d_row )
            out << endl << ws();
        last = me->d_loc;
    }

    void visit( BaseType* me )
    {
        Q_ASSERT( false );
        last = me->d_loc;
        out << QByteArray(me->getTypeName()).toLower();
    }
    void visit( Pointer* me )
    {
        last = me->d_loc;
        if( me->d_unsafe )
            out << "unsafe ";
        out << "pointer to ";
        if( me->d_to )
            me->d_to->accept(this);
        else
            out << "?";
    }
    void visit( Array* me)
    {
        last = me->d_loc;
        if( me->d_unsafe )
            out << "carray ";
        else
            out << "array ";
        if( me->d_lenExpr )
        {
            me->d_lenExpr->accept(this);
            out << " ";
        }
        out << "of ";
        if( me->d_type )
            me->d_type->accept(this);
        else
            out << "?";
    }
    void visit( Record* me )
    {
        last = me->d_loc;
        if( me->d_unsafe )
            out << "cstruct ";
        else
            out << "record ";
        if( me->d_base )
        {
            out << "(";
            me->d_base->accept(this);
            out << ") ";
        }
        level++;
        level++;
        for( int i = 0; i < me->d_fields.size(); i++ )
        {
            out << endl << ws();
            me->d_fields[i]->accept(this);
        }
        level--;
        out << endl << ws();
        out << "end";
        level--;
    }
    void visitPt( ProcType* me )
    {
        if( !me->d_formals.isEmpty() )
        {
            out << "(";
            for( int i = 0; i < me->d_formals.size(); i++ )
            {
                if( i != 0 )
                {
                    out << "; ";
                    checkNl(me->d_formals[i].data());
                }
                if( me->d_formals[i]->d_const )
                    out << "in ";
                else if( me->d_formals[i]->d_var )
                    out << "var ";
                me->d_formals[i]->accept(this);
            }
            out << ")";
        }
        if( !me->d_return.isNull() )
        {
            if( me->d_formals.isEmpty() )
                out << "()";
            out << ": ";
            me->d_return->accept(this);
        }
    }
    void visit( ProcType* me )
    {
        last = me->d_loc;
        out << "proc ";
        visitPt(me);
    }
    void visit( QualiType* me)
    {
        last = me->d_loc;
        if( me->d_quali )
            me->d_quali->accept(this);
        else
            out << "?";
    }
    void printVar( Named* me )
    {
        last = me->d_loc;
        visitIdentdef(me);
        out << ": ";
        if( me->d_type )
            me->d_type->accept(this);
        else
            out << "?";
    }
    void visit( Field* me)
    {
        printVar(me);
    }
    void visit( Variable* me)
    {
        printVar(me);
    }
    void visit( LocalVar* me)
    {
        printVar(me);
    }
    void visit( Parameter* me)
    {
        printVar(me);
    }
    void visit( NamedType* me)
    {
        checkNl(me);
        level++;
        visitIdentdef(me);
        out << " = ";
        if( me->d_type )
            me->d_type->accept(this);
        else
            out << "?";
        out << " ";
        level--;
    }
    void visit( Const* me)
    {
        checkNl(me);
        level++;
        visitIdentdef(me);
        out << " = ";
        if( me->d_constExpr )
            me->d_constExpr->accept(this);
        else
            out << "?";
        out << " ";
        level--;
    }
    static inline QByteArray modName( const QByteArrayList& in )
    {
        if( in.isEmpty() )
            return "SYSTEM";
        else
            return in.join('/');
    }

    void visit( Import* me)
    {
        checkNl(me);
        level++;
        if( me->d_aliasPos.isValid() )
        {
            out << me->d_name;
            out << " := ";
            if( me->d_mod )
                out << modName(me->d_mod->d_fullName);
            else
                out << "?";
        }else
        {
            if( me->d_mod )
                out << modName(me->d_mod->d_fullName);
            else
                out << "?";
        }
        level--;
    }
    void visitIdentdef( Named* me )
    {
        Q_ASSERT( mod );
        const bool isModLevel = me->d_scope && me->d_scope->getTag() != Thing::T_Procedure;
        out << me->d_name;
        switch(me->d_visibility)
        {
        case Named::ReadWrite:
            if( !mod->d_isDef && isModLevel )
                out << "*";
            break;
        case Named::ReadOnly:
            if( isModLevel )
                out << "-";
            break;
        }
    }
    void visit( Procedure* me)
    {
        checkNl(me);
        out << "proc ";
        if( me->d_receiver )
        {
            out << "(";
            me->d_receiver->accept(this);
            out << ") ";
        }
        visitIdentdef(me);
        ProcType* pt = me->getProcType();
        Q_ASSERT( pt );
        visitPt(pt);
        out << " ";
        // TODO proc body?
    }

    void visitScope( Scope* s )
    {
        QList<Named*> CONST, TYPE, VAR, PROC;
        for( int i = 0; i < s->d_order.size(); i++ )
        {
            switch( s->d_order[i]->getTag() )
            {
            case Thing::T_Const:
                CONST << s->d_order[i].data();
                break;
            case Thing::T_NamedType:
                TYPE << s->d_order[i].data();
                break;
            case Thing::T_LocalVar:
            case Thing::T_Variable:
                VAR << s->d_order[i].data();
                break;
            case Thing::T_Procedure:
                PROC << s->d_order[i].data();
                break;
            }
        }
        if( !CONST.isEmpty() )
        {
            out << "const ";
            level++;
            for( int i = 0; i < CONST.size(); i++ )
                CONST[i]->accept(this);
            level--;
            out << endl << ws();
        }
        if( !TYPE.isEmpty() )
        {
            out << "type ";
            level++;
            for( int i = 0; i < TYPE.size(); i++ )
                TYPE[i]->accept(this);
            level--;
            out << endl << ws();
        }
        if( !VAR.isEmpty() )
        {
            out << "var ";
            level++;
            for( int i = 0; i < VAR.size(); i++ )
                VAR[i]->accept(this);
            level--;
            out << endl << ws();
        }
        level++;
        for( int i = 0; i < PROC.size(); i++ )
            PROC[i]->accept(this);
        level--;
        out << endl << ws();
    }

    void visit( Module* me)
    {
        out << "// Automatically generated by the tree shaker of Oberon+ IDE on " <<
               QDateTime::currentDateTime().toString(Qt::ISODate) << endl << endl;
        mod = me;
        if( me->d_isDef )
            out << "definition ";
        else
            out << "module ";
        out << me->d_name;
        level++;
        out << endl << ws();
        out << "import";
        level++;
        out << endl << ws();
        for( int i = 0; i < me->d_imports.size(); i++ )
        {
            if( i != 0 )
                out << ", ";
            me->d_imports[i]->accept(this);
        }
        level--;
        out << endl << ws();
        visitScope( me );
        level --;
        out << endl;
        out << "end " << me->d_name;
    }
    void visit( Literal* me)
    {
        switch( me->d_vtype )
        {
        case Literal::Integer:
            out << me->d_val.toLongLong();
            break;
        case Literal::Real:
            out << me->d_val.toDouble();
            break;
        case Literal::Boolean:
            out << ( me->d_val.toBool() ? "true" : "false" );
            break;
        case Literal::String:
            out << "\"" << QString::fromUtf8(me->d_val.toByteArray()) << "\"";
            break;
        case Literal::Bytes:
            out << "$" << me->d_val.toByteArray().toHex() << "$";
            break;
        case Literal::Char:
            out << '0' << QByteArray::number(me->d_val.toUInt(),16) << 'x';
            break;
        case Literal::Nil:
            out << "nil";
            break;
        default:
            Q_ASSERT( false );
            break;
        }

    }
    void visit( SetExpr* me)
    {
        out << "{";
        for( int i = 0; i < me->d_parts.size(); i++ )
        {
            if( i != 0 )
                out << ", ";
            me->d_parts[i]->accept(this);
        }
        out << "} ";
    }
    void visit( IdentLeaf* me)
    {
        out << me->d_name;
    }
    void visit( UnExpr* me)
    {
        if( me->d_op == UnExpr::NEG )
            out << "-";
        else if( me->d_op == UnExpr::NOT )
            out << "not ";
        if( me->d_sub )
            me->d_sub->accept(this);
        else
            out << "?";
        if( me->d_op == UnExpr::DEREF )
            out << "^";
    }
    void visit( IdentSel* me)
    {
        if( me->d_sub )
            me->d_sub->accept(this);
        else
            out << "?";
        out << ".";
        out << me->d_name;
    }
    void visit( ArgExpr* me)
    {
        if( me->d_op == UnExpr::IDX )
            out << "[";
        else
            out << "(";
        for( int i = 0; i < me->d_args.size(); i++ )
        {
            if( i != 0 )
                out << ", ";
            me->d_args[i]->accept(this);
        }
        if( me->d_op == UnExpr::IDX )
            out << "]";
        else
            out << ") ";
    }
    void visit( BinExpr* me)
    {
        out << "(";
        if( me->d_lhs )
            me->d_lhs->accept(this);
        out << " ";
        switch( me->d_op )
        {
        case BinExpr::Range:
            out << "..";
            break;
        case BinExpr::EQ:
            out << "=";
            break;
        case BinExpr::NEQ:
            out << "#";
            break;
        case BinExpr::LT:
            out << "<";
            break;
        case BinExpr::LEQ:
            out << "<=";
            break;
        case BinExpr::GT:
            out << ">";
            break;
        case BinExpr::GEQ:
            out << ">=";
            break;
        case BinExpr::IN:
            out << "in";
            break;
        case BinExpr::IS:
            out << "is";
            break;
        case BinExpr::ADD:
            out << "+";
            break;
        case BinExpr::SUB:
            out << "-";
            break;
        case BinExpr::OR:
            out << "or";
            break;
        case BinExpr::MUL:
            out << "*";
            break;
        case BinExpr::FDIV:
            out << "/";
            break;
        case BinExpr::DIV:
            out << "div";
            break;
        case BinExpr::MOD:
            out << "mod";
            break;
        case BinExpr::AND:
            out << "&";
            break;
        default:
            out << "?";
            break;
        }
        out << " ";
        if( me->d_rhs )
            me->d_rhs->accept(this);
        out << ")";
    }

    // TODO
    void visit( BuiltIn* ) {}
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

Project::Project(QObject *parent) : QObject(parent),d_dirty(false),d_useBuiltInOakwood(false),
    d_useBuiltInObSysInner(false)
{
    d_mdl = new Model(this);
    //d_mdl->setSenseExt(true);
    d_mdl->getErrs()->setRecord(true);
    d_mdl->getErrs()->setReportToConsole(true); // TEST
    d_mdl->setFillXref(true);

    d_suffixes << ".Mod" << ".obn" << ".obx" << ".def";
}

void Project::clear()
{
    d_mdl->clear();
    d_modules.clear();
    d_dirs.clear();
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
    foreach( const QString& filePath, files )
        addFile(filePath);
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

void Project::setUseBuiltInObSysInner(bool on)
{
    d_useBuiltInObSysInner = on;
    touch();
}

bool Project::addFile(const QString& filePath, const QByteArrayList& importPath)
{
    if( d_files.contains(filePath) )
        return false;
    int pos = findImportPath(importPath);
    if( pos == -1 )
    {
        pos = d_dirs.size();
        d_dirs.append( FileGroup() );
        d_dirs.back().d_importPath = importPath;
    }
    FileGroup& fg = d_dirs[pos];
    FileRef ref( new File() );
    fg.d_files.append(ref.data());
    ref->d_group = &fg;
    ref->d_filePath = filePath;
    d_files.insert(filePath,ref);
    touch();
    return true;
}

bool Project::addImportPath(const QByteArrayList& importPath)
{
    int pos = findImportPath(importPath);
    if( pos == -1 )
    {
        pos = d_dirs.size();
        d_dirs.append( FileGroup() );
        d_dirs.back().d_importPath = importPath;
        touch();
        return true;
    }else
        return false;
}

bool Project::removeFile(const QString& path)
{
    FileHash::iterator i = d_files.find(path);
    if( i == d_files.end() )
        return false;
    const int pos = findImportPath( i.value()->d_group->d_importPath );
    Q_ASSERT( pos != -1 );
    d_dirs[pos].d_files.removeAll(i.value().data());
    d_files.erase(i);
    touch();
    return true;
}

bool Project::removeImportPath(const QByteArrayList& importPath)
{
    if( importPath.isEmpty() )
        return false;
    int pos = findImportPath(importPath);
    if( pos == -1 )
        return false;
    if( !d_dirs[pos].d_files.isEmpty() )
        return false;
    d_dirs.removeAt(pos);
    touch();
    return true;
}

Project::FileGroup Project::getRootModules() const
{
    for( int i = 0; i < d_dirs.size(); i++ )
    {
        if( d_dirs[i].d_importPath.isEmpty() )
            return d_dirs[i];
    }
    return FileGroup();
}

Project::FileList Project::getFilesInExecOrder() const
{
    const QList<Module*>& order = d_mdl->getDepOrder();
    FileList res;
    foreach( Module* m, order )
    {
        Q_ASSERT( m );
        if( !m->isFullyInstantiated() )
            continue;
        FileHash::const_iterator i = d_files.find( m->d_file );
        if( i == d_files.end() || i.value()->d_sourceCode.isEmpty() || i.value()->d_mod.isNull() )
            continue;
        res.append( i.value() );
    }
    return res;
}

Expression* Project::findSymbolBySourcePos(const QString& file, quint32 line, quint16 col) const
{
    FileMod f = findFile(file);
    if( f.first == 0 )
        return 0;

    try
    {
        ObxHitTest hit;
        hit.col = col;
        hit.line = line;
        f.second->accept(&hit);
    }catch( Expression* e )
    {
        return e;
    }catch(...)
    {

    }
    return 0;
}

Project::FileMod Project::findFile(const QString& file) const
{
    FileRef f = d_files.value(file);
    if( f.data() == 0 || f->d_mod.isNull() ) // || i.value().d_mod->d_hasErrors )
    {
        FileMod fm = d_modules.value(file.toLatin1());
        if( fm.first == 0 || fm.second == 0 )
            return FileMod();
        else
            return fm;
    }
    return qMakePair( f.data(), f->d_mod.data() );
}

ExpList Project::getUsage(Named* n) const
{
    const Model::XRef& xref = d_mdl->getXref();
    return xref.value(n);
}

QString Project::getWorkingDir(bool resolved) const
{
    if( d_workingDir.isEmpty() )
        return QFileInfo(d_filePath).dir().path();
    else if( !resolved )
        return d_workingDir;
    // else
    QString wd = d_workingDir;
    wd.replace("%PRODIR%", QFileInfo(d_filePath).dir().path() );
    wd.replace("%APPDIR%", QCoreApplication::applicationDirPath() );
    return wd;
}

void Project::setWorkingDir(const QString& wd)
{
    d_workingDir = wd;
    touch();
}

bool Project::printTreeShaken(const QString& module, const QString& fileName)
{
    FileRef f = d_files.value(module);
    if( f->d_mod.isNull() )
        return false;
    Ref<Module> m = d_mdl->treeShaken(f->d_mod.data());
    QFile out(fileName);
    if( !out.open(QIODevice::WriteOnly) )
        return false;


    ObxModuleDump dump;
    dump.out.setDevice( &out );
    m->accept(&dump);
    return true;
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

int Project::findImportPath(const QByteArrayList& importPath) const
{
    int pos = -1;
    for( int i = 0; i < d_dirs.size(); i++ )
    {
        if( d_dirs[i].d_importPath == importPath )
        {
            pos = i;
            break;
        }
    }
    return pos;
}

bool Project::recompile()
{
    d_modules.clear();
    Model::FileGroups fgs;
    for( int i = 0; i < d_dirs.size(); i++ )
    {
        Model::FileGroup fg;
        for( int j = 0; j < d_dirs[i].d_files.size(); j++ )
            fg.d_files << d_dirs[i].d_files[j]->d_filePath;
        fg.d_groupName = d_dirs[i].d_importPath;
        fgs << fg;
    }
    const bool res = d_mdl->parseFiles( fgs );
    QList<Module*> mods = d_mdl->getDepOrder();
    foreach( Module* m, mods )
    {
        if( m->d_file.isEmpty() || ( m->d_isDef && !d_files.contains(m->d_file) ) )
            continue; // e.g. SYSTEM
        FileHash::iterator i = d_files.find(m->d_file);
        if( i != d_files.end() )
        {
            if( m->d_metaActuals.isEmpty() )
                i.value()->d_mod = m; // d_mod always points to the generic (non-instantiated) module
            d_modules.insert(m->getName(),qMakePair(i.value().data(), m)); // d_modules contains also the instantiations
            //m->dump();
        }else
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
    FileHash::const_iterator i;
    for( i = d_files.begin(); i != d_files.end(); ++i )
        i.value()->d_sourceCode.clear();
    const quint32 errs = d_mdl->getErrs()->getErrCount();
    QList<Module*> mods = d_mdl->getDepOrder();
    foreach( Module* m, mods )
    {

        FileHash::iterator f = d_files.find(m->d_file);
        if( m->d_synthetic || !m->isFullyInstantiated() )
            ; // NOP
        else if( m->d_hasErrors )
        {
            qDebug() << "terminating because of errors in" << m->d_name;
            return false;
        }else if( m->d_isDef )
        {
            qDebug() << "allocating" << m->d_name;
#ifdef _DEBUG
            QBuffer buf;
            buf.open(QIODevice::WriteOnly);
            LjbcGen::allocateDef(m, &buf, d_mdl->getErrs());
            buf.close();
            qDebug() << "********** Definition of" << m->d_name;
            qDebug() << buf.buffer();
#else
            LjbcGen::allocateDef(m, 0, d_mdl->getErrs());
#endif
        }else if( f != d_files.end() )
        {
            qDebug() << "generating" << m->getName() << " " << m;
            QBuffer buf;
            buf.open(QIODevice::WriteOnly);
            LjbcGen::translate(m, &buf, false, d_mdl->getErrs() );
            buf.close();
            f.value()->d_sourceCode[m] = buf.buffer();
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
    out.setValue("BuiltInObSysInner", d_useBuiltInObSysInner );
    out.setValue("MainModule", d_main.first );
    out.setValue("MainProc", d_main.second );
    out.setValue("WorkingDir", d_workingDir );

    FileGroup root = getRootModules();
    out.beginWriteArray("Modules", root.d_files.size() ); // nested arrays don't work
    for( int i = 0; i < root.d_files.size(); i++ )
    {
        const QString absPath = root.d_files[i]->d_filePath;
        const QString relPath = dir.relativeFilePath( absPath );
        out.setArrayIndex(i);
        out.setValue("AbsPath", absPath );
        out.setValue("RelPath", relPath );
    }
    out.endArray();

    out.beginWriteArray("Packages", d_dirs.size() );
    for( int i = 0; i < d_dirs.size(); i++ )
    {
        out.setArrayIndex(i);
        out.setValue("Name", d_dirs[i].d_importPath.join('.') ); // '/' in key gives strange effects
    }
    out.endArray();

    for( int i = 0; i < d_dirs.size(); i++ )
    {
        if(d_dirs[i].d_importPath.isEmpty())
            continue;
        out.beginWriteArray("." + d_dirs[i].d_importPath.join('.'), d_dirs[i].d_files.size() );
        for( int j = 0; j < d_dirs[i].d_files.size(); j++ )
        {
            const QString absPath = d_dirs[i].d_files[j]->d_filePath;
            const QString relPath = dir.relativeFilePath( absPath );
            out.setArrayIndex(j);
            out.setValue("AbsPath", absPath );
            out.setValue("RelPath", relPath );
        }
        out.endArray();
    }

    d_dirty = false;
    emit sigModified(d_dirty);
    return true;
}

bool Project::loadFrom(const QString& filePath)
{
    clear();

    d_filePath = filePath;
    d_mdl->setFileRoot(d_filePath);

    QDir dir = QFileInfo(d_filePath).dir();

    QSettings in(d_filePath,QSettings::IniFormat);

    d_suffixes = in.value("Suffixes").toStringList();
    d_useBuiltInOakwood = in.value("BuiltInOakwood").toBool();
    d_useBuiltInObSysInner = in.value("BuiltInObSysInner").toBool();
    d_main.first = in.value("MainModule").toByteArray();
    d_main.second = in.value("MainProc").toByteArray();
    d_workingDir = in.value("WorkingDir").toString();

    int count = in.beginReadArray("Modules");
    for( int i = 0; i < count; i++ )
    {
        in.setArrayIndex(i);
        QString absPath = in.value("AbsPath").toString();
        const QString relPath = in.value("RelPath").toString();
        if( QFileInfo(absPath).exists() )
            addFile(absPath);
        else
        {
            absPath = dir.absoluteFilePath(relPath);
            if( QFileInfo(absPath).exists() )
                addFile(absPath);
            else
                qCritical() << "Could not open module" << relPath;
        }
    }
    in.endArray();

    QList<QByteArrayList> paths;
    count = in.beginReadArray("Packages");
    for( int i = 0; i < count; i++ )
    {
        in.setArrayIndex(i);
        QString name = in.value("Name").toString();
        paths << name.toLatin1().split('.');
        addImportPath( paths.back() );
    }
    in.endArray();

    for( int j = 0; j < paths.size(); j++ )
    {
        count = in.beginReadArray("." + paths[j].join('.'));
        for( int i = 0; i < count; i++ )
        {
            in.setArrayIndex(i);
            QString absPath = in.value("AbsPath").toString();
            const QString relPath = in.value("RelPath").toString();
            if( QFileInfo(absPath).exists() )
                addFile(absPath, paths[j]);
            else
            {
                absPath = dir.absoluteFilePath(relPath);
                if( QFileInfo(absPath).exists() )
                    addFile(absPath, paths[j]);
                else
                    qCritical() << "Could not open module" << relPath;
            }
        }
        in.endArray();
    }

    d_dirty = false;
    emit sigModified(d_dirty);
    emit sigRenamed();
    return true;
}

bool Project::saveTo(const QString& filePath)
{
    d_filePath = filePath;
    d_mdl->setFileRoot(d_filePath);
    const bool res = save();
    emit sigRenamed();
    return res;
}

