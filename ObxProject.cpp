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
    QList<Scope*> scopes;
    Scope* scopeHit;

    ObxHitTest():scopeHit(0){}

    void test( Scope* s )
    {
        if( ( s->d_loc.d_row == line && s->d_loc.d_col <= col ) ||
                ( s->d_loc.d_row < line && s->d_end.d_row > line ) ||
                ( s->d_end.d_row == line && s->d_end.d_col >= col ) )
        {
            if( scopeHit == 0 )
                scopeHit = s;
            else if( scopeHit->d_end.d_row - scopeHit->d_loc.d_row >= s->d_end.d_row - s->d_loc.d_row )
                scopeHit = s;
        }
    }

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
#if 0
        if( p->d_flag )
            p->d_flag->accept(this);
#endif
        if( p->d_to )
            p->d_to->accept(this); // look for qualis
    }

    void visit( Array* a )
    {
        //if( a->d_type->d_ident == 0 )
#if 0
        if( a->d_flag )
            a->d_flag->accept(this);
#endif
        if( a->d_type )
            a->d_type->accept(this); // look for qualis
        if( a->d_lenExpr )
            a->d_lenExpr->accept(this);
    }

    void visit( Record* r )
    {
#if 0
        if( r->d_flag )
            r->d_flag->accept(this);
#endif
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
        if( t->d_type && t->d_type->d_decl == t )
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
        scopes.push_back(m);
        test(m);
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
        scopes.pop_back();
    }

    void visit( Module* m )
    {
        scopes.push_back(m);
        test(m);
        for( int i = 0; i < m->d_order.size(); i++ )
            m->d_order[i]->accept(this);
        for( int i = 0; i < m->d_body.size(); i++ )
            m->d_body[i]->accept(this);
        for( int i = 0; i < m->d_helper.size(); i++ )
            m->d_helper[i]->accept(this);
        scopes.pop_back();
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
        {
            if( l->d_if[i] )
                l->d_if[i]->accept(this);
        }
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
            out << "cpointer to ";
        else
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
        {
            if( me->d_union )
                out << "cunion ";
            else
                out << "cstruct ";
        }else
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
    d_groups.clear();
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

bool Project::initializeFromPackageList(const PackageList& pl)
{
    clear();
    d_dirty = false;
    foreach( const Package& p, pl )
    {
        foreach( const QString& path, p.d_files )
            addFile(path,p.d_path);
    }

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

QString Project::renderMain() const
{
    QString res = d_main.first;
    if( !d_main.second.isEmpty() )
        res += "." + d_main.second;
    return res;
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

bool Project::addFile(const QString& filePath, const VirtualPath& package)
{
    if( d_files.contains(filePath) )
        return false;
    int pos = findPackage(package);
    if( pos == -1 )
    {
        pos = d_groups.size();
        d_groups.append( FileGroup() );
        d_groups.back().d_package = package;
    }
    FileGroup& fg = d_groups[pos];
    FileRef ref( new File() );
    fg.d_files.append(ref.data());
    ref->d_group = &fg;
    ref->d_filePath = filePath;
    d_files.insert(filePath,ref);
    touch();
    return true;
}

bool Project::addPackagePath(const VirtualPath& path)
{
    int pos = findPackage(path);
    if( pos == -1 )
    {
        pos = d_groups.size();
        d_groups.append( FileGroup() );
        d_groups.back().d_package = path;
        touch();
        return true;
    }else
        return false;
}

bool Project::removeFile(const QString& filePath)
{
    FileHash::iterator i = d_files.find(filePath);
    if( i == d_files.end() )
        return false;
    const int pos = findPackage( i.value()->d_group->d_package );
    Q_ASSERT( pos != -1 );
    d_groups[pos].d_files.removeAll(i.value().data());
    d_files.erase(i);
    touch();
    return true;
}

bool Project::removePackagePath(const VirtualPath& path)
{
    if( path.isEmpty() )
        return false;
    int pos = findPackage(path);
    if( pos == -1 )
        return false;
    if( !d_groups[pos].d_files.isEmpty() )
        return false;
    d_groups.removeAt(pos);
    touch();
    return true;
}

Project::FileGroup Project::getRootFileGroup() const
{
    return findFileGroup(QByteArrayList());
}

Project::FileGroup Project::findFileGroup(const VirtualPath& package) const
{
    for( int i = 0; i < d_groups.size(); i++ )
    {
        if( d_groups[i].d_package == package )
            return d_groups[i];
    }
    return FileGroup();
}

Expression* Project::findSymbolBySourcePos(const QString& file, quint32 line, quint16 col, Scope** scopePtr) const
{
    FileMod f = findFile(file);
    if( f.first == 0 )
        return 0;

    return findSymbolBySourcePos(f.second,line,col, scopePtr);
}

Expression*Project::findSymbolBySourcePos(Module* m, quint32 line, quint16 col, Scope** scopePtr) const
{
    Q_ASSERT(m);

    ObxHitTest hit;
    hit.col = col;
    hit.line = line;
    try
    {
        m->accept(&hit);
    }catch( Expression* e )
    {
        Q_ASSERT( !hit.scopes.isEmpty() );
        if( scopePtr )
            *scopePtr = hit.scopes.back();
        return e;
    }catch(...)
    {

    }
    if( scopePtr )
        *scopePtr = hit.scopeHit;
    return 0;
}

Project::FileMod Project::findFile(const QString& file) const
{
    FileRef f = d_files.value(file);
    if( f.data() == 0 || f->d_mod.isNull() ) // || i.value().d_mod->d_hasErrors )
    {
        FileMod fm = d_modules.value(file.toLatin1()); // includes also generic instances
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
    {
        if( !d_filePath.isEmpty() )
            return QFileInfo(d_filePath).dir().path();
        else
            return QCoreApplication::applicationDirPath();
    }
    else if( !resolved )
        return d_workingDir;
    // else
    QString wd = d_workingDir;
    wd.replace("%PRODIR%", QFileInfo(d_filePath).dir().path() );
    QString path;
#ifdef Q_OS_MAC
    QDir cur = QCoreApplication::applicationDirPath();
    cur.cdUp();
    cur.cdUp();
    cur.cdUp();
    path = cur.path();
#else
    path = QCoreApplication::applicationDirPath();
#endif
    wd.replace("%APPDIR%", path );
    return wd;
}

void Project::setWorkingDir(const QString& wd)
{
    d_workingDir = wd;
    touch();
}

QString Project::getBuildDir(bool resolved) const
{
    if( d_buildDir.isEmpty() )
    {
        if( !d_filePath.isEmpty() )
            return QFileInfo(d_filePath).dir().absoluteFilePath("build");
        else
            return QCoreApplication::applicationDirPath() + "/build";
    }
    else if( !resolved )
        return d_buildDir;
    // else
    QString bd = d_buildDir;
    bd.replace("%PRODIR%", QFileInfo(d_filePath).dir().path() );
    QString path;
#ifdef Q_OS_MAC
    QDir cur = QCoreApplication::applicationDirPath();
    cur.cdUp();
    cur.cdUp();
    cur.cdUp();
    path = cur.path();
#else
    path = QCoreApplication::applicationDirPath();
#endif
    bd.replace("%APPDIR%", path );
    return bd;
}

void Project::setBuildDir(const QString& bd)
{
    d_buildDir = bd;
    touch();
}

void Project::setOptions(const QByteArrayList& o)
{
    d_options = o;
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

static inline QByteArray escapeDot(QByteArray name)
{
    return "\"" + name + "\"";
}

static void removeRedundantImports(Module* cur, QSet<Module*>& imports )
{
    foreach( Import* i, cur->d_imports )
    {
        imports.remove( i->d_mod.data() );
        removeRedundantImports( i->d_mod.data(), imports );
    }
}

static QList<Module*> removeRedundantImports(Module* m)
{
    QSet<Module*> imports;
    foreach( Import* i, m->d_imports )
    {
        if( !i->d_mod->isFullyInstantiated() || i->d_mod->d_synthetic )
            continue;
        imports << i->d_mod.data();
    }
    foreach( Import* i, m->d_imports )
        removeRedundantImports(i->d_mod.data(), imports);
    return imports.toList();
}

bool Project::printImportDependencies(const QString& fileName, bool pruned)
{
    QFile f(fileName);
    if( !f.open(QIODevice::WriteOnly) )
        return false;
    QTextStream s(&f);

    s << "digraph \"Import Dependency Tree\" {" << endl;
    if( !pruned )
        s << "    graph [splines=ortho]" << endl;
    s << "    node [shape=box]" << endl;

    QList<Module*> mods = getModulesToGenerate(true);
    foreach( Module* m, mods )
    {
        if( !m->isFullyInstantiated() || m->d_synthetic )
            continue;
        QSet<QByteArray> names;
        if( pruned )
        {
            QList<Module*> imports = removeRedundantImports(m);
            foreach( Module* i, imports )
                names << escapeDot(i->getFullName());
        }else
        {
            foreach( Import* i, m->d_imports )
            {
                if( i->d_mod.isNull() || i->d_mod->d_synthetic )
                    continue;
                names << escapeDot(i->d_mod->getFullName());
            }
        }
        const QByteArray name = escapeDot(m->getFullName());
        // s << "    " << name << " [shape=box];" << endl;
        s << "    " << name << " -> {";
        bool first = true;
        foreach( const QByteArray& to, names )
        {
            if( !first )
                s << ", ";
            first = false;
            s << to;
        }
        s << "};" << endl;
    }

    s << "}";
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

int Project::findPackage(const VirtualPath& path) const
{
    int pos = -1;
    for( int i = 0; i < d_groups.size(); i++ )
    {
        if( d_groups[i].d_package == path )
        {
            pos = i;
            break;
        }
    }
    return pos;
}

#if 0
bool Project::generate(Module* m)
{
    Q_ASSERT( m );
    if( !m->isFullyInstantiated() )
        return false;
    FileHash::iterator f = d_files.find(m->d_file);
    if( f == d_files.end() )
        return false;

    qDebug() << "generating" << m->getName();
    QBuffer buf;
    buf.open(QIODevice::WriteOnly);
    LjbcGen::translate(m, &buf, false, d_mdl->getErrs() );
    buf.close();
    f.value()->d_byteCode[m] = buf.buffer();
    return true;
}
#endif

bool Project::reparse()
{
    d_modules.clear();
    PackageList fgs;
    for( int i = 0; i < d_groups.size(); i++ )
    {
        Package fg;
        for( int j = 0; j < d_groups[i].d_files.size(); j++ )
            fg.d_files << d_groups[i].d_files[j]->d_filePath;
        fg.d_path = d_groups[i].d_package;
        fgs << fg;
    }
    d_mdl->setOptions(d_options);
    const bool res = d_mdl->parseFiles( fgs );
    QList<Module*> mods = d_mdl->getDepOrder();
    foreach( Module* m, mods )
    {
        if( m->d_file.isEmpty() || ( m->d_isDef && !d_files.contains(m->d_file) ) )
            continue; // e.g. SYSTEM
        FileHash::iterator i = d_files.find(m->d_file);
        if( i != d_files.end() )
        {
            Q_ASSERT( m->d_metaActuals.isEmpty() );
            File* f = i.value().data();
            f->d_mod = m; // d_mod always points to the generic (non-instantiated) module
            d_modules.insert(m->getName(),qMakePair(f, m));
            // d_modules contains also the instantiations
            if( !m->d_metaParams.isEmpty() )
            {
                QList<Module*> insts = d_mdl->instances(m);
                foreach( Module* inst, insts )
                    d_modules.insert(inst->getName(),qMakePair(f, inst));
            }
            //m->dump();
        }else
        {
            qDebug() << "missing" << m->d_name;
            Q_ASSERT( false );
        }
    }
    emit sigReparsed();
    return res;
}

#if 0 // moved to IDE to make Project independent of backend
bool Project::generate()
{
    FileHash::const_iterator i;
    for( i = d_files.begin(); i != d_files.end(); ++i )
        i.value()->d_byteCode.clear();
    const quint32 errs = d_mdl->getErrs()->getErrCount();
    QList<Module*> mods = d_mdl->getDepOrder();
#if 0
    qDebug() << "******* module generating order:";
    QSet<Module*> test;
    foreach( Module* m, mods )
    {
        if( m->d_metaParams.isEmpty() )
        {
            qDebug() << m->getName();
            QList<Module*> result;
            m->findAllInstances(result);
            foreach( Module* inst, result )
            {
                if( test.contains(inst) )
                    qWarning() << "already seen" << inst->getName();
                else if( inst->isFullyInstantiated() )
                    qDebug() << "instance" << inst->getName();
                else
                    qWarning() << "not fully instantiated" << inst->getName();
                test.insert(inst);
            }
        }
    }
#endif
    QSet<Module*> generated;
    foreach( Module* m, mods )
    {
        if( m->d_synthetic )
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
        }else
        {
            if( m->d_metaParams.isEmpty() )
            {
                LjbcGen::allocateSlots(m);
                // module slots are allocated before generic instances are generated because records of
                // the module could be used in the generic instance

                QList<Module*> result;
                m->findAllInstances(result);
                foreach( Module* inst, result )
                {
                    // instances must be generated after the modules using them, otherwise we get !slotValid assertions
                    if( !generated.contains(inst) )
                    {
                        generated.insert(inst);
                        LjbcGen::allocateSlots(inst);
                        generate(inst);
                    }
                }
                // module is generated after the generic instances it depends on because there are required slots
                generate(m);
            }
        }
    }
    return errs == d_mdl->getErrs()->getErrCount();
}
#endif

QList<Module*> Project::getModulesToGenerate(bool includeTemplates) const
{
    QList<Module*> res;
    FileHash::const_iterator i;
    QList<Module*> mods = d_mdl->getDepOrder();
#if 0
    qDebug() << "******* module generating order:";
    QSet<Module*> test;
    foreach( Module* m, mods )
    {
        if( m->d_metaParams.isEmpty() )
        {
            qDebug() << m->getName();
            QList<Module*> result;
            m->findAllInstances(result);
            foreach( Module* inst, result )
            {
                if( test.contains(inst) )
                    qWarning() << "already seen" << inst->getName();
                else if( inst->isFullyInstantiated() )
                    qDebug() << "instance" << inst->getName();
                else
                    qWarning() << "not fully instantiated" << inst->getName();
                test.insert(inst);
            }
        }
    }
#endif
    foreach( Module* m, mods )
    {
        if( m->d_synthetic )
            ; // NOP
        else if( m->d_isDef || m->d_metaParams.isEmpty() )
            res.append(m); // we don't add generic modules because the instances to generate are identified by Module:findAllInstances
        else if( includeTemplates && m->d_metaActuals.isEmpty() )
            res.append(m);
    }
    return res;
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
    out.setValue("BuildDir", d_buildDir );
    out.setValue("Options", d_options.join(' ') );

    FileGroup root = getRootFileGroup();
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

    out.beginWriteArray("Packages", d_groups.size() );
    for( int i = 0; i < d_groups.size(); i++ )
    {
        out.setArrayIndex(i);
        out.setValue("Name", d_groups[i].d_package.join('.') ); // '/' in key gives strange effects
    }
    out.endArray();

    for( int i = 0; i < d_groups.size(); i++ )
    {
        if(d_groups[i].d_package.isEmpty())
            continue;
        out.beginWriteArray("." + d_groups[i].d_package.join('.'), d_groups[i].d_files.size() );
        for( int j = 0; j < d_groups[i].d_files.size(); j++ )
        {
            const QString absPath = d_groups[i].d_files[j]->d_filePath;
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

    QDir dir = QFileInfo(d_filePath).dir();

    QSettings in(d_filePath,QSettings::IniFormat);

    d_suffixes = in.value("Suffixes").toStringList();
    d_useBuiltInOakwood = in.value("BuiltInOakwood").toBool();
    d_useBuiltInObSysInner = in.value("BuiltInObSysInner").toBool();
    d_main.first = in.value("MainModule").toByteArray();
    d_main.second = in.value("MainProc").toByteArray();
    d_workingDir = in.value("WorkingDir").toString();
    d_buildDir = in.value("BuildDir").toString();
    d_options = in.value("Options").toByteArray().split(' ');

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
        addPackagePath( paths.back() );
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
    const bool res = save();
    emit sigRenamed();
    return res;
}

