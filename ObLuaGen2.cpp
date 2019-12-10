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

#include "ObLuaGen2.h"
#include "ObAst.h"
#include "ObErrors.h"
#include <QTextStream>
using namespace Ob;
using namespace Ast;

// WORK IN PROGRESS
struct LuaGen2Imp : public AstVisitor
{
    QTextStream out;
    Errors* err;
    Module* mod;
    quint16 level;
    bool ownsErr;

    void visit( BaseType* ) {}
    void visit( Pointer* ) {}
    void visit( Array* ) {}
    void visit( Record* ) {}
    void visit( ProcType* ) {}
    void visit( SelfRef* ) {}
    void visit( Field* ) {}
    void visit( Variable* ) {}
    void visit( LocalVar* ) {}
    void visit( Parameter* ) {}
    void visit( NamedType* ) {}
    void visit( Const* ) {}
    void visit( Import* ) {}
    void visit( Procedure* ) {}
    void visit( BuiltIn* ) {}
    void visit( Module* ) {}
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
    void visit( CallExpr* ) {}
    void visit( BinExpr* ) {}
};

bool LuaGen2::translate(Module* m, QIODevice* out, Errors* errs )
{
    Q_ASSERT( m != 0 && out != 0 );

    if( m->d_hasErrors )
        return false;

    LuaGen2Imp imp;

    imp.level = 0;
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

    imp.out.setDevice(out);

    m->accept(&imp);

    const bool hasErrs = ( imp.err->getErrCount() - errCount ) != 0;

    if( imp.ownsErr )
        delete imp.err;
    return hasErrs;
}
