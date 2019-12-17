#ifndef OBASTEVAL_H
#define OBASTEVAL_H

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

#include <QObject>
#include <QVariant>

class QTextStream;

namespace Ob
{
namespace Ast
{
    class Expression;
    class Thing;

    class Eval : public QObject
    {
    public:
        static void render( QTextStream&, Thing* );
        static QVariant evalConstExpr( Expression*, QString* = 0 );
    private:
        Eval(){}
    };
}
}

#endif // OBASTEVAL_H