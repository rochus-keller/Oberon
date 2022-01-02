#ifndef OBXEVALUATOR_H
#define OBXEVALUATOR_H

/*
* Copyright 2020-2021 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include <Oberon/ObxAst.h>
#include <Oberon/ObErrors.h>

namespace Obx
{
    class Evaluator : public QObject // for tr()
    {
    public:

        struct Result
        {
            QVariant d_value;
            uint d_vtype : 7; // Literal::ValueType
            uint d_strLen : 22;
            uint d_wide : 1; // mark WSTRING and WCHAR, double vs float, longint vs integer
            uint d_minInt : 1;
            uint d_dyn : 1;
            Result():d_vtype(Literal::Invalid),d_wide(false),d_strLen(0),d_dyn(0),d_minInt(0){}
        };

        static Result eval( Expression*, Scope*, bool supportVla = false, Ob::Errors* = 0 );

    private:
        Evaluator();
    };
}

#endif // OBXEVALUATOR_H
