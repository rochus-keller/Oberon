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
            Literal::ValueType d_type;
            bool d_wide;
            Result():d_type(Literal::Invalid),d_wide(false){}
        };

        static Result eval( Expression*, Module*, Ob::Errors* = 0 );

    private:
        Evaluator();
    };
}

#endif // OBXEVALUATOR_H
