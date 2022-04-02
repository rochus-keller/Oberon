#ifndef OBXVALIDATOR_H
#define OBXVALIDATOR_H

/*
* Copyright 2020 Rochus Keller <mailto:me@rochus-keller.ch>
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
    class Validator : public QObject // for tr
    {
    public:
        struct BaseTypes
        {
            BaseType* d_noType;
            BaseType* d_boolType;
            BaseType* d_charType;
            BaseType* d_wcharType;
            BaseType* d_byteType;
            BaseType* d_intType;
            BaseType* d_shortType;
            BaseType* d_longType;
            BaseType* d_realType;
            BaseType* d_longrealType;
            BaseType* d_setType;
            BaseType* d_stringType;
            BaseType* d_wstringType;
            BaseType* d_byteArrayType;
            BaseType* d_nilType;
            BaseType* d_voidType;
            Record* d_anyRec;
            BaseType* d_anyType;
            BaseTypes();
            void check() const;
        };

        // assumes imports are already resolved
        static bool check( Module*, const BaseTypes&, Ob::Errors*, Instantiator*);

        static bool includesType( quint8 lhs, quint8 rhs ); // lhs, rhs: Type::BT
        static QPair<quint8,bool> inclusiveType( quint8 lhs, quint8 rhs ); // lhs, rhs, return: Type::BT; bool: no information loss
    private:
        Validator();
    };
}

#endif // OBXVALIDATOR_H
