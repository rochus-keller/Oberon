#ifndef _OBX_MATH_
#define _OBX_MATH_
/*
* Copyright 2021 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon+ parser/compiler library.
*
* The following is the license that applies to this copy of the
* file. For a license to use the file under conditions
* other than those described here, please email to me@rochus-keller.ch.
*
* This file may be used under the terms of the GNU Lesser
* General Public License version 2.1 or version 3 as published by the Free
* Software Foundation and appearing in the file LICENSE.LGPLv21 and
* LICENSE.LGPLv3 included in the packaging of this file. Please review the
* following information to ensure the GNU Lesser General Public License
* requirements will be met: https://www.gnu.org/licenses/lgpl.html and
* http://www.gnu.org/licenses/old-licenses/lgpl-2.1.html.
*
* Alternatively this file may be used under the terms of the Mozilla 
* Public License. If a copy of the MPL was not distributed with this
* file, You can obtain one at https://mozilla.org/MPL/2.0/.
*/

#include "OBX.Runtime.h"

extern float Math$sqrt(float x);
extern float Math$power(float x, float b);
extern float Math$exp(float x);
extern float Math$ln(float x);
extern float Math$log(float x);
extern float Math$round(float x);
extern float Math$sin(float x);
extern float Math$cos(float x);
extern float Math$tan(float x);
extern float Math$arcsin(float x);
extern float Math$arccos(float x);
extern float Math$arctan(float x);
extern float Math$arctan2(float x, float y);
extern float Math$sinh(float x);
extern float Math$cosh(float x);
extern float Math$tanh(float x);
extern float Math$arcsinh(float x);
extern float Math$arccosh(float x);
extern float Math$arctanh(float x);


extern void Math$init$();
extern OBX$Cmd Math$cmd$(const char*);


#endif
