#ifndef _OBX_MATHL_
#define _OBX_MATHL_
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

//PROCEDURE sqrt (x : LONGREAL) : LONGREAL;
extern double MathL$sqrt(double x);

//PROCEDURE power (x,base : LONGREAL) : LONGREAL;
extern double MathL$power(double x, double b);

//PROCEDURE exp (x : LONGREAL): LONGREAL;
extern double MathL$exp(double x);

//PROCEDURE ln (x : LONGREAL) : LONGREAL;
extern double MathL$ln(double x);

//PROCEDURE log (x,base : LONGREAL) : LONGREAL;
extern double MathL$log(double x);

//PROCEDURE round (x : LONGREAL) : LONGREAL;
extern double MathL$round(double x);

//PROCEDURE sin (x : LONGREAL) : LONGREAL;
extern double MathL$sin(double x);

//PROCEDURE cos (x : LONGREAL) : LONGREAL;
extern double MathL$cos(double x);

//PROCEDURE tan (x : LONGREAL) : LONGREAL;
extern double MathL$tan(double x);

//PROCEDURE arcsin (x : LONGREAL) : LONGREAL;
extern double MathL$arcsin(double x);

//PROCEDURE arccos (x : LONGREAL) : LONGREAL;
extern double MathL$arccos(double x);

//PROCEDURE arctan (x : LONGREAL) : LONGREAL;
extern double MathL$arctan(double x);

//PROCEDURE arctan2(x,y : LONGREAL): LONGREAL;
extern double MathL$arctan2(double x, double y);

//PROCEDURE sinh (x:LONGREAL):LONGREAL;
extern double MathL$sinh(double x);

//PROCEDURE cosh (x:LONGREAL):LONGREAL;
extern double MathL$cosh(double x);

//PROCEDURE tanh (x:LONGREAL):LONGREAL;
extern double MathL$tanh(double x);

//PROCEDURE arcsinh(x:LONGREAL):LONGREAL;
extern double MathL$arcsinh(double x);

//PROCEDURE arccosh(x:LONGREAL):LONGREAL;
extern double MathL$arccosh(double x);

//PROCEDURE arctanh(x:LONGREAL):LONGREAL;
extern double MathL$arctanh(double x);


extern void MathL$init$();
extern OBX$Cmd MathL$cmd$(const char*);

#endif
