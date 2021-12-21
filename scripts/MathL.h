#ifndef _OBX_MATHL_
#define _OBX_MATHL_
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
extern void MathL$cmd$(const char*);

#endif
