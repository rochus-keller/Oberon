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

#include "MathL.h"
#include <math.h>

//PROCEDURE sqrt (x : LONGREAL) : LONGREAL;
double MathL$sqrt(double x)
{
	return sqrt(x);
}
//PROCEDURE power (x,base : LONGREAL) : LONGREAL;
double MathL$power(double x, double b)
{
	return pow(b,x);
}
//PROCEDURE exp (x : LONGREAL): LONGREAL;
double MathL$exp(double x)
{
	return exp(x);
}
//PROCEDURE ln (x : LONGREAL) : LONGREAL;
double MathL$ln(double x)
{
	return log(x);
}
//PROCEDURE log (x,base : LONGREAL) : LONGREAL;
double MathL$log(double x)
{
	return log10(x);
}
//PROCEDURE round (x : LONGREAL) : LONGREAL;
double MathL$round(double x)
{
	return round(x);
}
//PROCEDURE sin (x : LONGREAL) : LONGREAL;
double MathL$sin(double x)
{
	return sin(x);
}
//PROCEDURE cos (x : LONGREAL) : LONGREAL;
double MathL$cos(double x)
{
	return cos(x);
}
//PROCEDURE tan (x : LONGREAL) : LONGREAL;
double MathL$tan(double x)
{
	return tan(x);
}
//PROCEDURE arcsin (x : LONGREAL) : LONGREAL;
double MathL$arcsin(double x)
{
	return asin(x);
}
//PROCEDURE arccos (x : LONGREAL) : LONGREAL;
double MathL$arccos(double x)
{
	return acos(x);
}
//PROCEDURE arctan (x : LONGREAL) : LONGREAL;
double MathL$arctan(double x)
{
	return atan(x);
}
//PROCEDURE arctan2(x,y : LONGREAL): LONGREAL;
double MathL$arctan2(double x, double y)
{
	return atan2(x,y);
}
//PROCEDURE sinh (x:LONGREAL):LONGREAL;
double MathL$sinh(double x)
{
	return sinh(x);
}
//PROCEDURE cosh (x:LONGREAL):LONGREAL;
double MathL$cosh(double x)
{
	return cosh(x);
}
//PROCEDURE tanh (x:LONGREAL):LONGREAL;
double MathL$tanh(double x)
{
	return tanh(x);
}
//PROCEDURE arcsinh(x:LONGREAL):LONGREAL;
double MathL$arcsinh(double x)
{
	return asinh(x);
}
//PROCEDURE arccosh(x:LONGREAL):LONGREAL;
double MathL$arccosh(double x)
{
	return acosh(x);
}
//PROCEDURE arctanh(x:LONGREAL):LONGREAL;
double MathL$arctanh(double x)
{
	return atanh(x);
}

void MathL$init$()
{
}

OBX$Cmd MathL$cmd$(const char* name)
{
	if(name==0) return MathL$init$;
	return 0;
}
