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

#include "Math.h"
#include <math.h>

float Math$sqrt(float x)
{
	return sqrtf(x);
}

float Math$power(float x, float b)
{
	return powf(b,x);
}

float Math$exp(float x)
{
	return expf(x);
}

float Math$ln(float x)
{
	return logf(x);
}

float Math$log(float x)
{
	return log10f(x);
}

float Math$round(float x)
{
	return roundf(x);
}

float Math$sin(float x)
{
	return sinf(x);
}

float Math$cos(float x)
{
	return cosf(x);
}

float Math$tan(float x)
{
	return tanf(x);
}

float Math$arcsin(float x)
{
	return asinf(x);
}

float Math$arccos(float x)
{
	return acosf(x);
}

float Math$arctan(float x)
{
	return atanf(x);
}

float Math$arctan2(float x, float y)
{
	return atan2f(x,y);
}

float Math$sinh(float x)
{
	return sinhf(x);
}

float Math$cosh(float x)
{
	return coshf(x);
}

float Math$tanh(float x)
{
	return tanhf(x);
}

float Math$arcsinh(float x)
{
	return asinhf(x);
}

float Math$arccosh(float x)
{
	return acoshf(x);
}

float Math$arctanh(float x)
{
	return atanhf(x);
}

void Math$init$()
{
}

OBX$Cmd Math$cmd$(const char* name)
{
	if( name == 0 ) return Math$init$;
	return 0;
}

