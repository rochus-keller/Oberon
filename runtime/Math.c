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

