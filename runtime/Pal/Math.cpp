/*
* Copyright 2023 Rochus Keller <mailto:me@rochus-keller.ch>
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
*/

extern "C" {
#include "ObxPalApi.h"
#include <math.h>

double PAL_sin(double x)
{
    return sin(x);
}

double PAL_cos(double x)
{
    return cos(x);
}

double PAL_arctan(double x)
{
    return atan(x);
}

double PAL_sqrt(double x)
{
    return sqrt(x);
}

double PAL_ln(double x)
{
    return log(x);
}

double PAL_exp(double x)
{
    return exp(x);
}

double PAL_pow(double x, double e)
{
    return pow(x,e);
}

}
