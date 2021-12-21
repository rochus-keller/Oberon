#ifndef _OBX_MATH_
#define _OBX_MATH_
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
extern void Math$cmd$(const char*);


#endif
