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

public class Math
{

	//PROCEDURE sqrt (x : REAL) : REAL;
	public static float sqrt(float x)
	{
		return (float)System.Math.Sqrt(x);
	}
	//PROCEDURE power (x,base : REAL) : REAL;
	public static float power(float x, float b)
	{
		return (float)System.Math.Pow(b,x);
	}
	//PROCEDURE exp (x : REAL): REAL;
	public static float exp(float x)
	{
		return (float)System.Math.Exp(x);
	}
	//PROCEDURE ln (x : REAL) : REAL;
	public static float ln(float x)
	{
		return (float)System.Math.Log(x);
	}
	//PROCEDURE log (x,base : REAL) : REAL;
	public static float log(float x)
	{
		return (float)System.Math.Log10(x);
	}
	//PROCEDURE round (x : REAL) : REAL;
	public static float round(float x)
	{
		return (float)System.Math.Round(x);
	}
	//PROCEDURE sin (x : REAL) : REAL;
	public static float sin(float x)
	{
		return (float)System.Math.Sin(x);
	}
	//PROCEDURE cos (x : REAL) : REAL;
	public static float cos(float x)
	{
		return (float)System.Math.Cos(x);
	}
	//PROCEDURE tan (x : REAL) : REAL;
	public static float tan(float x)
	{
		return (float)System.Math.Tan(x);
	}
	//PROCEDURE arcsin (x : REAL) : REAL;
	public static float arcsin(float x)
	{
		return (float)System.Math.Asin(x);
	}
	//PROCEDURE arccos (x : REAL) : REAL;
	public static float arccos(float x)
	{
		return (float)System.Math.Acos(x);
	}
	//PROCEDURE arctan (x : REAL) : REAL;
	public static float arctan(float x)
	{
		return (float)System.Math.Atan(x);
	}
	//PROCEDURE arctan2(x,y : REAL): REAL;
	public static float arctan2(float x, float y)
	{
		return (float)System.Math.Atan2(x,y);
	}
	//PROCEDURE sinh (x:REAL):REAL;
	public static float sinh(float x)
	{
		return (float)System.Math.Sinh(x);
	}
	//PROCEDURE cosh (x:REAL):REAL;
	public static float cosh(float x)
	{
		return (float)System.Math.Cosh(x);
	}
	//PROCEDURE tanh (x:REAL):REAL;
	public static float tanh(float x)
	{
		return (float)System.Math.Tanh(x);
	}
	//PROCEDURE arcsinh(x:REAL):REAL;
	public static float arcsinh(float x)
	{
		throw new System.Exception("not implemented");
		// return (float)System.Math.Asinh(x); // not available in mono3
	}
	//PROCEDURE arccosh(x:REAL):REAL;
	public static float arccosh(float x)
	{
		throw new System.Exception("not implemented");
	}
	//PROCEDURE arctanh(x:REAL):REAL;
	public static float arctanh(float x)
	{
		throw new System.Exception("not implemented");
	}
	
	public static void begïn()
	{
	}

}
