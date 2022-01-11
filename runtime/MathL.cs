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

public class MathL
{

	//PROCEDURE sqrt (x : LONGREAL) : LONGREAL;
	public static double sqrt(double x)
	{
		return System.Math.Sqrt(x);
	}
	//PROCEDURE power (x,base : LONGREAL) : LONGREAL;
	public static double power(double x, double b)
	{
		return System.Math.Pow(b,x);
	}
	//PROCEDURE exp (x : LONGREAL): LONGREAL;
	public static double exp(double x)
	{
		return System.Math.Exp(x);
	}
	//PROCEDURE ln (x : LONGREAL) : LONGREAL;
	public static double ln(double x)
	{
		return System.Math.Log(x);
	}
	//PROCEDURE log (x,base : LONGREAL) : LONGREAL;
	public static double log(double x)
	{
		return System.Math.Log10(x);
	}
	//PROCEDURE round (x : LONGREAL) : LONGREAL;
	public static double round(double x)
	{
		return System.Math.Round(x);
	}
	//PROCEDURE sin (x : LONGREAL) : LONGREAL;
	public static double sin(double x)
	{
		return System.Math.Sin(x);
	}
	//PROCEDURE cos (x : LONGREAL) : LONGREAL;
	public static double cos(double x)
	{
		return System.Math.Cos(x);
	}
	//PROCEDURE tan (x : LONGREAL) : LONGREAL;
	public static double tan(double x)
	{
		return System.Math.Tan(x);
	}
	//PROCEDURE arcsin (x : LONGREAL) : LONGREAL;
	public static double arcsin(double x)
	{
		return System.Math.Asin(x);
	}
	//PROCEDURE arccos (x : LONGREAL) : LONGREAL;
	public static double arccos(double x)
	{
		return System.Math.Acos(x);
	}
	//PROCEDURE arctan (x : LONGREAL) : LONGREAL;
	public static double arctan(double x)
	{
		return System.Math.Atan(x);
	}
	//PROCEDURE arctan2(x,y : LONGREAL): LONGREAL;
	public static double arctan2(double x, double y)
	{
		return System.Math.Atan2(x,y);
	}
	//PROCEDURE sinh (x:LONGREAL):LONGREAL;
	public static double sinh(double x)
	{
		return System.Math.Sinh(x);
	}
	//PROCEDURE cosh (x:LONGREAL):LONGREAL;
	public static double cosh(double x)
	{
		return System.Math.Cosh(x);
	}
	//PROCEDURE tanh (x:LONGREAL):LONGREAL;
	public static double tanh(double x)
	{
		return System.Math.Tanh(x);
	}
	//PROCEDURE arcsinh(x:LONGREAL):LONGREAL;
	public static double arcsinh(double x)
	{
		throw new System.Exception("not implemented");
		// return System.Math.Asinh(x); // not available in mono3
	}
	//PROCEDURE arccosh(x:LONGREAL):LONGREAL;
	public static double arccosh(double x)
	{
		throw new System.Exception("not implemented");
	}
	//PROCEDURE arctanh(x:LONGREAL):LONGREAL;
	public static double arctanh(double x)
	{
		throw new System.Exception("not implemented");
	}

	public static void begïn()
	{
	}

}
