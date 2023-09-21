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
*/

using System.Globalization;

public class Out
{
private static CultureInfo ci = new CultureInfo("en-US");

//PROCEDURE Open;
public static void Open()
{
	// NOP
}

//PROCEDURE Char (ch: CHAR);
public static void Char(char ch)
{
	System.Console.Write(ch);
}

private static string toString(char[] str)
{
	int i = 0;
	while( i < str.Length && str[i] != 0 )
		i++;
	return new string(str,0,i);
}

//PROCEDURE String (str: ARRAY OF CHAR);
public static void String(char[] str)
{
	System.Console.Write(toString(str));
}

//PROCEDURE Int (i, n: INTEGER);
public static void Int(long i, int n)
{
	if( n > 0 )
		System.Console.Write("{0,"+n.ToString()+"}",i);
	else
		System.Console.Write(i); 
}

//PROCEDURE Real (x: REAL; n: INTEGER);
public static void Real(float x, int n)
{
	if( n > 0 )
		System.Console.Write(System.String.Format(ci, "{0,"+n.ToString()+":E}", x ));
	else	
		System.Console.Write(x); 
}

public static void LongReal(double x, int n)
{
	if( n > 0 )
		System.Console.Write(System.String.Format(ci, "{0,"+n.ToString()+":E}", x ));
	else	
		System.Console.Write(x); 
}

//PROCEDURE Ln;
public static void Ln()
{
 	System.Console.Write("\n");
}

public static void begïn()
{
}

}
