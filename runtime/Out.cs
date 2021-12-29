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
public static void Int(int i, int n)
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

//PROCEDURE Ln;
public static void Ln()
{
 	System.Console.Write("\n");
}

}
