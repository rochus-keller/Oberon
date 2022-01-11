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

using System.Text;
using System;
using System.Globalization;

public class In
{
private static CultureInfo ci = new CultureInfo("en-US");


//VAR Done: BOOLEAN;
public static bool Done = false; 

//PROCEDURE Open;
public static void Open()
{
	Done = true;
}

//PROCEDURE Char (VAR ch: CHAR);
public static void Char(ref char ch)
{
	int res = System.Console.Read();
	if( res > 0 )
	{
		Done = true;
		ch = (char)res;
	}else
	{
		Done = false;
		ch = '\0';
	}
}

private static StringBuilder blockingRead(bool stringEnd = false)
{
	// TODO: Oakwood expects some kind of stream which is already in memory, i.e. which is not provided by the user char by char and doesn't block
	StringBuilder str = new StringBuilder();
	System.IO.Stream stream = Console.OpenStandardInput();
	byte[] buf = new byte[1];
	char ch;
	bool expectEnd = false;
	bool nonWsFound = false;
	while( true )
	{
		//ConsoleKeyInfo res = Console.ReadKey(); // sleeps until a key is pressed, but doesn't work with stdin, just with real key presses
		//ch = res.KeyChar;
		ch = '\0';
		if( stream.Read(buf,0,1) > 0 )
			ch = (char)buf[0];
		bool isWs = System.Char.IsWhiteSpace(ch);
		if( !stringEnd && nonWsFound && isWs )
			break;
		else if( System.Char.IsControl(ch) )
			continue; 
		if( !isWs || nonWsFound )
			str.Append(ch); // ignore trailing WS
		if( expectEnd && ch == '"' )
			break;
		expectEnd = stringEnd;
		if( !isWs )
			nonWsFound = true;
	}
	return str;
}

//PROCEDURE Int (VAR i: INTEGER);
public static void Int(ref int i)
{
	// IntConst = digit {digit} | digit {hexDigit} “H”
	string str = blockingRead().ToString().ToUpper();
	Done = false;
	i = 0;
	if( str.Length == 0 )
		return;
	try
	{
		if( str.EndsWith("H") )
		{
			i = Convert.ToInt32(str.Substring(0,str.Length-1), 16);
			Done = true;
		}else
		{
			i = Convert.ToInt32(str, 10);
			Done = true;
		}
	}catch
	{
	}
}

public static void LongInt(ref long i)
{
	// IntConst = digit {digit} | digit {hexDigit} “H”
	string str = blockingRead().ToString().ToUpper();
	Done = false;
	i = 0;
	if( str.Length == 0 )
		return;
	try
	{
		if( str.EndsWith("H") )
		{
			i = Convert.ToInt64(str.Substring(0,str.Length-1), 16);
			Done = true;
		}else
		{
			i = Convert.ToInt64(str, 10);
			Done = true;
		}
	}catch
	{
	}
}

//PROCEDURE Real (VAR x: REAL);
public static void Real(ref float x)
{
	// RealConst = digit {digit} [ "." {digit} [“E” (“+” | “-”) digit {digit}]]
	string str = blockingRead().ToString().ToLower();
	Done = false;
	x = 0.0f;
	if( str.Length == 0 )
		return;
	/*
	const int Lhs = 0;
	const int Rhs = 1;
	const int Exp = 2;
	int s = Lhs;
	*/
	try
	{
		// we don't actually need this parser because syntax is directly supported by ToDouble
		/*
		for( int i = 0; i < str.Length; i++ )
		{
			switch( s )
			{
			case Lhs:
				if( str[i] == '.' )
					s = Rhs;
				else if( !System.Char.IsDigit(str[i]) )
					throw new Exception();
				break;
			case Rhs:
				if( str[i] == 'E' )
					s = Exp;
				else if( !System.Char.IsDigit(str[i]) )
					throw new Exception();
				break;
			case Exp:
				if( str[i] == '+' || str[i] == '-' )
					s = Rhs;
				else if( !System.Char.IsDigit(str[i]) )
					throw new Exception();
				break;
			}
		}
		*/
		x = (float)Convert.ToDouble(str,ci);
		Done = true;
	}catch
	{
	}
}

public static void LongReal(ref double x)
{
	// RealConst = digit {digit} [ "." {digit} [“E” (“+” | “-”) digit {digit}]]
	string str = blockingRead().ToString().ToLower();
	Done = false;
	x = 0.0f;
	if( str.Length == 0 )
		return;
	try
	{
		x = Convert.ToDouble(str,ci);
		Done = true;
	}catch
	{
	}
}

//PROCEDURE String (VAR str: ARRAY OF CHAR);
public static void String(char[] str)
{
	// StringConst = ‘”’ char {char} ‘”’
	Done = false;
	str[0] = '\0';

/*
	StringBuilder sb = new StringBuilder();
	char ch;
	bool expectEnd = false;
	while( true )
	{
		ConsoleKeyInfo res = Console.ReadKey(); // sleeps until a key is pressed
		ch = res.KeyChar;
		if( System.Char.IsControl(ch) )
			return;
		sb.Append(ch);
		if( expectEnd && ch == '"' )
			break;
		expectEnd = true;
	}
	string tmp = sb.ToString();
	*/
	string tmp = blockingRead(true).ToString();
	if( tmp.Length < 2 || !tmp.EndsWith("\"") || !tmp.StartsWith("\"") )
		return;
	tmp = tmp.Substring(1,tmp.Length-2); // remove ""
	Done = true;
	int i;
	for( i = 0; i < tmp.Length; i++ )
	{
		str[i] = tmp[i];
		if( str[i] < ' ' ) // The string must not contain characters less than blank such as EOL or TAB.
		{
			Done = false;
			str[0] = '\0';
			return;
		}
	}
	str[i] = '\0';
}

//PROCEDURE Name (VAR name: ARRAY OF CHAR);
public static void Name(char[] name)
{
	string tmp = blockingRead().ToString();
	Done = true;
	int i;
	for( i = 0; i < tmp.Length; i++ )
	{
		name[i] = tmp[i];
	}
	name[i] = '\0';
}

	public static void begïn()
	{
	}

}
