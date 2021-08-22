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

public class In
{
//VAR Done: BOOLEAN;
public static bool Done; 

//PROCEDURE Open;
public static void Open()
{
	// NOP
}

//PROCEDURE Char (VAR ch: CHAR);
public static void Char(ref char ch)
{
	// TODO
}

//PROCEDURE Int (VAR i: INTEGER);
public static void Int(ref int i)
{
	// TODO
}

//PROCEDURE Real (VAR x: REAL);
public static void Real(ref float x)
{
	// TODO
}

//PROCEDURE String (VAR str: ARRAY OF CHAR);
public static void String(ref char[] str)
{
	// TODO
}

//PROCEDURE Name (VAR name: ARRAY OF CHAR);
public static void Name(ref char[] name)
{
	// TODO
}

}
