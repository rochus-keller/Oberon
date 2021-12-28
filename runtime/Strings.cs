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

public class Strings
{
	// TODO make this more efficient some day 

	// PROCEDURE Length (s: ARRAY OF CHAR): INTEGER;
	public static int Length( char[] str )
	{
		int len = str.Length;
		for( int i=0; i < len; i++ )
		{
			if( str[i] == '\x0' )
				return i;
		}
		return len;
	}

	// PROCEDURE Insert (source: ARRAY OF CHAR; pos: INTEGER; VAR dest: ARRAY OF CHAR);
	public static void Insert( char[] source, int pos, char[] dest )
	{
		int slen = Length(source);
		string s = new string(source,0,slen);
		string d1 = new string(dest);
		char[] d2 = d1.Insert(pos,s).ToCharArray();
	
		int dlen = dest.Length-1;
		slen = d2.Length; // this is the length of the combined string without the terminating zero 
		int len = dlen < slen ? dlen : slen;
		for( int i = 0; i < len; i++ )
			dest[i]=d2[i];
		dest[len] = '\0';
	}

	// PROCEDURE Append (extra: ARRAY OF CHAR; VAR dest: ARRAY OF CHAR);
	public static void Append(char[] extra, char[] dst )
	{
		Insert(extra,Length(dst),dst);
	}

	// PROCEDURE Delete (VAR s: ARRAY OF CHAR; pos, n: INTEGER);
	public static void Delete( char[] s, int pos, int n )
	{
		string str = new string(s,0,Length(s));
		str = str.Remove(pos,n);
		int len = str.Length;
		for( int i = 0; i < len; i++ )
			s[i] = str[i];
		s[len] = '\0';
	}

	// PROCEDURE Replace (source: ARRAY OF CHAR; pos: INTEGER; VAR dest: ARRAY OF CHAR);
	public static void Replace( char[] src, int pos, char[] dst )
	{
		Delete(dst, pos, Length(src));
		Insert(src, pos, dst);
	}

	// PROCEDURE Extract (source: ARRAY OF CHAR; pos, n: INTEGER; VAR dest: ARRAY OF CHAR);
	public static void Extract(char[] src, int pos, int n, char[] dest )
	{
		for( int i = 0; i < n; i++ )
			dest[i] = src[i+pos];
	}

	// PROCEDURE Pos (pattern, s: ARRAY OF CHAR; pos: INTEGER): INTEGER;
	public static int Pos( char[] pattern, char[] s, int pos )
	{
		string str = new string(s,0,Length(s));
		string pat = new string(pattern,0,Length(pattern));
		return str.IndexOf(pat,pos);
	}

	// PROCEDURE Cap (VAR s: ARRAY OF CHAR);
	public static void Cap( char[] s )
	{
		int len = Length(s);
		for( int i = 0; i < len; i++ )
			s[i] = System.Char.ToUpper(s[i]);
	}
}
