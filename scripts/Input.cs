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

public class Input
{
	//CONST TimeUnit = 1000000;
	
	//PROCEDURE Available (): INTEGER;
	private static int cur = -1;
	public static int Available()
	{
		if( cur >= 0 )
			return 1;
		cur = System.Console.Read();
		if( cur >= 0 )
			return 1;
		else
			return 0;
	}
	
	//PROCEDURE Read (VAR ch: CHAR);
	public static void Read(ref char ch)
	{
		if( cur >= 0 )
		{
			ch = (char)cur;
			cur = -1;
		}else
			ch = '\0';
	}
	
	//PROCEDURE Mouse (VAR keys: SET; VAR x, y: INTEGER);
	public static void Mouse( ref uint keys, ref int x, ref int y)
	{
		// TODO
	}
	
	//PROCEDURE SetMouseLimits (w, h: INTEGER);
	public static void SetMouseLimits(int w, int h)
	{
		// NOP
	}
	
	static private System.DateTime start = System.DateTime.Now;
	
	//PROCEDURE Time (): INTEGER;
	public static int Time()
	{
		// A single tick represents one hundred nanoseconds or one ten-millionth of a second. There are 10,000 ticks in a millisecond.
		return (int)( ( System.DateTime.Now.Ticks - start.Ticks ) / 10 ); // microseconds
	}
	
}
