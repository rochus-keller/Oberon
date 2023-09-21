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

public class Input
{
	//CONST TimeUnit = 1000000;
	
	//PROCEDURE Available (): INTEGER;
	private static int cur = -1;
	public static int Available()
	{
		return XYplane.available();
	}
	
	//PROCEDURE Read (VAR ch: CHAR);
	public static void Read(ref char ch)
	{
		ch = XYplane.dequeue();
	}
	
	private static int _w = 0, _h = 0;
	
	//PROCEDURE Mouse (VAR keys: SET; VAR x, y: INTEGER);
	public static void Mouse( ref int keys, ref int x, ref int y)
	{
		XYplane.GetMouseState(ref keys, ref x, ref y);
		// System.Console.WriteLine("x,y: "+ x.ToString() + " " + y.ToString());
		if( x < 0 )
			x = 0;
		else if( _w != 0 && x > _w )
			x = _w;
		if( y < 0 )
			y = 0;
		else if( _h != 0 && y > _h )
			y = _h;
	}
	
	//PROCEDURE SetMouseLimits (w, h: INTEGER);
	public static void SetMouseLimits(int w, int h)
	{
		_w = w;
		_h = h;
	}
	
	static private System.DateTime start = System.DateTime.Now;
	
	//PROCEDURE Time (): INTEGER;
	public static int Time()
	{
		// A single tick represents one hundred nanoseconds or one ten-millionth of a second. There are 10,000 ticks in a millisecond.
		return (int)( ( System.DateTime.Now.Ticks - start.Ticks ) / 10 ); // microseconds
	}
	
	public static void begïn()
	{
	}

}
