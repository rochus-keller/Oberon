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

using System;
using System.IO;

public class Files
{
	public class Handle
	{
		public string name;
		public MemoryStream stream;
	}
	public class Rider
	{
		public bool eof;
		public int res;
		public int pos;
		public Handle file;
	}
	
	private static string toString(char[] str)
	{
		int i = 0;
		while( i < str.Length && str[i] != 0 )
			i++;
		return new string(str,0,i);
	}


//PROCEDURE Old (IN name: ARRAY OF CHAR): File;
	public static Handle Old(char[] filename)
	{
		string name = toString(filename);
		if( String.IsNullOrEmpty(name) )
			return null;
		try
		{
			Handle f = new Handle();
			f.name = name;
			f.stream = new MemoryStream(System.IO.File.ReadAllBytes(name));
			f.stream.Seek(0, SeekOrigin.Begin);
		    return f;
        }catch
        {
        	return null;
        }
	}
//PROCEDURE New (IN name: ARRAY OF CHAR): File;
	public static Handle New(char[] name)
	{
		Handle f = new Handle();
		f.name = toString(name);
		f.stream = new MemoryStream();
	    return f;
	}
//PROCEDURE Register (f: File);
	public static void Register(Handle f)
	{
		if( f.stream != null )
		{
			try
			{
				FileStream file = new FileStream( f.name, FileMode.Create, FileAccess.Write);
		        f.stream.WriteTo(file);
		        file.Close();
			}catch 
			{
			}
		}
	}
//PROCEDURE Close (f: File);
	public static void Close(Handle f)
	{
		if( f.stream != null )
		{
			f.stream.Close();
			f.stream = null;
		}
	}

//PROCEDURE Purge (f: File);
	public static void Purge(Handle f)
	{
		if( f.stream != null )
		{
			f.stream.SetLength(0);
			f.stream.Seek(0, SeekOrigin.Begin);
		}
	}

//PROCEDURE Delete (IN name: ARRAY OF CHAR; VAR res: INTEGER);
	public static void Delete(char[] filename, ref int res)
	{
		try
		{
			string name = toString(filename);
			System.IO.File.Delete(name);
			res = 0;
		}catch 
		{
			res = 1;
		}
	}

//PROCEDURE Rename (IN old, new: ARRAY OF CHAR;VAR res: INTEGER);
	public static void Rename(char[] oldName, char[] newName, ref int res)
	{
		try
		{
			string old = toString(oldName);
			string name = toString(newName);
			System.IO.File.Move(old, name);
			res = 0;
		}catch
		{
			res = 1;
		}
	}
	
//PROCEDURE Length (f: File): INTEGER;
	public static int Length(Handle f)
	{
		if( f.stream != null )
			return (int)f.stream.Length;
		else
			return 0;
	}

//PROCEDURE GetDate (f: File; VAR t, d: INTEGER);
// The encoding is: hour = t DIV 4096; minute = t DIV 64 MOD 64; second = t MOD 64; 
// year = d DIV 512; month = d DIV 32 MOD 16; day = d MOD 32.
	public static void GetDate(Handle f, ref int t, ref int d )
	{
		try
		{
			DateTime dt = System.IO.File.GetLastWriteTime(f.name);
			t = dt.Hour * 4096 + dt.Minute * 64 + dt.Second;
			d = dt.Year * 512 + dt.Month * 32 + dt.Day;
		}catch
		{
			t = 0;
			d = 0;
		}
	}

//PROCEDURE Set (VAR r: Rider; f: File; pos: INTEGER);
	public static void Set(Rider r, Handle f, int pos )
	{
		r.file = f;
		r.pos = pos;
		r.eof = false;
		r.res = 0;
	}
//PROCEDURE Pos (VAR r: Rider): INTEGER;
	public static int Pos(Rider r)
	{
		return r.pos;
	}
	
//PROCEDURE Base (VAR r: Rider): File;
	public static Handle Base(Rider r)
	{
		return r.file;
	}
	
//PROCEDURE Read (VAR r: Rider; VAR x: BYTE);
	public static void Read(Rider r, ref byte x)
	{
		if( r.file != null && r.file.stream != null )
		{	
			if( r.pos < 0 )
				r.pos = 0;
			if( r.file.stream.Seek(r.pos, SeekOrigin.Begin) == r.pos )
			{
				r.res = 0;
				r.eof = r.file.stream.Position >= r.file.stream.Length;
				if( r.eof )
					r.res = 1;
				else
				{
					x = (byte)r.file.stream.ReadByte();
					r.pos++;
				}
			}else
			{
				r.res = 1;
				x = 0;
			}
		}else
		{
			r.res = 1;
			x = 0;
		}
	}
	
//PROCEDURE ReadInt (VAR R: Rider; VAR x: INTEGER);
	public static void ReadInt(Rider r, ref int x)
	{
		byte x0 = 0, x1 = 0, x2 = 0, x3 = 0;
  		Read(r, ref x0); Read(r, ref x1); Read(r, ref x2); Read(r, ref x3);
    	x = ((x3 * 0x100 + x2) * 0x100 + x1) * 0x100 + x0;
	}
	
	public static void ReadLInt(Rider r, ref long x)
	{
		byte x0 = 0, x1 = 0, x2 = 0, x3 = 0, x4 = 0, x5 = 0, x6 = 0, x7 = 0;
  		Read(r, ref x0); Read(r, ref x1); Read(r, ref x2); Read(r, ref x3);
  		Read(r, ref x4); Read(r, ref x5); Read(r, ref x6); Read(r, ref x7);
    	x = ((((((x7 * 0x100 + x6) * 0x100 + x5) * 0x100 + x4) * 0x100 + x3) * 0x100 + x2) * 0x100 + x1) * 0x100 + x0;
	}
	
//PROCEDURE ReadReal (VAR R: Rider; VAR x: REAL);
	public static void ReadReal(Rider r, ref float x)
	{
		byte[] b = new byte[4];
		Read(r,ref b[0]);
		Read(r,ref b[1]);
		Read(r,ref b[2]);
		Read(r,ref b[3]);
		float[] f = new float[1];
		System.Buffer.BlockCopy(b,0,f,0,4);
		x = f[0];
	}
	
	public static void ReadLReal(Rider r, ref double x)
	{
		byte[] b = new byte[8];
		Read(r,ref b[0]);
		Read(r,ref b[1]);
		Read(r,ref b[2]);
		Read(r,ref b[3]);
		Read(r,ref b[4]);
		Read(r,ref b[5]);
		Read(r,ref b[6]);
		Read(r,ref b[7]);
		double[] d = new double[1];
		System.Buffer.BlockCopy(b,0,d,0,8);
		x = d[0];
	}

//PROCEDURE ReadNum (VAR R: Rider; VAR x: INTEGER);
	public static void ReadNum(Rider R, ref int x)
	{
	    uint n, y; byte b = 0;
  		n = 32; y = 0; Read(R, ref b);
    	while( b >= 0x80 ) { y = y + b-0x80 >> 7; n -= 7; Read(R, ref b); }
    	if( n <= 4 ) x = (int)(y + b % 0x10 >> 4); else x = (int)(y + b >> 7) >> (int)n-7;
	}
//PROCEDURE ReadString (VAR R: Rider; VAR x: ARRAY OF CHAR);
	public static void ReadString(Rider R, char[] x)
	{
		int i; byte ch = 0;
		i = 0; Read(R, ref ch);
		while( ch != 0 )
		{
		  if( i < x.Length-1 ) 
		  {
		  	x[i] = (char)ch; 
		  	i++;
		  }
		  Read(R, ref ch);
		}
		x[i] = '\0';
	}
//PROCEDURE ReadSet (VAR R: Rider; VAR x: SET);
	public static void ReadSet(Rider R, ref uint x)
	{
		int i = 0;
		ReadInt(R,ref i);
		x = (uint)i;
	}
//PROCEDURE ReadBool (VAR R: Rider; VAR x: BOOLEAN );
	public static void ReadBool(Rider R, ref bool x)
	{
		byte b = 0;
		Read(R,ref b);
		x = b != 0;
	}
//PROCEDURE ReadBytes (VAR r: Rider; VAR x: ARRAY OF BYTE; n: INTEGER);
	public static void ReadBytes(Rider r, byte[] x, int n )
	{
		int i;
	  	i = 0;  
		while( i < n ) { Read(r, ref x[i]); i++; }
	}
//PROCEDURE Write (VAR r: Rider; x: BYTE);
	public static void Write(Rider r, byte x)
	{
		if( r.file != null && r.file.stream != null )
		{	
			if( r.pos < 0 )
				r.pos = 0;
			if( r.file.stream.Seek(r.pos, SeekOrigin.Begin) == r.pos )
			{
				r.res = 0;
				r.eof = false;
				r.file.stream.WriteByte(x);
				r.pos++;
			}else
				r.res++;
		}else
			r.res++;
	}
//PROCEDURE WriteInt (VAR R: Rider; x: INTEGER);
	public static void WriteInt(Rider R, int x)
	{
		Write(R, (byte)(x % 0x100));
		Write(R, (byte)(x / 0x100 % 0x100));
		Write(R, (byte)(x / 0x10000 % 0x100));
		Write(R, (byte)(x / 0x1000000 % 0x100));
	}
	public static void WriteLInt(Rider R, long x)
	{
		Write(R, (byte)(x % 0x100));
		Write(R, (byte)(x / 0x100 % 0x100));
		Write(R, (byte)(x / 0x10000 % 0x100));
		Write(R, (byte)(x / 0x1000000 % 0x100));
		Write(R, (byte)(x / 0x100000000 % 0x100));
		Write(R, (byte)(x / 0x10000000000 % 0x100));
		Write(R, (byte)(x / 0x1000000000000 % 0x100));
		Write(R, (byte)(x / 0x100000000000000 % 0x100));
	}
//PROCEDURE WriteReal (VAR R: Rider; x: REAL);
	public static void WriteReal(Rider r, float x)
	{
		byte[] b = new byte[4];
		float[] f = new float[1];
		f[0] = x;
		System.Buffer.BlockCopy(f,0,b,0,4);
		Write(r,b[0]);
		Write(r,b[1]);
		Write(r,b[2]);
		Write(r,b[3]);
	}
	public static void WriteLReal(Rider r, double x)
	{
		byte[] b = new byte[8];
		double[] d = new double[1];
		d[0] = x;
		System.Buffer.BlockCopy(d,0,b,0,8);
		Write(r,b[0]);
		Write(r,b[1]);
		Write(r,b[2]);
		Write(r,b[3]);
		Write(r,b[4]);
		Write(r,b[5]);
		Write(r,b[6]);
		Write(r,b[7]);
	}
//PROCEDURE WriteNum (VAR R: Rider; x: INTEGER);
	public static void WriteNum(Rider R, int x)
	{
		while( x < -0x40 || x >= 0x40 ) { Write(R, (byte)(x % 0x80 + 0x80)); x = x >> 7; }
		Write(R, (byte)( x % 0x80));
	}
//PROCEDURE WriteString (VAR R: Rider; IN x: ARRAY OF CHAR);
	public static void WriteString(Rider R, char[] x)
	{
		int i; char ch;
		i = 0;
		do
		{ ch = x[i]; Write(R, (byte)ch); i++;
		}while( ch != '\0' );
	}
//PROCEDURE WriteSet (VAR R: Rider; x: SET);
	public static void WriteSet(Rider R, uint x)
	{
		WriteInt(R,(int)x);
	}
//PROCEDURE WriteBool (VAR R: Rider; x: BOOLEAN);
	public static void WriteBool(Rider R, bool x)
	{
		Write(R, (byte)(x ? 1:0) );
	}
//PROCEDURE WriteBytes (VAR r: Rider; VAR x: ARRAY OF BYTE;n: INTEGER);
	public static void WriteBytes(Rider r, byte[] x, int n)
	{
		int i = 0; 
    	while( i < n ) { Write(r, x[i]); i++; }
	}
}
