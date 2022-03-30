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

namespace OBX
{
	using System.Runtime.InteropServices;
	using System.Collections;
	using System.Reflection;
	using System.IO;
	using System;
	
	public delegate void Command();
	public class Runtime
	{
		public static int DIV( int a, int b )
		{
			// source: http://lists.inf.ethz.ch/pipermail/oberon/2019/013353.html
			if( a < 0 )
				return (a - b + 1) / b;
			else
				return a / b;
		}
		public static long DIV( long a, long b )
		{
			// source: http://lists.inf.ethz.ch/pipermail/oberon/2019/013353.html
			if( a < 0 )
				return (a - b + 1) / b;
			else
				return a / b;
		}
		public static int MOD( int a, int b )
		{
			// source: http://lists.inf.ethz.ch/pipermail/oberon/2019/013353.html
			if (a < 0)
				return (b - 1) + (a - b + 1) % b;
			else
				return a % b;
		}
		public static long MOD( long a, long b )
		{
			// source: http://lists.inf.ethz.ch/pipermail/oberon/2019/013353.html
			if (a < 0)
				return (b - 1) + (a - b + 1) % b;
			else
				return a % b;
		}
		public static int strcmp( char[] lhs, char[] rhs )
		{
		    // source: https://stackoverflow.com/questions/34873209/implementation-of-strcmp/34873406
			for (int i = 0; ; i++)
			{
				if( lhs[i] != rhs[i] )
				    return lhs[i] < rhs[i] ? -1 : 1;

				if( lhs[i] == '\0' )
				    return 0;
			}
	    }
		public static bool relOp( char[] l, char[] r, int op )
		{
			switch( op )
			{
			case 1: // EQ
				return strcmp(l,r) == 0;
			case 2: // NEQ
				return strcmp(l,r) != 0;
			case 3: // LT
				return strcmp(l,r) < 0;
			case 4: // LEQ
				return strcmp(l,r) <= 0;
			case 5: // GT
				return strcmp(l,r) > 0;
			case 6: // GEQ
				return strcmp(l,r) >= 0;
			}
			return false;
		}
		public static bool relOp( string l, string r, int op )
		{
			switch( op )
			{
			case 1: // EQ
				return String.Compare(l,r) == 0;
			case 2: // NEQ
				return String.Compare(l,r) != 0;
			case 3: // LT
				return String.Compare(l,r) < 0;
			case 4: // LEQ
				return String.Compare(l,r) <= 0;
			case 5: // GT
				return String.Compare(l,r) > 0;
			case 6: // GEQ
				return String.Compare(l,r) >= 0;
			}
			return false;
		}
		public static bool relOp( char l, char r, int op )
		{
			switch( op )
			{
			case 1: // EQ
				return l == r;
			case 2: // NEQ
				return l != r;
			case 3: // LT
				return l < r;
			case 4: // LEQ
				return l <= r;
			case 5: // GT
				return l > r;
			case 6: // GEQ
				return l >= r;
			}
			return false;
		}
		public static bool relOp( char[] l, char r, int op )
		{
			if( l.Length == 2 )
				return relOp(l[0],r,op);
			// else
			char[] rr = new char[2];
			rr[0] = r;
			rr[1] = '\0';
			return relOp(l,rr,op);
		}
		public static bool relOp( char l, char[] r, int op )
		{
			if( r.Length == 2 )
				return relOp(l,r[0],op);
			// else
			char[] ll = new char[2];
			ll[0] = l;
			ll[1] = '\0';
			return relOp(ll,r,op);
		}
		internal static string ptrToStr( IntPtr ptr, bool wide )
		{
			if( wide )
				return Marshal.PtrToStringUni(ptr);
			else
				return Marshal.PtrToStringAnsi(ptr);
		}
		public static bool relOp( IntPtr l, IntPtr r, int op, bool lwide, bool rwide )
		{
			return relOp( ptrToStr(l,lwide), ptrToStr(r,rwide), op );
		}
		public static bool relOp( IntPtr l, char r, int op, bool lwide, bool rwide )
		{
			return relOp( ptrToStr(l,lwide), toString(toString(r)), op );
		}
		public static bool relOp( IntPtr l, char[] r, int op, bool lwide, bool rwide )
		{
			return relOp( ptrToStr(l,lwide), toString(r), op );
		}
		public static bool relOp( char l, IntPtr r, int op, bool lwide, bool rwide )
		{
			return relOp( toString(toString(l)), ptrToStr(r,rwide), op );
		}
		public static bool relOp( char[] l, IntPtr r, int op, bool lwide, bool rwide )
		{
			return relOp( toString(l), ptrToStr(r,rwide), op );
		}
		public static int strlen( char[] str )
		{
			int len = str.Length;
			for( int i=0; i < len; i++ )
			{
				if( str[i] == '\x0' )
					return i;
			}
			// TEST System.Console.WriteLine("strlen no terminating zero");
			return len;
		}
		public static char[] join( char[] lhs, char[] rhs )
		{
			int lhslen = strlen(lhs);
			int rhslen = strlen(rhs);
			int count = lhslen + rhslen + 1;
			char[] res = new char[count];
			for( int i = 0; i < lhslen; i++ )
				res[i] = lhs[i];
			for( int i = 0; i < rhslen; i++ )
				res[i+lhslen] = rhs[i];
			res[lhslen+rhslen] = '\x0';
			return res;
		}
		public static char[] join( char lhs, char rhs )
		{
			return join(toString(lhs),toString(rhs));
		}
		public static char[] join( char lhs, char[] rhs )
		{
			return join(toString(lhs),rhs);
		}
		public static char[] join( char[] lhs, char rhs )
		{
			return join(lhs,toString(rhs));
		}
		public static char[] toString(char ch)
		{
			char[] str = new char[2];
			str[0] = ch;
			str[1] = '\0';
			return str;
		}
		public static int addElemToSet(int set_, int elem )
		{
			return set_ | ( 1 << elem );
		}
		public static int removeElemFromSet(int set_, int elem )
		{
			return set_ & ~( 1 << elem );
		}
		public static int addRangeToSet(int set_, int from, int to )
		{
			if( from > to )
				return set_;
			for( int i=from; i <= to; i++ )
				set_ = addElemToSet(set_,i);
			return set_;
		}
		public static bool IN(int elem, int set_)
		{
			return ( ( 1 << elem ) & set_ ) != 0;
		}
		public static bool ODD(int n)
		{
			return !(n % 2 == 0);
		}
		public static bool ODD(long n)
		{
			return !(n % 2 == 0);
		}
		public static void CheckOvf(int i, int width)
		{
			if( width == 1 && i >= 0 && i <= 255 )
				return;
			else if( width == 2 && i >= -32768 && i <= 32767 )
				return;
			// else
			throw new System.OverflowException();
		}
		public static void PACK(ref float x, int n)
		{
			x = x * (float)Math.Pow(2, n);
		}
		public static void UNPACK(ref float x, ref int n)
		{
			// UNPACK(4,-10) -> 1,2
			//System.Console.WriteLine("unpack("+x.ToString()+" "+n.ToString());
			frexp(ref x, ref n);
			//System.Console.WriteLine("->"+x.ToString()+" "+n.ToString());
		}
		public static void frexp(ref float d, ref int e)
		{
			// source: https://stackoverflow.com/questions/389993/extracting-mantissa-and-exponent-from-double-in-c-sharp/390072#390072
			// Translate the double into sign, exponent and mantissa.
			long bits = BitConverter.DoubleToInt64Bits(d);
			// Note that the shift is sign-extended, hence the test against -1 not 1
			bool negative = (bits & (1L << 63)) != 0;
			int exponent = (int) ((bits >> 52) & 0x7ffL);
			long mantissa = bits & 0xfffffffffffffL;

			// Subnormal numbers; exponent is effectively one higher,
			// but there's no extra normalisation bit in the mantissa
			if (exponent==0)
			{
				exponent++;
			}
			// Normal numbers; leave exponent as it is but add extra
			// bit to the front of the mantissa
			else
			{
				mantissa = mantissa | (1L << 52);
			}

			// Bias the exponent. It's actually biased by 1023, but we're
			// treating the mantissa as m.0 rather than 0.m, so we need
			// to subtract another 52 from it.
			exponent -= 1075;

			if (mantissa == 0) 
			{
				d = mantissa;
				e = exponent;
				return;
			}

			/* Normalize */
			while((mantissa & 1) == 0) 
			{    /*  i.e., Mantissa is even */
				mantissa >>= 1;
				exponent++;
			}
			d = mantissa;
			e = exponent;
		}
		public static byte[] toAnsi(char[] str)
		{
			byte[] res = new byte[str.Length];
			for( int i = 0; i < str.Length; i++ )
				res[i] = (byte)str[i];
			return res;
		}
		public static void writeBack(char[] to, byte[] from)
		{
			for( int i = 0; i < from.Length; i++ )
				to[i] = (char)from[i];
		}
		public static string toString(char[] str)
		{
			int i = 0;
			while( i < str.Length && str[i] != 0 )
				i++;
			return new string(str,0,i);
		}
		private static string assemblyPath( string name )
		{
			return AppDomain.CurrentDomain.BaseDirectory + name + ".dll";
		}
		public static bool loadModule( char[] name )
		{
			try
			{
				string n = toString(name);
				// Assembly a = Assembly.Load(n); looks at the wrong place with CoreCLR (but ok with .NET)
				string path = assemblyPath(n);
				Assembly a = Assembly.LoadFile(path);
				if( a == null )
				{
					Console.WriteLine("cannot load "+ path);
					return false;
				}
				Type t = a.GetType(n);
				if( t == null )
					return false;
				MethodInfo m = t.GetMethod("begïn");
				if( m == null )
				{
					System.Console.WriteLine("cannot find begïn method in " + n);
					return false;
				}
				m.Invoke(null,null);
				return true;
			}catch
			{
				return false;
			}
		}
		public static Command getCommand( char[] module, char[] proc )
		{
			try
			{
				string m = toString(module);
				string path = assemblyPath(m);
				Assembly a = Assembly.LoadFile(path);
				if( a == null )
				{
					Console.WriteLine("cannot load "+ path);
					return null;
				}
				Type t = a.GetType(m);
				if( t == null )
					return null;
				string n = toString(proc);
				MethodInfo p = t.GetMethod(n);
				if( p == null )
					return null;
				// else
				// Console.WriteLine("found "+new string(module)+"::"+new string(proc)); // TEST
				return (Command)Delegate.CreateDelegate(typeof(Command),p);
			}catch
			{
				return null;
			}
		}
		public static bool pcall(OBX.Command cmd, bool report)
		{
			try
			{
				cmd.Invoke();
				return true;
			}catch( Exception e )
			{
				if( report )
				{
					TextWriter errorWriter = Console.Error;
					errorWriter.WriteLine(e.ToString());
				}
				return false;
			}
		}
		public static void copy( char[] lhs, char[] rhs )
		{
			int i = 0; 
			while( rhs[i] != '\0' )
			{
				lhs[i] = rhs[i];
				i++;
			}
			lhs[i] = '\0';
		}
		public static void copy( IntPtr lhs, char[] rhs, bool wide )
		{
			int i = 0; 
			while( rhs[i] != '\0' )
			{
				if( wide )
					Marshal.WriteInt16(lhs, i * 2, rhs[i]);
				else
					Marshal.WriteByte(lhs, i, (byte)rhs[i]);
				i++;
			}
			if( wide )
				Marshal.WriteInt16(lhs, i * 2, 0);
			else
				Marshal.WriteByte(lhs, i, 0);
		}
		public static void copy( char[] lhs, IntPtr rhs, bool wide )
		{
			int i = 0; 
			while( true )
			{
				uint ch;
				if( wide )
					ch = (ushort)Marshal.ReadInt16(rhs, i * 2);
				else
					ch = Marshal.ReadByte(rhs, i);
				lhs[i] = (char)ch;
				if( ch == 0 )
					break;
				i++;
			}
		}
		public static void copy( IntPtr lhs, IntPtr rhs, bool lwide, bool rwide )
		{
			int i = 0; 
			while( true )
			{
				uint ch;
				if( rwide )
					ch = (ushort)Marshal.ReadInt16(rhs, i * 2);
				else
					ch = Marshal.ReadByte(rhs, i);
				if( lwide )
					Marshal.WriteInt16(lhs, i * 2, (short)(ushort)ch);
				else if( ch > 255 )
					Marshal.WriteByte(lhs, i, 0x20); // TODO
				else
					Marshal.WriteByte(lhs, i, (byte)ch);
				if( ch == 0 )
					break;
				i++;
			}
		}
		public static void checkPtrSize(int s)
		{
			if( s != IntPtr.Size )
			{
				throw new Exception(string.Format("This assembly only works in a {0} bit process", s * 8));
			}
		}
		
		private static Hashtable staticDelegs = new Hashtable();
		// TODO public static Delegate
		
		private static ArrayList keepRefs = new ArrayList();
		public static void addRef( object o )
		{
			if( !keepRefs.Contains(o) )
				keepRefs.Add(o);
		}
		
		public static string toHex(uint adr)
		{
			return String.Format("0x{0:x}",adr);
		}
		public static string toHex(ulong adr)
		{
			return String.Format("0x{0:x}",adr);
		}
		
		public static long Ash64(long x, int n, bool arithmetic)
		{
			if( n >= 0 )
				return x << n;
			else
				return x >> (-n); // uses shr.un
		}

		public static int Ash32(int x, int n, bool arithmetic)
		{
			if( n >= 0 )
				return x << n;
			else
				return x >> (-n); // uses shr.un
		}
		
		// https://stackoverflow.com/a/9995303/10830469
		public static byte[] StringToByteArray(string hex) {
			int len = hex.Length >> 1;
		    byte[] arr = new byte[len];

		    for (int i = 0; i < len; ++i)
		    {
		        arr[i] = (byte)((GetHexVal(hex[i << 1]) << 4) + (GetHexVal(hex[(i << 1) + 1])));
		    }

		    return arr;
		}
		public static int GetHexVal(char hex) {
		    int val = (int)hex;
		    //For uppercase A-F letters:
		    //return val - (val < 58 ? 48 : 55);
		    //For lowercase a-f letters:
		    //return val - (val < 58 ? 48 : 87);
		    //Or the two combined, but a bit slower:
        	return val - (val < 58 ? 48 : (val < 97 ? 55 : 87));
    	}
	}
}
