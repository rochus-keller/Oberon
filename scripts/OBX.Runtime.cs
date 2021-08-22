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
		public static int strlen( char[] str )
		{
			int len = str.Length;
			for( int i=0; i < len; i++ )
			{
				if( str[i] == '\x0' )
					return i;
			}
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
		public static bool ODD(int n)
		{
			return !(n % 2 == 0);
		}
		public static void PACK(ref float x, int n)
		{
			x = x * (float)System.Math.Pow(2, n);
		}
		public static void UNPACK(ref float x, ref int n)
		{
			frexp(ref x, ref n);
			x = x + x;
			n = n - 1;
		}
		public static void frexp(ref float d, ref int e)
		{
			// source: https://stackoverflow.com/questions/389993/extracting-mantissa-and-exponent-from-double-in-c-sharp/390072#390072
			// Translate the double into sign, exponent and mantissa.
			long bits = System.BitConverter.DoubleToInt64Bits(d);
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
	}
}
