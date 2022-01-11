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

using System.IO;
using System.Collections.Generic;
using System.Reflection;
using System;
using System.Runtime.InteropServices;

public class ObFiles
{
	static List<MemoryStream> s_buffers = new List<MemoryStream>();
	static readonly long s_start;
	static string[] s_files;

	static ObFiles()
	{
		s_start = System.DateTime.Now.Ticks;
	}
	

	private static int getFreeBufferSlot(MemoryStream b)
	{
		for( int i = 0; i < s_buffers.Count; i++ )
		{
		    if( s_buffers[i] == null )
		    {
		        s_buffers[i] = b;
		        return i;
		    }
		}
		s_buffers.Add( b );
		return s_buffers.Count - 1;
	}
	
	private static MemoryStream getBuffer(int i)
	{
		if( i >= 0 && i < s_buffers.Count )
		    return s_buffers[i];
		else
		    return null;
	}
	
	public static int getTime()
	{
		// A single tick represents one hundred nanoseconds or one ten-millionth of a second. There are 10,000 ticks in a millisecond.
		return (int)( ( System.DateTime.Now.Ticks - s_start ) / 10000 ); 
	}
  	  
	public static int listFiles()
	{
		s_files = System.IO.Directory.GetFiles(getPath());
		return s_files.Length;
	}

	public static char[] fileName(int i)
	{
		string name = System.IO.Path.GetFileName(s_files[i]);
		// String.ToCharArray returns a non zero-terminated array
		char[] str = new char[name.Length + 1];
		for( i = 0; i < name.Length; i++ )
			str[i] = name [i];
		str[i] = '\0';	
		return str;
	}
	
	private static string getPath()
	{
		string path = System.Environment.GetEnvironmentVariable("OBERON_FILE_SYSTEM_ROOT");
		if( path == null )
		{
			path = System.AppDomain.CurrentDomain.BaseDirectory;
			path += "Files";
		}
		return path;
	}
	
	private static string toString(char[] str)
	{
		int i = 0;
		while( i < str.Length && str[i] != 0 )
			i++;
		return new string(str,0,i);
	}
		
	public static int openFile(char[] filename)
	{
		string name = toString(filename);
		if( String.IsNullOrEmpty(name) )
			return -1;
		try
		{
			name = getPath() + Path.DirectorySeparatorChar + name;
			MemoryStream b = new MemoryStream(System.IO.File.ReadAllBytes(name));
			b.Seek(0, SeekOrigin.Begin);
		    return getFreeBufferSlot(b);
        }catch(Exception e)
        {
        	Console.WriteLine("cannot open file '"+name+"'\n"+e.ToString());
        	return -1;
        }
	}
	
	public static int newFile()
	{
		MemoryStream b = new MemoryStream();
        return getFreeBufferSlot(b);
	}
	
	public static void freeFile(int fb)
	{
		if( fb >= 0 && fb < s_buffers.Count )
		{
		    if( s_buffers[fb] != null )
		        s_buffers[fb].Close();
		    s_buffers[fb] = null;
		}
	}
	
	public static bool saveFile(char[] filename, int buffer)
	{
		MemoryStream buf = getBuffer(buffer);
		if( buf != null )
		{
			try
			{
				string name = getPath() + Path.DirectorySeparatorChar + toString(filename);
				FileStream file = new FileStream(name, FileMode.Create, FileAccess.Write);
		        buf.WriteTo(file);
		        file.Close();
		        return true;
			}catch 
			{
				return false;
			}
		}else
			return false;
	}
	
	public static bool removeFile(char[] filename)
	{
		try
		{
			string name = getPath() + Path.DirectorySeparatorChar + toString(filename);
			File.Delete(name);
			return true;
		}catch 
		{
			return false;
		}
	}
	
	public static bool renameFile(char[] oldName, char[] newName)
	{
		try
		{
			string old = getPath() + Path.DirectorySeparatorChar + toString(oldName);
			string name = getPath() + Path.DirectorySeparatorChar + toString(newName);
			File.Move(old, name);
			return true;
		}catch
		{
			return false;
		}
	}
	
	public static int length(int buffer)
	{
		MemoryStream buf = getBuffer(buffer);
		if( buf != null )
			return (int)buf.Length;
		else
			return 0;
	}
	
	public static bool setPos(int buffer, int pos)
	{
		MemoryStream buf = getBuffer(buffer);
		if( buf != null )
		{
			try
			{	
				if( pos < 0 )
					pos = 0;
				return buf.Seek(pos, SeekOrigin.Begin) == pos;
			}catch
			{
				return false;
			}
		}else
			return false;
	}
	
	public static int getPos(int buffer)
	{
		MemoryStream buf = getBuffer(buffer);
		if( buf != null )
			return (int)buf.Position;
		else
			return 0;
	}
	
	public static bool atEnd(int buffer)
	{
		MemoryStream buf = getBuffer(buffer);
		if( buf != null )
			return buf.Position >= buf.Length;
		else
			return false;
	}
	
	public static bool writeByte(int buffer, int byte_)
	{
		MemoryStream buf = getBuffer(buffer);
		if( buf != null )
		{
			buf.WriteByte((byte)byte_);
			return true;
		}else
			return false;
	}
	
	public static int readByte(int buffer)
	{
		MemoryStream buf = getBuffer(buffer);
		if( buf != null )
			return buf.ReadByte();
		else
			return 0;
	}

	public static void begïn()
	{
	}

}
