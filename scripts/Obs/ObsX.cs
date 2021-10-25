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
using SDL2CLI;
using System.Runtime.InteropServices;

public class ObsX
{
	static List<MemoryStream> s_buffers = new List<MemoryStream>();
	static readonly long s_start;
	static string[] s_files;
	static IntPtr s_window = IntPtr.Zero;
	static IntPtr s_renderer = IntPtr.Zero;
	static IntPtr s_texture = IntPtr.Zero;
	static int[] s_buffer = null;
	static uint[] s_pixelBuf = new uint[1000*1000];
	const int WIDTH = 1024;
	const int HEIGHT = 768;
	const uint BLACK = 0x000000; // 0x657b83;
	const uint WHITE = 0xFFFFFF; // 0xfdf6e3;
	static int s_x = 0, s_y = 0;
	static int s_lastUpdate = 0;
	static int s_sleepTime = 0;
	static bool s_left = false, s_mid = false, s_right = false, s_ctrl = false, s_shift = false;
	//static Queue<string> s_chars = new Queue<string>(); // causes runtime exception on Mono3
	static char[] queue = new char[100];
	static int head = 0, tail = 0, count = 0;
	
	private static void enqueue(char ch)
	{
		if( count == queue.Length )
		{
			Console.WriteLine("buffer overflow");
			return;
		}
		count++;
		queue[head] = ch;
		head = (head + 1) % queue.Length;
	}
	private static char dequeue()
	{
		if( count == 0 )
			return '\0';
		count--;
		char res = queue[tail]; 
		tail = (tail + 1) % queue.Length;
		return res;
	}

	static ObsX()
	{
		s_start = System.DateTime.Now.Ticks;
        if( SDL.SDL_Init(SDL.SDL_INIT_VIDEO) < 0 )
            Console.WriteLine("There was an issue initilizing SDL. {SDL.SDL_GetError()}");
	}
	
	private static void disposeWindow()
	{
		if( s_texture != IntPtr.Zero )
			SDL.SDL_DestroyTexture(s_texture);
		if( s_renderer != IntPtr.Zero )
			SDL.SDL_DestroyRenderer(s_renderer);
		if( s_window != IntPtr.Zero )
            SDL.SDL_DestroyWindow(s_window);
        s_window = IntPtr.Zero;
        s_renderer = IntPtr.Zero;
        s_texture = IntPtr.Zero;
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
	
	public static int[] createRasterBuffer(int len)
	{
		disposeWindow();
		s_buffer = null;
		
		s_window = SDL.SDL_CreateWindow("Oberon System on CLI and SDL",
				                  SDL.SDL_WINDOWPOS_UNDEFINED, 
				                  SDL.SDL_WINDOWPOS_UNDEFINED, 
				                  WIDTH,  
				                  HEIGHT, 
				                  SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN);

		if(s_window == IntPtr.Zero)
		{
			Console.WriteLine("There was an issue creating the window. {SDL.SDL_GetError()}");
			return null;
		}

		s_renderer = SDL.SDL_CreateRenderer(s_window, 
				                              -1, 
				                              SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED | 
				                              SDL.SDL_RendererFlags.SDL_RENDERER_PRESENTVSYNC);

		if(s_renderer == IntPtr.Zero)
		{
			Console.WriteLine("There was an issue creating the renderer. {SDL.SDL_GetError()}");
			disposeWindow();
			return null;
		}

		s_texture = SDL.SDL_CreateTexture(s_renderer,
				                               SDL.SDL_PIXELFORMAT_ARGB8888,
				                               (int)SDL.SDL_TextureAccess.SDL_TEXTUREACCESS_STREAMING,
				                               WIDTH,
				                               HEIGHT);
		if(s_texture == IntPtr.Zero)
		{
			Console.WriteLine("There was an issue creating the texture. {SDL.SDL_GetError()}");
			disposeWindow();
			return null;
		}

		s_buffer = new int[len];
		return s_buffer;
	}
	
	public static int getTime()
	{
		// A single tick represents one hundred nanoseconds or one ten-millionth of a second. There are 10,000 ticks in a millisecond.
		return (int)( ( System.DateTime.Now.Ticks - s_start ) / 10000 ); 
	}
  
  	private static void updateTexture()
  	{
    	SDL.SDL_Rect r;
    	r.x = 0;
		r.y = 0;
		r.w = WIDTH;
		r.h = HEIGHT;
		
		uint out_idx = 0;

		for( int line = HEIGHT-1; line >= 0; line-- ) 
		{
			int line_start = line * (WIDTH / 32);
        	// int line_start = (HEIGHT - line - 1) * (WIDTH / 32);
			for( int col = 0; col < WIDTH/32; col++) 
			{
				uint pixels = (uint)s_buffer[line_start + col];
				for( int b = 0; b < 32; b++ ) 
				{
					s_pixelBuf[out_idx] = (pixels & 1) != 0 ? WHITE : BLACK;
					pixels >>= 1;
					out_idx++;
				}
			}
		}

		unsafe
		{
		  fixed (uint* pArray = s_pixelBuf)
		  {
				IntPtr intPtr = new IntPtr((void *) pArray);
	   			SDL.SDL_UpdateTexture(s_texture, ref r, intPtr, r.w * 4);
		  }
		}
  	}
  	
  	private static string fromUtf8(byte[] str)
  	{
		int i = 0;
		while( i < str.Length && str[i] != 0 )
			i++;
  	    return System.Text.Encoding.UTF8.GetString(str,0,i);
	}
	
	public static bool processEvents(int sleep)
	{
		s_sleepTime = sleep;
        SDL.SDL_Event e;
        bool down = false;
        if( SDL.SDL_WaitEventTimeout(out e,sleep) == 1)
        {
            switch (e.type)
            {
            case SDL.SDL_EventType.SDL_QUIT:
                 return true;
            case SDL.SDL_EventType.SDL_MOUSEMOTION:
            	s_x = e.motion.x;
            	s_y = e.motion.y;
            	SDL.SDL_ShowCursor( ( s_x >= 0 && s_x < WIDTH ) || ( s_y >= 0 && s_y < HEIGHT ) ? SDL.SDL_DISABLE : SDL.SDL_ENABLE );
            	if( s_x < 0 )
            		s_x = 0;
            	if( s_x >= WIDTH )
            		s_x = WIDTH - 1;
            	if( s_y < 0 )
            		s_y = 0;
            	if( s_y >= HEIGHT )
            		s_y = HEIGHT - 1;
            	break;
		    case SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN: // SDL_MouseButtonEvent
		    case SDL.SDL_EventType.SDL_MOUSEBUTTONUP:
				down = e.button.state == SDL.SDL_PRESSED;
				switch( (uint)e.button.button )
				{
				case SDL.SDL_BUTTON_LEFT:
					if( s_ctrl && s_shift )
						s_right = down;
					else if( s_ctrl )
						s_mid = down;
					else
						s_left = down;
					break;
				case SDL.SDL_BUTTON_MIDDLE:
					s_mid = down;
					break;
				case SDL.SDL_BUTTON_RIGHT:
					if( s_ctrl )
						s_mid = down;
					else
						s_right = down;
					break;
				}
				if( !down )
					s_left = s_mid = s_right = false;
				break;
        	case SDL.SDL_EventType.SDL_TEXTINPUT: // SDL_TextInputEvent
        		byte[] arr = new byte[SDL.SDL_TEXTINPUTEVENT_TEXT_SIZE];
        		unsafe {
					Marshal.Copy((IntPtr)e.text.text, arr, 0, SDL.SDL_TEXTINPUTEVENT_TEXT_SIZE);
				}
        		string ch = fromUtf8(arr);
        		if( !String.IsNullOrEmpty(ch) )
        			enqueue(ch[0]); 
        		break;
		    case SDL.SDL_EventType.SDL_KEYDOWN:
		    case SDL.SDL_EventType.SDL_KEYUP: 
				down = e.key.state == SDL.SDL_PRESSED;
				switch( e.key.keysym.sym )
				{
				case SDL.SDL_Keycode.SDLK_LCTRL:
					s_ctrl = down;
					break;
				case SDL.SDL_Keycode.SDLK_LSHIFT:
					s_shift = down;
					break;
				case SDL.SDL_Keycode.SDLK_q:
					if( down && ( e.key.keysym.mod & SDL.SDL_Keymod.KMOD_CTRL ) != 0 )
						return true;
					break;
				case SDL.SDL_Keycode.SDLK_RETURN:
					if( down )
						enqueue('\r');
					break;
				case SDL.SDL_Keycode.SDLK_BACKSPACE:
					if( down )
						enqueue('\b');
					break;
				case SDL.SDL_Keycode.SDLK_TAB:
					if( down )
						enqueue('\t');
					break;
				case SDL.SDL_Keycode.SDLK_ESCAPE:
					if( down )
						enqueue('\u001B');
					break;
				}
				break;   
		    }       
        }

		int time = getTime();
		if( ( time - s_lastUpdate ) > 20 )
		{   
			s_lastUpdate = time;
		    updateTexture();
			SDL.SDL_RenderClear(s_renderer);
			
			SDL.SDL_Rect r;
			r.x = 0;
			r.y = 0;
			r.w = WIDTH;
			r.h = HEIGHT;

			SDL.SDL_RenderCopy(s_renderer, s_texture, ref r, ref r);
		    SDL.SDL_RenderPresent(s_renderer);
        }
    	return false;
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

	public class InputState 
	{ 
		public int keys; 
		public int x; 
		public int y; 
	}
	
	public static void getState(InputState state)
	{
		processEvents(s_sleepTime);
		state.x = s_x;
		state.y = HEIGHT - s_y - 1;
		state.keys = 0;
		if( s_left )
			state.keys |= 4;
		if( s_mid )
			state.keys |= 2;
		if( s_right )
			state.keys |= 1;
	}
	
	public static char nextKey()
	{
		return dequeue();
	}
}
