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
using System.Runtime.InteropServices;

public class XYplane
{
	//CONST draw = 1; erase = 0;
	public static int X = 0, Y = 0, W = 1024, H = 768;
	private static IntPtr window = IntPtr.Zero, renderer = IntPtr.Zero, texture = IntPtr.Zero;
	private const string nativeLibName = "SDL2";
	private static uint[] pixel;
	private const uint BLACK = 0x000000;
    private const uint WHITE = 0xFFFFFF;
    private const int QueueLen = 100;
    private static char[] queue = new char[QueueLen];
    private static int head = 0, tail = 0, count = 0;

	
		[DllImport(nativeLibName, CallingConvention = CallingConvention.Cdecl)]
		private static extern IntPtr SDL_CreateWindow(string title,
			int x,
			int y,
			int w,
			int h,
			int flags
		);
		
		[DllImport(nativeLibName, CallingConvention = CallingConvention.Cdecl)]
		private static extern IntPtr SDL_CreateRenderer(
			IntPtr window,
			int index,
			int flags
		);

		[DllImport(nativeLibName, CallingConvention = CallingConvention.Cdecl)]
		private static extern IntPtr SDL_CreateTexture(
			IntPtr renderer,
			uint format,
			int access,
			int w,
			int h
		);

		[DllImport(nativeLibName, CallingConvention = CallingConvention.Cdecl)]
		private static extern void SDL_DestroyTexture(IntPtr texture);

		[DllImport(nativeLibName, CallingConvention = CallingConvention.Cdecl)]
		public static extern void SDL_DestroyRenderer(IntPtr renderer);

		[DllImport(nativeLibName, CallingConvention = CallingConvention.Cdecl)]
		public static extern void SDL_DestroyWindow(IntPtr window);

		[DllImport(nativeLibName, CallingConvention = CallingConvention.Cdecl)]
		private static extern string SDL_GetError();

		[DllImport(nativeLibName, CallingConvention = CallingConvention.Cdecl)]
		public static extern int SDL_UpdateTexture(
			IntPtr texture,
			IntPtr rect,
			IntPtr pixels,
			int pitch
		);

		[DllImport(nativeLibName, CallingConvention = CallingConvention.Cdecl)]
		public static extern int SDL_RenderCopy(
			IntPtr renderer,
			IntPtr texture,
			IntPtr srcrect,
			IntPtr dstrect
		);
		
		[DllImport(nativeLibName, CallingConvention = CallingConvention.Cdecl)]
		public static extern void SDL_RenderPresent(IntPtr renderer);

		[DllImport(nativeLibName, CallingConvention = CallingConvention.Cdecl)]
		public static extern void SDL_GetWindowPosition(
			IntPtr window,
			out int x,
			out int y
		);
		
		[DllImport(nativeLibName, CallingConvention = CallingConvention.Cdecl)]
		internal static extern int SDL_GetMouseState(out int x, out int y);
	
		public static uint SDL_BUTTON(uint X)
		{
			// If only there were a better way of doing this in C#
			return (uint) (1 << ((int) X - 1));
		}

		public const uint SDL_BUTTON_LEFT =	1;
		public const uint SDL_BUTTON_MIDDLE =	2;
		public const uint SDL_BUTTON_RIGHT =	3;

		[StructLayout(LayoutKind.Sequential)]
		public unsafe struct SDL_TextInputEvent
		{
			public UInt32 type;
			public UInt32 timestamp;
			public UInt32 windowID;
			public fixed byte text[32];
		}

		[StructLayout(LayoutKind.Explicit)]
		public unsafe struct SDL_Event
		{
			[FieldOffset(0)]
			public uint type;
			[FieldOffset(0)]
			public SDL_TextInputEvent text;
			[FieldOffset(0)]
			private fixed byte padding[56];
		}
		
		[DllImport(nativeLibName, CallingConvention = CallingConvention.Cdecl)]
		public static extern int SDL_PollEvent(out SDL_Event _event);

		[DllImport(nativeLibName, CallingConvention = CallingConvention.Cdecl)]
		public static extern int SDL_RenderDrawPoint(
			IntPtr renderer,
			int x,
			int y
		);

		[DllImport(nativeLibName, CallingConvention = CallingConvention.Cdecl)]
		public static extern int SDL_SetRenderDrawColor(
			IntPtr renderer,
			byte r,
			byte g,
			byte b,
			byte a
		);
		
	private static void dispose()
	{
		if( texture != IntPtr.Zero )
		{
			SDL_DestroyTexture(texture);
			texture = IntPtr.Zero;
		}
		if( renderer != IntPtr.Zero )
		{
			SDL_DestroyRenderer(renderer);
			renderer = IntPtr.Zero;
		}
		if( window != IntPtr.Zero )
		{
			SDL_DestroyWindow(window);
			window = IntPtr.Zero;
		}
	}
	
	private static void enqueue( char c )
	{
		if( count == QueueLen )
		{
			// reset queue if it is full since apparently nobody is interested in the events
		  	head = 0; tail = 0; count = 0;
		}
		count++;
		queue[head] = c;
		head = (head + 1) % QueueLen;
  	}
  	
  	public static int available() 
  	{ 
  		if( window == IntPtr.Zero )
			return 0;
		processEvents();
  		return count; 
  	}
  	
  	public static char dequeue()
  	{
		if( count == 0 )
		  return '\0';
		count--;
		char res = queue[tail];
		tail = (tail + 1) % QueueLen;
		return res;
  	}
  	
	private unsafe static void processEvents()
	{
		SDL_Event e;
		while( SDL_PollEvent(out e) == 1 )
		{
			if( e.type == 771 ) // TEXTINPUT
			{
				string str = Marshal.PtrToStringAnsi(new IntPtr(e.text.text));
				if( str.Length != 0 )
					enqueue(str[0]);
			}
		}		
      	SDL_GetWindowPosition(window, out X, out Y);
      	SDL_RenderPresent(renderer);	
	}
	
	private static void update()
	{
		GCHandle pinnedArray = GCHandle.Alloc(pixel, GCHandleType.Pinned);
		IntPtr pointer = pinnedArray.AddrOfPinnedObject();
		SDL_UpdateTexture(texture, IntPtr.Zero, pointer, W * 4);
		pinnedArray.Free();
		SDL_RenderCopy(renderer, texture, IntPtr.Zero, IntPtr.Zero);
      	SDL_RenderPresent(renderer);
      	
	}
	
	public static void GetMouseState( ref int keys, ref int x, ref int y )
	{
		if( window == IntPtr.Zero )
		{
			keys = 0; x = 0; y = 0;
			return;
		}
		processEvents();
		
		int b = SDL_GetMouseState(out x, out y);
		// left = 2, middle = 1, right = 0
		if( ( b & SDL_BUTTON(SDL_BUTTON_LEFT) ) != 0 )
			keys |= 4;
		if( ( b & SDL_BUTTON(SDL_BUTTON_MIDDLE) ) != 0 )
			keys |= 2;
		if( ( b & SDL_BUTTON(SDL_BUTTON_RIGHT) ) != 0 )
			keys |= 1;
		y = H - y - 1;
	}
		
	//PROCEDURE Open;
	public static void Open()
	{
		dispose();
		
		window = SDL_CreateWindow("Oberon XY PLane",
		                      0x1FFF0000, 0x1FFF0000, // pos undefined
		                      W, H, 4); // show
		if( window == IntPtr.Zero )
		{
		  Console.WriteLine("There was an issue creating the window: " + SDL_GetError());
		  return;
		}

		renderer = SDL_CreateRenderer(window, -1, 2 + 4 ); // accelerated, vsync
		if( renderer == IntPtr.Zero ) 
		{
		  Console.WriteLine("There was an issue creating the renderer: "+ SDL_GetError());
		  dispose();
		  return;
		}
		
		texture = SDL_CreateTexture(renderer, 372645892, // format ARGB8888
		                                   1, // streaming
		                                   W, H);
		if( texture == IntPtr.Zero )
		{
		  Console.WriteLine("There was an issue creating the texture: "+ SDL_GetError());
		  dispose();
		  return;
		}
		
		pixel = new uint[W*H];
		Clear();
	}

	//PROCEDURE Clear;
	public static void Clear()
	{
		if( window == IntPtr.Zero )
			return;
		for( int i = 0; i < pixel.Length; i++ )
			pixel[i] = WHITE;
		update();
	}

	//PROCEDURE Dot (x, y, mode: INTEGER);
	public static void Dot(int x, int y, int mode)
	{
		if( window == IntPtr.Zero )
			return;
		// mode draw = 1; erase = 0;
		y = H - y - 1;
		pixel[ y * W + x ] = mode == 0 ? WHITE : BLACK;
		
		if( mode != 0 )
			SDL_SetRenderDrawColor(renderer,0,0,0,255);
		else
			SDL_SetRenderDrawColor(renderer,255,255,255,255);
		SDL_RenderDrawPoint(renderer,x,y);
      	// SDL_RenderPresent(renderer); // this is a slow operation, call it in processEvents
		
		//update(); // this is too slow
		// so we don't actually need a texture; directly drawing to renderer is possible and faster.
	}

	//PROCEDURE IsDot (x, y: INTEGER): BOOLEAN;
	public static bool IsDot(int x, int y)
	{
		if( window == IntPtr.Zero )
			return false;
		y = H - y - 1;
		return pixel[ y * W + x ] == BLACK;
	}

	//PROCEDURE Key (): CHAR;
	public static char Key()
	{
		if( window != IntPtr.Zero )
			processEvents();
		return dequeue();
	}
}
