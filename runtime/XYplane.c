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
*
* Alternatively this file may be used under the terms of the Mozilla 
* Public License. If a copy of the MPL was not distributed with this
* file, You can obtain one at https://mozilla.org/MPL/2.0/.
*/

#include "XYplane.h"

#ifdef _MSC_VER
#define inline
#endif

struct Window;
struct Renderer;
struct Texture;

struct Rect {
    int32_t x;
    int32_t y;
    int32_t w;
    int32_t h;
};

struct TextInputEvent
{
	uint32_t type;
	uint32_t timestamp;
	uint32_t windowID;
	char text[32];
};

union Event
{
	uint32_t type;
	struct TextInputEvent text;
	char padding[56];
};


#define BUTTON_LEFT 1
#define BUTTON_MIDDLE 2
#define BUTTON_RIGHT 3

static inline uint32_t BUTTON(uint32_t X)
{
	return (uint32_t) (1 << ((int32_t) X - 1));
}

static struct Window * (*CreateWindow)(char *, int32_t, int32_t, int32_t, int32_t, int32_t) = 0;
static struct Renderer * (*CreateRenderer)(struct Window *, int32_t, int32_t) = 0;
static struct Texture * (*CreateTexture)(struct Renderer *, int32_t, int32_t, int32_t, int32_t) = 0;
static void (*DestroyTexture)(struct Texture *) = 0;
static void (*DestroyRenderer)(struct Renderer *) = 0;
static void (*DestroyWindow)(struct Window *) = 0;
static char * (*GetError)() = 0;
static int32_t (*UpdateTexture)(struct Texture *, struct Rect *, void *, int32_t) = 0;
static int32_t (*RenderCopy)(struct Renderer *, struct Texture *, struct Rect *, struct Rect *);
static void (*RenderPresent)(struct Renderer *);
static void (*GetWindowPosition)(struct Window *, int32_t *, int32_t *);
static int32_t (*GetMouseState)(int32_t *, int32_t *);
static int32_t (*PollEvent)(union Event *);
static int32_t (*RenderDrawPoint)(struct Renderer *, int32_t, int32_t);
static int32_t (*SetRenderDrawColor)(struct Renderer *, uint8_t, uint8_t, uint8_t, uint8_t) = 0;

int32_t XYplane$X = 0, XYplane$Y = 0, XYplane$W = 1024, XYplane$H = 768;
static struct Window* window = 0;
static struct Renderer* renderer = 0;
static struct Texture* texture = 0;
static uint32_t* pixel = 0;

#define BLACK 0x000000
#define WHITE 0xFFFFFF
#define QueueLen 100
static char queue[QueueLen];
static int head = 0, tail = 0, count = 0;

static void processEvents();
static void enqueue( char c )
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

int XYplane$Available() 
{ 
	if( window == 0 )
		return 0;
	processEvents();
	return count; 
}

char XYplane$Dequeue()
{
	if( count == 0 )
	  return '\0';
	count--;
	char res = queue[tail];
	tail = (tail + 1) % QueueLen;
	return res;
}
  	
static void update()
{
	UpdateTexture(texture, 0, pixel, XYplane$W * 4);
	RenderCopy(renderer, texture, 0, 0);
  	RenderPresent(renderer);
}

static void processEvents()
{
	union Event e;
	while( PollEvent(&e) == 1 )
	{
		if( e.type == 771 ) // TEXTINPUT
		{
			uint32_t ch = OBX$UtfDecode((const uint8_t*)e.text.text,0);
			if( ch && ch <= 255 )
			 	enqueue(ch);
		}
	}		
  	GetWindowPosition(window, &XYplane$X, &XYplane$Y);
  	RenderPresent(renderer);	
}

static void dispose()
{
	if( texture != 0 )
	{
		DestroyTexture(texture);
		texture = 0;
	}
	if( renderer != 0 )
	{
		DestroyRenderer(renderer);
		renderer = 0;
	}
	if( window != 0 )
	{
		DestroyWindow(window);
		window = 0;
	}
	if( pixel != 0 )
	{
		free(pixel);
		pixel = 0;
	}
}

void XYplane$GetMouseState( int32_t* keys, int32_t* x, int32_t* y )
{
	if( window == 0 )
	{
		*keys = 0; *x = 0; *y = 0;
		return;
	}
	processEvents();
	
	int b = GetMouseState(x, y);
	// left = 2, middle = 1, right = 0
	if( ( b & BUTTON(BUTTON_LEFT) ) != 0 )
		*keys |= 4;
	if( ( b & BUTTON(BUTTON_MIDDLE) ) != 0 )
		*keys |= 2;
	if( ( b & BUTTON(BUTTON_RIGHT) ) != 0 )
		*keys |= 1;
	*y = XYplane$H - *y - 1;
}

void XYplane$Dot(int32_t x, int32_t y, int32_t mode)
{
	if( window == 0 )
		return;
	// mode draw = 1; erase = 0;
	y = XYplane$H - y - 1;
	pixel[ y * XYplane$W + x ] = mode == 0 ? WHITE : BLACK;
	
	if( mode != 0 )
		SetRenderDrawColor(renderer,0,0,0,255);
	else
		SetRenderDrawColor(renderer,255,255,255,255);
	RenderDrawPoint(renderer,x,y);
}

uint8_t XYplane$IsDot(int32_t x, int32_t y)
{
	if( window == 0 )
		return 0;
	y = XYplane$H - y - 1;
	return pixel[ y * XYplane$W + x ] == BLACK;
}

char XYplane$Key()
{
	if( window != 0 )
		processEvents();
	return XYplane$Dequeue();
}
	
void XYplane$Open()
{
	if( CreateWindow == 0 )
	{
		fprintf(stderr,"XYplane is not ready because SDL2 could not be loaded\n");
		return;
	}
	
	dispose();
	
	window = CreateWindow("Oberon XY PLane",
	                      0x1FFF0000, 0x1FFF0000, // pos undefined
	                      XYplane$W, XYplane$H, 4); // show
	if( window == 0 )
	{
		fprintf(stderr, "There was an issue creating the window: %s\n", GetError());
	  	return;
	}

	renderer = CreateRenderer(window, -1, 2 + 4 ); // accelerated, vsync
	if( renderer == 0 ) 
	{
	  fprintf(stderr, "There was an issue creating the renderer: %s\n", GetError());
	  dispose();
	  return;
	}
	
	texture = CreateTexture(renderer, 372645892, // format ARGB8888
	                                   1, // streaming
	                                   XYplane$W, XYplane$H);
	if( texture == 0 )
	{
	  fprintf(stderr, "There was an issue creating the texture: %s\n", GetError());
	  dispose();
	  return;
	}
	
	pixel = malloc(XYplane$W*XYplane$H*sizeof(uint32_t));
	XYplane$Clear();
}

void XYplane$Clear()
{
	if( window == 0 )
		return;
	for( int i = 0; i < XYplane$W*XYplane$H; i++ )
		pixel[i] = WHITE;
	update();
}

static int initDone = 0;
void XYplane$init$()
{
	if( initDone ) return;
	initDone = 1;
	
	void* $l = OBX$LoadDynLib("SDL2");
	if( $l == 0 )
		return;
		
    CreateWindow = (struct Window * (*)(char *, int32_t, int32_t, int32_t, int32_t, int32_t)) OBX$LoadProc($l,"SDL_CreateWindow");
    CreateRenderer = (struct Renderer * (*)(struct Window *, int32_t, int32_t)) OBX$LoadProc($l,"SDL_CreateRenderer");
    CreateTexture = (struct Texture * (*)(struct Renderer *, int32_t, int32_t, int32_t, int32_t)) OBX$LoadProc($l,"SDL_CreateTexture");
    DestroyTexture = (void (*)(struct Texture *)) OBX$LoadProc($l,"SDL_DestroyTexture");
    DestroyRenderer = (void (*)(struct Renderer *)) OBX$LoadProc($l,"SDL_DestroyRenderer");
    DestroyWindow = (void (*)(struct Window *)) OBX$LoadProc($l,"SDL_DestroyWindow");
    GetError = (char * (*)()) OBX$LoadProc($l,"SDL_GetError");
    UpdateTexture = (int32_t (*)(struct Texture *, struct Rect *, void *, int32_t)) OBX$LoadProc($l,"SDL_UpdateTexture");
    RenderCopy =  (int32_t (*)(struct Renderer *, struct Texture *, struct Rect *, struct Rect *)) OBX$LoadProc($l,"SDL_RenderCopy");
    RenderPresent = (void (*)(struct Renderer *)) OBX$LoadProc($l,"SDL_RenderPresent");
    GetWindowPosition = (void (*)(struct Window *, int32_t *, int32_t *)) OBX$LoadProc($l,"SDL_GetWindowPosition");
    GetMouseState = (int32_t (*)(int32_t *, int32_t *)) OBX$LoadProc($l,"SDL_GetMouseState");
    PollEvent = (int32_t (*)(union Event *)) OBX$LoadProc($l,"SDL_PollEvent");
    RenderDrawPoint = (int32_t (*)(struct Renderer *, int32_t, int32_t)) OBX$LoadProc($l,"SDL_RenderDrawPoint");
    SetRenderDrawColor = (int32_t (*)(struct Renderer *, uint8_t, uint8_t, uint8_t, uint8_t)) OBX$LoadProc($l,"SDL_SetRenderDrawColor");
}

OBX$Cmd XYplane$cmd$(const char* name)
{
	if(name==0) return XYplane$init$;
	return 0;
}
