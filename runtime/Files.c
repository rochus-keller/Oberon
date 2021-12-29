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

#include "Files.h"
#if defined(_WIN32) && !defined(__GNUC__)
  #include <direct.h>
#else
  #include <sys/stat.h>
#endif
#include <time.h>

struct Files$Handle$Class$ Files$Handle$class$ = { 
    0,
};
struct Files$Rider$Class$ Files$Rider$class$ = { 
    0,
};

void Files$Handle$init$(struct Files$Handle* r)
{
	r->class$ = &Files$Handle$class$;
}

void Files$Rider$init$(struct Files$Rider* r)
{
	r->class$ = &Files$Rider$class$;
}

struct Files$Handle* Files$Old(struct OBX$Array$1 filename)
{
	if( filename.$a == 0 || *(const char*)filename.$a == 0 )
		return 0;

	FILE* old = fopen((const char*)filename.$a, "r"); 
	if( old == 0 )
	{
		fprintf( stderr, "cannot open file for reading: %s\n", (const char*)filename.$a );
		return 0;
	}

	FILE* tmp = tmpfile();
	if( tmp == 0 )
	{
		fprintf( stderr, "cannot create temporary file for %s\n", (const char*)filename.$a );
		fclose(old);
		exit(-1);
	}
	
	int ch;
	while ( (ch = fgetc(old)) != EOF )
	{
   		fputc(ch, tmp);
	}
	fclose(old);
	
	struct Files$Handle* f = OBX$Alloc(sizeof(struct Files$Handle));
	f->class$ = &Files$Handle$class$;
	f->name = filename;
	if( f->name.$s )
	{
		const int len = strlen((const char*)filename.$a)+1;
		f->name = (struct OBX$Array$1){ len, 0, OBX$Alloc(len) };
		strcpy( f->name.$a, filename.$a );
	}
	f->stream = tmp;
	fseek(tmp, 0, SEEK_SET );
    return f;
}

struct Files$Handle* Files$New(struct OBX$Array$1 name)
{
	if( name.$a == 0 )
	{
		name = (struct OBX$Array$1){ 1, 0, OBX$Alloc(1) };
		*(char*)name.$a = 0;
	}
	
	FILE* tmp = tmpfile();
	if( tmp == 0 )
	{
		fprintf( stderr, "cannot create temporary file for %s\n", (const char*)name.$a );
		exit(-1);
	}
	
	struct Files$Handle* f = OBX$Alloc(sizeof(struct Files$Handle));
	f->class$ = &Files$Handle$class$;
	f->name = name;
	if( f->name.$s )
	{
		const int len = strlen((const char*)name.$a)+1;
		f->name = (struct OBX$Array$1){ len, 0, OBX$Alloc(len) };
		strcpy( f->name.$a, name.$a );
	}
	f->stream = tmp;
	fseek(tmp, 0, SEEK_SET );
    return f;
}

void Files$Register(struct Files$Handle* f)
{
	if( f->stream != 0 )
	{
		FILE* file = fopen((const char*)f->name.$a, "w"); 
		if( file == 0 )
		{
			fprintf( stderr, "cannot open file for writing: %s\n", (const char*)f->name.$a );
			return;
		}
		fseek(f->stream, 0, SEEK_SET );
		int ch;
		while ( (ch = fgetc(f->stream)) != EOF )
		{
	   		fputc(ch, file);
		}
		fclose(file);
	}
}

void Files$Close(struct Files$Handle* f)
{
	if( f->stream != 0 )
	{
		fclose(f->stream);
		f->stream = 0;
	}
}

void Files$Purge(struct Files$Handle* f)
{
	if( f->stream != 0 )
	{
		fclose(f->stream);
		f->stream = tmpfile();
		if( f->stream == 0 )
			fprintf( stderr, "cannot create temporary file for %s\n", (const char*)f->name.$a );
	}
}

void Files$Delete(struct OBX$Array$1 filename, int32_t* res)
{
	*res = remove((const char*)filename.$a);
	if( *res < 0 )
		*res = 1;
}

void Files$Rename(struct OBX$Array$1 oldName, struct OBX$Array$1 newName, int32_t* res)
{
	*res = rename((const char*)oldName.$a, (const char*)newName.$a);
	if( *res < 0 )
		*res = 1;
}

int32_t Files$Length(struct Files$Handle* f)
{
	if( f->stream != 0 )
	{
		fseek(f->stream, 0L, SEEK_END);
		return ftell(f->stream);
	}else
		return 0;
}

void Files$GetDate(struct Files$Handle* f, int32_t* t, int32_t* d )
{
	// The encoding is: hour = t DIV 4096; minute = t DIV 64 MOD 64; second = t MOD 64; 
	// year = d DIV 512; month = d DIV 32 MOD 16; day = d MOD 32.
	struct stat s;
    stat((const char*)f->name.$a,&s);
    struct tm* mt = localtime(&s.st_mtime);
	*t = mt->tm_hour * 4096 + mt->tm_min * 64 + mt->tm_sec;
	*d = mt->tm_year * 512 + mt->tm_mon * 32 + mt->tm_mday;
}

void Files$Set(struct Files$Rider* r, struct Files$Handle* f, int32_t pos )
{
	r->file = f;
	r->pos = pos;
	r->eof = 0;
	r->res = 0;
}

int32_t Files$Pos(struct Files$Rider* r)
{
	return r->pos;
}

struct Files$Handle* Files$Base(struct Files$Rider* r)
{
	return r->file;
}

void Files$Read(struct Files$Rider* r, uint8_t* x)
{
	if( r->file != 0 && r->file->stream != 0 )
	{	
		if( r->pos < 0 )
			r->pos = 0;
		const int len = Files$Length(r->file);
		if( fseek(r->file->stream, r->pos, SEEK_SET ) == 0 )
		{
			r->res = 0;
			r->eof = ftell(r->file->stream) >= len;
			if( r->eof )
				r->res = 1;
			else
			{
				*x = (uint8_t)fgetc(r->file->stream);
				r->pos++;
			}
		}else
		{
			r->res = 1;
			*x = 0;
		}
	}else
	{
		r->res = 1;
		*x = 0;
	}
}

void Files$Write(struct Files$Rider* r, uint8_t x)
{
	if( r->file != 0 && r->file->stream != 0 )
	{	
		if( r->pos < 0 )
			r->pos = 0;
		if( fseek(r->file->stream,r->pos, SEEK_SET) == 0 )
		{
			r->res = 0;
			r->eof = 0;
			putc(x, r->file->stream);
			r->pos++;
		}else
			r->res++;
	}else
		r->res++;
}

void Files$ReadInt(struct Files$Rider* r, int32_t* x)
{
	uint8_t x0 = 0, x1 = 0, x2 = 0, x3 = 0;
	Files$Read(r, &x0); Files$Read(r, &x1); Files$Read(r, &x2); Files$Read(r, &x3);
	*x = ((x3 * 0x100 + x2) * 0x100 + x1) * 0x100 + x0;
}

void Files$ReadReal(struct Files$Rider* r, float* x)
{
	union { uint8_t b[4]; float f; } u;
	Files$Read(r,&u.b[0]);
	Files$Read(r,&u.b[1]);
	Files$Read(r,&u.b[2]);
	Files$Read(r,&u.b[3]);
	*x = u.f;
}

void Files$ReadNum(struct Files$Rider* R, int32_t* x)
{
    uint32_t n, y; uint8_t b = 0;
	n = 32; y = 0; Files$Read(R, &b);
	while( b >= 0x80 ) { y = (y + b-0x80) >> 7; n -= 7; Files$Read(R, &b); }
	if( n <= 4 ) *x = (int)((y + b % 0x10) >> 4); else *x = (int)((y + b) >> 7) >> (int)(n-7);
}

void Files$ReadString(struct Files$Rider* R, struct OBX$Array$1 x)
{
	int i; uint8_t ch = 0;
	i = 0; Files$Read(R, &ch);
	char* str = (char*)x.$a;
	while( ch != 0 )
	{
	  if( i < x.$1-1 ) 
	  {
	  	str[i] = (char)ch; 
	  	i++;
	  }
	  Files$Read(R, &ch);
	}
	str[i] = '\0';
}

void Files$ReadSet(struct Files$Rider* R, uint32_t* x)
{
	int32_t i = 0;
	Files$ReadInt(R,&i);
	*x = (uint32_t)i;
}

void Files$ReadBool(struct Files$Rider* R, uint8_t* x)
{
	*x = 0;
	Files$Read(R,x);
}

void Files$ReadBytes(struct Files$Rider* r, struct OBX$Array$1 x, int32_t n )
{
	int i = 0;
	uint8_t* b = (uint8_t*)x.$a;
	while( i < n ) { Files$Read(r, &b[i]); i++; }
}

void Files$WriteInt(struct Files$Rider* R, int x)
{
	Files$Write(R, (uint8_t)(x % 0x100));
	Files$Write(R, (uint8_t)(x / 0x100 % 0x100));
	Files$Write(R, (uint8_t)(x / 0x10000 % 0x100));
	Files$Write(R, (uint8_t)(x / 0x1000000 % 0x100));
}

void Files$WriteReal(struct Files$Rider* r, float x)
{
	union { uint8_t b[4]; float f; } u;
	u.f = x;
	Files$Write(r,u.b[0]);
	Files$Write(r,u.b[1]);
	Files$Write(r,u.b[2]);
	Files$Write(r,u.b[3]);
}

void Files$WriteNum(struct Files$Rider* R, int x)
{
	while( x < -0x40 || x >= 0x40 ) { Files$Write(R, (uint8_t)(x % 0x80 + 0x80)); x = x >> 7; }
	Files$Write(R, (uint8_t)( x % 0x80));
}

void Files$WriteString(struct Files$Rider* R, struct OBX$Array$1 x)
{
	int i = 0; char ch;
	const char* str = (const char*)x.$a;
	do
	{ ch = str[i]; Files$Write(R, (uint8_t)ch); i++;
	}while( ch != '\0' );
}

void Files$WriteSet(struct Files$Rider* R, uint32_t x)
{
	Files$WriteInt(R,(int32_t)x);
}

void Files$WriteBool(struct Files$Rider* R, uint8_t x)
{
	Files$Write(R, x ? 1:0 );
}

void Files$WriteBytes(struct Files$Rider* r, struct OBX$Array$1 x, int n)
{
	int i = 0; 
	const uint8_t* b = (const uint8_t*)x.$a;
	while( i < n ) { Files$Write(r, b[i]); i++; }
}

void Files$init$()
{
}

OBX$Cmd Files$cmd$(const char* name)
{
	if( name == 0 ) return Files$init$;
	return 0;
}

