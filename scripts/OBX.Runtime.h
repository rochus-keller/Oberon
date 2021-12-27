#ifndef _OBX_RUNTIME_H_
#define _OBX_RUNTIME_H_

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

#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <wchar.h>
#include <assert.h>
#include <stdio.h>
#include <math.h>
#include <inttypes.h>
#include <stdlib.h>

struct OBX$Array$1 { uint32_t $1: 31; uint32_t $s: 1; void* $a; }; // $s..static, 1 if literal or pointer to stack, 0 if allocated with OBX$Alloc
struct OBX$Array$2 { uint32_t $1; uint32_t $2: 31; uint32_t $s: 1; void* $a; };
struct OBX$Array$3 { uint32_t $1,$2; uint32_t $3: 31; uint32_t $s: 1; void* $a; };
struct OBX$Array$4 { uint32_t $1,$2,$3; uint32_t $4: 31; uint32_t $s: 1; void* $a; };
struct OBX$Array$5 { uint32_t $1,$2,$3,$4; uint32_t $5: 31; uint32_t $s: 1; void* $a; };

struct OBX$Class {
    struct OBX$Class* super$;
};

struct OBX$Inst {
	void* class$;
};

typedef void (*OBX$NullMeth)(void*);

struct OBX$Deleg {
	void* inst;
	OBX$NullMeth func;
};

typedef void (*OBX$Cmd)(void);
typedef OBX$Cmd (*OBX$Lookup)(const char* name );

extern void* OBX$ClassOf(void* inst);
int OBX$IsSubclass( void* superClass, void* subClass );
uint32_t OBX$SetDiv( uint32_t lhs, uint32_t rhs );
int32_t OBX$Div32( int32_t a, int32_t b );
int64_t OBX$Div64( int64_t a, int64_t b );
int32_t OBX$Mod32( int32_t a, int32_t b );
int64_t OBX$Mod64( int64_t a, int64_t b );
extern void* OBX$Alloc( size_t );
extern int OBX$StrOp( const struct OBX$Array$1* lhs, int lwide, const struct OBX$Array$1* rhs, int rwide, int op );
extern struct OBX$Array$1 OBX$StrJoin( const struct OBX$Array$1* lhs, int lwide, const struct OBX$Array$1* rhs, int rwide );
extern struct OBX$Array$1 OBX$CharToStr( int lwide, wchar_t ch );
extern void OBX$StrCopy(struct OBX$Array$1* lhs, int lwide, const struct OBX$Array$1* rhs, int rwide );
extern void OBX$ArrCopy(void* lhs, const void* rhs, int dims, int size ); // lhs and rhs are pointer to OBX$Array$*
extern void* OBX$Copy(void* data, int len);
extern uint32_t OBX$UtfDecode(const uint8_t* in, int* len );
extern void* OBX$FromUtf(const char* in, int len, int wide ); // len is decoded len incl. terminating zero
extern void* OBX$FromUtf2(int len, int wide, int count, ...); // count of const char* str
extern void OBX$PrintA(int ln, const char*);

//int OBX$MaxI32(int32_t lhs, int32_t rhs ) { return lhs > rhs ? lhs : rhs; }
//int OBX$MaxI64(int64_t lhs, int64_t rhs ) { return lhs > rhs ? lhs : rhs; }
//int OBX$MaxF32(float lhs, float rhs ) { return lhs > rhs ? lhs : rhs; }
//int OBX$MaxF64(double lhs, double rhs ) { return lhs > rhs ? lhs : rhs; }

extern void OBX$Pack32(float* lhs, int rhs);
extern void OBX$Unpack32(float* lhs, int* rhs);

extern uint32_t OBX$MakeSet(int count, ... );
extern int64_t OBX$Asr64(int64_t x, int n);
extern int32_t OBX$Asr32(int32_t x, int n);
extern int16_t OBX$Asr16(int16_t x, int n);

extern OBX$Lookup OBX$LoadModule(const char* module); // load OBX module dynamically or statically
extern void OBX$RegisterModule(const char* module, OBX$Lookup);
extern OBX$Cmd OBX$LoadCmd(const char* module, const char* command);
extern void* OBX$LoadDynLib(const char* path); // load any shared library
extern OBX$Cmd OBX$LoadProc(void* lib, const char* name); // load any procedure of given shared library
extern void OBX$InitApp(int argc, char **argv);

#endif
