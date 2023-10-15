#ifndef _OBX_RUNTIME_H_
#define _OBX_RUNTIME_H_

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

#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <wchar.h>
#include <assert.h>
#include <stdio.h>
#include <math.h>
#include <inttypes.h>
#include <stdlib.h>
#include <setjmp.h>
#include <ctype.h>

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

struct OBX$Anyrec$Class$ {
    struct OBX$Anyrec$Class$* super$;
};
extern struct OBX$Anyrec$Class$ OBX$Anyrec$class$;
struct OBX$Anyrec {
    struct OBX$Anyrec$Class$* class$;
};
extern struct OBX$Anyrec OBX$defaultException;

typedef void (*OBX$NullMeth)(void*);

struct OBX$Deleg {
	void* inst;
	OBX$NullMeth func;
};

typedef void (*OBX$Cmd)(void);
typedef OBX$Cmd (*OBX$Lookup)(const char* name );

struct OBX$Jump
{
	void* inst;
	jmp_buf buf;
	struct OBX$Jump* prev;
};

extern struct OBX$Jump* OBX$PushJump();
extern struct OBX$Jump* OBX$TopJump();
extern void OBX$PopJump();

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
extern void OBX$Halt(int code, const char* file, int line);

//int OBX$MaxI32(int32_t lhs, int32_t rhs ) { return lhs > rhs ? lhs : rhs; }
//int OBX$MaxI64(int64_t lhs, int64_t rhs ) { return lhs > rhs ? lhs : rhs; }
//int OBX$MaxF32(float lhs, float rhs ) { return lhs > rhs ? lhs : rhs; }
//int OBX$MaxF64(double lhs, double rhs ) { return lhs > rhs ? lhs : rhs; }

extern void OBX$Pack32(float* lhs, int rhs);
extern void OBX$Unpack32(float* lhs, int* rhs);

extern uint32_t OBX$MakeSet(int count, ... );
extern int64_t OBX$Asr64(int64_t x, int n);
extern int32_t OBX$Asr32(int32_t x, int n);
extern int64_t OBX$Ash64(int64_t x, int n);
extern int32_t OBX$Ash32(int32_t x, int n);
extern uint64_t OBX$Lsl64(uint64_t x, int n);
extern uint32_t OBX$Lsl32(uint32_t x, int n);
extern uint64_t OBX$Ror64(uint64_t x, int n);
extern uint32_t OBX$Ror32(uint32_t x, int n);

extern OBX$Lookup OBX$LoadModule(const char* module); // load OBX module dynamically or statically
extern void OBX$RegisterModule(const char* module, OBX$Lookup);
extern OBX$Cmd OBX$LoadCmd(const char* module, const char* command);
extern void* OBX$LoadDynLib(const char* path); // load any shared library
extern OBX$Cmd OBX$LoadProc(void* lib, const char* name); // load any procedure of given shared library
extern void OBX$InitApp(int argc, char **argv);
extern const char* OBX$AppPath();

#endif
