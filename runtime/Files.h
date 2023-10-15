#ifndef _OBX_FILES_
#define _OBX_FILES_
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

#include "OBX.Runtime.h"

struct Files$Handle$Class$ {
    void* super$;
};
struct Files$Handle$Class$ Files$Handle$class$;
struct Files$Handle
{
    struct Files$Handle$Class$* class$;
	struct OBX$Array$1 name;
	FILE* stream;
};

struct Files$Rider$Class${
    void* super$;
};
struct Files$Rider$Class$ Files$Rider$class$;
struct Files$Rider
{
    struct Files$Rider$Class$* class$;
	uint8_t eof;
	int32_t res;
	int32_t pos;
	struct Files$Handle* file;
};
extern void Files$Rider$init$(struct Files$Rider*);
extern void Files$Handle$init$(struct Files$Handle*);

// PROC Old (IN name: ARRAY OF CHAR): File;
extern struct Files$Handle* Files$Old(struct OBX$Array$1 filename);
// PROC New (IN name: ARRAY OF CHAR): File;
extern struct Files$Handle* Files$New(struct OBX$Array$1 name);
// PROC Register (f: File);
extern void Files$Register(struct Files$Handle* f);
// PROC Close (f: File);
extern void Files$Close(struct Files$Handle* f);
// PROC Purge (f: File);
extern void Files$Purge(struct Files$Handle* f);
// PROC Delete (IN name: ARRAY OF CHAR; VAR res: INTEGER);
extern void Files$Delete(struct OBX$Array$1 filename, int32_t* res);
// PROC Rename (IN old, new: ARRAY OF CHAR;VAR res: INTEGER);
extern void Files$Rename(struct OBX$Array$1 oldName, struct OBX$Array$1 newName, int32_t* res);
// PROC Length (f: File): INTEGER;
extern int32_t Files$Length(struct Files$Handle* f);
// PROC GetDate (f: File; VAR t, d: INTEGER);
extern void Files$GetDate(struct Files$Handle* f, int32_t* t, int32_t* d );
// PROC Set (VAR r: Rider; f: File; pos: INTEGER);
extern void Files$Set(struct Files$Rider* r, struct Files$Handle* f, int32_t pos );
// PROC Pos (VAR r: Rider): INTEGER;
extern int32_t Files$Pos(struct Files$Rider* r);
// PROC Base (VAR r: Rider): File;
extern struct Files$Handle* Files$Base(struct Files$Rider* r);
// PROC Read (VAR r: Rider; VAR x: BYTE);
extern void Files$Read(struct Files$Rider* r, uint8_t* x);
// PROC ReadInt (VAR R: Rider; VAR x: INTEGER);
extern void Files$ReadInt(struct Files$Rider* r, int32_t* x);
extern void Files$ReadLInt(struct Files$Rider* r, int64_t* x);
// PROC ReadReal (VAR R: Rider; VAR x: REAL);
extern void Files$ReadReal(struct Files$Rider* r, float* x);
extern void Files$ReadLReal(struct Files$Rider* r, double* x);
// PROC ReadNum (VAR R: Rider; VAR x: INTEGER);
extern void Files$ReadNum(struct Files$Rider* R, int32_t* x);
// PROC ReadString (VAR R: Rider; VAR x: ARRAY OF CHAR);
extern void Files$ReadString(struct Files$Rider* R, struct OBX$Array$1 x);
// PROC ReadSet (VAR R: Rider; VAR x: SET);
extern void Files$ReadSet(struct Files$Rider* R, uint32_t* x);
// PROC ReadBool (VAR R: Rider; VAR x: BOOLEAN );
extern void Files$ReadBool(struct Files$Rider* R, uint8_t* x);
// PROC ReadBytes (VAR r: Rider; VAR x: ARRAY OF BYTE; n: INTEGER);
extern void Files$ReadBytes(struct Files$Rider* r, struct OBX$Array$1 x, int32_t n );
// PROC Write (VAR r: Rider; x: BYTE);
extern void Files$Write(struct Files$Rider* r, uint8_t x);
// PROC WriteInt (VAR R: Rider; x: INTEGER);
extern void Files$WriteInt(struct Files$Rider* R, int32_t x);
extern void Files$WriteLInt(struct Files$Rider* R, int64_t x);
// PROC WriteReal (VAR R: Rider; x: REAL);
extern void Files$WriteReal(struct Files$Rider* r, float x);
extern void Files$WriteLReal(struct Files$Rider* r, double x);
// PROC WriteNum (VAR R: Rider; x: INTEGER);
extern void Files$WriteNum(struct Files$Rider* R, int x);
// PROC WriteString (VAR R: Rider; IN x: ARRAY OF CHAR);
extern void Files$WriteString(struct Files$Rider* R, struct OBX$Array$1 x);
// PROC WriteSet (VAR R: Rider; x: SET);
extern void Files$WriteSet(struct Files$Rider* R, uint32_t x);
// PROC WriteBool (VAR R: Rider; x: BOOLEAN);
extern void Files$WriteBool(struct Files$Rider* R, uint8_t x);
// PROC WriteBytes (VAR r: Rider; VAR x: ARRAY OF BYTE;n: INTEGER);
extern void Files$WriteBytes(struct Files$Rider* r, struct OBX$Array$1 x, int n);

extern void Files$init$();
extern OBX$Cmd Files$cmd$(const char*);


#endif
