#ifndef _OBX_OUT_
#define _OBX_OUT_

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

#include "OBX.Runtime.h"

// PROCEDURE Length (IN s: ARRAY OF CHAR): INTEGER;
extern int32_t Strings$Length(struct OBX$Array$1 str);
// PROCEDURE Insert (source: ARRAY OF CHAR; pos: INTEGER; VAR dest: ARRAY OF CHAR);
extern void Strings$Insert(struct OBX$Array$1 source, int32_t pos, struct OBX$Array$1 dest );
// PROCEDURE Append (extra: ARRAY OF CHAR; VAR dest: ARRAY OF CHAR);
extern void Strings$Append(struct OBX$Array$1 extra, struct OBX$Array$1 dst);
// PROCEDURE Delete (VAR s: ARRAY OF CHAR; pos, n: INTEGER);
extern void Strings$Delete(struct OBX$Array$1 s, int pos, int n);
// PROCEDURE Replace (source: ARRAY OF CHAR; pos: INTEGER; VAR dest: ARRAY OF CHAR);
extern void Strings$Replace(struct OBX$Array$1 src, int pos, struct OBX$Array$1 dst);
// PROCEDURE Extract (source: ARRAY OF CHAR; pos, n: INTEGER; VAR dest: ARRAY OF CHAR);
extern void Strings$Extract(struct OBX$Array$1 src, int pos, int n, struct OBX$Array$1 dest);
// PROCEDURE Pos (pattern, s: ARRAY OF CHAR; pos: INTEGER): INTEGER;
extern int32_t Strings$Pos(struct OBX$Array$1 pattern, struct OBX$Array$1 s, int pos );
// PROCEDURE Cap (VAR s: ARRAY OF CHAR);
extern void Strings$Cap(struct OBX$Array$1 s );

extern void Strings$init$();
extern OBX$Cmd Strings$cmd$(const char*);

#endif
