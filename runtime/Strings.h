#ifndef _OBX_STRINGS_
#define _OBX_STRINGS_

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
