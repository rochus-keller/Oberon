#ifndef _OBX_IN_
#define _OBX_IN_

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

extern int In$Done;

//PROCEDURE Open;
extern void In$Open();

//PROCEDURE Char (VAR ch: CHAR);
extern void In$Char(char* ch);
//PROCEDURE Int (VAR i: INTEGER);
extern void In$Int(int32_t* i);
extern void In$LongInt(int64_t* i);
//PROCEDURE Real (VAR x: REAL);
extern void In$Real(float* x);
extern void In$LongReal(double* x);
//PROCEDURE String (VAR str: ARRAY OF CHAR);
extern void In$String(struct OBX$Array$1 str);
//PROCEDURE Name (VAR name: ARRAY OF CHAR);
extern void In$Name(struct OBX$Array$1 name);

extern void In$init$();
extern OBX$Cmd In$cmd$(const char*);

#endif
