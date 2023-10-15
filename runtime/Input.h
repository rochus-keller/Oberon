#ifndef _OBX_INPUT_
#define _OBX_INPUT_

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

// TODO complete

//PROCEDURE Available (): INTEGER;
extern int Input$Available();

//PROCEDURE Read (VAR ch: CHAR);
extern void Input$Read(char* ch);

//PROCEDURE Mouse (VAR keys: SET; VAR x, y: INTEGER);
extern void Input$Mouse( int32_t* keys, int32_t* x, int32_t* y);
	
//PROCEDURE SetMouseLimits (w, h: INTEGER);
extern void Input$SetMouseLimits(int32_t w, int32_t h);
	
extern int32_t Input$Time();

extern void Input$init$();
extern OBX$Cmd Input$cmd$(const char*);

#endif
