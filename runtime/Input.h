#ifndef _OBX_INPUT_
#define _OBX_INPUT_

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
