#ifndef _OBX_XYPLANE_
#define _OBX_XYPLANE_

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

extern int32_t XYplane$X, XYplane$Y, XYplane$W, XYplane$H;

//PROCEDURE Open;
extern void XYplane$Open();
//PROCEDURE Clear;
extern void XYplane$Clear();
//PROCEDURE Dot (x, y, mode: INTEGER);
extern void XYplane$Dot(int32_t x, int32_t y, int32_t mode);
//PROCEDURE IsDot (x, y: INTEGER): BOOLEAN;
extern uint8_t XYplane$IsDot(int32_t x, int32_t y);
//PROCEDURE Key (): CHAR;
extern char XYplane$Key();

extern void XYplane$GetMouseState( int32_t* keys, int32_t* x, int32_t* y );
extern int XYplane$Available();
extern char XYplane$Dequeue();

extern void XYplane$init$();
extern OBX$Cmd XYplane$cmd$(const char*);

#endif
