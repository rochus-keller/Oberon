#ifndef _OBX_XYPLANE_
#define _OBX_XYPLANE_

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
