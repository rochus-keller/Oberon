#ifndef _OBX_OBFILES_
#define _OBX_OBFILES_

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


// proc getTime(): integer 
extern int32_t ObFiles$getTime();
// proc listFiles(): integer 
extern int32_t ObFiles$listFiles();
// proc fileName(i: integer):^[]char
extern struct OBX$Array$1 ObFiles$fileName(int32_t i);
// proc openFile(in filename: []char): integer
extern int32_t ObFiles$openFile(struct OBX$Array$1 filename);
// proc newFile(): integer
extern int32_t ObFiles$newFile();
// proc freeFile(buffer: integer)
extern void ObFiles$freeFile(int32_t buffer);
// proc saveFile(in filename: []char; buffer: integer): boolean
extern int ObFiles$saveFile(struct OBX$Array$1 filename, int32_t buffer);
// proc removeFile(in filename: []char): boolean
extern int ObFiles$removeFile(struct OBX$Array$1 filename);
// proc renameFile(in oldName, newName: []char): boolean
extern int ObFiles$renameFile(struct OBX$Array$1 oldName, struct OBX$Array$1 newName);
// proc length(buffer: integer): integer
extern int32_t ObFiles$length(int32_t buffer);
// proc setPos(buffer, pos: integer): boolean
extern int ObFiles$setPos(int32_t buffer, int32_t pos);
// proc getPos(buffer: integer): integer
extern int32_t ObFiles$getPos(int32_t buffer);
// proc atEnd(buffer: integer): boolean
extern int ObFiles$atEnd(int32_t buffer);
// proc writeByte(buffer, byte_: integer): boolean
extern int ObFiles$writeByte(int32_t buffer, int32_t byte_);
// proc readByte(buffer: integer): integer
extern int32_t ObFiles$readByte(int32_t buffer);

extern void ObFiles$init$();
extern OBX$Cmd ObFiles$cmd$(const char*);
  
#endif
