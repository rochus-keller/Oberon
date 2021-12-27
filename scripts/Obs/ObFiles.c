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
#include "ObFiles.h"

// TODO complete

int32_t ObFiles$getTime()
{
}

int32_t ObFiles$listFiles()
{
}

struct OBX$Array$1 ObFiles$fileName(int32_t i)
{
}

int32_t ObFiles$openFile(struct OBX$Array$1 filename)
{
}

int32_t ObFiles$newFile()
{
}

void ObFiles$freeFile(int32_t buffer)
{
}

int ObFiles$saveFile(struct OBX$Array$1 filename, int32_t buffer)
{
}

int ObFiles$removeFile(struct OBX$Array$1 filename)
{
}

int ObFiles$renameFile(struct OBX$Array$1 oldName, struct OBX$Array$1 newName)
{
}

int32_t ObFiles$length(int32_t buffer)
{
}

int ObFiles$setPos(int32_t buffer, int32_t pos)
{
}

int32_t ObFiles$getPos(int32_t buffer)
{
}

int ObFiles$atEnd(int32_t buffer)
{
}

int ObFiles$writeByte(int32_t buffer, int32_t byte_)
{
}

int32_t ObFiles$readByte(int32_t buffer)
{
}


void ObFiles$init$()
{
}

OBX$Cmd ObFiles$cmd$(const char* name)
{
	if( name == 0 ) return ObFiles$init$;
	return 0;
}
