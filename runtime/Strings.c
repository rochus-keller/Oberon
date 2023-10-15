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

#include "Strings.h"
#include <ctype.h>

int32_t Strings$Length(struct OBX$Array$1 str)
{
	return strlen( (const char*)str.$a );
}

void Strings$Insert(struct OBX$Array$1 source, int32_t pos, struct OBX$Array$1 dest )
{
	const char* s = (const char*)source.$a;
	char* d = (char*)dest.$a;
	const int len = strlen(s);
	for(int i = pos; i < (len+pos); i++ )
	{
		d[i+len] = d[i];
		d[i] = s[i-pos];
	}
}

void Strings$Append(struct OBX$Array$1 extra, struct OBX$Array$1 dst)
{
	Strings$Insert(extra,Strings$Length(dst),dst);
}

void Strings$Delete(struct OBX$Array$1 s, int pos, int n)
{
	char* str = (char*)s.$a;
	const int len = strlen(str);
	for( int i = pos + n; i < len; i++ )
		str[i-n] = str[i];
	str[len-n] = 0;
}

void Strings$Replace(struct OBX$Array$1 src, int pos, struct OBX$Array$1 dst)
{
	Strings$Delete(dst, pos, Strings$Length(src));
	Strings$Insert(src, pos, dst);
}

void Strings$Extract(struct OBX$Array$1 src, int pos, int n, struct OBX$Array$1 dest)
{
	const char* s = (const char*)src.$a;
	char* d = (char*)dest.$a;
	strncpy(d,s+pos,n);
}

int32_t Strings$Pos(struct OBX$Array$1 pattern, struct OBX$Array$1 str, int pos )
{
	const char* p = (const char*)pattern.$a;
	const char* s = (const char*)str.$a;
	const char * res = strstr(s, p);
	if( res == 0 )
		return -1;
	else
		return res - s;
}

void Strings$Cap(struct OBX$Array$1 s )
{
	char* str = (char*)s.$a;
	const int len = strlen(str);
	for( int i = 0; i < len; i++ )
		str[i] = toupper(str[i]); // TODO: latin-1
}

void Strings$init$()
{
}

OBX$Cmd Strings$cmd$(const char* name)
{
	if(name==0) return Strings$init$;
	return 0;
}
