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

#include "In.h"
#include <ctype.h>
#include <errno.h>

int In$Done = 0;

void In$Open()
{
}

void In$Char(char* ch)
{
	assert( ch != 0 );
	const int res = getc(stdin);
	if( res != EOF )
	{
		In$Done = 1;
		*ch = (char)res;
	}else
	{
		In$Done = 0;
		*ch = '\0';
	}
}

static int blockingRead(char* buf, size_t size, int stringEnd)
{
	int ch;
	int expectEnd = 0;
	int nonWsFound = 0;
	int i = 0;
	while( i < size-1 )
	{
		ch = getc(stdin);
		if( ch == EOF )
			continue;
		int isWs = isspace(ch);
		if( !stringEnd && nonWsFound && isWs )
			break;
		else if( iscntrl(ch) )
			continue; 
		if( !isWs || nonWsFound )
			buf[i++] = ch; // ignore trailing WS
		if( expectEnd && ch == '"' )
			break;
		expectEnd = stringEnd;
		if( !isWs )
			nonWsFound = 1;
	}
	buf[i] = 0;
	return i;
}

void In$Int(int32_t* i)
{
	// IntConst = digit {digit} | digit {hexDigit} “H”
	char buf[100];
	const int count = blockingRead(buf,100,0);
	In$Done = 0;
	*i = 0;
	if( count == 0 )
		return;
	errno = 0;
	if( buf[count-1] == 'H' )
		*i = strtol(buf, 0,16);
	else
		*i = strtol(buf, 0,10);
	In$Done = errno == 0;
}

void In$LongInt(int64_t* i)
{
	// IntConst = digit {digit} | digit {hexDigit} “H”
	char buf[100];
	const int count = blockingRead(buf,100,0);
	In$Done = 0;
	*i = 0;
	if( count == 0 )
		return;
	errno = 0;
	if( buf[count-1] == 'H' )
		*i = strtoll(buf, 0,16);
	else
		*i = strtoll(buf, 0,10);
	In$Done = errno == 0;
}

void In$Real(float* x)
{
	// RealConst = digit {digit} [ "." {digit} [“E” (“+” | “-”) digit {digit}]]
	char buf[100];
	const int count = blockingRead(buf,100,0);
	In$Done = 0;
	*x = 0.0f;
	if( count == 0 )
		return;
	errno = 0;
	*x = strtof(buf, 0);
	In$Done = errno == 0;
}

void In$LongReal(double* x)
{
	// RealConst = digit {digit} [ "." {digit} [“E” (“+” | “-”) digit {digit}]]
	char buf[100];
	const int count = blockingRead(buf,100,0);
	In$Done = 0;
	*x = 0.0f;
	if( count == 0 )
		return;
	errno = 0;
	*x = strtod(buf, 0);
	In$Done = errno == 0;
}

void In$String(struct OBX$Array$1 arr)
{
	// StringConst = ‘”’ char {char} ‘”’
	In$Done = 0;
	char* str = (char*)arr.$a;
	str[0] = 0;
	int count = blockingRead(str,arr.$1,1);
	if( count < 2 || str[0] != '"' || str[count-1] != '"' )
		return;
	for(int i = 1; i < count; i++ )
		str[i-1] = str[i];
	count -= 2;
	str[count] = 0;
	In$Done = 1;
	for( int i = 0; i < count; i++ )
	{
		if( str[i] > 0 && str[i] < ' ' ) // The string must not contain characters less than blank such as EOL or TAB.
		{
			In$Done = 0;
			return;
		}
	}
}

void In$Name(struct OBX$Array$1 arr)
{
	char* str = (char*)arr.$a;
	str[0] = 0;
	int count = blockingRead(str,arr.$1,0);
	In$Done = count > 0;
}

void In$init$()
{
}

OBX$Cmd In$cmd$(const char* name)
{
	if(name==0) return In$init$;
	return 0;
}
