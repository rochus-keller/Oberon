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

#include "In.h"

int In$Done = 0;

void In$Open()
{
}

void In$Char(char* ch)
{
	assert( ch != 0 );
	const int res = getchar();
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

void In$init$()
{
}

void In$cmd$(const char* name)
{
	if(name==0) return In$Init();
	return 0;
}
