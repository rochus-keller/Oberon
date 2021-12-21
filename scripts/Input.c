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

#include "Input.h"
#include <time.h>
#include <sys/time.h>

static struct timeval start;

int32_t Input$Time()
{
#ifdef _USE_CPU_TIME_
	clock_t now = clock();
	if( now < 0 )
		return 0;
	return now * 1000000 / CLOCKS_PER_SEC;
#else
    static struct timeval now;
    gettimeofday(&now, 0);
    const long seconds = now.tv_sec - start.tv_sec;
    const long microseconds = now.tv_usec - start.tv_usec;
    return seconds*1000000 + microseconds;
#endif
}

void Input$init$()
{
    gettimeofday(&start, 0);
}

void Input$cmd$(const char* name)
{
	if( name == 0 ) return Input$init$;
	return 0;
}
