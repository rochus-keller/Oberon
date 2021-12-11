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
