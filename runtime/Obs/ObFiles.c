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
#include <time.h>

#if defined(_WIN32) && !defined(__GNUC__)
#define WIN32_LEAN_AND_MEAN
#include <Windows.h>

// Source: https://stackoverflow.com/questions/10905892/equivalent-of-gettimeday-for-windows/26085827

// MSVC defines this in winsock2.h!?
typedef struct timeval {
    long tv_sec;
    long tv_usec;
} timeval;

int gettimeofday(struct timeval * tp, struct timezone * tzp)
{
    // Note: some broken versions only have 8 trailing zero's, the correct epoch has 9 trailing zero's
    // This magic number is the number of 100 nanosecond intervals since January 1, 1601 (UTC)
    // until 00:00:00 January 1, 1970 
    static const uint64_t EPOCH = ((uint64_t) 116444736000000000ULL);

    SYSTEMTIME  system_time;
    FILETIME    file_time;
    uint64_t    time;

    GetSystemTime( &system_time );
    SystemTimeToFileTime( &system_time, &file_time );
    time =  ((uint64_t)file_time.dwLowDateTime )      ;
    time += ((uint64_t)file_time.dwHighDateTime) << 32;

    tp->tv_sec  = (long) ((time - EPOCH) / 10000000L);
    tp->tv_usec = (long) (system_time.wMilliseconds * 1000);
    return 0;
}
#else
#include <sys/time.h>
#endif

static struct timeval start;
// TODO complete

int32_t ObFiles$getTime()
{
    static struct timeval now;
    gettimeofday(&now, 0);
    const long seconds = now.tv_sec - start.tv_sec;
    const long microseconds = now.tv_usec - start.tv_usec;
    return seconds*1000000 + microseconds;
}

#if defined(_WIN32)
#define PATH_SEP '\\'
#else
#define PATH_SEP '/'
#endif

static void getPath( char* path, size_t size )
{
    const char* env = getenv("OBERON_FILE_SYSTEM_ROOT");
    if( env )
    {
        strncpy( path, env, size );
        return;
    }
    const char* ap = OBX$AppPath();
    const int len = strlen(ap);
    if( len + 1 + 5 + 1 > size )
    {
        *path = 0;
        return;
    }
    strcpy(path,ap);
    path[len] = PATH_SEP;
    strcpy(path+len+1,"Files");
}

static void getFilePath( char* filePath, size_t size, const char* fileName )
{
    getPath(filePath, size);
    const int len1 = strlen(filePath);
    const int len2 = strlen(fileName);
    if( len1 == 0 )
        return;
    if( len1 + 1 + len2 + 1 > size )
    {
        *filePath = 0;
        return;
    }
    filePath[len1] = PATH_SEP;
    strcpy(filePath+len1+1,fileName);
}

enum { MAX_FILES = 1000 };
static char* s_files[MAX_FILES];
static int s_count = 0;

static void clearFiles()
{
    for( int i = 0; i < s_count; i++ )
    {
        free(s_files[i]);
    }
    s_count = 0;
}

#if defined(_WIN32) && !defined(__GNUC__)
// source: https://stackoverflow.com/questions/2314542/listing-directory-contents-using-c-and-windows
static void readDir()
{
    clearFiles();

    WIN32_FIND_DATA fdFile;
    HANDLE hFind = NULL;

    char sPath[2048];
    enum { LEN = 2048 };
    char sPath[LEN];
    getPath(sPath,LEN);
    if( *sPath == 0 )
        return;

    if((hFind = FindFirstFile(sPath, &fdFile)) == INVALID_HANDLE_VALUE)
    {
        fprintf(stderr,"path not found: %s\n", sPath);
        return;
    }

    do
    {
        //Find first file will always return "."
        //    and ".." as the first two directories.
        if(strcmp(fdFile.cFileName, ".") != 0
                && strcmp(fdFile.cFileName, "..") != 0
                && !(fdFile.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) )
        {
            const int len = strlen(fdFile.cFileName);
            s_files[s_count] = malloc(len+1);
            strcpy(s_files[s_count],fdFile.cFileName);
            s_count++;
        }
    }while(FindNextFile(hFind, &fdFile)); //Find the next file.

    FindClose(hFind); //Always, Always, clean things up!
}
#else
#include <dirent.h>

static void readDir()
{
    clearFiles();

    enum { LEN = 300 };
    char path[LEN];
    getPath(path,LEN);
    if( *path == 0 )
        return;
    DIR *d;
    struct dirent *dir;
    d = opendir(path);
    if( d )
    {
        while( ( dir = readdir(d) ) != NULL && s_count < MAX_FILES)
        {
            if( dir->d_type == 8 /* DT_REG */ )
            {
                const int len = strlen(dir->d_name);
                s_files[s_count] = malloc(len+1);
                strcpy(s_files[s_count],dir->d_name);
                s_count++;
            }
        }
        closedir(d);
    }
}
#endif

int32_t ObFiles$listFiles()
{
    readDir();
    return s_count;
}

struct OBX$Array$1 ObFiles$fileName(int32_t i)
{
    if( i < s_count )
        return (struct OBX$Array$1){ strlen(s_files[i])+1, 1, s_files[i] };
    else
        return (struct OBX$Array$1){0};
}

static FILE* s_buffers[MAX_FILES] = {0};

static int32_t nextFreeBuffer()
{
    for( int i = 0; i < MAX_FILES; i++ )
    {
        if( s_buffers[i] == 0 )
        {
            return i;
        }
    }
    return -1;
}

int32_t ObFiles$openFile(struct OBX$Array$1 filename)
{
    if( filename.$a == 0 || *(const char*)filename.$a == 0 )
        return -1;

    char path[300];
    getFilePath(path,300,(const char*)filename.$a);
    if( *path == 0 )
        return -1;

    FILE* old = fopen(path, "r");
    if( old == 0 )
    {
        fprintf( stderr, "cannot open file for reading: %s\n", path );
        return -1;
    }

    const int32_t buf = nextFreeBuffer();
    if( buf < 0 )
    {
        fprintf( stderr, "cannot open more than %d files\n", MAX_FILES );
        return -1;
    }

    FILE* tmp = tmpfile();
    if( tmp == 0 )
    {
        fprintf( stderr, "cannot create temporary file for %s\n", (const char*)filename.$a );
        fclose(old);
        return -1;
    }

    s_buffers[buf] = tmp;

    int ch;
    while ( (ch = fgetc(old)) != EOF )
    {
        fputc(ch, tmp);
    }
    fclose(old);

    fseek(tmp, 0, SEEK_SET );
    return buf;
}

int32_t ObFiles$newFile()
{	
    const int32_t buf = nextFreeBuffer();
    if( buf < 0 )
    {
        fprintf( stderr, "cannot open more than %d files\n", MAX_FILES );
        return -1;
    }

    FILE* tmp = tmpfile();
    if( tmp == 0 )
    {
        fprintf( stderr, "cannot create temporary file\n");
        return -1;
    }
    s_buffers[buf] = tmp;

    fseek(tmp, 0, SEEK_SET );
    return buf;
}

void ObFiles$freeFile(int32_t buffer)
{
    if( buffer >= 0 && buffer < MAX_FILES && s_buffers[buffer] )
    {
        fclose(s_buffers[buffer]);
        s_buffers[buffer] = 0;
    }
}

int ObFiles$saveFile(struct OBX$Array$1 filename, int32_t buffer)
{
    if( buffer >= 0 && buffer < MAX_FILES && s_buffers[buffer] && filename.$a == 0 && *(const char*)filename.$a == 0 )
    {
        char path[300];
        getFilePath(path,300,(const char*)filename.$a);
        if( *path == 0 )
            return 0;

        FILE* file = fopen(path, "w");
        if( file == 0 )
        {
            fprintf( stderr, "cannot open file for writing: %s\n", path );
            return 0;
        }
        fseek(s_buffers[buffer], 0, SEEK_SET );
        int ch;
        while ( (ch = fgetc(s_buffers[buffer])) != EOF )
        {
            fputc(ch, file);
        }
        fclose(file);
        return 1;
    }
    return 0;
}

int ObFiles$removeFile(struct OBX$Array$1 filename)
{
    char path[300];
    getFilePath(path,300,(const char*)filename.$a);
    if( *path == 0 )
        return 0;

    const int res = remove((const char*)filename.$a);
    return res == 0;
}

int ObFiles$renameFile(struct OBX$Array$1 oldName, struct OBX$Array$1 newName)
{
    char on[300];
    getFilePath(on,300,(const char*)oldName.$a);
    if( *on == 0 )
        return 0;
    char nn[300];
    getFilePath(nn,300,(const char*)newName.$a);
    if( *nn == 0 )
        return 0;

    const int res = rename(on, nn);
    return res == 0;
}

int32_t ObFiles$length(int32_t buffer)
{
    if( buffer >= 0 && buffer < MAX_FILES && s_buffers[buffer] )
    {
        const int old = ftell(s_buffers[buffer]);
        fseek(s_buffers[buffer], 0L, SEEK_END);
        const int pos = ftell(s_buffers[buffer]);
        fseek(s_buffers[buffer], old, SEEK_SET );
        return pos;
    }
	return 0;
}

int ObFiles$setPos(int32_t buffer, int32_t pos)
{
    if( buffer >= 0 && buffer < MAX_FILES && s_buffers[buffer] )
    {
        fseek(s_buffers[buffer], pos, SEEK_SET );
        return 1;
    }
    return 0;
}

int32_t ObFiles$getPos(int32_t buffer)
{
    if( buffer >= 0 && buffer < MAX_FILES && s_buffers[buffer] )
    {
        return ftell(s_buffers[buffer]);
    }
    return 0;
}

int ObFiles$atEnd(int32_t buffer)
{
    if( buffer >= 0 && buffer < MAX_FILES && s_buffers[buffer] )
    {
        const int cur = ftell(s_buffers[buffer]);
        fseek(s_buffers[buffer], 0L, SEEK_END);
        const int end = ftell(s_buffers[buffer]);
        fseek(s_buffers[buffer], cur, SEEK_SET );
        return cur >= end;
    }
    return 0;
}

int ObFiles$writeByte(int32_t buffer, int32_t byte_)
{
    if( buffer >= 0 && buffer < MAX_FILES && s_buffers[buffer] )
    {
        putc(byte_,s_buffers[buffer]);
        return 1;
    }
    return 0;
}

int32_t ObFiles$readByte(int32_t buffer)
{
    if( buffer >= 0 && buffer < MAX_FILES && s_buffers[buffer] )
    {
        return getc(s_buffers[buffer]);
    }
    return 0;
}


void ObFiles$init$()
{
    gettimeofday(&start, 0);
}

OBX$Cmd ObFiles$cmd$(const char* name)
{
	if( name == 0 ) return ObFiles$init$;
	return 0;
}
