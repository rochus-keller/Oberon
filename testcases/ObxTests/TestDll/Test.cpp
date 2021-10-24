#include "Test.h"
#include <QtDebug>
#include <stddef.h>

extern "C" {

TESTSHARED_EXPORT int doit( char* str, int a )
{
    qDebug() << "hello from Test.dll" << a << str;
    ::strcpy(str, "gandalf");
    return 66;
}

struct Struct
{
    int a,b;
    char str[32];
    int d;
    short e;
};

TESTSHARED_EXPORT int doit2( Struct* test )
{
    qDebug() << "hello from Test.dll" << test->a << test->b << test->str << test->d << test->e << test;
    ::strcpy(test->str, "gandalf");
    return 66;
}

TESTSHARED_EXPORT int doit3( int* test, int count )
{
    qDebug() << "hello 3 from Test.dll" << count;
    for( int i = 0; i < count; i++ )
        test[i] = count - i;
    return 77;
}

TESTSHARED_EXPORT int doit4( unsigned short* str, int count )
{
    qDebug() << "hello 4 from Test.dll" << count << QString::fromUtf16(str,count);
    QByteArray ba = "this is a test";
    for( int i = 0; i < ba.size(); i++ )
        str[i] = (unsigned char)ba[i];
    str[ba.size()] = 0;
    return 88;
}

struct Struct2
{
    int a,b;
    unsigned short str[32];
    int d;
};

TESTSHARED_EXPORT int doit5( Struct2* test )
{
    qDebug() << "hello 5 from Test.dll" << QString::fromUtf16(test->str,32);
    QByteArray ba = "this is a test";
    for( int i = 0; i < ba.size(); i++ )
        test->str[i] = (unsigned char)ba[i];
    test->str[ba.size()] = 0;
    return 99;
}

TESTSHARED_EXPORT int SDL_hello( )
{
    qDebug() << "SDL hello from Test.dll";
    return 42;
}

static Struct ss;

TESTSHARED_EXPORT int doit6( Struct** out )
{
    ss.a = 1234567;
    ss.b = 89012345;
    strcpy(ss.str,"static struct");
    ss.d = 678910;
    ss.e = 456;
    qDebug() << "hello from doit6";
    *out = &ss;
    return 123;
}

TESTSHARED_EXPORT Struct* doit7()
{
    ss.a = 678910;
    ss.b = 9012345;
    strcpy(ss.str,"static struct 2");
    ss.d = 1234567;
    ss.e = 789;
    qDebug() << "hello from doit7"
             << offsetof(Struct,b)
             << offsetof(Struct,str)
            << offsetof(Struct,d)
            << offsetof(Struct,e)
            << sizeof(Struct);
    return &ss;
}

struct Struct3
{
    int x,y,z;
};

struct Struct4
{
	Struct3 a,b;
	float w;
};

TESTSHARED_EXPORT int doit8( Struct3 in )
{
    qDebug() << "hello from doit8" << in.x << in.y << in.z << sizeof(Struct4);
    return 234;
}

TESTSHARED_EXPORT Struct3 doit9( int x, int y, int z )
{
    qDebug() << "hello from doit9" << x << y << z;
    Struct3 s;
    s.x = x;
    s.y = y;
    s.z = z;
    return s;
}

typedef int (*Callback)(Struct*);

TESTSHARED_EXPORT int doit10(Callback cb)
{
    qDebug() << "hello from doit10" << ( cb != 0 );
    int res = 0;
    if( cb == 0 )
        return res;
    for( int i = 0; i < 10; i++ )
    {
        ss.d = i;
        strcpy( ss.str, QString("callback %1").arg(i).toUtf8().constData() );
        res += cb(&ss);
    }
    return res;
}
}
