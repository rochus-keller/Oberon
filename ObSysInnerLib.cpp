/*
* Copyright 2020 Rochus Keller <mailto:me@rochus-keller.ch>
*
* This file is part of the Oberon parser/compiler library.
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

#include "ObSysInnerLib.h"
#include <lua.hpp>
#include <LjTools/Engine2.h>
#include <QPainter>
#include <QKeyEvent>
#include <QDir>
#include <QCoreApplication>
#include <QtDebug>
#include <QDateTime>
using namespace Ob;

static QtDisplay* s_disp = 0;
static QtDisplay::IdleHandler s_ih = 0;

QtDisplay*QtDisplay::inst()
{
    if( s_disp == 0 )
        s_disp = new QtDisplay();
    return s_disp;
}

int QtDisplay::mapToQt(int yOb)
{
    return Height - yOb - 1;
}

int QtDisplay::mapToOb(int yQt)
{
    return Height - yQt - 1; // yQt runs from zero to Height - 1; yOb zero is therefore at Height - 1
}

void QtDisplay::paintEvent(QPaintEvent*)
{
    QPainter p(this);
    p.drawImage(0,0,d_img);
}

void QtDisplay::timerEvent(QTimerEvent*)
{
    if( s_ih )
        ; // TODO s_ih();
}

void QtDisplay::mouseMoveEvent(QMouseEvent* e)
{
    mapOb(e);
    if( d_lock == 0 )
        dispatchMouse(d_keys, d_xOb, d_yOb );
}

void QtDisplay::mousePressEvent(QMouseEvent* e)
{
    if( mapOb(e) && d_lock == 0 )
        dispatchMouse(d_keys, d_xOb, d_yOb );
}

void QtDisplay::mouseReleaseEvent(QMouseEvent* e)
{
    if( mapOb(e) && d_lock == 0 )
        dispatchMouse(d_keys, d_xOb, d_yOb );
}

void QtDisplay::keyPressEvent(QKeyEvent* e)
{
    if( charHandler != LUA_REFNIL && !Lua::Engine2::getInst()->isExecuting() )
    {
        lua_State* L = Lua::Engine2::getInst()->getCtx();
        lua_getref(L, charHandler);
        _String* str = LjLib::strCreate(L);
        str->string.resize(2);
        str->string[0] = e->text().toLatin1()[0];
        Lua::Engine2::getInst()->runFunction(1,0);
    }
}

void QtDisplay::keyReleaseEvent(QKeyEvent*)
{
    // NOP
}

bool QtDisplay::mapOb(QMouseEvent* p)
{
    if( !rect().contains(p->pos()) )
        return false;
    const int x = p->pos().x();
    const int y = mapToOb(p->pos().y());
    std::bitset<32> bits;
    bits.set( 2, p->buttons() & Qt::LeftButton );
    bits.set( 1, p->buttons() & Qt::MidButton );
    if( p->modifiers() == Qt::ControlModifier && ( p->buttons() & Qt::LeftButton ) )
    {
        bits.set( 1, true ); // mid button == CTRL + left button
        bits.set( 2, false );
    }
    bits.set( 0, p->buttons() & Qt::RightButton );

    const bool modified = d_xOb != x || d_yOb != y || d_keys.bits != bits;
    d_xOb = x;
    d_yOb = y;
    d_keys.bits = bits;
    return modified;
}

void QtDisplay::dispatchMouse(const _Set& keys, int x, int y)
{
    if( mouseHandler != LUA_REFNIL && !Lua::Engine2::getInst()->isExecuting() )
    {
        lua_State* L = Lua::Engine2::getInst()->getCtx();
        lua_getref(L, mouseHandler);
        _Set* s = LjLib::setCreate(L);
        s->bits = keys.bits;
        lua_pushinteger(L,x);
        lua_pushinteger(L,y);
        Lua::Engine2::getInst()->runFunction(3,0);
    }
}

QtDisplay::QtDisplay():d_img(Width,Height,QImage::Format_Mono),d_lock(0)
{
    setAttribute(Qt::WA_DeleteOnClose);

    d_img.fill(0);

    arrow = QByteArray::fromHex("0F0F 0060 0070 0038 001C 000E 0007 8003 C101 E300 7700 3F00 1F00 3F00 7F00 FF00");
    star = QByteArray::fromHex("0F0F 8000 8220 8410 8808 9004 A002 C001 7F7F C001 A002 9004 8808 8410 8220 8000");
    hook = QByteArray::fromHex("0C0C 070F 8707 C703 E701 F700 7F00 3F00 1F00 0F00 0700 0300 01");
    updown = QByteArray::fromHex("080E 183C 7EFF 1818 1818 1818 FF7E3C18");
    block = QByteArray::fromHex("0808 FFFF C3C3 C3C3 FFFF");

    clock = 0;
    start = QDateTime::currentDateTime();

    mouseHandler = LUA_REFNIL;
    charHandler = LUA_REFNIL;
    idleHandler = LUA_REFNIL;

    setFixedSize(Width,Height);
    show();
    setMouseTracking(true);
    setFocusPolicy(Qt::StrongFocus);
    setCursor(Qt::BlankCursor);
    // TODO startTimer(0); // idle timer
}

QtDisplay::~QtDisplay()
{
    lua_unref( Lua::Engine2::getInst()->getCtx(), charHandler );
    lua_unref( Lua::Engine2::getInst()->getCtx(), mouseHandler );
    lua_unref( Lua::Engine2::getInst()->getCtx(), idleHandler );
    s_disp = 0;
}

/*********************** Tools ************************************/

static void installRecord( lua_State* L, const char* metaName, const char* recordName,
                           const char* moduleName, lua_CFunction gc, lua_CFunction create, lua_CFunction index = 0 )
{
    if( luaL_newmetatable( L, metaName ) == 0 )
        luaL_error( L, "metatable '%s' already registered", metaName );

    const int metaTable = lua_gettop(L);
    lua_newtable(L);
    const int pubMeta = lua_gettop(L);

    lua_pushliteral(L, "__metatable" );
    lua_pushvalue(L,pubMeta);
    lua_rawset(L, metaTable); // bytecode running getmetatable() gets this table

    lua_getglobal(L, moduleName );
    const int module = lua_gettop(L);
    lua_pushstring(L,recordName);
    lua_pushvalue(L,pubMeta);
    lua_rawset(L, module);

    lua_pushliteral(L, "__new");
    lua_pushcfunction(L, create);
    lua_rawset(L, pubMeta);

    if( index )
    {
        lua_pushliteral(L, "__index");
        lua_pushcfunction(L , index );
        lua_rawset(L, metaTable);
    }

    lua_pushliteral(L, "__gc");
    lua_pushcfunction(L , gc );
    lua_rawset(L, metaTable);

#ifdef _DEBUG
    lua_pushliteral(L,"__classname");
    lua_pushstring(L,recordName);
    lua_rawset(L,metaTable);
#endif

    lua_pop(L, 3);  // metaTable, pubMeta, module
}

static int DIV(int a, int b) // Source: http://lists.inf.ethz.ch/pipermail/oberon/2019/013353.html
{
    if (a < 0)
        return (a - b + 1) / b;
    else
        return a / b;
}

static int MOD(int a, int b) // Source: http://lists.inf.ethz.ch/pipermail/oberon/2019/013353.html
{
    if (a < 0)
        return (b - 1) + ((a - b + 1)) % b;
    else
        return a % b;
}

struct BitStream
{
    quint8 d_byte, d_bit;
    QByteArray d_buf;

    BitStream( const QByteArray& buf ):d_buf(buf),d_byte(0),d_bit(0){}
    bool next()
    {
        quint8 cur = ( d_byte < d_buf.size() ? d_buf[d_byte] : 0 );
        quint8 res = cur & ( 1 << d_bit );
        d_bit++;
        if( d_bit > 7 )
        {
            d_byte++;
            d_bit = 0;
        }
        return res;
    }
    bool eof() const { return d_byte >= d_buf.size(); }
};

static QImage patternToImage( const QByteArray& pat )
{
    Q_ASSERT( pat.size() >= 2 );

    const int w = int(pat[0]);
    const int h = int(pat[1]);
    const int wBytes = ( w + 7 ) / 8;
    const int wSpare = wBytes * 8 - w;

    QImage img( w, h, QImage::Format_Mono );

    BitStream bs(pat.mid(2));
    for( int y = 0; y < h; y++ )
    {
        for( int x = 0; x < w; x++ )
            img.setPixel(x,y, bs.next() );
        for( int i = 0; i < wSpare; i++ )
            bs.next();
    }
    return img.mirrored();
}

static int Generic_new(lua_State* L)
{
    lua_newtable(L);
    const int top = lua_gettop(L);
    lua_pushvalue(L, lua_upvalueindex(1) );
    Q_ASSERT( lua_type(L, -1 ) == LUA_TTABLE );
    lua_setmetatable(L,top);
    return 1;
}

/*********************** Module File ******************************/
#define FileDesc_METANAME "ObnFileDesc"

FileDesc* FileDesc::check(lua_State *L, int narg, bool nilAllowed )
{
    if( nilAllowed && lua_isnil(L,narg) )
        return 0;
    return static_cast<FileDesc*>( luaL_checkudata( L, narg, FileDesc_METANAME ) );
}

FileDesc* FileDesc::create(lua_State* L)
{
    void* buf = lua_newuserdata( L, sizeof(FileDesc) );
    FileDesc* s = ::new( buf ) FileDesc();

    luaL_getmetatable( L, FileDesc_METANAME );
    if( !lua_istable(L, -1 ) )
        luaL_error( L, "internal error: no meta table for '%s'", FileDesc_METANAME );
    lua_setmetatable( L, -2 );
    return s;
}

int FileDesc::__new(lua_State* L)
{
    create(L);
    return 1;
}

int FileDesc::__gc(lua_State* L)
{
    FileDesc* s = check(L,1);
    s->~FileDesc();  // call destructor
    return 0;
}

static QString getFileSystemPath(lua_State* L)
{
    lua_getglobal(L,"fileSystemPath");
    const QByteArray path = lua_tostring(L,-1);
    lua_pop(L,1);
    if( !path.isEmpty() )
        return path;
    else
        return QDir::currentPath().toUtf8();
}

int FileDesc::Old(lua_State* L)
{
    _String* name = LjLib::strCheck(L, 1);
    QDir dir( getFileSystemPath(L) );
    QFile f( dir.absoluteFilePath(name->string.c_str()) );
    if( f.exists() )
    {
        f.open(QIODevice::ReadOnly);
        FileDesc* res = FileDesc::create(L);
        res->d_name = name->string.c_str();
        res->d_data = f.readAll();
    }else
        lua_pushnil(L);
    return 1;
}

int FileDesc::New(lua_State* L)
{
    FileDesc* res = create(L);
    _String* name = LjLib::strCheck(L, 1);
    res->d_name = name->string.c_str();
    return 1;
}

int FileDesc::Register(lua_State* L)
{
    FileDesc* f = check(L,1);
    QDir dir(getFileSystemPath(L));
    QFile out( dir.absoluteFilePath(f->d_name.data()) );
    out.open(QIODevice::WriteOnly);
    out.write(f->d_data);
    return 0;
}

int FileDesc::Delete(lua_State* L)
{
    _String* name = LjLib::strCheck(L, 1);
    QDir dir(getFileSystemPath(L));
    dir.remove(name->string.c_str());
    int res = 0;
    lua_pushinteger(L,res);
    return 1;
}

int FileDesc::Rename(lua_State* L)
{
    _String* old = LjLib::strCheck(L, 1);
    _String* new_ = LjLib::strCheck(L, 2);
    int res = lua_tointeger(L,3);
    QDir dir(getFileSystemPath(L));
    if( !QFileInfo(dir.absoluteFilePath(old->string.c_str())).exists() )
    {
        res = 2;
        lua_pushinteger(L,res);
        return 1;
    }
    QFileInfo info(dir.absoluteFilePath(new_->string.c_str()));
    if( info.exists() )
    {
        dir.remove(new_->string.c_str());
        res = 1;
        if( !dir.rename(old->string.c_str(),new_->string.c_str()) )
            res = 3;
    }else
    {
        res = 0;
        if( !dir.rename(old->string.c_str(),new_->string.c_str()) )
            res = 3;
    }
    lua_pushinteger(L,res);
    return 1;
}

int FileDesc::Length(lua_State* L)
{
    FileDesc* f = check(L,1);
    lua_pushinteger(L, f->d_data.size() );
    return 1;
}

void FileDesc::install(lua_State* L)
{
    installRecord(L,FileDesc_METANAME,"FileDesc","Files",__gc, __new);
}

#define Rider_METANAME "ObnRider"

Rider* Rider::check(lua_State *L, int narg)
{
#ifdef _DEBUG_
    lua_getmetatable(L,narg);
    lua_getfield(L,-1,"__classname");
    qDebug() << "class" << lua_tostring(L,-1);
    lua_pop(L,2);
#endif
    return static_cast<Rider*>( luaL_checkudata( L, narg, Rider_METANAME ) );
}

int Rider::__new(lua_State* L)
{
    void* buf = lua_newuserdata( L, sizeof(Rider) );
    Rider* s = ::new( buf ) Rider();

    s->eof = false;
    s->res = 0;
    s->d_file = LUA_REFNIL;

    luaL_getmetatable( L, Rider_METANAME );
    if( !lua_istable(L, -1 ) )
        luaL_error( L, "internal error: no meta table for '%s'", Rider_METANAME );
    lua_setmetatable( L, -2 );
    return 1;
}

int Rider::__gc(lua_State* L)
{
    Rider* s = check(L,1);
    lua_unref(L, s->d_file );
    s->~Rider();  // call destructor
    return 0;
}

int Rider::__index(lua_State* L)
{
    Rider* s = check(L,1);
    QByteArray name = lua_tostring(L,2);
    if( name == "eof" )
        lua_pushboolean(L,s->eof);
    else if( name == "res" )
        lua_pushinteger(L,s->res);
    else
        lua_pushnil(L);
    return 1;
}

int Rider::Set(lua_State* L)
{
    Rider* r = check(L,1);
    FileDesc* f = FileDesc::check(L,2,true);
    int pos = luaL_checkinteger(L,3);

    r->eof = false;
    r->res = 0;
    if( f == 0 )
    {
        r->d_buf.close();
        lua_unref(L,r->d_file);
        r->d_file = LUA_REFNIL;
        lua_pushvalue(L,1);
        return 1;
    }
    if( !r->d_buf.isOpen() )
    {
        r->d_buf.setBuffer( &f->d_data );
        r->d_buf.open( QIODevice::ReadWrite );
    }
    if( pos < 0 )
        pos = 0;
    r->d_buf.seek(pos);
    lua_pushvalue(L,2);
    r->d_file = lua_ref(L,1);
    lua_pushvalue(L,1);
    return 1;
}

int Rider::Pos(lua_State* L)
{
    Rider* r = check(L,1);
    lua_pushinteger( L,r->d_buf.pos() );
    lua_pushvalue(L,1);
    return 2;
}

int Rider::Base(lua_State* L)
{
    Rider* r = check(L,1);
    lua_rawgeti(L, LUA_REGISTRYINDEX, r->d_file);
    lua_pushvalue(L,1);
    return 2;
}

int Rider::ReadByte(lua_State* L)
{
    Rider* r = check(L,1);
    quint8 x;
    r->ReadByte(x);
    lua_pushvalue(L,1);
    lua_pushinteger(L,x);
    return 2;
}

void Rider::ReadByte(quint8& x)
{
    eof = false;
    res = 0;
    if( d_buf.atEnd() || !d_buf.isOpen() )
    {
        eof = true;
        x = 0;
        return;
    }
    if( !d_buf.getChar( (char*)&x ) )
        res = 1; // num of bytes not read
}

int Rider::Read(lua_State* L)
{
    Rider* r = check(L,1);
    quint8 x;
    r->ReadByte(x);
    lua_pushvalue(L,1);
    _String* s2 = LjLib::strCreate(L);
    s2->string.resize(2);
    s2->string[0] = x;
    return 2;
}

int Rider::ReadInt(lua_State* L)
{
    Rider* r = check(L,1);

    quint8 x0, x1, x2, x3;
    r->ReadByte(x0); r->ReadByte(x1); r->ReadByte(x2); r->ReadByte(x3);
    int x = ((x3 * 0x100 + x2) * 0x100 + x1) * 0x100 + x0;

    lua_pushvalue(L,1);
    lua_pushinteger(L,x);
    return 2;
}

int Rider::ReadString(lua_State* L)
{
    Rider* r = check(L,1);
    _String* x = LjLib::strCheck(L, 2);
    int i = 0;
    quint8 ch;
    r->ReadByte(ch);
    while( ch != 0 )
    {
      if( i < x->string.size()-1 )
      {
          x->string[i] = ch; i++;
      }
      r->ReadByte(ch);
    }
    x->string[i] = 0;
    lua_pushvalue(L,1);
    lua_pushvalue(L,2);
    return 2;
}

void Rider::WriteByte(quint8 x)
{
    if( !d_buf.putChar((char)x) )
        res++;
}

int Rider::WriteByte(lua_State* L)
{
    Rider* r = check(L,1);
    const int x = luaL_checkinteger(L,2);
    r->res = 0;
    r->WriteByte(x);
    lua_pushvalue(L,1);
    return 1;
}

int Rider::Write(lua_State* L)
{
    Rider* r = check(L,1);
    std::string ch;
    if( lua_type(L,2) == LUA_TSTRING )
        ch = lua_tostring(L,2);
    else
        ch = LjLib::strCheck(L, 2)->string;
    r->res = 0;
    if( ch.size() >= 1 )
        r->WriteByte(ch[0]);
    lua_pushvalue(L,1);
    return 1;
}

int Rider::WriteInt(lua_State* L)
{
    Rider* r = check(L,1);
    const int x = luaL_checkinteger(L,2);

    r->res = 0;
    r->WriteByte(MOD(x,0x100));
    r->WriteByte(MOD(DIV(x,0x100),0x100));
    r->WriteByte(MOD(DIV(x,0x10000),0x100));
    r->WriteByte(MOD(DIV(x,0x1000000),0x100));

    lua_pushvalue(L,1);
    return 1;
}

int Rider::WriteString(lua_State* L)
{
    Rider* r = check(L,1);
    _String* x = LjLib::strCheck(L, 2);

    r->res = 0;
    int i = 0;
    quint8 ch;
    do
    {
        ch = x->string[i];
        r->WriteByte(ch);
        i++;
    } while( !( ch == 0x0 ) );

    lua_pushvalue(L,1);
    return 1;
}

int Rider::RestoreList(lua_State* L)
{
    // NOP ?
    return 0;
}

void Rider::install(lua_State* L)
{
    installRecord(L,Rider_METANAME,"Rider","Files",__gc,__new,__index);
}

static const luaL_Reg Files_Reg[] =
{
    { "RestoreList", Rider::RestoreList },
    { "WriteString", Rider::WriteString },
    { "WriteInt", Rider::WriteInt },
    { "Write", Rider::Write },
    { "WriteByte", Rider::WriteByte },
    { "ReadString", Rider::ReadString },
    { "ReadInt", Rider::ReadInt },
    { "Read", Rider::Read },
    { "ReadByte", Rider::ReadByte },
    { "Base", Rider::Base },
    { "Pos", Rider::Pos },
    { "Set", Rider::Set },
    { "Length", FileDesc::Length },
    { "Rename", FileDesc::Rename },
    { "Delete", FileDesc::Delete },
    { "Register", FileDesc::Register },
    { "Old", FileDesc::Old },
    { "New", FileDesc::New },
    { NULL,		NULL	}
};

int SysInnerLib::installFiles(lua_State* L)
{
    luaL_register( L, "Files", Files_Reg ); // must come first because module is used in installs
    FileDesc::install(L);
    Rider::install(L);

    return 1;
}

/***************** MODULE FileDir ********************************/

static const luaL_Reg FileDir_Reg[] =
{
    // TODO
    { NULL,		NULL	}
};

int SysInnerLib::installFileDir(lua_State* L)
{
    luaL_register( L, "FileDir", FileDir_Reg );

    return 1;
}

/***************** MODULE Kernel ********************************/

static int Kernel_Time(lua_State* L)
{
    lua_pushinteger( L, QtDisplay::inst()->start.msecsTo(QDateTime::currentDateTime()) );
    return 1;
}

static int Kernel_Clock(lua_State* L)
{
    lua_pushinteger( L, QtDisplay::inst()->clock );
    return 1;
}

static int Kernel_SetClock(lua_State* L)
{
    const int dt = luaL_checkinteger(L,1);
    QtDisplay::inst()->clock = dt;
    return 0;
}

static const luaL_Reg Kernel_Reg[] =
{
    { "Time",		Kernel_Time	},
    { "Clock",		Kernel_Clock	},
    { "SetClock",		Kernel_SetClock	},
    { NULL,		NULL	}
};

int SysInnerLib::installKernel(lua_State* L)
{
    luaL_register( L, "Kernel", Kernel_Reg );

    return 1;
}

/***************** MODULE Display ********************************/

static int Display_Width(lua_State* L)
{
    lua_pushinteger(L, QtDisplay::Width );
    return 1;
}

static int Display_Height(lua_State* L)
{
    lua_pushinteger(L, QtDisplay::Height );
    return 1;
}

static int Display_arrow(lua_State* L)
{
    lua_pushlstring(L, QtDisplay::inst()->arrow.constData(), QtDisplay::inst()->arrow.size() );
    return 1;
}

static int Display_star(lua_State* L)
{
    lua_pushlstring(L, QtDisplay::inst()->star.constData(), QtDisplay::inst()->star.size() );
    return 1;
}

static int Display_hook(lua_State* L)
{
    lua_pushlstring(L, QtDisplay::inst()->hook.constData(), QtDisplay::inst()->hook.size() );
    return 1;
}

static int Display_updown(lua_State* L)
{
    lua_pushlstring(L, QtDisplay::inst()->updown.constData(), QtDisplay::inst()->updown.size() );
    return 1;
}

static int Display_block(lua_State* L)
{
    lua_pushlstring(L, QtDisplay::inst()->block.constData(), QtDisplay::inst()->block.size() );
    return 1;
}

enum Display_Operation { Display_replace = 0, Display_paint = 1, Display_invert = 2,
                       Display_PatternLen = 15 * 15 / 8 + 2 };

static void setPoint( QImage& img, int x, int y, int mode, int color )
{
    if( x < 0 || y < 0 || x >= img.width() || y >= img.height() )
        return;

    if( color > 1 )
        color = 1; // RISK

    const int dst = img.pixelIndex(x,y);
    if( mode == Display_replace )
        img.setPixel(x,y, color );
    else if( mode == Display_paint )
        img.setPixel(x,y, color || dst );
    else if( mode == Display_invert )
        img.setPixel(x,y, ( color || dst ) && !( color && dst ) );
    else
        Q_ASSERT(false);
}

static int Display_ReplConst(lua_State* L)
{
    const int color = luaL_checkinteger(L,1);
    const int x = luaL_checkinteger(L,2);
    int y = luaL_checkinteger(L,3);
    const int w = luaL_checkinteger(L,4);
    const int h = luaL_checkinteger(L,5);
    const int mode = luaL_checkinteger(L,6);

    // qDebug() << "ReplConst" << color << x << y << w << h << mode;

    QtDisplay* d = QtDisplay::inst();

    y = QtDisplay::mapToQt(y);

    if( w > 1 && h > 1 )
    {
        for( int i = x; i < x + w; i++ )
        {
            for( int j = y; j > y - h; j-- )
                setPoint( d->d_img, i, j, mode, color );
        }
    }else if( w > 1 )
    {
        for( int i = x; i < x + w; i++ )
            setPoint( d->d_img, i, y, mode, color );
    }else if( h > 1 )
    {
        for( int i = y; i > ( y - h ); i-- )
            setPoint( d->d_img, x, i, mode, color );
    }else
        setPoint( d->d_img, x, y, mode, color );
    d->update();
    return 0;
}

static void setPoint( QImage& img, int x, int y, int mode, int src, int color )
{
    if( x < 0 || y < 0 || x >= img.width() || y >= img.height() )
        return;

    if( color > 1 )
        color = 1; // RISK

    const int dst = img.pixelIndex(x,y);
    if( mode == Display_replace )
        img.setPixel(x,y, src == 0 ? 0 : color );
    else if( mode == Display_paint )
        img.setPixel(x,y, src == 0 ? dst : color );
    else if( mode == Display_invert )
        img.setPixel(x,y, src == 0 ? dst : abs(color - dst) );
    else
        Q_ASSERT(false);
}

static int Display_CopyPattern(lua_State* L)
{
    const int color = luaL_checkinteger(L,1);
    QByteArray pattern;
    switch( lua_type(L,2) )
    {
    case LUA_TSTRING:
        {
            size_t len;
            const char* buf = lua_tolstring(L,2,&len);
            pattern = QByteArray(buf,len);
        }
        break;
    case LUA_TTABLE:
        {
            const int len = lua_objlen(L,2);
            pattern.resize(len);
            for( int i = 1; i <= len; i++ )
            {
                lua_rawgeti(L,2,i);
                pattern[i-1] = lua_tointeger(L,-1);
                lua_pop(L,1);
            }
        }
        break;
    default:
        qWarning() << "Display_CopyPattern: invalid pattern";
        break;
    }
    const int x = luaL_checkinteger(L,3);
    int y = luaL_checkinteger(L,4);
    const int mode = luaL_checkinteger(L,5);

    //qDebug() << "CopyPattern" << color << x << y << mode;
    QtDisplay* d = QtDisplay::inst();

    QImage img = patternToImage(pattern);
    y = QtDisplay::mapToQt(y) - img.height() + 1;

    for( int yi = 0; yi < img.height(); yi++ )
    {
        for( int xi = 0; xi < img.width(); xi++ )
        {
            const int src = img.pixelIndex( xi, yi );
            setPoint( d->d_img, x + xi, y + yi, mode, src, color );
        }
    }
    d->update();
    return 0;
}

static int Display_CopyBlock(lua_State* L)
{
    const int sx = luaL_checkinteger(L,1);
    int sy = luaL_checkinteger(L,2);
    const int w = luaL_checkinteger(L,3);
    const int h = luaL_checkinteger(L,4);
    const int dx = luaL_checkinteger(L,5);
    int dy = luaL_checkinteger(L,6);
    const int mode = luaL_checkinteger(L,7);

    //qDebug() << "CopyBlock" << sx << sy << w << h << dx << dy << mode;
    QtDisplay* d = QtDisplay::inst();

    sy = QtDisplay::mapToQt(sy);
    dy = QtDisplay::mapToQt(dy);
    // qDebug() << "copy block source" << sx << sy << w << h << "dest" << dx << dy;
    QImage img( w, h, QImage::Format_Mono );
    for( int y = 0; y < h; y++ )
    {
        for( int x = 0; x < w; x++ )
            img.setPixel( x, y, d->d_img.pixelIndex( sx + x, sy - h + 1 + y ) );
    }
    //img.save(QString("%1-%2-%3-%4-%5-%6.png").arg(sx).arg(sy).arg(w).arg(h).arg(dx).arg(dy));
    for( int y = 0; y < h; y++ )
    {
        for( int x = 0; x < w; x++ )
            setPoint( d->d_img, dx + x, dy - h + 1 + y, mode, img.pixelIndex( x, y ) );
    }
    d->update();

    return 0;
}

static const luaL_Reg Display_Reg[] =
{
    { "ReplConst", Display_ReplConst },
    { "CopyPattern", Display_CopyPattern },
    { "CopyBlock", Display_CopyBlock },
    { NULL,		NULL	}
};

int SysInnerLib::installDisplay(lua_State* L)
{
    luaL_register( L, "Display", Display_Reg );

    const int module = lua_gettop(L);
    lua_pushliteral(L,"black");
    lua_pushinteger(L,0);
    lua_rawset(L, module);
    lua_pushliteral(L,"white");
    lua_pushinteger(L,1);
    lua_rawset(L, module);
    lua_pushliteral(L,"replace");
    lua_pushinteger(L,Display_replace);
    lua_rawset(L, module);
    lua_pushliteral(L,"paint");
    lua_pushinteger(L,Display_paint);
    lua_rawset(L, module);
    lua_pushliteral(L,"invert");
    lua_pushinteger(L,Display_invert);
    lua_rawset(L, module);
    lua_pushliteral(L,"PatternLen");
    lua_pushinteger(L,Display_PatternLen);
    lua_rawset(L, module);
    lua_pushliteral(L,"Width");
    lua_pushcfunction(L,Display_Width);
    lua_rawset(L, module);
    lua_pushliteral(L,"Height");
    lua_pushcfunction(L,Display_Height);
    lua_rawset(L, module);
    lua_pushliteral(L,"arrow");
    lua_pushcfunction(L,Display_arrow);
    lua_rawset(L, module);
    lua_pushliteral(L,"star");
    lua_pushcfunction(L,Display_star);
    lua_rawset(L, module);
    lua_pushliteral(L,"hook");
    lua_pushcfunction(L,Display_hook);
    lua_rawset(L, module);
    lua_pushliteral(L,"updown");
    lua_pushcfunction(L,Display_updown);
    lua_rawset(L, module);
    lua_pushliteral(L,"block");
    lua_pushcfunction(L,Display_block);
    lua_rawset(L, module);

    lua_newtable(L);
    const int FrameMsg = lua_gettop(L);
    lua_pushliteral(L,"FrameMsg");
    lua_pushvalue(L,FrameMsg);
    lua_rawset(L, module);

    lua_pushliteral(L,"__new");
    lua_pushvalue(L,FrameMsg);
    lua_pushcclosure(L, Generic_new, 1 );
    lua_rawset(L, FrameMsg);

    lua_newtable(L);
    const int FrameDesc = lua_gettop(L);
    lua_pushliteral(L,"FrameDesc");
    lua_pushvalue(L,FrameDesc);
    lua_rawset(L, module);

    lua_pushliteral(L,"__new");
    lua_pushvalue(L,FrameDesc);
    lua_pushcclosure(L, Generic_new, 1 );
    lua_rawset(L, FrameDesc);

    lua_pop(L, 2); // FrameMsg, FrameDesc

    return 1;
}

/***************** MODULE Modules ********************************/

static std::string importing;
static std::string imported;
static int root = LUA_REFNIL;
static int res = 0;

static int Modules_importing(lua_State* L)
{
    _String* str = LjLib::strCreate(L);
    str->string = importing;
    return 1;
}

static int Modules_imported(lua_State* L)
{
    _String* str = LjLib::strCreate(L);
    str->string = imported;
    return 1;
}

static int Modules_root(lua_State* L)
{
    lua_getref(L,root);
    return 1;
}

static int Modules_res(lua_State* L)
{
    lua_pushinteger(L,res);
    return 1;
}

static int Modules_Load(lua_State* L)
{
    // name: ARRAY OF CHAR; VAR newmod: Module
    _String* str = LjLib::strCheck(L, 1);
    importing = str->string;
    lua_getglobal(L,str->string.c_str());
    if( lua_type(L,-1) == LUA_TNIL )
    {
        // no module for name
        res = 1;
        return 1;
    }else
    {

        lua_pop(L,1); // module

        lua_getref(L,root);
        while( lua_istable(L,-1) )
        {
            lua_getfield( L,-1,"name" );
            if( !lua_isnil(L,-1) )
            {
                _String* name = LjLib::strCheck(L, -1);
                if( name->string == str->string )
                {
                    lua_pop(L,1); // name
                    res = 0;
                    imported = str->string;
                    return 1;
                }
            }
            lua_pop(L,1); // name
            lua_getfield( L,-1,"next" );
            lua_remove(L,-2); // prev table
        }
        lua_pop(L,1); // nil prev table

        lua_newtable(L);
        const int ModDesc = lua_gettop(L);

        lua_getglobal(L, "Modules" );
        lua_getfield(L,-1,"ModDesc");
        lua_remove(L,-2); // modules
        lua_setmetatable(L,ModDesc);

        lua_pushvalue(L,1); // string
        lua_setfield(L,ModDesc,"name");

        lua_pushliteral(L,"refcnt");
        lua_pushinteger(L,0);
        lua_rawset(L, ModDesc);

        lua_pushliteral(L,"code");
        lua_pushinteger(L,0);
        lua_rawset(L, ModDesc);

        res = 0;
        imported = str->string;

        lua_getref(L,root);
        lua_unref(L,root);
        lua_setfield(L,ModDesc,"next");
        lua_pushvalue(L,ModDesc);
        root = lua_ref(L,1);
    }
    return 1;
}

static int Modules_ThisCommand(lua_State* L)
{
    // mod: Module; name: ARRAY OF CHAR): Command;
    res = 5;
    if( !lua_istable(L,1) )
        lua_pushnil(L);
    else
    {
        lua_getfield(L,1,"name");
        _String* modName = LjLib::strCheck(L,-1);
        lua_pop(L,1); // name
        lua_getglobal(L,modName->string.c_str());
        const int module = lua_gettop(L);
        _String* str = LjLib::strCheck(L, 2);
        lua_getfield(L,module, str->string.c_str());
        if( !lua_isnil(L,-1) )
            res = 0;
        lua_remove(L,module);
    }
    return 1;
}

static int Modules_Free(lua_State* L)
{
    // name: ARRAY OF CHAR
    // NOP
    return 0;
}

static const luaL_Reg Modules_Reg[] =
{
    { "res", Modules_res },
    { "root", Modules_root },
    { "imported", Modules_imported },
    { "importing", Modules_importing },
    { "Load", Modules_Load },
    { "ThisCommand", Modules_ThisCommand },
    { "Free", Modules_Free },
    { NULL,		NULL	}
};

int SysInnerLib::installModules(lua_State* L)
{
    luaL_register( L, "Modules", Modules_Reg );
    const int module = lua_gettop(L);

    lua_newtable(L);
    const int ModDesc = lua_gettop(L);
    lua_pushliteral(L,"ModDesc");
    lua_pushvalue(L,ModDesc);
    lua_rawset(L, module);

    lua_pushliteral(L,"__new");
    lua_pushvalue(L,ModDesc);
    lua_pushcclosure(L, Generic_new, 1 );
    lua_rawset(L, ModDesc);


    lua_pop(L, 1); // ModDesc
    return 1;
}

/***************** MODULE Input ********************************/

static int Input_RegisterMouseHandler(lua_State* L)
{
    QtDisplay* d = QtDisplay::inst();
    lua_unref(L,d->mouseHandler);
    lua_pushvalue(L,1);
    d->mouseHandler = lua_ref(L,1);
    return 0;
}

static int Input_RegisterCharHandler(lua_State* L)
{
    QtDisplay* d = QtDisplay::inst();
    lua_unref(L,d->charHandler);
    lua_pushvalue(L,1);
    d->charHandler = lua_ref(L,1);
    return 0;
}

static int Input_RegisterIdleHandler(lua_State* L)
{
    QtDisplay* d = QtDisplay::inst();
    lua_unref(L,d->idleHandler);
    lua_pushvalue(L,1);
    d->idleHandler = lua_ref(L,1);
    return 0;
}

static int Input_Available(lua_State* L)
{
    // obsolete
    lua_pushinteger(L,0);
    return 1;
}

static int Input_Read(lua_State* L)
{
    // obsolete
    _String* s2 = LjLib::strCreate(L);
    s2->string.resize(2);
    s2->string[0] = 0;
    return 1;
}

static int Input_Mouse(lua_State* L)
{
    // _Set& keys, int& x, int& y

    _Set* s = LjLib::setCheck(L, 1);
    QtDisplay* d = QtDisplay::inst();
    d->d_lock++;
    QCoreApplication::processEvents();
    s->bits = d->d_keys.bits;
    lua_pushvalue(L,1);
    lua_pushinteger(L, d->d_xOb);
    lua_pushinteger(L, d->d_yOb);
    d->d_lock--;
    return 3;
}

static int Input_SetMouseLimits(lua_State* L)
{
    // int w, int h
    // NOP
    return 0;
}

static const luaL_Reg Input_Reg[] =
{
    { "SetMouseLimits",		Input_SetMouseLimits	},
    { "Mouse",		Input_Mouse	},
    { "Available",		Input_Available	},
    { "Read",		Input_Read	},
    { "RegisterMouseHandler",		Input_RegisterMouseHandler	},
    { "RegisterCharHandler",		Input_RegisterCharHandler	},
    { "RegisterIdleHandler",		Input_RegisterIdleHandler	},
    { NULL,		NULL	}
};

int SysInnerLib::installInput(lua_State* L)
{
    luaL_register( L, "Input", Input_Reg );

    return 1;
}

static void saveCall(lua_State* L)
{
    const int err = lua_pcall( L, 0, 0, 0 );
    switch( err )
    {
    case LUA_ERRRUN:
        qCritical() << lua_tostring( L, -1 );
        lua_pop( L, 1 );  /* remove error message */
        break;
    case LUA_ERRMEM:
        qCritical() << "Lua memory exception";
        break;
    case LUA_ERRERR:
        // should not happen
        qCritical() << "Lua unknown error";
        break;
    }
}

void SysInnerLib::install(lua_State* L)
{
    lua_pushcfunction(L, installFiles);
    saveCall(L);
    lua_pushcfunction(L, installFileDir);
    saveCall(L);
    lua_pushcfunction(L, installKernel);
    saveCall(L);
    lua_pushcfunction(L, installDisplay);
    saveCall(L);
    lua_pushcfunction(L, installModules);
    saveCall(L);
    lua_pushcfunction(L, installInput);
    saveCall(L);
}

void SysInnerLib::quit()
{
    if( s_disp )
        s_disp->close();
}

