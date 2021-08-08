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

#include "ObsDisplay.h"
#include <lua.hpp>
#include <LjTools/Engine2.h>
#include <QPainter>
#include <QKeyEvent>
#include <QDir>
#include <QCoreApplication>
#include <QtDebug>
#include <stdint.h>
using namespace Obs;

static Display* s_disp = 0;
static Display::IdleHandler s_ih = 0;
static QVector<quint32> s_raster;
extern int g_obxQuit;

Display*Display::inst()
{
    if( s_disp == 0 )
        s_disp = new Display();
    return s_disp;
}

bool Display::isOpen()
{
    return s_disp != 0 && !g_obxQuit;
}

static int Display_RegisterHandler(lua_State* L)
{
    // Params:
    // 1: function
    // 2: what

    if( !lua_isfunction(L,1) )
        qCritical() << "ERROR expecting function";
    Display* d = Display::inst();
    switch( lua_tointeger(L,2) )
    {
    case 1: // MouseHandler
        lua_unref(L,d->mouseHandler);
        lua_pushvalue(L,1);
        d->mouseHandler = lua_ref(L,1);
        break;
    case 2: // CharHandler
        lua_unref(L,d->charHandler);
        lua_pushvalue(L,1);
        d->charHandler = lua_ref(L,1);
        break;
    case 3: // IdleHandler
        lua_unref(L,d->idleHandler);
        lua_pushvalue(L,1);
        d->idleHandler = lua_ref(L,1);
        break;
    }

    return 0;
}

void Display::install(lua_State* L)
{
    lua_pushcfunction( L, Display_RegisterHandler );
    lua_setglobal( L, "Display_RegisterHandler" );
}

int Display::mapToQt(int yOb)
{
    return Height - yOb - 1;
}

int Display::mapToOb(int yQt)
{
    return Height - yQt - 1; // yQt runs from zero to Height - 1; yOb zero is therefore at Height - 1
}

void Display::paintEvent(QPaintEvent*)
{
    QPainter p(this);
    p.drawImage(0,0,d_img);
}

static QImage rasterToImage();

void Display::timerEvent(QTimerEvent*)
{
    if( idleHandler != LUA_REFNIL && !Lua::Engine2::getInst()->isExecuting() )
    {
        lua_State* L = Lua::Engine2::getInst()->getCtx();
        lua_getref(L, idleHandler); // ()
        Lua::Engine2::getInst()->runFunction(0,0);
    }
    if( !s_raster.isEmpty() )
    {
       d_img = rasterToImage();
       update();
    }
    // d_img.save("/home/me/temp.png");
}

void Display::mouseMoveEvent(QMouseEvent* e)
{
    if( mapOb(e) )
        dispatchMouse(d_keys, d_xOb, d_yOb );
}

void Display::mousePressEvent(QMouseEvent* e)
{
    if( mapOb(e) )
        dispatchMouse(d_keys, d_xOb, d_yOb );
}

void Display::mouseReleaseEvent(QMouseEvent* e)
{
    if( mapOb(e) )
        dispatchMouse(d_keys, d_xOb, d_yOb );
}

void Display::keyPressEvent(QKeyEvent* e)
{
    d_buffer.append(e->text().toLatin1()[0]);
    if( charHandler != LUA_REFNIL && !Lua::Engine2::getInst()->isExecuting() )
    {
        lua_State* L = Lua::Engine2::getInst()->getCtx();
        lua_getref(L, charHandler); // ( ch: CHAR )
        lua_pushinteger(L, e->text().toLatin1()[0] );
        Lua::Engine2::getInst()->runFunction(1,0);
        d_buffer.clear();
    }
}

void Display::keyReleaseEvent(QKeyEvent*)
{
    // NOP
}

bool Display::mapOb(QMouseEvent* p)
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

    const bool modified = d_xOb != x || d_yOb != y || d_keys != bits;
    d_xOb = x;
    d_yOb = y;
    d_keys = bits;
    return modified;
}

void Display::dispatchMouse(const ObSet& keys, int x, int y)
{
    if( mouseHandler != LUA_REFNIL && !Lua::Engine2::getInst()->isExecuting() )
    {
        lua_State* L = Lua::Engine2::getInst()->getCtx();
        lua_getref(L, mouseHandler); // ( keys: SET; x, y: INTEGER )
        lua_pushinteger(L,keys.to_ulong());
        lua_pushinteger(L,x);
        lua_pushinteger(L,y);
        Lua::Engine2::getInst()->runFunction(3,0);
    }
}

void Display::closeEvent(QCloseEvent* event)
{
    QWidget::closeEvent(event);
    event->accept();
    g_obxQuit = true;
}

Display::Display():d_img(Width,Height,QImage::Format_Mono),d_xOb(0),d_yOb(0)
{
    setAttribute(Qt::WA_DeleteOnClose);

    d_img.fill(0);

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
    startTimer(30); // idle timer
}

Display::~Display()
{
    lua_unref( Lua::Engine2::getInst()->getCtx(), charHandler );
    lua_unref( Lua::Engine2::getInst()->getCtx(), mouseHandler );
    lua_unref( Lua::Engine2::getInst()->getCtx(), idleHandler );
    s_disp = 0;
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

typedef uint8_t ByteArray[];

struct BitStream
{
    quint8 d_byte, d_bit;
    const uint8_t* d_buf;
    int d_count;

    BitStream( const uint8_t* buf, int count ):d_buf(buf),d_byte(0),d_bit(0),d_count(count){}
    bool next()
    {
        quint8 cur = ( d_byte < d_count ? d_buf[d_byte] : 0 );
        quint8 res = cur & ( 1 << d_bit );
        d_bit++;
        if( d_bit > 7 )
        {
            d_byte++;
            d_bit = 0;
        }
        return res;
    }
    bool eof() const { return d_byte >= d_count; }
};

static QImage patternToImage( uint8_t* pat, int count )
{
    Q_ASSERT( count >= 2 );

    const int w = int(pat[0]);
    const int h = int(pat[1]);
    const int wBytes = ( w + 7 ) / 8;
    const int wSpare = wBytes * 8 - w;

    QImage img( w, h, QImage::Format_Mono );

    BitStream bs(pat+2, count);
    for( int y = 0; y < h; y++ )
    {
        for( int x = 0; x < w; x++ )
            img.setPixel(x,y, bs.next() );
        for( int i = 0; i < wSpare; i++ )
            bs.next();
    }
    return img.mirrored();
}

static QImage rasterToImage()
{
    QImage img( Display::Width, Display::Height, QImage::Format_Mono );

#if 0
    BitStream bs( (const uint8_t*)s_raster.constData(), s_raster.size() * 4); // TODO: endianness?
    for( int y = 0; y < Display::Height; y++ )
    {
        for( int x = 0; x < Display::Width; x++ )
            img.setPixel(x,y, bs.next() );
    }
    return img.mirrored();
#else
    for( int line = Display::Height - 1; line >= 0; line--)
    {
        const int line_start = (Display::Height - line - 1) * (Display::Width / 32);
        for( int col = 0; col < Display::Width/32; col++ )
        {
            const int x = col * 32;
            quint32 pixels = s_raster[line_start + col];
            for( int b = 0; b < 32; b++ )
            {
                img.setPixel(x + b,line, (pixels & 1) );
                pixels >>= 1;
            }
        }
    }
#endif
    return img;
}

#ifdef _WIN32
#define DllExport __declspec(dllexport)
#else
#define DllExport
#endif

extern "C"
{

typedef struct{
    uint32_t keys;
    int x, y;
} InputState;

DllExport void ObsDisplay_getState( InputState* s )
{
    Q_ASSERT( s );
    QCoreApplication::processEvents(); // requred here because getState is called in an Oberon loop depending on state change
    Display* d = Display::inst();
    s->keys = d->d_keys.to_ulong();
    s->x = d->d_xOb;
    s->y = d->d_yOb;
}

DllExport char ObsDisplay_nextKey()
{
    Display* d = Display::inst();
    char ch = 0;
    if( !d->d_buffer.isEmpty() )
    {
        ch = d->d_buffer.front();
        d->d_buffer.pop_front();
    }
    return ch;
}

DllExport uint32_t ObsDisplay_getTime()
{
    const uint32_t now = Display::inst()->start.msecsTo(QDateTime::currentDateTime());
    return now;
}

DllExport int ObsDisplay_getClock()
{
    const int now = Display::inst()->clock;
    return now;
}

DllExport void ObsDisplay_setClock(int dt)
{
    Display::inst()->clock = dt;
}

DllExport void ObsDisplay_ReplConst(int color, int x, int y, int w, int h, int mode )
{
    //qDebug() << "ReplConst" << color << x << y << w << h << mode;
    Display* d = Display::inst();

    y = Display::mapToQt(y);

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
}

DllExport void ObsDisplay_CopyPattern(int color, ByteArray patadr, int count, int x, int y, int mode )
{
    //qDebug() << "CopyPattern" << color << patadr << count << x << y << mode;
    Display* d = Display::inst();

    QImage img = patternToImage(patadr,count);
    y = Display::mapToQt(y) - img.height() + 1;

    for( int yi = 0; yi < img.height(); yi++ )
    {
        for( int xi = 0; xi < img.width(); xi++ )
        {
            const int src = img.pixelIndex( xi, yi );
            setPoint( d->d_img, x + xi, y + yi, mode, src, color );
        }
    }
    d->update();
}

DllExport void ObsDisplay_CopyBlock(int sx, int sy, int w, int h, int dx, int dy, int mode)
{
    //qDebug() << "CopyBlock" << sx << sy << w << h << dx << dy << mode;
    Display* d = Display::inst();
    sy = Display::mapToQt(sy);
    dy = Display::mapToQt(dy);
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
}

DllExport void ObsDisplay_Dot(int color, int x, int y, int mode)
{
    //qDebug() << "Dot" << color << x << y << mode;
    Display* d = Display::inst();

    y = Display::mapToQt(y);
    setPoint( d->d_img, x, y, mode, color );
    d->update();
}

DllExport quint32* ObsDisplay_createRasterBuffer(int len)
{
    Display* d = Display::inst(); // open display
    g_obxQuit = false;
    s_raster.resize(len);
    return s_raster.data();
}


}
