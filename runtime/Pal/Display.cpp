/*
* Copyright 2023 Rochus Keller <mailto:me@rochus-keller.ch>
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
*/

#include <QWindow>
#include <QGuiApplication>
#include <QBackingStore>
#include <QMouseEvent>
#include <QPainter>
#include <QtDebug>

static int argc = 1;
static char * argv = "PAL";
static QGuiApplication* app = 0;
static bool quit = false;

class PalScreen : public QWindow
{
public:
    PalScreen(int width, int height, int format):
        x(0),y(0), w(width),h(height),f(format),buf(0),prev(0)
    {
        bs = new QBackingStore(this);
        const QSize s(w,h);
        resize(s);
        bs->resize(s);
        setMinimumSize(s);
        setMaximumSize(s);
        setCursor(Qt::BlankCursor);
        show();
    }

    void keyPressEvent(QKeyEvent * ev)
    {
        const QByteArray key = ev->text().toLatin1();
        if( !key.isEmpty() )
            queue.push_front(key[0]);
    }

    void mouseMoveEvent(QMouseEvent * ev)
    {
        x = ev->x();
        y = ev->y();
        setMouseButtons(ev->buttons(),ev->modifiers());
    }

    void mousePressEvent(QMouseEvent * ev)
    {
        x = ev->x();
        y = ev->y();
        setMouseButtons(ev->buttons(),ev->modifiers());
    }

    void setMouseButtons(Qt::MouseButtons b_, Qt::KeyboardModifiers m)
    {
        b = b_;
        if( ( b == Qt::RightButton || b == Qt::LeftButton ) && m == Qt::ControlModifier )
            b = Qt::MidButton;
        else if( b == Qt::LeftButton && m == ( Qt::ControlModifier | Qt::ShiftModifier ) )
            b = Qt::RightButton;
    }

    void hideEvent(QHideEvent *)
    {
        quit = true;
    }

    void updateMono( quint32* raster )
    {
        QImage img( w, h, QImage::Format_Mono );

        for( int line = h - 1; line >= 0; line--)
        {
            const int line_start = (h - line - 1) * (w / 32);
            for( int col = 0; col < w/32; col++ )
            {
                const int x = col * 32;
                quint32 pixels = raster[line_start + col];
                for( int b = 0; b < 32; b++ )
                {
                    img.setPixel(x + b,line, (pixels & 1) );
                    pixels >>= 1;
                }
            }
        }
        QRect rect(0, 0, w, h);
        bs->beginPaint(rect);
        QPainter p(bs->paintDevice());
        p.drawImage(0,0,img);
        bs->endPaint();
        bs->flush(rect);
    }

    void update8888( quint32* raster )
    {
        QImage img( w, h, QImage::Format_RGB888 );
        union
        {
            quint32 rgba;
            quint8 bytes[4];
        } point;

        for( int line = h - 1; line >= 0; line--)
        {
            for( int col = 0; col < w; col++ )
            {
                point.rgba = raster[line * w + col];
                img.setPixel(col,line, qRgb(point.bytes[2], point.bytes[1], point.bytes[0]) );
            }
        }
        QRect rect(0, 0, w, h);
        bs->beginPaint(rect);
        QPainter p(bs->paintDevice());
        p.drawImage(0,0,img);
        bs->endPaint();
        bs->flush(rect);
    }

    QBackingStore* bs;
    int x,y,w,h,f;
    Qt::MouseButtons b;
    void* buf;
    QList<char> queue;
    quint32 prev;
};

static PalScreen* ctx = 0;

extern "C" {
#include "ObxPalApi.h"

int PAL_open_screen(int width, int height, int format)
{
    if( ctx == 0 )
    {
        app = new QGuiApplication(argc,&argv);
        ctx = new PalScreen(width,height,format);
        return 1;
    }else
        return 0;
}

int PAL_process_event(int sleep, void* buffer)
{
    if( ctx == 0 )
        return 1;
    if( buffer == 0 )
        buffer = ctx->buf;
    const quint32 now = PAL_time();
    if( buffer && ( now - ctx->prev ) >= 30 )
    {
        ctx->buf = buffer;
        ctx->prev = now;
        switch( ctx->f)
        {
        case Mono:
            ctx->updateMono((quint32*)buffer);
            break;
        case Color8888:
            ctx->update8888((quint32*)buffer);
            break;
        }
    }
    QGuiApplication::processEvents();
    if( sleep )
        QGuiApplication::processEvents(QEventLoop::WaitForMoreEvents,sleep);
    if( quit )
    {
        delete ctx;
        ctx = 0;
        delete app;
        app = 0;
        PAL_dispose();
        return 1;
    }else
        return 0;
}

int PAL_next_key()
{
    if( ctx == 0 || ctx->queue.isEmpty() )
        return 0;
    return (quint8)ctx->queue.takeLast();
}

int PAL_pending_keys()
{
    if( ctx == 0 )
        return 0;
    else
        return ctx->queue.size();
}

int PAL_mouse_state(int* x, int* y, int* keys)
{
    PAL_process_event(0,0);
    if( ctx == 0 )
        return 0;
    *x = ctx->x;
    *y = ctx->h - ctx->y - 1;
    *keys = 0;
    if( ctx->b & Qt::LeftButton )
        *keys |= Left;
    if( ctx->b & Qt::MidButton )
        *keys |= Mid;
    if( ctx->b & Qt::RightButton )
        *keys |= Right;
    return 1;
}

}
