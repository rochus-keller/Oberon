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
#include <QThread>
#include <private/qhighdpiscaling_p.h>

static int argc = 1;
static char * argv = "PAL";
static QGuiApplication* app = 0;
static bool quit = false;

class PalScreen : public QWindow
{
public:
    PalScreen(int width, int height, int format, const char* title):
        x(0),y(0), w(width),h(height),f(format),buf(0), left(false), middle(false), right(false)
    {
        const qreal factor = QHighDpiScaling::factor(this);
        smooth = factor - qreal(qFloor(factor)) != 0.0;

        bs = new QBackingStore(this);
        const QSize s(w,h);
        resize(s);
        bs->resize(s);
        setCursor(Qt::BlankCursor);
        setTitle(QString::fromLatin1(title));
        show();

        startTimer(format == 0 ? 30: 20); // Mono format always updates the whole screen, thus slower
    }

    void keyPressEvent(QKeyEvent * ev)
    {
        // see also https://en.wikibooks.org/wiki/Oberon/ETH_Oberon/keyboard
        const QString key = ev->text();
        if( !key.isEmpty() && key[0].unicode() <= 127 ) // only ascii
        {
            quint8 ch = key[0].unicode();
            if( ev->modifiers() == Qt::AltModifier )
            {
                switch( ch )
                {
                case 'a':
                    ch = 131; // ä
                    break;
                case 'o':
                    ch = 132; // ö
                    break;
                case 'u':
                    ch = 133; // ü
                    break;
                case 'e':
                    ch = 145; // ë
                    break;
                case 'i':
                    ch = 146; // ï
                    break;
                case 'c':
                    ch = 147; // ç
                    break;
                case 'n':
                    ch = 149; // ñ
                    break;
                case 's':
                    ch = 150; // ß
                    break;
                }
            }else if( ev->modifiers() == ( Qt::AltModifier | Qt::ShiftModifier ) )
            {
                switch( ch )
                {
                case 'A':
                    ch = 128; // Ä
                    break;
                case 'O':
                    ch = 129; // Ô
                    break;
                case 'U':
                    ch = 130; // Û
                    break;
                }
            }
            queue.push_front(ch);
        }else if(!key.isEmpty())
        {
            quint8 ch = 0;
            switch( key[0].unicode() )
            {
            case 0xc4:
                ch = 128; // Ä
                break;
            case 0xd6:
                ch = 129; // Ô
                break;
            case 0xdc:
                ch = 130; // Û
                break;
            case 0xe4:
                ch = 131; // ä
                break;
            case 0xf6:
                ch = 132; // ö
                break;
            case 0xfc:
                ch = 133; // ü
                break;
            case 0xeb:
                ch = 145; // ë
                break;
            case 0xef:
                ch = 146; // ï
                break;
            case 0xe7:
                ch = 147; // ç
                break;
            case 0xf1:
                ch = 149; // ñ
                break;
            case 0xdf:
                ch = 150; // ß
                break;
            }
            if( ch )
                queue.push_front(ch);
        }else
        {
            if( ev->modifiers() == 0 || ev->modifiers() == Qt::KeypadModifier )
            {
                switch( ev->key() )
                {
                case Qt::Key_Left:
                    queue.push_front(196); // see System 3 TextFrames
                    break;
                case Qt::Key_Right:
                    queue.push_front(195);
                    break;
                case Qt::Key_Up:
                    queue.push_front(193); // see System 3 Tetris
                    break;
                case Qt::Key_Down:
                    queue.push_front(194);
                    break;
                case Qt::Key_Insert:
                    queue.push_front(160);
                    break;
                case Qt::Key_Delete:
                    queue.push_front(161);
                    break;
                case Qt::Key_PageUp:
                    queue.push_front(162);
                    break;
                case Qt::Key_PageDown:
                    queue.push_front(163);
                    break;
                case Qt::Key_F1:
                    queue.push_front(164);
                    break;
                case Qt::Key_F2:
                    queue.push_front(165);
                    break;
                case Qt::Key_F4:
                    queue.push_front(167);
                    break;
                case Qt::Key_Home:
                    queue.push_front(168);
                    break;
                case Qt::Key_End:
                    queue.push_front(169);
                    break;
                case Qt::Key_F5:
                    queue.push_front(245);
                    break;
                case Qt::Key_F6:
                    queue.push_front(246);
                    break;
                case Qt::Key_F7:
                    queue.push_front(247);
                    break;
                case Qt::Key_F8:
                    queue.push_front(248);
                    break;
                case Qt::Key_F9:
                    queue.push_front(249);
                    break;
                case Qt::Key_F10:
                    queue.push_front(250);
                    break;
                case Qt::Key_F11:
                    //queue.push_front(251);
                    if( windowState() & Qt::WindowFullScreen )
                        showNormal();
                    else
                        showFullScreen();
                    break;
                case Qt::Key_F12:
                    queue.push_front(252);
                    break;
                case Qt::Key_Escape:
                    queue.push_front(254);
                    break;
                }
            }
            else if( ev->modifiers() & Qt::ControlModifier && ev->key() == Qt::Key_Control)
                controlPress();
        }
    }

    void keyReleaseEvent(QKeyEvent * ev)
    {
        if( ev->key() == Qt::Key_Meta && right )
        {
            // this is to do a right to left interclick on mac
            right = middle = false;
            left = true; // right to middle interclick
        }
    }

    void mouseMoveEvent(QMouseEvent * ev)
    {
        setPos(ev->x(), ev->y());
        // setMouseButtons(ev->buttons(),ev->modifiers());
    }

    void mousePressEvent(QMouseEvent * ev)
    {
        setPos(ev->x(), ev->y());
        mousePress(ev->buttons(),ev->modifiers());
    }

    void mouseReleaseEvent(QMouseEvent *ev)
    {
        // setMouseButtons(ev->buttons(),ev->modifiers());
        left = middle = right = false;
    }

    void timerEvent(QTimerEvent *event)
    {
        if( !isExposed() )
            return;
        switch( f)
        {
        case 0: // Mono:
            updateMono();
            break;
        case 4: // Color8888:
            update8888();
            break;
        }
    }

    void exposeEvent(QExposeEvent *ev)
    {
        const QRect rect = ev->region().boundingRect();
        update( rect );
    }

    void resizeEvent(QResizeEvent *ev)
    {
        if( ev->oldSize().width() < ev->size().width() || ev->oldSize().height() < ev->size().height() )
            bs->resize(ev->size());
    }

    void setPos(int x_, int y_)
    {
        x = x_;
        y = y_;
        if( x < 0 )
            x = 0;
        if( y < 0 )
            y = 0;
        if( x >= w )
            x = w-1;
        if( y >= h )
            y = h-1;
    }

    void mousePress(Qt::MouseButtons b, Qt::KeyboardModifiers m)
    {
        left = b & Qt::LeftButton;
        middle = b & Qt::MidButton;
        right = b & Qt::RightButton;

        if( ( b & Qt::RightButton || b & Qt::LeftButton ) && m == Qt::ControlModifier )
        {
            left = right = false;
            middle = true;
        }
    }

    void controlPress()
    {
        if( left )
        {
            left = right = false;
            middle = true; // left to middle interclick
        }else if( right )
        {
            right = left = false;
            middle = true; // right to middle interclick
        }
    }

    void hideEvent(QHideEvent *)
    {
        quit = true;
    }

    void updateMono()
    {
        QImage img( w, h, QImage::Format_Mono );

        patches.clear();
        quint32* raster = (quint32*)buf;
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
        if( width() > w )
        {
            QRect rect( w, 0, width() - w + 1, height() );
            bs->beginPaint(rect);
            QPainter p(bs->paintDevice());
            p.fillRect(rect, Qt::black);
            bs->endPaint();
            bs->flush(rect);
        }
        if( height() > h )
        {
            QRect rect( 0, h, width(), height() - h + 1 );
            bs->beginPaint(rect);
            QPainter p(bs->paintDevice());
            p.fillRect(rect, Qt::black);
            bs->endPaint();
            bs->flush(rect);
        }

        QRect rect(0, 0, w, h);
        bs->beginPaint(rect);
        QPainter p(bs->paintDevice());
        p.drawImage(0,0,img);
        bs->endPaint();
        bs->flush(rect);
    }

    void update8888()
    {
        union
        {
            quint32 rgba;
            quint8 bytes[4];
        } point;
        quint32* raster = (quint32*)buf;

        if( patches.isEmpty() )
            return;

        QRegion reg;
        foreach( const QRect& r, patches )
            reg += r;
        patches.clear();

        const QRect bound = reg.boundingRect();

        if( bound.right() > w )
        {
            QRect rect( w, 0, width() - w + 1, height() );
            bs->beginPaint(rect);
            QPainter p(bs->paintDevice());
            p.fillRect(rect, Qt::black);
            bs->endPaint();
            bs->flush(rect);
        }

        if( bound.bottom() > h )
        {
            QRect rect( 0, h, width(), height() - h + 1 );
            bs->beginPaint(rect);
            QPainter p(bs->paintDevice());
            p.fillRect(rect, Qt::black);
            bs->endPaint();
            bs->flush(rect);
        }

        QRect patch = bound & QRect(0,0,w,h);
        if( patch.isEmpty() )
            return;

        // increase the path to left/up a pixel to acommodate roundoff error in SmoothPixmapTransform
        if( smooth )
            patch = patch.adjusted(-1,-1,1,1) & QRect(0,0,w,h);

        QImage img( patch.width(), patch.height(), QImage::Format_RGB888 );

        for( int y = 0; y < patch.height(); y++)
        {
            for( int x = 0; x < patch.width(); x++ )
            {
                point.rgba = raster[(y + patch.y()) * w + ( x + patch.x() )];
                img.setPixel(x,y, qRgb(point.bytes[2], point.bytes[1], point.bytes[0]) );
            }
        }

        bs->beginPaint(patch);
        QPainter p(bs->paintDevice());
        if( smooth )
            p.setRenderHint( QPainter::SmoothPixmapTransform, true); // affects QT_SCALE_FACTOR, renders smoother!
        p.drawImage(patch.x(),patch.y(),img);
        bs->endPaint();
        bs->flush(patch);
    }

    void update(const QRect& r )
    {
        patches.append(r);
    }

    QBackingStore* bs;
    int x,y,w,h,f;
    void* buf;
    QList<quint8> queue;
    QList<QRect> patches;
    bool left, middle, right; // current mouse button state
    bool smooth;
};

static PalScreen* ctx = 0;

extern "C" {
#include "ObxPalApi.h"

int PAL_open_screen(int width, int height, int format, const char* title, void* buffer)
{
    if( ctx == 0 )
    {
        app = new QGuiApplication(argc,&argv);
        ctx = new PalScreen(width,height,format,title);
        ctx->buf = buffer;
        return 1;
    }else
        return 0;
}

int PAL_process_events(int sleep)
{
    if( ctx == 0 )
        return 1;
    QGuiApplication::processEvents(QEventLoop::WaitForMoreEvents | QEventLoop::AllEvents);
    if( sleep )
        QThread::msleep(sleep);
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
    PAL_process_events(0);
    if( ctx == 0 || ctx->queue.isEmpty() )
        return 0;
    return ctx->queue.takeLast();
}

int PAL_pending_keys()
{
    PAL_process_events(0);
    if( ctx == 0 )
        return 0;
    else
        return ctx->queue.size();
}

int PAL_mouse_state(int* x, int* y, int* keys)
{
    PAL_process_events(0);
    if( ctx == 0 )
        return 0;
    *x = ctx->x;
    *y = ctx->h - ctx->y - 1;
    *keys = 0;
    if( ctx->left )
        *keys |= Left;
    if( ctx->middle )
        *keys |= Mid;
    if( ctx->right )
        *keys |= Right;
    return 1;
}

int PAL_modifier_state(int* modifiers)
{
    PAL_process_events(0);
    const Qt::KeyboardModifiers mods = QGuiApplication::keyboardModifiers();
    if( modifiers == 0 )
        return 0;
    *modifiers = 0;
    if( mods & Qt::ShiftModifier )
        *modifiers |= SHIFT;
    if( mods & Qt::AltModifier )
        *modifiers |= ALT;
    if( mods & Qt::ControlModifier )
        *modifiers |= CTRL;
    return 1;
}

int PAL_update( int x, int y, int w, int h)
{
    if( ctx == 0 )
        return 0;
    ctx->update(QRect(x,y,w,h));
    return 1;
}

}
