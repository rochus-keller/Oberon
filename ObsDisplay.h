#ifndef OBSDISPLAY_H
#define OBSDISPLAY_H

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

#include <QWidget>
#include <QDateTime>
#include <bitset>

typedef struct lua_State lua_State;

namespace Obs
{
    class Display : public QWidget
    {
        Q_OBJECT
    public:
        typedef std::bitset<32> ObSet;
        enum { Width = 1024, Height = 768 };
        // enum { Width = 1000, Height = 600 };

        QImage d_img;
        int d_xOb, d_yOb;
        ObSet d_keys;

        QDateTime start;
        int clock;

        int mouseHandler;
        int charHandler;
        int idleHandler;

        static Display* inst();
        static bool isOpen();
        static void install(lua_State *L);

        static int mapToQt(int yOb);
        static int mapToOb(int yQt);

        typedef void (*IdleHandler)();

    protected:
        void paintEvent(QPaintEvent *);
        void timerEvent(QTimerEvent *);
        void mouseMoveEvent(QMouseEvent *);
        void mousePressEvent(QMouseEvent *);
        void mouseReleaseEvent(QMouseEvent *);
        void keyPressEvent(QKeyEvent *);
        void keyReleaseEvent(QKeyEvent *);
        bool mapOb( QMouseEvent* );
        void dispatchMouse(const ObSet& keys, int x, int y);
    private:
        explicit Display();
        ~Display();
    };
}

#endif // OBSDISPLAY_H
