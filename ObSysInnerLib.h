#ifndef OBSYSINNERLIB_H
#define OBSYSINNERLIB_H

/*
* Copyright 2019 Rochus Keller <mailto:me@rochus-keller.ch>
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

#include <QBuffer>
#include <QWidget>
#include <Oberon/ObLjLib.h>
#include <QDateTime>

namespace Ob
{
    class QtDisplay : public QWidget
    {
        Q_OBJECT
    public:
        enum { Width = 1024, Height = 768 };
        // enum { Width = 1000, Height = 600 };

        QImage d_img;
        int d_xOb, d_yOb;
        Ob::_Set d_keys;
        int d_lock;

        QByteArray arrow;
        QByteArray star;
        QByteArray hook;
        QByteArray updown;
        QByteArray block;

        QDateTime start;
        int clock;

        int mouseHandler;
        int charHandler;
        int idleHandler;

        static QtDisplay* inst();

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
        void dispatchMouse(const Ob::_Set& keys, int x, int y);
    private:
        explicit QtDisplay();
        ~QtDisplay();
    };

    struct FileDesc;
    typedef FileDesc* File;
    struct Rider {

        // public:
        bool eof;
        int res;

        // c++ implementation:
        int d_fileRef; // luaL_ref value
        File d_file;
        int d_pos;

        Rider():d_file(0),d_pos(0){}

        static void install(lua_State* L);
        static int _new(lua_State* L);
        static Rider* check(lua_State *L, int narg = 1 );
        static int _gc(lua_State* L);
        static int _index(lua_State* L);

        static int Set(lua_State* L);
        static int Pos(lua_State* L);
        static int Base(lua_State* L);
        static int ReadByte(lua_State* L);
        void ReadByte(quint8& x);
        static int Read(lua_State* L);
        static int ReadInt(lua_State* L);
        static int ReadString(lua_State* L);
        void WriteByte(quint8 x);
        static int WriteByte(lua_State* L);
        static int Write(lua_State* L);
        static int WriteInt(lua_State* L);
        static int WriteString(lua_State* L);
        static int RestoreList(lua_State* L);
  };
    struct FileDesc {

        // c++ implementation:
        QByteArray d_name;
        QBuffer d_buf;

        static void install(lua_State* L);
        static FileDesc* create(lua_State* L);
        static int _new(lua_State* L);
        static FileDesc* check(lua_State *L, int narg = 1, bool nilAllowed = false );
        static int _gc(lua_State* L);

        static int Old(lua_State* L);
        static int New(lua_State* L);
        static int Register(lua_State* L);
        static int Delete(lua_State* L);
        static int Rename(lua_State* L);
        static int Length(lua_State* L);
    };

    // Files module
    // Attr fileSystemPath

    class SysInnerLib
    {
    public:
        static void install(lua_State *L);
        static void quit();
    protected:
        static int installFiles(lua_State *L);
        static int installFileDir(lua_State *L);
        static int installKernel(lua_State *L);
        static int installDisplay(lua_State *L);
        static int installModules(lua_State *L);
        static int installInput(lua_State *L);
    private:
        SysInnerLib();
    };
}

#endif // OBSYSINNERLIB_H
