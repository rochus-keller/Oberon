#-------------------------------------------------
#
# Project created by QtCreator 2022-01-04T01:43:59
# modified by Rochus till it worked
# result is libNAppGUI.so with no missing symbols
# note the res_assert.c/h files
#
#-------------------------------------------------

QT       -= core gui

TARGET = NAppGUI
TEMPLATE = lib

INCLUDEPATH += ../src/osbs ../src/sewer ../src/draw2d ../src/geom2d ../src/gui ../src/osgui ../src/core ../src/osapp 
DEFINES += NAPPGUI_LIBRARY CMAKE_RELEASE NAPPGUI_BUILD_DIR NAPPGUI_SOURCE_DIR NAPPGUI_BUILD
CONFIG += object_parallel_to_source #https://wiki.qt.io/Undocumented_QMake; if your version doesn't support, rename osbs/unix/sinfo.c and bsocket.c

SOURCES += \
    ../src/core/core.cpp \
    ../src/core/event.cpp \
    ../src/draw2d/draw2d.cpp \
    ../src/draw2d/drawg.cpp \
    ../src/geom2d/box2d.cpp \
    ../src/geom2d/cir2d.cpp \
    ../src/geom2d/col2d.cpp \
    ../src/geom2d/obb2d.cpp \
    ../src/geom2d/pol2d.cpp \
    ../src/geom2d/polabel.cpp \
    ../src/geom2d/polpart.cpp \
    ../src/geom2d/r2d.cpp \
    ../src/geom2d/s2d.cpp \
    ../src/geom2d/seg2d.cpp \
    ../src/geom2d/t2d.cpp \
    ../src/geom2d/tri2d.cpp \
    ../src/geom2d/v2d.cpp \
    ../src/gui/gui.cpp \
    ../src/osbs/osbs.cpp \
    ../src/osgui/osgui.cpp \
    ../src/sewer/bmath.cpp \
    ../src/sewer/sewer.cpp \
    ../src/core/array.c \
    ../src/core/bhash.c \
    ../src/core/buffer.c \
    ../src/core/clock.c \
    ../src/core/date.c \
    ../src/core/dbind.c \
    ../src/core/heap.c \
    ../src/core/hfile.c \
    ../src/core/keybuf.c \
    ../src/core/lex.c \
    ../src/core/nfa.c \
    ../src/core/obj.c \
    ../src/core/rbtree.c \
    ../src/core/regex.c \
    ../src/core/respack.c \
    ../src/core/stream.c \
    ../src/core/strings.c \
    ../src/core/tfilter.c \
    ../src/draw2d/btext.c \
    ../src/draw2d/color.c \
    ../src/draw2d/dctx.c \
    ../src/draw2d/font.c \
    ../src/draw2d/guicontext.c \
    ../src/draw2d/higram.c \
    ../src/draw2d/image.c \
    ../src/draw2d/imgutils.c \
    ../src/draw2d/palette.c \
    ../src/draw2d/pixbuf.c \
    ../src/gui/button.c \
    ../src/gui/combo.c \
    ../src/gui/component.c \
    ../src/gui/comwin.c \
    ../src/gui/drawctrl.c \
    ../src/gui/edit.c \
    ../src/gui/gbind.c \
    ../src/gui/globals.c \
    ../src/gui/imageview.c \
    ../src/gui/label.c \
    ../src/gui/layout.c \
    ../src/gui/listbox.c \
    ../src/gui/menu.c \
    ../src/gui/menuitem.c \
    ../src/gui/panel.c \
    ../src/gui/popup.c \
    ../src/gui/progress.c \
    ../src/gui/slider.c \
    ../src/gui/splitview.c \
    ../src/gui/tableview.c \
    ../src/gui/textview.c \
    ../src/gui/updown.c \
    ../src/gui/view.c \
    ../src/gui/window.c \
    ../src/osapp/osapp.c \
    ../src/osbs/log.c \
    ../src/osbs/bsocket.c \
    ../src/osgui/osguictx.c \
    ../src/sewer/blib.c \
    ../src/sewer/bmem.c \
    ../src/sewer/cassert.c \
    ../src/sewer/ptr.c \
    ../src/sewer/qsort.c \
    ../src/sewer/types.c \
    ../src/sewer/unicode.c \
	res_assert.c

HEADERS +=  \
    ../src/core/array.h \
    ../src/core/arrpt.h \
    ../src/core/arrpt.hpp \
    ../src/core/arrst.h \
    ../src/core/arrst.hpp \
    ../src/core/bhash.h \
    ../src/core/buffer.h \
    ../src/core/clock.h \
    ../src/core/core.h \
    ../src/core/date.h \
    ../src/core/dbind.h \
    ../src/core/event.h \
    ../src/core/heap.h \
    ../src/core/hfile.h \
    ../src/core/keybuf.h \
    ../src/core/rbtree.h \
    ../src/core/regex.h \
    ../src/core/respack.h \
    ../src/core/respackh.h \
    ../src/core/setpt.h \
    ../src/core/setpt.hpp \
    ../src/core/setst.h \
    ../src/core/setst.hpp \
    ../src/core/stream.h \
    ../src/core/strings.h \
    ../src/draw2d/color.h \
    ../src/draw2d/dctx.h \
    ../src/draw2d/draw.h \
    ../src/draw2d/draw2d.h \
    ../src/draw2d/drawg.h \
    ../src/draw2d/drawg.hpp \
    ../src/draw2d/font.h \
    ../src/draw2d/image.h \
    ../src/draw2d/palette.h \
    ../src/draw2d/pixbuf.h \
    ../src/geom2d/box2d.h \
    ../src/geom2d/box2d.hpp \
    ../src/geom2d/cir2d.h \
    ../src/geom2d/cir2d.hpp \
    ../src/geom2d/col2d.h \
    ../src/geom2d/col2d.hpp \
    ../src/geom2d/obb2d.h \
    ../src/geom2d/obb2d.hpp \
    ../src/geom2d/pol2d.h \
    ../src/geom2d/pol2d.hpp \
    ../src/geom2d/r2d.h \
    ../src/geom2d/r2d.hpp \
    ../src/geom2d/s2d.h \
    ../src/geom2d/s2d.hpp \
    ../src/geom2d/seg2d.h \
    ../src/geom2d/seg2d.hpp \
    ../src/geom2d/t2d.h \
    ../src/geom2d/t2d.hpp \
    ../src/geom2d/tri2d.h \
    ../src/geom2d/tri2d.hpp \
    ../src/geom2d/v2d.h \
    ../src/geom2d/v2d.hpp \
    ../src/gui/button.h \
    ../src/gui/cell.h \
    ../src/gui/combo.h \
    ../src/gui/comwin.h \
    ../src/gui/edit.h \
    ../src/gui/globals.h \
    ../src/gui/gui.h \
    ../src/gui/imageview.h \
    ../src/gui/label.h \
    ../src/gui/layout.h \
    ../src/gui/listbox.h \
    ../src/gui/menu.h \
    ../src/gui/menuitem.h \
    ../src/gui/panel.h \
    ../src/gui/popup.h \
    ../src/gui/progress.h \
    ../src/gui/slider.h \
    ../src/gui/splitview.h \
    ../src/gui/tableview.h \
    ../src/gui/textview.h \
    ../src/gui/updown.h \
    ../src/gui/view.h \
    ../src/gui/window.h \
    ../src/osapp/nappgui.h \
    ../src/osapp/osapp.h \
    ../src/osapp/osmain.h \
    ../src/osbs/bfile.h \
    ../src/osbs/bmutex.h \
    ../src/osbs/bproc.h \
    ../src/osbs/bthread.h \
    ../src/osbs/btime.h \
    ../src/osbs/log.h \
    ../src/osbs/osbs.h \
    ../src/osgui/osbutton.h \
    ../src/osgui/oscombo.h \
    ../src/osgui/oscomwin.h \
    ../src/osgui/osdrawctrl.h \
    ../src/osgui/osedit.h \
    ../src/osgui/osglobals.h \
    ../src/osgui/osgui.h \
    ../src/osgui/osguictx.h \
    ../src/osgui/oslabel.h \
    ../src/osgui/osmenu.h \
    ../src/osgui/osmenuitem.h \
    ../src/osgui/ospanel.h \
    ../src/osgui/ospopup.h \
    ../src/osgui/osprogress.h \
    ../src/osgui/osslider.h \
    ../src/osgui/ossplit.h \
    ../src/osgui/ostext.h \
    ../src/osgui/osupdown.h \
    ../src/osgui/osview.h \
    ../src/osgui/oswindow.h \
    ../src/sewer/bmath.h \
    ../src/sewer/bmath.hpp \
    ../src/sewer/bmem.h \
    ../src/sewer/bstd.h \
    ../src/sewer/cassert.h \
    ../src/sewer/ptr.h \
    ../src/sewer/sewer.h \
    ../src/sewer/types.h \
	../src/sewer/unicode.h

linux {
INCLUDEPATH += ../src/osgui/gtk3 ../src/draw2d/gtk3
INCLUDEPATH += /usr/include/cairo /usr/include/pango-1.0 /usr/include/glib-2.0 /usr/lib/i386-linux-gnu/glib-2.0/include \
			   /usr/include/gdk-pixbuf-2.0 /usr/include/gtk-3.0 /usr/include/atk-1.0
DEFINES += __GTK3__
LIBS += -L/usr/lib/i386-linux-gnu -lcairo -lpango-1.0 -lglib-2.0 -lgdk_pixbuf-2.0 -lgtk-3 -latk-1.0
SOURCES += \
    ../src/osbs/linux/sinfo.c \
    ../src/osbs/unix/bfile.c \
    ../src/osbs/unix/bmutex.c \
    ../src/osbs/unix/bproc.c \
    ../src/osbs/unix/bthread.c \
    ../src/osbs/unix/btime.c \
    ../src/draw2d/gtk3/dctx_gtk.c \
    ../src/draw2d/gtk3/draw2d_gtk.c \
    ../src/draw2d/gtk3/osfont.c \
    ../src/draw2d/gtk3/osimage.c \
    ../src/osapp/gtk3/osapp_gtk.c \
    ../src/osgui/gtk3/osbutton.c \
    ../src/osgui/gtk3/oscombo.c \
    ../src/osgui/gtk3/oscomwin.c \
    ../src/osgui/gtk3/oscontrol.c \
    ../src/osgui/gtk3/osdrawctrl.c \
    ../src/osgui/gtk3/osedit.c \
    ../src/osgui/gtk3/osglobals.c \
    ../src/osgui/gtk3/osgui_gtk.c \
    ../src/osgui/gtk3/oslabel.c \
    ../src/osgui/gtk3/oslistener.c \
    ../src/osgui/gtk3/osmenu.c \
    ../src/osgui/gtk3/osmenuitem.c \
    ../src/osgui/gtk3/ospanel.c \
    ../src/osgui/gtk3/ospopup.c \
    ../src/osgui/gtk3/osprogress.c \
    ../src/osgui/gtk3/osslider.c \
    ../src/osgui/gtk3/ossplit.c \
    ../src/osgui/gtk3/ostext.c \
    ../src/osgui/gtk3/osupdown.c \
    ../src/osgui/gtk3/osview.c \
    ../src/osgui/gtk3/oswindow.c \
    ../src/sewer/unix/bstdimp.c \
    ../src/sewer/unix/bmem_unix.c \
    ../src/osbs/unix/bsocket.c \
    ../src/osbs/unix/sinfo.c
HEADERS += ../src/osapp/osmain_gtk.h
}

win32 {
DEFINES += _WINDOWS
HEADERS += ../src/osapp/osmain_win.h 
SOURCES += \
    ../src/draw2d/win/dctx_win.cpp \
    ../src/draw2d/win/draw2d_win.cpp \
    ../src/draw2d/win/osfont.cpp \
    ../src/draw2d/win/osimage.cpp \
    ../src/osgui/win/oscontrol.cpp \
    ../src/osgui/win/osdrawctrl.cpp \
    ../src/osgui/win/osgui_win.cpp \
    ../src/osgui/win/osview.cpp \
    ../src/osapp/win/osapp_win.c \
    ../src/osbs/win/bfile.c \
    ../src/osbs/win/bmutex.c \
    ../src/osbs/win/bproc.c \
    ../src/osbs/win/bsocket.c \
    ../src/osbs/win/bthread.c \
    ../src/osbs/win/btime.c \
    ../src/osbs/win/sinfo.c \
    ../src/osgui/win/osbutton.c \
    ../src/osgui/win/oscombo.c \
    ../src/osgui/win/oscomwin.c \
    ../src/osgui/win/osedit.c \
    ../src/osgui/win/osglobals.c \
    ../src/osgui/win/osimglist.c \
    ../src/osgui/win/oslabel.c \
    ../src/osgui/win/oslistener.c \
    ../src/osgui/win/osmenu.c \
    ../src/osgui/win/osmenuitem.c \
    ../src/osgui/win/ospanel.c \
    ../src/osgui/win/ospopup.c \
    ../src/osgui/win/osprogress.c \
    ../src/osgui/win/osscroll.c \
    ../src/osgui/win/osslider.c \
    ../src/osgui/win/ossplit.c \
    ../src/osgui/win/osstyleXP.c \
    ../src/osgui/win/ostext.c \
    ../src/osgui/win/ostooltip.c \
    ../src/osgui/win/osupdown.c \
    ../src/osgui/win/oswindow.c \
    ../src/sewer/win/bmem_win.c \
    ../src/sewer/win/bstdimp.c
LIBS += -lGdi32 -lUser32 -lShell32 -lComdlg32
QMAKE_LFLAGS += "/DEF:$$PWD/NAppGUI.def"
#NOTE: /Gd i.e. __cdecl is default for MSVC CL
#DEF_FILE = 
}

macx {
SOURCES += \
    ../src/osbs/unix/bfile.c \
    ../src/osbs/unix/bmutex.c \
    ../src/osbs/unix/bproc.c \
    ../src/osbs/unix/bthread.c \
    ../src/osbs/unix/btime.c \
    ../src/osbs/unix/bsocket.c \
	../src/osbs/unix/sinfo.c \
	../src/sewer/unix/bmem_unix.c \
	../src/sewer/unix/bstdimp.c \
	../src/osbs/osx/sinfo.m \
	../src/draw2d/osx/dctx_osx.m \
	../src/draw2d/osx/draw2d_osx.m \
	../src/draw2d/osx/osfont.m \
	../src/draw2d/osx/osimage.m \
	../src/osapp/osx/osapp_osx.m \
	../src/osgui/osx/osbutton.m \
	../src/osgui/osx/oscolor.m \
	../src/osgui/osx/oscombo.m \
	../src/osgui/osx/oscomwin.m \
	../src/osgui/osx/oscontrol.m \
	../src/osgui/osx/osedit.m \
	../src/osgui/osx/osglobals.m \
	../src/osgui/osx/osgui_osx.m \
	../src/osgui/osx/oslabel.m \
	../src/osgui/osx/oslistener.m \
	../src/osgui/osx/ospanel.m \
	../src/osgui/osx/ospopup.m \
	../src/osgui/osx/osprogress.m \
	../src/osgui/osx/osslider.m \
	../src/osgui/osx/ostext.m \
	../src/osgui/osx/osupdown.m \
	../src/osgui/osx/osview.m \
	../src/osgui/osx/osdrawctrl.m \
	../src/osgui/osx/osmenu.m \
	../src/osgui/osx/osmenuitem.m \
	../src/osgui/osx/ossplit.m \
	../src/osgui/osx/oswindow.m
HEADERS += ../src/osapp/osmain_osx.h
LIBS += -framework CoreFoundation -framework CoreGraphics -framework CoreText -framework AppKit
}


