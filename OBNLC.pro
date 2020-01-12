#/*
#* Copyright 2019 Rochus Keller <mailto:me@rochus-keller.ch>
#*
#* This file is part of the Oberon to Lua compiler.
#*
#* The following is the license that applies to this copy of the
#* application. For a license to use the application under conditions
#* other than those described here, please email to me@rochus-keller.ch.
#*
#* GNU General Public License Usage
#* This file may be used under the terms of the GNU General Public
#* License (GPL) versions 2.0 or 3.0 as published by the Free Software
#* Foundation and appearing in the file LICENSE.GPL included in
#* the packaging of this file. Please review the following information
#* to ensure GNU General Public Licensing requirements will be met:
#* http://www.fsf.org/licensing/licenses/info/GPLv2.html and
#* http://www.gnu.org/copyleft/gpl.html.
#*/

QT       += core

QT       -= gui

TARGET = OBNLC
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app


SOURCES += \
    ObnlcMain.cpp \
    ObLuaGen.cpp \
    ObLuaGen2.cpp \
    ObLjbcGen.cpp \
    ../LjTools/LuaJitComposer.cpp \
    ../LjTools/LuaJitBytecode.cpp

include( Oberon.pri )

HEADERS += \
    ObLuaGen.h \
    ObLuaGen2.h \
    ObLjbcGen.h \
    ../LjTools/LuaJitComposer.h \
    ../LjTools/LuaJitBytecode.h

!win32 {
    QMAKE_CXXFLAGS += -Wno-reorder -Wno-unused-parameter -Wno-unused-function -Wno-unused-variable
}

CONFIG(debug, debug|release) {
        DEFINES += _DEBUG
}

CONFIG   += HAVE_LUAJIT #TODO
HAVE_LUAJIT {
    message( Using LuaJIT )

    INCLUDEPATH += .. ../LjTools/luajit-2.0
    DEFINES += OBNLC_USING_LUAJIT LUA_ENGINE_USE_DEFAULT_PRINT

    include( /home/me/Desktop/LuaJIT-2.0.5/src/LuaJit.pri ){
        LIBS += -ldl
    } else {
        LIBS += -lluajit
    }
    SOURCES += \
        ../LjTools/Engine2.cpp \
        ObLjLib.cpp

    HEADERS  += \
        ../LjTools/Engine2.h \
        ObLjLib.h

}

RESOURCES += \
    ObnLjEditor.qrc
