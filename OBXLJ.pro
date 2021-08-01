#/*
#* Copyright 2019, 2020, 2021 Rochus Keller <mailto:me@rochus-keller.ch>
#*
#* This file is part of the Oberon+ to LuaJIT compiler.
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

CONFIG += HAVE_GUI

HAVE_GUI {
    QT += gui widgets
    greaterThan(QT_MAJOR_VERSION, 4): QT += widgets
}

TARGET = OBXLJ
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

DEFINES += OBX_BBOX

include( ObxParser.pri )

DEFINES += LUA_ENGINE_USE_DEFAULT_PRINT
#DEFINES += _LJTOOLS_DONT_CREATE_TAIL_CALLS

SOURCES += \
    ObxLjMain.cpp \
    ../LjTools/LuaJitComposer.cpp \
    ../LjTools/LuaJitBytecode.cpp \
    ../LjTools/Engine2.cpp \
    ObxLjbcGen.cpp \
    ObxLibFfi.cpp \
    ObxLjRuntime.cpp \
    ObxCGen.cpp

HEADERS += \
    ../LjTools/LuaJitComposer.h \
    ../LjTools/LuaJitBytecode.h \
    ../LjTools/Engine2.h \
    ObxLjbcGen.h \
    ObxLibFfi.h \
    ObxLjRuntime.h \
    ObxCGen.h

HAVE_GUI {
    message( Compiling with GUI support )

    HEADERS += \
        ObsDisplay.h

    SOURCES += \
        ObsDisplay.cpp \
        ObsFiles.cpp

}

!win32 {
    QMAKE_CXXFLAGS += -Wno-reorder -Wno-unused-parameter -Wno-unused-function -Wno-unused-variable
}

CONFIG(debug, debug|release) {
        DEFINES += _DEBUG
}

win32 {
    INCLUDEPATH += .. ../LuaJIT/src
    LIBS += -L../LuaJIT/src -llua51
}

linux {
    include( ../LuaJIT/src/LuaJit.pri ){
        LIBS += -ldl
    } else {
        LIBS += -lluajit
    }

    QMAKE_LFLAGS += -rdynamic -ldl
    #rdynamic is required so that the LjLibFfi functions are visible to LuaJIT FFI
}
macx {
    include( ../LuaJIT/src/LuaJit.pri )
    QMAKE_LFLAGS += -rdynamic -ldl -pagezero_size 10000 -image_base 100000000
}


RESOURCES += \
    OBXLJ.qrc
