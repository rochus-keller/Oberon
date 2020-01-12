#/*
#* Copyright 2019 Rochus Keller <mailto:me@rochus-keller.ch>
#*
#* This file is part of the Oberon LuaJIT Viewer application.
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

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets printsupport

TARGET = ObnLjEditor
TEMPLATE = app

INCLUDEPATH += .. ../LjTools/luajit-2.0

SOURCES += ObnLjEditor.cpp \
    ../GuiTools/CodeEditor.cpp \
    ../LjTools/LuaJitBytecode.cpp \
    ../LjTools/Engine2.cpp \
    ../LjTools/Terminal2.cpp \
    ../LjTools/ExpressionParser.cpp \
    ../LjTools/BcViewer.cpp \
    ../LjTools/LuaJitEngine.cpp \
    ../LjTools/LuaJitComposer.cpp \
    ObnHighlighter.cpp \
    ObLuaGen.cpp \
    ObLjLib.cpp \
    ../LjTools/LjDisasm.cpp \
    ObLuaGen2.cpp \
    ObLjbcGen.cpp \
    ../LjTools/BcViewer2.cpp

HEADERS  += ObnLjEditor.h \
    ../GuiTools/CodeEditor.h \
    ../LjTools/LuaJitBytecode.h \
    ../LjTools/Engine2.h \
    ../LjTools/Terminal2.h \
    ../LjTools/ExpressionParser.h \
    ../LjTools/BcViewer.h \
    ../LjTools/LuaJitEngine.h \
    ../LjTools/LuaJitComposer.h \
    ObnHighlighter.h \
    ObLuaGen.h \
    ObLjLib.h \
    ../LjTools/LjDisasm.h \
    ObLuaGen2.h \
    ObLjbcGen.h \
    ../LjTools/BcViewer2.h

include( /home/me/Desktop/LuaJIT-2.0.5/src/LuaJit.pri ){
    LIBS += -ldl
} else {
    LIBS += -lluajit
}

include( Oberon.pri )
include( ../GuiTools/Menu.pri )

CONFIG(debug, debug|release) {
        DEFINES += _DEBUG
}

!win32 {
    QMAKE_CXXFLAGS += -Wno-reorder -Wno-unused-parameter -Wno-unused-function -Wno-unused-variable
}

RESOURCES += \
    ObnLjEditor.qrc
