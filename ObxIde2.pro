#/*
#* Copyright 2021 Rochus Keller <mailto:me@rochus-keller.ch>
#*
#* This file is part of the Oberon+ IDE application.
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

QT       += core gui network

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets printsupport # it is safe to remove printsupport

TARGET = ObxIDE
TEMPLATE = app

INCLUDEPATH += .. 

DEFINES += _MONO_ENGINE_EXT_ _OBX_USE_NEW_FFI_

SOURCES += ObxIde2.cpp \
    ../GuiTools/CodeEditor.cpp \
    ObnHighlighter.cpp \
    ../GuiTools/DocSelector.cpp \
    ../GuiTools/DocTabWidget.cpp \
    ObxIlEmitter.cpp \
    ObxPelibGen.cpp \
    ObxCilGen.cpp \
    ../MonoTools/MonoEngine.cpp \
    ../MonoTools/MonoDebugger.cpp \
    ../MonoTools/MonoIlView.cpp \
    ../MonoTools/MonoMdbGen.cpp \
    ObxCGen2.cpp


HEADERS  += ObxIde2.h \
    ../GuiTools/CodeEditor.h \
    ObnHighlighter.h \
    ../GuiTools/DocSelector.h \
    ../GuiTools/DocTabWidget.h \
    ObxIlEmitter.h \
    ObxPelibGen.h \
    ObxCilGen.h \
    ../MonoTools/MonoEngine.h \
    ../MonoTools/MonoDebugger.h \
    ../MonoTools/MonoDebuggerPrivate.h \
    ../MonoTools/MonoIlView.h \
    ../MonoTools/MonoMdbGen.h \
    ObxCGen2.h


include( ObxParser.pri )
include( ../GuiTools/Menu.pri )
include( ../PeLib/PeLib.pri )

CONFIG(debug, debug|release) {
        DEFINES += _DEBUG
}

!win32 {
    QMAKE_CXXFLAGS += -Wno-reorder -Wno-unused-parameter -Wno-unused-function -Wno-unused-variable
}

RESOURCES += \
    ObxIde2.qrc

RC_ICONS = images/Oberon.ico

CONFIG(static) {
    message(static build)
    RESOURCES += Font2.qrc
    LIBS += -lQt5Network
} else {
    message(dynamic build)
}

