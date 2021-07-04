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

QT       += core gui

greaterThan(QT_MAJOR_VERSION, 4): QT += widgets printsupport

TARGET = ObxIDE
TEMPLATE = app

INCLUDEPATH += .. ../LuaJIT/src

#DEFINES += LUA_USE_ASSERT
DEFINES += OBX_BBOX LUAIDE_EMBEDDED _HAS_GENERICS #_INSERT_DGBTRACE

SOURCES += ObxIde.cpp \
    ../GuiTools/CodeEditor.cpp \
    ../LjTools/LuaJitBytecode.cpp \
    ../LjTools/Engine2.cpp \
    ../LjTools/Terminal2.cpp \
    ../LjTools/ExpressionParser.cpp \
    ../LjTools/LuaJitEngine.cpp \
    ../LjTools/LuaJitComposer.cpp \
    ObnHighlighter.cpp \
    ../LjTools/LjDisasm.cpp \
    ../LjTools/BcViewer2.cpp \
    ../GuiTools/DocSelector.cpp \
    ../GuiTools/DocTabWidget.cpp \
    ../LjTools/BcViewer.cpp \
    ObxProject.cpp \
    ObxModel.cpp \
    ObxParser.cpp \
    ObxValidator.cpp \
    ObxEvaluator.cpp \
    ObxAst.cpp \
    ObxLjbcGen.cpp \
    ObxLibFfi.cpp \
    ../LjTools/LjBcDebugger.cpp \
    ObsDisplay.cpp \
    ObsFiles.cpp

HEADERS  += ObxIde.h \
    ../GuiTools/CodeEditor.h \
    ../LjTools/LuaJitBytecode.h \
    ../LjTools/Engine2.h \
    ../LjTools/Terminal2.h \
    ../LjTools/ExpressionParser.h \
    ../LjTools/LuaJitEngine.h \
    ../LjTools/LuaJitComposer.h \
    ObnHighlighter.h \
    ../LjTools/LjDisasm.h \
    ../LjTools/BcViewer2.h \
    ../GuiTools/DocSelector.h \
    ../GuiTools/DocTabWidget.h \
    ../LjTools/BcViewer.h \
    ObxProject.h \
    ObxModel.h \
    ObxParser.h \
    ObxValidator.h \
    ObxEvaluator.h \
    ObxAst.h \
    ObxLjbcGen.h \
    ObxLibFfi.h \
    ../LjTools/LjBcDebugger.h \
    ObsDisplay.h

# NOTE on LuaJIT: to make use of the line:column position information used in
# OberonIDE please use this version: https://github.com/rochus-keller/LuaJIT/tree/LjTools
# LuaJIT is separately built using make on Linux, "make clean && make -j4" on Mac and msvcbuild.bat
# on Windows; even if the Qt project includes LuaJIT by source the original build has to be run so 
# all required files are generated.

win32 {
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

include( Oberon.pri )
include( ../GuiTools/Menu.pri )

CONFIG(debug, debug|release) {
        DEFINES += _DEBUG
}

!win32 {
    QMAKE_CXXFLAGS += -Wno-reorder -Wno-unused-parameter -Wno-unused-function -Wno-unused-variable
}

RESOURCES += \
    ObxIde.qrc

