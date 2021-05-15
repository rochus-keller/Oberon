#/*
#* Copyright 2021 Rochus Keller <mailto:me@rochus-keller.ch>
#*
#* This file is part of the Oberon to Mono CLI compiler.
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

TARGET = OBXMC
CONFIG   += console
CONFIG   -= app_bundle

TEMPLATE = app

DEFINES += OBX_BBOX

INCLUDEPATH += ..

SOURCES += \
    ObxMcMain.cpp \
    ObxParser.cpp \
    ObxAst.cpp \
    ObxValidator.cpp \
    ObxEvaluator.cpp \
    ObxModel.cpp \
    ObErrors.cpp \
    ObToken.cpp \
    ObLexer.cpp \
    ObFileCache.cpp \
    ObRowCol.cpp \
    ObTokenType.cpp \
    ObxPelibGen.cpp

HEADERS += \
    ObxParser.h \
    ObxAst.h \
    ObxValidator.h \
    ObxEvaluator.h \
    ObxModel.h \
    ObErrors.h \
    ObToken.h \
    ObLexer.h \
    ObFileCache.h \
    ObRowCol.h \
    ObTokenType.h \
    ObxPelibGen.h

include( ../PeLib/PeLib.pri )

!win32 {
    QMAKE_CXXFLAGS += -Wno-reorder -Wno-unused-parameter -Wno-unused-function -Wno-unused-variable
}

CONFIG(debug, debug|release) {
        DEFINES += _DEBUG
}

RESOURCES += \
    OBXMC.qrc

