#/*
#* Copyright 2019, 2020, 2021 Rochus Keller <mailto:me@rochus-keller.ch>
#*
#* This file is part of the Oberon+ parser/code model library.
#*
#* The following is the license that applies to this copy of the
#* library. For a license to use the library under conditions
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

INCLUDEPATH +=  ..

DEFINES += _HAS_GENERICS OBX_BBOX

SOURCES += \
    $$PWD/ObToken.cpp \
    $$PWD/ObLexer.cpp \
    $$PWD/ObFileCache.cpp \
    $$PWD/ObErrors.cpp \
    $$PWD/ObRowCol.cpp \
    $$PWD/ObxValidator.cpp \
    $$PWD/ObxProject.cpp \
    $$PWD/ObxParser.cpp \
    $$PWD/ObxPackage.cpp \
    $$PWD/ObxModel.cpp \
    $$PWD/ObxEvaluator.cpp \
    $$PWD/ObxAst.cpp \
    $$PWD/ObTokenType.cpp

HEADERS  += \
    $$PWD/ObToken.h \
    $$PWD/ObLexer.h \
    $$PWD/ObFileCache.h \
    $$PWD/ObErrors.h \
    $$PWD/ObRowCol.h \
    $$PWD/ObxValidator.h \
    $$PWD/ObxProject.h \
    $$PWD/ObxParser.h \
    $$PWD/ObxPackage.h \
    $$PWD/ObxModel.h \
    $$PWD/ObxEvaluator.h \
    $$PWD/ObxAst.h \
    $$PWD/ObTokenType.h

