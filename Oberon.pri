#/*
#* Copyright 2019 Rochus Keller <mailto:me@rochus-keller.ch>
#*
#* This file is part of the Oberon parser/code model library.
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

SOURCES += \
    $$PWD/ObSynTree.cpp \
    $$PWD/ObTokenType.cpp \
    $$PWD/ObParser.cpp \
    $$PWD/ObToken.cpp \
    $$PWD/ObLexer.cpp \
    $$PWD/ObFileCache.cpp \
    $$PWD/ObErrors.cpp \
    $$PWD/ObCodeModel.cpp \
    $$PWD/ObAst.cpp \
    $$PWD/ObAstEval.cpp \
    $$PWD/ObAstValidator.cpp

HEADERS  += \
    $$PWD/ObSynTree.h \
    $$PWD/ObTokenType.h \
    $$PWD/ObParser.h \
    $$PWD/ObToken.h \
    $$PWD/ObLexer.h \
    $$PWD/ObFileCache.h \
    $$PWD/ObErrors.h \
    $$PWD/ObCodeModel.h \
    $$PWD/ObAst.h \
    $$PWD/ObAstEval.h \
    $$PWD/ObAstValidator.h

