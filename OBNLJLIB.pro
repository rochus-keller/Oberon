QT       -= core gui

TARGET = obnljlib
TEMPLATE = lib

DEFINES += OBNLJLIB_LIBRARY

INCLUDEPATH += .. ../LuaJIT/src

SOURCES += ObLjLib.cpp

HEADERS += ObLjLib.h

unix {
    target.path = /usr/lib
    INSTALLS += target
}
