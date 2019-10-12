QT       -= core gui

TARGET = obnljlib
TEMPLATE = lib

DEFINES += OBNLJLIB_LIBRARY

INCLUDEPATH += .. ../LjTools/luajit-2.0

SOURCES += ObLjLib.cpp

HEADERS += ObLjLib.h

unix {
    target.path = /usr/lib
    INSTALLS += target
}
