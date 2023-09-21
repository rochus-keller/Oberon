#-------------------------------------------------
#
# Project created by QtCreator 2023-09-20T11:44:44
#
#-------------------------------------------------

QT += core gui
TARGET = Pal
TEMPLATE = lib

DEFINES += PAL_LIBRARY

SOURCES += Pal.cpp \
    File.cpp \
    Display.cpp

HEADERS += Pal.h \
    ObxPalApi.h

unix {
    target.path = /usr/lib
    INSTALLS += target
}
