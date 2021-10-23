#-------------------------------------------------
#
# Project created by QtCreator 2021-10-15T03:19:03
#
#-------------------------------------------------

QT       -= gui

TARGET = Test
TEMPLATE = lib

DEFINES += TEST_LIBRARY

SOURCES += Test.cpp

HEADERS += Test.h\
        test_global.h

unix {
    target.path = /usr/lib
    INSTALLS += target
}
