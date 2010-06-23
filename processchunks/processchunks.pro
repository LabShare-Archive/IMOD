SOURCES += processchunks.cpp processhandler.cpp
HEADERS += processchunks.h processhandler.h
TEMPLATE = app
CONFIG += qt
INCLUDEPATH += . ../include

include (qconfigure)

INSTALLS += target
