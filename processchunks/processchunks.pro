SOURCES += processchunks.cpp processhandler.cpp machinehandler.cpp
HEADERS += processchunks.h processhandler.h machinehandler.h
TEMPLATE = app
CONFIG += qt
CONFIG += console
INCLUDEPATH += . ../include

include (qconfigure)

INSTALLS += target
