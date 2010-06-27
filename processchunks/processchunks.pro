SOURCES += processchunks.cpp processhandler.cpp machinehandler.cpp
HEADERS += processchunks.h processhandler.h machinehandler.h
TEMPLATE = app
CONFIG += qt
INCLUDEPATH += . ../include

include (qconfigure)

INSTALLS += target
