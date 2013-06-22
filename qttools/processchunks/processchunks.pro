SOURCES += processchunks.cpp machinehandler.cpp processhandler.cpp comfilejobs.cpp
HEADERS += processchunks.h machinehandler.h processhandler.h comfilejobs.h
TEMPLATE = app
CONFIG += qt
CONFIG += console
INCLUDEPATH += . ../../include

include (qconfigure)

INSTALLS += target
