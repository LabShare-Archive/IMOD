SOURCES += processchunks.cpp processhandler.cpp machinehandler.cpp comfilejobs.cpp
HEADERS += processchunks.h processhandler.h machinehandler.h comfilejobs.h
TEMPLATE = app
CONFIG += qt
CONFIG += console
INCLUDEPATH += . ../include

include (qconfigure)

INSTALLS += target
