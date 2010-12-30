SOURCES += processchunks.cpp processhandler.cpp machinehandler.cpp comfilejob.cpp
HEADERS += processchunks.h processhandler.h machinehandler.h comfilejob.h
TEMPLATE = app
CONFIG += qt
CONFIG += console
INCLUDEPATH += . ../include

include (qconfigure)

INSTALLS += target
