SOURCES += imodqtassist.cpp imod_assistant.cpp
HEADERS += imodqtassist.h imod_assistant.h
TEMPLATE = app
CONFIG += qt assistant
INCLUDEPATH += . ../include

include (qconfigure)

INSTALLS += target

iacpptarget.target = imod_assistant.cpp
iacpptarget.depends = ../imod/imod_assistant.cpp
iacpptarget.commands = \cp ../imod/imod_assistant.cpp .

iahtarget.target = imod_assistant.h
iahtarget.depends = ../imod/imod_assistant.h
iahtarget.commands = \cp ../imod/imod_assistant.h .

QMAKE_EXTRA_UNIX_TARGETS += iacpptarget iahtarget
