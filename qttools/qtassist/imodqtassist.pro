SOURCES += imodqtassist.cpp imod_assistant.cpp
HEADERS += imodqtassist.h imod_assistant.h
TEMPLATE = app
CONFIG += qt
INCLUDEPATH += . ../../include

include (qconfigure)

INSTALLS += target

iacpptarget.target = imod_assistant.cpp
iacpptarget.depends = ../../3dmod/imod_assistant.cpp
iacpptarget.commands = cp -f ../../3dmod/imod_assistant.cpp .

iahtarget.target = imod_assistant.h
iahtarget.depends = ../../3dmod/imod_assistant.h
iahtarget.commands = cp -f ../../3dmod/imod_assistant.h .

QMAKE_EXTRA_TARGETS += iacpptarget iahtarget
