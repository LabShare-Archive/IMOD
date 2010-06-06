SOURCES += midas.cpp slots.cpp file_io.cpp graphics.cpp transforms.cpp \
        imod_assistant.cpp

HEADERS += midas.h graphics.h slots.h imod_assistant.h

#MOC_DIR = tmp
#OBJECTS_DIR = tmp

TEMPLATE = app

# 1/25/03: (Qt 3.0.5) when opengl is added, program dies with highcolor style
# one solution is to leave opengl off and specify -lGL in LIBS in qconfigure
CONFIG += qt opengl assistant
QT += opengl

include (qconfigure)

INSTALLS += target

iacpptarget.target = imod_assistant.cpp
iacpptarget.depends = ../imod/imod_assistant.cpp
iacpptarget.commands = \cp ../imod/imod_assistant.cpp .

iahtarget.target = imod_assistant.h
iahtarget.depends = ../imod/imod_assistant.h
iahtarget.commands = \cp ../imod/imod_assistant.h .

QMAKE_EXTRA_UNIX_TARGETS += iacpptarget iahtarget
