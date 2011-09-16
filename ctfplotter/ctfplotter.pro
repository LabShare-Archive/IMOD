TEMPLATE = app
CONFIG += qt assistant
CONFIG       += exceptions
HEADERS       += defocusfinder.h linearfitting.h simplexfitting.h myapp.h plotter.h \
                 rangedialog.h angledialog.h imod_assistant.h slicecache.h ctfmain.h
SOURCES       = main.cpp \
                myapp.cpp \
                plotter.cpp \
                rangedialog.cpp\
                angledialog.cpp \
                simplexfitting.cpp \
                linearfitting.cpp \
                defocusfinder.cpp \
                slicecache.cpp \
                ctfutils.cpp \
                imod_assistant.cpp

RESOURCES    =plotter.qrc
                
include (qconfigure)

INSTALLS += target

iacpptarget.target = imod_assistant.cpp
iacpptarget.depends = ../3dmod/imod_assistant.cpp
iacpptarget.commands = \cp ../3dmod/imod_assistant.cpp .

iahtarget.target = imod_assistant.h
iahtarget.depends = ../3dmod/imod_assistant.h
iahtarget.commands = \cp ../3dmod/imod_assistant.h .

QMAKE_EXTRA_UNIX_TARGETS += iacpptarget iahtarget
