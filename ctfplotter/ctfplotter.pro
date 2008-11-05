TEMPLATE = app
CONFIG += qt
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
                imod_assistant.cpp
IMAGES        = images/zoomin.png  \
                images/zoomout.png \
                images/printer.png \
                images/moreTile.png \
                images/range.png \
                images/angle.png \
                images/save.png \
                images/ctfhelp.png 

include (qconfigure)

INSTALLS += target
