TEMPLATE = app
CONFIG += qt
CONFIG       += exceptions
HEADERS       += defocusfinder.h linearfitting.h simplexfitting.h myapp.h plotter.h \
                 rangedialog.h angledialog.h
SOURCES       = main.cpp \
                myapp.cpp \
                plotter.cpp \
                rangedialog.cpp\
                angledialog.cpp \
                simplexfitting.cpp \
                linearfitting.cpp \
                defocusfinder.cpp 
IMAGES        = images/zoomin.png  \
                images/zoomout.png \
                images/printer.png \
                images/moreTile.png \
                images/range.png \
                images/angle.png \
                images/save.png

include (qconfigure)

INSTALLS += target
