SOURCES += midas.cpp slots.cpp file_io.cpp graphics.cpp transforms.cpp \
        amat_to_rotmagstr.cpp gaussj.cpp

HEADERS += midas.h graphics.h slots.h

#MOC_DIR = tmp
#OBJECTS_DIR = tmp

TEMPLATE = app

# 1/25/03: (Qt 3.0.5) when opengl is added, program dies with highcolor style
# one solution is to leave opengl off and specify -lGL in LIBS in qconfigure
CONFIG += qt opengl

include (qconfigure)

INSTALLS += target
