SOURCES += mrc2tif.cpp tiff.c
TEMPLATE = app
CONFIG += qt
INCLUDEPATH += . ../../include

include (qconfigure)

INSTALLS += target

tiffc.target = 
tiffc.depends = ../../mrc/tiff.c
tiffc.commands = cp -f ../../mrc/tiff.c .

QMAKE_EXTRA_TARGETS += tiffc
