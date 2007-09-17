CONFIG       += exceptions
HEADERS       += defocusfinder.h linearfitting.h simplexfitting.h myapp.h plotter.h \
                 rangedialog.h angledialog.h
INCLUDEPATH  += /home4/mast/PCMOD/include
SOURCES       = main.cpp \
                myapp.cpp \
                plotter.cpp \
                rangedialog.cpp\
                angledialog.cpp \
                simplexfitting.cpp \
                linearfitting.cpp \
                defocusfinder.cpp 
LIBS += -L/scratch/shrek/build/IMODWorks/lib -L/home4/xiongq/LAPACK64 \
         -lm -llapack -lblas -liimod -limod -ltiff -lifft -lg2c
IMAGES        = images/zoomin.png  \
                images/zoomout.png \
                images/printer.png \
                images/moreTile.png \
                images/range.png \
                images/angle.png \
                images/save.png
