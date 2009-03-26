# Makefile for noskecommon

include ../../configure

O     = $(OBJEXT)
CXXFLAGS = $(LIBCFLAGS)  $(QTFLAGS)

IMODINCLUDES = imod.h imodview.h imodplug.h control.h undoredo.h

all : icontextra.$(O) qt_dialog_customizable.$(O)

install : 



icontextra.$(O) : icontextra.cpp $(IMODINCLUDES) _common_functions.h \
	icontextra.h

qt_dialog_customizable.$(O) : qt_dialog_customizable.cpp $(IMODINCLUDES) \
	qt_dialog_customizable.h


imod.h : ../../imod/imod.h
	$(CP) -f ../../imod/imod.h .
imodview.h : ../../imod/imodview.h
	$(CP) -f ../../imod/imodview.h .
imodplug.h : ../../imod/imodplug.h
	$(CP) -f ../../imod/imodplug.h .
control.h : ../../imod/control.h
	$(CP) -f ../../imod/control.h .
undoredo.h : ../../imod/undoredo.h
	$(CP) -f ../../imod/undoredo.h .

clean :
	$(FIND) . -type f -name "*.o*"  -exec rm "{}" \;
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;
	$(FIND) . -type f -name "*.$(DLLEXT)" -exec rm "{}" \;
	$(FIND) . -type f -name "*.exp*"  -exec rm "{}" \;
	$(FIND) . -type f -name "*.lib"  -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
	$(FIND) . -type f -name "moc_*" -exec rm "{}" \;
	$(FIND) . -type f -name "so_locations" -exec rm "{}" \;
	$(RM) $(IMODINCLUDES);

