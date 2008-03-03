# Makefile for beadhelper

include ../../configure

O     = $(OBJEXT)
CXXFLAGS = $(LIBCFLAGS)

PLUGS = beadhelper.$(DLLEXT)

IMODINCLUDES = imod.h imodview.h imodplug.h control.h undoredo.h

all : $(PLUGS)

install : $(PLUGS)
	$(CP) -f $(PLUGS) beadhelper.html $(IMOD_PLUGIN_DIR)


$(PLUGS) : icontextra.$(O) qt_dialog_customizable.$(O) beadhelper.$(O) moc_beadhelper.$(O)
	$(MAKELIBSO)$(PLUGS) icontextra.$(O) qt_dialog_customizable.$(O) \
	beadhelper.$(O) moc_beadhelper.$(O)

moc_beadhelper.cpp : beadhelper.h
	$(MOC) beadhelper.h -o moc_beadhelper.cpp

beadhelper.$(O) : $(IMODINCLUDES) _common_functions.h icontextra.h \
	beadhelper.h

icontextra.$(O) : $(IMODINCLUDES) _common_functions.h icontextra.h

qt_dialog_customizable.$(O) : $(IMODINCLUDES) qt_dialog_customizable.h


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

icontextra.h : ../drawingtools/icontextra.h
	$(CP) -f ../drawingtools/icontextra.h .
icontextra.cpp : ../drawingtools/icontextra.cpp
	$(CP) -f ../drawingtools/icontextra.cpp .
_common_functions.h : ../drawingtools/_common_functions.h
	$(CP) -f ../drawingtools/_common_functions.h .

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
	$(RM) _common_functions.h;
	$(RM) icontextra.h;
	$(RM) icontextra.cpp;



