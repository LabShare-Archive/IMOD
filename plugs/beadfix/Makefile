# Makefile for an old version of the beadfixer, used to test ability to build
# plugins.  The class is BeadFixer2 to avoid conflicts.

include ../../configure

O     = $(OBJEXT)
CXXFLAGS = $(LIBCFLAGS)

PLUGS = beadfix.$(DLLEXT)

all : $(PLUGS)

install : $(PLUGS)
	$(CP) -f $(PLUGS) $(IMOD_PLUGIN_DIR)


$(PLUGS) : beadfix.$(O) moc_beadfix.$(O)
	$(MAKELIBSO)$(PLUGS) beadfix.$(O) moc_beadfix.$(O)

moc_beadfix.cpp : beadfix.h
	$(MOC) beadfix.h -o moc_beadfix.cpp

beadfix.$(O) : model.h imodplugin.h imod.h imodview.h imodplug.h control.h 

model.h : ../../devkit/model.h
	$(CP) -f ../../devkit/model.h .
imodplugin.h : ../../devkit/imodplugin.h
	$(CP) -f ../../devkit/imodplugin.h .

imod.h : ../../imod/imod.h
	$(CP) -f ../../imod/imod.h .
imodview.h : ../../imod/imodview.h
	$(CP) -f ../../imod/imodview.h .
imodplug.h : ../../imod/imodplug.h
	$(CP) -f ../../imod/imodplug.h .
control.h : ../../imod/control.h
	$(CP) -f ../../imod/control.h .


clean :
	$(FIND) . -type f -name "*.o*"  -exec rm "{}" \;
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;
	$(FIND) . -type f -name "*.$(DLLEXT)" -exec rm "{}" \;
	$(FIND) . -type f -name "*.exp*"  -exec rm "{}" \;
	$(FIND) . -type f -name "*.lib"  -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
	$(FIND) . -type f -name "moc_*" -exec rm "{}" \;
	$(FIND) . -type f -name "so_locations" -exec rm "{}" \;




