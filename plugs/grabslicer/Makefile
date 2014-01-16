# Makefile for Grab with Note

include ../../configure

O     = $(OBJEXT)
CXXFLAGS = $(LIBCFLAGS) $(QTFLAGS)

PLUGS = grabslicer.$(DLLEXT)

all : $(PLUGS)

install : $(PLUGS)
	$(CP) -f $(PLUGS) $(IMOD_PLUGIN_DIR)
	$(CP) -f grabslicer.html $(PLUG_HELP_DIR)


$(PLUGS) : grabslicer.$(O) moc_grabslicer.$(O)
	$(MAKELIBSO)$(PLUGS) grabslicer.$(O) moc_grabslicer.$(O)

moc_grabslicer.cpp : grabslicer.h
	$(MOC) grabslicer.h -o moc_grabslicer.cpp

grabslicer.$(O) : imod.h imodview.h imodplug.h control.h undoredo.h

imod.h : ../../3dmod/imod.h
	$(CP) -f ../../3dmod/imod.h .
imodview.h : ../../3dmod/imodview.h
	$(CP) -f ../../3dmod/imodview.h .
imodplug.h : ../../3dmod/imodplug.h
	$(CP) -f ../../3dmod/imodplug.h .
control.h : ../../3dmod/control.h
	$(CP) -f ../../3dmod/control.h .
undoredo.h : ../../3dmod/undoredo.h
	$(CP) -f ../../3dmod/undoredo.h .


clean :
	$(FIND) . -type f -name "*.$(OBJEXT)"  -exec rm "{}" \;
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;
	$(FIND) . -type f -name "*.$(DLLEXT)" -exec rm "{}" \;
	$(FIND) . -type f -name "*.exp*"  -exec rm "{}" \;
	$(FIND) . -type f -name "*.lib"  -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
	$(FIND) . -type f -name "moc_*" -exec rm "{}" \;
	$(FIND) . -type f -name "so_locations" -exec rm "{}" \;




