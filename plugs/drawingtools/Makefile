# Makefile for drawingtools

include ../../configure

O     = $(OBJEXT)
CXXFLAGS = $(LIBCFLAGS)

PLUGS = drawingtools.$(DLLEXT)

all : $(PLUGS)

install : $(PLUGS)
	$(CP) -f $(PLUGS) drawingtools.html $(IMOD_PLUGIN_DIR)


$(PLUGS) : icontextra.$(O) drawingtools.$(O) moc_drawingtools.$(O)
	$(MAKELIBSO)$(PLUGS) icontextra.$(O) drawingtools.$(O) moc_drawingtools.$(O)

moc_drawingtools.cpp : drawingtools.h
	$(MOC) drawingtools.h -o moc_drawingtools.cpp

drawingtools.$(O) : imod.h imodview.h imodplug.h control.h undoredo.h

icontextra.$(O) : imod.h imodview.h imodplug.h control.h undoredo.h

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

icontextra.h : icontextra.h
	$(CP) -f icontextra.h .

clean :
	$(FIND) . -type f -name "*.o*"  -exec rm "{}" \;
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;
	$(FIND) . -type f -name "*.$(DLLEXT)" -exec rm "{}" \;
	$(FIND) . -type f -name "*.exp*"  -exec rm "{}" \;
	$(FIND) . -type f -name "*.lib"  -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
	$(FIND) . -type f -name "moc_*" -exec rm "{}" \;
	$(FIND) . -type f -name "so_locations" -exec rm "{}" \;




