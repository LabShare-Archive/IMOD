# Makefile for drawingtools

include ../../configure

O     = $(OBJEXT)
CXXFLAGS = $(LIBCFLAGS) $(QTFLAGS)

PLUGS = drawingtools.$(DLLEXT)

IMODINCLUDES = imod.h imodview.h imodplug.h control.h undoredo.h

all : $(PLUGS)

install : $(PLUGS)
	$(CP) -f $(PLUGS) drawingtools.html $(IMOD_PLUGIN_DIR)


$(PLUGS) : icontextra.$(O) qt_dialog_customizable.$(O) \
	drawingtools.$(O) moc_drawingtools.$(O)
	$(MAKELIBSO)$(PLUGS) icontextra.$(O) qt_dialog_customizable.$(O) \
	drawingtools.$(O) moc_drawingtools.$(O)


moc_drawingtools.cpp : drawingtools.h
	$(MOC) drawingtools.h -o moc_drawingtools.cpp

drawingtools.$(O) : $(IMODINCLUDES) _common_functions.h icontextra.h \
	qt_dialog_customizable.h drawingtools.h

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


_common_functions.h : ../noskecommon/_common_functions.h
	$(CP) -f ../noskecommon/_common_functions.h .
icontextra.h : ../noskecommon/icontextra.h
	$(CP) -f ../noskecommon/icontextra.h .
icontextra.cpp : ../noskecommon/icontextra.cpp
	$(CP) -f ../noskecommon/icontextra.cpp .
qt_dialog_customizable.h : ../noskecommon/qt_dialog_customizable.h
	$(CP) -f ../noskecommon/qt_dialog_customizable.h .
qt_dialog_customizable.cpp : ../noskecommon/qt_dialog_customizable.cpp
	$(CP) -f ../noskecommon/qt_dialog_customizable.cpp .


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
	$(RM) qt_dialog_customizable.h;
	$(RM) qt_dialog_customizable.cpp;



