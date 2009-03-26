# Makefile for beadhelper

MODULE = beadhelper

include ../../configure

O     = $(OBJEXT)
CXXFLAGS = $(LIBCFLAGS)  $(QTFLAGS)

PLUGS = $(MODULE).$(DLLEXT)

IMODINCLUDES = imod.h imodview.h imodplug.h control.h undoredo.h
COMMINCLUDES = icontextra.h _common_functions.h qt_dialog_customizable.h
IMODREALINCLUDES = ../../imod/imod.h ../../imod/imodview.h \
	../../imod/imodplug.h ../../imod/control.h ../../imod/undoredo.h

CONTEXTRA = ../noskecommon/icontextra
QTDIALOG = ../noskecommon/qt_dialog_customizable
COMMFUNC = ../noskecommon/_common_functions.h

all : $(PLUGS)

install : $(PLUGS)
	$(CP) -f $(PLUGS) $(MODULE).html $(IMOD_PLUGIN_DIR)


$(PLUGS) : $(CONTEXTRA).$(O) $(QTDIALOG).$(O) $(MODULE).$(O) \
	moc_$(MODULE).$(O) $(COMMINCLUDES)
	$(MAKELIBSO)$(PLUGS) $(CONTEXTRA).$(O) $(QTDIALOG).$(O) \
	$(MODULE).$(O) moc_$(MODULE).$(O)


moc_$(MODULE).cpp : $(MODULE).h
	$(MOC) $(MODULE).h -o moc_$(MODULE).cpp

$(MODULE).$(O) : $(IMODINCLUDES) $(COMMINCLUDES) $(MODULE).cpp $(MODULE).h 

# Build common object files in common directory
#
$(CONTEXTRA).$(O) : $(CONTEXTRA).cpp $(IMODREALINCLUDES) $(COMMFUNC) \
	$(CONTEXTRA).h
	(cd ../noskecommon ; $(MAKE) icontextra.$(O))

$(QTDIALOG).$(O) : $(QTDIALOG).cpp $(IMODREALINCLUDES) $(QTDIALOG).h
	(cd ../noskecommon ; $(MAKE) qt_dialog_customizable.$(O))


# Copy IMOD includes
#
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

# Copy common includes
#
_common_functions.h : $(COMMFUNC)
	$(CP) -f $(COMMFUNC) .
icontextra.h : $(CONTEXTRA).h
	$(CP) -f $(CONTEXTRA).h .
qt_dialog_customizable.h : $(QTDIALOG).h
	$(CP) -f $(QTDIALOG).h .


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



