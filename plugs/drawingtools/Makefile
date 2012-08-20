# Makefile for drawingtools

MODULE = drawingtools

include ../../configure

O     = $(OBJEXT)
CXXFLAGS = -DIMOD_PLUGIN $(LIBCFLAGS) $(QTFLAGS)

PLUGS = $(MODULE).$(DLLEXT)

IMODINCLUDES = imod.h imodview.h imodplug.h control.h undoredo.h
COMMINCLUDES = icontextra.h _common_functions.h customdialog.h
IMODREALINCLUDES = ../../3dmod/imod.h ../../3dmod/imodview.h \
	../../3dmod/imodplug.h ../../3dmod/control.h ../../3dmod/undoredo.h

CONTEXTRA = ../noskecommon/icontextra
QTDIALOG = ../noskecommon/customdialog
COMMFUNC = ../noskecommon/_common_functions.h
QTDLGMOC = ../noskecommon/moc_customdialog

all : $(PLUGS)

install : $(PLUGS)
	$(CP) -f $(PLUGS) $(IMOD_PLUGIN_DIR)
	$(CP) -f $(MODULE).html $(PLUG_HELP_DIR)
	$(CP) -f naming_help.html $(PLUG_HELP_DIR)

# DNM: This makes the analysis tools help available even though its install is not run
	$(CP) -f ../analysistools/analysistools.html $(PLUG_HELP_DIR)

$(PLUGS) : $(CONTEXTRA).$(O) $(QTDIALOG).$(O) $(MODULE).$(O) \
	moc_$(MODULE).$(O) $(COMMINCLUDES) $(QTDLGMOC).$(O) \
	livewire/LivewireCalculator.$(O) livewire/PointPriorityQueue.$(O) livewire/Threaded.$(O) livewire/moc_Threaded.$(O) livewire/WeightCalculator.$(O) livewire/general.$(O)
	$(MAKELIBSO)$(PLUGS) $(CONTEXTRA).$(O) $(QTDIALOG).$(O) \
	$(MODULE).$(O) moc_$(MODULE).$(O) $(QTDLGMOC).$(O) \
	livewire/LivewireCalculator.$(O) livewire/PointPriorityQueue.$(O) livewire/Threaded.$(O) livewire/moc_Threaded.$(O) livewire/WeightCalculator.$(O) livewire/general.$(O)


moc_$(MODULE).cpp : $(MODULE).h
	$(MOC) $(MODULE).h -o moc_$(MODULE).cpp

$(MODULE).$(O) : $(IMODINCLUDES) $(COMMINCLUDES) $(MODULE).cpp $(MODULE).h 

livewire/moc_Threaded.cpp : livewire/Threaded.h
	(cd livewire ; $(MAKE) moc_Threaded.cpp)

livewire/LivewireCalculator.$(O) : livewire/LivewireCalculator.cpp
	(cd livewire ; $(MAKE) LivewireCalculator.$(O))

livewire/PointPriorityQueue.$(O) : livewire/PointPriorityQueue.cpp
	(cd livewire ; $(MAKE) PointPriorityQueue.$(O))

livewire/moc_Threaded.$(O)       : livewire/moc_Threaded.cpp
	(cd livewire ; $(MAKE) moc_Threaded.$(O))

livewire/Threaded.$(O)       : livewire/Threaded.cpp
	(cd livewire ; $(MAKE) Threaded.$(O))

livewire/WeightCalculator.$(O)   : livewire/WeightCalculator.cpp
	(cd livewire ; $(MAKE) WeightCalculator.$(O))

livewire/general.$(O)   : livewire/general.cpp
	(cd livewire ; $(MAKE) general.$(O))

# Build common object files in common directory
#
$(CONTEXTRA).$(O) : $(CONTEXTRA).cpp $(IMODREALINCLUDES) $(COMMFUNC) \
	$(CONTEXTRA).h
	(cd ../noskecommon ; $(MAKE) icontextra.$(O))

$(QTDIALOG).$(O) : $(QTDIALOG).cpp $(IMODREALINCLUDES) $(QTDIALOG).h
	(cd ../noskecommon ; $(MAKE) customdialog.$(O))

$(QTDLGMOC).$(O) : $(QTDLGMOC).cpp
	(cd ../noskecommon ; $(MAKE) moc_customdialog.$(O))

$(QTDLGMOC).cpp : $(QTDIALOG).h
	(cd ../noskecommon ; $(MAKE) customdialog.cpp)

# Copy IMOD includes
#
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

# Copy common includes
#
_common_functions.h : $(COMMFUNC)
	$(CP) -f $(COMMFUNC) .
icontextra.h : $(CONTEXTRA).h
	$(CP) -f $(CONTEXTRA).h .
customdialog.h : $(QTDIALOG).h
	$(CP) -f $(QTDIALOG).h .


clean :
	$(FIND) . -type f -name "*.$(OBJEXT)"  -exec rm "{}" \;
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
	$(RM) customdialog.h;
	$(RM) customdialog.cpp;



