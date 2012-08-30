# Makefile for noskecommon

include ../../configure

O     = $(OBJEXT)
CXXFLAGS = $(LIBCFLAGS)  $(QTFLAGS)

IMODINCLUDES = imod.h imodview.h imodplug.h control.h undoredo.h

all : icontextra.$(O) customdialog.$(O)

install : 



icontextra.$(O) : icontextra.cpp $(IMODINCLUDES) _common_functions.h \
	icontextra.h

customdialog.$(O) : customdialog.cpp $(IMODINCLUDES) \
	customdialog.h

moc_customdialog.$(O) : moc_customdialog.cpp

moc_customdialog.cpp : customdialog.h
	$(MOC) customdialog.h -o moc_customdialog.cpp

includes : $(IMODINCLUDES)

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
	$(RM) $(IMODINCLUDES);

