include ../../configure

SDEXE = ../../sourcedoc/$(SOURCEDOC_EXE)
LHD = ../libhelp
INCD = ../../include
IMD = ../../libimod
CFD = ../../libcfshr
IIMD = ../../libiimod
QTD = ../../libdiaqt
PLD = ../../imod

HTMLS = $(LHD)/libimod.html $(LHD)/libiimod.html $(LHD)/imodel.html \
$(LHD)/iobj.html $(LHD)/icont.html $(LHD)/ipoint.html $(LHD)/imesh.html \
$(LHD)/imodel_files.html $(LHD)/ilist.html $(LHD)/istore.html \
$(LHD)/imat.html $(LHD)/iplane.html $(LHD)/iview.html $(LHD)/ilabel.html \
$(LHD)/mod_fwrap.html $(LHD)/b3dutil.html $(LHD)/autodoc.html \
$(LHD)/mrcfiles.html $(LHD)/iimage.html $(LHD)/plist.html \
$(LHD)/mrcslice.html $(LHD)/sliceproc.html $(LHD)/libmesh.html \
$(LHD)/libifft.html $(LHD)/libdiaqt.html $(LHD)/libcfshr.html \
$(LHD)/cfutils.html $(LHD)/3dmodplug.html

CFUTILS = $(CFD)/parselist.c $(CFD)/circlefit.c $(CFD)/filtxcorr.c \
$(CFD)/amoeba.c $(CFD)/samplemeansd.c $(CFD)/colormap.c $(CFD)/taperpad.c \
$(CFD)/reduce_by_binning.c $(CFD)/cubinterp.c $(CFD)/insidecontour.c \
$(CFD)/scaledsobel.c

# Make doc files and place them in help directory

all : $(HTMLS)

$(LHD)/libimod.html: libimod.html
	$(CP) libimod.html $(LHD)
$(LHD)/libcfshr.html: libcfshr.html
	$(CP) libcfshr.html $(LHD)
$(LHD)/libiimod.html: libiimod.html
	$(CP) libiimod.html $(LHD)

$(LHD)/imodel.html: imodel.html $(IMD)/imodel.c
	$(SDEXE) -d $(IMD) imodel.html $(LHD)/imodel.html

$(LHD)/iobj.html: iobj.html $(IMD)/iobj.c
	$(SDEXE) -d $(IMD) iobj.html $(LHD)/iobj.html

$(LHD)/icont.html: icont.html $(IMD)/icont.c
	$(SDEXE) -d $(IMD) icont.html $(LHD)/icont.html

$(LHD)/ipoint.html: ipoint.html $(IMD)/ipoint.c
	$(SDEXE) -d $(IMD) ipoint.html $(LHD)/ipoint.html

$(LHD)/imesh.html: imesh.html $(IMD)/imesh.c
	$(SDEXE) -d $(IMD) imesh.html $(LHD)/imesh.html

$(LHD)/imodel_files.html: imodel_files.html $(IMD)/imodel_files.c
	$(SDEXE) -d $(IMD) imodel_files.html $(LHD)/imodel_files.html

$(LHD)/ilist.html: ilist.html $(CFD)/ilist.c
	$(SDEXE) -d $(CFD) ilist.html $(LHD)/ilist.html

$(LHD)/istore.html: istore.html $(IMD)/istore.c $(INCD)/istore.h
	$(SDEXE) -d $(IMD) istore.html $(LHD)/istore.html

$(LHD)/imat.html: imat.html $(IMD)/imat.c
	$(SDEXE) -d $(IMD) imat.html $(LHD)/imat.html

$(LHD)/iplane.html: iplane.html $(IMD)/iplane.c $(INCD)/imodel.h
	$(SDEXE) -d $(IMD) iplane.html $(LHD)/iplane.html

$(LHD)/iview.html: iview.html $(IMD)/iview.c $(INCD)/imodel.h
	$(SDEXE) -d $(IMD) iview.html $(LHD)/iview.html

$(LHD)/ilabel.html: ilabel.html $(IMD)/ilabel.c $(INCD)/imodel.h
	$(SDEXE) -d $(IMD) ilabel.html $(LHD)/ilabel.html

$(LHD)/mod_fwrap.html: mod_fwrap.html $(IMD)/imodel_fwrap.c
	$(SDEXE) -d $(IMD) mod_fwrap.html $(LHD)/mod_fwrap.html

$(LHD)/b3dutil.html: b3dutil.html $(CFD)/b3dutil.c 
	$(SDEXE) -d $(CFD) b3dutil.html $(LHD)/b3dutil.html

$(LHD)/cfutils.html: cfutils.html $(CFUTILS)
	$(SDEXE) -d $(CFD) cfutils.html $(LHD)/cfutils.html

$(LHD)/autodoc.html: autodoc.html $(CFD)/autodoc.c
	$(SDEXE) -d $(CFD) autodoc.html $(LHD)/autodoc.html

$(LHD)/mrcfiles.html: mrcfiles.html $(IIMD)/mrcfiles.c
	$(SDEXE) -d $(IIMD) mrcfiles.html $(LHD)/mrcfiles.html

$(LHD)/iimage.html: iimage.html $(IIMD)/iimage.c $(IIMD)/iilikemrc.c \
	$(INCD)/iimage.h
	$(SDEXE) -d $(IIMD) iimage.html $(LHD)/iimage.html

$(LHD)/plist.html: plist.html $(IIMD)/plist.c
	$(SDEXE) -d $(IIMD) plist.html $(LHD)/plist.html

$(LHD)/mrcslice.html: mrcslice.html $(IIMD)/mrcslice.c $(INCD)/mrcslice.h
	$(SDEXE) -d $(IIMD) mrcslice.html $(LHD)/mrcslice.html

$(LHD)/sliceproc.html: sliceproc.html $(IIMD)/sliceproc.c
	$(SDEXE) -d $(IIMD) sliceproc.html $(LHD)/sliceproc.html

$(LHD)/libmesh.html: libmesh.html ../../libmesh/objprep.c \
	../../libmesh/remesh.c ../../libmesh/mkmesh.c ../../libmesh/skinobj.c
	$(SDEXE) -d ../../libmesh libmesh.html $(LHD)/libmesh.html

$(LHD)/libifft.html: libifft.html ../../libcfft/odfft.c \
	../../libcfft/todfft.c  ../../libcfft/thrdfft.c
	$(SDEXE) -d ../../libcfft libifft.html $(LHD)/libifft.html

$(LHD)/libdiaqt.html: libdiaqt.html $(QTD)/arrowbutton.cpp \
	$(QTD)/colorselector.cpp $(QTD)/dia_qtutils.cpp \
	$(QTD)/dialog_frame.cpp \
	$(QTD)/multislider.cpp $(QTD)/tooledit.cpp
	$(SDEXE) -d $(QTD) libdiaqt.html $(LHD)/libdiaqt.html


$(LHD)/3dmodplug.html: 3dmodplug.html $(PLD)/imodplug.h $(PLD)/imodview.h \
	$(PLD)/imod.h $(PLD)/control.h $(PLD)/undoredo.cpp $(PLD)/undoredo.h
	$(SDEXE) -d $(PLD) 3dmodplug.html $(LHD)/3dmodplug.html

# For testing
#	../../sourcedoc/$(SOURCEDOC_EXE) testin.html testout.html
#	../../sourcedoc/$(SOURCEDOC_EXE) -f testfin.html testfout.html

clean :
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
