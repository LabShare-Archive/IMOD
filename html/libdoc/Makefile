include ../../configure

SDEXE = ../../qttools/sourcedoc/$(SOURCEDOC_EXE)
LHD = ../libhelp
INCD = ../../include
IMD = ../../libimod
CFD = ../../libcfshr
IIMD = ../../libiimod
QTD = ../../libdiaqt
PLD = ../../3dmod

HTMLS = $(LHD)/libimod.html $(LHD)/libiimod.html $(LHD)/imodel.html $(LHD)/iobj.html \
$(LHD)/objgroup.html $(LHD)/icont.html $(LHD)/ipoint.html $(LHD)/imesh.html \
$(LHD)/imodel_files.html $(LHD)/ilist.html $(LHD)/istore.html $(LHD)/unitIO.html \
$(LHD)/imat.html $(LHD)/iplane.html $(LHD)/iview.html $(LHD)/ilabel.html \
$(LHD)/mod_fwrap.html $(LHD)/b3dutil.html $(LHD)/autodoc.html \
$(LHD)/mrcfiles.html $(LHD)/iimage.html $(LHD)/plist.html \
$(LHD)/mrcslice.html $(LHD)/sliceproc.html $(LHD)/libmesh.html \
$(LHD)/libifft.html $(LHD)/libdiaqt.html $(LHD)/libcfshr.html \
$(LHD)/cfutils.html $(LHD)/3dmodplug.html $(LHD)/libwarp.html \
$(LHD)/minimization.html $(LHD)/simplestat.html $(LHD)/filtxcorr.html \
$(LHD)/interpbin.html $(LHD)/robuststat.html $(LHD)/taperpad.html


CFUTILS = $(CFD)/parselist.c $(CFD)/samplemeansd.c $(CFD)/colormap.c \
 $(CFD)/insidecontour.c $(CFD)/convexbound.c $(CFD)/surfacesort.c $(CFD)/pctstretch.c \
 $(CFD)/piecefuncs.c $(CFD)/find_piece_shifts.c $(CFD)/parallelwrite.c $(CFD)/colormap.c

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

$(LHD)/objgroup.html: objgroup.html $(IMD)/objgroup.c
	$(SDEXE) -d $(IMD) objgroup.html $(LHD)/objgroup.html

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

$(LHD)/interpbin.html: interpbin.html $(CFD)/cubinterp.c $(CFD)/zoomdown.c \
	$(CFD)/reduce_by_binning.c $(CFD)/linearxforms.c $(CFD)/amat_to_rotmagstr.c
	$(SDEXE) -d $(CFD) interpbin.html $(LHD)/interpbin.html

$(LHD)/robuststat.html: robuststat.html $(CFD)/robuststat.c $(CFD)/percentile.c
	$(SDEXE) -d $(CFD) robuststat.html $(LHD)/robuststat.html

$(LHD)/simplestat.html: simplestat.html $(CFD)/simplestat.c $(CFD)/statfuncs.c
	$(SDEXE) -d $(CFD) simplestat.html $(LHD)/simplestat.html

$(LHD)/minimization.html: minimization.html $(CFD)/regression.c $(CFD)/gaussj.c \
	$(CFD)/sparselsqr.c $(CFD)/amoeba.c $(CFD)/circlefit.c
	$(SDEXE) -d $(CFD) minimization.html $(LHD)/minimization.html

$(LHD)/filtxcorr.html: filtxcorr.html $(CFD)/filtxcorr.c $(CFD)/scaledsobel.c 
	$(SDEXE) -d $(CFD) filtxcorr.html $(LHD)/filtxcorr.html

$(LHD)/taperpad.html: taperpad.html $(CFD)/taperpad.c $(CFD)/taperatfill.c
	$(SDEXE) -d $(CFD) taperpad.html $(LHD)/taperpad.html

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

$(LHD)/unitIO.html: unitIO.html $(IIMD)/unit_header.c $(IIMD)/unit_fileio.c
	$(SDEXE) -d $(IIMD) unitIO.html $(LHD)/unitIO.html

$(LHD)/mrcslice.html: mrcslice.html $(IIMD)/mrcslice.c $(INCD)/mrcslice.h
	$(SDEXE) -d $(IIMD) mrcslice.html $(LHD)/mrcslice.html

$(LHD)/sliceproc.html: sliceproc.html $(IIMD)/sliceproc.c
	$(SDEXE) -d $(IIMD) sliceproc.html $(LHD)/sliceproc.html

$(LHD)/libmesh.html: libmesh.html ../../libmesh/objprep.c \
	../../libmesh/remesh.c ../../libmesh/mkmesh.c ../../libmesh/skinobj.c
	$(SDEXE) -d ../../libmesh libmesh.html $(LHD)/libmesh.html

$(LHD)/libwarp.html: libwarp.html ../../libwarp/warputils.c \
	../../libwarp/warpfiles.c ../../libwarp/warpinterp.c ../../libwarp/maggradfield.c
	$(SDEXE) -d ../../libwarp libwarp.html $(LHD)/libwarp.html

$(LHD)/libifft.html: libifft.html ../../libfft/odfft.c \
	../../libfft/todfft.c  ../../libfft/thrdfft.c
	$(SDEXE) -d ../../libfft libifft.html $(LHD)/libifft.html

$(LHD)/libdiaqt.html: libdiaqt.html $(QTD)/arrowbutton.cpp \
	$(QTD)/colorselector.cpp $(QTD)/dia_qtutils.cpp \
	$(QTD)/dialog_frame.cpp $(QTD)/multislider.cpp $(QTD)/tooledit.cpp
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
