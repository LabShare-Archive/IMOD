# Makefile for imod and imodv
#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#  Revision 3.4  2002/09/27 21:15:34  rickg
#  Added new dependcies for imod_io.h and imod_client_message.*
#
#  Revision 3.3  2002/09/11 04:43:51  mast
#  Add dependencies on diaP.h
#
#  Revision 3.2  2002/09/04 00:19:46  mast
#  Add dependency of imodv on imod.h
#
#  Revision 3.1  2001/12/17 18:37:32  mast
#  Add imod_cachefill.c to make, add many dependencies on headers included by
#  imod.h
#
#  Revision 3.0  2001/11/29 18:10:47  rickg
#  *** empty log message ***
#
#  Revision 1.2  2001/11/27 15:59:54  mast
#  Add dependency on .version file
#
#
include ../configure

IMOD_OBJS = imod.o imod_io.o imod_input.o imod_display.o \
 imod_info.o imod_info_cb.o imod_menu.o slicer.o xyz.o \
 autox.o imod_object_edit.o imod_cont_edit.o \
 imod_workprocs.o wprint.o pixelview.o control.o \
 imodview.o imod_edit.o b3dgfx.o b3dfile.o \
 xtum.o xtilt.o xgraph.o xzap.o iproc.o sliceproc.o \
 imod_iscale.o imod_model_edit.o imod_cont_copy.o imod_moviecon.o imodplug.o \
 samplemeansd.o imod_cachefill.o imod_client_message.o

IMOD_X11_OBJS = ximodv.o imodv.o imod_model_draw.o

IMODVOBJS = imodv_gfx.o imodv_input.o imodv_menu.o imodv_objed.o\
 imodv_control.o imodv_light.o imodv_modeled.o imodv_movie.o imodv_views.o\
 imodv_stereo.o imodv_image.o imodv_ogl.o imodv_depthcue.o

GL_LIBS  = -lgutil -limage -lsphere -lfm_s $(GLIBS)

HELP_INCS = imodhelp.h menus.h hotkey.h

PROGS = imod imodv_gl imodv ximod ximodv

#############################################################################
all : imod imodv

##############################################################################
# imod

imod : $(IMOD_OBJS) $(IMOD_X11_OBJS) $(IMODVOBJS)
	$(IMODCC) -o imod $(IMOD_OBJS) $(IMOD_X11_OBJS) $(IMODVOBJS) \
	$(LDFLAGS) -limod -liimod -ldia $(TIFFLIBS) \
	$(GLIBS) $(XLIBS) $(PLUGLIBS) $(LIBS) -lm

#############################################################################
# imodv is the same program as imod.

imodv : imod
	if ( ! -e imodv) ln -s imod imodv


#############################################################################
# Dependencies

IMODDEPS = imod.h imodP.h b3dgfx.h autox.h imod_io.h sslice.h xxyz.h \
 $(INCDIR)imodel.h $(INCDIR)imodi.h $(INCDIR)dia.h $(INCDIR)iobj.h \
 $(INCDIR)icont.h $(INCDIR)imesh.h $(INCDIR)ipoint.h $(INCDIR)imat.h \
 $(INCDIR)iplane.h $(INCDIR)iview.h $(INCDIR)hvemtypes.h $(INCDIR)ilist.h \
 imod_io.h imod_client_message.h

IMODVDEPS = imodv.h $(INCDIR)imodel.h $(INCDIR)dia.h $(INCDIR)iobj.h \
 $(INCDIR)icont.h $(INCDIR)imesh.h $(INCDIR)ipoint.h $(INCDIR)imat.h \
 $(INCDIR)iplane.h $(INCDIR)iview.h $(INCDIR)hvemtypes.h $(INCDIR)ilist.h 


$(IMOD_OBJS) : $(IMODDEPS) $(IMODVDEPS)
imod_model_draw.o : $(IMODDEPS) $(IMODVDEPS)
imodv_gfx.o : $(IMODDEPS) $(IMODVDEPS)
imodv_image.o : $(IMODDEPS) $(IMODVDEPS)
imodv_input.o : $(IMODDEPS) $(IMODVDEPS)
imodv_ogl.o : $(IMODDEPS) $(IMODVDEPS)
ximodv.o : $(IMODDEPS) $(IMODVDEPS) ximodv.h
imod_moviecon.o : $(IMODVDEPS)
imodv_objed.o : $(IMODDEPS)
imodv_control.o : $(IMODDEPS)

$(IMODVOBJS) : $(IMODVDEPS)
imodv.o : $(IMODVDEPS) imod.h

DIAPUSERS = pixelview.o imod_model_edit.o imod_object_edit.o imodv_control.o \
 imodv_objed.o imod_cont_copy.o imod_workprocs.o slicer.o iproc.o xzap.o xyz.o
$(DIAPUSERS) : $(INCDIR)diaP.h

slicer.o : sslice.h slicer.h
imod_menu.o : imodhelp.h hotkey.h menus.h imod_info.h iproc.h options.h
imod_info.o : imod_info.h
imod_info_cb.o : imod_info.h options.h xzap.h
imod_io.o : imod_info.h xzap.h
iproc.o : iproc.h sliceproc.h
xzap.o : keypad.h xzap.h
xyz.c : keypad.h
sliceproc.o : sliceproc.h
imod_edit.o : xzap.h
imod.o : ../.version
imod_menu.o : ../.version
imodv_meno.o : ../.version


##############################################################################
# make help files for imod
# only imodhelp works now.  Take the current man page, imod.1, and run it
# through emacs to untabify it and create imodhelp.  Then make

mkargv : mkargv.o
	$(CC) $(CFLAGS) -o mkargv mkargv.o

imodhelp.h : imodhelp mkargv
	./mkargv imodhelp imodhelp.h "Imod_help_text[]"

#menus.h : menus.help mkargv
#	./mkargv menus.help menus.h Imod_menus_help[]

#hotkey.h : hotkey.help mkargv
#	./mkargv hotkey.help hotkey.h Imod_hotkey_help[]

##############################################################################

install: imod
	strip imod
	$(CP) imod $(INSTBINDIR)/
	(cd $(INSTBINDIR) ; if ( ! -e imodv) ln -s imod imodv )

clean : 
	$(FIND) . -type f -name "*.o" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
	$(FIND) . -type f -name "core" -exec rm "{}" \;
	$(FIND) . -type f -name "mkargv" -exec rm "{}" \;
	$(FIND) . -type f -name "imod" -exec rm "{}" \;
