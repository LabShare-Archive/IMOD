#
# Makefile for mrc programs.
#

#########################################################################
#  Copyright (C) 1995-1996 by Boulder Laboratory for 3-Dimensional Fine
#  Structure ("BL3DFS") and  the  Regents of the University of Colorado.
#
#  BL3DFS reserves the exclusive  rights of preparing derivative works,
#  distributing copies for  sale, lease or lending and displaying this 
#  software and documentation.                                      
#  Users may reproduce the software and documentation as long as the
#  copyright notice and other notices are preserved.                
#  Neither the software nor the documentation may be distributed for
#  profit, either in original form or in derivative works.          
#
#  THIS SOFTWARE AND/OR  DOCUMENTATION IS PROVIDED WITH NO WARRANTY,
#  EXPRESS OR IMPLIED, INCLUDING, WITHOUT LIMITATION,  WARRANTY  OF
#  MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE.
#
#  This work is supported by NIH biotechnology grant #RR00592, 
#  for the Boulder Laboratory for 3-Dimensional Fine Structure.
#  University of Colorado, MCDB Box 347, Boulder, CO 80309
########################################################################
#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#  Revision 3.2  2002/11/05 23:56:07  mast
#  Added -limod to be able to call imodCopyright
#
#  Revision 3.1  2001/12/31 19:21:25  mast
#  Added dm3props
#

include ../configure

MRCVOBJS  = mrcv.o mrcv_movie.o mrcv_draw.o mrcv_batch.o rgbv.o colormap.o

PROGS     = mrc2tif raw2mrc tif2mrc mrcbyte mrcx mrctaper dm3props $(RGBPROGS)

IILIBS    = -limod -liimod $(TIFFLIBS)

all : $(PROGS)

##############################################################################
# make programs.
#

raw2mrc : raw2mrc.o 
	$(CC) -o raw2mrc raw2mrc.o  $(LDFLAGS) $(IILIBS) $(CLIBS)

tif2mrc : tif2mrc.o tiff.o 
	$(CC) -o tif2mrc tif2mrc.o tiff.o $(LDFLAGS) $(IILIBS) $(CLIBS)

mrc2tif : mrc2tif.o tiff.o
	$(CC) -o  mrc2tif  mrc2tif.o tiff.o $(LDFLAGS) $(IILIBS) $(CLIBS)

mrcx : mrcx.o
	$(CC) -o mrcx mrcx.o $(LDFLAGS) $(IILIBS) $(CLIBS)

mrcbyte : mrcbyte.o 
	$(CC) -o mrcbyte mrcbyte.o $(LDFLAGS) $(IILIBS)  $(CLIBS)

mrctaper : mrctaper.o 
	$(CC) -o mrctaper mrctaper.o $(LDFLAGS) $(IILIBS)  $(CLIBS)

dm3props : dm3props.o 
	$(CC) -o dm3props dm3props.o $(LDFLAGS) $(IILIBS)  $(CLIBS)

#
# SGI only programs
#
frommrc : frommrc.o
	$(CC) -o frommrc frommrc.o $(LDFLAGS) $(IILIBS) -limage $(CLIBS)

tomrc : tomrc.o 
	$(CC) -o tomrc tomrc.o $(LDFLAGS) $(IILIBS) -limage $(CLIBS)

mrcv : $(MRCVOBJS) 
	$(CC)  -o mrcv $(MRCVOBJS) $(LDFLAGS) -limod $(IILIBS) \
	-lgutil -limage -lfm -lgl $(XLIBS) $(CLIBS)



##############################################################################
# install programs

install : $(PROGS)
	strip $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)
#	if (-x mrcv)    strip mrcv
#	if (-x mrcv)    $(CP) mrcv    $(INSTBINDIR)
#	if (-x mrcx)    strip mrcx
#	if (-x mrcx)    $(CP) mrcx    $(INSTBINDIR)
#	if (-x frommrc) strip frommrc
#	if (-x frommrc) $(CP) frommrc $(INSTBINDIR)
#	if (-x tomrc)   strip tomrc
#	if (-x tomrc)   $(CP) tomrc   $(INSTBINDIR)
#	if (-x tif2mrc) strip tif2mrc
#	if (-x tif2mrc) $(CP) tif2mrc $(INSTBINDIR)
#	if (-x mrc2tif) strip mrc2tif
#	if (-x mrc2tif) $(CP) mrc2tif $(INSTBINDIR)
#	if (-x raw2mrc) strip mrc2tif
#	if (-x raw2mrc) $(CP) raw2mrc $(INSTBINDIR)
#	if (-x ../xmrcv/xmrcv) $(CP) ../xmrcv/xmrcv $(INSTBINDIR)

clean : 
	$(FIND) . -type f -name "*.o" -exec rm "{}" \;
	$(FIND) . -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -perm 755 -exec rm "{}" \;
#	if (-e mrcv) \rm -f $(PROGS) endif

##############################################################################
#
# Not in bl3dfs distribution:
# older, test, and unsupported programs to make.
#

tifinfo : tifinfo.o
	$(CC) -o tifinfo tifinfo.o $(CLIBS)

rawmrc : rawmrc.o 
	$(CC) -o rawmrc rawmrc.o $(LDFLAGS) $(IILIBS) $(CLIBS)


addtomfft : addtomfft.o
	$(CC) -o addtomfft addtomfft.o -L$(LIBDIR) $(LIBS)

mrcinfo : mrcinfo.o 
	$(CC) -o mrcinfo mrcinfo.o -L$(LIBDIR) $(LIBS) 

mrcc : mrcc.o 
	$(CC) -o mrcc mrcc.o -L$(LIBDIR) $(LIBS)

mrctilt : mrctilt.o
	$(CC) -o mrctilt mrctilt.o -L$(LIBDIR) $(LIBS)

mrclog : mrclog.o 
	$(CC) -o mrclog mrclog.o -L$(LIBDIR) $(LIBS)

mrcshort : mrcshort.o 
	$(CC) -o mrcshort mrcshort.o -L$(LIBDIR) $(LIBS)

mrcmmm : mrcmmm.o 
	$(CC) -o mrcmmm mrcmmm.o -L$(LIBDIR) $(LIBS)

