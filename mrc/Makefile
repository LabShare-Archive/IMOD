#
# Makefile for mrc programs.
#
# $Id$
#

include ../configure

O = $(OBJEXT)
E = $(BINEXT)

MRCVOBJS  = mrcv.o mrcv_movie.o mrcv_draw.o mrcv_batch.o rgbv.o colormap.o

PROGS     = mrc2tif$(E) raw2mrc$(E) tif2mrc$(E) mrcbyte$(E) mrcx$(E) \
 mrctaper$(E) dm3props$(E) nad_eed_3d$(E) ctfphaseflip$(E) \
 $(RGBPROGS)


IILIBS    =  $(LIIMOD) $(LCFSHR) $(TIFFLIBS)

all : $(PROGS)

##############################################################################
# make programs.
#

raw2mrc$(E) : raw2mrc.$(O) 
	$(CC) -o raw2mrc raw2mrc.$(O)  $(LDFLAGS) $(IILIBS) $(CLIBS)

tif2mrc$(E) : tif2mrc.$(O) tiff.$(O) 
	$(CC) -o tif2mrc tif2mrc.$(O) tiff.$(O) $(LDFLAGS) $(IILIBS) $(CLIBS)

mrc2tif$(E) : mrc2tif.$(O) tiff.$(O)
	$(CC) -o  mrc2tif  mrc2tif.$(O) tiff.$(O) $(LDFLAGS) $(IILIBS) $(CLIBS)

mrcx$(E) : mrcx.$(O)
	$(CC) -o mrcx mrcx.$(O) $(LDFLAGS) $(IILIBS) $(CLIBS)

mrcbyte$(E) : mrcbyte.$(O) 
	$(CC) -o mrcbyte mrcbyte.$(O) $(LDFLAGS) $(IILIBS)  $(CLIBS)

mrctaper$(E) : mrctaper.$(O) 
	$(CC) -o mrctaper mrctaper.$(O) $(LDFLAGS) $(IILIBS)  $(CLIBS)

dm3props$(E) : dm3props.$(O) 
	$(CC) -o dm3props dm3props.$(O) $(LDFLAGS) $(IILIBS)  $(CLIBS)

nad_eed_3d$(E) : nad_eed_3d.$(O) nrutil.$(O)
	$(CC) -o nad_eed_3d nad_eed_3d.$(O) nrutil.$(O) $(LDFLAGS) $(IILIBS) \
	$(CLIBS)

ctfphaseflip$(E) : ctfphaseflip.$(O) ctfutils.$(O)
	$(CXX) -o ctfphaseflip ctfphaseflip.$(O) ctfutils.$(O) $(LDFLAGS) \
	$(IILIBS) $(LIFFT) $(CLIBS)

ctfutils.$(O) : ../ctfplotter/ctfutils.cpp
	$(CXX) -c -o ctfutils.$(O) $(CXXFLAGS) ../ctfplotter/ctfutils.cpp


#
# SGI only programs
#
frommrc : frommrc.$(O)
	$(CC) -o frommrc frommrc.$(O) $(LDFLAGS) $(IILIBS) -limage $(CLIBS)

tomrc : tomrc.$(O) 
	$(CC) -o tomrc tomrc.$(O) $(LDFLAGS) $(IILIBS) -limage $(CLIBS)

mrcv : $(MRCVOBJS) 
	$(CC)  -o mrcv $(MRCVOBJS) $(LDFLAGS) -limod $(IILIBS) \
	-lgutil -limage -lfm -lgl $(XLIBS) $(CLIBS)



##############################################################################
# install programs

install : $(PROGS)
	$(STRIP) $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean : 
	$(FIND) . -type f -name "*.o*" -exec rm "{}" \;
	$(FIND) . -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "core*" -exec rm "{}" \;
	$(FIND) . -type f -name "*.exe" -exec rm "{}" \;
	-\rm -f $(PROGS)

##############################################################################
#
# Not in bl3dfs distribution:
# older, test, and unsupported programs to make.
#

commonline$(E) : commonline.$(O) 
	$(CC) -o commonline commonline.$(O) $(LDFLAGS) $(IILIBS)  \
	$(LIFFT) $(CLIBS)

tifinfo$(E) : tifinfo.$(O)
	$(CC) -o tifinfo tifinfo.$(O) $(CLIBS)

rawmrc$(E) : rawmrc.$(O) 
	$(CC) -o rawmrc rawmrc.$(O) $(LDFLAGS) $(IILIBS) $(CLIBS)


addtomfft$(E) : addtomfft.$(O)
	$(CC) -o addtomfft addtomfft.$(O) -L$(LIBDIR) $(LIBS)

mrcinfo$(E) : mrcinfo.$(O) 
	$(CC) -o mrcinfo mrcinfo.$(O) -L$(LIBDIR) $(LIBS) 

mrcc$(E) : mrcc.$(O) 
	$(CC) -o mrcc mrcc.$(O) -L$(LIBDIR) $(LIBS)

mrctilt$(E) : mrctilt.$(O)
	$(CC) -o mrctilt mrctilt.$(O) -L$(LIBDIR) $(LIBS)

mrclog$(E) : mrclog.$(O) 
	$(CC) -o mrclog mrclog.$(O) -L$(LIBDIR) $(LIBS)

mrcshort$(E) : mrcshort.$(O) 
	$(CC) -o mrcshort mrcshort.$(O) -L$(LIBDIR) $(LIBS)

mrcmmm$(E) : mrcmmm.$(O) 
	$(CC) -o mrcmmm mrcmmm.$(O) -L$(LIBDIR) $(LIBS)
