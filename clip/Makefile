#####################
# Makefile for clip
#
#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#  Revision 3.0  2001/11/29 18:10:05  rickg
#  *** empty log message ***
#
#  Revision 1.2  2001/11/27 15:59:17  mast
#  Add dependency on .version file
#
#
include ../configure

OBJS = clip.o clip_proc.o clip_transform.o clip_io.o\
	correlation.o fft.o filter.o

all : clip

# DNM 3/12/01: put $(F77LIBS) back on the link list just in case it's needed
clip : $(OBJS)
	@echo "Making clip: libifft is in the flib source code."
	$(FLCC) -o clip $(OBJS) $(LDFLAGS)  \
	-limod -liimod -lifft $(TIFFLIBS) $(LIBS) $(F77LIBS)

install : clip
	strip clip
	$(CP) clip $(INSTBINDIR)

clean : 
	$(FIND) . -type f -name "*.o" -exec rm "{}" \;
	$(FIND) . -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "core" -exec rm "{}" \;
	$(FIND) . -type f -name "clip" -exec rm "{}" \;

$(OBJS) : clip.h
clip.o : ../.version


#
# If there is no fortran compiler available and hence
# no fftlib then uncomment the following rule
# to make a clip with to fft ability.
#.c.o:
#	$(CC) -c $(CFLAGS) -DNOFFTLIB $<

