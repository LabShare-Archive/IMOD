#####################
# Makefile for clip
#
# $Id$
# Log at end
#
include ../configure

O = $(OBJEXT)
OBJS = clip.$(O) clip_proc.$(O) clip_transform.$(O) clip_io.$(O)\
	correlation.$(O) fft.$(O) filter.$(O)

PROG = clip$(BINEXT)

all : $(PROG)

# DNM 10/24/04: removed F77LIBS now that libifft is C
$(PROG) : $(OBJS)
	$(CC) -o clip $(OBJS) $(LDFLAGS)  \
	$(LIIMOD) $(LCFSHR) $(LIFFT) $(TIFFLIBS) $(LIBS)

install : $(PROG)
	$(STRIP) $(PROG)
	$(CP) $(PROG) $(INSTBINDIR)

clean : 
	$(FIND) . -type f -name "*.o*" -exec rm "{}" \;
	$(FIND) . -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "core*" -exec rm "{}" \;
	$(FIND) . -type f -name "clip" -exec rm "{}" \;
	$(FIND) . -type f -name "clip.exe" -exec rm "{}" \;

$(OBJS) : clip.h
clip.$(O) : ../.version

#  $Log$
#  Revision 3.6  2005/11/23 16:25:11  mast
#  Use strip variable
#
#  Revision 3.5  2004/10/24 21:21:19  mast
#  Changes for libifft now being C version
#
#  Revision 3.4  2003/11/01 14:36:02  mast
#  Put libiimod befor libimod because of new error calls
#
#  Revision 3.3  2003/10/24 02:30:40  mast
#  Changes for windows/Intel compiler
#
#  Revision 3.2  2003/02/28 18:09:32  mast
#  Changes for cygwin/windows
#
#  Revision 3.1  2002/11/05 23:25:43  mast
#  Add -limod to call copyright function
#
#  Revision 3.0  2001/11/29 18:10:05  rickg
#  *** empty log message ***
#
#  Revision 1.2  2001/11/27 15:59:17  mast
#  Add dependency on .version file
#
