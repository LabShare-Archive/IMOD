#####################
# Makefile for clip
#
# $Id$
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
	$(FIND) . -type f -name "*.$(OBJEXT)" -exec rm "{}" \;
	$(FIND) . -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "core*" -exec rm "{}" \;
	$(FIND) . -type f -name "clip" -exec rm "{}" \;
	$(FIND) . -type f -name "clip.exe" -exec rm "{}" \;

$(OBJS) : clip.h
clip.$(O) : ../.version
