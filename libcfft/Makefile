#
# Makefile for libcfft.a
#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$

include ../configure

LIBI   = libifft.$(DLLEXT)
O     = $(OBJEXT)
CFLAGS = $(LIBCFLAGS)

CFFTOBJS = cmplft.$(O) hermft.$(O) odfft.$(O) srfp.$(O) diprp.$(O) mdftkd.$(O)\
           realft.$(O) todfft.$(O) thrdfft.$(O)

all : $(LIBI)

$(LIBI) : $(CFFTOBJS)
	$(FIND) . -type f -name $(LIBI) -exec rm "{}" \;
	$(MAKELIB)$(LIBI) $(LIBIFFTDEF) $(CFFTOBJS)
	$(RANLIB) $(LIBI)
	$(CP) libifft.$(LIBEXT) $(LIBDIR)

install : $(LIBI)
	$(CP) $(LIBI) $(INSTLIBDIR)

clean : 
	$(FIND) . -type f -name $(LIBI) -exec rm "{}" \;
	$(FIND) . -type f -name "*.exp" -exec rm "{}" \;
	$(FIND) . -type f -name "*.o*"  -exec rm "{}" \;
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"  -exec rm "{}" \;
