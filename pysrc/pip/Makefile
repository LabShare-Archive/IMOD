include ../../configure
# Makefile for the pip extension module - distributes its own source
#
# $Id$
#
#  $Log$
#  Revision 1.2  2006/10/16 02:12:48  mast
#  Removed b3dutil.h
#
#  Revision 1.1  2006/09/26 23:03:56  mast
#  Added to package
#
#

all:	parse_params.c parse_params.h $(TWOPYTHONS) 
	python setup.py build

twopys:	parse_params.c parse_params.h
	"$(ALTPYTHON)"/python setup.py build

parse_params.c:	../../libcfshr/parse_params.c
	$(CP) ../../libcfshr/parse_params.c .

parse_params.h:	../../include/parse_params.h
	$(CP) ../../include/parse_params.h .

install: all
	$(CP) build/lib.*/pip.* $(INSTDIR)/pylib
	if [ `pwd` != $(INSTDIR)/pysrc/pip ] ; \
		then mkdir -p $(INSTDIR)/pysrc/pip ; fi
	if [ `pwd` != $(INSTDIR)/pysrc/pip ] ; \
		then $(CP) *.c *.h setup.py $(INSTDIR)/pysrc/pip ; fi

clean:
	$(FIND) . -type f -name "parse_params.*"  -exec rm "{}" \;
	$(FIND) . -depth -type d -name "build"  -exec rm -rf "{}" \;
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;
