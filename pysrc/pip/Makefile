include ../../configure
# Makefile for the pip extension module - distributes its own source
#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#

all:	parse_params.c parse_params.h b3dutil.h $(TWOPYTHONS) 
	python setup.py build

twopys:	parse_params.c parse_params.h b3dutil.h
	"$(ALTPYTHON)"/python setup.py build

parse_params.c:	../../libimod/parse_params.c
	$(CP) ../../libimod/parse_params.c .

parse_params.h:	../../include/parse_params.h
	$(CP) ../../include/parse_params.h .

b3dutil.h:	../../include/b3dutil.h
	$(CP) ../../include/b3dutil.h .

install: all
	$(CP) build/lib.*/pip.* $(INSTDIR)/pylib
	if [ `pwd` != $(INSTDIR)/pysrc/pip ] ; \
		then mkdir -p $(INSTDIR)/pysrc/pip ; fi
	if [ `pwd` != $(INSTDIR)/pysrc/pip ] ; \
		then $(CP) *.c *.h setup.py $(INSTDIR)/pysrc/pip ; fi

clean:
	$(FIND) . -type f -name "parse_params.*"  -exec rm "{}" \;
	$(FIND) . -type f -name "b3dutil.h"  -exec rm "{}" \;
	$(FIND) . -depth -type d -name "build"  -exec rm -rf "{}" \;
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;
