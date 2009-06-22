include ../../configure
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#  Revision 3.4  2004/04/18 22:22:41  mast
#  Added LIBFFLAGS etc for 64-bit compilation
#
#  Revision 3.3  2003/12/17 22:11:56  mast
#  Added irdbinned
#
#  Revision 3.2  2003/10/24 02:31:04  mast
#  Changes for windows/Intel compiler
#
#  Revision 3.1  2002/02/26 23:01:22  mast
#  Added nbytes_and_flags to test for structure of extended header
#

CFLAGS = $(LIBCFLAGS)
FFLAGS = $(LIBFFLAGS)

O        = $(OBJEXT)
OBJS = blockio.$(O) altskip.$(O) icllim.$(O) imopen.$(O) imposn.$(O) \
 imunit.$(O) irdhdr.$(O) irdlin.$(O) irdpas.$(O) irepak.$(O) isetdn.$(O) \
 iwrlin.$(O) iwrpas.$(O) convert_vms.$(O) nbytes_and_flags.$(O) \
 irdbinned.$(O)
MODULES = imsubs.$(O)

all: $(OBJS) $(MODULES)

blockio.$(O): environ.h
$(OBJS):   imsubs.$(O)

clean:
	$(FIND) . -type f -name "*.o*" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
