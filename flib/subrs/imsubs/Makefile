include ../../configure
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#  Revision 3.2  2003/10/24 02:31:04  mast
#  Changes for windows/Intel compiler
#
#  Revision 3.1  2002/02/26 23:01:22  mast
#  Added nbytes_and_flags to test for structure of extended header
#

O        = $(OBJEXT)
OBJS = blockio.$(O) altskip.$(O) icllim.$(O) imopen.$(O) imposn.$(O) \
 imunit.$(O) irdhdr.$(O) irdlin.$(O) irdpas.$(O) irepak.$(O) isetdn.$(O) \
 iwrlin.$(O) iwrpas.$(O) convert_vms.$(O) nbytes_and_flags.$(O) \
 irdbinned.$(O)

all: $(OBJS)

blockio.$(O): environ.h
$(OBJS):   imsubs.inc

clean:
	$(FIND) . -type f -name "*.o*" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
