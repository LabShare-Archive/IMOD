include ../../configure
#
# 7/18/00 CER add move strupcase.o here from hvem
#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#  Revision 3.1  2002/08/18 23:01:58  mast
#  Added iclavgsd
#

O        = $(OBJEXT)
OBJS =  bigfft.$(O) bigfilt.$(O) cinterp.$(O) filter.$(O) icalc_angles.$(O) \
 icalc_matrix.$(O) iclcdn.$(O) iclden.$(O) iclscl.$(O) inside.$(O) \
 interp.$(O) qdinterp.$(O) get_nxyz.$(O) filterpart.$(O) cubinterp.$(O) \
 strupcase.$(O) iclavgsd.$(O)


all: $(OBJS)


clean:
	$(FIND) . -type f -name "*.o*" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;


