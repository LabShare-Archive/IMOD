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

OBJS =  bigfft.o bigfilt.o cinterp.o filter.o icalc_angles.o icalc_matrix.o\
 iclcdn.o iclden.o iclscl.o inside.o interp.o qdinterp.o get_nxyz.o\
 filterpart.o cubinterp.o strupcase.o iclavgsd.o


all: $(OBJS)


clean:
	$(FIND) . -type f -name "*.o" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;


