include ../../configure

OBJS = amat_to_rotmag.o amat_to_rotmagstr.o rotmag_to_amat.o\
 rotmagstr_to_amat.o xfapply.o xfinvert.o xflincom.o xfcopy.o xfmult.o xfrdall.o\
 xfread.o xfunit.o xfwrite.o

all: $(OBJS)

clean:
	$(FIND) . -type f -name "*.o" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
