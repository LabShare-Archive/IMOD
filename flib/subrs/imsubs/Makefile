include ../../configure

OBJS = blockio.o altskip.o icllim.o imopen.o imposn.o imunit.o irdhdr.o\
 irdlin.o irdpas.o irepak.o isetdn.o iwrlin.o iwrpas.o convert_vms.o

all: $(OBJS)

blockio.o: environ.h
$(OBJS):   imsubs.inc

clean:
	$(FIND) . -type f -name "*.o" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
