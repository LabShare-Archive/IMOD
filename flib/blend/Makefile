include ../configure


PROGS = blendmont reducemont

BLOBJS = blendmont.o bsubs.o shuffler.o smoothgrid.o sdsearch.o sdcalc.o\
 setoverlap.o

REDOBJS = reducemont.o shuffler.o setoverlap.o

SLOPOBJS = sloppymont.o shuffler.o setoverlap.o

all: $(PROGS)

blendmont: $(BLOBJS)
	$(FC)  -o blendmont $(LDFLAGS) $(BLOBJS) $(IMLIBS) $(FORTCLIBS)

reducemont: $(REDOBJS)
	$(FC) -o reducemont $(LDFLAGS) $(REDOBJS) $(IMLIBS) $(FORTCLIBS)

sloppymont: $(SLOPOBJS)
	$(FC) -o sloppymont $(LDFLAGS) $(SLOPOBJS) $(IMLIBS) $(FORTCLIBS)

install: $(PROGS)
	strip $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.o *~ blendmont reducemont
