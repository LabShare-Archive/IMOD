include ../../configure

OBJS = avgsd.o dirnm.o hdiag.o multr.o stepwz.o correl.o eigenv.o matinv.o\
 polyfit.o betacf.o fvalue.o nestanova.o betai.o gammln.o tvalue.o\
 weightpoly.o lsfit.o localpolyfit.o gaussj.o lsfit2.o

all: $(OBJS)

clean:
	$(FIND) . -type f -name "*.o" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;

$(OBJS): $(INCDIR)statsize.inc
