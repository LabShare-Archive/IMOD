include ../../configure

FFLAGS = $(LIBFFLAGS)

O        = $(OBJEXT)
OBJS = avgsd.$(O) dirnm.$(O) hdiag.$(O) multr.$(O) stepwz.$(O) correl.$(O) \
 eigenv.$(O) matinv.$(O) polyfit.$(O) betacf.$(O) fvalue.$(O) nestanova.$(O) \
 betai.$(O) gammln.$(O) tvalue.$(O) amoeba.$(O) \
 weightpoly.$(O) lsfit.$(O) localpolyfit.$(O) gaussj.$(O) lsfit2.$(O)

all: $(OBJS)

clean:
	$(FIND) . -type f -name "*.o*" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;

$(OBJS): $(INCDIR)statsize.inc
