include ../../configure

FFLAGS = $(LIBFFLAGS)

O        = $(OBJEXT)
OBJS = cmplft.$(O) diprp.$(O) fft1d.$(O) hermft.$(O) inv21.$(O) mdftkd.$(O) \
 mr_tfft.$(O) mr_tfft_nyq.$(O) odfft.$(O) real_ft.$(O) realft.$(O) \
 rsymft.$(O) sdiad.$(O) srfp.$(O) todfft.$(O)

all: $(OBJS)

clean:
	$(FIND) . -type f -name "*.o*" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
