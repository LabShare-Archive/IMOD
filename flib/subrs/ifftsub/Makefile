include ../../configure

OBJS = cmplft.o diprp.o fft1d.o hermft.o inv21.o mdftkd.o mr_tfft.o\
 mr_tfft_nyq.o odfft.o real_ft.o realft.o rsymft.o sdiad.o srfp.o todfft.o

all: $(OBJS)

clean:
	$(FIND) . -type f -name "*.o" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
