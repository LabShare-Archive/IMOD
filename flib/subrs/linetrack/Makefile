include ../../configure

FFLAGS = $(LIBFFLAGS)
CFLAGS = $(LIBCFLAGS)

O = $(OBJEXT)
OBJS  = linetrack.$(O) linesubs.$(O) reducepts.$(O) conttrack.$(O) b3dputs.$(O)


all : $(OBJS)


clean :
	$(FIND) . -type f -name "*.o*"  -exec rm "{}" \;
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
