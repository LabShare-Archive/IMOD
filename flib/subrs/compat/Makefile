include ../../configure

CFLAGS = $(LIBCFLAGS)
FFLAGS = $(LIBFFLAGS)

OBJS = ransub.$(OBJEXT) ourrnd.$(OBJEXT) degtrig.$(OBJEXT) $(DATEOBJ)

all: $(OBJS)

clean:
	$(FIND) . -type f -name "*.$(OBJEXT)" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
