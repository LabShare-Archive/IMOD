include ../configure
O        = $(OBJEXT)

# 7/16/00 CER add dtrigs to library search

TAOBJS = tiltalign.$(O) input_model.$(O) input_vars.$(O) find_surfaces.$(O) \
 solve_xyzd.$(O) funct.$(O) metro.$(O) map_vars.$(O)

PROG = tiltalign$(BINEXT)

all: $(PROG)


tiltalign.$(O): tiltalign.f
	$(FC) -c $(NOOPFFLAGS) tiltalign.f

$(PROG): $(TAOBJS)
	$(FC) -o tiltalign $(TAOBJS) $(LDFFLAGS) $(LDFLAGS) $(IMLIBS) \
	$(FORTCLIBS) $(TILTALILIB)


input_model.$(O): $(INCDIR)/model.inc
funct.$(O):       $(INCDIR)/alivar.inc
input_vars.$(O):  $(INCDIR)/alivar.inc
tiltalign.$(O):   $(INCDIR)/alivar.inc

install: $(PROG)
	strip $(PROG) 
	$(CP) $(PROG) $(INSTBINDIR)

clean:
	-\rm -f *.o* *.exe *~ tiltalign

