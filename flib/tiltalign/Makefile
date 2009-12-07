include ../configure
O        = $(OBJEXT)
E        = $(BINEXT)

TAOBJS = tiltalign.$(O) input_model.$(O) input_vars.$(O) \
 solve_xyzd.$(O) funct.$(O) map_vars.$(O) beamtilt.$(O) fill_matrices.$(O)

# PROGS = tiltalign$(E) xfforfidless$(E)
PROGS = tiltalign$(E)

all: $(PROGS)


tiltalign.$(O): tiltalign.f
	$(FC) -c $(NOOPFFLAGS) tiltalign.f

tiltalign$(E): $(TAOBJS)
	$(FC) -o tiltalign $(TAOBJS) $(LDFFLAGS) $(LSMLMOD) $(LIMOD) \
	 $(IMLIBS) $(FORTCLIBS) $(TILTALILIB)

xfforfidless$(E): xfforfidless.$(O) fill_matrices.$(O)
	$(FC) -o xfforfidless xfforfidless.$(O) fill_matrices.$(O) \
	 $(LDFFLAGS) $(IMLIBS) $(FORTCLIBS)

input_model.$(O): $(INCDIR)/smallmodel.inc
funct.$(O):       $(INCDIR)/alivar.inc
input_vars.$(O):  $(INCDIR)/alivar.inc
tiltalign.$(O):   $(INCDIR)/alivar.inc

install: $(PROGS)
	$(STRIP) $(PROGS) 
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.o* *.exe *~ tiltalign xfforfidless

