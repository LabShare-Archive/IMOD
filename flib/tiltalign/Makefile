include ../configure
O        = $(OBJEXT)
E        = $(BINEXT)

# Objects that depend on alivar:
ALIDEP = tiltalign.$(O) input_model.$(O) input_vars.$(O) patchtrack.$(O) \
 solve_xyzd.$(O) funct.$(O) map_vars.$(O) beamtilt.$(O) robustfit.$(O) 
# Other objects
TAOBJS =  $(ALIDEP) find_surfaces.$(O) fill_matrices.$(O) alivar.$(O) \

PROGS = tiltalign$(E) xfforfidless$(E)
#PROGS = tiltalign$(E)

all: $(PROGS)


tiltalign.$(O): tiltalign.f90
	$(FC) -c $(NOOPFFLAGS) tiltalign.f90

tiltalign$(E): $(TAOBJS)
	$(FC) -o tiltalign $(TAOBJS) $(LDFFLAGS) $(LSMLMOD) $(LIMOD) \
	 $(IMLIBS) $(FORTCLIBS) $(TILTALILIB)  $(LAPACKLIBS)

xfforfidless$(E): xfforfidless.$(O) fill_matrices.$(O)
	$(FC) -o xfforfidless xfforfidless.$(O) fill_matrices.$(O) \
	 $(LDFFLAGS) $(IMLIBS) $(FORTCLIBS)

input_model.$(O): $(INCDIR)/smallmodel.inc
$(ALIDEP):       alivar.$(O)

install: $(PROGS)
	$(STRIP) $(PROGS) 
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.$(OBJEXT) *.exe *~ tiltalign xfforfidless alivar.mod mapsep.mod  \
	arraymaxes.mod functvars.mod
