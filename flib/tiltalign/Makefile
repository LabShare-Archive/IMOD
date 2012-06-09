include ../configure
O        = $(OBJEXT)
E        = $(BINEXT)

TAOBJS = tiltalign.$(O) input_model.$(O) input_vars.$(O) find_surfaces.$(O) \
 solve_xyzd.$(O) funct.$(O) map_vars.$(O) beamtilt.$(O) fill_matrices.$(O) alivar.$(O)

# PROGS = tiltalign$(E) xfforfidless$(E)
PROGS = tiltalign$(E)

all: $(PROGS)


tiltalign.$(O): tiltalign.f90
	$(FC) -c $(NOOPFFLAGS) tiltalign.f90

tiltalign$(E): $(TAOBJS)
	$(FC) -o tiltalign $(TAOBJS) $(LDFFLAGS) $(LSMLMOD) $(LIMOD) \
	 $(IMLIBS) $(FORTCLIBS) $(TILTALILIB)  $(LAPACKLIBS)

xfforfidless$(E): xfforfidless.$(O) fill_matrices.$(O)
	$(FC) -o xfforfidless xfforfidless.$(O) fill_matrices.$(O) \
	 $(LDFFLAGS) $(IMLIBS) $(FORTCLIBS)

input_model.$(O): $(INCDIR)/smallmodel.inc alivar.$(O)
funct.$(O):       alivar.$(O)
input_vars.$(O):  alivar.$(O)
tiltalign.$(O):   alivar.$(O)
map_vars.$(O):    alivar.$(O)
solve_xyzd.$(O):  alivar.$(O)
beamtilt.$(O):	  alivar.$(O)

install: $(PROGS)
	$(STRIP) $(PROGS) 
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.o* *.exe *~ tiltalign xfforfidless alivar.mod
