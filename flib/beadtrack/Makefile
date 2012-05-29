include ../configure

O        = $(OBJEXT)
BDOBJS = beadtrack.$(O) proc_model.$(O) proc_vars.$(O) solve_xyzd.$(O) tltcntrl.$(O) \
 funct.$(O) map_vars.$(O) tracksubs.$(O) tiltali.$(O) fill_matrices.$(O) alivar.$(O)

PROG = beadtrack$(BINEXT)

all: $(PROG)

tiltali.$(O): tiltali.f
	$(FC) -c $(NOOPFFLAGS) tiltali.f

$(PROG): $(BDOBJS)
	$(FC) -o beadtrack $(BDOBJS) $(LDFFLAGS) $(LSMLMOD) $(LIMOD) \
	 $(IMLIBS) $(FORTCLIBS) $(TILTALILIB) $(LAPACKLIBS)

beadtrack.$(O):   $(INCDIR)/smallmodel.inc $(INCDIR)/statsize.inc tltcntrl.$(O) \
	tracksubs.$(O)
proc_model.$(O):  $(INCDIR)/smallmodel.inc
tracksubs.$(O):   $(INCDIR)/smallmodel.inc  $(INCDIR)/statsize.inc
proc_vars.$(O):   tltcntrl.$(O)
tiltali.$(O):     tltcntrl.$(O)
tltcntrl.$(O):    alivar.$(O)

solve_xyzd.$(O):  ../tiltalign/solve_xyzd.f  alivar.$(O)
	$(FC) $(FFLAGS) -c -o solve_xyzd.$(O) ../tiltalign/solve_xyzd.f

funct.$(O):       ../tiltalign/funct.f  alivar.$(O)
	$(FC) $(FFLAGS) -c -o funct.$(O) ../tiltalign/funct.f

map_vars.$(O):    ../tiltalign/map_vars.f  alivar.$(O)
	$(FC) $(FFLAGS) -c -o map_vars.$(O) ../tiltalign/map_vars.f

fill_matrices.$(O):    ../tiltalign/fill_matrices.f
	$(FC) $(FFLAGS) -c -o fill_matrices.$(O) ../tiltalign/fill_matrices.f

alivar.$(O):    ../tiltalign/alivar.f
	$(FC) $(FFLAGS) -c -o alivar.$(O) ../tiltalign/alivar.f



install: $(PROG)
	$(STRIP) $(PROG)
	$(CP) $(PROG) $(INSTBINDIR)

clean:
	-\rm -f *.o* *.exe *~ beadtrack alivar.mod tltcntrl.mod arraymaxes.mod \
	mapsep.mod cgpixels.mod

