include ../configure

O        = $(OBJEXT)
BDOBJS = beadtrack.$(O) proc_model.$(O) proc_vars.$(O) solve_xyzd.$(O) tltcntrl.$(O) \
 funct.$(O) map_vars.$(O) tracksubs.$(O) tiltali.$(O) fill_matrices.$(O) alivar.$(O)

PROG = beadtrack$(BINEXT)

all: $(PROG)

#tiltali.$(O): tiltali.f90
#	$(FC) -c $(NOOPFFLAGS) tiltali.f90
$(PROG): $(BDOBJS)
	$(FC) -o beadtrack $(BDOBJS) $(LDFFLAGS) $(LSMLMOD) $(LIMOD) \
	 $(IMLIBS) $(FORTCLIBS) $(TILTALILIB) $(LIFFT) $(LAPACKLIBS)

beadtrack.$(O):   $(INCDIR)/smallmodel.inc90 tltcntrl.$(O) tracksubs.$(O)
proc_model.$(O):  $(INCDIR)/smallmodel.inc90
tracksubs.$(O):   $(INCDIR)/smallmodel.inc90
proc_vars.$(O):   tltcntrl.$(O)
tiltali.$(O):     tltcntrl.$(O)
tltcntrl.$(O):    alivar.$(O)

solve_xyzd.$(O):  ../tiltalign/solve_xyzd.f90  alivar.$(O)
	$(FC) $(FFLAGS) -c -o solve_xyzd.$(O) ../tiltalign/solve_xyzd.f90

funct.$(O):       ../tiltalign/funct.f90  alivar.$(O)
	$(FC) $(FFLAGS) -c -o funct.$(O) ../tiltalign/funct.f90

map_vars.$(O):    ../tiltalign/map_vars.f90  alivar.$(O)
	$(FC) $(FFLAGS) -c -o map_vars.$(O) ../tiltalign/map_vars.f90

fill_matrices.$(O):    ../tiltalign/fill_matrices.f90
	$(FC) $(FFLAGS) -c -o fill_matrices.$(O) ../tiltalign/fill_matrices.f90

alivar.$(O):    ../tiltalign/alivar.f90
	$(FC) $(FFLAGS) -c -o alivar.$(O) ../tiltalign/alivar.f90



install: $(PROG)
	$(STRIP) $(PROG)
	$(CP) $(PROG) $(INSTBINDIR)

clean:
	-\rm -f *.$(OBJEXT) *.exe *~ beadtrack alivar.mod tltcntrl.mod \
		arraymaxes.mod mapsep.mod cgpixels.mod

