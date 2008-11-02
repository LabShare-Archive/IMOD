include ../configure

O        = $(OBJEXT)
BDOBJS = beadtrack.$(O) proc_model.$(O) proc_vars.$(O) solve_xyzd.$(O)\
 funct.$(O) map_vars.$(O) tracksubs.$(O) tiltali.$(O) find_surfaces.$(O) \
 fill_matrices.$(O)

PROG = beadtrack$(BINEXT)

all: $(PROG)

tiltali.$(O): tiltali.f
	$(FC) -c $(NOOPFFLAGS) tiltali.f

$(PROG): $(BDOBJS)
	$(FC) -o beadtrack $(BDOBJS) $(LDFFLAGS) $(LSMLMOD) $(LIMOD) \
	 $(IMLIBS) $(FORTCLIBS) $(TILTALILIB)

beadtrack.$(O):   $(INCDIR)/smallmodel.inc  $(INCDIR)/statsize.inc \
 $(INCDIR)/alivar.inc tltcntrl.inc
proc_model.$(O):  $(INCDIR)/smallmodel.inc
tracksubs.$(O):   $(INCDIR)/smallmodel.inc  $(INCDIR)/statsize.inc
funct.$(O):       $(INCDIR)/alivar.inc
proc_vars.$(O):   $(INCDIR)/alivar.inc tltcntrl.inc
tiltali.$(O):     $(INCDIR)/alivar.inc tltcntrl.inc

solve_xyzd.$(O):  ../tiltalign/solve_xyzd.f
	$(FC) $(FFLAGS) -c -o solve_xyzd.$(O) ../tiltalign/solve_xyzd.f

funct.$(O):       ../tiltalign/funct.f
	$(FC) $(FFLAGS) -c -o funct.$(O) ../tiltalign/funct.f

map_vars.$(O):    ../tiltalign/map_vars.f
	$(FC) $(FFLAGS) -c -o map_vars.$(O) ../tiltalign/map_vars.f

fill_matrices.$(O):    ../tiltalign/fill_matrices.f
	$(FC) $(FFLAGS) -c -o fill_matrices.$(O) ../tiltalign/fill_matrices.f

find_surfaces.$(O):       ../tiltalign/find_surfaces.f
	$(FC) $(FFLAGS) -c -o find_surfaces.$(O) ../tiltalign/find_surfaces.f


install: $(PROG)
	$(STRIP) $(PROG)
	$(CP) $(PROG) $(INSTBINDIR)

clean:
	-\rm -f *.o* *.exe *~ beadtrack

