include ../configure

# 7/17/00 CER add -ldtrigs to library seach

O        = $(OBJEXT)
BDOBJS = beadtrack.$(O) proc_model.$(O) proc_vars.$(O) solve_xyzd.$(O)\
 funct.$(O) metro.$(O) map_vars.$(O) tracksubs.$(O) tiltali.$(O)

PROG = beadtrack$(BINEXT)

all: $(PROG)

FFLAGS = $(FFALLBACK)

tiltali.$(O): tiltali.f
	$(FC) -c $(NOOPFFALLBACK) tiltali.f

$(PROG): $(BDOBJS)
	$(FC) -o beadtrack $(BDOBJS) $(LDFFLAGS) $(LDFALLBACK) $(IMLIBS) \
	$(FORTCLIBS) $(TILTALILIB)

beadtrack.$(O):   $(INCDIR)/model.inc  $(INCDIR)/statsize.inc \
 $(INCDIR)/alivar.inc tltcntrl.inc
proc_model.$(O):  $(INCDIR)/model.inc
tracksubs.$(O):   $(INCDIR)/model.inc  $(INCDIR)/statsize.inc
funct.$(O):       $(INCDIR)/alivar.inc
proc_vars.$(O):   $(INCDIR)/alivar.inc tltcntrl.inc
tiltali.$(O):     $(INCDIR)/alivar.inc tltcntrl.inc

metro.$(O):       ../tiltalign/metro.f
	$(FC) $(FFLAGS) -c -o metro.$(O) ../tiltalign/metro.f

solve_xyzd.$(O):  ../tiltalign/solve_xyzd.f
	$(FC) $(FFLAGS) -c -o solve_xyzd.$(O) ../tiltalign/solve_xyzd.f

funct.$(O):       ../tiltalign/funct.f
	$(FC) $(FFLAGS) -c -o funct.$(O) ../tiltalign/funct.f

map_vars.$(O):    ../tiltalign/map_vars.f
	$(FC) $(FFLAGS) -c -o map_vars.$(O) ../tiltalign/map_vars.f


install: $(PROG)
	$(STRIP) $(PROG)
	$(CP) $(PROG) $(INSTBINDIR)

clean:
	-\rm -f *.o* *.exe *~ beadtrack

