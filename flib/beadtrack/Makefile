include ../configure

# 7/17/00 CER add -ldtrigs to library seach

BDOBJS = beadtrack.o proc_model.o proc_vars.o solve_xyzd.o\
 funct.o metro.o map_vars.o tracksubs.o findxf.o tiltali.o

all: beadtrack

FFLAGS = $(FFALLBACK)

tiltali.o: tiltali.f
	$(FC) -c $(NOOPFFALLBACK) tiltali.f

beadtrack: $(BDOBJS)
	$(FC) -o beadtrack $(BDOBJS) $(LDFALLBACK) $(IMLIBS) $(FORTCLIBS) $(TILTALILIB)

beadtrack.o:   $(INCDIR)/model.inc  $(INCDIR)/statsize.inc alivar.inc
findxf.o:      $(INCDIR)/statsize.inc
proc_model.o:  $(INCDIR)/model.inc
tracksubs.o:   $(INCDIR)/model.inc  $(INCDIR)/statsize.inc
funct.o:       alivar.inc
proc_vars.o:   alivar.inc
tiltali.o:     alivar.inc

metro.o:       metro.f
solve_xyzd.o:  solve_xyzd.f
funct.o:       funct.f
map_vars.o:    map_vars.f

alivar.inc:
	if (! -e alivar.inc) ln -s ../tiltalign/alivar.inc .

metro.f:
	if (! -e metro.f) ln -s ../tiltalign/metro.f .

funct.f:
	if (! -e funct.f) ln -s ../tiltalign/funct.f .

map_vars.f:
	if (! -e map_vars.f) ln -s ../tiltalign/map_vars.f .

solve_xyzd.f:
	if (! -e solve_xyzd.f) ln -s ../tiltalign/solve_xyzd.f .

install: beadtrack
	strip beadtrack
	$(CP) beadtrack $(INSTBINDIR)

clean:
	-\rm -f *.o *~ beadtrack

