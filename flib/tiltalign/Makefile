include ../configure

# 7/16/00 CER add dtrigs to library search

TAOBJS = tiltalign.o input_model.o input_vars.o find_surfaces.o solve_xyzd.o\
 funct.o metro.o map_vars.o

all: tiltalign


tiltalign.o: tiltalign.f
	$(FC) -c $(NOOPFFLAGS) -g tiltalign.f

tiltalign: $(TAOBJS)
	$(FC) -o tiltalign $(TAOBJS) $(LDFLAGS) $(IMLIBS) $(FORTCLIBS) $(TILTALILIB)


input_model.o: $(INCDIR)/model.inc
funct.o:       alivar.inc
input_vars.o:  alivar.inc
tiltalign.o:   alivar.inc

install: tiltalign
	strip tiltalign 
	$(CP) tiltalign $(INSTBINDIR)

clean:
	-\rm -f *.o *~ tiltalign

