#
# makefile for midas
#
include ../configure

OBJS = midas.o midas_cb.o midas_io.o midas_gl.o \
       midas_trans.o amat_to_rotmagstr.o gaussj.o


all : midas

midas : $(OBJS)
	$(CC) -o midas $(OBJS) $(LDFLAGS) \
	-limod -ldia -liimod $(TIFFLIBS) $(GLIBS) $(XLIBS) $(CLIBS)


$(OBJS) : midas.h

install : midas
	strip midas
	$(CP) midas $(INSTBINDIR)

clean :
	$(FIND) . -type f -name "*.o"  -exec rm "{}" \;
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"  -exec rm "{}" \;
	$(FIND) . -type f -name midas  -exec rm "{}" \;
