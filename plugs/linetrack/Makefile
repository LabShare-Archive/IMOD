include ../../configure

MAKELIBSO = $(MAKELIB)

# Debug attempt?
# ld -shared -all -L/druid/lib -show_unref -no_unresolved -o linetrack.so linetrack.o reducepts.o linegui.o -limod -lXm -lX11 -lXt -lftn -lm -lc /usr/lib/crtn.o
#


PLUGS = linetrack.so

OBJS  = linetrack.o linesubs.o reducepts.o linegui.o conttrack.o


all : $(PLUGS)

install : $(PLUGS)
	$(CP) -f $(PLUGS) $(IMOD_PLUGIN_DIR)

linetrack.so : $(OBJS)
	$(MAKEF77PLUGIN) linetrack.so $(OBJS)

#linegui.o : linegui.c
#	$(CC) $(CFLAGS) -I/home/kremer/src/IMOD/include -c linegui.c

# Debug
#linetrack.o : linetrack.f
#	$(FC) -g -c linetrack.f

clean :
	$(FIND) . -type f -name "*.o"  -exec rm "{}" \;
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;
	$(FIND) . -type f -name "*.so" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
	$(FIND) . -type f -name "so_locations" -exec rm "{}" \;




