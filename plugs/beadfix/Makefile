include ../../configure

#MAKELIBSO = $(MAKELIB)

PLUGS = beadfix.so

all : $(PLUGS)

install : $(PLUGS)
	$(CP) -f $(PLUGS) $(IMOD_PLUGIN_DIR)


beadfix.so : beadfix.o
	$(MAKELIBSO) beadfix.so beadfix.o

#beadfix.o : beadfix.c
#	$(CC) $(CFLAGS) -c -I$(INCDIR) beadfix.c


clean :
	$(FIND) . -type f -name "*.o"  -exec rm "{}" \;
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;
	$(FIND) . -type f -name "*.so" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
	$(FIND) . -type f -name "so_locations" -exec rm "{}" \;




