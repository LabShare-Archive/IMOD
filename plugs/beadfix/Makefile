include ../../configure

PLUGS = beadfix.so

all : $(PLUGS)

install : $(PLUGS)
#	$(CP) -f $(PLUGS) $(IMOD_PLUGIN_DIR)


beadfix.so : beadfix.o moc_beadfix.o
	$(MAKELIBSO) beadfix.so beadfix.o moc_beadfix.o

moc_beadfix.cpp : beadfix.h
	$(MOC) beadfix.h -o moc_beadfix.cpp



clean :
	$(FIND) . -type f -name "*.o"  -exec rm "{}" \;
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;
	$(FIND) . -type f -name "*.so" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
	$(FIND) . -type f -name "moc_*" -exec rm "{}" \;
	$(FIND) . -type f -name "so_locations" -exec rm "{}" \;




