#
# Example make file for imod plugins.
#


# Select how to make the shared library.
#
# SGI IRIX 5.3
MAKELIBSO = ld -shared -all -o

# SUN SOLARIS
# MAKELIBSO = ld -G -o


SHELL = /usr/bin/csh
PLUGS = testplug.so gfxshell.so testc++.so


all : $(PLUGS) minfo


testplug.so : plugshell.o
	$(MAKELIBSO) testplug.so plugshell.o

gfxshell.so : gfxshell.o
	$(MAKELIBSO) gfxshell.so gfxshell.o

minfo : minfo.o
	$(CC) -o minfo minfo.o -limod -lm -lc

meshcustom : meshcustom.o
	$(CC) -o meshcustom meshcustom.o -limod -lm -lc

testc++.so : imodPluginTest.o imodPluginWindow.o
	$(MAKELIBSO) testc++.so imodPluginTest.o imodPluginWindow.o -lC

clean :
	rm -rf *.so
	rm -rf *.o
	rm -rf *~
	rm -rf #*#
	rm -rf so_locations
	rm -rf minfo
