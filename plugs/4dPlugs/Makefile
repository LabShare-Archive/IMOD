#
# Plugin modules for using 4D Scope data.
#

#
# Autoconfig for IMOD source.
#
include ../../configure

# If the above is not included then
# select how to make the shared library.
#
# SGI IRIX 5.3
# MAKELIBSO = ld -shared -all -o
#
# SUN SOLARIS
# MAKELIBSO = ld -G -o
#MAKELIBSO = $(MAKELIB)

#
# If IMOD_PLUGIN_DIR isn't set up in the environment
# select a place to install the plugs.
# IMOD_PLUGIN_DIR = /usr/local/IMOD/plugins
#

PLUGS = matchPoint.so label.so lineage.so time.so mocp.so

all : $(PLUGS)

install : $(PLUGS)
	$(CP) -f $(PLUGS) $(IMOD_PLUGIN_DIR)

clean : 
	$(FIND) . -type f -name "*.o"  -exec rm "{}" \;
	$(FIND) . -type f -name "*~"   -exec rm "{}" \;
	$(FIND) . -type f -name "*.so" -exec rm "{}" \;
	$(FIND) . -type f -name "core" -exec rm "{}" \;

###########################################################################

label.so : label.o
	$(MAKELIBSO) label.so label.o

# Lineage display  plugin.
olineage.so : olineage.o ps.o
	$(MAKELIBSO) olineage.so olineage.o ps.o

# Lineage display  plugin.
lineage.so : lineage.o ps.o cellstat.o
	$(MAKELIBSO) lineage.so lineage.o ps.o cellstat.o

# Automatic match point plugin.
matchPoint.so : matchPoint.o
	$(MAKELIBSO) matchPoint.so matchPoint.o

# Improved time controls plugin
time.so : time.o
	$(MAKELIBSO) time.so time.o

# Improved model, object, contour and point controls plugin.
mocp.so : mocp.o
	$(MAKELIBSO) mocp.so mocp.o


