#
# Makefile for IMOD model utility programs
#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#  Revision 3.3  2003/02/27 16:43:46  mast
#  Remove imodsendevent
#
#  Revision 3.2  2002/09/13 22:08:17  mast
#  Added imodsendevent
#
#  Revision 3.1  2001/12/17 18:53:27  mast
#  Added imodfillin
#
include ../configure

PROGS = imodauto imodinfo imodmesh imodtrans imodmop imodcmopp \
rec2imod imod2rib imod2vrml imod2meta wmod2imod imodjoin imodextract \
imodsortsurf imodexplode patch2imod imod2patch echo2 imodfillin

all : $(PROGS)

imodinfo : imodinfo.o 
	$(CC) -o imodinfo imodinfo.o $(LDFLAGS) -limod $(CLIBS)

imodmesh : imodmesh.o mkmesh.o parselist.o
	$(CC) -o imodmesh mkmesh.o imodmesh.o parselist.o \
	$(LDFLAGS) -limod $(CLIBS)

imodtrans.o : imodtrans.c
	$(CC) $(CFALLBACK) -c imodtrans.c
imodtrans : imodtrans.o
	$(CC) -o imodtrans imodtrans.o $(LDFALLBACK) -limod $(CLIBS)

imodmop : imodmop.o moplib.o
	$(CC) -o imodmop imodmop.o moplib.o $(LDFLAGS) -limod \
	-liimod $(TIFFLIBS) $(CLIBS)

imodcmopp : imodcmopp.o moplib.o
	$(CC) -o imodcmopp imodcmopp.o moplib.o \
	$(LDFLAGS) -limod -liimod $(TIFFLIBS) $(CLIBS)

imodauto : imodauto.o 
	$(CC) -o imodauto imodauto.o $(LDFLAGS) -limod -liimod $(TIFFLIBS) \
	$(CLIBS)

rec2imod : rec2imod.o recfile.o
	$(CC) -o rec2imod rec2imod.o recfile.o $(LDFLAGS) -limod $(CLIBS)

imod2rib: imod2rib.o
	$(CC) -o imod2rib imod2rib.o $(LDFLAGS) -limod $(CLIBS)

imod2vrml : imod2vrml.o
	$(CC) -o imod2vrml imod2vrml.o $(LDFLAGS) -limod $(CLIBS)

imod2meta : imod2meta.o
	$(CC) -o imod2meta imod2meta.o $(LDFLAGS) -limod $(CLIBS)

wmod2imod : wmod2imod.o
	$(CC) -o wmod2imod wmod2imod.o $(LDFLAGS) -limod $(CLIBS)

imodjoin :  imodjoin.o parselist.o
	$(CC) -o imodjoin imodjoin.o parselist.o $(LDFLAGS) -limod $(CLIBS)

imodextract : imodextract.o parselist.o
	$(CC) -o imodextract imodextract.o parselist.o $(LDFLAGS) -limod $(CLIBS)

imodsortsurf : imodsortsurf.o parselist.o
	$(CC) -o imodsortsurf imodsortsurf.o parselist.o $(LDFLAGS) -limod $(CLIBS)

imodexplode : imodexplode.o parselist.o
	$(CC) -o imodexplode imodexplode.o parselist.o $(LDFLAGS) -limod $(CLIBS)

patch2imod : patch2imod.o 
	$(CC) -o patch2imod patch2imod.o $(LDFLAGS) -limod $(CLIBS)

imod2patch : imod2patch.o 
	$(CC) -o imod2patch imod2patch.o $(LDFLAGS) -limod $(CLIBS)

echo2 : echo2.o 
	$(CC) -o echo2 echo2.o $(LDFLAGS)

imodfillin : imodfillin.o parselist.o
	$(CC) -o imodfillin imodfillin.o parselist.o $(LDFLAGS) -limod $(CLIBS)

install : $(PROGS)
	strip $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

cyginstall : $(PROGS)
	$(FIND) . -name "*.exe" -exec strip "{}" \; -exec $(CP) "{}" $(INSTBINDIR) \;

clean : 
	$(FIND) . -type f -name "*.o" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "core*" -exec rm "{}" \;
	$(FIND) . -type f -name "*.exe" -exec rm "{}" \;
	$(FIND) . -type f -perm 755 -exec rm "{}" \;


##############################################################################
# unsupported programs

imod2nff : imod2nff.o
	$(CC) -o imod2nff imod2nff.o $(LDFLAGS) -limod $(CLIBS)

imod2synu : imod2synu.o
	$(CC) -o imod2synu imod2synu.o $(LDFLAGS) -limod $(CLIBS)

imod2wmod : imod2wmod.o $(IMODELOBJS)
	$(CC) -o imod2wmod imod2wmod.o $(LDFLAGS) -limod $(CLIBS)

# oldpore, help edit old yeast pore models
oldpore : oldpore.o
	$(CC) -o oldpore oldpore.o $(LDFLAGS) -limod $(CLIBS)

imodclean : imodclean.o
	$(CC) -o imodclean imodclean.o $(LDFLAGS) -limod $(CLIBS)

.C.o:
	CC -c $(CFLAGS) $<

imodold : imodold.o 
	$(CC) -o imodold imodold.o $(LDFLAGS) -limod $(CLIBS)

imodmkhv : imodmkhv.o 
	$(CC) -o imodmkhv imodmkhv.o $(LDFLAGS) -limod $(CLIBS)





