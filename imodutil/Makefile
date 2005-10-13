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
#  Revision 3.10  2005/08/18 04:16:35  mast
#  Added imodwincpu via a flag
#
#  Revision 3.9  2005/07/06 20:54:20  mast
#  Removed fallback for imodtrans; regular n32 build now works
#
#  Revision 3.8  2004/09/28 22:18:46  mast
#  Removed parselist entries since it is libimod now
#
#  Revision 3.7  2004/09/28 15:15:37  mast
#  Add parselist for imodinfo
#
#  Revision 3.6  2003/11/01 14:36:23  mast
#  Put libiimod befor libimod because of new error calls
#
#  Revision 3.5  2003/10/24 02:30:40  mast
#  Changes for windows/Intel compiler
#
#  Revision 3.4  2003/02/28 18:09:32  mast
#  Changes for cygwin/windows
#
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

O = $(OBJEXT)
E = $(BINEXT)

IILIBS    = $(LIMOD) $(LIIMOD) $(TIFFLIBS)

PROGS = imodauto$(E) imodinfo$(E) imodmesh$(E) imodtrans$(E) imodmop$(E) \
 imodcmopp$(E) rec2imod$(E) imod2rib$(E) imod2vrml$(E) imod2meta$(E) \
 wmod2imod$(E) imodjoin$(E) imodextract$(E) imodsortsurf$(E) imodexplode$(E) \
 patch2imod$(E) imod2patch$(E) echo2$(E) imodfillin$(E) $(IMODWINCPU)

all : $(PROGS)

imodinfo$(E) : imodinfo.$(O)
	$(CC) -o imodinfo imodinfo.$(O) $(LDFLAGS) $(LIMOD) \
	$(CLIBS)

imodmesh$(E) : imodmesh.$(O) mkmesh.$(O)
	$(CC) -o imodmesh mkmesh.$(O) imodmesh.$(O) \
	$(LDFLAGS) $(LIMOD) $(CLIBS)

imodtrans$(E) : imodtrans.$(O)
	$(CC) -o imodtrans imodtrans.$(O) $(LDFLAGS) $(LIMOD) $(CLIBS)

imodmop$(E) : imodmop.$(O) moplib.$(O)
	$(CC) -o imodmop imodmop.$(O) moplib.$(O) $(LDFLAGS) $(IILIBS) $(CLIBS)

imodcmopp$(E) : imodcmopp.$(O) moplib.$(O)
	$(CC) -o imodcmopp imodcmopp.$(O) moplib.$(O) \
	$(LDFLAGS) $(IILIBS) $(CLIBS)

imodauto$(E) : imodauto.$(O) 
	$(CC) -o imodauto imodauto.$(O) $(LDFLAGS) $(IILIBS) $(CLIBS)

rec2imod$(E) : rec2imod.$(O) recfile.$(O)
	$(CC) -o rec2imod rec2imod.$(O) recfile.$(O) $(LDFLAGS) $(LIMOD) $(CLIBS)

imod2rib$(E): imod2rib.$(O)
	$(CC) -o imod2rib imod2rib.$(O) $(LDFLAGS) $(LIMOD) $(CLIBS)

imod2vrml$(E) : imod2vrml.$(O)
	$(CC) -o imod2vrml imod2vrml.$(O) $(LDFLAGS) $(LIMOD) $(CLIBS)

imod2meta$(E) : imod2meta.$(O)
	$(CC) -o imod2meta imod2meta.$(O) $(LDFLAGS) $(LIMOD) $(CLIBS)

wmod2imod$(E) : wmod2imod.$(O)
	$(CC) -o wmod2imod wmod2imod.$(O) $(LDFLAGS) $(LIMOD) $(CLIBS)

imodjoin$(E) :  imodjoin.$(O)
	$(CC) -o imodjoin imodjoin.$(O) $(LDFLAGS) $(IILIBS) $(CLIBS)

imodextract$(E) : imodextract.$(O)
	$(CC) -o imodextract imodextract.$(O) $(LDFLAGS) $(LIMOD) $(CLIBS)

imodsortsurf$(E) : imodsortsurf.$(O)
	$(CC) -o imodsortsurf imodsortsurf.$(O) $(LDFLAGS) $(LIMOD) $(CLIBS)

imodexplode$(E) : imodexplode.$(O)
	$(CC) -o imodexplode imodexplode.$(O) $(LDFLAGS) $(LIMOD) $(CLIBS)

patch2imod$(E) : patch2imod.$(O) 
	$(CC) -o patch2imod patch2imod.$(O) $(LDFLAGS) $(LIMOD) $(CLIBS)

imod2patch$(E) : imod2patch.$(O) 
	$(CC) -o imod2patch imod2patch.$(O) $(LDFLAGS) $(LIMOD) $(CLIBS)

echo2$(E) : echo2.$(O) 
	$(CC) -o echo2 echo2.$(O) $(LDFLAGS)

imodfillin$(E) : imodfillin.$(O)
	$(CC) -o imodfillin imodfillin.$(O) $(LDFLAGS) $(LIMOD) $(CLIBS)

imodwincpu.exe : imodwincpu.obj
	$(CC) -o imodwincpu imodwincpu.obj $(LDFLAGS)

install : $(PROGS)
	strip $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean : 
	$(FIND) . -type f -name "*.o*" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "core*" -exec rm "{}" \;
	$(FIND) . -type f -name "*.exe" -exec rm "{}" \;
	$(FIND) . -type f -perm 755 -exec rm "{}" \;


##############################################################################
# unsupported programs

imod2nff : imod2nff.$(O)
	$(CC) -o imod2nff imod2nff.$(O) $(LDFLAGS) $(LIMOD) $(CLIBS)

imod2synu : imod2synu.$(O)
	$(CC) -o imod2synu imod2synu.$(O) $(LDFLAGS) $(LIMOD) $(CLIBS)

imod2wmod : imod2wmod.$(O) $(IMODELOBJS)
	$(CC) -o imod2wmod imod2wmod.$(O) $(LDFLAGS) $(LIMOD) $(CLIBS)

# oldpore, help edit old yeast pore models
oldpore : oldpore.$(O)
	$(CC) -o oldpore oldpore.$(O) $(LDFLAGS) $(LIMOD) $(CLIBS)

imodclean : imodclean.$(O)
	$(CC) -o imodclean imodclean.$(O) $(LDFLAGS) $(LIMOD) $(CLIBS)

imodold : imodold.$(O) 
	$(CC) -o imodold imodold.$(O) $(LDFLAGS) $(LIMOD) $(CLIBS)

imodmkhv : imodmkhv.$(O) 
	$(CC) -o imodmkhv imodmkhv.$(O) $(LDFLAGS) $(LIMOD) $(CLIBS)





