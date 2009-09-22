#
# Makefile for IMOD model utility programs
#
# $Id$
# Log at end
#
include ../configure

O = $(OBJEXT)
E = $(BINEXT)

IILIBS    = $(LIIMOD) $(LIMOD) $(LCFSHR) $(TIFFLIBS)
MODLIBS   = $(LIMOD) $(LCFSHR)

PROGS = imodauto$(E) imodinfo$(E) imodmesh$(E) imodtrans$(E) imodmop$(E) \
 rec2imod$(E) imod2rib$(E) imod2vrml$(E) imod2meta$(E) \
 wmod2imod$(E) imodjoin$(E) imodextract$(E) imodsortsurf$(E) imodexplode$(E) \
 patch2imod$(E) imod2patch$(E) echo2$(E) imodfillin$(E) imodcurvature$(E) \
 imodfindbeads$(E) point2model$(E) flattenwarp$(E) $(IMODWINCPU)

all : $(PROGS)

imodinfo$(E) : imodinfo.$(O)
	$(CC) -o imodinfo imodinfo.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

imodmesh$(E) : imodmesh.$(O) 
	$(CC) -o imodmesh imodmesh.$(O) \
	$(LDFLAGS) $(LIMESH) $(MODLIBS) $(CLIBS)

imodtrans$(E) : imodtrans.$(O)
	$(CC) -o imodtrans imodtrans.$(O) $(LDFLAGS) $(IILIBS) $(CLIBS)

imodmop$(E) : imodmop.$(O) 
	$(CC) -o imodmop imodmop.$(O) $(LDFLAGS) $(IILIBS) $(CLIBS)

imodauto$(E) : imodauto.$(O) 
	$(CC) -o imodauto imodauto.$(O) $(LDFLAGS) $(IILIBS) $(CLIBS)

rec2imod$(E) : rec2imod.$(O) recfile.$(O)
	$(CC) -o rec2imod rec2imod.$(O) recfile.$(O) $(LDFLAGS) $(MODLIBS) \
	$(CLIBS)

imod2rib$(E): imod2rib.$(O)
	$(CC) -o imod2rib imod2rib.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

imod2vrml$(E) : imod2vrml.$(O)
	$(CC) -o imod2vrml imod2vrml.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

imod2meta$(E) : imod2meta.$(O)
	$(CC) -o imod2meta imod2meta.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

wmod2imod$(E) : wmod2imod.$(O)
	$(CC) -o wmod2imod wmod2imod.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

imodjoin$(E) :  imodjoin.$(O)
	$(CC) -o imodjoin imodjoin.$(O) $(LDFLAGS) $(IILIBS) $(CLIBS)

imodextract$(E) : imodextract.$(O)
	$(CC) -o imodextract imodextract.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

imodsortsurf$(E) : imodsortsurf.$(O)
	$(CC) -o imodsortsurf imodsortsurf.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

imodexplode$(E) : imodexplode.$(O)
	$(CC) -o imodexplode imodexplode.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

patch2imod$(E) : patch2imod.$(O) 
	$(CC) -o patch2imod patch2imod.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

imod2patch$(E) : imod2patch.$(O) 
	$(CC) -o imod2patch imod2patch.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

echo2$(E) : echo2.$(O) 
	$(CC) -o echo2 echo2.$(O) $(LDFLAGS)

imodfillin$(E) : imodfillin.$(O)
	$(CC) -o imodfillin imodfillin.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

imodcurvature$(E) : imodcurvature.$(O)
	$(CC) -o imodcurvature imodcurvature.$(O) $(LDFLAGS) $(MODLIBS) \
	$(CLIBS)

imodfindbeads$(E) : imodfindbeads.$(O)
	$(CC) -o imodfindbeads imodfindbeads.$(O) $(LDFLAGS) $(IILIBS) \
	$(LIFFT) $(CLIBS)

point2model$(E) : point2model.$(O)
	$(CC) -o point2model point2model.$(O) $(LDFLAGS) $(IILIBS) \
	$(CLIBS)

flattenwarp$(E) : flattenwarp.$(O)
	$(CC) -o flattenwarp flattenwarp.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

imodwincpu.exe : imodwincpu.obj
	$(CC) -o imodwincpu imodwincpu.obj $(LDFLAGS)

imodwincpu.obj : imodwincpu.cpp CpuUsage.h PerfCounters.h
	$(CXX) -c $(CXXFLAGS) $(WINCPUVC) imodwincpu.cpp

install : $(PROGS)
	$(STRIP) $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean : 
	$(FIND) . -type f -name "*.o*" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "core*" -exec rm "{}" \;
	$(FIND) . -type f -name "*.exe" -exec rm "{}" \;


##############################################################################
# unsupported programs

imod2nff : imod2nff.$(O)
	$(CC) -o imod2nff imod2nff.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

imod2synu : imod2synu.$(O)
	$(CC) -o imod2synu imod2synu.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

imod2wmod : imod2wmod.$(O) $(IMODELOBJS)
	$(CC) -o imod2wmod imod2wmod.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

# oldpore, help edit old yeast pore models
oldpore : oldpore.$(O)
	$(CC) -o oldpore oldpore.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

imodclean : imodclean.$(O)
	$(CC) -o imodclean imodclean.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

imodold : imodold.$(O) 
	$(CC) -o imodold imodold.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

imodmkhv : imodmkhv.$(O) 
	$(CC) -o imodmkhv imodmkhv.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)


#  $Log$
#  Revision 3.22  2009/01/21 04:47:46  mast
#  Step deleting mode 755 files
#
#  Revision 3.21  2009/01/07 03:13:29  mast
#  Added flattenwarp
#
#  Revision 3.20  2008/06/19 23:27:11  mast
#  Added imodfindbeads
#
#  Revision 3.19  2007/10/18 22:20:20  mast
#  Added point2model
#
#  Revision 3.18  2007/09/19 23:48:39  mast
#  Changes for library rearrangements
#
#  Revision 3.17  2007/04/26 19:13:48  mast
#  Removed imodcmopp
#
#  Revision 3.16  2006/09/19 22:24:47  mast
#  Added a flag for imodwincpu
#
#  Revision 3.15  2006/09/12 15:01:36  mast
#  imodmesh uses library
#
#  Revision 3.14  2006/06/14 04:28:17  mast
#  Added imodcurvature
#
#  Revision 3.13  2005/11/23 16:24:37  mast
#  Use strip variable
#
#  Revision 3.12  2005/10/19 14:42:52  mast
#  Switched imodtrans to using libiimod
#
#  Revision 3.11  2005/10/13 20:18:24  mast
#  Switched imodjoin to using libiimod
#
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



