#
# Makefile for IMOD model utility programs
#
# $Id$
#
include ../configure

O = $(OBJEXT)
E = $(BINEXT)

IILIBS    = $(LIIMOD) $(LIMOD) $(LCFSHR) $(TIFFLIBS)
MODLIBS   = $(LIMOD) $(LCFSHR)

PROGS = clonemodel$(E) clonevolume$(E) imodauto$(E) imod-dist$(E) \
 imodinfo$(E) imodmesh$(E) imodtrans$(E) imodmop$(E) rec2imod$(E) \
 imod2rib$(E) imod2vrml$(E) imod2vrml2$(E) imod2obj$(E) \
 imod2meta$(E) wmod2imod$(E) imodjoin$(E) imodextract$(E) imodsortsurf$(E) \
 imodexplode$(E) patch2imod$(E) imod2patch$(E) echo2$(E) imodfillin$(E) \
 imodcurvature$(E) imodfindbeads$(E) point2model$(E) flattenwarp$(E) \
 $(IMODWINCPU)

all : $(PROGS)

clonemodel$(E) : clonemodel.$(O)
	$(CC) -o clonemodel clonemodel.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

clonevolume$(E) : clonevolume.$(O)
	$(CC) -o clonevolume clonevolume.$(O) $(LDFLAGS) $(IILIBS) $(CLIBS)

imodinfo$(E) : imodinfo.$(O)
	$(CC) -o imodinfo imodinfo.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

imod-dist$(E) : imod-dist.$(O)
	$(CC) -o imod-dist imod-dist.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

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

imod2vrml2$(E) : imod2vrml2.$(O)
	$(CC) -o imod2vrml2 imod2vrml2.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

imod2obj$(E) : imod2obj.$(O)
	$(CXX) -o imod2obj imod2obj.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

imod2ccdbxml $(E) : imod2ccdbxml.$(O)
	$(CC) -o imod2ccdbxml imod2ccdbxml.$(O) $(LDFLAGS) $(MODLIBS) $(CLIBS)

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
	$(LAPACKLINK) -o flattenwarp flattenwarp.$(O) $(LDFLAGS) $(MODLIBS) \
	$(CLIBS) $(LAPACKLIBS) $(LAPACK_FLIBS) $(LIMESH)

imodwincpu.exe : imodwincpu.obj
	$(CC) -o imodwincpu imodwincpu.obj $(LDFLAGS)

imodwincpu.obj : imodwincpu.cpp CpuUsage.h PerfCounters.h
	$(CXX) -c $(CXXFLAGS) $(WINCPUVC) imodwincpu.cpp

install : $(PROGS)
	$(STRIP) $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean : 
	$(FIND) . -type f -name "*.$(OBJEXT)" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "core*" -exec rm "{}" \;
	$(FIND) . -type f -name "*.exe" -exec rm "{}" \;
	-\rm -f $(PROGS)


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
