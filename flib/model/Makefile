#  Makefile for flib/model
#
#  $Id$
#  Log at end
#

include ../configure

E        = $(BINEXT)
O        = $(OBJEXT)
LDFLG    = $(LDFFLAGS)

PROGS = convertmod$(E) model2point$(E) imavgstat$(E) solvematch$(E) \
 xfmodel$(E) boxstartend$(E) endmodel$(E) realscalemod$(E) scalemodel$(E) \
 checkmtmod$(E) joinmodel$(E) mtsmooth$(E) reducemtmod$(E) xfinterstack$(E) \
 clipmodel$(E) mtmodel$(E) remapmodel$(E) mtlengths$(E) contourmod$(E)\
 smoothsurf$(E) reducecont$(E) fenestra$(E) ccderaser$(E) edgeeraser$(E) \
 refinematch$(E) findwarp$(E) sumdensity$(E) mtrotlong$(E) howflared$(E) \
 tomopitch$(E) fiberpitch$(E) corrsearch3d$(E) repackseed$(E) resamplemod$(E) \
 findhotpixels$(E) xfjointomo$(E) boxavg$(E) tiltxcorr$(E) findbeads3d$(E) \
 sortbeadsurfs$(E)
# mtteach$(E)  tomoblobs$(E)

WLIBS = $(LWMOD) $(LIMOD) $(IMLIBS) $(FORTCLIBS)
SMALLLIBS = $(LSMLMOD) $(LIMOD) $(IMLIBS) $(FORTCLIBS)


all: $(PROGS)

addtostack$(E): addtostack.$(O)
	$(FC) -o addtostack addtostack.$(O) $(LDFLG) $(WLIBS)

convertmod$(E): convertmod.$(O)
	$(FC) -o convertmod convertmod.$(O) $(LDFLG) $(WLIBS)

model2point$(E): model2point.$(O)
	$(FC) -o model2point model2point.$(O) $(LDFLG) $(WLIBS)

imavgstat$(E): imavgstat.$(O)
	$(FC) -o imavgstat imavgstat.$(O) $(LDFLG) $(WLIBS)

solvematch$(E): solvematch.$(O) solve_wo_outliers.$(O)
	$(FC) -o solvematch solvematch.$(O) solve_wo_outliers.$(O) $(LDFLG) $(WLIBS)

xfmodel$(E): xfmodel.$(O)
	$(FC) -o xfmodel xfmodel.$(O) $(LDFLG) $(WLIBS)

boxstartend$(E): boxstartend.$(O)
	$(FC) -o boxstartend boxstartend.$(O) $(LDFLG) $(SMALLLIBS)

endmodel$(E): endmodel.$(O)
	$(FC) -o endmodel endmodel.$(O) $(LDFLG) $(WLIBS)

realscalemod$(E): realscalemod.$(O)
	$(FC) -o realscalemod realscalemod.$(O) $(LDFLG) $(WLIBS)

scalemodel$(E): scalemodel.$(O)
	$(FC) -o scalemodel scalemodel.$(O) $(LDFLG) $(WLIBS)

checkmtmod$(E): checkmtmod.$(O)
	$(FC) -o checkmtmod checkmtmod.$(O) $(LDFLG) $(WLIBS)

joinmodel$(E): joinmodel.$(O)
	$(FC) -o joinmodel joinmodel.$(O) $(LDFLG) $(WLIBS)

mtsmooth$(E): mtsmooth.$(O)
	$(FC) -o mtsmooth mtsmooth.$(O) $(LDFLG) $(WLIBS)

reducemtmod$(E): reducemtmod.$(O)
	$(FC) -o reducemtmod reducemtmod.$(O) $(LDFLG) $(WLIBS)

xfinterstack$(E): xfinterstack.$(O)
	$(FC) -o xfinterstack xfinterstack.$(O) $(LDFLG) $(WLIBS)

clipmodel$(E): clipmodel.$(O)
	$(FC) -o clipmodel clipmodel.$(O) $(LDFLG) $(WLIBS)

mtmodel$(E): mtmodel.$(O)
	$(FC) -o mtmodel mtmodel.$(O) $(LDFLG) $(WLIBS)

remapmodel$(E): remapmodel.$(O)
	$(FC) -o remapmodel remapmodel.$(O) $(LDFLG) $(WLIBS)

mtlengths$(E): mtlengths.$(O)
	$(FC) -o mtlengths mtlengths.$(O) $(LDFLG) $(WLIBS)

mtrotlong$(E): mtrotlong.$(O)
	$(FC) -o mtrotlong mtrotlong.$(O) $(LDFLG) $(WLIBS)

mtteach$(E): mtteach.$(O) mtsubs.$(O)
	$(FC) -o mtteach mtteach.$(O) mtsubs.$(O) $(LDFLG) $(SMALLLIBS)

contourmod$(E): contourmod.$(O)
	$(FC) -o contourmod contourmod.$(O) $(LDFLG) $(WLIBS)

smoothsurf$(E): smoothsurf.$(O)
	$(FC) -o smoothsurf smoothsurf.$(O) $(LDFLG) $(WLIBS)

reducecont$(E): reducecont.$(O)
	$(FC) -o reducecont reducecont.$(O) $(LDFLG) $(WLIBS)

fenestra$(E): fenestra.$(O) planefit.$(O)
	$(FC) -o fenestra fenestra.$(O) planefit.$(O) $(LDFLG) $(WLIBS)

ccderaser$(E): ccderaser.$(O)
	$(FC) -o ccderaser ccderaser.$(O) $(LDFLG) $(WLIBS)

edgeeraser$(E): edgeeraser.$(O)
	$(FC) -o edgeeraser edgeeraser.$(O) $(LDFLG) $(WLIBS)

refinematch$(E): refinematch.$(O) solve_wo_outliers.$(O) get_region_contours.$(O)
	$(FC) -o refinematch refinematch.$(O) $(LDFLG) solve_wo_outliers.$(O) \
	get_region_contours.$(O) $(SMALLLIBS)

findwarp$(E): findwarp.$(O) solve_wo_outliers.$(O) get_region_contours.$(O)
	$(FC) -o findwarp findwarp.$(O) $(LDFLG) solve_wo_outliers.$(O) \
	get_region_contours.$(O) $(SMALLLIBS)

corrsearch3d$(E): corrsearch3d.$(O) get_region_contours.$(O)
	$(FC) -o corrsearch3d corrsearch3d.$(O) $(LDFLG) \
	get_region_contours.$(O) $(SMALLLIBS)

tomopitch$(E): tomopitch.$(O)
	$(FC) -o tomopitch tomopitch.$(O) $(LDFLG) $(SMALLLIBS)

fiberpitch$(E): fiberpitch.$(O)
	$(FC) -o fiberpitch fiberpitch.$(O) $(LDFLG) $(WLIBS)

repackseed$(E): repackseed.$(O)
	$(FC) -o repackseed repackseed.$(O) $(LDFLG) $(WLIBS)

resamplemod$(E): resamplemod.$(O)
	$(FC) -o resamplemod resamplemod.$(O) $(LDFLG) $(WLIBS)

sumdensity$(E): sumdensity.$(O)
	$(FC) -o sumdensity sumdensity.$(O) $(LDFLG) $(WLIBS)

howflared$(E): howflared.$(O) planefit.$(O)
	$(FC) -o howflared howflared.$(O) planefit.$(O) $(LDFLG) $(SMALLLIBS)

findhotpixels$(E): findhotpixels.$(O)
	$(FC) -o findhotpixels findhotpixels.$(O) $(LDFLG) $(SMALLLIBS)

xfjointomo$(E): xfjointomo.$(O)
	$(FC) -o xfjointomo xfjointomo.$(O) $(LDFLG) $(SMALLLIBS)

boxavg$(E): boxavg.$(O)
	$(FC) -o boxavg boxavg.$(O) $(LDFLG) $(SMALLLIBS)

findbeads3d$(E): findbeads3d.$(O)
	$(FC) -o findbeads3d findbeads3d.$(O) $(LDFLG) $(SMALLLIBS)

tiltxcorr$(E): tiltxcorr.$(O)
	$(FC) -o tiltxcorr tiltxcorr.$(O) $(LDFLG) $(SMALLLIBS)

tiltxcorr.$(O): tiltxcorr.f
	$(FC) $(FFLAGS) $(OPENMPF) -c tiltxcorr.f

tomoblobs$(E): tomoblobs.$(O)
	$(FC) -o tomoblobs tomoblobs.$(O) $(LDFLG) $(IMLIBS) $(FORTCLIBS)

sortbeadsurfs$(E): sortbeadsurfs.$(O)
	$(FC) -o sortbeadsurfs sortbeadsurfs.$(O) $(LDFLG) $(SMALLLIBS)

install: $(PROGS)
	$(STRIP) $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.o* *~ *.exe $(PROGS)

#  $Log$
#  Revision 3.20  2009/12/09 15:34:37  mast
#  remove find_surfaces from sortbeadsurfs
#
#  Revision 3.19  2009/12/09 01:33:43  mast
#  sortbeadsurfs
#
#  Revision 3.18  2009/10/02 20:28:09  mast
#  Added tiltxcorr; mtteach but still commented out
#
#  Revision 3.17  2008/12/11 06:08:09  mast
#  Added findbeads3d
#
#  Revision 3.16  2007/12/05 21:41:56  mast
#  *** empty log message ***
#
#  Revision 3.15  2007/10/21 17:13:40  mast
#  Add boxavg
#
#  Revision 3.14  2007/09/19 23:48:14  mast
#  Changes for library rearrangements
#
#  Revision 3.13  2006/10/26 19:08:08  mast
#  Added xfjointomo
#
#  Revision 3.12  2006/10/17 22:58:55  mast
#  Added findhotpixels
#
#  Revision 3.11  2006/10/14 21:20:40  mast
#  Added howflared
#
#  Revision 3.10  2006/09/11 22:18:12  mast
#  Added mtrotlong
#
#  Revision 3.9  2006/08/21 16:50:00  mast
#  Changed findwarp and refinematch links
#
#  Revision 3.8  2006/08/16 23:49:59  mast
#  changed corrsearch3d link
#
#  Revision 3.7  2006/06/29 05:10:14  mast
#  Changes for new model libraries
#
#  Revision 3.6  2006/06/22 01:09:45  mast
#  Replaced LDFLAGS with LDFFLAGS for Intel Mac
#
#  Revision 3.5  2005/05/12 05:07:40  mast
#  Use strip variable
#
#  Revision 3.4  2004/09/10 18:10:59  mast
#  Added sumdensity
#
#  Revision 3.3  2003/10/24 02:30:57  mast
#  Changes for windows/Intel compiler
#
#  Revision 3.2  2003/09/06 00:02:06  mast
#  Added resamplemod
#
#  Revision 3.1  2002/05/23 04:41:37  mast
#  Added repackseed
#
