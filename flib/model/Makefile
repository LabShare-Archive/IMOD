#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#  Revision 3.3  2003/10/24 02:30:57  mast
#  Changes for windows/Intel compiler
#
#  Revision 3.2  2003/09/06 00:02:06  mast
#  Added resamplemod
#
#  Revision 3.1  2002/05/23 04:41:37  mast
#  Added repackseed
#

include ../configure

E        = $(BINEXT)
O        = $(OBJEXT)
LDFLG    = $(LDFFLAGS) $(LDFLAGS)

PROGS = convertmod$(E) model2point$(E) imavgstat$(E) solvematch$(E) \
 xfmodel$(E) boxstartend$(E) endmodel$(E) realscalemod$(E) scalemodel$(E) \
 checkmtmod$(E) joinmodel$(E) mtsmooth$(E) reducemtmod$(E) xfinterstack$(E) \
 clipmodel$(E) mtmodel$(E) remapmodel$(E) mtlengths$(E) contourmod$(E)\
 smoothsurf$(E) reducecont$(E) fenestra$(E) ccderaser$(E) edgeeraser$(E) \
 refinematch$(E) findwarp$(E) sumdensity$(E) \
 tomopitch$(E) fiberpitch$(E) corrsearch3d$(E) repackseed$(E) resamplemod$(E)

LIBS = $(FORTCLIBS)

all: $(PROGS)

addtostack$(E): addtostack.$(O)
	$(FC) -o addtostack addtostack.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

convertmod$(E): convertmod.$(O)
	$(FC) -o convertmod convertmod.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

model2point$(E): model2point.$(O)
	$(FC) -o model2point model2point.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

imavgstat$(E): imavgstat.$(O)
	$(FC) -o imavgstat imavgstat.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

solvematch$(E): solvematch.$(O) solve_wo_outliers.$(O)
	$(FC) -o solvematch solvematch.$(O) solve_wo_outliers.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

xfmodel$(E): xfmodel.$(O)
	$(FC) -o xfmodel xfmodel.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

boxstartend$(E): boxstartend.$(O)
	$(FC) -o boxstartend boxstartend.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

endmodel$(E): endmodel.$(O)
	$(FC) -o endmodel endmodel.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

realscalemod$(E): realscalemod.$(O)
	$(FC) -o realscalemod realscalemod.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

scalemodel$(E): scalemodel.$(O)
	$(FC) -o scalemodel scalemodel.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

checkmtmod$(E): checkmtmod.$(O)
	$(FC) -o checkmtmod checkmtmod.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

joinmodel$(E): joinmodel.$(O)
	$(FC) -o joinmodel joinmodel.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

mtsmooth$(E): mtsmooth.$(O)
	$(FC) -o mtsmooth mtsmooth.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

reducemtmod$(E): reducemtmod.$(O)
	$(FC) -o reducemtmod reducemtmod.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

xfinterstack$(E): xfinterstack.$(O)
	$(FC) -o xfinterstack xfinterstack.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

clipmodel$(E): clipmodel.$(O)
	$(FC) -o clipmodel clipmodel.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

mtmodel$(E): mtmodel.$(O)
	$(FC) -o mtmodel mtmodel.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

remapmodel$(E): remapmodel.$(O)
	$(FC) -o remapmodel remapmodel.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

mtlengths$(E): mtlengths.$(O)
	$(FC) -o mtlengths mtlengths.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

contourmod$(E): contourmod.$(O)
	$(FC) -o contourmod contourmod.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

smoothsurf$(E): smoothsurf.$(O)
	$(FC) -o smoothsurf smoothsurf.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

reducecont$(E): reducecont.$(O)
	$(FC) -o reducecont reducecont.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

fenestra$(E): fenestra.$(O) planefit.$(O)
	$(FC) -o fenestra fenestra.$(O) planefit.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

ccderaser$(E): ccderaser.$(O)
	$(FC) -o ccderaser ccderaser.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

edgeeraser$(E): edgeeraser.$(O)
	$(FC) -o edgeeraser edgeeraser.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

refinematch$(E): refinematch.$(O) solve_wo_outliers.$(O)
	$(FC) -o refinematch refinematch.$(O) $(LDFLG) solve_wo_outliers.$(O) $(IMLIBS) $(LIBS)

findwarp$(E): findwarp.$(O) solve_wo_outliers.$(O)
	$(FC) -o findwarp findwarp.$(O) $(LDFLG) solve_wo_outliers.$(O) $(IMLIBS) $(LIBS)

tomopitch$(E): tomopitch.$(O)
	$(FC) -o tomopitch tomopitch.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

fiberpitch$(E): fiberpitch.$(O)
	$(FC) -o fiberpitch fiberpitch.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

corrsearch3d$(E): corrsearch3d.$(O)
	$(FC) -o corrsearch3d corrsearch3d.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

repackseed$(E): repackseed.$(O)
	$(FC) -o repackseed repackseed.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

resamplemod$(E): resamplemod.$(O)
	$(FC) -o resamplemod resamplemod.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

sumdensity$(E): sumdensity.$(O)
	$(FC) -o sumdensity sumdensity.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

install: $(PROGS)
	strip $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.o* *~ *.exe $(PROGS)

