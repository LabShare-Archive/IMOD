include ../configure
#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#  Revision 3.1  2002/05/23 04:41:37  mast
#  Added repackseed
#

PROGS = convertmod model2point imavgstat solvematch xfmodel\
 boxstartend endmodel realscalemod scalemodel checkmtmod joinmodel mtsmooth\
 reducemtmod xfinterstack clipmodel mtmodel remapmodel mtlengths contourmod\
 smoothsurf reducecont fenestra ccderaser edgeeraser refinematch findwarp\
 tomopitch fiberpitch corrsearch3d repackseed resamplemod

LIBS = $(FORTCLIBS)

all: $(PROGS)

convertmod: convertmod.o
	$(FC) -o convertmod convertmod.o $(LDFLAGS) $(IMLIBS) $(LIBS)

model2point: model2point.o
	$(FC) -o model2point model2point.o $(LDFLAGS) $(IMLIBS) $(LIBS)

imavgstat: imavgstat.o
	$(FC) -o imavgstat imavgstat.o $(LDFLAGS) $(IMLIBS) $(LIBS)

solvematch: solvematch.o solve_wo_outliers.o
	$(FC) -o solvematch solvematch.o solve_wo_outliers.o $(LDFLAGS) $(IMLIBS) $(LIBS)

xfmodel: xfmodel.o
	$(FC) -o xfmodel xfmodel.o $(LDFLAGS) $(IMLIBS) $(LIBS)

boxstartend: boxstartend.o
	$(FC) -o boxstartend boxstartend.o $(LDFLAGS) $(IMLIBS) $(LIBS)

endmodel: endmodel.o
	$(FC) -o endmodel endmodel.o $(LDFLAGS) $(IMLIBS) $(LIBS)

realscalemod: realscalemod.o
	$(FC) -o realscalemod realscalemod.o $(LDFLAGS) $(IMLIBS) $(LIBS)

scalemodel: scalemodel.o
	$(FC) -o scalemodel scalemodel.o $(LDFLAGS) $(IMLIBS) $(LIBS)

checkmtmod: checkmtmod.o
	$(FC) -o checkmtmod checkmtmod.o $(LDFLAGS) $(IMLIBS) $(LIBS)

joinmodel: joinmodel.o
	$(FC) -o joinmodel joinmodel.o $(LDFLAGS) $(IMLIBS) $(LIBS)

mtsmooth: mtsmooth.o
	$(FC) -o mtsmooth mtsmooth.o $(LDFLAGS) $(IMLIBS) $(LIBS)

reducemtmod: reducemtmod.o
	$(FC) -o reducemtmod reducemtmod.o $(LDFLAGS) $(IMLIBS) $(LIBS)

xfinterstack: xfinterstack.o
	$(FC) -o xfinterstack xfinterstack.o $(LDFLAGS) $(IMLIBS) $(LIBS)

clipmodel: clipmodel.o
	$(FC) -o clipmodel clipmodel.o $(LDFLAGS) $(IMLIBS) $(LIBS)

mtmodel: mtmodel.o
	$(FC) -o mtmodel mtmodel.o $(LDFLAGS) $(IMLIBS) $(LIBS)

remapmodel: remapmodel.o
	$(FC) -o remapmodel remapmodel.o $(LDFLAGS) $(IMLIBS) $(LIBS)

mtlengths: mtlengths.o
	$(FC) -o mtlengths mtlengths.o $(LDFLAGS) $(IMLIBS) $(LIBS)

contourmod: contourmod.o
	$(FC) -o contourmod contourmod.o $(LDFLAGS) $(IMLIBS) $(LIBS)

smoothsurf: smoothsurf.o
	$(FC) -o smoothsurf smoothsurf.o $(LDFLAGS) $(IMLIBS) $(LIBS)

reducecont: reducecont.o
	$(FC) -o reducecont reducecont.o $(LDFLAGS) $(IMLIBS) $(LIBS)

fenestra: fenestra.o planefit.o
	$(FC) -o fenestra fenestra.o planefit.o $(LDFLAGS) $(IMLIBS) $(LIBS)

ccderaser: ccderaser.o
	$(FC) -o ccderaser ccderaser.o $(LDFLAGS) $(IMLIBS) $(LIBS)

edgeeraser: edgeeraser.o
	$(FC) -o edgeeraser edgeeraser.o $(LDFLAGS) $(IMLIBS) $(LIBS)

refinematch: refinematch.o solve_wo_outliers.o
	$(FC) -o refinematch refinematch.o $(LDFLAGS) solve_wo_outliers.o $(IMLIBS) $(LIBS)

findwarp: findwarp.o solve_wo_outliers.o
	$(FC) -o findwarp findwarp.o $(LDFLAGS) solve_wo_outliers.o $(IMLIBS) $(LIBS)

tomopitch: tomopitch.o
	$(FC) -o tomopitch tomopitch.o $(LDFLAGS) $(IMLIBS) $(LIBS)

fiberpitch: fiberpitch.o
	$(FC) -o fiberpitch fiberpitch.o $(LDFLAGS) $(IMLIBS) $(LIBS)

corrsearch3d: corrsearch3d.o
	$(FC) -o corrsearch3d corrsearch3d.o $(LDFLAGS) $(IMLIBS) $(LIBS)

repackseed: repackseed.o
	$(FC) -o repackseed repackseed.o $(LDFLAGS) $(IMLIBS) $(LIBS)

resamplemod: resamplemod.o
	$(FC) -o resamplemod resamplemod.o $(LDFLAGS) $(IMLIBS) $(LIBS)

install: $(PROGS)
	strip $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.o *~ $(PROGS)

