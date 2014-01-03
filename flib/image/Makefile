#  $Id$
#
include ../configure

O        = $(OBJEXT)
E        = $(BINEXT)
LDFLG    = $(LDFFLAGS)

PROGS = addtostack$(E) alterheader$(E) assemblevol$(E) avganova$(E) \
 avgstack$(E) calc$(E) checkxforms$(E) combinefft$(E) edgemtf$(E) \
 densmatch$(E) edpiecepoint$(E) enhance$(E) eraser$(E) excise$(E) \
 extposition$(E) extracttilts$(E) extractpieces$(E) extstack$(E) fftrans$(E) \
 findcontrast$(E) findsection$(E) fixmont$(E) goodframe$(E) \
 header$(E) matchvol$(E) montagesize$(E) mtdetect$(E) mtteach$(E) \
 newstack$(E) rotatevol$(E) rotmont$(E) subimage$(E) subimanova$(E) \
 subimstat$(E) taperoutvol$(E) tapervoledge$(E) tomopieces$(E) \
 vmstocsh$(E) warpvol$(E) xcorrstack$(E) xfinverse$(E) xfproduct$(E) \
 xftoxg$(E) xyzproj$(E) extractmagrad$(E) mtffilter$(E) maxjoinsize$(E) \
 binvol$(E) numericdiff$(E) stitchalign$(E) densnorm$(E) xfsimplex$(E) \
 fixboundaries$(E)

LIBS = $(FORTCLIBS)

all: $(PROGS)

addtostack$(E): addtostack.$(O)
	$(FC) -o addtostack addtostack.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

alterheader$(E): alterheader.$(O)
	$(FC) -o alterheader alterheader.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

assemblevol$(E): assemblevol.$(O)
	$(FC) -o assemblevol assemblevol.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

avganova$(E): avganova.$(O)
	$(FC) -o avganova avganova.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

avgstack$(E): avgstack.$(O)
	$(FC) -o avgstack avgstack.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

binvol$(E): binvol.$(O)
	$(FC) -o binvol binvol.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

binvol.$(O): binvol.f90
	$(FC) $(FFLAGS) $(OPENMPF) -c binvol.f90

calc$(E): calc.$(O)
	$(FC) -o calc calc.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

calc.$(O): calc.f
	$(FC) $(NOOPFFLAGS) -c calc.f

checkxforms$(E): checkxforms.$(O)
	$(FC) -o checkxforms checkxforms.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

combinefft$(E): combinefft.$(O) taperprep.$(O)
	$(FC) -o combinefft combinefft.$(O) taperprep.$(O) $(LDFLG) \
	$(IMLIBS) $(LIFFT) $(LIBS)

densmatch$(E): densmatch.$(O)
	$(FC) -o densmatch densmatch.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

densnorm$(E): densnorm.$(O)
	$(FC) -o densnorm densnorm.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

edgemtf$(E): edgemtf.$(O)
	$(FC) -o edgemtf edgemtf.$(O) $(LDFLG) $(IMLIBS) $(LIFFT) $(LIBS)

edpiecepoint$(E): edpiecepoint.$(O)
	$(FC) -o edpiecepoint edpiecepoint.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

enhance$(E): enhance.$(O)
	$(FC) -o enhance enhance.$(O) $(LDFLG) $(IMLIBS) $(LIFFT) $(LIBS)

eraser$(E): eraser.$(O)
	$(FC) -o eraser eraser.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

excise$(E): excise.$(O)
	$(FC) -o excise  excise.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

extposition$(E): extposition.$(O)
	$(FC) -o extposition extposition.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

extractpieces$(E): extractpieces.$(O)
	$(FC) -o extractpieces extractpieces.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

extractmagrad$(E): extractmagrad.$(O)
	$(FC) -o extractmagrad extractmagrad.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

extracttilts$(E): extracttilts.$(O)
	$(FC) -o extracttilts extracttilts.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

extstack$(E): extstack.$(O)
	$(FC) -o extstack extstack.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

fixboundaries$(E): fixboundaries.$(O)
	$(FC) -o fixboundaries fixboundaries.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

fftrans$(E): fftrans.$(O)
	$(FC) -o fftrans fftrans.$(O) $(LDFLG) $(IMLIBS) $(LIFFT) $(LIBS)

findcontrast$(E): findcontrast.$(O)
	$(FC) -o findcontrast findcontrast.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

findsection$(E): findsection.$(O)
	$(FC) -o findsection findsection.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

fixmont$(E): fixmont.$(O)
	$(FC) -o fixmont fixmont.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

goodframe$(E): goodframe.$(O)
	$(FC) -o goodframe goodframe.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

header$(E): header.$(O)
	$(FC) -o header header.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

matchvol$(E): matchvol.$(O) rotmatwarpsubs.$(O) rotmatwarp.$(O)
	$(FC) -o matchvol matchvol.$(O) rotmatwarpsubs.$(O) rotmatwarp.$(O) \
	$(LDFLG) $(IMLIBS) $(LIBS)

maxjoinsize$(E): maxjoinsize.$(O)
	$(FC) -o maxjoinsize maxjoinsize.$(O) $(LDFLG) $(IMLIBS) $(LIWARP) $(LIBS)

montagesize$(E): montagesize.$(O)
	$(FC) -o montagesize montagesize.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

mtdetect$(E): mtdetect.$(O) mtsubs.$(O)
	$(FC) -o mtdetect mtdetect.$(O) mtsubs.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

mtffilter$(E): mtffilter.$(O)
	$(FC) -o mtffilter mtffilter.$(O) $(LDFLG) $(IMLIBS) $(LIFFT) $(LIBS)

mtteach$(E): mtteach.$(O) mtsubs.$(O)
	$(FC) -o mtteach mtteach.$(O) mtsubs.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

newstack$(E): newstack.$(O)
	$(FC) -o newstack newstack.$(O) $(LDFLG) $(IMLIBS) $(LIWARP) $(LIFFT) $(LIBS) 

numericdiff$(E): numericdiff.$(O)
	$(FC) -o numericdiff numericdiff.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

rotatevol$(E): rotatevol.$(O) rotmatwarpsubs.$(O) rotmatwarp.$(O)
	$(FC) -o rotatevol rotatevol.$(O) rotmatwarpsubs.$(O) rotmatwarp.$(O) \
	$(LDFLG) $(IMLIBS) $(LIBS)

rotmatwarpsubs.$(O): rotmatwarpsubs.f rotmatwarp.$(O)
	$(FC) $(FFLAGS) $(OPENMPF) -c rotmatwarpsubs.f

rotmont$(E): rotmont.$(O)
	$(FC) -o rotmont rotmont.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

subimage$(E): subimage.$(O)
	$(FC) -o subimage subimage.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

stitchalign$(E): stitchalign.$(O) stitchvars.$(O)
	$(FC) -o stitchalign stitchalign.$(O) stitchvars.$(O) $(LDFLG) \
	$(IMLIBS) $(LIBS)

subimanova$(E): subimanova.$(O)
	$(FC) -o subimanova subimanova.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

subimstat$(E): subimstat.$(O)
	$(FC) -o subimstat subimstat.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

taperoutvol$(E): taperoutvol.$(O) taperprep.$(O)
	$(FC) -o taperoutvol taperoutvol.$(O) taperprep.$(O) $(LDFLG) \
	$(IMLIBS) $(LIBS)

tapervoledge$(E): tapervoledge.$(O)
	$(FC) -o tapervoledge tapervoledge.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

tomopieces$(E): tomopieces.$(O)
	$(FC) -o tomopieces tomopieces.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

vmstocsh$(E): vmstocsh.$(O)
	$(FC) -o vmstocsh vmstocsh.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

warpvol.$(O): warpvol.f
	$(FC) $(FFLAGS) $(OPENMPF) -c warpvol.f

warpvol$(E): warpvol.$(O) rotmatwarpsubs.$(O) rotmatwarp.$(O)
	$(FC) -o warpvol rotmatwarpsubs.$(O) warpvol.$(O) rotmatwarp.$(O) \
	$(LDFLG) $(IMLIBS) $(LIBS)

xcorrstack$(E): xcorrstack.$(O)
	$(FC) -o xcorrstack xcorrstack.$(O) $(LDFLG) $(IMLIBS) $(LIFFT) $(LIBS)

xfinverse$(E): xfinverse.$(O)
	$(FC) -o xfinverse xfinverse.$(O) $(LDFLG) $(IMLIBS) $(LIWARP) $(LIBS)

xfproduct$(E): xfproduct.$(O)
	$(FC) -o xfproduct xfproduct.$(O) $(LDFLG) $(IMLIBS) $(LIWARP) $(LIBS)

xfsimplex$(E): xfsimplex.$(O)
	$(FC) -o xfsimplex xfsimplex.$(O) $(LDFLG) $(IMLIBS) $(LIWARP) $(LIFFT) $(LIBS)

xftoxg$(E): xftoxg.$(O)
	$(FC) -o xftoxg xftoxg.$(O) $(LDFLG) $(IMLIBS) $(LIWARP) $(LIBS)

xyzproj$(E): xyzproj.$(O)
	$(FC) -o xyzproj xyzproj.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

install: $(PROGS)
	$(STRIP) $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.$(OBJEXT) *~ *.mod *.exe $(PROGS)

rotatevol.$(O): rotmatwarp.$(O)
matchvol.$(O): rotmatwarp.$(O)
warpvol.$(O): rotmatwarp.$(O)
stitchalign.$(O): stitchvars.$(O)

