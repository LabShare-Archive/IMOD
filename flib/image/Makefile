#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$
#  Revision 3.8  2005/04/16 15:35:13  mast
#  Added numericdiff
#
#  Revision 3.7  2004/10/28 22:22:40  mast
#  Added binvol
#
#  Revision 3.6  2004/10/28 21:14:36  mast
#  Added maxjoinsize
#
#  Revision 3.5  2004/10/24 20:44:10  mast
#  Added dependecies on ftbuf.inc
#
#  Revision 3.4  2004/03/30 18:53:13  mast
#  Added mtffilter
#
#  Revision 3.3  2004/03/22 05:40:15  mast
#  Added extractmagrad
#
#  Revision 3.2  2003/10/24 02:30:57  mast
#  Changes for windows/Intel compiler
#
#  Revision 3.1  2003/10/10 20:46:01  mast
#  Changed rotatevol, matchvol, warpvol to use common include and subroutines
#

include ../configure

O        = $(OBJEXT)
E        = $(BINEXT)
LDFLG    = $(LDFFLAGS) $(LDFLAGS)

PROGS = addtostack$(E) alterheader$(E) assemblevol$(E) avganova$(E) \
 avgstack$(E) boxavg$(E) calc$(E) checkxforms$(E) combinefft$(E) \
 densmatch$(E) edmont$(E) edpiecepoint$(E) enhance$(E) eraser$(E) excise$(E) \
 extposition$(E) extracttilts$(E) extractpieces$(E) extstack$(E) fftrans$(E) \
 filltomo$(E) findcontrast$(E) findsection$(E) fixmont$(E) goodframe$(E) \
 header$(E) matchvol$(E) montagesize$(E) mtdetect$(E) mtteach$(E) \
 newstack$(E) rotatevol$(E) rotmont$(E) subimage$(E) subimanova$(E) \
 subimstat$(E) taperoutvol$(E) tapervoledge$(E) tiltxcorr$(E) tomopieces$(E) \
 vmstocsh$(E) warpvol$(E) xcorr$(E) xfinverse$(E) xfproduct$(E) xfsimplex$(E) \
 xftoxg$(E) xyzproj$(E) extractmagrad$(E) mtffilter$(E) maxjoinsize$(E) \
 binvol$(E) numericdiff$(E)

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

boxavg$(E): boxavg.$(O)
	$(FC) -o boxavg boxavg.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

calc$(E): calc.$(O)
	$(FC) -o calc calc.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

calc.$(O): calc.f
	$(FC) $(NOOPFFLAGS) -c calc.f

checkxforms$(E): checkxforms.$(O)
	$(FC) -o checkxforms checkxforms.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

combinefft$(E): combinefft.$(O)
	$(FC) -o combinefft combinefft.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

densmatch$(E): densmatch.$(O)
	$(FC) -o densmatch densmatch.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

edmont$(E): edmont.$(O)
	$(FC) -o edmont edmont.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

edpiecepoint$(E): edpiecepoint.$(O)
	$(FC) -o edpiecepoint edpiecepoint.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

enhance$(E): enhance.$(O)
	$(FC) -o enhance enhance.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

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

fftrans$(E): fftrans.$(O) giantfft.$(O)
	$(FC) -o fftrans fftrans.$(O) giantfft.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

filltomo$(E): filltomo.$(O)
	$(FC) -o filltomo filltomo.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

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

matchvol$(E): matchvol.$(O) rotmatwarpsubs.$(O)
	$(FC) -o matchvol matchvol.$(O) rotmatwarpsubs.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

maxjoinsize$(E): maxjoinsize.$(O)
	$(FC) -o maxjoinsize maxjoinsize.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

montagesize$(E): montagesize.$(O)
	$(FC) -o montagesize montagesize.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

mtdetect$(E): mtdetect.$(O) mtsubs.$(O)
	$(FC) -o mtdetect mtdetect.$(O) mtsubs.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

mtffilter$(E): mtffilter.$(O)
	$(FC) -o mtffilter mtffilter.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

mtteach$(E): mtteach.$(O) mtsubs.$(O)
	$(FC) -o mtteach mtteach.$(O) mtsubs.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

newstack$(E): newstack.$(O)
	$(FC) -o newstack newstack.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

numericdiff$(E): numericdiff.$(O)
	$(FC) -o numericdiff numericdiff.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

rotatevol$(E): rotatevol.$(O) rotmatwarpsubs.$(O)
	$(FC) -o rotatevol rotatevol.$(O) rotmatwarpsubs.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

rotmont$(E): rotmont.$(O)
	$(FC) -o rotmont rotmont.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

subimage$(E): subimage.$(O)
	$(FC) -o subimage subimage.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

subimanova$(E): subimanova.$(O)
	$(FC) -o subimanova subimanova.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

subimstat$(E): subimstat.$(O)
	$(FC) -o subimstat subimstat.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

taperoutvol$(E): taperoutvol.$(O)
	$(FC) -o taperoutvol taperoutvol.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

tapervoledge$(E): tapervoledge.$(O)
	$(FC) -o tapervoledge tapervoledge.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

tiltxcorr$(E): tiltxcorr.$(O)
	$(FC) -o tiltxcorr tiltxcorr.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

tomopieces$(E): tomopieces.$(O)
	$(FC) -o tomopieces tomopieces.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

vmstocsh$(E): vmstocsh.$(O)
	$(FC) -o vmstocsh vmstocsh.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

warpvol$(E): warpvol.$(O) rotmatwarpsubs.$(O)
	$(FC) -o warpvol rotmatwarpsubs.$(O) warpvol.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

xcorr$(E): xcorr.$(O)
	$(FC) -o xcorr xcorr.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

xfinverse$(E): xfinverse.$(O)
	$(FC) -o xfinverse xfinverse.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

xfproduct$(E): xfproduct.$(O)
	$(FC) -o xfproduct xfproduct.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

xfsimplex$(E): xfsimplex.$(O)
	$(FC) -o xfsimplex xfsimplex.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

xftoxg$(E): xftoxg.$(O)
	$(FC) -o xftoxg xftoxg.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

xyzproj$(E): xyzproj.$(O)
	$(FC) -o xyzproj xyzproj.$(O) $(LDFLG) $(IMLIBS) $(LIBS)

install: $(PROGS)
	$(STRIP) $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.o* *~ *.exe $(PROGS)

rotatevol.$(O): rotmatwarp.inc
matchvol.$(O): rotmatwarp.inc
warpvol.$(O): rotmatwarp.inc
rotmatwarpsubs.$(O): rotmatwarp.inc
giantfft.$(O): $(INCDIR)/ftbuf.inc
enhance.$(O): $(INCDIR)/ftbuf.inc
fftrans.$(O): $(INCDIR)/ftbuf.inc
