include ../configure

PROGS = addtostack alterheader assemblevol avganova avgstack boxavg \
        calc checkxforms combinefft \
        densmatch edmont edpiecepoint enhance eraser excise extposition \
        extracttilts extractpieces extstack fftrans filltomo findcontrast \
        findsection fixmont goodframe header matchvol montagesize mtdetect\
        mtteach newstack rotatevol rotmont subimage subimanova subimstat \
        taperoutvol tapervoledge tiltxcorr tomopieces vmstocsh warpvol xcorr\
        xfinverse xfproduct xfsimplex xftoxg xyzproj 
 
LIBS = $(FORTCLIBS)

all: $(PROGS)

addtostack: addtostack.o
	$(FC) -o addtostack addtostack.o $(LDFLAGS) $(IMLIBS) $(LIBS)

alterheader: alterheader.o
	$(FC) -o alterheader alterheader.o $(LDFLAGS) $(IMLIBS) $(LIBS)

assemblevol: assemblevol.o
	$(FC) -o assemblevol assemblevol.o $(LDFLAGS) $(IMLIBS) $(LIBS)

avganova: avganova.o
	$(FC) -o avganova avganova.o $(LDFLAGS) $(IMLIBS) $(LIBS)

avgstack: avgstack.o
	$(FC) -o avgstack avgstack.o $(LDFLAGS) $(IMLIBS) $(LIBS)

boxavg: boxavg.o
	$(FC) -o boxavg boxavg.o $(LDFLAGS) $(IMLIBS) $(LIBS)

calc: calc.o
	$(FC) -o calc calc.o $(LDFLAGS) $(IMLIBS) $(LIBS)

calc.o: calc.f
	$(FC) $(NOOPFFLAGS) -c calc.f

checkxforms: checkxforms.o
	$(FC) -o checkxforms checkxforms.o $(LDFLAGS) $(IMLIBS) $(LIBS)

combinefft: combinefft.o
	$(FC) -o combinefft combinefft.o $(LDFLAGS) $(IMLIBS) $(LIBS)

densmatch: densmatch.o
	$(FC) -o densmatch densmatch.o $(LDFLAGS) $(IMLIBS) $(LIBS)

edmont: edmont.o
	$(FC) -o edmont edmont.o $(LDFLAGS) $(IMLIBS) $(LIBS)

edpiecepoint: edpiecepoint.o
	$(FC) -o edpiecepoint edpiecepoint.o $(LDFLAGS) $(IMLIBS) $(LIBS)

enhance: enhance.o
	$(FC) -o enhance enhance.o $(LDFLAGS) $(IMLIBS) $(LIBS)

eraser: eraser.o
	$(FC) -o eraser eraser.o $(LDFLAGS) $(IMLIBS) $(LIBS)

excise: excise.o
	$(FC) -o excise  excise.o $(LDFLAGS) $(IMLIBS) $(LIBS)

extposition: extposition.o
	$(FC) -o extposition extposition.o $(LDFLAGS) $(IMLIBS) $(LIBS)

extractpieces: extractpieces.o
	$(FC) -o extractpieces extractpieces.o $(LDFLAGS) $(IMLIBS) $(LIBS)

extracttilts: extracttilts.o
	$(FC) -o extracttilts extracttilts.o $(LDFLAGS) $(IMLIBS) $(LIBS)

extstack: extstack.o
	$(FC) -o extstack extstack.o $(LDFLAGS) $(IMLIBS) $(LIBS)

fftrans: fftrans.o giantfft.o
	$(FC) -o fftrans fftrans.o giantfft.o $(LDFLAGS) $(IMLIBS) $(LIBS)

filltomo: filltomo.o
	$(FC) -o filltomo filltomo.o $(LDFLAGS) $(IMLIBS) $(LIBS)

findcontrast: findcontrast.o
	$(FC) -o findcontrast findcontrast.o $(LDFLAGS) $(IMLIBS) $(LIBS)

findsection: findsection.o
	$(FC) -o findsection findsection.o $(LDFLAGS) $(IMLIBS) $(LIBS)

fixmont: fixmont.o
	$(FC) -o fixmont fixmont.o $(LDFLAGS) $(IMLIBS) $(LIBS)

goodframe: goodframe.o
	$(FC) -o goodframe goodframe.o $(LDFLAGS) $(IMLIBS) $(LIBS)

header: header.o
	$(FC) -o header header.o $(LDFLAGS) $(IMLIBS) $(LIBS)

matchvol: matchvol.o
	$(FC) -o matchvol matchvol.o $(LDFLAGS) $(IMLIBS) $(LIBS)

montagesize: montagesize.o
	$(FC) -o montagesize montagesize.o $(LDFLAGS) $(IMLIBS) $(LIBS)

mtdetect: mtdetect.o mtsubs.o
	$(FC) -o mtdetect mtdetect.o mtsubs.o $(LDFLAGS) $(IMLIBS) $(LIBS)

mtteach: mtteach.o mtsubs.o
	$(FC) -o mtteach mtteach.o mtsubs.o $(LDFLAGS) $(IMLIBS) $(LIBS)

newstack: newstack.o
	$(FC) -o newstack newstack.o $(LDFLAGS) $(IMLIBS) $(LIBS)

rotatevol: rotatevol.o
	$(FC) -o rotatevol rotatevol.o $(LDFLAGS) $(IMLIBS) $(LIBS)

rotmont: rotmont.o
	$(FC) -o rotmont rotmont.o $(LDFLAGS) $(IMLIBS) $(LIBS)

subimage: subimage.o
	$(FC) -o subimage subimage.o $(LDFLAGS) $(IMLIBS) $(LIBS)

subimanova: subimanova.o
	$(FC) -o subimanova subimanova.o $(LDFLAGS) $(IMLIBS) $(LIBS)

subimstat: subimstat.o
	$(FC) -o subimstat subimstat.o $(LDFLAGS) $(IMLIBS) $(LIBS)

taperoutvol: taperoutvol.o
	$(FC) -o taperoutvol taperoutvol.o $(LDFLAGS) $(IMLIBS) $(LIBS)

tapervoledge: tapervoledge.o
	$(FC) -o tapervoledge tapervoledge.o $(LDFLAGS) $(IMLIBS) $(LIBS)

tiltxcorr: tiltxcorr.o
	$(FC) -o tiltxcorr tiltxcorr.o $(LDFLAGS) $(IMLIBS) $(LIBS)

tomopieces: tomopieces.o
	$(FC) -o tomopieces tomopieces.o $(LDFLAGS) $(IMLIBS) $(LIBS)

vmstocsh: vmstocsh.o
	$(FC) -o vmstocsh vmstocsh.o $(LDFLAGS) $(IMLIBS) $(LIBS)

warpvol: warpvol.o
	$(FC) -o warpvol warpvol.o $(LDFLAGS) $(IMLIBS) $(LIBS)

xcorr: xcorr.o
	$(FC) -o xcorr xcorr.o $(LDFLAGS) $(IMLIBS) $(LIBS)

xfinverse: xfinverse.o
	$(FC) -o xfinverse xfinverse.o $(LDFLAGS) $(IMLIBS) $(LIBS)

xfproduct: xfproduct.o
	$(FC) -o xfproduct xfproduct.o $(LDFLAGS) $(IMLIBS) $(LIBS)

xfsimplex: xfsimplex.o
	$(FC) -o xfsimplex xfsimplex.o $(LDFLAGS) $(IMLIBS) $(LIBS)

xftoxg: xftoxg.o
	$(FC) -o xftoxg xftoxg.o $(LDFLAGS) $(IMLIBS) $(LIBS)

xyzproj: xyzproj.o
	$(FC) -o xyzproj xyzproj.o $(LDFLAGS) $(IMLIBS) $(LIBS)

install: $(PROGS)
	strip $(PROGS)
	$(CP) $(PROGS) $(INSTBINDIR)

clean:
	-\rm -f *.o *~ $(PROGS)
