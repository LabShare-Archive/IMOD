c	DNM 3/1/01: make this an include file to facilitate increasing the
c       number of units
	parameter (maxunit=20,maxstream=30)
C
C	CER 6/29/00 replace $ with G for g77 in im$com and im$bit
C
	COMMON/IMGCOM/ LSTREAM(maxstream),NBHDR,NBW,NBW3,NB(5),NBL,
     &      NCRS(3,maxunit),MODE(maxunit),NCRST(3,maxunit),
     &      NXYZ(3,maxunit),CEL(6,maxunit),MAPCRS(3,maxunit),
     &      DENMMM(3,maxunit),STUFF(31,maxunit),ORIGXY(2,maxunit),
     &      NLAB(maxunit),LABLS(20,10,maxunit),FLAG(maxunit),
     &      NOCON(maxunit),nbsym(maxunit),numopen,print,
     &      spider(maxunit),lrecspi(maxunit),lbasspi(maxunit),
     &      mrcflip(maxunit)
	LOGICAL FLAG,NOCON,print,spider,mrcflip
        common /imGbit/ ibleft(maxunit),bytcur(maxunit)
        byte bytcur
