c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c	DNM 3/1/01: make this an include file to facilitate increasing the
c       number of units

	integer maxunit,maxstream
	parameter (maxunit=20,maxstream=30)
	integer*4 LSTREAM(maxstream),NBHDR,NBW,NBW3,NB(5),NBL,
     &      NCRS(3,maxunit),MODE(maxunit),NCRST(3,maxunit),
     &	    NXYZ(3,maxunit),MAPCRS(3,maxunit),NLAB(maxunit),
     &	    LABLS(20,10,maxunit),nbsym(maxunit),numopen,
     &      lrecspi(maxunit),lbasspi(maxunit)

	real*4 DENMMM(3,maxunit),STUFF(31,maxunit),ORIGXY(2,maxunit),
     &      CEL(6,maxunit)
	logical FLAG(maxunit),NOCON(maxunit),spider(maxunit),
     &      mrcflip(maxunit),print
C
C	CER 6/29/00 replace $ with G for g77 in im$com and im$bit
C
	COMMON/IMGCOM/ LSTREAM,NBHDR,NBW,NBW3,NB,NBL, NCRS,MODE,
     &	    NCRST, NXYZ,CEL,MAPCRS, DENMMM,STUFF,ORIGXY, NLAB,LABLS,
     &	    FLAG, NOCON,nbsym,numopen,print, spider,lrecspi,lbasspi,
     &      mrcflip

	integer*4  ibleft(maxunit)
        byte bytcur(maxunit)
        common /imGbit/ ibleft,bytcur
