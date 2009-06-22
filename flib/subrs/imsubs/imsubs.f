c       Module for imsubs variables
c
c       $Id$
c       Log at end of file
c
      module imsubs
      implicit none
      integer maxunit,maxstream
      parameter (maxunit=20,maxstream=30)
      integer*4 LSTREAM(maxstream),NBHDR,NBW,NBW3,NB(7),NBL,
     &    NCRS(3,maxunit),MODE(maxunit),NCRST(3,maxunit),
     &    NXYZ(3,maxunit),MAPCRS(3,maxunit),NLAB(maxunit),
     &    LABLS(20,10,maxunit),nbsym(maxunit),numopen,ifBrief,
     &    lrecspi(maxunit),lbasspi(maxunit)

      real*4 DENMMM(3,maxunit),STUFF(27,maxunit),ORIGXYZ(3,maxunit),
     &    CEL(6,maxunit),rms(maxunit)
      integer*1 cmap(4,maxunit),stamp(4,maxunit)
      logical FLAG(maxunit),NOCON(maxunit),spider(maxunit),
     &    mrcflip(maxunit),print
      integer*4 istuff(27,maxunit)
      equivalence (istuff,stuff)

      integer*4  ibleft(maxunit)
      integer*1 bytcur(maxunit)
      DATA NBHDR/1024/, NBW/4/, NBW3/12/, NB/1,2,4,4,8,2,2/, NBL/800/
      DATA FLAG/maxunit*.TRUE./, NOCON/maxunit*.FALSE./, numopen/0/
      data spider/maxunit*.false./, print/.true./, ifBrief/-1/
      data ibleft/maxunit*0/
      end module imsubs

c       $Log$
c       Revision 3.6  2006/09/28 21:23:00  mast
c       Added brief header output variable
c
c       Revision 3.5  2005/12/09 04:39:44  mast
c       gfortran: .xor., continuation, byte, or open fixes
c       
c       Revision 3.4  2005/11/11 22:36:22  mast
c       Changes for unsigned mode
c       
c       Revision 3.3  2002/07/31 17:42:59  mast
c       For new format, defined cmap,rms, and stamp, changed origxy to
c       origxyz, changed size of stuff to 27
c       
c       Revision 3.2  2002/06/26 00:24:59  mast
c       *** empty log message ***
c       
c       Revision 3.1  2002/06/26 00:24:42  mast
c       Made explicit declarations of all variables, for use with implicit
c       none
c       
c       DNM 3/1/01: make this an include file to facilitate increasing the
c       number of units
