c       Modules for alignment variables shared by tiltalign and beadtrack
c
c       $Id$
c       
c       Array size limits: in one place!
      module arraymaxes
      implicit none
      integer maxgrp,maxview
      parameter (maxgrp=20,maxview=1440)
      integer maxprojpt,maxreal
      parameter (maxprojpt=140000,maxreal=1000)
      end module arraymaxes

c       Main alignment variables
      module alivar
      use arraymaxes
      implicit none
c       
      real*4 xx(maxprojpt),yy(maxprojpt),xyz(3,maxreal),dxy(2,maxview)
      real*4 xresid(maxprojpt),yresid(maxprojpt)
      integer*4 nrealpt,isecview(maxprojpt),irealstr(maxreal)
c       
      integer*4 maptilt(maxview),mapgmag(maxview),mapcomp(maxview)
      integer*4 mapdmag(maxview),mapskew(maxview),maprot(maxview)
      integer*4 lintilt(maxview),lingmag(maxview),lincomp(maxview)
      integer*4 lindmag(maxview),linskew(maxview),linrot(maxview)
      integer*4 mapalf(maxview),linalf(maxview)
      integer*4 mapdmagstart,mapdumdmag,ifanyalf,nview,ifrotfix
      real*4 dumdmagfac
      real*4 frctilt(maxview),frcgmag(maxview),frccomp(maxview)
      real*4 frcdmag(maxview),frcskew(maxview),frcrot(maxview)
      real*4 rot(maxview),tilt(maxview),gmag(maxview),comp(maxview)
      real*4 tiltinc(maxview),dmag(maxview),skew(maxview)
      real*4 frcalf(maxview),alf(maxview)
      real*4 fixedtilt,fixedgmag,fixedcomp,fixeddmag,fixedskew
      real*4 fixedtilt2,fixedrot,fixedalf,projStrRot,projSkew,beamTilt
      integer*4 mapviewtofile(maxview),mapfiletoview(maxview)
      integer*4 nfileviews,mapProjStretch,mapBeamTilt
c       
      real*4 glbrot(maxview),glbtilt(maxview),glbalf(maxview)
      real*4 glbgmag(maxview),glbdmag(maxview),glbskew(maxview)
      integer*4 incrgmag,incrdmag,incrskew,incrtilt,incralf,incrrot
      end module alivar

c       Mapping variables
      module mapsep
      use arraymaxes
      implicit none
      integer*4 ivsep(maxview,maxgrp),nsepingrp(maxgrp),ngsep
      end module mapsep
