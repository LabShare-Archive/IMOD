c       Modules for alignment variables shared by tiltalign and beadtrack
c
c       $Id$
c       
c       Array size limits: in one place!
      module arraymaxes
      implicit none
      integer maxgrp,maxview
      parameter (maxgrp=20)
      integer maxprojpt,maxreal
      end module arraymaxes

c       Main alignment variables
      module alivar
      use arraymaxes
      implicit none
c       
      real*4, allocatable :: xx(:),yy(:),xyz(:,:),dxy(:,:)
      real*4, allocatable :: xresid(:),yresid(:)
      integer*4, allocatable :: isecview(:),irealstr(:)
c       
      integer*4, allocatable :: maptilt(:),mapgmag(:),mapcomp(:)
      integer*4, allocatable :: mapdmag(:),mapskew(:),maprot(:)
      integer*4, allocatable :: lintilt(:),lingmag(:),lincomp(:)
      integer*4, allocatable :: lindmag(:),linskew(:),linrot(:)
      integer*4, allocatable :: mapalf(:),linalf(:)
      integer*4 nrealpt,mapdmagstart,mapdumdmag,ifanyalf,nview,ifrotfix
      real*4 dumdmagfac
      real*4, allocatable :: frctilt(:),frcgmag(:),frccomp(:)
      real*4, allocatable :: frcdmag(:),frcskew(:),frcrot(:)
      real*4, allocatable :: rot(:),tilt(:),gmag(:),comp(:)
      real*4, allocatable :: tiltinc(:),dmag(:),skew(:)
      real*4, allocatable :: frcalf(:),alf(:)
      real*4 fixedtilt,fixedgmag,fixedcomp,fixeddmag,fixedskew
      real*4 fixedtilt2,fixedrot,fixedalf,projStrRot,projSkew,beamTilt
      integer*4, allocatable :: mapviewtofile(:),mapfiletoview(:)
      integer*4 nfileviews,mapProjStretch,mapBeamTilt
c       
      real*4, allocatable :: glbrot(:),glbtilt(:),glbalf(:)
      real*4, allocatable :: glbgmag(:),glbdmag(:),glbskew(:)
      integer*4 incrgmag,incrdmag,incrskew,incrtilt,incralf,incrrot

      CONTAINS

      subroutine allocateAlivar(numProjPt, numView, numReal, ierr)
      implicit none
      integer*4 numProjPt, numView, numReal, ierr
      maxProjPt = numprojPt + 10
      maxview = numView + 4
      maxreal = numReal + 4
      allocate(xx(maxprojpt),yy(maxprojpt),xyz(3,maxreal),dxy(2,maxview),
     &    xresid(maxprojpt),yresid(maxprojpt),
     &    isecview(maxprojpt),irealstr(maxreal),
     &    maptilt(maxview),mapgmag(maxview),mapcomp(maxview),
     &    mapdmag(maxview),mapskew(maxview),maprot(maxview),
     &    lintilt(maxview),lingmag(maxview),lincomp(maxview),
     &    lindmag(maxview),linskew(maxview),linrot(maxview),
     &    mapalf(maxview),linalf(maxview),
     &    frctilt(maxview),frcgmag(maxview),frccomp(maxview),
     &    frcdmag(maxview),frcskew(maxview),frcrot(maxview),
     &    rot(maxview),tilt(maxview),gmag(maxview),comp(maxview),
     &    tiltinc(maxview),dmag(maxview),skew(maxview),
     &    frcalf(maxview),alf(maxview),mapviewtofile(maxview),mapfiletoview(nfileviews),
     &    glbrot(maxview),glbtilt(maxview),glbalf(maxview),
     &    glbgmag(maxview),glbdmag(maxview),glbskew(maxview), stat=ierr)
      return
      end subroutine allocateAlivar
      end module alivar

c       Mapping variables
      module mapsep
      use arraymaxes
      implicit none
      integer*4 nsepingrp(maxgrp),ngsep
      integer*4, allocatable :: ivsep(:,:)

      CONTAINS

      subroutine allocateMapsep(ierr)
      integer*4 ierr
      allocate(ivsep(maxview,maxgrp), stat=ierr)
      return
      end subroutine allocateMapsep
      end module mapsep
