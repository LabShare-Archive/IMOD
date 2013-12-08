      integer limverts,limtri,limpoly,limeshobj,limsurf,limcont,limshift
      parameter (limverts=900000,limtri=limverts*2,limpoly=50000)
      parameter (limeshobj=255,limsurf=10000,limcont=limpoly)
      parameter (limshift=20000)
      real*4 verts(3,limverts)                  !vertex array
      integer*4 indvert(3,limtri)               !index to 3 vertices of triangl
      real*4 xyrot(3,2,limtri)                  !rotated X and Y coords
      real*4 zrot(limtri)                       !rotated Z coord
      real*4 cbet(limtri),sbet(limtri)          !cosine and sine of beta
      real*4 cgam(limtri),sgam(limtri)          !cosine and sine of gamma
      real*4 trixmin(limtri),trixmax(limtri)
      real*4 triymin(limtri),triymax(limtri)
      real*4 trizmin(limtri),trizmax(limtri)
      real*4 polyxmin(limpoly),polyxmax(limpoly)
      real*4 polyymin(limpoly),polyymax(limpoly)
      real*4 polyzmin(limpoly),polyzmax(limpoly)
      real*4 surfxmin(limsurf),surfxmax(limsurf)
      real*4 surfymin(limsurf),surfymax(limsurf)
      real*4 surfzmin(limsurf),surfzmax(limsurf)
      real*4 contxmin(limcont),contxmax(limcont)
      real*4 contymin(limcont),contymax(limcont)
      real*4 contzval(limcont)
      real*4 polyarea(limpoly),surfarea(limsurf)
      real*4 shifts(3,limshift)
      logical*1 shifted(limshift)
      integer*4 ninpoly(limpoly)                !# of triangles in polygon
      integer*4 istrpoly(limpoly)               !starting triangle on Z-plane
      integer*4 iobjmesh(limeshobj)             !objects with meshes loaded
      integer*4 iobjpoly(limeshobj)             !starting polygon of object
      integer*4 npolyobj(limeshobj)             !# of polygons in object
      integer*4 listsurf(limpoly)               !list of polygons by surface
      integer*4 ninsurf(limsurf)                !# of polygons in surface
      integer*4 istrsurf(limsurf)               !starting polygon of surface
      integer*4 iobjsurf(limeshobj)             !starting surface of object
      integer*4 nsurfobj(limeshobj)             !# of surfaces in object
      integer*4 listcont(limcont)               !list of contours by surface
      integer*4 istrcont(limsurf)               !starting contour of surface
      integer*4 ncontinsurf(limsurf)            !# of contours in surface
      integer*4 iobjshift(limeshobj)            !list of shifted objects 
      integer*4 nitemshift(limeshobj)           !# of items shifted in object
      integer*4 istrshift(limeshobj)            !index to first shift of object
      integer*4 nverts                          !# of vertices loaded
      integer*4 ntriang                         !# of triangles
      integer*4 npoly                           !# of polygons
      integer*4 nsurf                           !# of surfaces
      integer*4 ncont                           !# of contours
      integer*4 nmeshloaded                     !# of meshes loaded
      integer*4 nobjshifted                     !# of objects shifted
      common /mtkcom/npoly,ntriang,nverts,nsurf,ncont,nmeshloaded,
     &    nobjshifted,
     &    verts,indvert,xyrot,zrot,cbet,sbet,cgam,sgam,
     &    ninpoly,istrpoly,iobjmesh,iobjpoly,npolyobj,listsurf,
     &    ninsurf,istrsurf,iobjsurf,nsurfobj, listcont,istrcont,
     &    ncontinsurf,iobjshift,nitemshift,istrshift,shifts,shifted,
     &    trixmin,trixmax,triymin,triymax,trizmin,trizmax,
     &    polyxmin,polyxmax,polyymin,polyymax,polyzmin,polyzmax,
     &    surfxmin,surfxmax,surfymin,surfymax,surfzmin,surfzmax,
     &    contxmin,contxmax,contymin,contymax,contzval,
     &    polyarea,surfarea
