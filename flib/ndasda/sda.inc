      parameter (maxverts=1000000,maxtri=maxverts,maxpoly=10000, maxpore=10000)
      real*4 verts(3,maxverts)                  !normal-vertex array
      real*4 vertout(3,maxverts)                !normal-vertex array for output
      integer*4 listvert(maxverts/2)            !list of indexes of vertices
      integer*4 indvert(3,maxtri)               !index to 3 vertices of triangl
      integer*4 indnorm(3,maxtri)               !index to normals at vertices
      integer*4 indedge(3,maxtri)               !# of triangle over each edge
      integer*4 numedge(3,maxtri)               !# of edge in adjacent triangle
      real*4 coeff(4,3,maxtri)                  !normal interpolation coefficnt
      real*4 triarea(maxtri)                    !area of triangles
      integer*4 ninpoly(maxpoly)                !# of triangles in polygon
      integer*4 istrpoly(maxpoly)               !starting triangle on Z-plane
      integer*4 nvert                           !# of vertices
      integer*4 npoly                           !# of polygons = Z-planes
      integer*4 ntriang                         !# of triangles
      real*4 pore(3,maxpore)                    !pore location, on surface
      integer*4 itypore(maxpore)                !object # (type) of pore
      integer*4 itype(maxpore)                  !possibly modified types
      integer*4 itripore(maxpore)               !triangle that pore is in
      integer*4 npore                           !# of pores on current surface
      real*4 trisize                            !mean "size" of triangle
      real*4 triszsq                            !mean "size" squared
      real*4 volsize                            !largest extent of volume
      common /sdacom/npoly,ntriang,npore,nvert,trisize,triszsq, volsize,verts,indvert,
     &    indnorm, indedge,numedge,coeff,triarea,ninpoly,istrpoly,pore,itype,
     &    itripore,itypore,listvert,vertout

