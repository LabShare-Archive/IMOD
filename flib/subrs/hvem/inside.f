C       FUNCTION INSIDE -- CONTAINMENT TEST
C       !
C       Returns .true. if point [x], [y] is on or inside the polygon defined 
C       by vertices in [xvert], [yvert], or .false. if point is outside.
C       [np] is the number of vertices in polygon
C       !
c       New version by DNM, 8/23/00, replaces winding test with atan's.
c       Based on algorithm in "Computational Geometry in C", Joseph O'Rourke,
c       1998, with modifications to speed up search for ray crossings.
c       
      logical function inside(xvert,yvert,np,x,y)
      real*4 xvert(*),yvert(*)
      inside = InsideContour(xvert,yvert,np,x,y) .ne. 0
      return 
      end
