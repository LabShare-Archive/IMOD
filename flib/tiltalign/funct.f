c****   FUNCT performs the necessary tasks required by the METRO routine for
c       conjugate gradient minimization.  For the current values of the
c       all alignment variables such as rotation, tilt, and mag (which are
c       obtained, whether they are fixed values or variables, by calling the
c       routine REMAP_PARAMS), and the current values of the real-space (x,y,
c       z) coordinates of the points (which are kept in the list VAR, after
c       the "geometric" variables on that list), it computes the projection
c       coordinates of each point, the best displacement dx, dy, for each
c       view; the residuals for each point (computed minus measured
c       projection coordinate), and the error term, which is the sum of the
c       squares of the residuals.  It then computes the derivative of the
c       error with respect to each variable and returns these derivatives in
c       the array GRAD.  The derivatives with respect to the geometric
c       variables are obtained from the following relations:
c       _    xproj = a*x + b*y + c*z + dx(view)
c       _    xproj = d*x + e*y + f*z + dy(view)
c       where the six terms are a product of distortion, X-axis tilt, Y-axis
c       tilt, projection, and image rotation matrices.  The latest version
c       takes the derivative of one of component matrices and forms the
c       product with that derivative and the rest of the matrices.
c       
c       The derivatives with respect to the coordinate variables are obtained
c       from expressions relating the projection coordinates to the real-
c       space (x,y,z) coordinates of all but the last point.  These
c       expressions incorporate the constraint that the centroid of the
c       (x,y,z) points is (0,0,0) and the fact that the total error will be a
c       minimum when the centroid of the measured projection points in one
c       view matches the centroid of the projection of the real-space
c       points.  ANGLES ARE EXPECTED TO BE RADIANS.
c       
c       $Id$
c       Log at end of file
c       
      subroutine funct(nvarsrch,var,ferror,grad)
c       
      implicit none
      include 'alivar.inc'
      integer ms
      parameter (ms=maxview)
c       
      real*4 grad(*),var(*),ferror
      integer*4 nvarsrch
c       
      double precision error, gradsum
      logical*1 realinview(maxprojpt)
c       
      real*4 xbar(ms),ybar(ms),xproj(maxprojpt),yproj(maxprojpt)
      real*4 xcen(ms),ycen(ms),zcen(ms)
c       
c       a, b, etc are the quantities in the above equations for each view
c       aon is a over n (# of points in that view)
c       aprime is derivative of a with respect to tilt angle
c       
      real*4 a(ms),b(ms),c(ms),d(ms),e(ms),f(ms)
      real*4 aon(ms),bon(ms),con(ms),don(ms),eon(ms),fon(ms)
      real*4 cbeta(ms),sbeta(ms),calf(ms),salf(ms)
      real*4 cgam(ms),sgam(ms),cdel(ms),sdel(ms),xmag(ms)
c       
      integer*2 indvreal(maxprojpt)
      integer*4 nptinview(maxview),indvproj(maxprojpt)
      real*4 coefx(3*maxreal),coefy(3*maxreal), resprod(6,maxprojpt)
      real*4 dmat(9,ms), xtmat(9,ms), ytmat(9,ms), rmat(4,ms), dermat(9)
      real*4 projMat(4), beamInv(9,ms), beamMat(6,ms), umat(9)
      save xbar,ybar,nptinview,realinview,indvproj,indvreal
c       
      logical firsttime,xyzfixed
      common /functfirst/ firsttime,xyzfixed
c       
      integer*4 nprojpt, iv, jpt, i, ivbase,nvmat,icoordbas,kxlas,kylas,kzlas
      integer*4 kz,kx,ky,ipt,ivar,iptinv,jx,jy,jz,iy,ix,kpt,jj, istrType
      real*4 afac,bfac,cfac,dfac,efac,ffac,xpxrlas,xpyrlas,xpzrlas,ypxrlas
      real*4 ypyrlas,ypzrlas,valadd,cosPSkew,sinPSkew,cosBeam,sinBeam
      real*4 cos2rot, sin2rot
      real*8 gradientSum
c       
c       Stretch type: 1 for dmag = stretch on X axis, skew = X axis rotation
c       2 for dmag = stretch on X axis, skew = +Y-axis and -X-axis rotation
c       3 for dmag = +Y-axis and -X-axis stretch, skew = +Y-axis and -X-axis 
c       rotation
c       Since the X-stretch frequently occurs because of thinning, the third
c       formulation would often affect mag inappropriately.
c       Neither the second nor the third seemed to reduce solved rotation
c       
      istrType = 1
c       
c       first time in, precompute the mean projection coords in each view
c       and build indexes to the points in each view.
c       
      nprojpt=irealstr(nrealpt+1)-1
      if(firsttime)then
        if(nrealpt*nview.gt.maxprojpt)call errorexit(
     &      'TOO MANY 3-D POINTS AND VIEWS FOR ARRAYS IN FUNCT', 0)
        do iv=1,nview
          xbar(iv)=0.
          ybar(iv)=0.
          nptinview(iv)=0
        enddo
c         
        do jpt=1,nrealpt
          do iv=1,nview
            realinview(jpt+(iv-1)*nrealpt)=.false.
          enddo
          do i=irealstr(jpt),irealstr(jpt+1)-1
            iv=isecview(i)
            ivbase=(iv-1)*nrealpt
            nptinview(iv)=nptinview(iv)+1
            indvreal(ivbase+nptinview(iv))=jpt
            indvproj(ivbase+nptinview(iv))=i
            realinview(jpt+ivbase)=.true.
            xbar(iv)=xbar(iv)+xx(i)
            ybar(iv)=ybar(iv)+yy(i)
          enddo
        enddo
c         
        do iv=1,nview
          xbar(iv)=xbar(iv)/nptinview(iv)
          ybar(iv)=ybar(iv)/nptinview(iv)
        enddo
        firsttime=.false.
c$$$      call remap_params(var)
c$$$      do iv=1,nview
c$$$      write(*,113)rot(iv),maprot(iv),linrot(iv),frcrot(iv),
c$$$      &              tilt(iv),maptilt(iv),lintilt(iv),frctilt(iv),
c$$$      &              gmag(iv),mapgmag(iv),lingmag(iv),frcgmag(iv)
c$$$      113     format(f11.7,2i4,f6.3,f11.7,2i4,f6.3,f11.7,2i4,f6.3)
c$$$      enddo
c$$$      print *,nvarsrch
c$$$      write(*,'(7f11.7)')(var(i),i=1,nvarsrch)
      endif
c       
c       precompute the a-f and items related to them.  Store the component
c       matrices to use for computing gradient coefficients
c       
      call remap_params(var)
c       
      call fill_proj_matrix(projStrRot, projSkew, projMat, cosPSkew,
     &    sinPSkew, cos2rot, sin2rot)
      call fill_beam_matrices(beamTilt, beamInv, beamMat, cosBeam, sinBeam)
      do i=1,nview
        xmag(i)=gmag(i)+dmag(i)
        call fill_dist_matrix(gmag(i), dmag(i), skew(i), comp(i),
     &      istrType, dmat(1,i), cdel(i), sdel(i))
        call fill_xtilt_matrix(alf(i), ifanyalf, xtmat(1,i), calf(i),
     &      salf(i))
        call fill_ytilt_matrix(tilt(i), ytmat(1,i), cbeta(i), sbeta(i))
        call fill_rot_matrix(rot(i), rmat(1,i), cgam(i), sgam(i))
        call matrix_to_coef(dmat(1,i), xtmat(1,i), beamInv, ytmat(1,i),
     &      beamMat, projMat, rmat(1,i), a(i), b(i), c(i), d(i), e(i), f(i))
      enddo
c       
      do i=1,nview
        aon(i)=-a(i)/nptinview(i)
        bon(i)=-b(i)/nptinview(i)
        con(i)=-c(i)/nptinview(i)
        don(i)=-d(i)/nptinview(i)
        eon(i)=-e(i)/nptinview(i)
        fon(i)=-f(i)/nptinview(i)
      enddo
c       
c       s0=secnds(0.)
      nvmat=3*(nrealpt-1)                       !# of x,y,z variables
      icoordbas=nvarsrch-nvmat                  !offset to x,y,z's
      kzlas=icoordbas+nrealpt*3                 !indexes of x,y,z of last point
      kylas=kzlas-1
      kxlas=kylas-1
c       
c       get xproj and yproj: for now, these will be projected points minus
c       the dx, dy values
c       compute the coordinates of the last point: minus the sum of the rest
c       
      do iv=1,nview
        xcen(iv)=0.
        ycen(iv)=0.
        zcen(iv)=0.
      enddo
      var(kxlas)=0.
      var(kylas)=0.
      var(kzlas)=0.
c       
      do jpt=1,nrealpt
        kz=icoordbas+jpt*3
        ky=kz-1
        kx=ky-1
c         
        if(jpt.lt.nrealpt)then                  !accumulate last point coords
          var(kxlas)=var(kxlas)-var(kx)
          var(kylas)=var(kylas)-var(ky)
          var(kzlas)=var(kzlas)-var(kz)
        endif
c         
        xyz(1,jpt)=var(kx)                      !unpack the coordinates
        xyz(2,jpt)=var(ky)
        xyz(3,jpt)=var(kz)
c         
        do i=irealstr(jpt),irealstr(jpt+1)-1
          iv=isecview(i)
          xproj(i)=a(iv)*var(kx)+b(iv)*var(ky)+c(iv)*var(kz)
          yproj(i)=d(iv)*var(kx)+e(iv)*var(ky)+f(iv)*var(kz)
          xcen(iv)=xcen(iv)+var(kx)
          ycen(iv)=ycen(iv)+var(ky)
          zcen(iv)=zcen(iv)+var(kz)
        enddo
      enddo
c       
c       get xcen, ycen, zcen scaled, and get the dx and dy
c       
      do iv=1,nview
        xcen(iv)=xcen(iv)/nptinview(iv)
        ycen(iv)=ycen(iv)/nptinview(iv)
        zcen(iv)=zcen(iv)/nptinview(iv)
        dxy(1,iv)=xbar(iv)
     &      -a(iv)*xcen(iv)-b(iv)*ycen(iv)-c(iv)*zcen(iv)
        dxy(2,iv)=ybar(iv)
     &      -d(iv)*xcen(iv)-e(iv)*ycen(iv)-f(iv)*zcen(iv)
      enddo
c       
c       adjust xproj&yproj by dxy, get residuals and errors
c       
      error=0.
      do i=1,nprojpt
        iv=isecview(i)
        xproj(i)=xproj(i)+dxy(1,iv)
        yproj(i)=yproj(i)+dxy(2,iv)
        xresid(i)=xproj(i)-xx(i)
        yresid(i)=yproj(i)-yy(i)
        error=error + xresid(i)**2 + yresid(i)**2
      enddo
c       
c       precompute products needed for gradients
c       
      do iv=1,nview
        ivbase=(iv-1)*nrealpt
        do iptinv=1,nptinview(iv)
          ipt=indvproj(ivbase+iptinv)
          jpt=indvreal(ivbase+iptinv)
          resprod(1,ipt) = 2. * (xyz(1,jpt) - xcen(iv)) * xresid(ipt)
          resprod(2,ipt) = 2. * (xyz(2,jpt) - ycen(iv)) * xresid(ipt)
          resprod(3,ipt) = 2. * (xyz(3,jpt) - zcen(iv)) * xresid(ipt)
          resprod(4,ipt) = 2. * (xyz(1,jpt) - xcen(iv)) * yresid(ipt)
          resprod(5,ipt) = 2. * (xyz(2,jpt) - ycen(iv)) * yresid(ipt)
          resprod(6,ipt) = 2. * (xyz(3,jpt) - zcen(iv)) * yresid(ipt)
        enddo
      enddo

      ferror=error
c       write(*,'(f25.15)')error
c       
c       compute derivatives of error w/r to search parameters
c       first clear out all the gradients
c       
      do ivar=1,nvarsrch
        grad(ivar)=0.
      enddo
c       
c       loop on views: consider each of the parameters
c       
      ivar=0
      do iv=1,nview
        ivbase=(iv-1)*nrealpt
c         
c         rotation: add gradient for this view to any variables that it is
c         mapped to
c         These equations are valid as long as rotation is the last operation
c         
        gradsum=0.
        if(maprot(iv).gt.0)then
          do iptinv=1,nptinview(iv)
            ipt=indvproj(ivbase+iptinv)
            gradsum=gradsum+2.*
     &          ((ybar(iv)-yproj(ipt))*xresid(ipt)
     &          +(xproj(ipt)-xbar(iv))*yresid(ipt))
          enddo
          grad(maprot(iv))=grad(maprot(iv))+frcrot(iv)*gradsum
          if(linrot(iv).gt.0) grad(linrot(iv))=grad(linrot(iv))+
     &        (1.-frcrot(iv))*gradsum
        endif
c         
c         tilt: add gradient for this tilt angle to the variable it is mapped
c         from, if any
c         
        if(maptilt(iv).ne.0)then
          call zero_matrix(dermat, 9)
          dermat(1)=-sbeta(iv)
          dermat(3)=cbeta(iv)
          dermat(7)=-cbeta(iv)
          dermat(9)=-sbeta(iv)

          call matrix_to_coef(dmat(1,iv),xtmat(1,iv),beamInv,dermat, beamMat,
     &        projMat, rmat(1,iv), afac,bfac,cfac,dfac,efac,ffac)
          gradsum = gradientSum(indvproj, ivbase, nptinview(iv), resprod,
     &        afac, bfac, cfac, dfac, efac, ffac)
          grad(maptilt(iv))=grad(maptilt(iv))+frctilt(iv)*gradsum
          if(lintilt(iv).gt.0) grad(lintilt(iv))=grad(lintilt(iv))+
     &        (1.-frctilt(iv))*gradsum
c           
        endif
c         
c         mag: add gradient for this view to the variable it is mapped from
c         
        if(mapgmag(iv).gt.0)then
          call zero_matrix(dermat, 9)
          if (istrType .eq. 1)then
            dermat(1)=cdel(iv)
            dermat(4)=sdel(iv)
            dermat(5)=1.
          else
            dermat(1)=cdel(iv)
            dermat(2)=-sdel(iv)
            dermat(4)=-sdel(iv)
            dermat(5)=cdel(iv)
          endif
          dermat(9)=comp(iv)
          call matrix_to_coef(dermat,xtmat(1,iv),beamInv,ytmat(1,iv),beamMat,
     &        projMat, rmat(1,iv), afac,bfac,cfac,dfac,efac,ffac)
          gradsum = gradientSum(indvproj, ivbase, nptinview(iv), resprod,
     &        afac, bfac, cfac, dfac, efac, ffac)
c           write(*,'(i4,3f9.5,f16.10)')iv, gmag(iv),dmag(iv),skew(iv),gradsum
          grad(mapgmag(iv))=grad(mapgmag(iv))+frcgmag(iv)*gradsum
          if(lingmag(iv).gt.0) grad(lingmag(iv))=grad(lingmag(iv))+
     &        (1.-frcgmag(iv))*gradsum
        endif
c         
c         comp: add gradient for this view to the variable it is mapped from
c         These equation are valid as long as the there is nothing else in
c         the final column of the distortion matrix
c         
        if(mapcomp(iv).gt.0)then
          gradsum=0.
          cfac=c(iv)/comp(iv)
          ffac=f(iv)/comp(iv)
          do iptinv=1,nptinview(iv)
            ipt=indvproj(ivbase+iptinv)
            jpt=indvreal(ivbase+iptinv)
            gradsum = gradsum +  cfac * resprod(3,ipt) + ffac*resprod(6,ipt)
          enddo
          grad(mapcomp(iv))=grad(mapcomp(iv))+frccomp(iv)*gradsum
          if(lincomp(iv).gt.0) grad(lincomp(iv))=grad(lincomp(iv))+
     &        (1.-frccomp(iv))*gradsum
        endif
c         
c         dmag: add gradient for this view to the variable it is mapped from
c         
        if(mapdmag(iv).gt.0)then
          call zero_matrix(dermat, 9)
          if (istrType .eq. 1)then
            dermat(1)=cdel(iv)
            dermat(4)=sdel(iv)
          else if (istrType .eq. 2) then
            dermat(1)=cdel(iv)
            dermat(4)=-sdel(iv)
          else
            dermat(1)=-cdel(iv)
            dermat(2)=-sdel(iv)
            dermat(4)=sdel(iv)
            dermat(5)=cdel(iv)
          endif
          call matrix_to_coef(dermat,xtmat(1,iv),beamInv,ytmat(1,iv),beamMat,
     &        projMat, rmat(1,iv), afac,bfac,cfac,dfac,efac,ffac)
          gradsum = gradientSum(indvproj, ivbase, nptinview(iv), resprod,
     &        afac, bfac, cfac, dfac, efac, ffac)
c           
c           if this parameter maps to the dummy dmag, then need to subtract
c           the fraction times the gradient sum from gradient of every real
c           variable
c           
          if(mapdmag(iv).eq.mapdumdmag.or.lindmag(iv).eq.mapdumdmag)
     &        then
            if(mapdmag(iv).eq.mapdumdmag)then
              valadd=dumdmagfac*frcdmag(iv)*gradsum
              if(lindmag(iv).gt.0) grad(lindmag(iv))=
     &            grad(lindmag(iv))+(1.-frcdmag(iv))*gradsum
            else
              valadd=dumdmagfac*(1.-frcdmag(iv))*gradsum
              grad(mapdmag(iv))=grad(mapdmag(iv))+frcdmag(iv)*gradsum
            endif
            do jj=mapdmagstart,mapdumdmag-1
              grad(jj)=grad(jj)+valadd
            enddo
          else
            grad(mapdmag(iv))=grad(mapdmag(iv))+frcdmag(iv)*gradsum
            if(lindmag(iv).gt.0) grad(lindmag(iv))=grad(lindmag(iv))+
     &          (1.-frcdmag(iv))*gradsum
          endif
        endif
c         
c         skew: add gradient for this view to the variable it is mapped from
c         
        if(mapskew(iv).gt.0)then
          call zero_matrix(dermat, 9)
          if (istrType .eq. 1)then
            dermat(1)=-xmag(iv)*sdel(iv)
            dermat(4)=xmag(iv)*cdel(iv)
          else if (istrType .eq. 2)then
            dermat(1)=-xmag(iv)*sdel(iv)
            dermat(2)=-gmag(iv)*cdel(iv)
            dermat(4)=-xmag(iv)*cdel(iv)
            dermat(5)=-gmag(iv)*sdel(iv)
          else
            dermat(1)=-(gmag(iv)-dmag(iv))*sdel(iv)
            dermat(2)=-xmag(iv)*cdel(iv)
            dermat(4)=-(gmag(iv)-dmag(iv))*cdel(iv)
            dermat(5)=-xmag(iv)*sdel(iv)
          endif
          call matrix_to_coef(dermat,xtmat(1,iv),beamInv,ytmat(1,iv),beamMat,
     &        projMat, rmat(1,iv), afac,bfac,cfac,dfac,efac,ffac)
          gradsum = gradientSum(indvproj, ivbase, nptinview(iv), resprod,
     &        afac, bfac, cfac, dfac, efac, ffac)
          grad(mapskew(iv))=grad(mapskew(iv))+frcskew(iv)*gradsum
          if(linskew(iv).gt.0) grad(linskew(iv))=grad(linskew(iv))+
     &        (1.-frcskew(iv))*gradsum
c           
        endif
c         
c         alpha: add gradient for this view to the variable it is mapped from
c         
        if(mapalf(iv).gt.0)then
          call zero_matrix(dermat, 9)
          dermat(5)=-salf(iv)
          dermat(6)=-calf(iv)
          dermat(8)=calf(iv)
          dermat(9)=-salf(iv)
          call matrix_to_coef(dmat(1,iv),dermat,beamInv,ytmat(1,iv),beamMat,
     &        projMat, rmat(1,iv), afac,bfac,cfac,dfac,efac,ffac)
          gradsum = gradientSum(indvproj, ivbase, nptinview(iv), resprod,
     &        afac, bfac, cfac, dfac, efac, ffac)
c           write(*,'(3i4,f7.4,f16.10)')iv,mapalf(iv),linalf(iv)
c           &         ,frcalf(iv),gradsum
          grad(mapalf(iv))=grad(mapalf(iv))+frcalf(iv)*gradsum
          if(linalf(iv).gt.0) grad(linalf(iv))=grad(linalf(iv))+
     &        (1.-frcalf(iv))*gradsum
        endif
      enddo
c       
c       projection skew: do gradient
c       
      if (mapProjStretch .gt. 0) then
        grad(mapProjStretch) = 0.
c         
        dermat(1) = -sinPSkew + cosPSkew * sin2rot
        dermat(2) = -cosPSkew * cos2rot
        dermat(3) = dermat(2)
        dermat(4) = -sinPSkew - cosPSkew * sin2rot
        do iv =1 ,nview
          ivbase=(iv-1)*nrealpt
          call matrix_to_coef(dmat(1,iv),xtmat(1,iv),beamInv,ytmat(1,iv),
     &        beamMat, dermat,rmat(1,iv), afac,bfac,cfac,dfac,efac,ffac)
          gradsum = gradientSum(indvproj, ivbase, nptinview(iv), resprod,
     &        afac, bfac, cfac, dfac, efac, ffac)
          grad(mapProjStretch) = grad(mapProjStretch) + gradsum
        enddo
      endif
c       
c       beam tilt: the derivative is of the product of beamInv, ytmat and
c       beamMat, so pass two unit matrices and derivative
c       
      if (mapBeamTilt .gt. 0) then
        grad(mapBeamTilt) = 0.
        dermat(1) = 0.
        call zero_matrix(umat, 9)
        umat(1) = 1.
        umat(5) = 1.
        umat(9) = 1.
        do iv = 1, nview
          ivbase=(iv-1)*nrealpt
          dermat(2) = -cosBeam * sbeta(iv)
          dermat(3) = -sinBeam * sbeta(iv)
          dermat(4) = cosBeam * sbeta(iv)
          dermat(5) = 2. * cosBeam * sinBeam * (cbeta(iv) - 1.)
          dermat(6) = (cosBeam**2 - sinBeam**2) * (1- cbeta(iv))
          call matrix_to_coef(dmat(1,iv),xtmat(1,iv),umat,umat,
     &        dermat, projMat,rmat(1,iv), afac,bfac,cfac,dfac,efac,ffac)
          gradsum = gradientSum(indvproj, ivbase, nptinview(iv), resprod,
     &        afac, bfac, cfac, dfac, efac, ffac)
          grad(mapBeamTilt) = grad(mapBeamTilt) + gradsum
        enddo
      endif
c       
c       loop on points, get derivatives w/r to x, y, or z
c       
      if(xyzfixed)return
      do jpt=1,nrealpt
        jz=3*jpt
        jy=jz-1
        jx=jy-1
        iy=0
c         
c         for each projection of the real point, find how that point
c         contributes to the derivative w/r to each of the x,y,z
c         
        do i=irealstr(jpt),irealstr(jpt+1)-1
          iv=isecview(i)
          ivbase=(iv-1)*nrealpt
          iy=iy+2
          ix=iy-1
c           
c           the relation between the projection (x,y) and the set of (x,y,z)
c           contains the term dxy, which is actually a sum of the (x,y,z).
c           There is a 3 by 3 matrix of possibilities: the first set of 3
c           possibilities is whether this real point is the last one, or
c           whether the last point is projected in this view or not.
c           
          if(jpt.eq.nrealpt)then
            xpxrlas=-a(iv)
            xpyrlas=-b(iv)
            xpzrlas=-c(iv)
            ypxrlas=-d(iv)
            ypyrlas=-e(iv)
            ypzrlas=-f(iv)
          elseif(realinview(nrealpt+ivbase))then
            xpxrlas=0.
            xpyrlas=0.
            xpzrlas=0.
            ypxrlas=0.
            ypyrlas=0.
            ypzrlas=0.
          else
            xpxrlas=-aon(iv)
            xpyrlas=-bon(iv)
            xpzrlas=-con(iv)
            ypxrlas=-don(iv)
            ypyrlas=-eon(iv)
            ypzrlas=-fon(iv)
          endif
c           
c           The second set of three possibilities is whether the point whose
c           coordinate that we are taking the derivative w/r to is the same
c           as the real point whose projections are being considered
c           (kpt.eq.jpt), and whether the former point is or is not projected
c           in the view being considered.
c           
          do kpt=1,nrealpt-1
            kz=3*kpt
            ky=kz-1
            kx=ky-1
c             
            if(kpt.eq.jpt)then
              coefx(kx)=a(iv)+xpxrlas
              coefx(ky)=b(iv)+xpyrlas
              coefx(kz)=c(iv)+xpzrlas
              coefy(kx)=d(iv)+ypxrlas
              coefy(ky)=e(iv)+ypyrlas
              coefy(kz)=f(iv)+ypzrlas
            elseif(realinview(kpt+ivbase))then
              coefx(kx)=xpxrlas
              coefx(ky)=xpyrlas
              coefx(kz)=xpzrlas
              coefy(kx)=ypxrlas
              coefy(ky)=ypyrlas
              coefy(kz)=ypzrlas
            else
              coefx(kx)=aon(iv)+xpxrlas
              coefx(ky)=bon(iv)+xpyrlas
              coefx(kz)=con(iv)+xpzrlas
              coefy(kx)=don(iv)+ypxrlas
              coefy(ky)=eon(iv)+ypyrlas
              coefy(kz)=fon(iv)+ypzrlas
            endif
c             
          enddo
c           
c           The coefficients directly yield derivatives
c           
          do ivar=1,nvmat
            grad(ivar+icoordbas)=grad(ivar+icoordbas)
     &          +2.*xresid(i)*coefx(ivar)+2.*yresid(i)*coefy(ivar)
          enddo
c           
        enddo
      enddo
c       
c       write(*,'(i4,2f16.10)')(i,var(i),grad(i),i=1,nvarsrch)
      return
      end


c       GRADIENTSUM forms the standard gradient sum over the points 
c       in a view for the given factors AFAC - FFAC
c       INDVPROJ has indices from point in view to residual products in
c       RESPROD, IVBASE is the base point number for the view,
c       NPTINVIEW is the number of points in the VIEW
c       
      real*8 function gradientSum(indvproj, ivbase, nptinview, resprod,
     &    afac, bfac, cfac, dfac, efac, ffac)
      implicit none
      integer*4 indvproj(*), ivbase, nptinview,iptinv,ipt
      real*4 resprod(6,*), afac, bfac, cfac, dfac, efac, ffac
      real*8 gradsum
      gradsum = 0.
      do iptinv=1,nptinview
        ipt=indvproj(ivbase+iptinv)
        gradsum = gradsum + afac * resprod(1,ipt) + bfac * resprod(2,ipt)
     &      + cfac * resprod(3,ipt) + dfac * resprod(4,ipt) +
     &      efac * resprod(5,ipt) + ffac * resprod(6,ipt)
      enddo
      gradientSum = gradsum
      return
      end


c***    REMAP_PARAMS returns the complete set of geometric variables based
c       on the current values of the search parameters.

      subroutine remap_params(varlist)
      implicit none
      include 'alivar.inc'
c       
      real*4 varlist(*)
      real*4 sum,varsave
      integer*4 i
c       
c       4/10/05: eliminated global rotation, it is just like the others now
c       so extra arguments to map_one_var were removed
c       
      call map_one_var(varlist,rot,maprot,frcrot,linrot,fixedrot,
     &    nview,glbrot,incrrot)
c       
      do i=1,nview
        if(maptilt(i).gt.0)then
          if(lintilt(i).gt.0)then
            tilt(i)=frctilt(i)*varlist(maptilt(i))+(1.-frctilt(i))*
     &          varlist(lintilt(i))+tiltinc(i)
          elseif(lintilt(i).eq.-1)then
            tilt(i)=frctilt(i)*varlist(maptilt(i))+(1.-frctilt(i))*
     &          fixedtilt+tiltinc(i)
          elseif(lintilt(i).eq.-2)then
            tilt(i)=frctilt(i)*varlist(maptilt(i))+(1.-frctilt(i))*
     &          fixedtilt2+tiltinc(i)
          else
            tilt(i)=varlist(maptilt(i))+tiltinc(i)
          endif
        endif
      enddo
c       
      call map_one_var(varlist,gmag,mapgmag,frcgmag,lingmag,fixedgmag,
     &    nview,glbgmag,incrgmag)
c       
      call map_one_var(varlist,comp,mapcomp,frccomp,lincomp,fixedcomp,
     &    nview,glbgmag,0)
c       
      call map_one_var(varlist,skew,mapskew,frcskew,linskew,fixedskew,
     &    nview,glbskew,incrskew)
c       
      if(mapdumdmag.gt.mapdmagstart) then
c         
c         if there are any dmag variables, the dummy variable is some factor
c         times the sum of the real variables.  Save that position on
c         varlist, put the value there, and compose all of the view
c         parameters as usual
c         
        sum=0.
        do i=mapdmagstart,mapdumdmag-1
          sum=sum+varlist(i)
        enddo
        varsave=varlist(mapdumdmag)
        varlist(mapdumdmag)=dumdmagfac*sum
      endif
c       
      call map_one_var(varlist,dmag,mapdmag,frcdmag,lindmag,fixeddmag,
     &    nview,glbdmag,incrdmag)
      if(mapdumdmag.gt.mapdmagstart)varlist(mapdumdmag)=varsave
c       
      if(ifanyalf.ne.0)call map_one_var(varlist,alf,mapalf,frcalf,
     &    linalf,fixedalf, nview,glbalf,incralf)
c       
      if (mapProjStretch .gt. 0) then
c        projStretch = varlist(mapProjStretch)
        projSkew = varlist(mapProjStretch)
      endif
      if (mapBeamTilt .gt. 0) beamTilt = varlist(mapBeamTilt)
      return
      end


      
      subroutine map_one_var(varlist,val,map,frc,lin,fixed,nview,glb,
     &    incr)
      implicit none
      real*4 varlist(*),val(*),frc(*),glb(*),fixed
      integer*4 map(*),lin(*),nview,incr
      integer*4 i
      do i=1,nview
        if(map(i).gt.0)then
          if(lin(i).gt.0)then
            val(i) =frc(i) * varlist(map(i))+ (1.-frc(i)) * varlist(lin(i))
          elseif(lin(i).lt.0)then
            val(i)=frc(i)*varlist(map(i))+(1.-frc(i))*fixed
          else
            val(i)=varlist(map(i))
          endif
          if (incr.ne.0) val(i) = val(i) + glb(i)
        endif
      enddo
      return
      end


c       MATRIX_TO_COEF takes the distortion matrix DIST, x-axis tilt matrix
c       XTILT, Y axis tilt matrix YTILT, projection stretch matrix PROJSTR,
c       and rotation matrix ROT, and computes the 6 components of the 2x3
c       product in A, B, C, D, E, F
c       
      subroutine matrix_to_coef(dist, xtilt, beamInv, ytilt, beamMat,
     &    projStr, rot, a, b, c, d, e, f)
      implicit none
      real*4 dist(*), xtilt(*), ytilt(*), rot(*), projStr(*), beamInv(*)
      real*4 beamMat(*), a, b, c, d, e, f
      real*8 tmp(9)
      integer*4 i

      do i = 1, 9
        tmp(i) = dist(i)
      enddo
      call mat_product(tmp, 3, 3, xtilt, 3, 3)
      call mat_product(tmp, 3, 3, beamInv, 3, 3)
      call mat_product(tmp, 3, 3, ytilt, 3, 3)
      call mat_product(tmp, 3, 3, beamMat, 2, 3)
      call mat_product(tmp, 2, 3, rot, 2, 2)
      call mat_product(tmp, 2, 3, projStr, 2, 2)
      a = tmp(1)
      b = tmp(2)
      c = tmp(3)
      d = tmp(4)
      e = tmp(5)
      f = tmp(6)
      return
      end


c       $Log$
c       Revision 3.6  2007/05/04 00:02:54  mast
c       Switched from projection stretch to skew between estimated scope axes
c
c       Revision 3.5  2007/02/19 21:07:52  mast
c       Added beam tilt and changed matrix sizes to accommodate
c
c       Revision 3.4  2005/04/10 18:02:50  mast
c       Eliminated global rotation variable due to ineffectiveness in finding
c       angle for some small tilt range situations with mappings
c       
c       Revision 3.3  2004/10/24 22:28:52  mast
c       Changed handling of rotation variables, added projection stretch,
c       made subroutines for filling matrices
c       
c       Revision 3.2  2004/10/09 23:51:12  mast
c       Switched to matrix formulation to simplify code, speeded up
c       computation 2-fold by saving intermediate residual products for
c       gradients, added declarations.
c       
c       Revision 3.1  2002/07/28 22:39:19  mast
c       Standardized error exit and output
c       
