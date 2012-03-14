c       SDINTSCAN compares images in two arrays ARRAY and BRRAY (dimensioned
c       NX,NY, with image dimensions NX by NY), scanning over a matrix of
c       integral displacements between the two images and finding the
c       displacement with the minimum standard deviation of the pixel-by-
c       pixel difference.  I[XY]BOX[01] defines the array index limits of
c       the box to be compared in ARRAY.  ID[XY][01] define the lower and
c       upper limits of the X and Y displacements from this position in
c       BRRAY.  The routine returns the minimum standard deviation SDMIN,
c       the displacement IDXMIN, IDYMIN at which this minimum occurs, and the
c       mean image difference (B minus A) at that position, DDENMIN.
c       
      subroutine sdintscan(array,brray,nx,ny,ixbox0,iybox0,ixbox1,
     &    iybox1 ,idx0,idy0,idx1,idy1,sdmin,ddenmin,idxmin,idymin)
c       
      real*4 array(nx,ny),brray(nx,ny)
c       
      sdmin=1.e10
c       sdmax=0.
      do idy=idy0,idy1
        do idx=idx0,idx1
          call sdcalc(array,brray,nx,ny,ixbox0,iybox0,ixbox1,
     &        iybox1 ,float(idx),float(idy),sd,dden)
          if(sd.lt.sdmin)then
            sdmin=sd
            idxmin=idx
            idymin=idy
            ddenmin=dden
c             write(*,101)idx,idy,sd,' *'
c101         format(2i4,f10.4,a)
          else
c             write(*,101)idx,idy,sd,' '
          endif
c           sdmax=max(sdmax,sd)
        enddo
      enddo
c       ratio=1.0
c       if(sdmax.gt.0.)ratio=sdmin/sdmax
c       write(*,'(f7.4)')ratio
      return
      end

      

c       BIGSEARCH compares images in two arrays ARRAY and BRRAY (dimensioned
c       NX,NY, with image dimensions NX by NY), and finds the 
c       displacements between the two images
c       with the minimum standard deviation of the pixel-by-
c       pixel difference.  I[XY]BOX[01] defines the array index limits of
c       the box to be compared in ARRAY.  DXMIN and DYMIN, upon entering,
c       should have the starting displacement between the box position in
c       ARRAY and the position to be compared in BRRAY.  Initially it will
c       search with a step size of 1, up to a distance of LIMSTEP from the
c       initial displacements.  It will do NITER iterations, cutting the step
c       size by a factor of 2 on each iteration.  D[XY]MIN will be returned
c       with the displacement that has the minimum standard deviation SDMIN,
c       and DDENMIN is the difference in density at that displacement (B-A).
c       
      subroutine bigsearch(array,brray,nx,ny,ixbox0,iybox0,ixbox1
     &    ,iybox1,dxmin,dymin,sdmin,ddenmin,niter,limstep)
c       
      parameter (ichklim=100)
      real*4 array(nx,ny),brray(nx,ny)
      logical*1 checked(-ichklim:ichklim,-ichklim:ichklim)
      logical xchanged,ychanged,keepon
c       
      dxcen=dxmin                               !all moves relative to center
      dycen=dymin                               !at the initial position
      call sdcalc(array,brray,nx,ny,ixbox0,iybox0,ixbox1,
     &    iybox1 ,dxmin,dymin,sdmin,ddenmin)
      ndxy=2**(niter-1)                         !initial # of steps to move by
      dxy=1./ndxy                               !final true step size
      ndlim=ichklim
      ndlim=min(ndlim,ndxy*limstep)             !limiting # of steps
c       
c       set grid to unchecked state except at center
      do i=-ndlim,ndlim
        do j=-ndlim,ndlim
          checked(j,i)=.false.
        enddo
      enddo
      checked(0,0)=.true.
c       
c       
      idxmin=0
      idymin=0
c       write(*,101)idxmin,idymin,sdmin
c101   format(2i5,f10.4)
      
      do iter=1,niter
        xchanged=.true.
        ychanged=.true.
c         keep doing the x-y-diagonal series as long as something changes
        do while(xchanged.or.ychanged)
c           move in x until reach minimum
c           first try a positive step
          xchanged=.false.
          idir=1
          newidx=idxmin+idir*ndxy
          if(abs(newidx).le.ndlim.and..not.checked(newidx,idymin))
     &        then
            call sdcalc(array,brray,nx,ny,ixbox0,iybox0,ixbox1,
     &          iybox1 ,dxcen+newidx*dxy,dycen+idymin*dxy,sd,dden)
            checked(newidx,idymin)=.true.       !mark as checked
c             write(*,101)newidx,idymin,sd
            if(sd.lt.sdmin)then
              sdmin=sd
              ddenmin=dden
              xchanged=.true.
              idxmin=newidx
            else                                !if positive step does no good
              idir=-1                           !switch direction
            endif
          else                                  !or if positive has been
            idir=-1                             !checked or is too far, switch
          endif
c           no, in whichever direction is selected, keep on moving until
c           get to a higher value or edge or area
          keepon=.true.
          do while(keepon)
            newidx=idxmin+idir*ndxy
            if(abs(newidx).le.ndlim.and..not.checked(newidx,idymin))
     &          then
              call sdcalc(array,brray,nx,ny,ixbox0,iybox0,ixbox1,
     &            iybox1 ,dxcen+newidx*dxy,dycen+idymin*dxy,sd,dden)
              checked(newidx,idymin)=.true.
c               write(*,101)newidx,idymin,sd
              if(sd.lt.sdmin)then
                sdmin=sd
                ddenmin=dden
                xchanged=.true.
                idxmin=newidx
              else                              !if value is higher, stop going
                keepon=.false.
              endif
            else                                !or if already checked, or too
              keepon=.false.                    !far, stop going
            endif
          enddo
c           
c           follow exact same procedure in y direction
          ychanged=.false.
          idir=1
          newidy=idymin+idir*ndxy
          if(abs(newidy).le.ndlim.and..not.checked(idxmin,newidy))then
            call sdcalc(array,brray,nx,ny,ixbox0,iybox0,ixbox1,
     &          iybox1 ,dxcen+idxmin*dxy,dycen+newidy*dxy,sd,dden)
            checked(idxmin,newidy)=.true.
c             write(*,101)idxmin,newidy,sd
            if(sd.lt.sdmin)then
              sdmin=sd
              ddenmin=dden
              ychanged=.true.
              idymin=newidy
            else
              idir=-1
            endif
          else
            idir=-1
          endif
          keepon=.true.
          do while(keepon)
            newidy=idymin+idir*ndxy
            if(abs(newidy).le.ndlim.and..not.checked(idxmin,newidy))
     &          then
              call sdcalc(array,brray,nx,ny,ixbox0,iybox0,ixbox1,
     &            iybox1 ,dxcen+idxmin*dxy,dycen+newidy*dxy,sd,dden)
              checked(idxmin,newidy)=.true.
c               write(*,101)idxmin,newidy,sd
              if(sd.lt.sdmin)then
                sdmin=sd
                ddenmin=dden
                ychanged=.true.
                idymin=newidy
              else
                keepon=.false.
              endif
            else
              keepon=.false.
            endif
          enddo
c           
c           if no change above, check the 4 corners
          do ixdir=-1,1,2
            do iydir=-1,1,2
c               keep checking corners only if nothing changed yet
              if(.not.(xchanged.or.ychanged))then
                newidx=idxmin+ixdir*ndxy
                newidy=idymin+iydir*ndxy
                if(abs(newidy).le.ndlim.and.abs(newidx).le.ndlim) then
                  if (.not.checked(newidx,newidy))then
                    call sdcalc(array,brray,nx,ny,ixbox0,iybox0,ixbox1,
     &                  iybox1 ,dxcen+newidx*dxy,dycen+newidy*dxy,sd,
     &                  dden)
c                     write(*,101)newidx,newidy,sd
                    checked(newidx,newidy)=.true.
                    if(sd.lt.sdmin)then
                      sdmin=sd
                      ddenmin=dden
                      xchanged=.true.
                      ychanged=.true.
                      idxmin=newidx
                      idymin=newidy
                    endif
                  endif
                endif
              endif
            enddo
          enddo
c           if nothing ever changed in last loop, drop out of loop
        enddo
        ndxy=ndxy/2                             ! and cut the step size
      enddo
      dxmin=dxcen+idxmin*dxy
      dymin=dycen+idymin*dxy
      return
      end
