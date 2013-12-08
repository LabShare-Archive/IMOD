****    EDGEERASER.FOR
c       
c       This program erases bad portions of an image near its edges.  It is
c       designed to eliminate the sharp borders created when an image
c       digitized from film does not fill the entire frame.  The image is
c       tapered down to the mean intensity gradually to eliminate the strong
c       artifacts that would otherwise occur in a tomographic reconstruction.
c       
c       To use the program, first prepare an IMOD model file to indicate
c       which edges need to be erased.  Each edge should be specified with a
c       separate contour; do not go around a corner with a single contour to
c       specify two edges.  Draw the contour just inside the edge of the bad
c       region on the image.  The program will erase along that entire side
c       of the image, so you should generally draw a contour along the whole
c       length of a side.  If you do not, the program will extrapolate from
c       the endpoints of a contour.
c       
c       The inputs are:
c       
c       Input image file
c       
c       Output image file, or <Return> to place modified sections back into
c       the input file.  USE REPLACEMENT OPTION WITH CAUTION
c       
c       Model file
c       
c       Distance over which the image should be tapered down to the mean
c       intensity, or / to accept the default.  This taper will occur inside
c       of the drawn contour, so that the mean intensity is reached at the
c       position of the contour.
c       
c       David Mastronarde 12/4/98
c       
c       
      parameter (imsiz=4200)
      parameter (mxd=50)
      real*4 array(imsiz*imsiz),title(20),orig(3),delt(3)
      integer*4 nxyz(3),mxyz(3)
      equivalence (nx,nxyz(1)),(ny,nxyz(2)),(nz,nxyz(3))
      character*80 infile,outfile,ptfile
      real*4 xfix(5000),yfix(5000)
      include 'model.inc'
      logical readw_or_imod, typeonlist
      common /bigarr/array
c       
c       7/7/00 CER: remove the encode's; titlech is the temp space
c       
      character*80 titlech
c       
      write(*,'(1x,a,$)')'Input image file: '
      read(*,'(a)')infile
      call imopen(1,infile,'old')
      call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean)
      if(nx*ny.gt.imsiz**2)stop 'IMAGE TOO LARGE'
      call irtdel(1,delt)
      call irtorg(1,orig(1),orig(2),orig(3))
c       
      print *,'Enter output file name (Return to put modified'//
     &    ' sections back in input file)'
      read(*,'(a)')outfile
c       
      imfilout=1
      if(outfile.ne.' ')then
        imfilout=2
        call imopen(2,outfile,'new')
        call itrhdr(2,1)
      endif
c       
75    write(*,'(1x,a,$)')'Model file: '
      read(*,'(a)')ptfile
      if(.not.readw_or_imod(ptfile))then
        print *,'Bad model file, try again'
        go to 75
      endif

      do i=1,n_point
        do j=1,3
          p_coord(j,i)=(p_coord(j,i)+orig(j))/delt(j)
        enddo
      enddo
c       
      ntaper=10
      write(*,'(1x,a,i2,a,$)')'Distance over which to taper image '
     &    //'(/ for',ntaper,'): '
      read(5,*)ntaper
c       
c       start looping on sections; need to read regardless
c       
      do izsect=0,nz-1
        call irdsec(1,array,*99)
        nfix=0
c         
c         scan through points to see if any need fixing
c         
        do iobj=1,max_mod_obj
          if(npt_in_obj(iobj).gt.0)then
            ibase=ibase_obj(iobj)
            if(nint(p_coord(3,object(ibase+1))).eq.izsect)then
              nfix=1
              ninobj=npt_in_obj(iobj)
              xs=p_coord(1,object(ibase+1))
              xn=p_coord(1,object(ibase+ninobj))
              ys=p_coord(2,object(ibase+1))
              yn=p_coord(2,object(ibase+ninobj))
              dx=abs(xn-xs)
              dy=abs(yn-ys)
              xmid=(xs+xn)/2.
              ymid=(ys+yn)/2.
              if(dx.gt.dy.and.ymid.lt.ny/2)then
c                 
c                 case 1: wipe out below the contour
c                 
                do ip=1,ninobj
                  ipt=object(ibase+ip)
                  xfix(ip)=p_coord(1,ipt)+1.
                  yfix(ip)=p_coord(2,ipt)+1.
                enddo
                call sortfix(xfix,yfix,ninobj)
                sum=0.
                do ix=1,nx
                  iys=interpline(xfix,yfix,ninobj,ix)
                  colsum=0.
                  do iy=iys,ny
                    colsum=colsum+array(ix+(iy-1)*nx)
                  enddo
                  sum=sum+colsum/(ny+1-iys)
                enddo
                denmean=sum/nx
                do ix=1,nx
                  iys=interpline(xfix,yfix,ninobj,ix)
                  iytap=iys+ntaper
                  do iy=1,iys
                    array(ix+(iy-1)*nx)=denmean
                  enddo
                  do iy=iys+1,iytap
                    f=float(iy-iys)/(iytap+1-iys)
                    array(ix+(iy-1)*nx)=(1.-f)*denmean+
     &                  f*array(ix+(iy-1)*nx)
                  enddo
                enddo
              elseif(dx.gt.dy.and.ymid.gt.ny/2)then
c                 
c                 case 2: wipe out above
c                 
                do ip=1,ninobj
                  ipt=object(ibase+ip)
                  xfix(ip)=p_coord(1,ipt)+1.
                  yfix(ip)=p_coord(2,ipt)+1.
                enddo
                call sortfix(xfix,yfix,ninobj)
                sum=0.
                do ix=1,nx
                  iys=interpline(xfix,yfix,ninobj,ix)
                  colsum=0.
                  do iy=1,iys
                    colsum=colsum+array(ix+(iy-1)*nx)
                  enddo
                  sum=sum+colsum/iys
                enddo
                denmean=sum/nx
                do ix=1,nx
                  iys=interpline(xfix,yfix,ninobj,ix)
                  iytap=iys-ntaper
                  do iy=iys,ny
                    array(ix+(iy-1)*nx)=denmean
                  enddo
                  do iy=iytap,iys-1
                    f=float(iys-iy)/(iys+1-iytap)
                    array(ix+(iy-1)*nx)=(1.-f)*denmean+
     &                  f*array(ix+(iy-1)*nx)
                  enddo
                enddo
              elseif(xmid.lt.nx/2)then
c                 
c                 case 3: wipe out to left of the contour
c                 
                do ip=1,ninobj
                  ipt=object(ibase+ip)
                  xfix(ip)=p_coord(1,ipt)+1.
                  yfix(ip)=p_coord(2,ipt)+1.
                enddo
                call sortfix(yfix,xfix,ninobj)
                sum=0.
                do iy=1,ny
                  ixs=interpline(yfix,xfix,ninobj,iy)
                  colsum=0.
                  do ix=ixs,nx
                    colsum=colsum+array(ix+(iy-1)*nx)
                  enddo
                  sum=sum+colsum/(nx+1-ixs)
                enddo
                denmean=sum/ny
                do iy=1,ny
                  ixs=interpline(yfix,xfix,ninobj,iy)
                  ixtap=ixs+ntaper
                  do ix=1,ixs
                    array(ix+(iy-1)*nx)=denmean
                  enddo
                  do ix=ixs+1,ixtap
                    f=float(ix-ixs)/(ixtap+1-ixs)
                    array(ix+(iy-1)*nx)=(1.-f)*denmean+
     &                  f*array(ix+(iy-1)*nx)
                  enddo
                enddo
              else
c                 
c                 case 4: wipe out to right
c                 
                do ip=1,ninobj
                  ipt=object(ibase+ip)
                  xfix(ip)=p_coord(1,ipt)+1.-yextra
                  yfix(ip)=p_coord(2,ipt)+1.
                enddo
                call sortfix(yfix,xfix,ninobj)
                sum=0.
                do iy=1,ny
                  ixs=interpline(yfix,xfix,ninobj,iy)
                  colsum=0.
                  do ix=1,ixs
                    colsum=colsum+array(ix+(iy-1)*nx)
                  enddo
                  sum=sum+colsum/ixs
                enddo
                denmean=sum/ny
                do iy=1,ny
                  ixs=interpline(yfix,xfix,ninobj,iy)
                  ixtap=ixs-ntaper
                  do ix=ixs,nx
                    array(ix+(iy-1)*nx)=denmean
                  enddo
                  do ix=ixtap,ixs-1
                    f=float(ixs-ix)/(ixs+1-ixtap)
                    array(ix+(iy-1)*nx)=(1.-f)*denmean+
     &                  f*array(ix+(iy-1)*nx)
                  enddo
                enddo
              endif
            endif
          endif
        enddo
c         
        call iclden(array,nx,ny,1,nx,1,ny,dmint,dmaxt,dmeant)
        tmin=min(tmin,dmint)
        tmax=max(tmax,dmaxt)
        tsum=tsum+dmeant
c         
c         write out if any changes or if new output file
c         
        if(nfix.gt.0.or. imfilout.eq.2)then
          call imposn(imfilout,izsect,0)
          call iwrsec(imfilout,array)
        endif
      enddo
c       
      tmean=tsum/nz
c       
c       7/7/00 CER: remove the encodes
c       
c       encode(80,109,title)
      write(titlech,109)   
      read(titlech,'(20a4)')(title(kti),kti=1,20)
109   format('EDGEERASER: bad edge replaced with taper to mean')
      call iwrhdr(imfilout,title,1,tmin,tmax,tmean)
      call imclose(imfilout)
      call exit(0)
99    stop 'error'
      end


      function interpline(xfix,yfix,ninobj,ix)
      real*4 xfix(*),yfix(*)
      if(ix.lt.xfix(1))then
        interpline=yfix(1)+0.9
        return
      endif
      do i=2,ninobj
        if(ix.lt.xfix(i))then
          interpline=yfix(i-1)+(ix-xfix(i-1))*(yfix(i)-yfix(i-1))/
     &        (xfix(i)-xfix(i-1)) + 0.9
          return
        endif
      enddo
      interpline=yfix(ninobj)+0.9
      return
      end

      subroutine sortfix(x,y,n)
      real*4 x(*), y(*)
      if(x(1).gt.x(n))then
        do i=1,n/2
          ni=n+1-i
          tmp=x(i)
          x(i)=x(ni)
          x(ni)=tmp
          tmp=y(i)
          y(i)=y(ni)
          y(ni)=tmp
        enddo
      endif
c       
      do i=1,n-1
        do j=i+1,n
          if(x(i).gt.x(j))then
            tmp=x(i)
            x(i)=x(j)
            x(j)=tmp
c             
c             this is not perfect - it's better NOT to swap the Y vales
c             tmp=y(i)
c             y(i)=y(j)
c             y(j)=tmp
          endif
        enddo
      enddo
      return
      end
