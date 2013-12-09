c       FINDSECTION  analyes groups of slices from a tomogram, detects
c       the boundaries of the section, and recommends how much to change
c       tilt angles to make the section flat, how much to shift the tilt
c       axis in Z to produce centered slices, and how thick to make the
c       slices.  It can also recommend how much X-axis tilt is needed to
c       make the section flat in the orthogonal direction as well.
c       
c       The program detects the section boundary at a certain position in
c       the tomogram by measuring the standard deviation of image intensity
c       at each value of Y in the tomogram slice.  It measures S.D. in a
c       "patch" with a width in the X direction and extent in the Z
c       direction specified by the user.  To find the bottom edge of the
c       section, it takes the mean S.D. over several Y values at the bottom
c       of the slice and over Y values just below the middle of the slice.
c       It finds the point where S.D. rises above halfway between these mean
c       values and fits a line to the nearby S.D. values.  The Y values where
c       this line crosses the halfway point and the mean at the bottom of the
c       slice are estimates of the position of the middle and the end of the
c       edge, respectively.  The mid-positions are fairly accurate and are
c       used to determine the midpoint of the section in Y; the end positions
c       are less accurate and are used just to obtain a more conservative
c       measure of section thickness.  The same procedure is used on the top
c       of the section.
c       
c       This analysis is performed at a series of patch positions across
c       the width of the tomogram.  It can be done over a set of files (each
c       being a portion of the same tomogram), with the same patch positions
c       being analyzed in each.  After analyzing a set of patches for one
c       file, the program determines what rotation is required to make the
c       section be flat.  It reports the upward shift needed to center
c       the section in Y, and the slice thickness needed to contain either
c       edge middles or edge ends.  These values are derived and reported 
c       before and after the recommended rotation is applied.  Thickness
c       values are rounded up to integers suitable for taking 3D FFTs.
c       After all files are analyzed, the program makes the same analysis and
c       report based on the data from all of the files.  It then computes an
c       X-axis tilt and reports thickness and rotation if that tilt is taken
c       into account as well.
c       
c       Entries to the program:
c       
c       Width of patch in X, number of patches to analyze across the width
c       .  of the tomogram
c       
c       Number of points to average to get mean S.D. at the edge of the
c       .  slice, number to average to get mean S.D. near the middle of the
c       .  slice, and number of points to fit the line to.  Note that the
c       .  within-section average is taken on one side of the middle, not
c       .  across the center of the slice, so the number of points should be
c       .  constrained accordingly.  Defaults are 3, 10, and 4.
c       
c       Spacing between tomogram samples (the distance in Y in the tilt 
c       .  images.)  If a non-zero number is entered, the program will
c       .  compute the tilt around the X-axis that will make the tomogram be
c       .  flat in its Z dimension.
c       
c       Number of files to analyze
c       
c       For each file, first enter the name of the file.
c       Then enter the starting and ending slices to analyze (/ for all in
c       .  the file)
c       Then enter a list of patches to drop from the analysis for that file,
c       .  or Return to retain all patches.
c       
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       Revision 3.1  2002/06/25 15:25:00  mast
c       Adjusted sample coordinates to be centered around zero to correct
c       problem with computation of shift with X-axis tilt.
c       
c       
c       David Mastronarde, November 1995
c       2/18/99: added X axis tilt output
c       
      parameter (nxdim=10000,nydim=500,limpos=50,limthk=500,limtot=500)
      COMMON //NX,NY,NZ
C       
      DIMENSION NXYZ(3),MXYZ(3),ARRAY(nxdim,nydim)
C       
      CHARACTER*80 FILIN
C       
      EQUIVALENCE (NX,NXYZ)
      real*4 xsum(limthk,limpos),xsqsum(limthk,limpos),
     &    sdp(limthk),xcen(limtot),ycen(limtot),thkmid(limtot),
     &    thkedge(limtot),xx(500),yy(500),ysamp(limtot)
      integer*4 ifuse(limtot),idrop(limtot)
c       
      write(*,'(1x,a,$)')'Width of patch, number of patches across: '
      read(5,*)nxpatch,npatch
      if(npatch.gt.limpos)stop 'TOO MANY PATCHES FOR ARRAYS'
      navedge=3
      navmid=10
      nfit=4
      write(*,'(1x,a,/,a,3i3,a,$)')'Number of points to average at'//
     &    ' edges, number to average in middle,','and number to fit'//
     &    ' line to (/ for',navedge,navmid,nfit,'):: '
      read(5,*)navedge,navmid,nfit
c       
      write(*,'(1x,a,$)')'For analysis of X-axis tilt, enter '//
     &    'distance between sample tomograms: '
      read(5,*)deltay
      if (deltay.gt.0.) write(*,'(/,a,/)')' TOMOGRAMS MUST BE '//
     &    'ENTERED IN ORDER OF INCREASING SAMPLE COORDINATE'
      if (deltay.lt.0.) write(*,'(/,a,/)')' TOMOGRAMS MUST BE '//
     &    'ENTERED IN ORDER OF DECREASING SAMPLE COORDINATE'
c       
      write(*,'(1x,a,$)')'Number of tomogram files: '
      read(5,*)nfiles
      if(nfiles*npatch.gt.limtot)stop
     &    'TOO MANY TOTAL PATCHES FOR ARRAYS'
      call ialprt(.false.)
      ipbase=1
      ysample = -deltay * (nfiles-1.)/2.
      do ifile=1,nfiles
        write(*,'(1x,a,i2,a,$)')'Name of tomogram file #',ifile,': '
        read(5,'(a)')FILIN
        call imopen(1,filin,'ro')
        call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean)
        print *,'NX, NY, NZ:',nx,ny,nz
        if(nx.gt.nxdim.or.ny.gt.nydim.or.ny.gt.limthk) stop
     &      'IMAGES TOO LARGE FOR ARRAYS'
        izst=0
        iznd=nz-1
        write(*,'(1x,a,2i4,a,$)')
     &      'Starting and ending section #s (/ for',izst,iznd,'): '
        read(5,*)izst,iznd
        izst=max(0,izst)
        iznd=min(nz-1,iznd)
c         
        idx=nint(float(nx)/npatch)
        idxmax=(nx-nxpatch)/max(1,(npatch-1))
        idx=min(idx,idxmax)
        do i=1,npatch
          do j=1,ny
            xsum(j,i)=0.
            xsqsum(j,i)=0.
          enddo
        enddo
        do iz=izst,iznd
          call imposn(1,iz,0)
          call irdpas(1,array,nxdim,nydim,0,nx-1,0,ny-1,*99)
          do iy=1,ny
            do ipatch=1,npatch
              ixst=(nx-nxpatch)/2+(ipatch-0.5*(npatch+1.))*idx
              do ix=ixst+1,ixst+nxpatch
                den=array(ix,iy)
                xsum(iy,ipatch)=xsum(iy,ipatch)+den
                xsqsum(iy,ipatch)=xsqsum(iy,ipatch)+den**2
              enddo
            enddo
          enddo
        enddo
        ninpat=(iznd+1-izst)*nxpatch
        write(6,*)
        write(6,*)
        do ipatch=1,npatch
          do iy=1,ny
            call sums_to_avgsd(xsum(iy,ipatch),xsqsum(iy,ipatch),
     &          ninpat,avg,sdp(iy),sem)
          enddo
          xct=(ipatch-0.5*(npatch+1.))*idx
          midst=max(1,ny/2+1-navmid)
          limy=1
          limst=1
          do idir=-1,1,2
            botedge=topedge
            botmid=topmid
            call avgsd(sdp(midst),navmid,avgmid,sdmid,sem)
            call avgsd(sdp(limst),navedge,avgedge,sdedge,sem)
            crit=0.5*(avgedge+avgmid)
            iypast=limy-idir*(nfit/2)
            do while(sdp(iypast).lt.crit)
              iypast=iypast-idir
            enddo
            linst=iypast+idir*(nfit/2)
            linnd=linst-idir*(nfit-1)
            nd=0
            do iy=linnd,linst,idir
              nd=nd+1
              xx(nd)=iy
              yy(nd)=sdp(iy)
            enddo
            call lsfit(xx,yy,nd,slop,bint,ro)
            topmid=(crit-bint)/slop
            topedge=(avgedge-bint)/slop
            topedge=min(float(ny),max(1.,topedge))
c             
c             prepare for next direction
c             
            midst=min(ny/2+1,ny+1-navmid)
            limy=ny
            limst=ny+1-navedge
          enddo
          yct=0.5*(topmid+botmid)-0.5*(ny+1)
          thkmidt=topmid-botmid
          thkedgt=topedge-botedge
          write(6,102)ipatch,xct,yct,thkmidt,thkedgt
102       format(i3,'  X =',f7.1,'  Y =',f7.1,' thickness',f7.1,
     &        ' at mid-edge,',f7.1,' at edge end')
          xcen(ipbase+ipatch-1)=xct
          ycen(ipbase+ipatch-1)=yct
          thkmid(ipbase+ipatch-1)=thkmidt
          thkedge(ipbase+ipatch-1)=thkedgt
          ifuse(ipbase+ipatch-1)=1
          ysamp(ipbase+ipatch-1)=ysample
        enddo
        write(*,'(1x,a,$)')'List of #s of patches to drop from'//
     &      ' analysis (Return for none): '
        call rdlist(5,idrop,ndrop)
        do i=1,ndrop
          if(idrop(i).ge.0.and.idrop(i).le.npatch)
     &        ifuse(ipbase+idrop(i)-1)=0
        enddo
        call analyzespots(trim(filin),xcen(ipbase),
     &      ycen(ipbase), thkmid(ipbase),thkedge(ipbase),
     &      ifuse(ipbase),npatch, ysamp(ipbase),0.)
        ipbase=ipbase+npatch
        ysample=ysample+deltay
      enddo
c       
c       DNM 6/25/02: need to make the ysamp values symmetric around zero
c       i.e., assume that the samples are symmetric in data set
c       
      ymiddle = deltay * (nfiles - 1) / 2
      do i = 1, ipbase - 1
        ysamp(i) = ysamp(i) - ymiddle
      enddo
      call analyzespots('all files',xcen,ycen,
     &    thkmid,thkedge,ifuse,npatch*nfiles, ysamp, deltay)
      call exit(0)
99    stop 'read error'
      end


      subroutine analyzespots(fillab,xcen,ycen,
     &    thkmid,thkedge,ifuse,nspots,ysamp,doxtilt)
      character*(*) fillab
      real*4 xcen(*),ycen(*),thkmid(*),thkedge(*),ysamp(*)
      real *4 xx(1000),yy(1000),zz(1000)
      integer*4 ifuse(*)
      write (6,101)fillab
101   format(//,' Analysis of positions from ',a,':')
      call findshift('unrotated','middle',ycen,thkmid,ifuse,nspots)
      call findshift('unrotated',' edge ',ycen,thkedge,ifuse,nspots)
      nd=0
      do i=1,nspots
        if(ifuse(i).ne.0)then
          nd=nd+1
          xx(nd)=xcen(i)
          yy(nd)=ycen(i)
        endif
      enddo
      call lsfit(xx,yy,nd,slop,bint,ro)
      ang=atand(slop)
      do i=1,nspots
        yy(i)=xcen(i)*sind(-ang)+ycen(i)*cosd(-ang)
      enddo
      write(6,102)slop,-ang,ang
102   format(' slope =',f8.4,': to rotate by',f6.1,', add',f6.1,
     &    ' to all angles')
      call findshift(' rotated ','middle',yy,thkmid,ifuse,nspots)
      call findshift(' rotated ',' edge ',yy,thkedge,ifuse,nspots)
      if(doxtilt.eq.0.)return
      nd=0
      do i=1,nspots
        if(ifuse(i).ne.0)then
          nd=nd+1
          xx(nd)=xcen(i)
          zz(nd)=ycen(i)
          yy(nd)=ysamp(i)
        endif
      enddo
      call lsfit2(xx,yy,zz,nd,a,b,c)
      alpha=atand(b)
      slop=a/(cosd(alpha)-b*sind(alpha))
      theta=-atand(slop)
      costh=cosd(theta)
      sinth=sind(theta)
      cosal=cosd(alpha)
      sinal=sind(alpha)
      do i=1,nspots
        zp=ycen(i)*cosal-ysamp(i)*sinal
        yy(i)=xcen(i)*sinth+zp*costh
      enddo
      write(6,103)alpha,theta,-theta
103   format(/' The pitch between samples can be corrected with ',
     &    'an X-axis tilt of',f7.2,/,' In this case, rotate by',f6.1,
     &    ', i.e., add',f6.1,' to all angles')
      call findshift('x-tilted ','middle',yy,thkmid,ifuse,nspots)
      call findshift('x-tilted ',' edge ',yy,thkedge,ifuse,nspots)
      return
      end

      subroutine findshift(rotlab,midlab,ycen,thick,ifuse,nspots)
      character*(*) rotlab,midlab
      real*4 ycen(*),thick(*)
      integer*4 ifuse(*)
      bot=1.e10
      top=-1.e10
      do i=1,nspots
        if(ifuse(i).ne.0)then
          bot=min(bot,ycen(i)-thick(i)/2)
          top=max(top,ycen(i)+thick(i)/2)
        endif
      enddo
      realthk=top-bot
      shift=-0.5*(top+bot)
      ithick=2*(int(realthk/2.+0.99))
      ithick=niceframe(ithick,2,19)
      write(6,101)rotlab,midlab,shift,realthk,ithick
101   format(1x,a,', from edge ',a,', shift up',f7.1,', thickness',
     &    f6.1,', set to',i5)
      return
      end
