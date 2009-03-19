c       TOMOPITCH  analyes simple models of the boundaries of the section
c       in slices from a tomogram and recommends how much to change
c       tilt angles to make the section flat, how much to shift the tilt
c       axis in Z to produce centered slices, and how thick to make the
c       slices.  It can also recommend how much X-axis tilt is needed to
c       make the section flat in the orthogonal direction as well.  It can
c       also be used with a model drawn on a whole tomogram, possibly binned 
c       down.
c       
c       See manpage for more details.
c       
c       David Mastronarde, January 2000
c       5/20/01: Added analysis of single file with multiple time indexes
c       
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       Log at end
c       
      implicit none
      include 'smallmodel.inc'
      integer limtot
      parameter (limtot=500)
C       
      CHARACTER*120 FILIN
C	
      logical readSmallMod,usetimes
      integer getimodhead,getimodmaxes,getimodtimes,getimodscales
      real*4 xcen(limtot),ycen(limtot),thkmid(limtot),ysamp(limtot)
      integer*4 ifuse(limtot),itimes(max_obj_num)
      integer*4 iobjVert(2), iobjHoriz(limtot), indHoriz(limtot)
      real*4 yVert(2), zSpanVert(2), zmean(limtot), ymean(limtot)
      character*120 message
c       
      integer*4 npatch,nfiles,ifile,ipbase,ierr,ifflip,i,iobj,j,indy,indz
      integer*4 maxx,maxy,maxz,mintime,maxtime,numobj,iobj1,iobj2
      real*4 border,deltay,ysample,xyscal,zscal,xofs,yofs,zofs
      real*4 ximscale,yimscale,zimscale,ymiddle, scaleFac
      real*4 yval, zval, zspan, diffMin, xend, yend, zend, yline, zline
      real*4 alphaAdd, thetaAdd, shiftAdd, alphaOld, thetaOld, shiftOld
      integer*4 ifNoAlpha, ifNoTheta, ifNoShift
      integer*4 nVertical, nHoriz, ip1, ip2, imodobj, imodcont
      integer*4 lnblnk
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg, PipNumberOfEntries
      integer*4 PipGetString,PipGetFloat
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  tomopitch
c       
      integer numOptions
      parameter (numOptions = 9)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'model:InputFile:FNM:@extra:ExtraThickness:F:@'//
     &    'spacing:SpacingInY:F:@scale:ScaleFactor:F:@'//
     &    'angle:AngleOffsetOld:F:@zshift:ZShiftOld:F:@'//
     &    'xtilt:XAxisTiltOld:F:@param:ParameterFile:PF:@help:usage:B:'

      npatch=2
      scaleFac = 1.
      deltay = 0.
      border = 2.
      ifNoAlpha = 1
      ifNoTheta = 1
      ifNoShift = 1
c	
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'tomopitch',
     &    'ERROR: TOMOPITCH - ', .true., 1, 0, 0, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0
      if (pipinput) then
        ierr = PipNumberOfEntries('ModelFile', nfiles)
        if (nfiles .eq. 0) call exitError(
     &      'ERROR: TOMOPITCH - NO MODEL FILE ENTERED')
        ierr = PipGetFloat('SpacingInY', deltay)
        ierr = PipGetFloat('ExtraThickness', border)
        ierr = PipGetFloat('ScaleFactor', scaleFac)
        ifNoAlpha = PipGetFloat('XAxisTiltOld', alphaOld)
        ifNoTheta = PipGetFloat('AngleOffsetOld', thetaOld)
        ifNoShift = PipGetFloat('ZShiftOld', shiftOld)
      else
        write(*,'(1x,a,$)')'Additional thickness to add outside model'
     &      //' lines: '
        read(5,*)border
c         
        write(*,'(1x,a,$)')'For analysis of X-axis tilt, enter '//
     &      'distance between sample tomograms: '
        read(5,*)deltay
c         
        write(*,'(1x,a,$)')'Number of model files: '
        read(5,*)nfiles
      endif
      if (deltay.gt.0..and. nfiles.gt.0)
     &    write(*,'(/,a,/)')' MODEL FILES OR TIMES MUST '//
     &    'OCCUR IN ORDER OF INCREASING SAMPLE COORDINATE'
      if (deltay.lt.0. .and. nfiles.gt.0)
     &    write(*,'(/,a,/)')' MODEL FILES OR TIMES MUST '//
     &    'OCCUR IN ORDER OF DECREASING SAMPLE COORDINATE'

      ipbase=1
      ysample = -deltay * (nfiles-1.)/2.
      nVertical = 0
      nHoriz = 0
      indy = 2
      indz = 3

      ifile = 1
      usetimes = .false.

      do while(ifile.le.nfiles)
        if (.not.usetimes .and. nVertical.eq.0 .and. nHoriz.eq.0)then
          if (pipinput) then
            ierr = PipGetString('ModelFile', filin)
          else
            write(*,'(1x,a,i2,a,$)')'Name of model file #',ifile,': '
            read(5,'(a)')FILIN
          endif
          if(.not.readSmallMod(filin))then
            write(*,'(/,a,a)') 'ERROR: TOMOPITCH - READING MODEL FILE ',filin
            call exit(1)
          endif
          ierr=getimodhead(xyscal,zscal,xofs,yofs,zofs,ifflip)
          ierr=getimodmaxes(maxx,maxy,maxz)
          ierr=getimodscales(ximscale,yimscale,zimscale)
c	    print *,xofs, yofs, zofs, ifflip
c	    print *,maxx,maxy,maxz
c           
c           convert to centered index coordinates - maxes are correct
c           
          do i=1,n_point
            p_coord(1,i)=(p_coord(1,i)-xofs)/ximscale-maxx/2
            p_coord(2,i)=(p_coord(2,i)-yofs)/yimscale-maxy/2
            p_coord(3,i)=(p_coord(3,i)-zofs)/zimscale-maxz/2
c	      write(*,'(3f7.1)')(p_coord(j,i), j=1,3)
          enddo
c           
c           If there is one file, see if there are multiple times
c           
          if (nfiles.eq.1) then
            ierr=getimodtimes(itimes)
            mintime=100000
            maxtime=0
            do iobj=1,max_mod_obj
              if (npt_in_obj(iobj).gt.0)then
                mintime=min(mintime,itimes(iobj))
                maxtime=max(maxtime,itimes(iobj))
              endif
            enddo
            if (maxtime .gt. 0) then
              usetimes = .true.
              nfiles = maxtime
c               
c               DNM 11/11/03: adjust ysample here, not below
c               
              ysample = -deltay * (nfiles-1.)/2.
              if (mintime .eq. 0) then
                write(*,'(/,a,/,a)')'ERROR: TOMOPITCH - THE MODEL FILE HAS '//
     &              'MULTIPLE TIMES BUT HAS SOME CONTOURS WITH',
     &              'ERROR: (line 2):    '//
     &              'NO TIME INDEX. THESE CONTOURS SHOULD BE ELIMINATED'
                call exit(1)
              endif
            else
c               
c               now check for whether there are multiple lines in one model
c               2/3/05: DO NOT swap y and Z if y is the long dimension,
c               data are already flipped back so Z is between-slice dimension
c               
              do iobj = 1, max_mod_obj
                call objtocont(iobj, obj_color, imodobj, imodcont)
                if (npt_in_obj(iobj) .gt. 2) then
                  write(*,'(/,a,i5,a,i3,a)') 'ERROR: TOMOPITCH - contour',
     &                imodcont, ' in object', imodobj,' has more than 2 points'
                  call exit(1)
                endif
                if (npt_in_obj(iobj) .gt. 1) then
                  ip1=object(ibase_obj(iobj)+1)
                  ip2=object(ibase_obj(iobj)+npt_in_obj(iobj))
                  yval = 0.5 * (p_coord(indy, ip1) + p_coord(indy, ip2))
                  zval = 0.5 * (p_coord(indz, ip1) + p_coord(indz, ip2))
                  zspan = abs(p_coord(indz, ip1) - p_coord(indz, ip2))
                  if (abs(p_coord(1, ip1) - p_coord(1, ip2)) .lt.
     &                0.2 * zspan)
     &                then
                    nVertical = nVertical + 1
                    if (nVertical .le. 2) then
                      iobjVert(nVertical) = iobj
                      yVert(nVertical) = yval
                      zSpanVert(nVertical) = zspan
                    endif
                  else if (zspan .lt.
     &                  0.2 * abs(p_coord(1, ip1) - p_coord(1, ip2))) then
                    nHoriz = nHoriz + 1
                    if (nHoriz .gt. limtot) call exitError(
     &                  'TOO MANY LINES FOR ARRAYS')
                    iobjHoriz(nHoriz) = iobj
                    zmean(nHoriz) = zval
                    ymean(nHoriz) = yval
                    indHoriz(nHoriz) = nHoriz
                  else
                    write(*, '(a,i5,a,i3,a)') 'ERROR: TOMOPITCH - contour',
     &                  imodcont, ' in object', imodobj,
     &                  ' is too diagonal to analyze'
                    call exit(1)
                  endif
                endif
              enddo
c		
c               Now require 2 horiz & 2 vert, or even number of horiz lines
c               
              if (nVertical .gt. 0 .and. nVertical .ne. 2 .and.
     &            nHoriz .ne. 2) call exitError('TO USE CROSSED LINES'//
     &            ', YOU MUST HAVE 2 HORIZONTAL AND 2 VERTICAL LINES')
              if (mod(nHoriz, 2) .ne. 0) call exitError(
     &            'YOU MUST HAVE AN EVEN NUMBER OF LINES '//
     &            'IN A SINGLE MODEL FILE')
              if (nVertical .gt. 0) then
c                 
c                 for crossed lines, set up for 3 "files" and handle below
c                 adjust horizontal index to  match top & bottom
c                 
                nfiles = 3
                if ((yVert(1).gt.yVert(2) .and. ymean(1).lt.ymean(2)) .or.
     &              (yVert(1).lt.yVert(2) .and. ymean(1).gt.ymean(2))) then
                  indHoriz(1) = 2
                  indHoriz(2) = 1
                endif
                if (abs(zmean(1) - zmean(2)) .gt. 0.3 *
     &              min(zSpanVert(1), zSpanVert(2))) call exitError(
     &              'DISTANCE BETWEEN HORIZONTAL LINES MUST BE LESS '
     &              //'THAN 0.3 TIMES LENGTH OF VERTICAL ONES')

              else if (nHoriz .gt. 2) then
c                 
c                 for multiple horizontal lines, sort them by Z and make sure
c                 they make sense
c                 
                nfiles = nHoriz / 2
                do i = 1, nHoriz - 1
                  do j = i + 1, nHoriz
                    if (zmean(indHoriz(i)) .gt. zmean(indHoriz(j))) then
                      iobj = indHoriz(i)
                      indHoriz(i) = indHoriz(j)
                      indHoriz(j) = iobj
                    endif
                  enddo
                enddo
                
                do i = 1,nfiles
                  j = 2 * i
                  diffMin = 1.e10
                  if (i .gt. 1) diffMin = min(diffMin,
     &                zmean(indHoriz(j - 1)) - zmean(indHoriz(j - 2)))
                  if (i .lt. nfiles) diffMin = min(diffMin,
     &                zmean(indHoriz(j + 1)) - zmean(indHoriz(j)))
                  if (zmean(indHoriz(j)) - zmean(indHoriz(j - 1)) .gt.
     &                0.3 * diffMin)
     &                call exitError('THE SPACING BETWEEN TWO LINES '//
     &                'OF A PAIR MUST BE LESS THAN 0.3 TIMES THE '//
     &                'SPACING BETWEEN PAIRS')
                enddo
              else
c                 
c                 otherwise, need to restore indexes
c                 
                indy = 2
                indz = 3
              endif
            endif
          endif
        endif
        if(nfiles*npatch.gt.limtot)call exitError(
     &      'ERROR: TOMOPITCH - TOO MANY TOTAL PATCHES FOR ARRAYS')
c         
c         now get the first 2 contours in model or time or next pair
c         of horizontal lines, or make up lines in crossed case
c         
        if (nVertical .gt. 0 .and. ifile .gt. 1) then
c           
c           shift the horizontal lines to the endpoints of the vertical ones
c           
          ysample = 0.
          do i = 1, 2
c             
c             get appropriate endpoint of line
c             
            ip1 = object(ibase_obj(iobjVert(i)) + 1)
            ip2 = object(ibase_obj(iobjVert(i)) + npt_in_obj(iobjVert(i)))
            if ((ifile.eq.2 .and. p_coord(indz, ip1).gt.p_coord(indz, ip2))
     &          .or.(ifile.eq.3 .and.
     &          p_coord(indz, ip1).lt.p_coord(indz, ip2))) ip1 = ip2
            xend = p_coord(1,ip1)
            yend = p_coord(indy,ip1)
            zend = p_coord(indz,ip1)
c	      print *,i,iobjVert(i),ip1,ip2,xend,yend,zend
c             
c             solve for y and z coordinate of existing horizontal line at
c             that X value, and shift Y and Z of endpoints to move that
c             point to the desired endpoint
c             
            iobj = iobjHoriz(indHoriz(i))
            ip1 = object(ibase_obj(iobj) + 1)
            ip2 = object(ibase_obj(iobj) + npt_in_obj(iobj))
            yline = p_coord(indy, ip1) + (xend - p_coord(1, ip1)) *
     &          (p_coord(indy, ip2) - p_coord(indy, ip1)) /
     &          (p_coord(1, ip2) - p_coord(1, ip1))
            zline = p_coord(indz, ip1) + (xend - p_coord(1, ip1)) *
     &          (p_coord(indz, ip2) - p_coord(indz, ip1)) /
     &          (p_coord(1, ip2) - p_coord(1, ip1))
            p_coord(indy, ip1) = p_coord(indy, ip1) + yend - yline
            p_coord(indy, ip2) = p_coord(indy, ip2) + yend - yline
            p_coord(indz, ip1) = p_coord(indz, ip1) + zend - zline
            p_coord(indz, ip2) = p_coord(indz, ip2) + zend - zline
            ysample = ysample + 0.5 * zend
          enddo
          iobj1 = iobjHoriz(1)
          iobj2 = iobjHoriz(2)
          numobj = 2
          write(message,'(a,f7.0)')'lines moved to Y =', ysample + maxz / 2.

        else if (nHoriz .gt. 2 .or. nVertical .gt. 0)then
c           
c           get pair of horizontal lines
c           
          numobj = 2
          ip1 = indHoriz(2 * ifile - 1)
          ip2 = indHoriz(2 * ifile)
          iobj1 = iobjHoriz(ip1)
          iobj2 = iobjHoriz(ip2)
          ysample = (zmean(ip1) + zmean(ip2)) / 2.
          write(message,'(a,f7.0)')'line pair at Y =', ysample + maxz / 2.

        else if (usetimes) then
c           
c           get lines at next time
c           
          numobj=0
          do iobj = 1, max_mod_obj
            if (npt_in_obj(iobj) .gt. 0 .and.
     &          itimes(iobj) .eq. ifile .and. numobj .le. 2) then
              numobj = numobj + 1
              if (numobj .eq. 1) iobj1 = iobj
              if (numobj .eq. 2) iobj2 = iobj
            endif
          enddo
          write(message,'(a,i3)')'time index',ifile
        else
c           
c           or just get first two object
c           
          numobj = min(2, max_mod_obj)
          iobj1 = 1
          iobj2 = 2
          message = filin
        endif

        if (numobj.gt.0.or..not.usetimes) then
          if (numobj.lt.2) then
            write(*, '(/,a,i3,a)')'ERROR: TOMOPITCH - MODEL OR TIME',ifile,
     &          ' DOES NOT HAVE ENOUGH CONTOURS'
            call exit(1)
          endif
          if(npt_in_obj(iobj1).lt.2.or.npt_in_obj(iobj2).lt.2)then
            write(*, '(/,a,i3)')'ERROR: TOMOPITCH - THERE ARE NOT TWO POINTS ',
     &          'IN FIRST TWO CONTOURS OF MODEL OR TIME',ifile
            call exit(1)
          endif


          call addlinepair(message, iobj1, iobj2, scaleFac * ysample,
     &        border, scaleFac, indy, maxx, xcen, ycen, thkmid,
     &        ifuse, ysamp, npatch, ipbase)
        endif
        ysample=ysample+deltay
        ifile=ifile+1
      enddo
c       
c       DNM 6/25/02: need to make the ysamp values symmetric around zero
c       i.e., assume that the samples are symmetric in data set
c       11/11/03: remove adjustment, which must have broken the 3-model case
c       
      message='all files'
      if(usetimes)message='all time indexes'
      if (nHoriz.gt.2 .or. nVertical.gt.0) then
        message = 'all line pairs'
        deltay = 1.
      endif
      if(nfiles.eq.1)deltay=0.
      call analyzespots(message(1:lnblnk(message)),xcen,ycen,
     &    thkmid,ifuse,ipbase-1, ysamp, deltay,alphaAdd, thetaAdd, shiftAdd)
      if (ifNoAlpha .eq. 0 .or. ifNoTheta .eq. 0 .or. ifNoShift .eq. 0)
     &    print *
      if (ifNoAlpha .eq. 0) write(*,102)
     &    'X axis tilt - ', alphaOld, alphaAdd, alphaOld + alphaAdd
      if (ifNoTheta .eq. 0) write(*,102)
     &    'Angle offset -', thetaOld, thetaAdd, thetaOld + thetaAdd
      if (ifNoShift .eq. 0) write(*,101)
     &    'Z shift -     ', shiftOld, shiftAdd, shiftOld + shiftAdd
101   format(1x, a, ' Original:', f8.1,'   Added:',f8.1,'   Total:',f8.1)
102   format(1x, a, ' Original:', f8.2,'   Added:',f8.2,'   Total:',f8.2)
      call exit(0)
      end


      subroutine addlinepair(message, iobj1, iobj2, ysample, border,
     &    scaleFac, indy, maxx,
     &    xcen, ycen, thkmid, ifuse, ysamp, npatch, ipbase)
      implicit none
      include 'smallmodel.inc'
      character*(*) message
      integer*4 iobj1, iobj2, npatch, ipbase, maxx, indy, ifuse(*)
      real*4 ysample, border, scaleFac, xcen(*), ycen(*), thkmid(*), ysamp(*)
      integer*4 ibottop(2)
      real*4 slope(2),bintcp(2),xleft(2),xright(2)
      integer*4 ip1,ip2,ibt,line
      real*4 x1,y1,x2,y2,xlo,xhi,yll,ylr,yul,yur,alphaAdd, thetaAdd, shiftAdd
      integer*4 lnblnk

      ibottop(1)=iobj1
      ibottop(2)=iobj2
      if(p_coord(indy,object(ibase_obj(iobj1)+1)) +
     &    p_coord(indy,object(ibase_obj(iobj1)+npt_in_obj(iobj1))).gt.
     &    p_coord(indy,object(ibase_obj(iobj2)+1)) +
     &    p_coord(indy,object(ibase_obj(iobj2)+npt_in_obj(iobj2)))) then
        ibottop(1)=iobj2
        ibottop(2)=iobj1
      endif
      
      do line=1,2
        ibt=ibottop(line)
        ip1=object(ibase_obj(ibt)+1)
        ip2=object(ibase_obj(ibt)+npt_in_obj(ibt))
        x1=scaleFac * p_coord(1,ip1)
        y1=scaleFac * p_coord(indy,ip1)
        x2=scaleFac * p_coord(1,ip2)
        y2=scaleFac * p_coord(indy,ip2)
        slope(line)=(y2-y1)/(x2-x1)
        bintcp(line)=y2-slope(line)*x2
        xleft(line)=min(x1,x2)
        xright(line)=max(x1,x2)
      enddo	  
      
      xlo=min(xleft(1),xleft(2),-0.45 * scaleFac * maxx)
      xhi=max(xright(1),xright(2),0.45 * scaleFac * maxx)
      yll=xlo*slope(1)+bintcp(1)
      ylr=xhi*slope(1)+bintcp(1)
      yul=xlo*slope(2)+bintcp(2)
      yur=xhi*slope(2)+bintcp(2)
      xcen(ipbase)=xlo
      ycen(ipbase)=(yll+yul)/2.
      thkmid(ipbase)=2*border+yul-yll
      ysamp(ipbase)=ysample
      xcen(ipbase+1)=xhi
      ycen(ipbase+1)=(ylr+yur)/2.
      thkmid(ipbase+1)=2*border+yur-ylr
      ysamp(ipbase+1)=ysample
      ifuse(ipbase)=1
      ifuse(ipbase+1)=1
c       
      call analyzespots(message(1:lnblnk(message)),xcen(ipbase),
     &    ycen(ipbase), thkmid(ipbase),ifuse(ipbase),npatch,
     &    ysamp(ipbase),0., alphaAdd, thetaAdd, shiftAdd)
      ipbase=ipbase+npatch
      return
      end


      subroutine analyzespots(fillab,xcen,ycen,
     &    thkmid,ifuse,nspots,ysamp,doxtilt, alpha, thetaAdd, shiftAdd)
      implicit none
      character*(*) fillab
      real*4 xcen(*),ycen(*),thkmid(*),ysamp(*)
      integer*4 ifuse(*),nspots
      real*4 xx(1000),yy(1000),zz(1000),doxtilt, thetaAdd, shiftAdd
c       
      integer*4 nd,i
      real*4 ang,slop,bint,ro,a,b,c,alpha,theta,costh,sinth
      real*4 cosal,sinal,zp
      real*4 atand,cosd,sind
c       
      write (6,101)fillab
101   format(//,' Analysis of positions from ',a,':')
      call findshift('unrotated',ycen,thkmid,ifuse,nspots, shiftAdd)
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
      write(6,102)slop,ang
102   format(' slope =',f8.4,': to make level, add',f6.1,
     &    ' to total angle offset')
      call findshift(' rotated ',yy,thkmid,ifuse,nspots, shiftAdd)
c       
      if(doxtilt.eq.0.)return
c       
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
      theta=-atand(a)
      costh=cosd(theta)
      sinth=sind(theta)
      alpha=atand(b / (costh - a * sinth))
      cosal=cosd(alpha)
      sinal=sind(alpha)
      do i=1,nspots
        zp=xcen(i)*sinth + ycen(i)*costh
        yy(i)=-ysamp(i)*sinal + zp*cosal
      enddo
c$$$	alpha=atand(b)
c$$$	slop=a/(cosd(alpha)-b*sind(alpha))
c$$$	theta=-atand(slop)
c$$$	costh=cosd(theta)
c$$$	sinth=sind(theta)
c$$$	cosal=cosd(alpha)
c$$$	sinal=sind(alpha)
c$$$	do i=1,nspots
c$$$    zp=ycen(i)*cosal-ysamp(i)*sinal
c$$$    yy(i)=xcen(i)*sinth+zp*costh
c$$$	enddo
      write(6,103)alpha,-theta
103   format(/' Pitch between samples can be corrected with ',
     &    'an added X-axis tilt of',f7.2,/,' In this case, to make ',
     &    'level, add',f6.1,' to total angle offset')
      thetaAdd = -theta
      call findshift('x-tilted ',yy,thkmid,ifuse,nspots, shiftAdd)
      return
      end

      subroutine findshift(rotlab,ycen,thick,ifuse,nspots, shift)
      implicit none
      character*(*) rotlab
      real*4 ycen(*),thick(*), shiftAdd
      integer*4 ifuse(*),nspots
c       
      real*4 bot,top,realthk,shift
      integer*4 i,ithick
      integer*4 niceframe
c       
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
      write(6,101)rotlab,shift,realthk,ithick
101   format(1x,a,' lines imply added Z shift of',f7.1,'; thickness of',
     &    f6.1,', set to',i5)
      return
      end

c       $Log$
c       Revision 3.15  2008/07/15 18:19:54  mast
c       Fixed error messages to come out on one line on Windows
c
c       Revision 3.14  2006/06/29 05:09:43  mast
c       Switched to use small model
c
c       Revision 3.13  2006/05/02 19:41:21  mast
c       Added options to enter old values and get sums for output
c
c       Revision 3.12  2005/10/11 21:37:30  mast
c       Updated fallback PIP options
c	
c       Revision 3.11  2005/02/03 17:23:38  mast
c       Stopped swapping Y and Z of Y was long dimension, this made it fail
c       for very thick whole tomogram
c	
c       Revision 3.10  2004/06/15 05:44:33  mast
c       Generated an error messagewith more than 2 points per contour for
c       whole tomogram
c	
c       Revision 3.9  2004/05/21 20:09:48  mast
c       Added "added" to X-axis tilt report
c	
c       Revision 3.8  2003/11/14 00:49:51  mast
c       convert to PIP input, add whole-tomogram options for modeling
c	
c       Revision 3.7  2003/10/14 23:14:04  mast
c       More terminology changes
c	
c       Revision 3.6  2003/10/03 00:59:35  mast
c       Changed terminology to refered to tilt angle offset
c	
c       Revision 3.5  2002/07/21 19:31:11  mast
c       *** empty log message ***
c	
c       Revision 3.4  2002/07/21 19:30:50  mast
c       Standardized error output and made model coordinates get scaled
c       correctly
c	
c       Revision 3.3  2002/06/25 15:25:18  mast
c       Adjusted sample coordinates to be centered around zero to correct
c       problem with computation of shift with X-axis tilt.
c	
c       Revision 3.2  2002/05/21 03:12:17  mast
c       Remove ; at end of two lines, declare lnblnk
c	
c       Revision 3.1  2002/05/20 15:42:47  mast
c       Added analysis of single file with multiple time indexes
c	
