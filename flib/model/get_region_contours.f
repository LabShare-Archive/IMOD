c       GET_REGION_CONTOURS will read the MODELFILE as a small model, and
c       extract contours.  It first determines whether the contours lie in 
c       X/Y or X/Z planes, setting IFFLIP to 1 if they are in Y/Z planes.
c       It extracts the planar points into XVERT, YVERT, where INDVERT is an 
c       index to the start of each contour, NVERT contains the number of
c       points in each contour, ZCONT is the contour Y or Z value, NCONT is
c       the number of contours.  LIMCONT and LIMVERT specify the limiting
c       dimensions for contours and vertices.  If IMUNIT is greater than 0
c       the model is scaled to the index coordinates of the image file open 
c       on that unit
c
c       If contours are not planar in either direction it issues a warning
c       using PROGNAME as the program name and and adopts the flip value
c       from the model header, which is not necessarily correct.
c
c       $Id$
c
      subroutine get_region_contours(modelfile, progname, xvert, yvert, nvert,
     &    indvert, zcont,  ncont, ifflip, limcont, limvert, imunit)
      implicit none
      include 'smallmodel.inc'
      character*(*) modelfile, progname
      real*4 xvert(*), yvert(*), zcont(*)
      integer*4 nvert(*), indvert(*), ncont, ifflip, limcont, limvert, imunit
      integer*4 ip, ipt, iobj,iy,iz, indy, indz, indcur,ierr,getimodhead
      logical*4 exist, readSmallMod, zplanar, yplanar
      real*4 xyscal,zscal,xofs,yofs,zofs
c
      exist=readSmallMod(modelfile)
      if(.not.exist)call exitError('ERROR READING MODEL')
      if (imunit .gt. 0) then
        call scaleModelToImage(imunit, 0)
      else
        call scale_model(0)
      endif
      ncont = 0
c       
c       figure out if all contours are coplanar Z or Y
c       Loop through contours until both planar flags go false or all done
c       
      iobj = 1
      zplanar = .true.
      yplanar = .true.
      do while (iobj .le. max_mod_obj .and. (zplanar .or. yplanar))
        if (npt_in_obj(iobj).ge.3) then
          ip = 2
          ipt=abs(object(ibase_obj(iobj)+1))
          iy = nint(p_coord(2,ipt))
          iz = nint(p_coord(3,ipt))
          do while (ip .le. npt_in_obj(iobj) .and. (zplanar .or. yplanar))
            ipt=abs(object(ibase_obj(iobj)+ip))
            if (nint(p_coord(2, ipt)) .ne. iy) yplanar = .false.
            if (nint(p_coord(3, ipt)) .ne. iz) zplanar = .false.
            ip = ip + 1
          enddo
        endif
        iobj = iobj + 1
      enddo
c       
c       Set flip flag or fallback to what is in model header
c       
      if (zplanar .and. .not. yplanar) then
        ifflip = 0
      else if (yplanar .and. .not. zplanar) then
        ifflip = 1
      else
        ierr=getimodhead(xyscal,zscal,xofs,yofs,zofs,ifflip)
        write(*,'(/,a,a,a)')'WARNING: ',progname,' - CONTOURS NOT ALL '//
     &      'COPLANAR, USING HEADER FLIP FLAG'
      endif
      indy=2
      indz=3
      if (ifflip.ne.0)then
        indy=3
        indz=2
      endif
c       
c       Load planar data into x/y arrays
c       
      indcur=0
      do iobj=1,max_mod_obj
        if(npt_in_obj(iobj).ge.3)then
          ncont=ncont+1
          if(ncont.gt.limcont)call exitError('TOO MANY CONTOURS IN MODEL')
          nvert(ncont)=npt_in_obj(iobj)
          if(indcur+nvert(ncont).gt.limvert)call exitError(
     &        'TOO MANY POINTS IN CONTOURS')
          do ip=1,nvert(ncont)
            ipt=abs(object(ibase_obj(iobj)+ip))
            xvert(ip+indcur)=p_coord(1,ipt)
            yvert(ip+indcur)=p_coord(indy,ipt)
          enddo
          zcont(ncont)=p_coord(indz,ipt)
          indvert(ncont)=indcur+1
          indcur=indcur+nvert(ncont)
        endif
      enddo
      if (progname .ne. 'FILLTOMO')
     &    print *,ncont, ' contours available for deciding which patches to analyze'
      return
      end


c       summarizeDrops outputs a summary of residuals dropped as outliers,
c       breaking them into 10 bins from the minimum to either the maximum or
c       5 SDs above the mean, whichever is less.  DROPSUM has the residuals,
c       NLISTD is the number of values, and meanText is text to indicate if
c       they are mean residuals or not.
c
      subroutine summarizeDrops(dropsum, nlistd, meanText)
      implicit none
      character*(*) meanText
      real*4 dropsum(*)
      integer*4 nlistd,i,j,inbin
      real*4 dropMin, dropMax, dropAvg, dropsd, dropsem, binlo, binhi, dropBin
c    
      dropMin = 1.e10
      dropMax = 0.
      do i = 1, nlistd
        dropMin = min(dropMin, dropsum(i))
        dropMax = max(dropMax, dropsum(i))
      enddo
      call avgsd(dropsum, nlistd, dropavg, dropsd, dropsem)
      dropBin = (min(dropMax, dropAvg + 5 * dropsd) - dropMin) / 10.
c       
      do j = 1, 10
        binlo = dropMin + (j - 1) * dropBin
        binhi = binLo + dropBin
        if (j .eq. 10) binhi = dropMax + 0.001
        inbin = 0
        do i = 1, nlistd
          if (dropsum(i) .ge. binlo .and. dropsum(i) .lt. binhi)
     &        inbin = inbin + 1
        enddo
        if (inbin .gt. 0) write(*,109)inbin, meanText, binlo, binhi
109     format(i8,' with ',a,'residuals in',f10.2,' -',f10.2)
      enddo
      return
      end
