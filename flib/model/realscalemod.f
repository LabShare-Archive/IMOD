*       * * * * REALSCALEMOD * * * * *
c       
c       REALSCALEMOD will take a model and rescale it into microns.
c       It will adjust not only for pixel size and section thickness but
c       also for any tilts during microscopy.
c       
c       If the sections were significantly tilted during microscopy, the
c       program can adjust for these tilts given the proper information.
c       Prepare a file in which the first line shows the Z value and the
c       tilt of the first tilted section (or of the first section, if that
c       one was tilted), and each successive line shows the Z value and tilt
c       for each section on which tilt was changed.  Z values should occur in
c       ascending order.
c       
c       Entries to the program are:
c       
c       Name of model file to be scaled
c       
c       Name of output file in which to place scaled model
c       
c       Name of file with tilt information, or Return if none
c       
c       IF the model has no scaling information in its header, also make the
c       following three entries:
c       
c       .  Magnification of negatives (without any commas)
c       
c       .  Scale, in microns per pixel, at which negatives were digitized
c       
c       .  Section thickness in nanometers
c       
c       David Mastronarde  11/19/93; modified for IMOD 4/24/97
c       
      include 'model.inc'
      character*80 modelfile,newmodel,tiltfile
      logical exist,readw_or_imod
      real*4 tiltzstrt(1000),tilt(1000),costilt(1000),remapz(1000)
c       
91    write(*,'(1x,a,$)')'Name of input model file: '
      read(*,'(a)')modelfile
c       
c       read in the model
c       
75    exist=readw_or_imod(modelfile)
      if(.not.exist)go to 91
c       
      write(*,'(1x,a,$)')'Name of output file for scaled model: '
      read(*,'(a)')newmodel
c       
      write(*,'(1x,a,$)')
     &    'Name of file of tilt info (Return if none): '
      read(*,'(a)')tiltfile
      if(tiltfile.ne.' ')then
        call dopen(3,tiltfile,'ro','f')
        ntilts=0
3       i=ntilts+1
        read(3,*,end=5)tiltzstrt(i),tilt(i)
        ntilts=i
        costilt(ntilts)=cosd(tilt(ntilts))
        go to 3
5       remapz(1)=tiltzstrt(1)
        do i=1,ntilts-1
          remapz(i+1)=remapz(i)+(tiltzstrt(i+1)-tiltzstrt(i))
     &        /costilt(i)
        enddo
        zmax=-1.e10
        do i=1,n_point
          zz=p_coord(3,i)
          if(zz.ge.tiltzstrt(1))then
            itilt=ntilts
            do while(zz.lt.tiltzstrt(itilt))
              itilt=itilt-1
            enddo
            znew=remapz(itilt)+(zz-tiltzstrt(itilt))/
     &          costilt(itilt)
            p_coord(3,i)=znew
            zmax=max(zmax,znew)
          endif
        enddo
        print *,'new maximum z is',zmax
      endif
c       
      defscal=1.e6
      ierr=getimodhead(xyscal,zscale,xofs,yofs,zofs,ifflip)
      if(ierr.eq.0.and.abs(xyscal-defscal)/defscal.gt.1.e-5)then
        write(*,'(a,f10.6,a)')' Scale set from model header at',
     &      xyscal,' microns/pixel'
        zscal=xyscal*zscale
      else
        ifflip=0
        write(*,'(1x,a,$)')'Magnification of negatives: '
        read(*,*)xmag
        write(*,'(1x,a,$)')'Scale at which negatives were digitized'
     &      //' (microns per pixel from VIDS): '
        read(*,*)umperpix
        write(*,'(1x,a,$)')'Nominal section thickness (nm): '
        read(*,*)secthick
c         
        xyscal=umperpix/xmag
        zscal=secthick/1000.
      endif
      if(ifflip.eq.0)then
        indy=2
        indz=3
      else
        indz=2
        indy=3
      endif
c       
      do i=1,n_point
        p_coord(1,i)=p_coord(1,i)*xyscal
        p_coord(indy,i)=p_coord(indy,i)*xyscal
        p_coord(indz,i)=p_coord(indz,i)*zscal
      enddo
c       
77    call write_wmod(newmodel)
      print *, 'RESCALED MODEL WRITTEN'
      call exit(0)
      end
