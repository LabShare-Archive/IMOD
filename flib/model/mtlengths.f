*       * * * * MTLENGTHS * * * * *
c       
c       MTLENGTHS computes the lengths, in microns, of contours in
c       IMOD models, and produces a list giving the object number and contour
c       length for each contour.  It can correct for tilting of sections
c       during microscopy.  See the man page for full details.
c       
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       Revision 3.6  2005/01/07 15:56:43  mast
c       Fixed to work with pixel sizes in header
c       
c       Revision 3.5  2003/10/10 20:42:42  mast
c       Used new subroutine for getting input/output files
c       
c       Revision 3.4  2003/10/09 02:33:33  mast
c       converted to use autodoc
c       
c       Revision 3.3  2003/09/05 18:15:34  mast
c       Finish declarations for implicit none
c       
c       Revision 3.2  2003/09/05 00:15:52  mast
c       Incorporated changes from "supermtl" version for correcting for
c       obliqueness, and implemented PIP input
c       
c       
c       David Mastronarde  1/27/90; modified for IMOD 4/4/97
c       
      implicit none
      include 'model.inc'
      character*120 modelfile,newmodel,tiltfile
      logical exist,readw_or_imod
      integer limsec
      parameter (limsec=1000)
      real*4 tiltzstrt(limsec),tilt(limsec),costilt(limsec)
      real*4 remapz(limsec),cosaxis(limsec)
      real*4 sinaxis(limsec),sintilt(limsec),axisangle(limsec)
      real*4 g(2,3,limsec)
      integer*4 iobjexcl(255)
      integer getimodhead
c       
      integer*4 ierr,ntilts,ifxfout,i,nxg,ifinvert,ifflip,indx,indy
      real*4 theta,smag,str,phi,rawangle,defscal,zscal,zscale,xyscal
      real*4 xofs,yofs,zofs,xmag,umperpix,secthick,zadd,posdifall
      integer*4 lenobj,nobjexcl,iout,iobj,ninobj,ibase,ipt,ip1,itmp
      integer*4 ifaxis,ndifall,nposall,itilt,nposbetter,ndifs,imodobj
      real*4 posdifsum,costmp,sintmp,dx,dy,dz,utlenpos,utlenneg
      integer*4 ifexclude,ie,ip2,iz,nnegbetter,nout,imodcont,indz
      real*4 fracbetter,avgdif,zminall,zmaxall,zminobj,zmaxobj,zraw
      real*4 sum,sumpos,sumneg,sumposlen,sumneglen,scalezdist,thickadj
      real*4 zminscl,zmaxscl,tiltshift,zminlen,zmaxlen,sumlen
      integer*4 izmax
      integer*4 lookuptilt
      real*4 cosd,sind,untiltlen,scalez
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetBoolean
      integer*4 PipGetString,PipGetFloat
      integer*4 PipGetNonOptionArg, PipGetInOutFile
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  mtlengths
c       
      integer numOptions
      parameter (numOptions = 16)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'model:ModelFile:FN:@output:OutputFile:FN:@'//
     &    'tilt:TiltFile:FN:@extra:ExtraLength:F:@'//
     &    'magnification:Magnification:F:@scale:ScaleDigitized:F:@'//
     &    'section:SectionThickness:F:@exclude:ExcludeObjects:LI:@'//
     &    'marker:MarkerObject:I:@correct:CorrectObliques:B:@'//
     &    'axis:AxisAngle:F:@align:AlignmentTransforms:FN:@'//
     &    'invert:InvertAngles:B:@untilt:UntiltTransforms:FN:@'//
     &    'param:ParameterFile:PF:@help:usage:B:'
c       
c       set defaults
c       
      tiltfile = ' '
      newmodel = ' '
      zadd = 0
      nobjexcl=0
      ifinvert = 0
      ifxfout=0
      lenobj = 0
      ifaxis = 0
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'mtlengths',
     &    'ERROR: MTLENGTHS - ', .true., 1, 1, 1, numOptArg,
     &    numNonOptArg)
      pipinput = numOptArg + numNonOptArg .gt. 0
c       
      if (PipGetInOutFile('ModelFile', 1, 'Name of input model file',
     &    modelfile) .ne. 0) call errorexit('NO INPUT FILE SPECIFIED')
c       
c       read in the model
c       
      exist=readw_or_imod(modelfile)
      if(.not.exist)call errorexit('OPENING MODEL FILE')
c       
      if (pipinput) then
        ierr = PipGetString('TiltFile', tiltfile)
      else
        write(*,'(1x,a,$)')
     &      'Name of file of tilt info (Return if none): '
        read(*,'(a)')tiltfile
      endif
      ntilts=0
      if(tiltfile.ne.' ')then
        call dopen(3,tiltfile,'ro','f')
3       i=ntilts+1
        read(3,*,end=5)tiltzstrt(i),tilt(i)
        ntilts=i
        costilt(ntilts)=cosd(tilt(ntilts))
        sintilt(ntilts)=sind(tilt(ntilts))
        go to 3
5       remapz(1)=tiltzstrt(1)
        do i=1,ntilts-1
          remapz(i+1)=remapz(i)+(tiltzstrt(i+1)-tiltzstrt(i))
     &        /costilt(i)
        enddo
        close(3)
c         
        if (pipinput) then
          ierr = PipGetBoolean('CorrectObliques', ifaxis)
          if(ifaxis.ne.0)then
            if (PipGetFloat('AxisAngle', rawangle) .gt. 0) call errorexit
     &          ('YOU MUST ENTER THE ANGLE OF THE TILT AXIS')

            do i=1,limsec
              cosaxis(i)=cosd(-rawangle)
              sinaxis(i)=sind(-rawangle)
              axisangle(i)=rawangle
            enddo

            tiltfile = ' '
            ierr  = PipGetString('AlignmentTransforms', tiltfile)
            if(tiltfile.ne.' ')then
              call dopen(3,tiltfile,'ro','f')
              call xfrdall(3,g,nxg,*96)
              do i=1,nxg
                call amat_to_rotmagstr(g(1,1,i),theta,smag,str,phi)
                cosaxis(i)=cosd(-(rawangle+theta))
                sinaxis(i)=sind(-(rawangle+theta))
                axisangle(i)=rawangle+theta
              enddo
            endif

            ierr = PipGetBoolean('InvertAngles', ifinvert)
            if(ifinvert.ne.0)then
              do i=1,ntilts
                tilt(i)=-tilt(i)
                sintilt(i)=-sintilt(i)
              enddo
            endif
            tiltfile = ' '
            ierr  = PipGetString('UntiltTransforms', tiltfile)
            if(tiltfile.ne.' ')ifxfout=1
          endif
        endif
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
        if (pipinput) then
          if (PipGetFloat('Magnification', xmag) .gt. 0 .or.
     &        PipGetFloat('ScaleDigitized', umperpix) .gt. 0 .or.
     &        PipGetFloat('SectionThickness', secthick) .gt. 0) call
     &        errorexit('NO SCALE IN MODEL HEADER - YOU MUST ENTER'//
     &        ' SCALE PARAMETERS')
        else
          write(*,'(1x,a,$)')'Magnification of negatives: '
          read(*,*)xmag
          write(*,'(1x,a,$)')'Scale at which negatives were digitized'
     &        //' (microns per pixel from VIDS): '
          read(*,*)umperpix
          write(*,'(1x,a,$)')'Nominal section thickness (nm): '
          read(*,*)secthick
        endif
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
      call scale_model(0)
c       
      if (pipinput) then
        ierr = PipGetFloat('ExtraLength', zadd)
        if (PipGetString('OutputFile', newmodel) .gt. 0 .and.
     &      numNonOptArg .gt. 1) ierr = PipGetNonOptionArg(2, newmodel)
        ierr = PipGetInteger('MarkerObject', lenobj)
        if (PipGetString('ExcludeObjects', tiltfile) .eq. 0)
     &      call parselist(tiltfile, iobjexcl, nobjexcl)
      else
        write(*,'(1x,a,/,a,$)')'Enter amount to add to maximum Z in '//
     &      'each object to account for','   section thickness (i.e.'//
     &      ', 1 to add one section worth of length): '
        read(5,*)zadd
c         
        write(*,'(1x,a,$)')'Name of output file (Return for terminal): '
        read(*,'(a)')newmodel
c         
        print *,'Enter list of IMOD object #s to exclude, or ',
     &      'Return to include all objects'
        call rdlist(5,iobjexcl,nobjexcl)
      endif
      if(newmodel.eq.' ')then
        iout=6
      else
        iout=7
        call dopen(7,newmodel,'new','f')
      endif
      call PipDone()
c       
c       invert objects if starting z > ending z.  Scale X and Y, leave Z
c       unscaled so it can be used for section lookup
c       
      do iobj=1,max_mod_obj
        ninobj=npt_in_obj(iobj)
        if(ninobj.gt.0)then
          ibase=ibase_obj(iobj)
          do ipt=1,ninobj
            ip1=abs(object(ipt+ibase))
            p_coord(1,ip1)=p_coord(1,ip1)*xyscal
            p_coord(indy,ip1)=p_coord(indy,ip1)*xyscal
          enddo
          if(p_coord(indz,abs(object(1+ibase))).gt.
     &        p_coord(indz,abs(object(ninobj+ibase))))then
            do ipt=1,ninobj/2
              itmp=object(ibase+ipt)
              object(ibase+ipt)=object(ibase+ninobj+1-ipt)
              object(ibase+ninobj+1-ipt)=itmp
            enddo
          endif
        endif
      enddo

      if(ifaxis.ne.0)then
        tiltzstrt(ntilts+1)=1.e6
        ndifall=0
        nposall=0
        posdifall=0.
        do itilt=1,ntilts
          nposbetter=0
          posdifsum=0.
          ndifs=0
          costmp=costilt(itilt)
          sintmp=sintilt(itilt)
          do iobj=1,max_mod_obj
            if(npt_in_obj(iobj).gt.0)then
              imodobj=256-obj_color(2,iobj)
              if(obj_color(1,iobj).eq.0)imodobj=-imodobj
              ibase=ibase_obj(iobj)
              ifexclude=0
              do ie=1,nobjexcl
                if(imodobj.eq.iobjexcl(ie))ifexclude=1
              enddo
              if(ifexclude.eq.0)then
                do ipt=1,npt_in_obj(iobj)-1
                  ip1=abs(object(ipt+ibase))
                  ip2=object(ipt+1+ibase)
                  if(p_coord(indz,ip1).ge.tiltzstrt(itilt).and.
     &                p_coord(indz,ip1).lt.tiltzstrt(itilt+1).and.
     &                ip2.gt.0)then
                    dx=p_coord(1,ip2)-p_coord(1,ip1)
                    dy=p_coord(indy,ip2)-p_coord(indy,ip1)
                    dz=p_coord(indz,ip2)-p_coord(indz,ip1)
                    iz=p_coord(indz,ip1)+1.01
                    utlenpos=untiltlen(dx,dy,dz,zscal,costmp,sintmp,
     &                  cosaxis(iz),sinaxis(iz))
                    utlenneg=untiltlen(dx,dy,dz,zscal,costmp,-sintmp,
     &                  cosaxis(iz),sinaxis(iz))
                    ndifs=ndifs+1
                    posdifsum=posdifsum+utlenpos-utlenneg
                    if(utlenpos.gt.utlenneg) nposbetter=nposbetter+1
                  endif
                enddo
              endif
            endif
          enddo
          nnegbetter=ndifs-nposbetter
          fracbetter=float(nposbetter)/ndifs
          avgdif=posdifsum/ndifs
          write(*,123)tiltzstrt(itilt),tilt(itilt),ndifs,fracbetter,
     &        avgdif
123       format(' z start =',f5.0,', tilt =',f6.1,', # seg =',
     &        i5, ', frac better =',f6.3,', dif =',f9.4)
          ndifall=ndifall+ndifs
          nposall=nposall+nposbetter
          posdifall=posdifall+posdifsum
        enddo
        fracbetter=float(nposall)/ndifall
        avgdif=posdifall/ndifall
        write(*,124)ndifall,fracbetter, avgdif
124     format(' Overall, # segments =',
     &      i5, ', frac better =',f6.3,', avg dif =',f9.4)
      endif
      zminall=1.e10
      zmaxall=-1.e10
      nout=0
      do iobj=1,max_mod_obj
        zminobj=1.e10
        zmaxobj=-1.e10
        if(npt_in_obj(iobj).gt.0)then
          call objtocont(iobj,obj_color,imodobj,imodcont)
c           if(obj_color(1,iobj).eq.0)imodobj=-imodobj
          ibase=ibase_obj(iobj)
          do ipt=1,npt_in_obj(iobj)
            ip1=abs(object(ipt+ibase))
            zraw=p_coord(indz,ip1)
            zminobj=min(zraw,zminobj)
            zmaxobj=max(zraw,zmaxobj)
          enddo
          zminall=min(zminall,zminobj)
          zmaxall=max(zmaxall,zmaxobj)
          ifexclude=0
          do ie=1,nobjexcl
            if(imodobj.eq.iobjexcl(ie))ifexclude=1
          enddo
          if(lenobj.eq.imodobj)ifexclude=0
          if(ifexclude.eq.0)then
            sum=0.
            sumpos=0.
            sumneg=0.
            do ipt=1,npt_in_obj(iobj)-1
              ip1=abs(object(ipt+ibase))
              ip2=object(ipt+1+ibase)
              if(ip2.gt.0) then
                dx=p_coord(1,ip2)-p_coord(1,ip1)
                dy=p_coord(indy,ip2)-p_coord(indy,ip1)
                scalezdist=sqrt(dx**2+dy**2+
     &              (scalez(p_coord(indz,ip1),zscal,tiltzstrt,remapz,
     &              costilt,ntilts)-
     &              scalez(p_coord(indz,ip2),zscal,tiltzstrt,remapz,
     &              costilt,ntilts))**2)
                sum=sum+scalezdist
                if(ifaxis.ne.0)then
                  itilt=lookuptilt(p_coord(indz,ip1),tiltzstrt,ntilts)
                  if(itilt.eq.0)then
                    sumpos=sumpos+scalezdist
                    sumneg=sumneg+scalezdist
                  else
                    dz=p_coord(indz,ip2)-p_coord(indz,ip1)
                    iz=p_coord(indz,ip1)+1.01
                    sumpos=sumpos+untiltlen(dx,dy,dz,zscal,
     &                  costilt(itilt),sintilt(itilt),
     &                  cosaxis(iz),sinaxis(iz))
                    sumneg=sumneg+untiltlen(dx,dy,dz,zscal,
     &                  costilt(itilt),-sintilt(itilt),
     &                  cosaxis(iz),sinaxis(iz))
                  endif
                endif
              endif
            enddo
            if(zadd.gt.0.) then
              thickadj =scalez(zmaxobj+zadd,zscal,
     &            tiltzstrt,remapz,costilt,ntilts) - scalez(zmaxobj,
     &            zscal,tiltzstrt,remapz,costilt,ntilts)
              sum=sum+thickadj
              if(ifaxis.ne.0)then
                sumpos=sumpos+thickadj
                sumneg=sumneg+thickadj
              endif
            endif
            if(imodobj.eq.lenobj)then
              zminlen=zminobj
              zmaxlen=zmaxobj
              sumlen=sum
              sumposlen=sumpos
              sumneglen=sumneg
            else
              if(ifaxis.eq.0)then
                write(iout,'(i4,f12.4)')imodobj,sum
c                 write(iout,'(i4,i5,f12.4)')imodobj,imodcont,sum
              else
                write(iout,'(i4,i5,3f12.4)')imodobj,imodcont,sum,
     &              sumpos,sumneg
              endif
              nout=nout+1
            endif
          endif
        endif
      enddo
      zminscl=scalez(zminall,zscal,tiltzstrt,remapz,costilt,ntilts)
      zmaxscl=scalez(zmaxall+zadd,zscal,tiltzstrt,remapz,costilt,
     &    ntilts)
      write(*,103)zminall,zmaxall,zminscl,zmaxscl,zmaxscl-zminscl
103   format(' Original minimum and maximum Z in the model were ',f8.2
     &    ,' and'f8.2,/, ' These values were scaled to',f9.3,' and',
     &    f9.3,' microns (difference',f9.3,')')
      if(lenobj.ne.0)then
        zminscl=scalez(zminlen,zscal,tiltzstrt,remapz,costilt,ntilts)
        zmaxscl=scalez(zmaxlen+zadd,zscal,tiltzstrt,remapz,costilt,
     &      ntilts)
        write(*,105)zminlen,zmaxlen,zminscl,zmaxscl,zmaxscl-zminscl
105     format(' Length marker minimum and maximum Z were ',f8.2
     &      ,' and'f8.2,/, ' These values were scaled to',f9.3,' and',
     &      f9.3,' microns (difference',f9.3,')')
        if(ifaxis.eq.0)then
          write(*,106)sumlen
106       format(' Length marker length is',f9.3,' microns')
        else
          write(*,107)sumlen,sumposlen,sumneglen
107       format(' Length marker length in microns:',f9.3,
     &        ' (with no obliquity correction)',/,7x,f9.3,
     &        ' (with correction), ',f9.3,' (with tilts inverted)')
        endif
      endif
      if(zadd.ne.0)write(*,104)zadd
104   format(' This includes the effects of adding',f5.1,
     &    ' section at high Z')
      close(7)
      print *,nout,' objects output'
      if(ifxfout.ne.0)then
        call dopen(1,tiltfile,'new','f')
        izmax=max(nint(zmaxall+1),nxg)
        do iz=1,izmax
          itilt=lookuptilt(iz-1.0,tiltzstrt,ntilts)
          if(itilt.eq.0)then
            call xfunit(g(1,1,iz),1.0)
          else
            call rotmagstr_to_amat(0.,1.,1./costilt(itilt),
     &          axisangle(iz),g(1,1,iz))
            tiltshift=(zscal/xyscal)*(sintilt(itilt)/costilt(itilt))
            g(1,3,iz)=tiltshift*cosd(axisangle(iz))
            g(2,3,iz)=tiltshift*sind(axisangle(iz))
          endif
          if(iz.gt.1)then
            g(1,3,iz)=g(1,3,iz)+g(1,3,iz-1)
            g(2,3,iz)=g(2,3,iz)+g(2,3,iz-1)
          endif
          call xfwrite(1,g(1,1,iz),*97)
        enddo
        close(1)
        print *,izmax,' untilting transforms written to file'
      endif
      call exit(0)
96    call errorexit( 'READING TRANSFORMS')
97    call errorexit( 'WRITING TRANSFORMS')
      end


c       SCALEZ will scale the Z index coordinate ZZ by first remapping
c       the Z values via the tilt remappings, then by multiplying by ZSCAL
c       
      real*4 function scalez(zz,zscal,tiltzstrt,remapz,costilt,ntilts)
      implicit none
      real*4 tiltzstrt(*),remapz(*),costilt(*)
      real*4 zz,zscal
      real*4 scalezz
      integer*4 itilt,ntilts
      scalez=zz
      if(ntilts.gt.0)then
        if(zz.ge.tiltzstrt(1))then
          itilt=ntilts
          do while(zz.lt.tiltzstrt(itilt))
            itilt=itilt-1
          enddo
          scalez=remapz(itilt)+(zz-tiltzstrt(itilt))/costilt(itilt)
        endif
      endif
      scalez=scalez*zscal
      return
      end


      real*4 function untiltlen(dx,dy,dz,zscal,costilt,sintilt,cosaxis,
     &    sinaxis)
      implicit none
      real*4 dx,dy,dz,zscal,costilt,sintilt,cosaxis,sinaxis, xv, yv, t
      xv=dx*cosaxis-dy*sinaxis
      yv=dx*sinaxis+dy*cosaxis
      t=dz*zscal
      untiltlen=sqrt(((xv+t*sintilt)/costilt)**2+yv**2+t**2)
      return
      end

      integer*4 function lookuptilt(zz,tiltzstrt,ntilts)
      implicit none
      real*4 tiltzstrt(*),zz
      integer*4 ntilts, itilt
      itilt=0
      if(ntilts.gt.0.and.zz.ge.tiltzstrt(1))then
        itilt=ntilts
        do while(zz.lt.tiltzstrt(itilt))
          itilt=itilt-1
        enddo
      endif
      lookuptilt=itilt
      return
      end

      subroutine errorexit(message)
      character*(*) message
      print *
      print *,'ERROR: MTLENGTHS - ',message
      call exit(1)
      end
