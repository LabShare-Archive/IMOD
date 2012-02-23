c       RESAMPLEMOD
c       
c       A program to turn a fiber model so that it is perpendicular to the
c       Z axis, then resample at section intervals so that it can be
c       analyzed with NDA.
c       See man page for details
c       
c       $Id$
c       

      implicit none
      include 'model.inc'
      integer limsec,limobj
      parameter (limsec=10000,limobj=1000)
      character*120 modelfile,newmodel
      logical exist,readw_or_imod
      real*4 rmat(3,3),vecmean(3),vecnorm(3)
      integer*4 iobjuse(limobj),iobjexcl(limobj)
      integer getimodhead
      real*4 xmt(limsec),ymt(limsec),zmt(limsec)
      logical failed
      integer*4 ierr, nobjuse, nobjexcl, ifflip, indz, indy, invaxis
      real*4 xyscal,zscale,xofs,yofs,zofs, xmin, ymin, zmin, xt, yt, zt
      integer*4 iobj,ninobj, ibase, ipt, itmp, nvec, i, ifuse, ifexcl, j
      real*4 xshft, yshft, zshft, tmp, frac, xmax, ymax, zmax
      integer*4 izst, iznd, newnin, iz, maxx, maxy, maxz, imodobj, imodcont
      integer*4 skipInv

      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetBoolean
      integer*4 PipGetString
      integer*4 PipGetInOutFile
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  resamplemod
c       
      integer numOptions
      parameter (numOptions = 8)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@output:OutputFile:FN:@'//
     &    'exclude:ExcludeObjects:LI:@'//
     &    'direction:DirectionObjects:LI:@main:MainAxis:I:@'//
     &    'skip:SkipInversion:B:@param:ParameterFile:PF:@'//
     &    'help:usage:B:'

      skipInv = 0
      invaxis = 3
c       
c       Pip startup: set error, parse options, check help, set flag if used
c       
      call PipReadOrParseOptions(options, numOptions, 'resamplemod',
     &    'ERROR: RESAMPLEMOD - ', .false., 2, 1, 1, numOptArg,
     &    numNonOptArg)
c       
      if (PipGetInOutFile('InputFile', 1, ' ', modelfile) .ne. 0)
     &    call errorexit('NO INPUT FILE SPECIFIED')
c       
c       read in the model
c       
      exist=readw_or_imod(modelfile)
      if(.not.exist)call errorexit('READING MODEL')

      if (PipGetInOutFile('OutputFile', 2, ' ', newmodel) .ne. 0)
     &    call errorexit('NO OUTPUT FILE SPECIFIED')

      nobjuse=0
      if (PipGetString('DirectionObjects', modelfile) .eq. 0)
     &    call parselist(modelfile,iobjuse,nobjuse)

      nobjexcl=0
      if (PipGetString('ExcludeObjects', modelfile) .eq. 0)
     &    call parselist(modelfile,iobjexcl,nobjexcl)

      ifflip=0
      ierr=getimodhead(xyscal,zscale,xofs,yofs,zofs,ifflip)
      if(ierr.ne.0)call errorexit('GETTING SCALING FROM MODEL HEADER')
      call scale_model(0)

      if(ifflip.eq.0)then
        indy=2
        indz=3
      else
        indz=2
        indy=3
      endif

      ierr = PipGetBoolean('SkipInversion', skipInv)
      ierr = PipGetInteger('MainAxis', invaxis)
      invaxis = max(1,min(3,invaxis))
      if (invaxis.eq.2)then
        invaxis = indy
      elseif(invaxis.eq.3)then
        invaxis = indz
      endif
      call PipDone()
c       
c       invert objects if starting z > ending z. 
c       
      if (skipInv.eq.0)then

        do iobj=1,max_mod_obj
          ninobj=npt_in_obj(iobj)
          if(ninobj.gt.0)then
            ibase=ibase_obj(iobj)
            if(p_coord(invaxis,abs(object(1+ibase))).gt.
     &          p_coord(invaxis,abs(object(ninobj+ibase))))then
              do ipt=1,ninobj/2
                itmp=object(ibase+ipt)
                object(ibase+ipt)=object(ibase+ninobj+1-ipt)
                object(ibase+ninobj+1-ipt)=itmp
              enddo
            endif
          endif
        enddo
      endif

      do i=1,3
        vecmean(i)=0.
        vecnorm(i)=0.
      enddo
      vecnorm(indz)=1.
      nvec=0
c       
c       get mean vector to turn to vertical, including Z scale
c       
      do iobj=1,max_mod_obj
        call objtocont(iobj,obj_color,imodobj,imodcont)
        ifuse=0
        if(nobjuse.eq.0)then
          ifuse=1
        else
          do i=1,nobjuse
            if(imodobj.eq.iobjuse(i))ifuse=1
          enddo
        endif
        ninobj=npt_in_obj(iobj)
        if(ninobj.gt.1.and.ifuse.eq.1)then
          ibase=ibase_obj(iobj)
          i = abs(object(ibase + 1))
          ipt = abs(object(ibase + ninobj))
c           
c           If inversion was skipped, check if this object needs to be 
c           inverted for adding into the sum
c           
          if (skipInv .ne. 0 .and. 
     &        p_coord(invaxis, i) .gt. p_coord(invaxis, ipt)) then
            i = abs(object(ibase + ninobj))
            ipt = abs(object(ibase + 1))
          endif
          
          vecmean(1)=vecmean(1)+ p_coord(1,ipt) - p_coord(1,i)
          vecmean(indy)=vecmean(indy)+ p_coord(indy,ipt) - p_coord(indy,i)
          vecmean(indz)=vecmean(indz)+ zscale*(
     &        p_coord(indz,ipt) - p_coord(indz,i))
          nvec=nvec+1
        endif
      enddo
      if(nvec.eq.0)stop 'NO CONTOURS FOUND'
      do i=1,3
        vecmean(i)=vecmean(i)/nvec
      enddo

      call vectors_to_rmat(vecmean,vecnorm,rmat)

      write(*,'(a,3f10.1)')'Mean vector:', (vecmean(i),i=1,3)
      print *,'Rotation matrix:'
      write(*,'(3f8.4)')((rmat(i,ipt),i=1,3),ipt=1,3)
      xt = vecmean(1)
      yt = vecmean(2)
      zt = vecmean(3)
      xmt(1)=xt*rmat(1,1)+yt*rmat(1,2)+zt*rmat(1,3)
      ymt(1)=xt*rmat(2,1)+yt*rmat(2,2)+zt*rmat(2,3)
      zmt(1)=(xt*rmat(3,1)+yt*rmat(3,2)+zt*rmat(3,3))/zscale
      write(*,'(a,3f10.1)')'Rotated vector:', xmt(1),ymt(1),zmt(1)

      xmin=1.e10
      ymin=1.e10
      zmin=1.e10
      do iobj=1,max_mod_obj
        call objtocont(iobj,obj_color,imodobj,imodcont)
        ifexcl=0
        do i=1,nobjexcl
          if(imodobj.eq.iobjexcl(i))ifexcl=1
        enddo
        ninobj=npt_in_obj(iobj)
        if(ninobj.gt.0)then
          ibase=ibase_obj(iobj)
c           
c           rotate the points in 3-D, scaling then unscaling by Z
c           
          do i=1,ninobj
            ipt=abs(object(i+ibase))
            xt=p_coord(1,ipt)
            yt=p_coord(indy,ipt)
            zt=zscale*p_coord(indz,ipt)
            xmt(i)=xt*rmat(1,1)+yt*rmat(1,2)+zt*rmat(1,3)
            ymt(i)=xt*rmat(2,1)+yt*rmat(2,2)+zt*rmat(2,3)
            zmt(i)=(xt*rmat(3,1)+yt*rmat(3,2)+zt*rmat(3,3))/zscale
            xmin=min(xmt(i),xmin)
            ymin=min(ymt(i),ymin)
            zmin=min(zmt(i),zmin)
          enddo
          xshft=10-xmin
          yshft=10-ymin
          zshft=1-nint(zmin)
c           
c           if not resampling, just place points back into contour
c           
          if(ifexcl.eq.1.or.ninobj.eq.1)then
            do i=1,ninobj
              ipt=abs(object(i+ibase))
              p_coord(1,ipt)=xmt(i)
              p_coord(indy,ipt)=ymt(i)
              p_coord(indz,ipt)=zmt(i)
            enddo
          else
c             
c             invert contour if this has gotten it backwards
c             
            if(zmt(1).gt.zmt(ninobj))then
              do i=1,ninobj/2
                j=ninobj+1-i
                tmp=xmt(i)
                xmt(i)=xmt(j)
                xmt(j)=tmp
                tmp=ymt(i)
                ymt(i)=ymt(j)
                ymt(j)=tmp
                tmp=zmt(i)
                zmt(i)=zmt(j)
                zmt(j)=tmp
              enddo
            endif
c             
c             get z limits for resampling, set first point into contour
c             
            call object_mover(iobj,failed)
            if(failed)stop 'insufficient object space'
            ibase=ibase_obj(iobj)
            izst=nint(zmt(1))
            if(izst.le.zmt(1)+0.01)izst=izst+1
            iznd=nint(zmt(ninobj))
            if(iznd.ge.zmt(ninobj)-0.01)iznd=iznd-1
            ipt=abs(object(1+ibase))
            p_coord(1,ipt)=xmt(1)
            p_coord(indy,ipt)=ymt(1)
            p_coord(indz,ipt)=zmt(1)
            newnin=1
c             
c             interpolate a point at each new Z level
c             
            do iz=izst,iznd
              j=1
              do while (j.lt.ninobj.and.
     &            .not.(zmt(j).le.iz.and.zmt(j+1).gt.iz))
                j=j+1
              enddo
              newnin=newnin+1
              if(newnin.gt.ninobj)then
                n_point=n_point+1
                ntot_in_obj=ntot_in_obj+1
                object(ibase+newnin)=n_point
                pt_label(n_point)=0
              endif
              ipt=object(ibase+newnin)
              frac=0.
              if(zmt(j+1)-zmt(j).gt.0.01)
     &            frac=(iz-zmt(j))/(zmt(j+1)-zmt(j))
              p_coord(1,ipt)=xmt(j)+frac*(xmt(j+1)-xmt(j))
              p_coord(indy,ipt)=ymt(j)+frac*(ymt(j+1)-ymt(j))
              p_coord(indz,ipt)=iz
            enddo
c             
c             finish with last point
c             
            newnin=newnin+1
            if(newnin.gt.ninobj)then
              n_point=n_point+1
              ntot_in_obj=ntot_in_obj+1
              object(ibase+newnin)=n_point
              pt_label(n_point)=0
            endif
            ipt=object(ibase+newnin)
            p_coord(1,ipt)=xmt(ninobj)
            p_coord(indy,ipt)=ymt(ninobj)
            p_coord(indz,ipt)=zmt(ninobj)
            npt_in_obj(iobj)=newnin
            ibase_free=max(ibase_free,ibase+newnin)
          endif
        endif
      enddo
c       
c       shift the points to be positive
c       
      xmax=-1000.
      ymax=-1000.
      zmax=-1000.
      do iobj=1,max_mod_obj
        ninobj=npt_in_obj(iobj)
        if(ninobj.gt.0)then
          ibase=ibase_obj(iobj)
          do ipt=1,ninobj
            i=abs(object(ipt+ibase))
            p_coord(1,i)=p_coord(1,i)+xshft
            p_coord(indy,i)=p_coord(indy,i)+yshft
            p_coord(indz,i)=p_coord(indz,i)+zshft
            xmax=max(xmax,p_coord(1,i))
            ymax=max(ymax,p_coord(indy,i))
            zmax=max(zmax,p_coord(indz,i))
          enddo 
        endif
      enddo
      maxx=10*(int(xmax+19)/10)
      maxy=10*(int(ymax+19)/10)
      maxz=nint(zmax)+2
      call scale_model(1)
      call putimodmaxes(maxx,maxy,maxz)
      call write_wmod(newmodel)
      call exit
      end


      subroutine vectors_to_rmat(avec,bvec,rm)
      implicit none
      real*4 rm(3,3),avec(*),bvec(*),cvec(3)
      real*4 x, y, z, sina, cosa, angle, sind, acosd, omca, absval
      equivalence (x,cvec(1)),(y,cvec(2)),(z,cvec(3))
      integer*4 i, j

      call crossproduct(avec,bvec,cvec)
      do i=1,3
        do j=1,3
          rm(i,j)=0.
          if(i.eq.j)rm(i,j)=1.
        enddo
      enddo
      if(absval(cvec).eq.0.)return
      cosa = (avec(1)*bvec(1) + avec(2)*bvec(2) + avec(3)*bvec(3)) /
     &    (absval(avec)*absval(bvec))
      angle = acosd(cosa)
      sina = -sind(angle)
      omca=1.0-cosa
      call normalize(cvec)
      rm(1,1) = x * x * omca + cosa
      rm(1,2) = y * x * omca + (sina * z)
      rm(1,3) = z * x * omca - (sina * y)

      rm(2,1) = x * y * omca - (sina * z)
      rm(2,2) = y * y * omca + cosa
      rm(2,3) = z * y * omca + (sina * x)

      rm(3,1) = x * z * omca + (sina * y)
      rm(3,2) = y * z * omca - (sina * x)
      rm(3,3) = z * z * omca + cosa
      return
      end



      real*4 function absval(vec)
      implicit none
      real*4 vec(3)
      absval = sqrt(vec(1)**2+vec(2)**2+vec(3)**2)
      return
      end


      subroutine crossproduct(avec,bvec,cvec)
      implicit none
      real*4 avec(*),bvec(*),cvec(*)
      cvec(1)=avec(2)*bvec(3)-avec(3)*bvec(2)
      cvec(2)=avec(3)*bvec(1)-avec(1)*bvec(3)
      cvec(3)=avec(1)*bvec(2)-avec(2)*bvec(1)
      return
      end



      subroutine normalize(vect)
      implicit none
      real*4 vect(3),sum,fac
      integer*4 i
      sum=0.
      do i=1,3
        sum=sum+vect(i)**2
      enddo
      fac=sqrt(sum)
      do i=1,3
        vect(i)=vect(i)/fac
      enddo
      return
      end

      subroutine copyvec(a,b)
      implicit none
      real*4 a(3),b(3)
      b(1)=a(1)
      b(2)=a(2)
      b(3)=a(3)
      return
      end


      subroutine errorexit(message)
      character*(*) message
      print *
      print *,'ERROR: RESAMPLEMOD - ',message
      call exit(1)
      end
