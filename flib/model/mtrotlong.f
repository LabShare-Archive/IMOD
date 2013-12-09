c       MTROTLONG - TO EXTRACT MT ENDS IN LONGITUDINAL ORIENTATION
c
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c
      implicit none
      include 'model.inc'
      integer limpt
      parameter (limpt=10000)
      character*160 modelfile,pointfile,secode,objcode,intout
      logical exist,readw_or_imod
      real*4 xx(limpt),yy(limpt),zz(limpt),rot(limpt)
      integer*4 iconstr(limpt)
c
      integer*4 iobject, ninobj, ipnt1, ipnt2, ifcommand,nxrot,nyrot,nzrot,i
      integer*4 iobj,iobjin,icont,ifend,iftop,npt,nptft,ipt,icen,imid
      integer*4 indy,indz,ipnt,nchar, ifxy
      real*4 zdif,xdif,ydif,slopex,bint,ro,gamma,slopey,beta, alpha,cosang
      real*4 sinang,zout,yout, slopez
c
      integer*4 iobjfromcont
      real*4 atand, cosd, sind, goodangle       
      logical b3dxor
c       
c       read model in
c       
      call getinout(1,modelfile,pointfile)
75    exist=readw_or_imod(modelfile)
      if(.not.exist)then
        write(*, '(/,a, a)')'ERROR: MTROTLONG - READING MODEL FILE ', 
     &      trim(modelfile) 
        call exit(1)
      endif
      call scale_model(0)
      do iobject=1,max_mod_obj
        ninobj=npt_in_obj(iobject)
        if(ninobj.gt.0)then
          ipnt1=abs(object(1+ibase_obj(iobject)))
          ipnt2=abs(object(ninobj+ibase_obj(iobject)))
          write(*,101) iobject,obj_color(2,iobject),ninobj,
     &        nint(p_coord(1,ipnt1)),
     &        nint(p_coord(2,ipnt1)), nint(p_coord(3,ipnt1)),
     &        nint(p_coord(1,ipnt2)),
     &        nint(p_coord(2,ipnt2)), nint(p_coord(3,ipnt2))
101       format(3i5,2x,3i5,2x,3i5)
        endif
      enddo
c	write(*,'(1x,a,$)')'Starting contour number for each object: '
c	call rdlist(5,iconstr,nconstr)
c	write(*,'(1x,a,$)')
c       &	    '1 if model built on -Y flipped data, 0 if not: '
c	read(5,*)ifflip
      write(*,'(1x,a,$)')'1 if image volume is flipped so X/Y planes'
     &    //' look good; 0 if not (X/Z good): '
      read(5,*)ifxy
      write(*,'(1x,a,$)')'0 for no command file, 1 single or 2'//
     &    ' double rotatevol output: '
      read(5,*)ifcommand
      if(ifcommand.ne.0)then
        write(*,'(1x,a,$)')'Name of output command file: '
        read(5,'(a)')modelfile
        call dopen(3,modelfile,'new','f')
        write(*,'(1x,a,$)')'Name of data volume: '
        read(5,'(a)')modelfile
        write(*,'(1x,a,$)')'NX, NY, NZ of single rotated file: '
        read(5,*)nxrot,nyrot,nzrot
        write(*,'(1x,a,$)')'Single letter codes for objects: '
        read(5,'(a)')objcode
        write(*,'(1x,a,$)')'Single letter codes for start/end: '
        read(5,'(a)')secode

        if (ifcommand.gt.1)then
          call dopen(4,'interleave', 'new', 'f')
          write(4,105)2*nzrot+1
          do i = 0, nzrot
            write(4,104)'mtl.st2'
            write(4,105)i
            if (i .lt. nzrot) then
              write(4,104)'mtl.st1'
              write(4,105)i
            endif
          enddo
          write(4,104)'1'
          write(4,104)'mtl.sth'
          write(4,104)'/'
          write(4,104)'/'
          write(4,104)'0'
          write(4,104)'0'
          write(4,104)'0'
        endif
      endif
10    write(*,'(1x,a,/,a,$)')
     &    'object #, contour #, do start (0) or end (1), rotate end',
     &    '  to bottom (0) or to top (1), # of points to fit: '
      read(5,*)iobjin,icont,ifend,iftop,npt
      if(iobjin.eq.0)then
        close(3)
        write(*,'(//,a,a,a)')'The command file ',
     &      trim(pointfile),' is ready to run'
        call exit(0)
      endif
c	iobj=iconstr(iobjin)+icont-1
      iobj=iobjfromcont(iobjin,icont)
      if(iobj.eq.0)then
        write(*,'(/,a,i4,a,i4,a)')'ERROR: MTROTLONG - Contour ',icont,
     &      ' in object ',iobj, ' does not exist'
        call exit(1)
      endif
      ninobj=npt_in_obj(iobj)
      if(ninobj.le.2)then
        write(*,'(/,a,i4,a,i4,a)')'ERROR: MTROTLONG - Contour ',icont,
     &      ' in object ',iobj, ' has too few points'
        call exit(1)
      endif
      nptft=min(ninobj,npt)
      ipt=1
      icen=1
      imid=nptft
      if(ifend.ne.0)then
        ipt=ninobj+1-nptft
        icen=nptft
        imid=1
      endif
      indy=2
      indz=3
c	if(ifflip.ne.0)then
c       indy=3
c       indz=2
c	endif
      do i=1,nptft
        ipnt=abs(object(ipt+ibase_obj(iobj)))
        ipt=ipt+1
        xx(i)=p_coord(1,ipnt)
        yy(i)=p_coord(indy,ipnt)
        zz(i)=p_coord(indz,ipnt)
      enddo
      zdif=abs(zz(nptft)-zz(1))
      xdif=abs(xx(nptft)-xx(1))
      ydif=abs(yy(nptft)-yy(1))
c       
c       find out which axis is the principal one: three cases to handle
c       
      if(zdif.lt.xdif.or.zdif.lt.ydif)then
        if(ifxy.eq.0)then
          if(xdif.lt.ydif)then
c             
c             Y is the leading axis; fit X to Y and get gamma
c             
            call lsfit(yy,xx,nptft,slopex,bint,ro)
            gamma=atand(slopex)-90.
            if(b3dxor(iftop.ne.0, yy(icen).gt.yy(imid)))
     &          gamma=goodangle(gamma+180.)
          else
c	      
c             X is leading axis, fit Y to X to get gamma, then add 90
c             
            call lsfit(xx,yy,nptft,slopey,bint,ro)
            gamma=-atand(slopey)
            if(b3dxor(iftop.ne.0, xx(icen).gt.xx(imid)))
     &          gamma=goodangle(gamma+180.)
          endif
c	    
c           either case, rotate the coordinates, fit Z to new X, get beta
c	    
          cosang=cosd(gamma)
          sinang=sind(gamma)
          do i=1,nptft
            rot(i)=cosang*xx(i)-sinang*yy(i)
          enddo
          call lsfit(rot,zz,nptft,slopez,bint,ro)
          beta=atand(slopez)-90.
          alpha=-90.
        else
          if(xdif.lt.ydif)then
c             
c             Y is the leading axis; fit X to Y and get gamma
c             
            call lsfit(yy,xx,nptft,slopex,bint,ro)
            gamma=atand(slopex)
            if(b3dxor(iftop.ne.0, yy(icen).gt.yy(imid)))
     &          gamma=goodangle(gamma+180.)
          else
c	      
c             X is leading axis, fit Y to X to get gamma, then add 90
c             
            call lsfit(xx,yy,nptft,slopey,bint,ro)
            gamma=90.-atand(slopey)
            if(b3dxor(iftop.ne.0, xx(icen).gt.xx(imid)))
     &          gamma=goodangle(gamma+180.)
          endif
c	    
c           either case, rotate the coordinates, fit Z to new Y, get alpha
c	    
          cosang=cosd(gamma)
          sinang=sind(gamma)
          do i=1,nptft
            rot(i)=sinang*xx(i)+cosang*yy(i)
          enddo
          call lsfit(rot,zz,nptft,slopez,bint,ro)
          alpha=-atand(slopez)
          beta=0.
        endif
      else
c         
c         Z leading: fit X to Z to get beta, rotate, fit Y to new Z, get alph
c         
        call lsfit(zz,xx,nptft,slopex,bint,ro)
        beta=-atand(slopex)
        cosang=cosd(-beta)
        sinang=sind(-beta)
        do i=1,nptft
          rot(i)=sinang*xx(i)+cosang*zz(i)
        enddo
        call lsfit(rot,yy,nptft,slopey,bint,ro)
        alpha=atand(slopey)-90.
        if(b3dxor(iftop.ne.0, zz(icen).gt.zz(imid)))
     &      alpha=goodangle(alpha+180.)
        gamma=0.
      endif
c       
      zout=nint(zz(icen))+0.5
      yout=nint(yy(icen))+0.5
      write(*,102)iobjin,icont,ifend,nptft,
     &    nint(xx(icen)),yout,zout,
     &    gamma,beta,alpha
102   format(2i4,2i3,/,i4,2f6.1,/,3f7.1)

      if(ifcommand.ne.0)then
        if (icont .lt. 1000) then
          write(intout, '(i3.3)')icont
          nchar = 3
        else
          call int_iwrite(intout,icont,nchar)
        endif
        pointfile=objcode(iobjin:iobjin)//intout(1:nchar)//
     &      secode(ifend+1:ifend+1)//'.st'
        write(3,104)'$rotatevol'
104     format(a)
        write(3,104)modelfile
        if(ifcommand.eq.1)then
          write(3,104)pointfile
        else
          write(3,104)'mtl.st1'
        endif
        write(3,104)
        write(3,'(3i5)')nxrot,nyrot,nzrot
        write(3,105) nint(xx(icen)),yout,zout,gamma,beta,alpha
105     format(i6,2f8.1,/,3f7.1)
        if (ifcommand.ne.1)then
          write(3,104)'$rotatevol'
          write(3,104)modelfile
          write(3,104)'mtl.st2'
          write(3,104)
          write(3,'(3i5)')nxrot,nyrot,nzrot+1
          write(3,105) nint(xx(icen)),yout,zout,gamma,beta,alpha
          write(3,104)'$newstack <interleave'
          write(3,104)'$\\mv mtl.sth '//pointfile
        endif

        write(3,104)'$alterheader '//pointfile
        write(3,104)'org'
        write(3,104)'0,0,0'
        write(3,104)'tlt'
        write(3,104)'0,0,0'
        if (ifcommand.ne.1)then
          write(3,104)'del'
          write(3,104)'1.,1.,0.5'
        endif
        write(3,104)'done'
      endif
      go to 10
      end

      function goodangle(theta)
      goodangle=theta
      if(goodangle.le.-180.)goodangle=goodangle+360.
      if(goodangle.gt.180.)goodangle=goodangle-360.
      return
      end

