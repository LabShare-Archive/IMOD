*       * * * * XFINTERSTACK * * * * *
c       
c       This program will transform a model that has been built on one data
c       stack so that it will match the coordinates of a different data stack
c       that has some sections overlapping the first stack.
c       
c       To use it, you must first build a model file in which each object
c       has two points: the first point placed on a point in the first
c       data stack, the second point placed on the exactly corresponding
c       point on the same section in the second data stack.  This does not
c       require very many objects - 10 should be plenty.
c       
c       Entries to the program are:
c       
c       Name of the model file containing the objects with corresponding
c       points on an overlapping section.
c       
c       The Z value (typically the section #) for the points on the section
c       in the first data stack.  Only model objects whose first point has
c       this Z value will be included
c       
c       1 for a full report of deviations of points from perfect
c       correspondence (see below)
c       
c       (After the program solves for a transformation):
c       Name of the model file to be transformed
c       
c       0 to transform model from fitting the first stack to fitting the
c       second stack, or 1 to transform it from fitting the second stack
c       to fitting the first stack.
c       
c       If the second point in each correspondence object has a different
c       Z value from the first point (i.e. if the two data stacks do not have
c       congruent Z coordinates), then the Z values of all points in the
c       model being transformed will be adjusted by the difference in Z's
c       of the correspondence points.
c       
c       When the program solves for the transformation between a pair of
c       sections, it applies the transformation to the points on the second
c       section of the pair, and computes the displacement, or deviation,
c       between each point and the corresponding point on the first section
c       of the pair.  It then reports the mean deviation for all of the
c       points, the maximum deviation, and the object number of the point
c       with maximum deviation.  In addition, you may elect to have a
c       complete report of the deviations of all points for particularly bad
c       sections.  If you choose this option, you control which sections are
c       reported by specifying criterion values for the mean and maximum
c       deviations; the full report will be made for any sections with mean
c       or maximum deviations greater than the respective criteria.
c       
*       David Mastronarde 1/4/90
c       
      include 'model.inc'

      parameter (limpnts=4)                     !min # of points for regression
      real*4 f(2,3),g(2,3)
      parameter (idim=400)
      include 'statsize.inc'
      real*4 xr(msiz,idim), xm(msiz), sd(msiz), ssd(msiz,msiz), b1(msiz)
      character*320 modelfile,newmodel
      logical exist,readw_or_imod
c       
c       get parameters
c       
70    write(*,'(1x,a,/,a,$)')
     &    'Enter name of model file with objects showing the exact'
     &    //' correspondence between','    the two data stacks: '
      read(*,'(a)')modelfile
c       
75    exist=readw_or_imod(modelfile)
      if(.not.exist)go to 70
c       
      write(*,'(1x,a,$)')
     &    'Z value of points in first data stack: '
      read(*,*)zfid
c       
      write(*,'(1x,a,$)')'1 for full report of deviations'//
     &    ', 0 for none: '
      read(*,*)iffullrpt
c       
c       get points out of objects with 2 points at this Z value
c       
      write(*,122)
122   format(40x,'Deviations between transformed 2nd set',/,
     &    41x,' of points and 1st set of points',/,
     &    56x,'Mean      Max  @object #')
      npnts=0
      delz=1.e6
      do iobject=1,max_mod_obj
        if(npt_in_obj(iobject).ge.2)then
          ipnt1=object(1+ibase_obj(iobject))
          ipnt2=object(2+ibase_obj(iobject))
          if(abs(p_coord(3,ipnt1)-zfid).lt.1.e-3)then
            delztmp=p_coord(3,ipnt2)-p_coord(3,ipnt1)
            if(delz.eq.1.e6)then
              delz=delztmp
            else
              if(abs(delz-delztmp).gt..001)
     &            stop 'SECOND POINTS DO NOT ALL HAVE SAME Z VALUE' 
            endif
            npnts=npnts+1
c             
c             second point in object goes in 1st and 2nd
c             column: independent vars
c             
            xr(1,npnts)=p_coord(1,ipnt2)
            xr(2,npnts)=p_coord(2,ipnt2)
c             
c             first point goes in columns 4 and 5, dependent vars
c             
            xr(4,npnts)=p_coord(1,ipnt1)
            xr(5,npnts)=p_coord(2,ipnt1)
            xr(6,npnts)=iobject
          endif
        endif
      enddo
      if(npnts.ge.limpnts)then
c         
c         now if there are at least limpnts points, do regressions:
c         first 1st pt x then 1st pt. y as function of 2nd pt
c         x and y
c         
        do isol=1,2
c           
c           move 4th or 5th column into 3rd for regression
c           
          do i=1,npnts
            xr(3,i)=xr(3+isol,i)
          enddo
c           call multr(xr,3,npnts,sx,ss,ssd,d,r,xm,sd,b,b1,const, rsq ,fra)
          call multRegress(xr,msiz,1,2,npnts,1,0,b1,msiz,const,xm,sd,ssd)
c           
c           save results in xform for izsec
c           
          f(isol,1)=b1(1)
          f(isol,2)=b1(2)
          f(isol,3)=const
        enddo
c         
c         compute mean and max deviation between points in adjacent
c         sections after the transformation is applied
c         
        devsum=0.
        devmax=-1.
        do ipnt=1,npnts
          call xfapply(f,0.,0.,xr(1,ipnt),xr(2,ipnt),xx,
     &        yy)
          xdev=xr(4,ipnt)-xx
          ydev=xr(5,ipnt)-yy
          devpnt=sqrt(xdev**2+ydev**2)
          xr(7,ipnt)=xr(4,ipnt)+xcen
          xr(8,ipnt)=xr(5,ipnt)+ycen
          xr(9,ipnt)=xdev
          xr(10,ipnt)=ydev
          xr(11,ipnt)=atan2d(ydev,xdev)
          xr(12,ipnt)=devpnt
          devsum=devsum+devpnt
          if(devpnt.gt.devmax)then
            devmax=devpnt
            iobjmax=xr(6,ipnt)
          endif
        enddo
        devavg=devsum/npnts
C         
c         keep track of the highest transforms obtained
c         
        nfout=max0(nfout,indf)
        write(*,121)npnts,xfid,devavg,devmax,iobjmax
121     format(' xform obtained from',i4,
     &      ' points with Z of',f8.3,2x,2f10.2,i5)
        call xfwrite(6,f,*94)
        if(iffullrpt.ne.0)
     &      write(*,124)((xr(i,j),i=6,12),j=1,npnts)
124     format('    Object        position        deviation',
     &      ' vector   angle   magnitude',/,
     &      (f10.0,4f10.2,f9.0,f10.2))
c         
c         now get model file and other parameters
c         
80      write(*,'(1x,a,$)')'Name of input model file to transform: '
        read(*,'(a)')modelfile
85      exist=readw_or_imod(modelfile)
        if(.not.exist)go to 80
c         
        print *,'Enter name of new transformed model file '//
     &      '(Return for new version of old file)'
        read(*,'(a)')newmodel
        if(newmodel.eq.' ')newmodel=modelfile
c         
        write(*,'(1x,a,/,a,$)')'Enter 0 to transform from the'//
     &      ' alignment of the first point in each',
     &      '   object to the alignment of the second point,'//
     &      ' or 1 for the reverse: '
        read(*,*)ifreverse
        call xfcopy(f,g)
        if(ifreverse.eq.0)then
          call xfinvert(f,g)
        else
          delz=-delz
        endif
c         
c         transform the model
c         
        do i=1,n_point
          call xfapply(g,0.,0.,p_coord(1,i),p_coord(2,i)
     &        ,p_coord(1,i),p_coord(2,i))
          p_coord(3,i)=p_coord(3,i)+delz
        enddo
c         
c         write it out
c         
77      call write_wmod(newmodel)
        stop 'TRANSFORMED MODEL WRITTEN'
c         
      else
        print *,'less than',limpnts,' points for z of ',zfid
        stop 'CANNOT PROCEED'
      endif
94    print *,'error writing out f'
      stop
      end
