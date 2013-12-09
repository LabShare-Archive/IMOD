*       * * * * IMAVGSTAT * * * * *
c       
c       IMAVGSTAT generates statistics on the mean and standard deviation
c       of image density in selected areas of images that are obtained by
c       averaging multiple samples.  It can also produce a new set of
c       averaged images that are all normalized to have the same average
c       density in specified reference areas.  A set of images showing the
c       standard deviation at each pixel may also be produced and used for
c       statistical analysis by other programs such as SUBIMSTAT.
c       Typically, the program would be used to compare averages of
c       different sample sets.
c       
c       For details see man page
c
c       David Mastronarde 1/23/90; modified for IMOD 4/25/97
c       
c       $Id$
c
      parameter (narealim=2500,npixlim=100000,ixdim=2100,iydim=2100
     &    ,maxsec=1000,meanlim=100000)
      parameter (idimbc=(ixdim*iydim+3)/4)
c       
c       allow really big image as long as it is not being transformed at all
c       thus array is big, brray and crray are one third the size.
c       
      real*4 array(4*idimbc),brray(idimbc),crray(idimbc),drray(idimbc)
      equivalence (brray(1),array(1+idimbc))
     &    ,(crray(1),array(1+2*idimbc)),(drray(1),array(1+3*idimbc))
      common /bigarr/array
c       
      integer*4 listind(npixlim),npixarea(narealim),nxyz(3)
      integer*4 iobjin(narealim),nsumarea(narealim),mxyz(3)
      integer*4 listype(40,40),ntype(40),ntypelist(100),
     &    itypsec(maxsec),listbase(narealim+1),listz(maxsec)
      real*4 xvert(500),yvert(500),xvtmp(5),yvtmp(5)
      real*4 title(20),cell(6),crit(0:2),rectratio(narealim)
      equivalence (nx,nxyz(1)),(ny,nxyz(2)),(nz,nxyz(3))
      logical galigned,putnewavg,keep(maxsec),over(0:2),statout,
     &    insubset(maxsec),sdimage,normalize
      character dat*9,tim*8
c       
c       7/7/00 CER: remove the encode's; titlech is the temp space
c       
      character*80 titlech
      real*4 areamean(meanlim),allmean(0:narealim)
      real*4 allsd(0:narealim),subsem(0:narealim),subsd(0:narealim)
      real*4 submean(0:narealim),critbase(0:2)
c       
c       default values controlling automatic outlier elimination
c       
      data delfact/0.003/,fractlim/0.33/
      data crit/2.,2.,2./,ifandcrit/0/
c       
      real*4 glist(2,3,500),flist1(2,3,maxsec),flist2(2,3,maxsec)
      real*4 prod1(2,3),prod2(2,3,maxsec)
c       
      include 'model.inc'
      character*100 modelfile,filin,filout
      character*120 copyline
      character*3 oldnew
      logical exist,readw_or_imod
      integer*4 nxyzin(3),mxyzin(3),nsumcheck(narealim),
     &    npixcheck(narealim),nxyzst(3)/0,0,0/
c       
      fracmatt=0.05
      call time(tim)
      call date(dat)
c       
      write(*,'(1x,a,$)')'0 to place output in new files, or 1 to '
     &    //'append to existing files: '
      read(5,*)ifappend
c       
91    print *,'Enter name of model file with summing regions ',
     &    '(Return if none)'
      read(*,'(a)')modelfile
c       
c       read in the model
c       
      if(modelfile.ne.' ')then
        exist=readw_or_imod(modelfile)
        if(.not.exist)go to 91
      endif
c       
      print *,'Enter name of file of G transforms used to align'//
     &    ' averages from','   different data sets (Return if none)'
      read(*,'(a)')filin
      galigned=filin.ne.' '
      if(galigned)then
        call dopen(1,filin,'ro','f')
        call xfrdall(1,glist,nglist,*98)
        close(1)
      endif
c       
      print *,'Enter name of file to output statistics into '
     &    //'(Return for none): '
      read(*,'(a)')filout
      statout=filout.ne.' '
      if(statout)then
        if(ifappend.ne.0)call dopen(3,filout,'ro','f')
        call dopen(4,filout,'new','f')
      endif
c       
      print *,'Enter file name to get a new stack of aligned averages'
     &    ,' (Return for none)'
      read(*,'(a)')filin
      putnewavg=filin.ne.' '
      if(putnewavg)then
        oldnew='new'
        if(ifappend.ne.0)then
          call copy_to_backup(filin)
          oldnew='old'
        endif
        call imopen(2,filin,oldnew)
      endif
c       
      print *,'Enter file name to get standard deviation images'
     &    ,' (Return for none)'
      read(*,'(a)')filin
      sdimage=filin.ne.' '
      if(sdimage)then
        oldnew='new'
        if(ifappend.ne.0)then
          call copy_to_backup(filin)
          oldnew='old'
        endif
        call imopen(3,filin,oldnew)
      endif
c       
      if(modelfile.ne.' ')then
        print *,'Name of output image file to see pixels in areas'//
     &      ' (Return for none)'
        read(*,'(a)')filout
c         
        print *,'Enter list of contours specifying LOW density',
     &      ' normalizing areas'
        call rdobjcont(iobjin,nobjlo)
c         
        print *,'Enter list of contours specifying HIGH density',
     &      ' normalizing areas'
        call rdobjcont(iobjin(nobjlo+1),nobjhi)
        indhi=nobjlo+nobjhi
c         
c         don't normalize if low and high areas are the same
c         
        normalize=iobjin(1).ne.iobjin(nobjlo+1)
c         
        do ind=1,indhi
          if(npt_in_obj(iobjin(ind)).lt.3)then
            call objtocont(iobjin(ind),obj_color,imodobj,imodcont)
            write(*,'(a,i4,a,i5,a,i5,a,/,a)')'Object',imodobj,
     &          ', contour' ,imodcont,' (WIMP object',iobjin(ind),
     &          ') needs at least',
     &          ' 3 points to specify a density normalizing area'
            stop
          endif
        enddo
c         
        print *,'Enter list of contours specifying summing regions'
        call rdobjcont(iobjin(indhi+1),nregion)
c         
        nregion=nregion+indhi
        do i=indhi+1,nregion
          if(npt_in_obj(iobjin(i)).lt.2 .or.
     &        npt_in_obj(iobjin(i)).eq.3) then
            call objtocont(iobjin(ind),obj_color,imodobj,imodcont)
            write(*,'(a,i4,a,i5,a,i5,a,/,a)')'Object',imodobj,
     &          ', contour' ,imodcont,' (WIMP object',iobjin(i),
     &          ') needs at least',
     &          ' 4 points to specify a summing region'
            stop
          endif
        enddo
c         
        do i=indhi+1,nregion
          nsumarea(i)=1
          rectratio(i)=0.
        enddo
        print *, 'For each of these regions, enter the number',
     &      ' of summing areas, or minus the','    desired width',
     &      ' (in pixels) of the summing areas (enter / for all 1''s)'
        read(*,*)(nsumarea(i),i=indhi+1,nregion)
c         
        print *,'Enter a value for each region:',
     &      '   For regions specified by two points, enter 0 for'//
     &      ' circles, 1 for squares,',
     &      '      or ratio of width to length for rectangles;',
     &      '   For regions specified by >3 points, just enter 0',
     &      ' (enter / for all 0''s)'
        read(*,*)(rectratio(i),i=indhi+1,nregion)
c         
      else
        nregion=0
        indhi=0
        nobjlo=0
        nobjhi=0
        normalize=.false.
        filout=' '
      endif
c       
      write(*,'(1x,a,$)')'Image file NX and NY dimensions: '
      read(*,*)nx,ny
      if(nx*ny.gt.4*idimbc)stop 'IMAGE SIZE TOO LARGE'
      if(nx*ny.gt.idimbc)print *,'WARNING: IMAGE SIZE TOO LARGE '//
     &    'TO TRANSFORM IMAGES OR TO OUTPUT NEW AVERAGES'
c       
c       get lists of pixels in all of the summing areas
c       
      nareatot=0
      listbase(1)=1
      do iregion=1,nregion
        iobj=iobjin(iregion)
        nvert=npt_in_obj(iobj)
        do ii=1,nvert
          ipt=abs(object(ii+ibase_obj(iobj)))
          xvert(ii)=p_coord(1,ipt)
          yvert(ii)=p_coord(2,ipt)
        enddo
        if(iregion.le.indhi)then
c           
c           if its in low or high density area, accumulate into single area
c           
          if(iregion.eq.1.or.iregion.eq.nobjlo+1)then
            nareatot=nareatot+1
            npixarea(nareatot)=0
          endif
          call listinside(xvert,yvert,nvert,nx,ny,
     &        listind(listbase(nareatot)+npixarea(nareatot)),npixadd)
          npixarea(nareatot)=npixarea(nareatot)+npixadd
          listbase(nareatot+1)=listbase(nareatot)+npixarea(nareatot)
c           
        elseif((nsumarea(iregion).eq.1.or.nsumarea(iregion).eq.0)
     &        .and.nvert.ge.4)then
c           
c           if its a multi-point
c           area that is not to be divided up, just take the whole area
c           
          nareatot=nareatot+1
          call listinside(xvert,yvert,nvert,nx,ny,
     &        listind(listbase(nareatot)),npixarea(nareatot))
          listbase(nareatot+1)=listbase(nareatot)+npixarea(nareatot)
        else
          if(nvert.eq.2)then
            if(nsumarea(iregion).lt.0)then
c               
c               if the region is a line (2 points), first find out how many
c               areas to divide it into, if pixel spacing is specified by a
c               negative NSUMAREA
c               
              pxln=sqrt((xvert(2)-xvert(1))**2+(yvert(2)-yvert(1))**2)
              nsumarea(iregion)=-(pxln-1.)/nsumarea(iregion)
            endif
c             
            if(rectratio(iregion).gt.0)then
c               
c               make up a quadrilateral if rectangles requested
c               
              nvert=4
              xvec=-0.5*rectratio(iregion)*(yvert(2)-yvert(1))
     &            /nsumarea(iregion)
              yvec=0.5*rectratio(iregion)*(xvert(2)-xvert(1))
     &            /nsumarea(iregion)
              xvert(4)=xvert(1)-xvec
              xvert(1)=xvert(1)+xvec
              yvert(4)=yvert(1)-yvec
              yvert(1)=yvert(1)+yvec
              xvert(3)=xvert(2)-xvec
              xvert(2)=xvert(2)+xvec
              yvert(3)=yvert(2)-yvec
              yvert(2)=yvert(2)+yvec
            else
c               
c               if circles requested, find pixels in circles along line
c               
              rad=0.5*sqrt((xvert(2)-xvert(1))**2+
     &            (yvert(2)-yvert(1))**2)/nsumarea(iregion)
              do iarea=1,nsumarea(iregion)
                frac=(iarea-0.5)/nsumarea(iregion)
                xcen=(1-frac)*xvert(1)+frac*xvert(2)
                ycen=(1-frac)*yvert(1)+frac*yvert(2)
                ixst=max(1.,xcen-rad-0.5)
                ixnd=min(float(nx),xcen+rad+1.5)
                iyst=max(1.,ycen-rad-0.5)
                iynd=min(float(ny),ycen+rad+1.5)
                npix=0
                nareatot=nareatot+1
                do iy=iyst,iynd
                  do ix=ixst,ixnd
                    if(sqrt((ix-0.5-xcen)**2+(iy-0.5-ycen)**2)
     &                  .le.rad)then
                      listind(npix+listbase(nareatot))=ix+(iy-1)*nx
                      npix=npix+1
                    endif
                  enddo
                enddo
                npixarea(nareatot)=npix
                listbase(nareatot+1)=listbase(nareatot)
     &              +npixarea(nareatot)
              enddo
            endif
          endif
          if(nvert.gt.2)then
c             
c             if more than 2 vertices, process one or more quadrilaterals
c             
            if(nvert.le.5)then
c               
c               if a quadrilateral, copy 1st point into 5th and find out
c               which sides are longer and should be divided up
c               set up for one segment, set up istr to be first index of
c               quadrilateral and iend to be last
c               
              xvert(5)=xvert(1)
              yvert(5)=yvert(1)
              istr=1
              if(quadlength(xvert,yvert,1,4).lt.
     &            quadlength(xvert,yvert,2,5))istr=2
              iend=istr+3
              nsegment=1
            else
c               
c               if more than 4 vertices, set up for multiple segments
c               
              istr=1
              iend=2*(nvert/2)
              nsegment=(iend-istr-1)/2
            endif
c             
c             compute total length to be divided up
c             
            pxlentot=0.
            do iseg=0,nsegment-1
              pxlentot=pxlentot+
     &            quadlength(xvert,yvert,istr+iseg,iend-iseg)
            enddo
c             
            nsumartot=0
            do iseg=1,nsegment
c               
c               find number of summing areas for this segment: either the
c               total desired summing areas apportioned by the length of this
c               segment, or the length divided by the desired pixels per
c               summing area
c               
              if(nsumarea(iregion).ge.0)then
                nsumtmp=max(1.,nsumarea(iregion)*
     &              (quadlength(xvert,yvert,istr,iend)+0.01)/pxlentot)
              else
                nsumtmp=-(quadlength(xvert,yvert,istr,iend)-1.)/
     &              nsumarea(iregion)
              endif
c               
c               interpolate 4 corners of each summing area, get pixel lists
c               
              do iarea=1,nsumtmp
                frac=float(iarea-1)/nsumtmp
                xvtmp(1)=(1.-frac)*xvert(istr)+frac*xvert(istr+1)
                xvtmp(4)=(1.-frac)*xvert(iend)+frac*xvert(iend-1)
                yvtmp(1)=(1.-frac)*yvert(istr)+frac*yvert(istr+1)
                yvtmp(4)=(1.-frac)*yvert(iend)+frac*yvert(iend-1)
                frac=float(iarea)/nsumtmp
                xvtmp(2)=(1.-frac)*xvert(istr)+frac*xvert(istr+1)
                xvtmp(3)=(1.-frac)*xvert(iend)+frac*xvert(iend-1)
                yvtmp(2)=(1.-frac)*yvert(istr)+frac*yvert(istr+1)
                yvtmp(3)=(1.-frac)*yvert(iend)+frac*yvert(iend-1)
                nareatot=nareatot+1
                call listinside(xvtmp,yvtmp,4,nx,ny,
     &              listind(listbase(nareatot)),npixarea(nareatot))
                listbase(nareatot+1)=listbase(nareatot)
     &              +npixarea(nareatot)
              enddo
c               
c               after each segment, accumulate number of areas and advance to
c               next segment
c               
              nsumartot=nsumartot+nsumtmp
              istr=istr+1
              iend=iend-1
            enddo
            nsumarea(iregion)=nsumartot
          endif
        endif
      enddo
c       
      if(filout.ne.' ')then
        call imopen(1,filout,'NEW')
        nz=1
c         
c         7/7/00 CER: remove the encodes
c         
c         encode(80,300,title)dat,tim
        write(titlech,300) dat,tim
        read(titlech,'(20a4)')(title(kti),kti=1,20)
300     format('IMAVGSTAT: File of summing areas',t57,a9,2x,a8)
        call icrhdr(1,nxyz,nxyz,0,title,1)
        do i=1,3
          cell(i)=nxyz(i)
          cell(i+3)=90.
        enddo
        call ialcel(1,cell)
        do i=1,nx*ny
          array(i)=0.
        enddo
        do iarea=1,nareatot
          do i=listbase(iarea),listbase(iarea+1)-1
            array(listind(i))=iarea+3.
          enddo
        enddo
        call iclden(array,nx,ny,1,nx,1,ny,dmin,dmax,dmean)
        call iwrsec(1,array)
        call iwrhdr(1,title,0,dmin,dmax,dmean)
        call imclose(1)
      endif
c       
c       get overall characteristics of stacks
c       
      write(*,'(1x,a,$)')'Number of data sets to compare: '
      read(*,*)nsets
c       
c       find out if doing subsets for each data set
c       
      do i=1,nsets
        ntypelist(i)=0
      enddo
      print *,'Enter # of subsets of positions to average for each'//
     &    ' set (/ for none)'
      read(*,*)(ntypelist(i),i=1,nsets)
c       
c       read in old stuff for all files if appending, check validity of
c       various parameters.  Start with image file.
c       
      nsetout=0
      if(ifappend.ne.0)then
        if(putnewavg)then
          call irdhdr(2,nxyzin,mxyzin,modein,dmin2,dmax2,dmean2)
          nsetout=nxyzin(3)
          if(nx.ne.nxyzin(1).or.ny.ne.nxyzin(2).or.modein.ne.2)
     &        stop 'OLD AVERAGE IMAGE FILE DOES NOT MATCH'
          dsum=dmean2*nx*ny*nsetout
          call imposn(2,nsetout,0)
        endif
c         
        if(sdimage)then
          call irdhdr(3,nxyzin,mxyzin,modein,sdmin,sdmax,sdmean)
          if(nsetout.eq.0)nsetout=nxyzin(3)
          if(nx.ne.nxyzin(1).or.ny.ne.nxyzin(2).or.modein.ne.2.or.
     &        nsetout.ne.nxyzin(3))
     &        stop 'OLD STANDARD DEVIATION IMAGE FILE DOES NOT MATCH'
          sdsum=sdmean*nx*ny*nsetout
          call imposn(3,nsetout,0)
        endif
c         
        if(statout)then
          read(3,*)nregcheck
          read(3,*)(nsumcheck(i),i=1,nregcheck)
          read(3,*)nareacheck
          read(3,*)(npixcheck(i),i=1,nareacheck)
          read(3,*)nsetcheck
          if(nsetout.eq.0)nsetout=nsetcheck
          if(nsetout.ne.nsetcheck.or.nregcheck.ne.nregion-indhi.or.
     &        nareacheck.ne.nareatot-2)
     &        stop 'OLD STATISTICS FILE DOES NOT MATCH'
          do i=1,nregcheck
            if(nsumarea(i+indhi).ne.nsumcheck(i))
     &          stop 'OLD STATISTICS FILE DOES NOT MATCH'
          enddo
          do i=1,nareacheck
            if(npixarea(i+2).ne.npixcheck(i))
     &          stop 'OLD STATISTICS FILE DOES NOT MATCH'
          enddo
        endif
      endif
c       
c       compute new total number of sets to be output
c       
      do i=1,nsets
        nsetout=nsetout+max(1,ntypelist(i))
      enddo
c       
c       set up file if putting out new averages
c       
      nz=nsetout
      do i=1,3
        cell(i)=nxyz(i)
        cell(i+3)=90.
      enddo
      if(putnewavg)then
        if(ifappend.eq.0)then
c           
c           7/7/00 CER: remove the encodes
c           
c           encode(80,301,title)dat,tim
          write(titlech,301) dat,tim
          read(titlech,'(20a4)')(title(kti),kti=1,20)
301       format('IMAVGSTAT: Aligned, normalized averages',t57,a9,2x,
     &        a8)
          call icrhdr(2,nxyz,nxyz,2,title,1)
          dmin2=1.e10
          dmax2=-1.e10
          dsum=0.
        endif
        call ialsiz(2,nxyz,nxyzst)
        call ialsam(2,nxyz)
        call ialcel(2,cell)
      endif
      if(sdimage)then
        if(ifappend.eq.0)then
c           encode(80,302,title)dat,tim
          write(titlech,302) dat,tim
          read(titlech,'(20a4)')(title(kti),kti=1,20)
302       format('IMAVGSTAT: Standard deviation images',t57,a9,2x,a8)
          call icrhdr(3,nxyz,nxyz,2,title,1)
          sdmin=1.e10
          sdmax=-1.e10
          sdsum=0.
        endif
        call ialsiz(3,nxyz,nxyzst)
        call ialsam(3,nxyz)
        call ialcel(3,cell)
      endif
c       
c       set up file if putting out statistics
c       
      if(statout)then
        write(4,'(i7,a)')nregion-indhi,' summing regions with '//
     &      'the following numbers of summing areas:'
        write(4,'(19i4)')(nsumarea(i),i=indhi+1,nregion)
        write(4,'(i7,a)')nareatot-2,' total summing areas with the'//
     &      ' following numbers of pixels:'
        write(4,'(15i5)')(npixarea(i),i=3,nareatot)
        write(4,'(i7,a)')nsetout,' data sets'
c         
c         if appending, copy rest of old file
c         
        if(ifappend.ne.0)then
C           
C           7/20/00  CER remove the q's
C           
14        read(3,'(a)',end=15) copyline
          nchar = len_trim(copyline)
          write(4,'(a)')copyline(1:nchar)
          go to 14
15        continue
        endif
      endif
c       
c       loop through data sets
c       
      do iset=1,nsets
c         
        write(*,'(/,a,i3,/)')' Next enter parameters for data set #',
     &      iset
        write(*,'(1x,a,$)')'Name of image stack to be averaged: '
        read(*,'(a)')filin
c         
        call imopen(1,filin,'ro')
        call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean)
c         
        izst=0
        iznd=nz-1
        nlistz=nz
        do i=1,nlistz
          listz(i)=i-1
        enddo
        print *,'Enter list of section #''s to try '//
     &      'to include (ranges OK, enter / for all)'
        call rdlist(5,listz,nlistz)
c         
        write(*,'(1x,a,$)')'# of sets of F transforms applied to '//
     &      'stack before averaging (0, 1 or 2): '
        read(*,*)nfsets
c         c         
c         get the f transforms used before averaging
c         
        nfsets=min(2,nfsets)
        do jfset=1,nfsets
          write(*,'(1x,a,i2,a,$)')'File name for transform set #',
     &        jfset,': '
          read(*,'(a)')filin
          call dopen(1,filin,'ro','f')
          if(jfset.eq.1)then
            call xfrdall(1,flist1,nflist1,*98)
          else
            call xfrdall(1,flist2,nflist2,*98)
          endif
          close(1)
        enddo
        if(nfsets.eq.2.and.nflist1.ne.nflist2)stop
     &      'Not the same number of transforms in the two files'
c         
        if(nfsets.gt.0)then
          write(*,'(1x,a,$)')'Offset to add to section # to get'//
     &        ' line in transform file (1st line 0): '
          read(*,*)iofsxf
c           
c           set true range of sections based on # of transforms and offset
c           
          izst=max(izst,-iofsxf)
          iznd=min(iznd,nflist1-1-iofsxf)
        endif
c         
        if(galigned)then
          write(*,'(1x,a,$)')'Line number of G transform for this '//
     &        'set in file of G''s (1st is 0): '
          read(*,*)lineg
        endif
c         
        if(ntypelist(iset).gt.0)then
c           
c           get subset lists
c           
          print *,'Name of file with list of',
     &        ' position #''s for each section'
          read(*,'(a)')filin
          call dopen(1,filin,'ro','f')
          read(1,*)(itypsec(i),i=1,nz)
          close(1)
c           
          write(*,'(a,i3,a)')' Enter',ntypelist(iset),
     &        ' lists of positions'//
     &        ' to average, each list on a separate line'
          do isub=1,ntypelist(iset)
            call rdlist(5,listype(1,isub),ntype(isub))
          enddo
        else
c           
c           or set up to take all sections
c           
          ntypelist(iset)=1
          ntype(1)=1
          listype(1,1)=1
          do i=1,nz
            itypsec(i)=1
          enddo
        endif
c         
c         write(*,'(a,i4,a,i4)')' Sections will be included from',
c         &           izst,' to',iznd
        isecst=izst+1
        isecnd=iznd+1
c         
c         loop on subsets
c         
        do isub=1,ntypelist(iset)
c           
c           loop on sections
c           
          insubset(nz+1)=.false.
          do ilist=1,nlistz
            isect=listz(ilist)+1
            if(isect.ge.isecst.and.isect.le.isecnd)then
c               
c               find out if section is legal value and is one of type on list
c               
              insubset(isect)=.false.
              do i=1,ntype(isub)
                if(itypsec(isect).eq.listype(i,isub))
     &              insubset(isect)=.true.
              enddo
            else
c               
c               if illegal section #, change to nz; insubset will be false
c               
              listz(ilist)=nz
              isect=nz+1
            endif
            if(insubset(isect))then
              call imposn(1,isect-1,0)
              call irdsec(1,array,*99)
c               
c               get transform to apply to section
c               
              if(nfsets.le.0)then
                call xfunit(prod1,1.)
              elseif(nfsets.eq.1)then
                call xfcopy(flist1(1,1,isect+iofsxf),prod1)
              else
                call xfmult(flist1(1,1,isect+iofsxf),flist2(1,1,isect+iofsxf)
     &              ,prod1)
              endif
c               
              if(galigned)then
                call xfmult(prod1,glist(1,1,lineg+1),prod2(1,1,isect))
              else
                call xfcopy(prod1,prod2(1,1,isect))
              endif
c               
c               transform section: copy back into array
c               
              if(nfsets.gt.0.or.galigned)then
                call cubinterp(array,brray,nx,ny,nx,ny,prod2(1,1,isect)
     &              ,nx/2.,ny/2.,prod2(1,3,isect),prod2(2,3,isect),1.
     &              ,dmean,0)
                do i=1,nx*ny
                  array(i)=brray(i)
                enddo
              endif
c               
c               do sums in areas
c               
              do iarea=1,nareatot
                sum=0.
                do ipix=listbase(iarea),listbase(iarea+1)-1
                  sum=sum+array(listind(ipix))
                enddo
                areamean(isect+nz*iarea)=sum/npixarea(iarea)
              enddo
              areamean(isect)=areamean(isect+nz*2)-areamean(isect+nz)
            endif
          enddo
          ifcutoff=0
          ifsearch=0
c           
c           figure out which outliers to drop or set KEEP to true 1st time
c           
20        nkeep=0
          overmax=0.
          do ilist=1,nlistz
            isect=listz(ilist)+1
            keep(isect)=insubset(isect)
            if(ifcutoff.ne.0.and.keep(isect).and.(crit(0).gt.0.
     &          .or.crit(1).gt.0..or.crit(2).gt.0.))then
              do i=0,2
                over(i)=crit(i).gt.0.
                if(over(i))then
                  amtover=(abs(areamean(isect+nz*i)
     &                -allmean(i))/allsd(i))/crit(i)
                  overmax=max(overmax,amtover)
                  over(i)=amtover.gt.1.
                endif
              enddo
              if(ifandcrit.eq.0)then
                keep(isect)=.not.(over(0).or.over(1).or.over(2))
              else
                keep(isect)=.not.(over(0).and.over(1).and.over(2))
              endif
            endif
            if(keep(isect))nkeep=nkeep+1
          enddo
          if(ifsearch.le.0.or.nkeep.ne.nkeeplast)then
c             
c             compute mean and sd from sections not being dropped
c             
            semfac=1./sqrt(float(nkeep))
            semsum=0
            do iarea=0,nareatot
              sum=0.
              sumsq=0.
              do ilist=1,nlistz
                isect=listz(ilist)+1
c                 do isect=isecst,isecnd
                if(keep(isect))then
                  sum=sum+areamean(isect+nz*iarea)
                  sumsq=sumsq+areamean(isect+nz*iarea)**2
                endif
              enddo
              call sums_to_avgsd(sum,sumsq,nkeep,
     &            submean(iarea),subsd(iarea))
c               
c               for summing areas, scale means so low maps to 0, high maps
c               to 100; but only if normalizing
c               
              if(iarea.gt.2.and.normalize)then
                submean(iarea)=100.*(submean(iarea)-submean(1))/
     &              submean(0)
                subsd(iarea)=100.*subsd(iarea)/abs(submean(0))
              endif
              subsem(iarea)=subsd(iarea)*semfac
              if(iarea.gt.2)semsum=semsum+subsem(iarea)
c               
c               1st time through, store values in allmean, allsd
c               
              if(ifcutoff.eq.0)then
                allmean(iarea)=submean(iarea)
                allsd(iarea)=subsd(iarea)
              endif
            enddo
            semavg=semsum/(nareatot-2)
c             if(ifsearch.gt.0)print *,nkeep,factcur,semavg
          endif
c           
c           if doing a search for minimum sem
c           
          if(ifsearch.gt.0)then
c             c         
c             save factor that gives minimum
c             
            if(semavg.lt.semmin)then
              semmin=semavg
              factmin=factcur
            endif
c             
            if(nkeep.ge.fractlim*(nlistz).and.factcur.gt.0.)then
c               
c               if first time through, now set factor to maximum needed,
c               in any case, decrement factor
c               
              if(ifsearch.gt.1)factcur=max(1.,overmax)+2.*delfact
              ifsearch=1
              factcur=factcur-delfact
            else
c               
c               if # of sections drops below minimum fraction allowed, or if
c               factcur somehow becomes negative, set  to minimum factor and
c               do one more reound to finish up
c               
              factcur=factmin
              ifsearch=-1
            endif
c             
c             set new crit values and loop back
c             
            do i=0,2
              crit(i)=critbase(i)*factcur
            enddo
            nkeeplast=nkeep
            if(ifsearch.eq.-1)write(*,'(a,3f6.2)')' Best criteria are'
     &          ,crit(1),crit(2),crit(0)
            go to 20
          endif
c           
c           write out the means
c           
          if(ifcutoff.eq.0)
     &        print *,'    Values with no outliers eliminated'
          print *,'First 3 lines are high-low difference, low, and '
     &        //'high normalizing areas'
          write(*,'(a,i4,a)') ' Area,  Mean,     S.D.,   S.E.M. from',
     &        nkeep,' sections averaged'
          write(*,'(i4,3f10.3)')(ia,submean(ia),subsd(ia),subsem(ia)
     &        ,ia=0,nareatot)
          write(*,'(/,a,f8.3)')' Mean S.E.M. of summing areas =',semavg
c           
c           find out if go back for new eliminations
c           
          write(*,'(1x,a,/,a,$)')'1 to specify new criteria for'//
     &        ' eliminating outliers,',' -1 to scale criteria until'//
     &        ' lowest SEM''s are found, 0 to go on: '
          read(*,*)ifcutoff
          if(ifcutoff.gt.0)then
            write(*,'(1x,a,/,a,$)')'Enter 3 criterion #''s of SD''s '
     &          //'for dropping outlier', '   based on low area,'//
     &          ' high area, or difference (0 for none): '
            read(*,*)crit(1),crit(2),crit(0)
            write(*,'(1x,a,$)')'0 to drop if any one over, 1 to drop'
     &          //' only if all over criterion: '
            read(*,*)ifandcrit
            ifsearch=0
            go to 20
          elseif(ifcutoff.lt.0)then
            nkeeplast=nkeep
            factcur=1.
            do i=0,2
              critbase(i)=crit(i)
              crit(i)=crit(i)*factcur
            enddo
            semmin=1.e10
            ifsearch=2
            go to 20
          endif
c           
c           write out statistics
c           
          if(statout)then
            write(4,'(i7,a,i3)')nkeep,' sections averaged for set #',
     &          iset
            write(4,'(i4,3f10.3)')(ia,submean(ia),subsd(ia),subsem(ia)
     &          ,ia=3,nareatot)
          endif
c           
c           Make new averaged section and write it, if desired
c           
          if(putnewavg.or.sdimage)then
            nxmatt=nx*fracmatt
            nymatt=ny*fracmatt
            do i=1,nx*ny
              crray(i)=0.
            enddo
c             
            do ilist=1,nlistz
              isect=listz(ilist)+1
c               do isect=isecst,isecnd
              if(keep(isect))then
                call imposn(1,isect-1,0)
                call irdsec(1,array,*99)
c                 
c                 find mean density of edges
c                 
                call iclden(array,nx,ny,1,nx,1,nymatt,dmin,dmax,dmbot)
                call iclden(array,nx,ny,1,nx,ny+1-nymatt,ny,dmin,dmax,
     &              dmtop)
                call iclden(array,nx,ny,1,nxmatt,1,ny,dmin,dmax,
     &              dmleft)
                call iclden(array,nx,ny,nx+1-nxmatt,nx,1,ny,dmin,dmax,
     &              dmright)
                dfill=0.5*((dmbot+dmtop)*nx*nymatt+
     &              (dmleft+dmright)*ny*nxmatt)/(nx*nymatt+ny*nxmatt)
c                 
c                 transform section
c                 
                call cubinterp(array,brray,nx,ny,nx,ny,prod2(1,1,isect),
     &              nx/2.,ny/2.,prod2(1,3,isect),prod2(2,3,isect),1.,
     &              dfill,0)
c                 
c                 add it into crray, add squares in drray
c                 
                if(sdimage)then
                  do i=1,nx*ny
                    crray(i)=crray(i)+brray(i)
                    drray(i)=drray(i)+brray(i)**2
                  enddo
                else
                  do i=1,nx*ny
                    crray(i)=crray(i)+brray(i)
                  enddo
                endif
              endif
            enddo
c             
c             scale array down
c             
            if(normalize)then
              facnorm=100./(submean(2)-submean(1))
              pixadd=-facnorm*submean(1)
            else
              facnorm=1.
              pixadd=0.
            endif
            sclfac=facnorm/nkeep
            if(sdimage)then
              sdfac=0.
              if(nkeep.gt.1)sdfac=facnorm/sqrt(nkeep-1.)
              do i=1,nx*ny
                drray(i)=
     &              sdfac*sqrt(max(0.,drray(i)-crray(i)**2/nkeep))
              enddo
              call iclden(drray,nx,ny,1,nx,1,ny,tmin,tmax,tmean)
              sdsum=sdsum+nx*ny*tmean
              sdmin=min(sdmin,tmin)
              sdmax=max(sdmax,tmax)
              call iwrsec(3,drray)
            endif
c             
            if(putnewavg)then
              do i=1,nx*ny
                crray(i)=crray(i)*sclfac+pixadd
              enddo
c               
c               print *,'Means in areas'
c               do iarea=1,nareatot
c               sum=0.
c               do ipix=listbase(iarea),listbase(iarea+1)-1
c               sum=sum+crray(listind(ipix))
c               enddo
c               submean(iarea)=sum/npixarea(iarea)
c               enddo
c               write(*,'(8f10.3)')(submean(i),i=1,nareatot)
c               
              call iclden(crray,nx,ny,1,nx,1,ny,tmin,tmax,tmean)
              dsum=dsum+nx*ny*tmean
              dmin2=min(dmin2,tmin)
              dmax2=max(dmax2,tmax)
              call iwrsec(2,crray)
            endif
          endif
        enddo
        call imclose(1)
      enddo
      if(putnewavg)then
        call iwrhdr(2,title,-1,dmin2,dmax2,dsum/(nsetout*nx*ny))
        call imclose(2)
      endif
      if(sdimage)then
        call iwrhdr(3,title,-1,sdmin,sdmax,sdsum/(nsetout*nx*ny))
        call imclose(3)
      endif
      stop
98    stop 'ERROR READING TRANSFORMS'
99    stop 'ERROR READING IMAGE FILE'
      end






c       LISTINSIDE takes a list of NVERT vertices of a polygon specified
c       in the arrays XVERT and YVERT and, for an image size of NX by NY
c       pixels, computes a list of pixels contained in the polygon.
c       The list is returned as one-dimensional indexes in LISTIND, NPIX has
c       the number of pixels.  Whether a pixel is inside is determined by the
c       coordinates of the center of the pixel, e.g. (0.5,0.5) for pixel
c       (1,1).
c       
      subroutine listinside(xvert,yvert,nvert,nx,ny,listind,npix)
      real*4 xvert(*),yvert(*)
      integer*4 listind(*)
      logical inside
c       
c       first find min and max coordinates of polygon to reduce search
c       
      xmin=1.e10
      xmax=-1.e10
      ymin=1.e10
      ymax=-1.e10
      do i=1,nvert
        xmin=min(xmin,xvert(i))
        ymin=min(ymin,yvert(i))
        xmax=max(xmax,xvert(i))
        ymax=max(ymax,yvert(i))
      enddo
      ixstr=max(1.,xmin-0.5)
      iystr=max(1.,ymin-0.5)
      ixend=min(nx,ifix(xmax+1.5))
      iyend=min(ny,ifix(ymax+1.5))
c       
c       scan through rectangular area, finding out if each point is inside
c       and adding it to list if so
c       
      npix=0
      do iy=iystr,iyend
        do ix=ixstr,ixend
          xx=ix-0.5
          yy=iy-0.5
          if(inside(xvert,yvert,nvert,xx,yy))then
            npix=npix+1
            listind(npix)=ix+nx*(iy-1)
          endif
        enddo
      enddo
      return
      end


c       QUADLENGTH returns the average length of one side of the
c       quadrilateral with vertices in XVERT,YVERT, described by the points
c       in ISTR, ISTR+1, IEND-1, and IEND
c       
      function quadlength(xvert,yvert,istr,iend)
      real*4 xvert(*),yvert(*)
      quadlength=0.5*(sqrt((xvert(istr+1)-xvert(istr))**2+
     &    (yvert(istr+1)-yvert(istr))**2)
     &    +sqrt((xvert(iend)-xvert(iend-1))**2+
     &    (yvert(iend)-yvert(iend-1))**2))
      return
      end


      subroutine rdobjcont(iwobj,nwobj)
      integer*4 iwobj(*)
      print *,' Enter successive pairs of numbers all on one line,',
     &    ' each pair being either IMOD object and contour #s,',
     &    ' or a WIMP object # and 0'
10    call rdlist(5,iwobj,nval)
      if(mod(nval,2).ne.0)then
        print *,'You must enter an even number of values; try again'
        go to 10
      endif
      nwobj=nval/2
      do i=1,nwobj
        iobj=iobjfromcont(iwobj(2*i-1),iwobj(2*i))
        if(iobj.eq.0)then
          write(*,'(a,i5,a,i5,a)')' Object',iwobj(2*i-1),', contour',
     &        iwobj(2*i),' does not exist; try again'
          go to 10
        endif
        iwobj(i)=iobj
      enddo
      return
      end
