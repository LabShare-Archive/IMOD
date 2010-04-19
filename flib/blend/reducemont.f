*       * * * * * * * * REDUCEMONT.FOR * * * * * * *
*       
*       REDUCEMONT will reduce images in size by an integral factor (1, 2, 3,
c       etc.) using simple averaging of blocks of pixels.  It will also 
c       "recut" an image into different sized pieces.  Specifically, it can
c       take either a whole or a montaged image as input and produce a whole
c       or a montaged image as output, with the montage pieces of a desired
c       size, and with a different total size if desired.  Header origin
c       and scaling information are generated so that the new images will
c       have the same coordinate system as the original images.  Thus,
c       models will display properly on reduced image stacks.
*       
*       When you specify the maximum frame size and minimum overlap of the
*       output image, the program will pick the largest frame size less than
*       that maximum, with the smallest overlap greater than that minimum,
*       so that the resulting image will contain at least as many pixels as
*       the desired output image.  It picks a frame size that is a multiple
*       of 2 and has no prime factor greater than 19 (so that fourier
*       transforms can be run on the resulting pieces).
*       
*       Entries are as follows:
*       
*       Input image file
*       Name of input file with list of piece coordinates, if image is a
*       .  montage
*       Output image file
*       Name of new file for list of coordinates of pieces in the output file
*       
*       Data mode for output file (the default is same as mode of input)
*       
*       1 to float each section to maximum range for the data mode, 0 not to
*       
*       List of sections to be included in output file, or / to include all
*       .  sections from the input file in the output file.  Ranges may be
*       .  entered (e.g. 0-5,8-14,17-23)
*       
*       Reduction factor by which to reduce the size of the images
*       
*       Minimum and maximum X, and minimum and maximum Y coordinates that
*       .	should be included from the input image.  Enter "/" to obtain
*       .	the entire input image.
*       
*       Maximum limit on the X and Y frame size for output pieces, and
*       .	minimum limit on overlap between output pieces.  The program
*       .	will then choose new frame sizes and overlaps based on these
*       .	limits
*       
*       0 to accept the program's choices of frame size and overlap.  When
*       .	running interactively, entering 1 will allow you to loop back
*       .	and enter new minimum and maximum X and Y coordinates and a
*       .	new maximum frame and minimum overlap.  Note that on the first
*       .     two entries, the program will enforce a minimum overlap of 2;
*       .     if for some reason you want an overlap of 0, you need to loop
*       .     back so that you enter the frame size and overlap 3 times.
*       
*       To link: reducemont,shuffler,niceframe,setoverlap
*       .   plus the usual libraries.
*       
*       David Mastronarde, February 1993
c       
c       $Id$
c       Log at end of file
c       
c       
      use blendvars
      implicit none
c       
      character*80 filnam
      integer*4 mxyzin(3),nxyzst(3)/0,0,0/
      real*4 cell(6)/1.,1.,1.,0.,0.,0./
c       
c       
c       7/7/00 CER: remove the encode's; titlech is the temp space
c       
      character*80 titlech
c       
      integer*4 listz(limsect),izwant(limsect)
      character dat*9, tim*8
      real*4 title(20),delt(3)
      logical anypixels,dofast,anylinesout,xinlong
      integer*4 modepow(0:15)/8,15,8,0,0,0,0,0,0,9,10,11,12,13,14,15/
c       
      integer*4 modein,nlistz,minzpc,maxzpc,nsect,nxtotpix,nytotpix
      integer*4 maxxpiece,maxypiece,modeout,iffloat,i,minxoverlap
      integer*4 minyoverlap,ntrial,nreduce,minxwant,minywant,maxxwant
      integer*4 maxywant,nxtotwant,nytotwant,newxpieces,newypieces
      integer*4 newxtotpix,newytotpix,newxframe,newyframe,newminxpiece
      integer*4 newminypiece,ifwant
      real*4 xorig,yorig,zorig,dmin,dmax,outmin,outmax,definmin
      real*4 definmax,reduce,curinmin,curinmax,pixscale,pixadd,tsum
      integer*4 ixfrm,iyfrm,ipc,nshort,nlong,ishort,ilong,indbray
      integer*4 newuse,newpcxll,newpcyll,nxfast,nyfast,iyfast,ixfast
      integer*4 indylo,indyhi,nlinesout,indxlo,indxhi,inonepiece
      integer*4 ixinll,iyinll,inpieceno,indx,indy,iyst,iynd,ioutbase
      integer*4 ixst,ixnd,indbase,ix,iy,nsum,ilineout,ifill
      real*4 dminout,dmaxout,grandsum,sum,val,tmpsum,tmean
      integer*4 nzwant,newxoverlap,newyoverlap,kti,izsect,iwant
      integer*4 ixout,iyout,ixinpc,iyinpc,ifrevise
c       
      call setExitPrefix('ERROR: REDUCEMONT -')
c       
c       Allocate arrays now
      allocate(array(maxsiz), brray(maxbsiz),  xcray(idimc/2), xdray(idimc/2),
     &    stat = i)
      if (i .ne. 0) call exitError('ALLOCATING HUGE ARRAYS')
c
      write(*,'(1x,a,$)')'Input image file: '
      read(5,'(a)')filnam
      call imopen(1,filnam,'ro')
      call irdhdr(1,nxyzin,mxyzin,modein,dmin,dmax,dmean)
      if (nxin * nyin .gt. maxsiz) call exitError(
     &    'IMAGES TOO LARGE FOR ARRAYS')
c       
      call irtdel(1,delt)
      call irtorg(1,xorig,yorig,zorig)
c       
      write(*,'(1x,a,$)') 'Piece list file if image is a'//
     &    ' montage, otherwise Return: '
      read(*,'(a)')filnam
      call read_piece_list(filnam,ixpclist,iypclist,izpclist,
     &    npclist)
c       
c       if no pieces, set up mocklist
      if(npclist.eq.0)then
        do i=1,nzin
          ixpclist(i)=0
          iypclist(i)=0
          izpclist(i)=i-1
        enddo
        npclist=nzin
      endif
c	
      call fill_listz(izpclist,npclist,listz,nlistz)
      minzpc=listz(1)
      maxzpc=listz(nlistz)
      nsect=maxzpc+1-minzpc
c       
c       now check lists and get basic properties of overlap etc
c       
      call checklist(ixpclist,npclist,1,nxin,minxpiece,nxpieces,
     &    nxoverlap)
      call checklist(iypclist,npclist,1,nyin,minypiece,nypieces,
     &    nyoverlap)
      if(nxpieces.le.0. or. nypieces.le.0)stop 'PIECE LIST NOT GOOD'
c       
      nxtotpix=nxpieces*(nxin-nxoverlap)+nxoverlap
      nytotpix=nypieces*(nyin-nyoverlap)+nyoverlap
      maxxpiece=minxpiece+nxtotpix-1
      maxypiece=minypiece+nytotpix-1
      write(*,115)nxtotpix,'X',nxpieces,nxin,nxoverlap
      write(*,115)nytotpix,'Y',nypieces,nyin,nyoverlap
115   format(i6,' total ',a1,' pixels in',i3,' pieces of',
     &    i4, ' pixels, with overlap of',i4)
c       
c       
      write(*,'(1x,a,$)')'Output image file: '
      read(5,'(a)')filnam
      call imopen(2,filnam,'new')
      call itrhdr(2,1)
      call ialnbsym(2,0)
c       
30    write(*,'(1x,a,$)')'Name of output piece list file: '
      read(5,'(a)')filnam
      call dopen(3,filnam,'new','f')
c	open(3,file=filnam,form='formatted',status='new'
c       &	    ,carriagecontrol='list',err=30)
c       
18    modeout=modein
      write(*,'(1x,a,i2,a,$)')'Mode for output file [',modeout,']: '
      read(5,*)modeout
      if(modeout.lt.0.or.modeout.gt.15.or.
     &    (modeout.ge.3.and.modeout.le.8))then
        print *,'bad mode value'
        go to 18
      endif
      call ialmod(2,modeout)
c       
c       set up output range, and default input range and minimum.  If real
c       input, use actual input range and min; otherwise use theoretical
c       range for the input mode.  This will preserve data values if no float
c       
      outmin=0.
      outmax=2**modepow(modeout)-1.
      if(modein.eq.2)then
        definmax=dmax
        definmin=dmin
        if(modeout.eq.2)then
          outmin=dmin
          outmax=dmax
        endif
      else
        definmax=2**modepow(modein)-1.
        definmin=0.
      endif
c       
      write(*,'(1x,a,$)')
     &    '1 to float each section to maximum range, 0 not to: '
      read(5,*)iffloat
c       
c       
c       get list of sections desired, set up default as all sections
c       
      do i=1,nlistz
        izwant(i)=listz(i)
      enddo
      nzwant=nlistz
      print *,'Enter list of sections to be included in output '//
     &    'file (ranges ok)','   or / to include all sections'
      do i=1,nlistz
        izwant(i)=listz(i)
      enddo
      nzwant=nlistz
      call rdlist(5,izwant,nzwant)
c       
c       output size limited by line length in X
c       
      minxoverlap=2
      minyoverlap=2
      ntrial=0
      reduce=nreduce
32    write(*,'(1x,a,$)')'Reduction factor: '
      read(5,*)nreduce
      reduce=nreduce
      minxwant=minxpiece
      minywant=minypiece
      maxxwant=minxwant+nxtotpix-1
      maxywant=minywant+nytotpix-1
      write(*,'(1x,a,/,a,4i6,a,$)')'Enter Min X, Max X, Min Y, and'//
     &    ' Max Y coordinates to process into output section,'
     &    ,'    or / for whole input section [='
     &    ,minxwant,maxxwant,minywant,maxywant,']: '
      read(5,*)minxwant,maxxwant,minywant,maxywant
      minxwant=int(minxwant/reduce)
      minywant=int(minywant/reduce)
      maxxwant=int(maxxwant/reduce)
      maxywant=int(maxywant/reduce)
      nxtotwant=2*((maxxwant+2-minxwant)/2)
      nytotwant=2*((maxywant+2-minywant)/2)
      write(*,'(1x,a,$)')
     &    'Maximum new X and Y frame size, minimum overlap: '
      read(5,*)newxframe,newyframe,minxoverlap,minyoverlap
      if(ntrial.le.1)then			!on first 2 trials, enforce min
        minxoverlap=max(2,minxoverlap)          !overlap of 2 so things look
        minyoverlap=max(2,minyoverlap)          !nice in wimp.  After that, let
      endif					!the user have it.
      ntrial=ntrial+1
      if(newxframe.gt. maxlinelength)then
        print *,'too large in X for arrays'
        go to 32
      endif
c       
      call setoverlap(nxtotwant,minxoverlap,newxframe,2,newxpieces,
     &    newxoverlap,newxtotpix)
      call setoverlap(nytotwant,minyoverlap,newyframe,2,newypieces,
     &    newyoverlap,newytotpix)
c       
      write(*,115)newxtotpix,'X',newxpieces,newxframe,newxoverlap
      write(*,115)newytotpix,'Y',newypieces,newyframe,newyoverlap
c       
      write(*,'(1x,a,$)')
     &    '1 to revise reduction, frame size, or overlap: '
      read(5,*)ifrevise
      if(ifrevise.ne.0)go to 32	  
c       
      newminxpiece=minxwant-(newxtotpix-nxtotwant)/2
      newminypiece=minywant-(newytotpix-nytotwant)/2
      nxout=newxframe
      nyout=newyframe
      nzout=1
      dminout=1.e10
      dmaxout=-1.e10
      grandsum=0.
      call ialsiz(2,nxyzout,nxyzst)

      nzout=0
c       
c       initialize memory allocator
c       
      call clearShuffle()
      npixin=nxin*nyin
      maxload=min(maxsiz/npixin,memlim)
c       
c       loop on z: do everything within each section for maximum efficiency
c       
      do ilistz=1,nlistz
        izsect=listz(ilistz)
        write(*,'(a,i4)')' working on section #',izsect
        xinlong=nxpieces .gt. nypieces
c         
c         test if this section is wanted in output: if not, skip out
c         
        ifwant=0
        do iwant=1,nzwant
          if(izwant(iwant).eq.izsect)ifwant=1
        enddo
        if(ifwant.eq.0)go to 92
c         
c         make a map of pieces in this sections
c         
        do iyfrm=1,nypieces
          do ixfrm=1,nxpieces
            mappiece(ixfrm,iyfrm)=0
          enddo
        enddo
        do ipc=1,npclist
          if(izpclist(ipc).eq.izsect)then
            ixfrm=1+(ixpclist(ipc)-minxpiece)/(nxin-nxoverlap)
            iyfrm=1+(iypclist(ipc)-minypiece)/(nyin-nyoverlap)
            mappiece(ixfrm,iyfrm)=ipc
          endif
        enddo
c         
        call  crossvalue(xinlong,nxpieces,nypieces,nshort,nlong)
        if(iffloat.eq.0)then
          curinmin=definmin
          curinmax=definmax
        else
          curinmax=-1.e10
          curinmin=1.e10
          do ilong=nlong,1,-1
            do ishort=nshort,1,-1
              call crossvalue(xinlong,ishort,ilong,ixfrm,iyfrm)
              if(mappiece(ixfrm,iyfrm).gt.0)then
                call shuffler(mappiece(ixfrm,iyfrm),indbray)
                do i=indbray,indbray+nxin*nyin-1
                  curinmin=min(curinmin,array(i))
                  curinmax=max(curinmax,array(i))
                enddo
              endif
            enddo
          enddo
        endif
c         
c         now get output scaling factor and additive factor
c         
        pixscale=(outmax-outmin)/max(1.,curinmax-curinmin)
        pixadd=outmin-pixscale*curinmin
c         
c         look through memory list and renumber them with priorities
c         backwards from the first needed piece
c         
        newuse=jusecount-1
        do ilong=1,nlong
          do ishort=1,nshort
            call crossvalue(xinlong,ishort,ilong,ixfrm,iyfrm)
            if(mappiece(ixfrm,iyfrm).gt.0)then
              do i=1,maxload
                if(izmemlist(i).eq.mappiece(ixfrm,iyfrm))then
                  lastused(i)=newuse
                  newuse=newuse-1
                endif
              enddo
            endif
          enddo
        enddo
c         
c         GET THE PIXEL OUT
c         -  loop on output frames; within each frame loop on little boxes
c         
        call  crossvalue(xinlong,newxpieces,newypieces,nshort,nlong)
c	  
        do ilong=1,nlong
          do ishort=1,nshort
            call crossvalue(xinlong,ishort,ilong,ixout,iyout)
            write(*,'(a,2i4)')' composing frame at',ixout,iyout
            newpcyll=newminypiece+(iyout-1)*(newyframe-newyoverlap)
            newpcxll=newminxpiece+(ixout-1)*(newxframe-newxoverlap)
            anypixels=.false.
            anylinesout=.false.
            tsum=0.
c             
c             do fast little boxes
c             
            nxfast=(newxframe+(ifastsiz-1))/ifastsiz
            nyfast=(newyframe+(ifastsiz-1))/ifastsiz
c             
c             loop on boxes, get lower & upper limits in each box
c             
            do iyfast=1,nyfast
              indylo=newpcyll+(iyfast-1)*ifastsiz
              indyhi=min(indylo+ifastsiz,newpcyll+newyframe)-1
              nlinesout=indyhi+1-indylo
c               
c               fill array with dmean
c               
              do i=1,nxout*nlinesout
                brray(i)=dmean
              enddo
c		
              do ixfast=1,nxfast
                indxlo=newpcxll+(ixfast-1)*ifastsiz
                indxhi=min(indxlo+ifastsiz,newpcxll+newxframe)-1
c                 
c                 check # of edges, and prime piece number, for each corner
c                 
                dofast=.true.
                do indy=indylo,indyhi,indyhi-indylo
                  do indx=indxlo,indxhi,indxhi-indxlo
                    call checkbox(indx,indy,nreduce,inpieceno,ixinll,
     &                  iyinll)
                    if(indx.eq.indxlo.and.indy.eq.indylo)then
                      inonepiece=inpieceno
                      ixinpc=ixinll
                      iyinpc=iyinll
                    endif
                    dofast=dofast.and.inpieceno.eq.inonepiece
                  enddo
                enddo
c                 
c                 ALL ON ONE PIECE:
c                 
                if(dofast.and.inpieceno.gt.0)then
                  call shuffler(inpieceno,indbray)
                  do indy=indylo,indyhi
                    iyst=iyinpc+(indy-indylo)*nreduce
                    iynd=iyst+nreduce-1
                    ioutbase=nxout*(indy-indylo)+1-newpcxll
                    do indx=indxlo,indxhi
                      if(nreduce.eq.1)then
                        brray(ioutbase+indx)=
     &                      array(ixinpc+indx-indxlo+nxin*
     &                      (iyinpc+indy-indylo)+indbray)
                      else
                        ixst=ixinpc+(indx-indxlo)*nreduce
                        ixnd=ixst+nreduce-1
                        sum=0.
                        do iy=iyst,iynd
                          indbase=nxin*iy+indbray
                          do ix=ixst,ixnd
                            sum=sum+array(indbase+ix)
                          enddo
                        enddo
                        brray(ioutbase+indx)=sum/nreduce**2
                      endif
                    enddo
                  enddo
                  anypixels=.true.
                elseif(.not.dofast)then
c                   
c                   in or near an edge: loop on each pixel
c                   
                  do indy=indylo,indyhi
                    ioutbase=nxout*(indy-indylo)+1-newpcxll
                    do indx=indxlo,indxhi
                      call checkbox(indx,indy,nreduce,inpieceno,ixinpc,
     &                    iyinpc)
                      if(inpieceno.gt.0)then
c                         
c                         if this output pixel comes all from one input piece
c                         
                        call shuffler(inpieceno,indbray)
                        if(nreduce.eq.1)then
                          brray(ioutbase+indx)=array(ixinpc+nxin*
     &                        iyinpc+indbray)
                        else
                          iynd=iyinpc+nreduce-1
                          ixnd=ixinpc+nreduce-1
                          sum=0.
                          do iy=iyinpc,iynd
                            indbase=nxin*iy+indbray
                            do ix=ixinpc,ixnd
                              sum=sum+array(indbase+ix)
                            enddo
                          enddo
                          brray(ioutbase+indx)=sum/nreduce**2
                        endif
                        anypixels=.true.
                      elseif(inpieceno.eq.0)then
c                         
c                         but if it comes from more than one piece, need to
c                         get each pixel independently
c                         
                        ixst=indx*nreduce
                        ixnd=ixst+nreduce-1
                        iyst=indy*nreduce
                        iynd=iyst+nreduce-1
                        nsum=0
                        sum=0.
                        do iy=iyst,iynd
                          do ix=ixst,ixnd
                            call findpixel(ix,iy,inpieceno,ixinpc,
     &                          iyinpc)
                            if(inpieceno.gt.0)then
                              call shuffler(inpieceno,indbray)
                              sum=sum+array(ixinpc+nxin*(iyinpc)+
     &                            indbray)
                              nsum=nsum+1
                            endif
                          enddo
                        enddo
                        if(nsum.gt.0)then
                          brray(ioutbase+indx)=sum/nsum
                          anypixels=.true.
                        endif
                      endif
                    enddo
                  enddo
                endif
              enddo
c               
c               if any pixels have been present in this frame, write line out
c               
              if(anypixels)then
                tmpsum=0.
                do i=1,nxout*nlinesout
                  val=pixscale*brray(i)+pixadd
                  brray(i)=val
                  dminout=min(dminout,val)
                  dmaxout=max(dmaxout,val)
                  tmpsum=tmpsum+val
                enddo
                tsum=tsum+tmpsum
                ilineout=(iyfast-1)*ifastsiz
                call imposn(2,nzout,ilineout)
                call iwrsecl(2,brray,nlinesout)
c                 
c                 if this is the first time anything is written, and it
c                 wasn't the first set of lines, then need to go back and
c                 fill the lower part of frame with mean values
c                 
                if(.not.anylinesout.and.iyfast.gt.1)then
                  val=dmean*pixscale+pixadd
                  do i=1,nxout*ifastsiz
                    brray(i)=val
                  enddo
                  call imposn(2,nzout,0)
                  do ifill=1,iyfast-1
                    call iwrsecl(2,brray,ifastsiz)
                  enddo
                  tsum=tsum+val*nxout*(iyfast-1)*ifastsiz
                endif
                anylinesout=.true.
              endif
            enddo
c             
c             if any pixels present, write piece coordinates
c             
            if(anypixels)then
              grandsum=grandsum+tsum
              write(*,'(a,i5)')' wrote new frame #',nzout
              nzout=nzout+1
c               
c		12/12/03: no longer keep header up to date in case of crashes
c               
              write(3,'(2i6,i4)')newpcxll,newpcyll,izsect
            endif
c             
          enddo
        enddo
92    enddo
c       
      tmean=grandsum/(nzout*float(nxout*nyout))
      call ialsiz(2,nxyzout,nxyzst)
      call ialsam(2,nxyzout)
      cell(1)=nxout*nreduce*delt(1)
      cell(2)=nyout*nreduce*delt(2)
      cell(3)=nzout*delt(3)
      call ialcel(2,cell)
      call date(dat)
      call time(tim)
c       
c       7/7/00 CER: remove the encodes
c       
c       encode(80,90,title) nreduce,dat,tim
      write(titlech,90) nreduce,dat,tim
90    format( 'REDUCEMONT: recut and reduced by factor of',i3,
     &    t57, a9, 2x, a8 )
      read(titlech,'(20a4)')(title(kti),kti=1,20)
      call iwrhdr(2,title,1,dmin,dmax,dmean)
      close(3)
      call imclose(2)
      call exit(0)
98    call exitError('reading transforms')
      end



      subroutine checkbox(indx,indy,nreduce,inpieceno,ixinll,iyinll)
      implicit none
      integer*4 indx,indy,nreduce,inpieceno,ixinll,iyinll
      integer*4 ix,iy,inpt,ixinpc,iyinpc
c       
      ix=indx*nreduce
      iy=indy*nreduce
      call findpixel(ix,iy,inpieceno,ixinll,iyinll)
      if(nreduce.eq.1)return
c       
      call findpixel(ix+nreduce-1,iy,inpt,ixinpc,iyinpc)
      if(inpt.ne.inpieceno)go to 10
c       
      call findpixel(ix+nreduce-1,iy+nreduce-1,inpt,ixinpc,iyinpc)
      if(inpt.ne.inpieceno)go to 10
c       
      call findpixel(ix,iy+nreduce-1,inpt,ixinpc,iyinpc)
      if(inpt.eq.inpieceno)return
c       
10    inpieceno=0
      return
      end



      subroutine findpixel(ix,iy,inpieceno,ixinpc,iyinpc)
      use blendvars
      implicit none
      integer*4 ix,iy,inpieceno,ixinpc,iyinpc
C       
      integer*4 nxdel,nydel,ipcx,ipcy,idx,idy,newx,newpcx
      integer*4 newy,newpcy
      integer*4 nxtotpix,nytotpix,maxxpiece,maxypiece
c       
c       DNM 8/18/02: need to either recompute these or equivalence them
c       them to something in common
c       
      nxtotpix=nxpieces*(nxin-nxoverlap)+nxoverlap
      nytotpix=nypieces*(nyin-nyoverlap)+nyoverlap
      maxxpiece=minxpiece+nxtotpix-1
      maxypiece=minypiece+nytotpix-1
c       
      inpieceno=-1
      if(ix.lt.minxpiece.or.iy.lt.minypiece.or.
     &    ix.gt.maxxpiece.or.iy.gt.maxypiece)return
      nxdel=nxin-nxoverlap
      nydel=nyin-nyoverlap
c       
c       get piece # and coordinate within piece
c       
      ipcx=1+(ix-minxpiece)/nxdel
      ixinpc=mod(ix-minxpiece,nxdel)
      ipcy=1+(iy-minypiece)/nydel
      iyinpc=mod(iy-minypiece,nydel)
c       
c       shift into the lower piece if below the middle of overlap zone 
c       (except for 1st piece) or if past the last piece.
c       
      if((ixinpc.lt.nxoverlap/2.and.ipcx.gt.1) .or. ipcx.gt.nxpieces)then
        ipcx=ipcx-1
        ixinpc=ixinpc+nxdel
      endif
      if((iyinpc.lt.nyoverlap/2.and.ipcy.gt.1) .or. ipcy.gt.nypieces)then
        ipcy=ipcy-1
        iyinpc=iyinpc+nydel
      endif
c       
c       if outside a piece in either coordinate (negative overlap) then
c       return on blank location
c       
      if (ixinpc .ge. nxin .or. iyinpc .ge. nyin) return
c       
c       if the piece exist, done
c       
      if(mappiece(ipcx,ipcy).gt.0)go to 10
      idx=-1
      idy=-1
c       
c       set up for potential changes in X or Y
c       
      if(ixinpc.lt.nxoverlap.and.ipcx.gt.1)then
        idx=ixinpc-nxoverlap/2
        newx=ixinpc+nxdel
        newpcx=ipcx-1
      elseif(ixinpc.ge.nxdel.and.ipcx.lt.nxpieces)then
        idx=nxin-1-nxoverlap/2-ixinpc
        newx=ixinpc-nxdel
        newpcx=ipcx+1
      endif
      if(iyinpc.lt.nyoverlap.and.ipcy.gt.1)then
        idy=iyinpc-nyoverlap/2
        newy=iyinpc+nydel
        newpcy=ipcy-1
      elseif(iyinpc.ge.nydel.and.ipcy.lt.nypieces)then
        idy=nyin-1-nyoverlap/2-iyinpc
        newy=iyinpc-nydel
        newpcy=ipcy+1
      endif
c       
c       take care of the various cases
c       
      if(idx.lt.0.and.idy.lt.0)return
      if(idx.ge.0.and.idy.lt.0)then
        if(mappiece(newpcx,ipcy).le.0)return
        go to 8
      elseif(idx.lt.0.and.idy.ge.0)then
        if(mappiece(ipcx,newpcy).le.0)return
        go to 9
      else
        if(idx.le.idy)then
          if(mappiece(newpcx,ipcy).gt.0)go to 8
          if(mappiece(ipcx,newpcy).gt.0)go to 9
        else
          if(mappiece(ipcx,newpcy).gt.0)go to 9
          if(mappiece(newpcx,ipcy).gt.0)go to 8
        endif
        if(mappiece(newpcx,newpcy).le.0)return
        ipcx=newpcx
        ixinpc=newx
        go to 9
      endif
      return
c       
8     ipcx=newpcx
      ixinpc=newx
      go to 10
9     ipcy=newpcy
      iyinpc=newy
10    inpieceno=mappiece(ipcx,ipcy)
      return
      end


      subroutine crossvalue(xinlong,nxpieces,nypieces,nshort,nlong)
      implicit none
      logical xinlong
      integer*4 nxpieces,nypieces,nshort,nlong
      if(xinlong)then
        nshort=nypieces
        nlong=nxpieces
      else
        nshort=nxpieces
        nlong=nypieces
      endif
      return
      end

c       $Log$
c       Revision 3.9  2007/04/07 21:35:49  mast
c       Switched to exitError
c
c       Revision 3.8  2005/02/28 21:10:44  mast
c       Changed for shuffle changes
c	
c       Revision 3.7  2005/01/13 17:24:33  mast
c       Put in error check for image size and allowed negative overlaps
c	
c       Revision 3.6  2003/12/12 22:01:35  mast
c       remove test output
c	
c       Revision 3.5  2003/12/12 22:00:09  mast
c       Fixed problems with setting cell sizes and consolidated header entry
c       settings at end of file - no more intermediate saving of header
c	
c       Revision 3.4  2003/12/12 20:36:47  mast
c       Preserve pixel size of input file in Z
c	
c       Revision 3.3  2003/06/20 20:19:07  mast
c       Standardized error exits
c	
c       Revision 3.2  2002/08/19 05:00:47  mast
c       Corrected some declaration errors
c	
c       Revision 3.1  2002/08/19 04:23:33  mast
c       Changed to use new include file; made declarations for implicit
c       none.
c	
