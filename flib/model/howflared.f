c       HOWFLARED
c       
c       Computes various measures of microtubule flaring and curvature from a
c       model with tracings of the microtubule walls.  
c       See man page for details.
c
c       Written by David Mastronarde, 1996
c       
c       $Id$
c
      implicit none
      include 'smallmodel.inc'
      integer limd,limcol
      parameter (limd=1000,limcol=30)
      logical readSmallMod,exist,started
      character*120 modelfile,fileout,colstring,curveout
      integer*4 iobjatz(99),nwall(2),ipackstr(99)
      real*4 xobj(99),xwall(limd,2),ywall(limd,2),xt(limd),yt(limd)
      real*4 xlr(limd),arsum(3),arsqrt(3),fitmin(2),angsum(3)
      real*4 width(limd),col(limd,limcol),coltmp(limcol),finalang(2),totlen(2)
      integer*4 izsav(2,limd),icolout(limd),iobjsave(limd),isurfsave(limd)
      integer*4 itimes(max_obj_num), isurfs(max_obj_num)
      logical objused(max_obj_num)
      real*4 rmat(3,3),packxyz(3,limd),packrot(3,limd)
      real*4 xcurve(limd),ycurve(limd)
      integer*4 ncolout,ident,ierr,iobj,jobj,imodobj,imodcont,nwidth,iModel
      integer*4 jpt,izobj,natz,intobj,npack,ipt,intpack,itmp,lr,i,j,iztime
      integer*4 ndat,lop,ibas,ifflip,numModels,numStartEnds, numIDs, idStart
      integer*4 iobjLim,ncurve
      real*4 fittop,fitbot,fitmax,ymax,ymin,ytmp,xint,pol,xx,yy,xlas
      real*4 baslas,ylas,xfit,delx,delxw,dely,avwidth,sdwidth,sem,centroid(3)
      real*4 avcol,sdcol,semcol,base,slope,xleft,delang,seglen,pixsize,dxlas
      real*4 pixsizeDef,xyscal,zscale,xofs,yofs,zofs,widthDef,defscal,dylas
      real*4 fitStartDef, fitEndDef, cosang, sinang
      logical*4 useSurf,noPairs
      integer*4 getimodtimes, getimodhead, getImodSurfaces
      real*4 xinterp, acosd, atan2d, cosd, sind, atand
c       
      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetInteger,PipGetBoolean,PipGetTwoFloats
      integer*4 PipGetString,PipGetFloat, PipGetLogical
      integer*4 PipGetInOutFile, PipNumberOfEntries
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  howflared
c       
      integer numOptions
      parameter (numOptions = 11)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'output:OutputFile:FN:@columns:ColumnsToOutput:LI:@'//
     &    'point:PointOutputFile:FN:@pixel:PixelSizeDefault:F:@'//
     &    'width:WidthDefault:F:@surface:UseSurfaceNumbers:B:@'//
     &    'nopairs:NoPairsOrMarkers:B:@model:ModelFile:FNM:@'//
     &    'fit:FitTopAndBottom:FPM:@id:Identifier:IM:@help:usage:B:'
c
      pixsizeDef = 1.
      widthDef = 20.
      idStart = 1
      fitStartDef = 0.
      fitEndDef = 0.
      useSurf = .false.
      noPairs = .false.
      curveout = ' '

      print *,'Columns 1-6 are for LEFT, 7-12 for RIGHT, ',
     &    '13-18 for SUM, 19-22 LEFT, 23-26 RIGHT'
      print *,'The pattern for 1-18 is: 1 area, 2 square root area, ',
     &    '3 angular sum (radians),',' 4 width-normalized',
     &    ' area, 5 normalized root, 6 normalized angular sum'
      print *,'19/23 is total length, 20/24 is final angle (degrees),',
     &    '21/25 is angle change per unit length, ',
     &    '22/26 is average radius of curvature'
c
      call PipReadOrParseOptions(options, numOptions, 'howflared',
     &    'ERROR: HOWFLARED - ', .false., 3, 0, 1, numOptArg,
     &    numNonOptArg)
      if (PipGetInOutFile('OutputFile', 1, ' ', fileout) .ne. 0)
     &    call exitError('NO OUTPUT FILE SPECIFIED')

      if (PipGetString('ColumnsToOutput', colstring) .ne. 0)
     &    call exitError('NO LIST OF COLUMNS TO OUTPUT ENTERED')

      call parselist(colstring,icolout,ncolout)
      do i=1,ncolout
        if(icolout(i).lt.0.or.icolout(i).gt.26)call exitError(
     &      'ILLEGAL COLUMN NUMBER ENTERED')
      enddo

      ierr = PipGetFloat('PixelSizeDefault', pixsizeDef)
      ierr = PipGetFloat('WidthDefault', widthDef)
      ierr = PipGetLogical('UseSurfaceNumbers', useSurf)
      ierr = PipGetLogical('NoPairsOrMarkers', noPairs)
      if (useSurf .and. noPairs) call exitError(
     &    'YOU CANNOT ENTER BOTH -surface AND -nopairs')

      ierr = PipNumberOfEntries('ModelFile', numModels)
      if (ierr.ne. 0 .or. numModels .eq. 0)
     &    call exitError('NO INPUT MODELS ENTERED')

      ierr = PipNumberOfEntries('FitTopAndBottom', numStartEnds)
      if (numStartEnds .gt. 1 .and. numStartEnds .ne. numModels)call exitError
     &    ('NUMBER OF ENTRIES FOR START AND END VALUES MUST EQUAL 0, 1, OR'//
     &    ' THE NUMBER OF MODELS')

      ierr = PipNumberOfEntries('Identifier', numIDs)
      if (numIDs .gt. 1 .and. numIDs .ne. numModels)call exitError
     &    ('NUMBER OF IDENTIFIER VALUES MUST EQUAL 0, 1, OR'//
     &    ' THE NUMBER OF MODELS')

c       
c       open the output file
c
      call dopen(1,fileout,'new','f')
      i = ncolout+2
      if (useSurf) i = i + 1
      write(1,'(i4)')i
c       
c       Open file for curve output if present
c
      ierr = PipGetString('PointOutputFile', curveout)
      if (curveout .ne. ' ') call dopen(2, curveout, 'new', 'f')
c       
c       loop on the models and get the model name and optional fit start and
c       end and identifier for each model
c
      do iModel = 1, numModels
        ierr = PipGetString('ModelFile', modelfile)
        print *,'Processing model: ', trim(modelfile)

        if(.not.readSmallMod(modelfile)) call exitError('READING MODEL FILE')
c         
        if (iModel .le. numStartEnds) ierr = PipGetTwoFloats('FitTopAndBottom',
     &      fitStartDef, fitEndDef)
        fittop = fitStartDef
        fitbot = fitEndDef

        if (iModel .le. numIDs) ierr = PipGetInteger('Identifier', idStart)
        ident = idStart
        idStart = idStart + 1
c         
        pixsize = pixsizeDef
        defscal=1.e6
        ierr=getImodHead(xyscal,zscale,xofs,yofs,zofs,ifflip)
        if(ierr.eq.0.and.abs(xyscal-defscal)/defscal.gt.1.e-5) then
          pixsize = 1000. * xyscal
          print *,'Pixel size set from model header to', pixsize
        else
          print *,'Using default pixel size of',pixsize
        endif
c         
c         look at each object and try to find match, and find the left and
c         right walls and the connector
c         
        ierr=getImodTimes(itimes)
        ierr=getImodSurfaces(isurfs)
        do iobj=1,max_mod_obj
          objused(iobj) = .false.
        enddo
        nwidth=0
        call scale_model(0)
        
        do jobj=1,max_mod_obj
          if (npt_in_obj(jobj).gt.1 .and. .not.objused(jobj))then
            jpt=object(ibase_obj(jobj)+1)
            izobj=nint(p_coord(3,jpt))
c             
c             have a candidate for matching, now scan through objects
c             starting with this one
c             
            natz=0
            intobj=0
            npack = 0
            iztime=izobj+1
            if (itimes(jobj).gt.0) iztime=itimes(jobj)
            iobjLim = max_mod_obj
            if (noPairs) iobjLim = jobj
            do iobj=jobj,iobjLim
              if(npt_in_obj(iobj).gt.1 .and. .not.objused(iobj))then
                ipt=object(ibase_obj(iobj)+1)
c                 
c                 object matches if it is same imod object, same time, and
c                 either at an actual time or at same Z
c                 
                if (obj_color(2,iobj).eq. obj_color(2,jobj) .and.
     &              itimes(iobj).eq.itimes(jobj) .and.
     &              (.not.useSurf .or. isurfs(iobj) .eq. isurfs(jobj)) .and.
     &              (itimes(iobj).gt.0. .or.
     &              nint(p_coord(3,ipt)).eq.izobj))then
c                   
c                   store data about connector or about wall line
c                   
                  if(npt_in_obj(iobj).eq.2)then
                    intobj=iobj
                    intpack = npack + 1
                  else
                    natz=min(99,natz+1)
                    iobjatz(natz)=iobj
                    xobj(natz)=p_coord(1,ipt)
                    ipackstr(natz) = npack+1
                  endif
                  objused(iobj)=.true.
                  if (npack + npt_in_obj(iobj) .gt. limd) 
     &                call exitError('TOO MANY POINTS FOR ARRAYS')
                  do i=1,npt_in_obj(iobj)
                    ipt=object(ibase_obj(iobj)+i)
                    do j = 1, 2
                      packxyz(j, i + npack) = p_coord(j, ipt)
                    enddo
                    packxyz(3, i + npack) = zscale * p_coord(3, ipt)
                  enddo
                  npack = npack + npt_in_obj(iobj)
                endif
              endif
            enddo
            
            call objtocont(jobj, obj_color, imodobj, imodcont)
            if(natz.gt.2)then
              write(*,'(/,a,i4,a,i4,a,i4,a,i4)') 'ERROR: HOWFLARED -',
     &            natz - 1,' contours match up with object',imodobj,
     &            ', contour',imodcont,', Z/time', iztime
              call exit(1)
            endif
c             
c             got one or two: fit to a plane and get rotated points
            call planefit(packxyz, npack, centroid, rmat, packrot)
c             do i=1,npack
c             do j=1,3
c             packrot(j,i)=packxyz(j,i)
c             enddo
c             print *,(packxyz(j,i),j=1,3),(packrot(j,i),j=1,3)
c             enddo
            
c             switch so left is first, copy into wall arrays
c             
            if(natz .eq. 2 .and. xobj(2).lt.xobj(1))then
              itmp=iobjatz(1)
              iobjatz(1)=iobjatz(2)
              iobjatz(2)=itmp
              itmp=ipackstr(1)
              ipackstr(1)=ipackstr(2)
              ipackstr(2)=itmp
            endif
            ymin=1.e10
            ymax=-1.e10
            do lr=1,natz
              iobj=iobjatz(lr)
              nwall(lr)=npt_in_obj(iobj)
              do i=1,nwall(lr)
                ipt=ipackstr(lr)+i-1
                xwall(i,lr)=packrot(1,ipt)
                ywall(i,lr)=packrot(2,ipt)
                ymin=min(ymin,ywall(i,lr))
                ymax=max(ymax,ywall(i,lr))
              enddo
            enddo
c             
c             compute top and bottom limits to fit
c             
            if(fittop.eq.0.)then
              fitmax=ymax
            elseif(fittop.lt.1.)then
              fitmax=ymax-fittop*(ymax-ymin)
            else
              fitmax=ymax-fittop
            endif
            do lr=1,natz
c               
c               bottom = 0 means use the second point or the marker,
c               otherwise use a fractional distance or an absolute distance
c               
              if(fitbot.eq.0.)then
                fitmin(lr) = ywall(2,lr)
              elseif(fitbot.lt.1.)then
                fitmin(lr)=ymax-fitbot*(ymax-ymin)
              else
                fitmin(lr)=ymax-fitbot
              endif
              if (intobj.ne.0)then
                i = intpack + 2 - lr
                if (packrot(1,intpack).lt.packrot(1,intpack+1))
     &              i = intpack + lr - 1
                if (fitbot .gt. 0) then
                  fitmin(lr)=max(fitmin(lr),packrot(2,i))
                else
                  fitmin(lr)=packrot(2,i)
                endif
              endif
            enddo
c             
            ndat=0
c             print *,'fitmax & min', fitmax, (fitmin(lr), lr =1 ,natz)
            do lr=1,natz
c               
c               do fit for one wall: use all points from limit of this wall,
c               interpolate at lower limit and maybe at upper limit
c               
              do i=1,nwall(lr)
                ytmp=ywall(i,lr)
                if(ytmp.ge.fitmin(lr).and.ytmp.le.fitmax)then
                  ndat=ndat+1
                  xt(ndat)=xwall(i,lr)
                  yt(ndat)=ytmp
                  xlr(ndat)=lr-1
                endif
              enddo
c               print *,lr,' native',ndat
              ytmp=fitmin(lr)
              xint=xinterp(xwall(1,lr),ywall(1,lr),nwall(lr),ytmp)
              if(xint.ne.-99999. .and. abs(ytmp-yt(ndat)) .gt. 1.e-3)then
                ndat=ndat+1
                xt(ndat)=xint
                yt(ndat)=ytmp
                xlr(ndat)=lr-1
              endif
c               print *,lr,' bottom',ndat
              if(fittop.ne.0.)then
                ytmp=fitmax
                xint=xinterp(xwall(1,lr),ywall(1,lr),nwall(lr),ytmp)
                if(xint.ne.-99999.)then
                  ndat=ndat+1
                  xt(ndat)=xint
                  yt(ndat)=ytmp
                  xlr(ndat)=lr-1
                endif
              endif
c               print *,lr,' top',ndat
c               
c               Then add points interpolated from Y positions of other wall
c               
              if (natz .eq. 2) then
                lop=3-lr
                do j=1,nwall(lop)
                  ytmp=ywall(j,lop)
                  if(ytmp.ge.fitmin(lop).and.ytmp.le.fitmax)then
                    xint=xinterp(xwall(1,lr),ywall(1,lr),nwall(lr),ytmp)
                    if(xint.ne.-99999.)then
                      ndat=ndat+1
                      xt(ndat)=xint
                      yt(ndat)=ytmp
                      xlr(ndat)=lr-1
                    endif
                  endif
                enddo
              endif
c               print *,lr,' other',ndat
            enddo
c             
c             fit points from both walls to a single 3-parameter fit
c             
            if (ndat .eq. 0) then
              write(*,'(/,a,i4,a,i4,a,i4)')
     &            'ERROR: HOWFLARED - no points to fit to for obj',
     &            imodobj, ' surf',isurfs(jobj), ' cont',imodcont
              call exit(1)
            endif
            if (natz .eq. 2) then
              call lsfit2(yt,xlr,xt,ndat,slope,delxw,xleft)
            else
              call lsfit(yt,xt,ndat,slope,xleft,delxw)
              delxw = widthDef / pixsize
            endif
c             print *,'fit',ndat,slope,delxw,xleft,ywall(1,1)
            nwidth=nwidth+1
            width(nwidth)=delxw * pixsize
            do i = 1, 26
              col(nwidth, i) = 0.
            enddo
            arsum(2) = 0.
            angsum(2) = 0.
            do lr=1,natz
              pol=2*lr-3
              arsum(lr)=0.
              angsum(lr)=0.
              totlen(lr) = 0.
              finalang(lr) = 0.
              dxlas = -slope
              dylas = -1.
c               
c               integrate either from bottom of fit or from the zero-marker
c               
              if(intobj.eq.0)then
                ylas=fitmin(lr)
                xfit=(lr-1)*delxw+xleft
                baslas=slope*ylas+xfit
              else
                if (packrot(1,intpack).lt.packrot(1,intpack+1)) then
                  ylas=packrot(2,intpack + lr - 1)
                else
                  ylas=packrot(2,intpack + 2 - lr)
                endif
                baslas=xinterp(xwall(1,lr),ywall(1,lr),nwall(lr),ylas)
              endif
              xlas=xinterp(xwall(1,lr),ywall(1,lr),nwall(lr),ylas)
              ncurve = 1
              xcurve(1) = baslas
              ycurve(1) = ylas
              started=.false.
              if(xlas.ne.-99999.)then
                do i=1,nwall(lr)
                  yy=ywall(i,lr)
                  xx=xwall(i,lr)
                  if(yy.lt.ylas .or. started)then
c                     
c                     first time, if not a paired item, figure out polarity
c                     
                    if (.not.started .and. natz .eq. 1)
     &                  pol = sign(1., xwall(nwall(1),1) - xx)
c                     
c                     sum area between segment and baseline if still going
c                     down
c                     
                    base=baslas+slope*(yy-ylas)
                    if (yy.lt.ylas) arsum(lr)=arsum(lr)+
     &                  pol*(ylas-yy)*(xx-base+xlas-baslas)/2.
                    baslas=base
c                     
c                     sum product of angular deviation and length
c                     
                    delx=xx-xlas
                    dely=yy-ylas
                    seglen=sqrt(delx**2 + dely**2)
                    totlen(lr) = totlen(lr) + seglen
                    if (seglen.gt.1.e-6) then
                      delang = atan2d(dely, delx) - atan2d(dylas, dxlas)
                      if (delang .lt. -180.) delang = delang + 360.
                      if (delang .gt. 180.) delang = delang - 360.
                      finalang(lr) = finalang(lr) + pol * delang
                      angsum(lr) = angsum(lr) + seglen * finalang(lr) *
     &                    3.14159 / 180.
                    endif
c                     print *,itimes(jobj),lr,pol,i,delx,dely,seglen,delang,finalang(lr)
                    xlas=xx
                    ylas=yy
                    dxlas = delx
                    dylas = dely
                    started = .true.
                    ncurve = ncurve + 1
                    xcurve(ncurve) = xx
                    ycurve(ncurve) = yy
                  endif
                enddo
c                 
c                 Output points for this wall if more than one
c
                if (curveout .ne. ' ' .and. ncurve .gt. 1) then
                  delang = atand(slope)
                  cosang = pixsize * cosd(delang)
                  sinang = pixsize * sind(delang)
                  if (noPairs) then
                    write(2,111)ncurve,ident,imodobj,imodcont,iztime,lr
                  else if (useSurf) then
                    write(2,111)ncurve,ident,imodobj,isurfs(jobj),iztime,lr
                  else
                    write(2,111)ncurve,ident,imodobj,iztime,lr
                  endif
111               format(6i5)
                  do i = 1, ncurve
                    delx = xcurve(i) - xcurve(1)
                    dely = ycurve(i) - ycurve(1)
                    xx = cosang * delx - sinang * dely
                    yy = sinang * delx + cosang * dely
                    write(2,'(2f9.2)')xx,yy
                  enddo
                endif
              endif
            enddo
            arsum(3)=arsum(1)+arsum(2)
            angsum(3)=angsum(1)+angsum(2)
            do i=1,3
              arsqrt(i)=sign(sqrt(abs(arsum(i))),arsum(i))
              ibas=6*i-5
              col(nwidth,ibas)=arsum(i) * pixsize**2
              col(nwidth,ibas+1)=arsqrt(i) * pixsize
              col(nwidth,ibas+2)=angsum(i) *pixsize
            enddo
            do i = 1, natz
              ibas=19 + (i-1)*4
              col(nwidth,ibas) = totlen(i) * pixsize
              col(nwidth,ibas+1) = finalang(i)
              if (totlen(i) .gt. 0.01) col(nwidth,ibas+2) =
     &            finalang(i)/(totlen(i)*pixsize)
              if (abs(finalang(i)) .gt. 0.1) col(nwidth,ibas+3) =
     &            (totlen(i)*pixsize*360.) / (2.*3.14159*finalang(i))
            enddo
            izsav(1,nwidth)=iztime
            iobjsave(nwidth)=imodobj
            isurfsave(nwidth)=isurfs(jobj)
            if (noPairs) isurfsave(nwidth) = imodcont
c             write(1,101)ident,izsav(1,nwidth)
c             &		  ,(arsum(i),arsqrt(i),angsum(i),i=1,3)
c             101	    format(2i5,(6f10.2))
          endif
        enddo
c           
        call avgsd(width,nwidth,avwidth,sdwidth,sem)
        write(*,107)avwidth,sdwidth,nwidth
107     format('Width mean =',f8.2,', s.d. =',f8.2,', n =',i4)
        do j=1,nwidth
          do i=1,3
            ibas=6*i-5
            col(j,ibas+3)=col(j,ibas)/avwidth**2
            col(j,ibas+4)=col(j,ibas+1)/avwidth
            col(j,ibas+5)=col(j,ibas+2)/avwidth
          enddo
          do i=1,ncolout
            coltmp(i)=col(j,icolout(i))
          enddo
          if (useSurf .or. noPairs) then
            write(1,102)ident,iobjsave(j),isurfsave(j),izsav(1,j),
     &          (coltmp(i),i=1,ncolout)
102         format(4i5,(14f10.3))
          else
            write(1,103)ident,iobjsave(j),izsav(1,j),
     &          (coltmp(i),i=1,ncolout)
103         format(3i5,(14f10.3))
          endif
        enddo
        do i=1,26
          call avgsd(col(1,i),nwidth,avcol,sdcol,semcol)
          write(*,108)i,avcol,sdcol,nwidth
108       format(i3,2f10.3,i4)
        enddo
      enddo
      close(1)
      if (curveout .ne. ' ') close(2)
      end
      
      
      real*4 function xinterp(x,y,n,yint)
      implicit none
      real*4 x(*),y(*),yint
      integer*4 n,i
      
      xinterp=-99999.
      do i=1,n-1
        if(yint.ge.min(y(i+1),y(i)).and.yint.le.max(y(i+1),y(i)))then
          xinterp=x(i)+(x(i+1)-x(i))*(yint-y(i))/(y(i+1)-y(i))
          return
        endif
      enddo
      return
      end
