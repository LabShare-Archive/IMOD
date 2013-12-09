C       BSUBS.FOR has subroutines needed by BLEND only:
c       
c       READ_LIST
c       FUNCTION ONEINTRP
c       FASTINTERP
c       JOINT_TO_ROTRANS
c       EDGE_TO_ROTRANS
c       SOLVE_ROTRANS
c       FUNCTION STANDEV
c       EDGESWAP
c       DOEDGE
c       LINCOM_ROTRANS
c       RECEN_ROTRANS
c       COUNTEDGES
c       DXYDGRINTERP
c       CROSSVALUE
c       XCORREDGE
c       FIND_BEST_SHIFTS
c       findBestGradient
c       findEdgeToUse
c       getDataLimits
c       IWRBINNED
c       GETEXTRAINDENTS
c       IBINPAK
c       readExclusionModel
c       DUMPEDGE
c       
c       $Id$


c       READ_LIST get the name of a piece list file, reads the file
c       and returns the # of pieces NPCLIST, the min and max z values MINZPC
c       and MAXZPC, the piece coordinates I[XYZ]PCLIST, the optional negative
c       number within each section, NEGLIST, and a logical variable MULTINEG
c       that is true if the section 
c       
      subroutine read_list(ixpclist,iypclist,izpclist,neglist,
     &    multineg,npclist,minzpc,maxzpc,anyneg,pipinput)
c       
      implicit none
      integer*4 ixpclist(*),iypclist(*),izpclist(*),neglist(*)
      logical multineg(*),anyneg,pipinput
      integer*4 npclist,minzpc,maxzpc
      logical gotfirst,anyzero
      real*4 freinp(10)
      character*120 filnam,dummy
      character*32 errmess(4)/
     &    'error opening file',
     &    'error reading file, line',
     &    'bad number of values on line',
     &    'bad multinegative specification'/ 
      integer*4 ierr,lenact,ninp,ineg,ipc,iz,listfirst
      integer*4 PipGetString
c       
c       get file name and open file
c       
      if (pipinput) then
        if (PipGetString('PieceListInput', filnam) .ne. 0) call exitError
     &      ('NO INPUT PIECE LIST FILE SPECIFIED')
      else
        write(*,'(1x,a,$)')'name of input piece list file: '
        read(5,'(a)')filnam
      endif
      ierr=1
      npclist=0
      anyneg=.false.
c       7/14/00 CER: remove carriagecontrol for LINUX
      open(3,file=filnam,form='formatted',status='old'
     &    ,err=20)
      minzpc=100000
      maxzpc=-100000
c       
c       read each line in turn, get numbers with free format input
c       
12    ierr=2
      read(3,'(a)',err=20,end=14)dummy
      lenact=len(dummy)
      do while(dummy(lenact:lenact).eq.' '.or. dummy(lenact:lenact).eq.char(0))
        lenact=lenact-1
        if (lenact .le. 0) go to 12
      enddo
      call frefor(dummy,freinp,ninp)
      ierr=3
      if(ninp.lt.3.or.ninp.gt.4)go to 20        !error if fewer than 3 numbers
      npclist=npclist+1
      ixpclist(npclist)=freinp(1)
      iypclist(npclist)=freinp(2)
      izpclist(npclist)=freinp(3)
      neglist(npclist)=0                        !if 4th number, its a neg #
      if(ninp.eq.4)neglist(npclist)=freinp(4)
      minzpc=min(minzpc,izpclist(npclist))
      maxzpc=max(maxzpc,izpclist(npclist))
      go to 12
c       
c       now check for multinegative montaging: all pieces in a section must
c       be labeled by negative if any are
c       
14    ierr=4
      do iz=minzpc,maxzpc
        ineg=iz+1-minzpc
        multineg(ineg)=.false.
        anyzero=.false.
        gotfirst=.false.
        do ipc=1,npclist
          if(izpclist(ipc).eq.iz)then
            anyzero=anyzero.or.(neglist(ipc).eq.0)
            if(gotfirst)then
              multineg(ineg)=
     &            multineg(ineg).or.(neglist(ipc).ne.listfirst)
            else
              listfirst=neglist(ipc)
              gotfirst=.true.
            endif
          endif
        enddo
        if(multineg(ineg).and.anyzero)go to 20
        anyneg=anyneg.or.multineg(ineg)
      enddo
      close(3)
      return
20    write(*,'(1x,a,a,a,a,i6)')'ERROR: BLENDMONT - ',
     &    trim(filnam),' - ', errmess(ierr),npclist+1
      close(3)
      call exit(1)
      end



c       function ONEINTRP used interpolation in a real array CRRAY
c       (dimensions NX,NY) to find the pixel value of the point at
c       coordinates (X,Y).  Coordinates run from 0 to NX-1.  A real value
c       is returned.  If the pixel is not within the array, DFILL is returned
c       (from value in common).  IZPC is the piece number (numbered from 1)
c       The interpolation order is set from the value in common.
c       
      real*4 function oneintrp(crray,nx,ny,x,y,izpc)
c       
      use blendvars
      implicit none
      integer*4 nx, ny,izpc
      real*4 x,y
      real*4 crray(nx,ny)
      integer*4 ixp,iyp,ixpp1,ixpm1,iypp1,iypm1,ixpp2,iypp2
      real*4 dx,dy,xp,yp,v2,v4,v6,v8,v5,a,b,c,d,vmin,vmax
      real*4 dxm1,dxdxm1,fx1,fx2,fx3,fx4,dym1,dydym1,v1,v3
c       
      oneintrp=dfill
      xp=x+1.
      yp=y+1.
      if (doFields) then
        call interpolateGrid(xp, yp, fieldDx(1,1,memIndex(izpc)),
     &      fieldDy(1,1,memIndex(izpc)), lmField, nxField, nyField,
     &      xFieldstrt, yFieldStrt, xFieldIntrv, yFieldIntrv, dx, dy)
        xp = xp + dx
        yp = yp + dy
      endif
      
      if (interpOrder .le. 1) then
c         
c         Linear interpolation
c         
        IXP = XP
        IYP = YP
        IF (IXP .ge. 1 .and. IXP .le. NX .and. IYP .ge. 1 .and.
     &      IYP .le. NY) then
          DX = XP - IXP
          DY = YP - IYP
          IXPP1 = MIN(NX,IXP + 1)
          IYPP1 = MIN(NY,IYP + 1)
          oneintrp = (1. - dy) * ((1. - dx) * crray(ixp, iyp) +
     &        dx * crray(ixpp1, iyp)) +
     &        dy * ((1. - dx) * crray(ixp, iypp1) +
     &        dx * crray(ixpp1, iypp1))
        endif
      elseif (interpOrder .eq. 2) then
c         
c         Old quadratic interpolation
c         
        ixp=nint(xp)
        iyp=nint(yp)
        if(ixp.lt.1.or.ixp.gt.nx.or.iyp.lt.1.or.iyp.gt.ny)return
        DX = XP - IXP
        DY = YP - IYP
c         but if on an integer boundary already, done
        if(dx.eq.0.and.dy.eq.0.)then
          oneintrp=crray(ixp,iyp)
          return
        endif
c         
        IXPP1 = MIN(NX,IXP + 1)
        IXPM1 = MAX(1,IXP - 1)
        IYPP1 = MIN(NY,IYP + 1)
        IYPM1 = MAX(1,IYP - 1)
C         
C         Set up terms for quadratic interpolation
C         
        V2 = CRRAY(IXP, IYPM1)
        V4 = CRRAY(IXPM1, IYP)
        V5 = CRRAY(IXP, IYP)
        V6 = CRRAY(IXPP1, IYP)
        V8 = CRRAY(IXP, IYPP1)
c         find min and max of all 5 points
        vmax=max(v2,v4,v5,v6,v8)
        vmin=min(v2,v4,v5,v6,v8)
C         
        A = (V6 + V4)*.5 - V5
        B = (V8 + V2)*.5 - V5
        C = (V6 - V4)*.5
        D = (V8 - V2)*.5
C         
c         limit the new density to between the min and max of original points
        oneintrp = max(vmin,min(vmax,A*DX*DX + B*DY*DY + C*DX + D*DY + V5))
      else
c         
c         cubic interpolation
c         
        IXP = XP
        IYP = YP
        IF (IXP .ge. 1 .and. IXP .le. NX .and. IYP .ge. 1 .and.
     &      IYP .le. NY) then
          
          DX = XP - IXP
          DY = YP - IYP
          IXPP1 = MIN(NX,IXP + 1)
          IXPM1 = MAX(1,IXP - 1)
          IYPP1 = MIN(NY,IYP + 1)
          IYPM1 = MAX(1,IYP - 1)
          ixpp2 = min(nx, ixp + 2)
          iypp2 = min(ny, iyp + 2)
          
          dxm1 = dx-1.
          dxdxm1=dx*dxm1
          fx1=-dxm1*dxdxm1
          fx4=dx*dxdxm1
          fx2=1+dx**2*(dx-2.)
          fx3=dx*(1.-dxdxm1)
          
          dym1 = dy-1.
          dydym1=dy*dym1
          
          v1=fx1*crray(ixpm1,iypm1)+fx2*crray(ixp,iypm1)+
     &        fx3*crray(ixpp1,iypm1)+fx4*crray(ixpp2,iypm1)
          v2=fx1*crray(ixpm1,iyp)+fx2*crray(ixp,iyp)+
     &        fx3*crray(ixpp1,iyp)+fx4*crray(ixpp2,iyp)
          v3=fx1*crray(ixpm1,iypp1)+fx2*crray(ixp,iypp1)+
     &        fx3*crray(ixpp1,iypp1)+fx4*crray(ixpp2,iypp1)
          v4=fx1*crray(ixpm1,iypp2)+fx2*crray(ixp,iypp2)+
     &        fx3*crray(ixpp1,iypp2)+fx4*crray(ixpp2,iypp2)
          oneintrp=-dym1*dydym1*v1+(1.+dy**2*(dy-2.))*v2+
     &        dy*(1.-dydym1)*v3 +dy*dydym1*v4
          
        endif
      endif
      return
      end



c       FASTINTERP uses quadratic interpolation to fill a portion of an
c       output array DRRAY (dimensions NXD x NYD) from pixels in
c       the input real array CRRAY (dimensions NXC x NYC).  It will fill an
c       area within the coordinates from INDXLO to INDXHI and from INDYLO to
c       INDYHI, where the x coordinate of the lower left corner of the output
c       array is NEWPCXLL.  The transformation is specified by the
c       2x2 matrix AMAT and FDX and FDY, where these must be prepared so that
c       the coordinates in the input array (range 0 to NXC-1 etc) are
c       obtained simply by X = A11 * INDX + A12 * INDY + DX et (i.e. no
c       center offsets are needed).  Pixels outside the input array are
c       filled with DFILL from common.  IZPC is the piece number.
c       
      subroutine fastinterp(drray,nxd,nyd, crray,nxc,nyc,indxlo,
     &    indxhi,indylo, indyhi,newpcxll,amat,fdx, fdy,izpc)
c       
      use blendvars
      implicit none
      integer*4 nxd,nyd,nxc,nyc,indxlo, indxhi,indylo, indyhi
      integer*4 newpcxll,izpc
      real*4 drray(nxd,nyd)
      real*4 crray(nxc,nyc)
      real*4 amat(2,2),dx,dy,fdx,fdy,xbase,ybase
      integer*4 indy,iyout,indx,ixout,ixp,iyp,ixpp1,ixpm1,iypp1,iypm1
      integer*4 ixpp2,iypp2
      real*4 pixval,xp,yp,v2,v4,v6,v8,v5,a,b,c,d,vmin,vmax,rx,ry
      real*4 dxm1,dxdxm1,fx1,fx2,fx3,fx4,dym1,dydym1,v1,v3
c       
      do indy=indylo,indyhi
        iyout=indy+1-indylo
        ry = indy
        xbase = amat(1,2)*ry+fdx
        ybase = amat(2,2)*ry+fdy
        if (interpOrder .le. 1) then
c           
c           Linear interpolation
c           
          do indx=indxlo,indxhi
            ixout=indx+1-newpcxll
            pixval=dfill
            rx = indx
            if (secHasWarp) then
              call interpolateGrid(rx - 0.5, indy - 0.5, warpDx, warpDy, lmWarpX, nxWarp,
     &            nyWarp, xWarpstrt, yWarpStrt, xWarpIntrv, yWarpIntrv, dx, dy)
c              if (indy .eq. 3845) print *,'fi',indx,rx,ry,dx,dy
              rx = rx + dx
              ry = indy + dy
              xbase = amat(1,2)*ry+fdx
              ybase = amat(2,2)*ry+fdy
            endif
            xp=amat(1,1)*rx+xbase
            yp=amat(2,1)*rx+ybase
            if (doFields) then
              call interpolateGrid(xp, yp, fieldDx(1,1,memIndex(izpc)),
     &            fieldDy(1,1,memIndex(izpc)), lmField, nxField, nyField,
     &            xFieldstrt, yFieldStrt, xFieldIntrv, yFieldIntrv, dx, dy)
              xp = xp + dx
              yp = yp + dy
            endif
            IXP = XP
            IYP = YP
            IF (IXP .ge. 1 .and. IXP .lt. NXC .and. IYP .ge. 1 .and.
     &          IYP .lt. NYC) then
              DX = XP - IXP
              DY = YP - IYP
              IXPP1 = IXP + 1
              IYPP1 = IYP + 1
              pixval = (1. - dy) * ((1. - dx) * crray(ixp, iyp) +
     &            dx * crray(ixpp1, iyp)) +
     &            dy * ((1. - dx) * crray(ixp, iypp1) +
     &            dx * crray(ixpp1, iypp1))
            endif
            drray(ixout,iyout)=pixval             
          enddo
c           
        elseif (interpOrder .eq. 2) then
c           
c           Old quadratic interpolation
c           
          do indx=indxlo,indxhi
            ixout=indx+1-newpcxll
            pixval=dfill
            rx = indx
            if (secHasWarp) then
              call interpolateGrid(rx - 0.5, indy - 0.5, warpDx, warpDy, lmWarpX, nxWarp,
     &            nyWarp, xWarpstrt, yWarpStrt, xWarpIntrv, yWarpIntrv, dx, dy)
              rx = rx + dx
              ry = indy + dy
              xbase = amat(1,2)*ry+fdx
              ybase = amat(2,2)*ry+fdy
            endif
            xp=amat(1,1)*rx+xbase
            yp=amat(2,1)*rx+ybase
            if (doFields) then
              call interpolateGrid(xp, yp, fieldDx(1,1,memIndex(izpc)),
     &            fieldDy(1,1,memIndex(izpc)), lmField, nxField, nyField,
     &            xFieldstrt, yFieldStrt, xFieldIntrv, yFieldIntrv, dx, dy)
              xp = xp + dx
              yp = yp + dy
            endif
            ixp=nint(xp)
            iyp=nint(yp)
            if(ixp.lt.1.or.ixp.gt.nxc.or.iyp.lt.1.or.iyp.gt.nyc)
     &          go to 80
C             
C             Do quadratic interpolation
C             
            DX = XP - IXP
            DY = YP - IYP
            v5=crray(ixp,iyp)
c             
c             but if on an integer boundary already, done
c             
            if(dx.eq.0.and.dy.eq.0.)then
              pixval=v5
              go to 80
            endif
c             
            IXPP1 = MIN(NXC,IXP + 1)
            IXPM1 = MAX(1,IXP - 1)
            IYPP1 = MIN(NYC,IYP + 1)
            IYPM1 = MAX(1,IYP - 1)
C             
C             Set up terms for quadratic interpolation
C             
            V2 = CRRAY(IXP, IYPM1)
            V4 = CRRAY(IXPM1, IYP)
            V6 = CRRAY(IXPP1, IYP)
            V8 = CRRAY(IXP, IYPP1)
c             
c             find min and max of all 5 points
c             
            vmax=max(v2,v4,v5,v6,v8)
            vmin=min(v2,v4,v5,v6,v8)
C             
            A = (V6 + V4)*.5 - V5
            B = (V8 + V2)*.5 - V5
            C = (V6 - V4)*.5
            D = (V8 - V2)*.5
C             
c             limit the new density to between min and max of original points
c             
            pixval = max(vmin,min(vmax,
     &          A*DX*DX + B*DY*DY + C*DX + D*DY + V5))
80          drray(ixout,iyout)=pixval
          enddo
        else
c           
c           cubic interpolation
c           
          do indx=indxlo,indxhi
            ixout=indx+1-newpcxll
            pixval=dfill
            rx = indx
            if (secHasWarp) then
              call interpolateGrid(rx - 0.5, indy - 0.5, warpDx, warpDy, lmWarpX, nxWarp,
     &            nyWarp, xWarpstrt, yWarpStrt, xWarpIntrv, yWarpIntrv, dx, dy)
              rx = rx + dx
              ry = indy + dy
              xbase = amat(1,2)*ry+fdx
              ybase = amat(2,2)*ry+fdy
            endif
            xp=amat(1,1)*rx+xbase
            yp=amat(2,1)*rx+ybase
            if (doFields) then
              call interpolateGrid(xp, yp, fieldDx(1,1,memIndex(izpc)),
     &            fieldDy(1,1,memIndex(izpc)), lmField, nxField, nyField,
     &            xFieldstrt, yFieldStrt, xFieldIntrv, yFieldIntrv, dx, dy)
              xp = xp + dx
              yp = yp + dy
            endif
            IXP = XP
            IYP = YP
            IF (IXP .ge. 2 .and. IXP .lt. NXC - 1 .and. IYP .ge. 2 .and.
     &          IYP .lt. NYC - 1) then

              DX = XP - IXP
              DY = YP - IYP
              IXPP1 = IXP + 1
              IXPM1 = IXP - 1
              IYPP1 = IYP + 1
              IYPM1 = IYP - 1
              ixpp2 = ixp + 2
              iypp2 = iyp + 2
              
              dxm1 = dx-1.
              dxdxm1=dx*dxm1
              fx1=-dxm1*dxdxm1
              fx4=dx*dxdxm1
              fx2=1+dx**2*(dx-2.)
              fx3=dx*(1.-dxdxm1)
              
              dym1 = dy-1.
              dydym1=dy*dym1
              
              v1=fx1*crray(ixpm1,iypm1)+fx2*crray(ixp,iypm1)+
     &            fx3*crray(ixpp1,iypm1)+fx4*crray(ixpp2,iypm1)
              v2=fx1*crray(ixpm1,iyp)+fx2*crray(ixp,iyp)+
     &            fx3*crray(ixpp1,iyp)+fx4*crray(ixpp2,iyp)
              v3=fx1*crray(ixpm1,iypp1)+fx2*crray(ixp,iypp1)+
     &            fx3*crray(ixpp1,iypp1)+fx4*crray(ixpp2,iypp1)
              v4=fx1*crray(ixpm1,iypp2)+fx2*crray(ixp,iypp2)+
     &            fx3*crray(ixpp1,iypp2)+fx4*crray(ixpp2,iypp2)
              pixval=-dym1*dydym1*v1+(1.+dy**2*(dy-2.))*v2+
     &            dy*(1.-dydym1)*v3 +dy*dydym1*v4
c               
            endif
            drray(ixout,iyout)=pixval
          enddo
        endif
      enddo
      return
      end



c       JOINT_TO_ROTRANS uses the values in the list of NEDGE edge functions 
c       in DXGRID and DYGRID (dimensions IXDIM x IYDIM, # of values in X and
c       Y directions NXGRID x NYGRID, pixel interval between values INTXGRID
c       and INTYGRID in X and Y directions, x and y coordinates of the lower-
c       left corner of the edge in the lower and upper piece in IXPCLO,
c       IYPCLO and IXPCHI,IYPCHI) to derive a rotation/translation
c       around the center of the joint; it returns the angle in THETAMIN and
c       the translation in DXMIN and DYMIN.
c       
      subroutine joint_to_rotrans(dxgrid,dygrid,ixdim,iydim,nxgrid,
     &    nygrid,intxgrid,intygrid,ixpclo,iypclo,ixpchi,iypchi,nedge
     &    ,r)
c       
      real*4 dxgrid(ixdim,iydim,*),dygrid(ixdim,iydim,*)
      integer*4 nxgrid(*),nygrid(*),ixpclo(*),ixpchi(*),iypclo(*)
     &    ,iypchi(*)
c       
c       structure /rotrans/
c       real*4 theta,dx,dy,xcen,ycen
c       end structure
      real*4 r(6)
c       
      parameter (npnts=5000)
      real*4 x1(npnts),y1(npnts),x2(npnts),y2(npnts)
c       
c       make set of coordinate pairs in each grid, add up sums
      x1sum=0.
      x2sum=0.
      y1sum=0.
      y2sum=0.
      nn=0
      do ied=1,nedge
        do ix=1,nxgrid(ied)
          do iy=1,nygrid(ied)
            nn=nn+1
            x1(nn)=(ix-1)*intxgrid+ixpclo(ied)
            y1(nn)=(iy-1)*intygrid+iypclo(ied)
            x2(nn)=(ix-1)*intxgrid+ixpchi(ied)+dxgrid(ix,iy,ied)
            y2(nn)=(iy-1)*intygrid+iypchi(ied)+dygrid(ix,iy,ied)
            x1sum=x1sum+x1(nn)
            y1sum=y1sum+y1(nn)
          enddo
        enddo
      enddo
c       
c       get center in lower image and shift points to center around there
c       
      r(4)=x1sum/nn
      r(5)=y1sum/nn
      do i=1,nn
        x1(i)=x1(i)-r(4)
        x2(i)=x2(i)-r(4)
        y1(i)=y1(i)-r(5)
        y2(i)=y2(i)-r(5)
      enddo
      call solve_rotrans(x1,y1,x2,y2,nn,r(1),r(2),r(3))
      return
      end



c       EDGE_TO_ROTRANS uses the values in an edge function specified by
c       DXGRID and DYGRID (dimensions IXDIM x IYDIM, # of values in X and Y
c       directions NXGRID x NYGRID, pixel interval between values INTXGRID
c       and INTYGRID in X and Y directions) to derive a rotation/translation
c       around the center of the edge; it returns the angle in THETAMIN and
c       the translation in DXMIN and DYMIN.
c       
      subroutine edge_to_rotrans(dxgrid,dygrid,ixdim,iydim,nxgrid,
     &    nygrid,intxgrid,intygrid,thetamin,dxmin,dymin)
c       
      real*4 dxgrid(ixdim,iydim),dygrid(ixdim,iydim)
      parameter (npnts=1000)
      real*4 x1(npnts),y1(npnts),x2(npnts),y2(npnts)
c       
c       make a set of coordinate pairs centered around center of grid
c       
      xcen=(nxgrid-1)/2. + 1.
      ycen=(nygrid-1)/2. + 1.
      nn=0
      do ix=1,nxgrid
        do iy=1,nygrid
          nn=nn+1
          x1(nn)=(ix-xcen)*intxgrid
          y1(nn)=(iy-ycen)*intygrid
          x2(nn)=x1(nn)+dxgrid(ix,iy)
          y2(nn)=y1(nn)+dygrid(ix,iy)
        enddo
      enddo
      call solve_rotrans(x1,y1,x2,y2,nn,thetamin,dxmin,dymin)
      return
      end



c       SOLVE_ROTRANS takes a list of NN coordinate pairs in X1,Y1 and X2,Y2
c       and finds the rotation THETAMIN about the origin and translation
c       DXMIN and DYMIN that best superimposes the points
c       
      subroutine solve_rotrans(x1,y1,x2,y2,nn,thetamin,dxmin,dymin)
      real*4 x1(*),y1(*),x2(*),y2(*)
c       
c       the real way to do it
c       
      x1s=0.
      x2s=0.
      y1s=0.
      y2s=0.
      do i=1,nn
        x1s=x1s+x1(i)
        x2s=x2s+x2(i)
        y1s=y1s+y1(i)
        y2s=y2s+y2(i)
      enddo
      x1s=x1s/nn
      x2s=x2s/nn
      y1s=y1s/nn
      y2s=y2s/nn
      ssd23=0.
      ssd14=0.
      ssd13=0.
      ssd24=0.
      do i=1,nn
        ssd23=ssd23+(y1(i)-y1s)*(x2(i)-x2s)
        ssd14=ssd14+(x1(i)-x1s)*(y2(i)-y2s)
        ssd13=ssd13+(x1(i)-x1s)*(x2(i)-x2s)
        ssd24=ssd24+(y1(i)-y1s)*(y2(i)-y2s)
      enddo
      thetamin=0.
      if(abs(ssd13+ssd24).gt.1.e-20)
     &    thetamin=atand(-(ssd23-ssd14)/(ssd13+ssd24))
      costh=cosd(thetamin)
      sinth=sind(thetamin)
      dxmin=x2s-x1s*costh+y1s*sinth
      dymin=y2s-x1s*sinth-y1s*costh

c$$$    c         
c$$$    c         search for angle that minimizes the sd of differences between pairs
c$$$    c         set up for big range of angles
c$$$    c         
c$$$    thetalo=-10.
c$$$    thetahi=10.
c$$$    dtheta=1.
c$$$    cuttheta=10.
c$$$    niter=4
c$$$    sdmin=1.e10
c$$$    do iter=1,niter
c$$$    c
c$$$    c           get number of steps between lo and hi
c$$$    c
c$$$    nsteps=(thetahi-thetalo)/dtheta+1
c$$$    dtheta=(thetahi-thetalo)/(nsteps+1)
c$$$    do istep=1,nsteps
c$$$    theta=thetalo+dtheta*(istep-1)
c$$$    costh=cosd(theta)
c$$$    sinth=sind(theta)
c$$$    dxsum=0.
c$$$    dxsumsq=0.
c$$$    dysum=0.
c$$$    dysumsq=0.
c$$$    c             
c$$$    c             for each theta value, back-rotate the points in X2,y2 and compute
c$$$    c             sd of their difference from points in x1,y1
c$$$    c
c$$$    do i=1,nn
c$$$    dx=x2(i)*costh+y2(i)*sinth - x1(i)
c$$$    dy=-x2(i)*sinth+y2(i)*costh - y1(i)
c$$$    dxsum=dxsum+dx
c$$$    dxsumsq=dxsumsq+dx**2
c$$$    dysum=dysum+dy
c$$$    dysumsq=dysumsq+dy**2
c$$$    enddo
c$$$    xsd=standev(dxsum,dxsumsq,nn)
c$$$    ysd=standev(dysum,dysumsq,nn)
c$$$    c
c$$$    c             just minimize the sum of the two sd's
c$$$    c
c$$$    if(xsd+ysd.lt.sdmin)then
c$$$    sdmin=xsd+ysd
c$$$    dxmin=dxsum/nn
c$$$    dymin=dysum/nn
c$$$    thetamin=theta
c$$$    endif
c$$$    enddo
c$$$    c
c$$$    c           for next round, go between two points around minimum, cut step size
c$$$    c
c$$$    thetalo=thetamin-dtheta
c$$$    thetahi=thetamin+dtheta
c$$$    dtheta=dtheta/cuttheta
c$$$    enddo
      return
      end
c       
      function standev(sum,sumsq,nsum)
      standev=0
      if(nsum.le.1)return
      diff=sumsq-sum**2/nsum
      if(diff.gt.0.)standev=sqrt(diff/(nsum-1))
      return
      end



c       EDGESWAP manages the edge function buffers.  Given the edge # and
c       type with  IEDGE and IXY, it looks to see if that function is already
c       in the buffers, reads it in if not (replacing the edge function that
c       has the longest time since last use), and returns the index of the
c       edge in the buffers in INDBUF.
c       
      subroutine edgeswap(iedge,ixy,indbuf)
c       
      use blendvars
      implicit none
      integer*4 iedge,ixy,indbuf
      integer*4 minused,ioldest,i
c       
      indbuf=ibufedge(iedge,ixy)
      if(indbuf.eq.0)then
c         
c         find oldest
c         
        minused=jusedgct+1
        do i=1,limedgbf
          if(minused.gt.lasedguse(i))then
            minused=lasedguse(i)
            ioldest=i
          endif
        enddo
c         
c         mark oldest as no longer present, mark this as present
c         
        if(iedgbflist(ioldest).gt.0)
     &      ibufedge(iedgbflist(ioldest),ixybflist(ioldest))=0
        iedgbflist(ioldest)=iedge
        ixybflist(ioldest)=ixy
        indbuf=ioldest
        ibufedge(iedge,ixy)=indbuf
c         
c         read edge into  buffer
c         
        call readEdgeFunc(iedge, ixy, ioldest)
      endif
c       
c       mark this one as used most recently
c       
      jusedgct=jusedgct+1
      lasedguse(indbuf)=jusedgct
      return
      end


c       READEDGEFUNC actually reads in the edge function IEDGE, direction
c       IXY, to the given buffer INDBUF, and swaps bytes as necessary
c       
      subroutine readEdgeFunc(iedge, ixy, indbuf)
      use blendvars
      implicit none
      integer*4 iedge,ixy,indbuf
      integer*4 ix,iy,nxgr,nygr,idum1,idum2
c       
      if (needbyteswap.eq.0)then
        read(iunedge(ixy),rec=1+iedge)
     &      nxgr,nygr,ixgrdstbf(indbuf),ixofsbf(indbuf)
     &      ,iygrdstbf(indbuf),iyofsbf(indbuf)
     &      ,((dxgrbf(ix,iy,indbuf),dygrbf(ix,iy,indbuf)
     &      ,ddengrbf(ix,iy,indbuf),ix=1,nxgr),iy=1,nygr)
      else
        read(iunedge(ixy),rec=1+iedge)nxgr,nygr
        call convert_longs(nxgr,1)
        call convert_longs(nygr,1)
        read(iunedge(ixy),rec=1+iedge)
     &      idum1,idum2,ixgrdstbf(indbuf),ixofsbf(indbuf)
     &      ,iygrdstbf(indbuf),iyofsbf(indbuf)
     &      ,((dxgrbf(ix,iy,indbuf),dygrbf(ix,iy,indbuf)
     &      ,ddengrbf(ix,iy,indbuf),ix=1,nxgr),iy=1,nygr)
        call convert_longs(ixgrdstbf(indbuf),1)
        call convert_longs(ixofsbf(indbuf),1)
        call convert_longs(iygrdstbf(indbuf),1)
        call convert_longs(iyofsbf(indbuf),1)
        do iy=1,nygr
          call convert_floats(dxgrbf(1,iy,indbuf),nxgr)
          call convert_floats(dygrbf(1,iy,indbuf),nxgr)
          call convert_floats(ddengrbf(1,iy,indbuf),nxgr)
        enddo
      endif
      if (ifskipEdge(iedge,ixy) .gt. 0) then
        dxgrid(1:nxgr,1:nygr) = 0.
        dygrid(1:nxgr,1:nygr) = 0.
        ddengrid(1:nxgr,1:nygr) = 0.
      endif
c      print *,'read all of edge',ixy,iedge,ixpclist(ipiecelower(iedge,ixy)),
c     &    iypclist(ipiecelower(iedge,ixy)),nxgr,nygr
      nxgrbf(indbuf)=nxgr
      nygrbf(indbuf)=nygr
      intxgrbf(indbuf)=intgrcopy(ixy)
      intygrbf(indbuf)=intgrcopy(3-ixy)
      return
      end


c       DOEDGE "does" edge # IEDGE, whose direction is given by IXY.  It
c       looks to see if this edge is on a joint between negatives; if so it
c       composes the whole list of edges along that joint and arranges to
c       find the edge functions for all of those edges, from the center
c       outward.  Edge functions are found, then smoothed using the other
c       arguments as parameters, then written to the appropriate edge file
c       
      subroutine doedge(iedge,ixy,edgedone,sdcrit,devcrit,nfit,
     &    norder,nskip,docross,xcreadin,xclegacy,edgedispx,edgedispy,idimedge)
c       
      use blendvars
      implicit none
c       real*4 array(*)
      integer*4 nfit(2),nskip(2)
      logical docross,xcreadin,xclegacy
      integer*4 iedge,ixy,norder,idimedge
      real*4 sdcrit,devcrit
c       
      logical edgedone(idimedge,2)
      real*4 edgedispx(idimedge,2),edgedispy(idimedge,2)
c       
      integer limpneg
      parameter (limpneg=20)
      integer*4 multcoord(limpneg),multedge(limpneg),multmp(limpneg)
     &    ,mcotmp(limpneg),igrstr(2),igrofs(2)
c       
      integer*4 intxgrid,intygrid,nmult,intscan,ipclo,ipcup,ipc,mltco
      integer*4 i,j,itmp,midcoord,mindiff,imult,imid,middone,indlow
      integer*4 indup,ixdisp,iydisp,ixdispmid,iydispmid,lastedge
      integer*4 lastxdisp,lastydisp,idiff,jedge,nxgr,nygr,ix,iy,indentXcorr
      real*4 xdisp,ydisp,theta,edgedx,edgedy,dxmid,dymid,xdispl,ydispl
      real*4 costh,sinth,xrel,yrel,thetamid,delIndent(2)
      integer*4 indentUse(2), limitLo, limitHi, limitLo2, limitHi2
      real*4 cosd,sind
      real*8 wallstart, walltime
c       
c       make list of edges to be done
c       
      intxgrid=intgrid(ixy)
      intygrid=intgrid(3-ixy)
      nmult=1
      multedge(1)=iedge
      intscan=6
      ipclo=ipiecelower(iedge,ixy)
      ipcup=ipieceupper(iedge,ixy)
      ipcBelowEdge = ipclo
      if(neglist(ipclo).ne.neglist(ipcup))then
c         
c         if edge is across a negative boundary, need to look for all such
c         edges and add them to list
c         
        nmult=0
        intscan=9
        do i=1,nedge(ixy)
          ipc=ipiecelower(i,ixy)
          if(izpclist(ipclo).eq.izpclist(ipc).and.
     &        neglist(ipclo).eq. neglist(ipc).and.
     &        neglist(ipcup).eq.neglist(ipieceupper(i,ixy)))then
            nmult=nmult+1
            multedge(nmult)=i
c             get coordinate of edge in ortho direction
            if(ixy.eq.1)then
              mltco=iypclist(ipc)
            else
              mltco=ixpclist(ipc)
            endif
            multcoord(nmult)=mltco
          endif
        enddo
c         
c         order list to go out from center of edge.  GROSS.. first order it
c         
        do i=1,nmult-1
          do j=i,nmult
            if(multcoord(i).gt.multcoord(j))then
              itmp=multcoord(i)
              multcoord(i)=multcoord(j)
              multcoord(j)=itmp
              itmp=multedge(i)
              multedge(i)=multedge(j)
              multedge(j)=itmp
            endif
          enddo
        enddo
        midcoord=(multcoord(nmult)+multcoord(1))/2
c         
c         find element closest to center
c         
        mindiff=100000
        do i=1,nmult
          idiff=abs(multcoord(i)-midcoord)
          if(idiff.lt.mindiff)then
            mindiff=idiff
            imid=i
          endif
        enddo
c         
c         set up order from there to top then back from middle to bottom
c         
        imult=0
        do i=imid,nmult
          imult=imult+1
          multmp(imult)=multedge(i)
          mcotmp(imult)=multcoord(i)
        enddo
        middone=imult
        do i=imid-1,1,-1
          imult=imult+1
          multmp(imult)=multedge(i)
          mcotmp(imult)=multcoord(i)
        enddo
        do i=1,nmult
          multedge(i)=multmp(i)
          multcoord(i)=mcotmp(i)
        enddo
      endif
c       
c       finally ready to set up to get edge
c       
      do imult=1,nmult
        jedge=multedge(imult)
c         
        call shuffler(ipiecelower(jedge,ixy),indlow)
        call shuffler(ipieceupper(jedge,ixy),indup)
c         
        if(imult.eq.1)then
c           
c           for first time, set these parameters to
c           their default values with 0 offsets
c           
          ixdisp=0
          iydisp=0
          if(docross)then
            if(xcreadin)then
              xdisp=edgedispx(jedge,ixy)
              ydisp=edgedispy(jedge,ixy)
            else
              call getExtraIndents(ipiecelower(jedge,ixy),
     &            ipieceupper(jedge,ixy), ixy, delIndent)
              indentXcorr = 2
              if (delIndent(ixy) .gt. 0. .and. ifillTreatment .eq. 1)
     &            indentXcorr = int(delIndent(ixy)) + 2
              if (ifskipEdge(jedge,ixy) .gt. 0) then
                xdisp = 0.
                ydisp = 0.
              else
                call xcorredge(array(indlow),array(indup),
     &              ixy,xdisp,ydisp,xclegacy,indentXcorr)
              endif
              edgedispx(jedge,ixy)=xdisp
              edgedispy(jedge,ixy)=ydisp
            endif
            ixdisp=nint(xdisp)
            iydisp=nint(ydisp)
c            write(*,'(1x,a,2i4,a,2i5)') char(ixy+ichar('W'))//' edge, pieces'
c     &          ,ipiecelower(jedge,ixy),ipieceupper(jedge,ixy),
c     &          '  ixydisp:',ixdisp,iydisp
          endif
          ixdispmid=ixdisp
          iydispmid=iydisp
c           
        else
          if(imult.eq. middone+1)then
c             
            theta=thetamid                      !at midway point, restore
            edgedx=dxmid                        !values from first (middle)
            edgedy=dymid                        !edge
            lastedge=multedge(1)
            lastxdisp=ixdispmid
            lastydisp=iydispmid
          else
            call edge_to_rotrans(dxgrid,dygrid,ixgdim,iygdim,nxgr,
     &          nygr,intxgrid,intygrid,theta,edgedx,edgedy)
            if(imult.eq.2)then
              thetamid=theta                    !if that was first edge, save
              dxmid=edgedx                      !the value
              dymid=edgedy
            endif
            lastedge=multedge(imult-1)
          endif
c           
c           find displacement of center of next edge relative to center of
c           last edge.  First get x/y displacements between edges
c           
          xdispl=ixpclist(ipieceupper(jedge,ixy))-
     &        ixpclist(ipieceupper(lastedge,ixy))
          ydispl=iypclist(ipieceupper(jedge,ixy))-
     &        iypclist(ipieceupper(lastedge,ixy))
          costh=cosd(theta)
          sinth=sind(theta)
c           rotate vector by theta and displace by dx, dy; the movement in
c           the tip of the displacement vector is the expected relative
c           displacement between this frame and the last
          xrel=xdispl*costh - ydispl*sinth + edgedx - xdispl
          yrel=xdispl*sinth + ydispl*costh + edgedy - ydispl
c           add pixel displacement of last frame to get total expected pixel
c           displacement of this frame
          ixdisp=nint(xrel)+lastxdisp
          iydisp=nint(yrel)+lastydisp
        endif
c         
c         Determine extra indentation if distortion corrections
c         
        call getExtraIndents(ipiecelower(jedge,ixy), ipieceupper(jedge,ixy),
     &      ixy, delIndent)
c         if (doFields) write(*,'(1x,a,2i4,a,2f5.1)')
c         &           char(ixy+ichar('W'))//' edge, pieces'
c         &           ,ipiecelower(jedge,ixy),ipieceupper(jedge,ixy),
c         &           '  extra indents:',delIndent(1),delIndent(2)
c         
        indentUse(1) = indent(1) + nint(delIndent(1))
        indentUse(2) = indent(2) + nint(delIndent(2))
c         
c         Determine data limits for edge in long dimension if flag set
        limitLo = 0
        limitHi = 0
        if (limitData) then
          call getDataLimits(ipiecelower(jedge,ixy), 3-ixy, 2, limitLo,
     &        limitHi)
          call getDataLimits(ipieceupper(jedge,ixy), 3-ixy, 1, limitLo2,
     &        limitHi2)
          limitLo = max(limitLo, limitLo2)
          limitHi = min(limitHi, limitHi2)
        endif
c          
        call setgridchars(nxyzin,noverlap,iboxsiz,indentUse,intgrid,
     &      ixy,ixdisp,iydisp,limitLo,limitHi,nxgr,nygr,igrstr,igrofs)
        if (nxgr .gt. nxgrid(ixy) .or. nygr .gt. nygrid(ixy)) call exitError(
     &      'ONE GRID HAS MORE POINTS THAN ORIGINALLY EXPECTED')
        lastxdisp=ixdisp
        lastydisp=iydisp
c         
c        write(*,'(1x,a,2i4,a,2i4,a,2i5,a,2i4)')
c     &      char(ixy+ichar('W'))//' edge, pieces'
c     &      ,ipiecelower(jedge,ixy),ipieceupper(jedge,ixy),
c     &      '  ngrid:',nxgr,nygr,'  start lower:',igrstr,
c     &      '  upper:',igrofs
        if (ifskipEdge(jedge, ixy) .eq. 0) then
          wallstart = walltime()
          call findedgefunc(array(indlow),array(indup),nxin,nyin,
     &        igrstr(1),igrstr(2),igrofs(1),igrofs(2),nxgr,nygr,
     &        intxgrid,intygrid,iboxsiz(ixy),iboxsiz(3-ixy),intscan,
     &        dxgrid, dygrid,sdgrid, ddengrid,ixgdim,iygdim)
c           
          if (izUnsmoothedPatch .ge. 0) then
            do iy=1,nygr
              do ix=1,nxgr
                write(10,'(3i6,3f9.2,f12.4)')igrstr(1)+(ix-1)*intxgrid,
     &              igrstr(2)+(iy-1)*intygrid,izUnsmoothedPatch,dxgrid(ix,iy),
     &              dygrid(ix,iy),0.,sdgrid(ix,iy)
              enddo
            enddo
            izUnsmoothedPatch = izUnsmoothedPatch + 1
          endif
          call smoothgrid(dxgrid,dygrid,sdgrid,ddengrid,ixgdim,
     &        iygdim,nxgr,nygr,sdcrit, devcrit,nfit(ixy),nfit(3-ixy),
     &        norder, nskip(ixy),nskip(3-ixy))
c          write(*,'(a,f10.6)')'Edge function time',walltime()-wallstart
          if (izSmoothedPatch .ge. 0) then
            do iy=1,nygr
              do ix=1,nxgr
                write(11,'(3i6,3f9.2,f12.4)')igrstr(1)+(ix-1)*intxgrid,
     &              igrstr(2)+(iy-1)*intygrid,izSmoothedPatch,dxgrid(ix,iy),
     &              dygrid(ix,iy),0.,sdgrid(ix,iy)
              enddo
            enddo
            izSmoothedPatch = izSmoothedPatch + 1
          endif
        else
          dxgrid(1:nxgr,1:nygr) = 0.
          dygrid(1:nxgr,1:nygr) = 0.
          ddengrid(1:nxgr,1:nygr) = 0.
        endif
c         
c$$$        xrel = 0.
c$$$        yrel = 0.
c$$$        xdispl = 0.
c$$$        do ix = 1,nxgr
c$$$          do iy = 1,nygr
c$$$            costh = sqrt(dxgrid(ix,iy)**2 + dygrid(ix,iy)**2)
c$$$            xrel = xrel + costh
c$$$            yrel = max(yrel, costh)
c$$$            xdispl = xdispl + ddengrid(ix,iy)
c$$$          enddo
c$$$        enddo
c$$$        xdispl = xdispl /(nxgr * nygr)
c$$$        write(*,'(1x,a,2i4,a,2f6.2,a,f8.1)')
c$$$     &      char(ixy+ichar('W'))//' edge, pieces'
c$$$     &      ,ipiecelower(jedge,ixy),ipieceupper(jedge,ixy),
c$$$     &      '  mean, max vector:',xrel/(nxgr*nygr), yrel,' den diff:',xdispl
        
        if (needByteSwap .eq. 0) then
          write(iunedge(ixy),rec=jedge+1)nxgr,nygr,(igrstr(i),igrofs(i)
     &        ,i=1,2) ,((dxgrid(ix,iy),dygrid(ix,iy),
     &        ddengrid(ix,iy),ix=1,nxgr),iy=1,nygr)
        else
c           
c           In case there were incomplete edges, be able to write swapped
          ixdisp = nxgr
          iydisp = nygr
          call convert_longs(ixdisp, 1)
          call convert_longs(iydisp, 1)
          call convert_longs(igrstr, 2)
          call convert_longs(igrofs, 2)
          do iy = 1, nygr
            call convert_floats(dxgrid(1,iy), nxgr)
            call convert_floats(dygrid(1,iy), nxgr)
            call convert_floats(ddengrid(1,iy), nxgr)
          enddo
          write(iunedge(ixy),rec=jedge+1)ixdisp, iydisp,(igrstr(i),igrofs(i)
     &        ,i=1,2) ,((dxgrid(ix,iy),dygrid(ix,iy),
     &        ddengrid(ix,iy),ix=1,nxgr),iy=1,nygr)
        endif
c         
        edgedone(jedge,ixy)=.true.
c         
c         Write records for any edges not done yet
        ixdisp = -1
        if (needByteSwap .ne. 0) call convert_longs(ixdisp, 1)
        do ix = lastWritten(ixy) + 1, jedge - 1
          if (.not.edgedone(ix,ixy))
     &        write(iunedge(ixy),rec=ix+1)ixdisp, ixdisp
        enddo
        lastWritten(ixy) = jedge
      enddo
      return
      end



c       LINCOM_ROTRANS forms a linear combination of two rotation/
c       translations R1 and R2, with weights W1 and W2, shifts the result to
c       the center of R2, and puts the result in S
c       
      subroutine lincom_rotrans(r1,w1,r2,w2,s)
c       
c       structure /rotrans/
c       real*4 theta,dx,dy,xcen,ycen
c       end structure
      real*4 r1(6),r2(6),rt1(6),rt2(6),s(6)
c       
      call recen_rotrans(r1,r2(4),r2(5),rt1)
      call xfcopy(r2,rt2)
      rt2(1)=w1*rt1(1) + w2*r2(1)
      rt2(2)=w1*rt1(2) + w2*r2(2)
      rt2(3)=w1*rt1(3) + w2*r2(3)
      call xfcopy(rt2,s)
      return
      end



C       RECEN_ROTRANS shift the rotation/translation R to the new center
c       XCENEW,YCENEW and returns the result in S
c       
      subroutine recen_rotrans(r,xcenew,ycenew,s)
c       
c       structure /rotrans/
c       real*4 theta,dx,dy,xcen,ycen
c       end structure
      real*4 r(6),s(6)
c       
      sinth=sind(r(1))
      cosm1=cosd(r(1))-1.
      s(1)=r(1)
      s(2)=r(2)+cosm1*(xcenew-r(4))-sinth*(ycenew-r(5))
      s(3)=r(3)+cosm1*(ycenew-r(5))+sinth*(xcenew-r(4))
      s(4)=xcenew
      s(5)=ycenew
      return
      end

c$$$    subroutine recen_xform(f,dxcen,dycen,g)
c$$$    structure /xform/
c$$$    real*4 a(2,2),dx,dy
c$$$    end structure
c$$$    record /xform/ f,g
c$$$    g=f
c$$$    g.dx=f.dx+(f.a(1,1)-1.)*dxcen + f.a(1,2)*dycen
c$$$    g.dy=f.dy+(f.a(2,2)-1.)*dycen + f.a(2,1)*dxcen
c$$$    return
c$$$    end



c       COUNTEDGES takes coordinate INDX, INDY in the output image, converts
c       it to XG,YG with the inverse of optional g transform, and analyses
c       the edges and pieces that the point is in, or at least near
c       
      subroutine countedges(indx,indy,xg,yg, useEdges)
c       
      use blendvars
      implicit none
      integer*4 indx,indy
      real*4 xg,yg
c       
      logical edgeonlist,needcheck(maxInPc,2),ngframe, useEdges, inLimit(2)
      real*4 xycur(2)
      integer*4 movedPiece(4), k, axisin, maxInside, ipcCross
      integer*4 ixframe,iyframe,ipc,ixfrm,iyfrm, limitLo, j, lookxfr, lookyfr
      integer*4 indinp,newedge,newpiece,iflo,listno,ixy,i, idSearch, limitHi
      real*4 xtmp,xframe,yframe,ytmp,xbak,ybak,distmin,dist
      real*4 xpcCross, ypcCross
      logical b3dxor
c       
      numpieces=0
      ipcCross = 0
      numedges(1)=0
      numedges(2)=0
      idSearch = 1
c       
c       get frame # that it is nominally in: actual one or nearby valid frame
c       
      xg=indx
      yg=indy
      if (secHasWarp) then
        call interpolateGrid(xg - 0.5, yg - 0.5, warpDx, warpDy, lmWarpX, nxWarp, nyWarp,
     &      xWarpStrt, yWarpStrt, xWarpIntrv, yWarpIntrv, xtmp, ytmp)
c        if (indy .eq. 3845) print *,'ce',indx,xg,yg,xtmp,ytmp
        xg = xg + xtmp
        yg = yg + ytmp
      endif
      if(dogxforms)then
        xtmp=xg
        xg=ginv(1,1)*xtmp+ginv(1,2)*yg+ginv(1,3)
        yg=ginv(2,1)*xtmp+ginv(2,2)*yg+ginv(2,3)
      endif
      xframe=(xg-minxpiece-nxoverlap/2)/(nxin-nxoverlap)
      yframe=(yg-minypiece-nyoverlap/2)/(nyin-nyoverlap)
      ixframe=xframe+1.                         !truncation gives proper frame
      iyframe=yframe+1.
      ngframe=ixframe.lt.1.or.ixframe.gt.nxpieces.or. iyframe.lt.1.or.
     &    iyframe.gt.nypieces.or. mappiece(min(nxpieces,max(1,ixframe)),
     &    min(nypieces,max(1,iyframe))).eq.0
      if(multng)then
c         
c         if there are multineg h's (including piece shifting!), need to make
c         sure point is actually in frame, but if frame no good, switch to
c         nearest frame first 
        if (ngframe) call findNearestPiece(ixframe, iyframe)
        ipc=mappiece(ixframe,iyframe)
        call positionInPiece(xg, yg, ipc, xbak, ybak)
        ngframe=xbak.lt.0.or.xbak.gt.nxin-1.or. ybak.lt.0.or.ybak.gt.nyin-1

        if (ngframe) then
c           
c           Use the error to switch to a nearby frame as center of search
c           
          xtmp = xbak + ixpclist(ipc)
          ytmp = ybak + iypclist(ipc)
          ixframe=(xtmp-minxpiece-nxoverlap/2)/(nxin-nxoverlap)+1.
          iyframe=(ytmp-minypiece-nyoverlap/2)/(nyin-nyoverlap)+1.
c           
c           If still not in a frame, start in the nearest and expand the search
c
          if (ixframe.lt.1.or.ixframe.gt.nxpieces.or. iyframe.lt.1.or.
     &        iyframe.gt.nypieces.or. mappiece(min(nxpieces,max(1,ixframe)),
     &        min(nypieces,max(1,iyframe))).eq.0) then
            call findNearestPiece(ixframe, iyframe)
            idSearch = 2
          endif
        else
c         
c           but even if frame is good, if in a corner, switch to the 9-piece
c           search to start with most interior point
c         
          ngframe= (xbak.lt.edgelonear(1).and.ybak.lt.edgelonear(2)).or.
     &        (xbak.lt.edgelonear(1).and.ybak.gt.edgehinear(2)).or.
     &        (xbak.gt.edgehinear(1).and.ybak.lt.edgelonear(2)).or.
     &        (xbak.gt.edgehinear(1).and.ybak.gt.edgehinear(2))
        endif
      endif
c       
c       if not a good frame, look in square of nine potential pieces,
c       switch to the one that the point is a minimum distance from
c       
      if(ngframe)then
        distmin=1.e10
c         
c         continue loop to find most piece where point is most interior,
c         not just to find the first one.  This should be run rarely so
c         it is not a big drain
c         

        ixfrm=max(1,min(nxpieces,ixframe-idSearch))
        do while( ixfrm.le.min(nxpieces,max(1,ixframe+idSearch)))
          iyfrm=max(1,min(nypieces,iyframe-idSearch))
          do while( iyfrm.le.min(nypieces,max(1,iyframe+idSearch)))
            ipc=mappiece(ixfrm,iyfrm)
            if(ipc.ne.0)then
c               
c               get real coordinate in piece, adjusting for h if present
c               
              call positionInPiece(xg, yg, ipc, xtmp, ytmp)
c               
c               distance is negative for a piece that point is actually in;
c               it is negative of distance from nearest edge
c               
              dist=max(-xtmp,xtmp-(nxin-1),-ytmp,ytmp-(nyin-1))
              if(dist.lt.distmin)then
                distmin=dist
                minxframe=ixfrm
                minyframe=iyfrm
              endif
            endif
            iyfrm=iyfrm+1
          enddo
          ixfrm=ixfrm+1
        enddo
c
c         return if no pieces in this loop.  This seems odd but there are
c         weird edge effects if it returns just because it is not inside any
        if (distmin .eq. 1.e10) return
        ixframe=minxframe
        iyframe=minyframe
      endif
c       
c       initialize list of pieces with this piece # on it, then start looping
c       over the pieces present in the list
c       Keep track of min and max frame numbers on list
      numpieces=1
      inpiece(1)=mappiece(ixframe,iyframe)
      indinp=1
      needcheck(1,1)=.true.
      needcheck(1,2)=.true.
      minxframe = ixframe
      maxxframe = ixframe
      minyframe = iyframe
      maxyframe = iyframe
      inpxframe(1) = ixframe
      inpyframe(1) = iyframe
      call positionInPiece(xg, yg, inpiece(1), xinpiece(1), yinpiece(1))
c
      do while (indinp.le.numpieces)
c         
c         come into this loop looking at a piece onlist; use true
c         coordinates in piece to see if point is near/in an edge to another
c         
        ipc=inpiece(indinp)
        xycur(1) = xinpiece(indinp)
        xycur(2) = yinpiece(indinp)
c        print *,'looking at',ipc, xycur(1),xycur(2)
c         
c         check the x and y directions to see if point is near an edge
c         
        do ixy=1,2
          do iflo = 0, 1
            if(needcheck(indinp,ixy))then
              newedge=0
              if(iflo .eq. 1 .and. xycur(ixy).lt.edgelonear(ixy).and.
     &            iedgelower(ipc,ixy).gt.0)then
                newedge=iedgelower(ipc,ixy)
                newpiece=ipiecelower(newedge,ixy)
              endif
              if(iflo .eq. 0 .and. xycur(ixy).gt.edgehinear(ixy).and.
     &            iedgeupper(ipc,ixy).gt.0)then
                newedge=iedgeupper(ipc,ixy)
                newpiece=ipieceupper(newedge,ixy)
              endif
c               
c               But if there are 4 pieces, make sure this picks up an edge
c               between this piece and an existing piece
              if (newedge .eq. 0 .and. numpieces .ge. 4) then
                lookxfr = inpxframe(indinp) + (2 - ixy) * (1 - 2 * iflo)
                lookyfr = inpyframe(indinp) + (ixy - 1) * (1 - 2 * iflo)
                do i = 1, numpieces
                  if (inpxframe(i) .eq. lookxfr .and. 
     &                inpyframe(i) .eq. lookyfr) then
                    newpiece = inpiece(i)
                    if (iflo .eq. 0) newedge = iedgeupper(ipc,ixy)
                    if (iflo .eq. 1) newedge = iedgelower(ipc,ixy)
                    exit
                  endif
                enddo
              endif
c             
c               if check picked up a new edge, see if edge is on list already
c             
              if(newedge.ne.0)then
                edgeonlist=.false.
                do i=1,numedges(ixy)
                  edgeonlist=edgeonlist.or.(inedge(i,ixy).eq.newedge)
                enddo
c                 
c                 if not, add it, and the implied piece, to list
c               
                if(.not.edgeonlist)then
                  listno=0
                  do i=1,numpieces
                    if(newpiece.eq.inpiece(i))listno=i
                  enddo
                  if(listno.eq.0)then
c                   
c                     but if adding a new piece, check for point actually in
c                     piece first
c                     
                    call positionInPiece(xg, yg, newpiece, xbak, ybak)
                    if(xbak.ge.0.and.xbak.le.nxin-1.and.
     &                  ybak.ge.0.and.ybak.le.nyin-1)then
                      numpieces=numpieces+1
                      inpiece(numpieces)=newpiece
                      xinpiece(numpieces) = xbak
                      yinpiece(numpieces) = ybak
c                       
c                       Get new frame numbers and maintain min/max
                      if (ixy .eq. 1) then
                        inpxframe(numpieces) = inpxframe(indinp) + 1 - 2 * iflo
                        inpyframe(numpieces) = inpyframe(indinp)
                      else
                        inpxframe(numpieces) = inpxframe(indinp)
                        inpyframe(numpieces) = inpyframe(indinp) + 1 - 2 * iflo
                      endif
                      minxframe = min(minxframe, inpxframe(numpieces))
                      minyframe = min(minyframe, inpyframe(numpieces))
                      maxxframe = max(maxxframe, inpxframe(numpieces))
                      maxyframe = max(maxyframe, inpyframe(numpieces))
c                       
c                       If there are crossed limits to edges, then still need
c                       to check this axis for this new piece
                      needcheck(numpieces,ixy) =
     &                    edgelonear(ixy) .ge. edgehinear(ixy)
                      needcheck(numpieces,3-ixy)=.true.
                      listno=numpieces
                    else if (numpieces .eq. 1 .and. anyDisjoint(
     &                    inpxframe(indinp), inpyframe(indinp))) then
c                       
c                       If the point was NOT in this overlapping piece, and
c                       any corners are disjoint, find cross-corner piece
c                       Look across upper and lower edges on the other axis
c                       from the rejected piece to see if point in other piece
                      newedge = iedgelower(newpiece, 3-ixy)
                      if (newedge .ne. 0) then
                        call positionInPiece(xg, yg,
     &                      ipiecelower(newedge,3-ixy), xbak, ybak)
                        if (xbak.ge.0.and.xbak.le.nxin-1.and.
     &                      ybak.ge.0.and.ybak.le.nyin-1) then
                          ipcCross = ipiecelower(newedge,3-ixy)
                          xpcCross = xbak
                          ypcCross = ybak
                        endif
                      endif
c
                      newedge = iedgeupper(newpiece, 3-ixy)
                      if (newedge .ne. 0) then
                        call positionInPiece(xg, yg,
     &                      ipieceupper(newedge,3-ixy), xbak, ybak)
                        if (xbak.ge.0.and.xbak.le.nxin-1.and.
     &                      ybak.ge.0.and.ybak.le.nyin-1) then
                          ipcCross = ipieceupper(newedge,3-ixy)
                          xpcCross = xbak
                          ypcCross = ybak
                        endif
                      endif
                    endif
                  endif
c                 
c                   add edge to list only if legal piece found
c                   
                  if(listno.gt.0)then
                    numedges(ixy)=numedges(ixy)+1
                    inedge(numedges(ixy),ixy)=newedge
                    if(iflo.eq.0)then
                      inedupper(numedges(ixy),ixy)=listno
                      inedlower(numedges(ixy),ixy)=indinp
                    else
                      inedupper(numedges(ixy),ixy)=indinp
                      inedlower(numedges(ixy),ixy)=listno
                    endif
                  endif
                endif
              endif
            endif
          enddo
        enddo
        indinp=indinp+1
      enddo
c      if (indy.eq.998) write(*,'(2i5,i3,a,4i3,10i5)')indx,indy,numPieces,
c     &    ' pieces',minxframe,maxxframe,minyframe,maxyframe,(inPiece(i),i=1,numPieces)
c       
c       If the pieces extend too far in either direction, we need to reduce the
c       list to the ones where the point is most interior
      if (maxxframe .gt. minxframe + 1 .or. maxyframe .gt. minyframe + 1) then
c         
c         first find axis where point is most interior
        maxInside = -10000000
        do i = 1, numPieces
          if (min(xinpiece(i), nxin - 1 - xinpiece(i)) .gt. maxInside) then
            maxInside = min(xinpiece(i), nxin - 1 - xinpiece(i))
            axisin = 1
          endif
          if (min(yinpiece(i), nyin - 1 - yinpiece(i)) .gt. maxInside) then
            maxInside = min(yinpiece(i), nyin - 1 - xinpiece(i))
            axisin = 2
          endif
        enddo
c        if (indy.eq.685)print *,'maxInside, axisin',maxInside,axisin
c         
c         do the most interior axis first and find best set of frames for it
c         Then do the other axis, with new constraint on first axis
        do ixy = 1, 2
          if (axisin .eq. ixy) then
            call mostInteriorFrames(1, xinpiece, nxin, ixframe, iyframe)
            minxframe = ixframe
            maxxframe = min(ixframe + 1, maxxframe)
            if (ixy .eq. 1) call constrainFrames(minxframe, maxxframe,
     &          minyframe, maxyframe, inpxframe, inpyframe)
          else
            call mostInteriorFrames(2, yinpiece, nyin, ixframe, iyframe)
            minyframe = iyframe
            maxyframe = min(iyframe + 1, maxyframe)
            if (ixy .eq. 1) call constrainFrames(minyframe, maxyframe,
     &          minxframe, maxxframe, inpyframe, inpxframe)
          endif
c          if(indx.eq.37) print *,ixy, minxframe,maxxframe,minyframe,maxyframe
        enddo
c         
c         Repack the frames, retaining only the ones within range and keeping
c         track of former numbers
        j = 0
        do i = 1, numPieces
          if (inpxframe(i).ge.minxframe .and. inpxframe(i) .le. maxxframe .and.
     &        inpyframe(i).ge.minyframe .and. inpyframe(i) .le. maxyframe) then
            j = j + 1
            inPiece(j) = inPiece(i)
            xinPiece(j) = xinPiece(i)
            yinPiece(j) = yinPiece(i)
            inpxFrame(j) = inpxFrame(i)
            inpyFrame(j) = inpyFrame(i)
            movedPiece(j) = i
          endif
        enddo
        numPieces = j
c         
c         Repack edges too
        do ixy = 1, 2
          j = 0
          do i = 1, numedges(ixy)
            ixfrm = 0
            iyfrm = 0
c             
c             Find the pieces that the lower and upper pieces became; if both
c             exist, retain the edge and reassign the numbers
            do k = 1, numPieces
              if (inedlower(i, ixy) .eq. movedPiece(k)) ixfrm = k
              if (inedupper(i, ixy) .eq. movedPiece(k)) iyfrm = k
            enddo
            if (ixfrm .gt. 0 .and. iyfrm .gt. 0) then
              j = j + 1
              inedlower(j, ixy) = ixfrm
              inedupper(j, ixy) = iyfrm
              inedge(j, ixy) = inedge(i,ixy)
            endif
          enddo
          numedges(ixy) = j
        enddo
      endif
c       
c       If there is one piece and a potential cross-piece, consider switching
      if (numPieces .eq. 1 .and. ipcCross .ne. 0) then
        ixframe = (ixpclist(ipcCross) - minxpiece) / (nxin - nxoverlap) + 1
        iyframe = (iypclist(ipcCross) - minypiece) / (nyin - nyoverlap) + 1
        ixy = mapDisjoint(min(ixframe,inpxFrame(1)), min(iyframe,inpyFrame(1)))
c         
c         Use cross piece if point is more interior along the other axis from
c         the disjoint edges (ixy is 1,2 for X, 3,4 for Y)
        if (ixy .gt. 0) then
          if ((ixy .le. 2 .and. min(ypcCross, nyin-1-ypcCross) .gt.
     &        min(yinpiece(1), nyin - 1 - yinpiece(1))) .or.
     &        (ixy .gt. 2 .and. min(xpcCross, nxin-1-xpcCross) .gt.
     &        min(xinpiece(1), nxin - 1 - xinpiece(1)))) then
            inpiece(1) = ipcCross
            xinpiece(1) = xpcCross
            yinpiece(1) = ypcCross
            inpxFrame(1) = ixframe
            inpyFrame(1) = iyframe
          endif
        endif
      endif
c       
c       If there are two pieces and the limit flag is set, find out if one
c       should be thrown away
      if (limitData .and. numPieces .eq. 2) then
        ixy = 1
        if (numedges(2) .gt. 0) ixy = 2
        do i = 1, 2
          iflo = 1
c           
c           if the piece is lower, get the limits on upper side
          if (i .eq. inedlower(1, ixy)) iflo = 2
          call getDataLimits(inpiece(i), 3-ixy, iflo, limitLo, limitHi)
          inLimit(i) = (ixy .eq. 1 .and. yinPiece(i) .ge. limitLo .and.
     &        yinPiece(i) .le. limitHi) .or. (ixy .eq. 2 .and.
     &        xinPiece(i) .ge. limitLo .and. xinPiece(i) .le. limitHi)
        enddo
        if (b3dxor(inLimit(1), inLimit(2))) then
          numPieces = 1
          numEdges(ixy) = 0
          if (inLimit(2)) then
            xinPiece(1) = xinPiece(2)
            yinPiece(1) = yinPiece(2)
            inPiece(1) = inPiece(2)
          endif
        endif
      endif
c       
c       Replace edges with ones to use if called for
      if (useEdges) then
        do ixy = 1, 2
          do i = 1, numedges(ixy)
            call findEdgeToUse(inedge(i,ixy), ixy, newedge)
            if (newedge .ne. 0) inedge(i, ixy) = newedge
          enddo
        enddo
      endif
      return

      CONTAINS
c       
c       Find which set of pieces has the point most interior in a given
c       direction
      subroutine mostInteriorFrames(ixy, xyinpiece, nxyin, ixbest, iybest)
      real*4 xyinpiece(*), closest, distin(2,2), dist1, dist2
      integer*4 nxyin, ixbest, iybest, ix, iy, ixy, maxOneCol,ixOneCol,iyOneCol
      maxInside = -100000
      maxOneCol = -100000
      ixbest = minxframe
      iybest = minyframe
      ixOneCol = minxframe
      iyOneCol = minyframe
      do iyfrm = minyframe, max(minyframe, maxyframe-1)
        do ixfrm = minxframe, max(minxframe, maxxframe-1)
c           
c           Find the minimum distance to edge for frames that fit in this range
          distin = 1000000.
          do i = 1, numPieces
            ix = inpxframe(i)+1-ixfrm
            iy = inpyframe(i)+1-iyfrm
            if ((ix+1)/2 .eq. 1 .and. (iy+1)/2 .eq. 1)  distin(ix,iy) =
     &          min(xyinpiece(i), nxyin-xyinpiece(i))
          enddo
          if (ixy .eq. 1) then
            dist1 = min(distin(1,1), distin(1,2))
            dist2 = min(distin(2,1), distin(2,2))
          else
            dist1 = min(distin(1,1), distin(2,1))
            dist2 = min(distin(1,2), distin(2,2))
          endif
c           
c           Keep track of which range maximizes this distance separately for
c           ones with one column and two
          if (dist1 .lt. 999999. .and. dist2 .lt. 999999.) then
            closest = (dist1 + dist2) / 2.
            if (closest .le. nxyin .and. closest .gt. maxInside) then
              maxInside = closest
              ixbest = ixfrm
              iybest = iyfrm
            endif
          else
            closest = min(dist1, dist2)
            if (closest .le. nxyin .and. closest .gt. maxOneCol) then
              maxOneCol = closest
              ixOneCol = ixfrm
              iyOneCol = iyfrm
            endif
          endif
c          if (indy.eq.685.and.(indx+1)/2.eq.798/2) print *,ixfrm, iyfrm,dist1,dist2,closest
        enddo
      enddo
      if (maxInside .lt. 0 .and. maxOneCol .gt. 0) then
        ixbest = ixOneCol
        iybest = iyOneCol
      endif
      return
      end subroutine mostInteriorFrames

      subroutine constrainFrames(minAxis, maxAxis, minOther, maxOther,
     &    inpfAxis, inpfOther)
      integer*4 minAxis, maxAxis, minOther, maxOther, inpfAxis(*), inpfOther(*)
      i = minOther
      minOther = maxOther
      maxOther = i
      do i = 1, numPieces
        if (inpfAxis(i) .ge. minAxis .and. inpfAxis(i) .le. maxAxis) then
          minOther = min(minOther, inpfOther(i))
          maxOther = max(maxOther, inpfOther(i))
        endif
      enddo
      return
      end subroutine constrainFrames
      
      end subroutine countedges


c       Computes position of global point xg, yg (after g transforms) in piece
c       ipc, returns result in xinpc, yinpc
c
      subroutine positionInPiece(xg, yg, ipc, xinpc, yinpc)
      use blendvars
      implicit none
      real*4 xg, yg, xinpc, yinpc, xtmp
      integer*4 ipc
      xinpc = xg - ixpclist(ipc)
      yinpc = yg - iypclist(ipc)
      if (multng) then
        xtmp = xinpc
        xinpc=hinv(1,1,ipc)*xtmp+hinv(1,2,ipc)*yinpc +hinv(1,3,ipc)
        yinpc=hinv(2,1,ipc)*xtmp+hinv(2,2,ipc)*yinpc +hinv(2,3,ipc)
      endif
      return
      end


c       initNearList sets up an ordered list of dx, dy values to nearby pieces 
c       up to the maximum distance in maxDistNear
c
      subroutine initNearList()
      use blendvars
      implicit none
      integer*4 limx, limy, idx, idy, i, j, itmp
      real*4 temp
      
      limx = min(nxpieces - 1, maxDistNear)
      limy = min(nypieces - 1, maxDistNear)
      numPcNear = 0
      do idx = -limx, limx
        do idy = -limy, limy
          if (idx .ne. 0 .or. idy .ne. 0) then
            numPcNear = numPcNear + 1
            idxPcNear(numPcNear) = idx
            idyPcNear(numPcNear) = idy
            array(numPcNear) = idx**2 + idy**2
          endif
        enddo
      enddo
      do i = 1, numPcNear - 1
        do j = i + 1, numPcNear
          if (array(i) .gt. array(j)) then
            temp = array(i)
            array(i) = array(j)
            array(j) = temp
            itmp = idxPcNear(i)
            idxPcNear(i) = idxPcNear(j)
            idxPcNear(j) = itmp
            itmp = idyPcNear(i)
            idyPcNear(i) = idyPcNear(j)
            idyPcNear(j) = itmp
          endif
        enddo
      enddo
      return
      end


c       findNearestPiece first modifies the frame numbers to be within the
c       range for the montage, then checks whether this piece exists.  If not
c       it switches to the nearest piece that does exist
c
      subroutine findNearestPiece(ixFrame, iyFrame)
      use blendvars
      implicit none
      integer*4 ixFrame, iyFrame, i, ixnew, iynew
      ixFrame = max(1, min(nxpieces, ixFrame))
      iyFrame = max(1, min(nypieces, iyFrame))
      if (mappiece(ixFrame, iyFrame) .ne. 0) return
      do i = 1, numPcNear
        ixnew = ixFrame + idxPcNear(i)
        iynew = iyFrame + idyPcNear(i)
        if (ixnew .ge. 1 .and. ixnew .le. nxpieces .and. iynew .ge. 1 .and. 
     &      iynew .le. nypieces) then
          if (mappiece(ixnew, iynew) .ne. 0) then
            ixFrame = ixnew
            iyFrame = iynew
            return
          endif
        endif
      enddo
      return
      end

c       DXYDGRINTERP takes a coordinate X1,Y1 in the lower piece of the edge
c       at index INDEDG in the edge buffer, finds the coordinate within the
c       edge function grid, uses bilinear interpolation to find the values of
c       DX, DY and DDEN, and returns the coordinate in the upper piece, X2,Y2
c       and the average density difference DDEN
c       
      subroutine dxydgrinterp(x1,y1,indedg,x2,y2,dden)
c       
      use blendvars
      implicit none
      real*4 x1,x2,y1,y2,dden
      integer*4 indedg
c       
      real*4 xingrid,yingrid,xgrid,ygrid,fx1,fx,c00,c10,c01,c11
      real*4 fy1,fy,dxinterp,dyinterp
      integer*4 ixg,iyg,ixg1,iyg1
c       
c       find fractional coordinate within edge grid
c       
      xingrid=x1-ixgrdstbf(indedg)
      yingrid=y1-iygrdstbf(indedg)
      xgrid=1.+(xingrid)/intxgrbf(indedg)
      ygrid=1.+(yingrid)/intygrbf(indedg)
c       
c       find all fractions and indices needed for bilinear interpolation
c       
      ixg=xgrid
      ixg=max(1,min(nxgrbf(indedg)-1,ixg))
      iyg=ygrid
      iyg=max(1,min(nygrbf(indedg)-1,iyg))
      fx1=max(0.,min(1.,xgrid-ixg))             !NO EXTRAPOLATIONS ALLOWED
      fx=1.-fx1
      ixg1=ixg+1
      fy1=max(0.,min(1.,ygrid-iyg))
      fy=1.-fy1
      iyg1=iyg+1
      c00=fx*fy
      c10=fx1*fy
      c01=fx*fy1
      c11=fx1*fy1
c       
c       interpolate
c       
      dxinterp=c00*dxgrbf(ixg,iyg,indedg)+c10*dxgrbf(ixg1,iyg,indedg)
     &    +c01*dxgrbf(ixg,iyg1,indedg)+c11*dxgrbf(ixg1,iyg1,indedg)
      dyinterp=c00*dygrbf(ixg,iyg,indedg)+c10*dygrbf(ixg1,iyg,indedg)
     &    +c01*dygrbf(ixg,iyg1,indedg)+c11*dygrbf(ixg1,iyg1,indedg)
      dden=c00*ddengrbf(ixg,iyg,indedg)+c10*ddengrbf(ixg1,iyg,indedg)
     &    +c01*ddengrbf(ixg,iyg1,indedg)+c11*ddengrbf(ixg1,iyg1,indedg)
c       
      x2=xingrid+dxinterp+ixofsbf(indedg)
      y2=yingrid+dyinterp+iyofsbf(indedg)
      return
      end

      subroutine crossvalue(xinlong,nxpieces,nypieces,nshort,nlong)
      logical xinlong
      if(xinlong)then
        nshort=nypieces
        nlong=nxpieces
      else
        nshort=nxpieces
        nlong=nypieces
      endif
      return
      end


c       Computes sizes for doing edge cross-correlation in one direction
c       ixy is 1/2 for X/Y edges, nbin is the binning, indentXC is the indent
c       from full size.  It returns indentUse with the actual indent being
c       used, nxybox with the image box size after binning, nExtra with the
c       the number of extra pixels in each dimension (from extraWidth), and
c       the padded sizes for correlation in nxpad, nypad.  The padded size
c       for filtering to compute real-space correlations is in nxCCC, nyCCC
c       
      subroutine xcorrSizes(ixy, nbin, indentXC, indentUse, nxybox, nExtra,
     &    nxpad, nypad, nxCCC, nyCCC, maxLongShift)
      use blendvars
      implicit none
      integer*4 indentXC,ixy,nbin,indentUse, nxybox(2), nExtra(2),nxpad, nypad
      integer*4 nxCCC, nyCCC, iyx, nxybord(2), npadCCC, niceframe,maxLongShift
      real*4 cccPadFrac
      cccPadFrac = 0.1
      iyx=3-ixy
      indentUse = min(indentXC, (noverlap(ixy) - 8) / 2)
      nxybox(ixy) = (noverlap(ixy) - indentUse * 2) / nbin
      nxybox(iyx) = min((nxyzin(iyx) - max(2*nbin, nxyzin(ixy) / 20)), 
     &    int(aspectmax*noverlap(ixy))) / nbin
      nExtra(iyx)=0
      nExtra(ixy) = min(2 * (nint(extraWidth * nxybox(ixy)) / 2),
     &    (nxyzin(ixy) - max(nbin, indentUse, nxyzin(ixy) / 20) * 2) / nbin -
     &    nxybox(ixy))
      nxybox(ixy) = nxybox(ixy) + nExtra(ixy)
      maxLongShift = nint(max(1.9 * noverlap(ixy) / nbin, 1.5 * nxybox(ixy)))
c       
c       get the padded size
c       Limit the long dimension padding to that needed for the maximum shift
      nxybord(ixy)=max(5,nint(padFrac*nxybox(ixy)))
      nxybord(iyx)=min(max(5,nint(padFrac*nxybox(iyx))), 
     &    max(5, nint(0.45 * maxLongShift)))
      nxpad=niceframe(nxybox(1)+2*nxybord(1),2,19)
      nypad=niceframe(nxybox(2)+2*nxybord(2),2,19)
c
c       12/28/10: these sizes are now irrelevant
      npadCCC = max(16, nint(cccPadFrac * nxybox(1)))
      nxCCC = niceframe(nxybox(1) + 2 * npadCCC, 2, 19)
      npadCCC = max(16, nint(cccPadFrac * nxybox(2)))
      nyCCC = niceframe(nxybox(2) + 2 * npadCCC, 2, 19)
      return
      end


c       Does cross-correlation on an edge
c
      subroutine xcorredge(crray,drray,ixy,xdisp,ydisp, legacy, indentXC)
      use blendvars
      implicit none
      real*4 crray(*),drray(*),xdisp,ydisp
      integer*4 indentXC,ixy
      logical legacy
      integer*4 nxybox(2),ind0(2),ind1(2),idispl(2)
      real*4 ctf(8193),rdispl(2)
      real*4 overfrac,delta,sdmin,ddenmin
      real*4 xpeak(limXcorrPeaks),ypeak(limXcorrPeaks),peak(limXcorrPeaks)
      integer*4 indentSD,niter,limstep,iyx,nxpad,nypad, indentUse
      integer*4 ixdispl,iydispl,i,nExtra(2),nbin, ierr, nxtrim, nytrim
      integer*4 nsmooth, nxsmooth, nysmooth, indPeak, nxCCC, nyCCC
      integer*4 taperAtFill, nsum, maxLongShift
      real *8 cccMax, ccc, CCCoefficient,walltime,wallstart

      indentSD=5                                !indent for sdsearch
      overfrac=0.9                              !fraction of overlap to use
      niter=4                                   !iterations for sdsearch
      limstep=10                                !limiting distance
      nbin = nbinXcorr
      nsmooth = 6
      wallstart = walltime()
c       
c       find size and limits of box in overlap zone to cut out
c       
      iyx=3-ixy
      call xcorrSizes(ixy, nbin, indentXC, indentUse, nxybox, nExtra,
     &    nxpad, nypad, nxCCC, nyCCC, maxLongShift)
      indentSD = indentSD + indentUse
      ind0(iyx)=nxyzin(iyx)/2 - (nbin * nxybox(iyx))/2
      ind1(iyx)=ind0(iyx) + nbin * nxybox(iyx) - 1
      ind0(ixy)=nxyzin(ixy) - noverlap(ixy) + indentUse - nbin * nExtra(ixy)
      ind1(ixy)=ind0(ixy) + nbin * nxybox(ixy) - 1
c
c       Set up smoothing over some pixels, but no more than half of the pad
      nxSmooth = nxybox(1) + min(2 * nsmooth, (nxpad - nxybox(1)) / 2)
      nySmooth = nxybox(2) + min(2 * nsmooth, (nypad - nxybox(2)) / 2)
c      print *,ixy,indentUse, nxybox(ixy),nxybox(iyx),ind0(iyx),ind1(iyx),
c     &    ind0(ixy),ind1(ixy)
c      print *,nxpad,nypad, nxSmooth, nySmooth

      if(nxybox(1)*nxybox(2)*nbin**2 .gt.maxbsiz .or. nxpad*nypad.gt.idimc .or.
     &    nxCCC*nyCCC .gt. idimc) call
     &    exitError('CORRELATION ARRAYS WERE NOT MADE LARGE ENOUGH')
c       
c       get the first image, lower piece
c       
      call ibinpak(brray, crray,nxin,nyin,ind0(1),ind1(1),ind0(2),
     &    ind1(2), nbin)
      if (ifillTreatment .eq. 2)
     &    ierr = taperAtFill(brray, nxybox(1),nxybox(2), 64, 0)
      if (nxSmooth .gt. nxybox(1) .and. nySmooth .gt. nxybox(2)) then
        call smoothoutpad(brray,nxybox(1),nxybox(2),xcray,nxSmooth,nxSmooth,
     &      nySmooth)
        call taperoutpad(xcray,nxSmooth,nySmooth,xcray,nxpad+2,nxpad,
     &      nypad,0,0.)
      else
        call taperoutpad(brray,nxybox(1),nxybox(2),xcray,nxpad+2,nxpad,
     &      nypad,0,0.)
      endif
      call meanzero(xcray,nxpad+2,nxpad,nypad)
      call dumpedge(xcray,nxpad+2,nxpad,nypad,ixy,0)
      call todfft(xcray,nxpad,nypad,0)
c       
c       get the second image, upper piece
c       
      ind0(ixy)=indentUse
      ind1(ixy)=ind0(ixy) + nbin * nxybox(ixy) - 1
c       
      call ibinpak(brray, drray,nxin,nyin,ind0(1),ind1(1),ind0(2),
     &    ind1(2), nbin)
      if (ifillTreatment .eq. 2)
     &    ierr = taperAtFill(brray, nxybox(1),nxybox(2), 64, 0)
      if (nxSmooth .gt. nxybox(1) .and. nySmooth .gt. nxybox(2)) then
        call smoothoutpad(brray,nxybox(1),nxybox(2),xdray,nxSmooth,nxSmooth,
     &      nySmooth)
        call taperoutpad(xdray,nxSmooth,nySmooth,xdray,nxpad+2,nxpad,
     &      nypad,0,0.)
      else
        call taperoutpad(brray,nxybox(1),nxybox(2),xdray,nxpad+2,nxpad,
     &      nypad,0,0.)
      endif
      call meanzero(xdray,nxpad+2,nxpad,nypad)
      call dumpedge(xdray,nxpad+2,nxpad,nypad,ixy,0)
      call todfft(xdray,nxpad,nypad,0)
c       
c       Multiply all filter parameters by the binning so they are equivalent
c       to frequencies in unbinned images
c
      call setctfwsr(nbin*sigma1,nbin*sigma2,nbin*radius1,nbin*radius2,ctf,
     &    nxpad,nypad,delta)
      if (numXcorrPeaks .gt. 1 .and. .not.legacy .and. delta .ne. 0.) then
c         
c         If we are going to evaluate multiple peaks, use the square root of
c         this filter function and apply it to each image
        do i = 1, 8193
          ctf(i) = sqrt(ctf(i))
        enddo
        call filterpart(xcray,xcray,nxpad,nypad,ctf,delta)
        call filterpart(xdray,xdray,nxpad,nypad,ctf,delta)
        do i = 1, (nxpad+2)*nypad/2
          xeray(i) = xcray(i)
        enddo
      elseif (delta.ne.0.) then
        call filterpart(xcray,xcray,nxpad,nypad,ctf,delta)
      endif
c       
c       multiply xcray by complex conjugate of xdray, put back in xcray
c       
      call conjugateProduct(xcray, xdray, nxpad, nypad)
c       
      call todfft(xcray,nxpad,nypad,1)
      call xcorrPeakFind(xcray,nxpad+2,nypad,xpeak,ypeak,peak,
     &    max(16, numXcorrPeaks))
c      write(*,'(a,f10.6)')'Initial cross-corr time',walltime()-wallstart
c       
c       Eliminate any peaks that shift beyond maximum along edge
c       leave indPeak pointing to first good peak
      indPeak = 0
      do i = 1, max(16, numXcorrPeaks)
        if ((ixy .eq. 1 .and. abs(ypeak(i)) .gt. maxLongShift) .or.
     &      (ixy .eq. 2 .and. abs(xpeak(i)) .gt. maxLongShift)) then
          peak(i) = -1.e30
c          print *,'eliminated', i, xpeak(i), ypeak(i)
        elseif (indPeak .eq. 0 .and. peak(i) .gt. -1.e29) then
          indPeak = i
        endif
      enddo
c       
c       But if no peak was legal, zero out the shift
      if (indPeak .eq. 0) then
        indPeak = 1
        xpeak(1) = nExtra(1)
        ypeak(1) = nExtra(2)
      endif
      if (numXcorrPeaks .gt. 1 .and. .not.legacy) then
c         
c         If there was no filtering, pad second image into xdray then get 
c         first image again
        if (delta .eq. 0) then
          call taperoutpad(brray,nxybox(1),nxybox(2),xdray,nxpad+2,nxpad,
     &        nypad,0,0.)
          ind0(ixy)=nxyzin(ixy) - noverlap(ixy) + indentUse - nbin * nExtra(ixy)
          ind1(ixy)=ind0(ixy) + nbin * nxybox(ixy) - 1
          call ibinpak(brray, crray,nxin,nyin,ind0(1),ind1(1),ind0(2),
     &        ind1(2), nbin)
          if (ifillTreatment .eq. 2)
     &        ierr = taperAtFill(brray, nxybox(1),nxybox(2), 64, 0)
          call taperoutpad(brray,nxybox(1),nxybox(2),xeray,nxpad+2,nxpad,
     &        nypad,0,0.)
        else
c           
c           Otherwise, back-transform the filtered images
          call todfft(xeray,nxpad,nypad,1)
          call todfft(xdray,nxpad,nypad,1)
c         
          call dumpedge(xeray,nxpad+2,nxpad,nypad,ixy,0)
          call dumpedge(xdray,nxpad+2,nxpad,nypad,ixy,0)
        endif
        cccMax = -10.
        do i = 1, numXcorrPeaks
          if (peak(i) .gt. -1.e29) then
c             
c             Reject peak at no zero image offset from fixed pattern noise
            if (.not. (ixy .eq. 1 .and.
     &          abs(nxin + nbin * (xpeak(i) - nExtra(1)) - nxoverlap) .le. 3.)
     &          .or. (ixy .eq. 2 .and.
     &          abs(nyin + nbin * (ypeak(i) - nExtra(2)) - nyoverlap) .le. 3.))
     &          then
              nxtrim = min(4, nxybox(1) / 8) + (nxpad - nxybox(1)) / 2
              nytrim = min(4, nxybox(2) / 8) + (nypad - nxybox(2)) / 2
              ccc = CCCoefficient(xeray, xdray, nxpad + 2, nxpad, nypad,
     &            xpeak(i), ypeak(i), nxtrim, nytrim, nsum)
c               write(*,'(i3,a,2f7.1,a,e14.7,a,i8,a,f8.5)')i,' at ',xpeak(i), 
c     &            ypeak(i), ' peak =',peak(i), ' nsum = ', nsum, ' cc =',ccc
              if (ccc .gt. cccMax .and. (i .eq. 1 .or. nsum .gt.
     &            (nxpad - 2 * nxtrim) * (nypad - 2 * nytrim) / 8)) then
                cccMax = ccc
                indPeak = i
              endif
            endif
          endif
          
        enddo
        i = indPeak
c        write(*,'(i3,a,2f7.1,a,e14.7,a,f8.5)')i,' at ',xpeak(i), 
c     &      ypeak(i), ' peak =',peak(i), ' cc =',cccMax
c        write(*,'(a,f10.6)')'time after CCC',walltime()-wallstart
      endif
      call dumpedge(xcray,nxpad+2,nxpad,nypad,ixy,1)
c       
c       return the amount to shift upper to align it to lower (verified)
c       
      xdisp = nbin * (xpeak(indPeak) - nExtra(1))
      ydisp = nbin * (ypeak(indPeak) - nExtra(2))
c       write(*,'(2f8.2,2f8.2)')xpeak(indPeak),ypeak(indPeak),xdisp,ydisp
      if(legacy)return
c       
c       the following is adopted largely from setgridchars
c       
      ixdispl=nint(xdisp)
      iydispl=nint(ydisp)
      if(ixy.eq.1)then
        idispl(1)=nxin-nxoverlap+ixdispl
        idispl(2)=iydispl
      else
        idispl(1)=ixdispl
        idispl(2)=nyin-nyoverlap+iydispl
      endif
      iyx=3-ixy
c       
c       get size of box, limit to size of overlap zone
c       
      nxybox(ixy)=min(noverlap(ixy),nxyzin(ixy)-idispl(ixy))
      nxybox(ixy)=min(nxybox(ixy)-indentSD*2, nint(overfrac*nxybox(ixy)))
      nxybox(iyx)=nxyzin(iyx)-abs(idispl(iyx))
      nxybox(iyx)=min(nxybox(iyx)-indentSD*2, nint(aspectmax*nxybox(ixy)))
      do i=1,2
        ind0(i)=(nxyzin(i)+idispl(i)-nxybox(i))/2
        ind1(i)=ind0(i)+nxybox(i)
        rdispl(i)=-idispl(i)
      enddo
c       
c       integer scan is not needed, but to use it uncomment this 
c       
c       intscan=6
c       call sdintscan(crray,drray,nxin,nyin,ind0(1),ind0(2),ind1(1),
c       &           ind1(2),-idispl(1)-intscan,-idispl(2)-intscan,
c       &           -idispl(1)+intscan,-idispl(2)+intscan,sdmin,ddenmin,
c       &           idxmin,idymin)
c       rdispl(1)=idxmin
c       rdispl(2)=idymin
c       
      call bigsearch(crray,drray,nxin,nyin,ind0(1),ind0(2),ind1(1),
     &    ind1(2),rdispl(1),rdispl(2),sdmin,ddenmin,niter,limstep)
      
      if(ixy.eq.1)then
        xdisp=-rdispl(1)-(nxin-nxoverlap)
        ydisp=-rdispl(2)
      else
        xdisp=-rdispl(1)
        ydisp=-rdispl(2)-(nyin-nyoverlap)
      endif
c      write(*,'(a,f10.6)')'time after big search',walltime()-wallstart
c       write(*,'(2f8.2,2f8.2)')xpeak(indPeak),ypeak(indPeak),xdisp,ydisp
      return
      end


c       Solve for the shifts of all the pieces based on the displacements
c       across their edges
c
      subroutine find_best_shifts(dxgridmean,dygridmean,idimedge,idir,
     &    izsect,h, nsum,bavg,bmax,aavg,amax)

      use blendvars
      implicit none
      integer*4 idir,izsect,nsum,idimedge
      real*4 bavg,bmax,aavg,amax
c       
      real*4 h(2,3,*)
      real*4 dxgridmean(idimedge,2),dygridmean(idimedge,2)
      integer*4 numInRow,newGroup,lowup
c       
c       Set maxvar higher to get comparisons
      integer maxvar, maxGaussj
      parameter (maxGaussj = 10, maxvar = maxGaussj)
      real*4 a(maxvar,maxvar)
c       
      real*4 critmaxmove,critMoveDiff, wErrMean, wErrMax
      integer*4 intervalForTest, numAvgForTest,findPieceShifts,maxiter
      integer*4 nvar,ipc,ivar,m,ixy,iedge,neighpc,neighvar,ipclo,i,j, numGroups
      integer*4 numPrev, ndxy, nallvar, nextvar, nextCheck, numToCheck, igroup
      real*4 asum,bsum,xsum,ysum,bdist,adist, dxgroup, dygroup
      real*8 wallstart, walltime, wallAdj, wallGaussj
      integer*4 gaussj
c$$$c       
c$$$c       variables for SVD
c$$$      integer maxgels
c$$$      parameter (maxgels = 12000)
c$$$      real*8 daa(maxgels, maxgels), dbb(maxgels, 2), dwork(50*maxgels)
c$$$      real*8 singval(maxgels),acond
c$$$      common /sngval/daa,dbb,dwork,singval
c       
c       The data coming in are the displacements of upper piece from
c       being in alignment with the lower piece if idir = 1, or the
c       shift needed to align upper piece with lower if idir = -1
c       
c       build list of variables: ALL means all pieces that have an edge
c       
      nallvar=0
      do ipc=1,npclist
        if (izpclist(ipc).eq.izsect) then
          call xfunit(h(1,1,ipc),1.)
          call xfunit(hinv(1,1,ipc),1.)
          indvar(ipc) = 0
          if (iedgelower(ipc,1).gt.0.or.
     &        iedgelower(ipc,2).gt.0.or.
     &        iedgeupper(ipc,1).gt.0.or.
     &        iedgeupper(ipc,2).gt.0) then
            nallvar=nallvar+1
            if (nallvar.gt.limvar)then
              print *,'nallvar, limvar',nallvar, limvar
              call exitError(
     &            'ARRAYS WERE NOT MADE LARGE ENOUGH FOR FIND_BEST_SHIFTS')
            endif
            iallVarpc(nallvar) = ipc
            ivarGroup(nallvar) = 0
            indvar(ipc) = nallvar
          endif
        endif
      enddo
c       
c       Classify pieces into separate groups if any by following connections
c       between them
      call sortVarsIntoGroups()
c
      nsum=0
      bsum=0.
      bmax=0.
      asum=0.
      amax=0.
      bavg=0.
      aavg=0.
      numPrev = 0
      if (nallvar.eq.1) return
c       
c       Loop on groups, set up to do fit for each group
      do igroup = 1, numGroups
        nvar = 0
        do ivar = 1, nallVar
          if (ivarGroup(ivar) .eq. igroup) then
            nvar = nvar + 1
            ivarpc(nvar) = iallVarpc(ivar)
            indvar(ivarpc(nvar)) = nvar
          endif
        enddo
c        print *,nvar
        if (nvar .gt. 1) then
c       
c           build matrix of simultaneous equations for minimization solution
c           by matrix inversion or SVD
          do ivar=1,nvar-1
            ipc=ivarpc(ivar)
            do m=1,nvar-1
              rowTmp(m) = 0.
              bb(1,ivar)=0.
              bb(2,ivar)=0.
            enddo
c             
            do ixy=1,2
              if (includeEdge(1, ipc,ixy, iedge)) then
                rowTmp(ivar)=rowTmp(ivar)+1
                neighpc=ipiecelower(iedge,ixy)
                neighvar=indvar(neighpc)
c                 
c                 for a regular neighbor, enter a -1 in its term; but for the
c                 last variable being eliminated, enter a +1 for ALL other
c                 variables instead
c             
                if(neighvar.ne.nvar)then
                  rowTmp(neighvar)=rowTmp(neighvar)-1
                else
                  do m=1,nvar-1
                    rowTmp(m)=rowTmp(m)+1
                  enddo
                endif
c             
c                 when this piece is an upper piece, subtract displacements
c                 from constant term
c                 
                bb(1,ivar)=bb(1,ivar)-idir*dxgridmean(iedge,ixy)
                bb(2,ivar)=bb(2,ivar)-idir*dygridmean(iedge,ixy)
              endif
c           
              if (includeEdge(2, ipc,ixy, iedge)) then
                rowTmp(ivar)=rowTmp(ivar)+1
                neighpc=ipieceupper(iedge,ixy)
                neighvar=indvar(neighpc)
                if(neighvar.ne.nvar)then
                  rowTmp(neighvar)=rowTmp(neighvar)-1
                else
                  do m=1,nvar-1
                    rowTmp(m)=rowTmp(m)+1
                  enddo
                endif
c             
c                 when a lower piece, add displacements to constant terms
c             
                bb(1,ivar)=bb(1,ivar)+idir*dxgridmean(iedge,ixy)
                bb(2,ivar)=bb(2,ivar)+idir*dygridmean(iedge,ixy)
              endif
            enddo
c$$$c         
c$$$c         LOAD FOR SVD
c$$$            do m=1,nvar-1
c$$$              daa(ivar,m) = rowTmp(m)
c$$$            enddo
c$$$            dbb(ivar,1) = bb(1,ivar)
c$$$            dbb(ivar,2) = bb(2,ivar)
c             
c             Load the row data in a if below maxvar
            if (nvar .le. maxvar) then
              do m=1,nvar-1
                a(m,ivar) = rowTmp(m)
              enddo
            endif
          enddo
c$$$c       
c$$$c       Solve SVD
c$$$          wallstart = walltime()
c$$$          acond = -1.
c$$$          call dgelss(nvar-1,nvar-1,2,daa,maxgels, dbb, maxgels, singval,
c$$$     &        acond, m, dwork, 50*maxgels, ixy)
c$$$          write(*,'(a,i5,a,i5,a,f10.4)')'dgelss  rank',m,
c$$$     &        '  info',ixy,'  time', walltime()-wallstart

c           
c           Solve by iteration first
          if (nvar .gt. maxGaussj) then
            critMaxMove = 1.e-4
            numAvgForTest = 10
            intervalForTest = 100
            critMoveDiff = 1.e-6
            maxiter = 100 + nvar * 10
            wallstart = walltime()
            wallstart = walltime()
            if (findPieceShifts(ivarpc, nvar, indvar, ixpclist, iypclist,
     &          dxgridmean, dygridmean, idir, ipiecelower, ipieceupper,
     &          ifskipEdge, limedge, dxyvar, limvar, iedgelower, iedgeupper,
     &          limnpc, fpsWork, 1, 0, 2, robustCrit, critMaxMove,
     &          critMoveDiff, maxiter, numAvgForTest, intervalForTest, i,
     &          wErrMean, wErrMax)
     &          .ne. 0) call exitError('CALLING findPieceShifts')
            wallAdj = walltime()-wallstart
c            write(*,'(i6,a,f8.4,a,f15.6)')i, ' iterations, time',wallAdj

c$$$            ysum = 0.
c$$$            xsum = 0.
c$$$            do ivar=1,nvar-1
c$$$              ipc=ivarpc(ivar)
c$$$              do ixy = 1,2
c$$$                xsum = max(xsum, abs(dxyvar(ivar,ixy) - dbb(ivar,ixy)))
c$$$                do j=1,2
c$$$                  if (includeEdge(2, ipc,j, iedge)) then
c$$$                    numInRow = 0
c$$$                    neighpc=ipieceupper(iedge,j)
c$$$                    neighvar=indvar(neighpc)
c$$$                    if (neighvar.ne.nvar) ysum = max(ysum, abs(
c$$$     &                  (dbb(ivar,ixy) - dbb(neighvar,ixy))
c$$$     &                  - (dxyvar(ivar,ixy) - dxyvar(neighvar,ixy))))
c$$$                  endif
c$$$                enddo
c$$$              enddo
c$$$            enddo
c$$$            write(*,'(a,f12.7,a,f12.7)')'Shift adj - SVD max position diff'
c$$$     &          ,xsum,'  edge diff',ysum
          endif
c       
c           solve the equations with gaussj if within range

c       
c           write(*,'(9i5)')(ivarpc(i),i=1,nvar)
c           write(*,'(8f7.1)')((a(j,i),i=1,nvar-1),j=1,nvar-1)
c           write(*,'(8f9.2)')((bb(j,i),i=1,nvar-1),j=1,2)
          if (nvar .le. maxvar) then
            wallstart = walltime()
            i = gaussj(a,nvar-1,maxvar,bb,2,2)
            if (i .gt. 0) call exitError(
     &          'SINGULAR MATRIX WHEN SOLVING LINEAR EQUATIONS IN GAUSSJ')
            if (i .lt. 0) call exitError(
     &          'TOO MANY VARIABLES TO SOLVE LINEAR EQUATIONS WITH GAUSSJ')
            wallGaussj = walltime() - wallstart
c             write(*,'(8f9.2)')((bb(j,i),i=1,nvar-1),j=1,2)
          endif
c       
c           Use the iteration solution, do comparisons if both were done
          if (nvar .gt. maxGaussj) then
            do j = 1,2
              xsum = 0.
              do i = 1,nvar-1 
                if (nvar .le. maxvar) xsum =
     &              max(xsum, abs(bb(j,i) - dxyvar(i,j)))
                bb(j,i) = dxyvar(i,j)
              enddo
              if (nvar .le. maxvar)write(*,'(a,i2,a,f15.7)'),'axis', j,
     &            ' max shift adj - gaussj difference',xsum
c               write(*,'(8f9.2)')(bb(j,i),i=1,nvar-1)
            enddo
            if (nvar .le. maxvar) write(*,'(a,f12.6, a, f12.6)')'gaussj time',
     &          wallGaussj, '   shift adj time', wallAdj
          endif
        endif
c
c         take the b values as dx and dy; compute the
c         sum to get the shift for the final piece
        xsum=0.
        ysum=0.
        do i=1,nvar-1
          h(1,3,ivarpc(i))=bb(1,i)
          h(2,3,ivarpc(i))=bb(2,i)
          xsum=xsum+bb(1,i)
          ysum=ysum+bb(2,i)
        enddo
        h(1,3,ivarpc(nvar))=-xsum
        h(2,3,ivarpc(nvar))=-ysum
c
c         For multiple groups, find the mean displacement between this group
c         and all previous ones and adjust shifts to make that mean be zero
c         but to keep the overall mean zero
        if (igroup .gt. 1) then
          dxgroup = 0.
          dygroup = 0.
          ndxy = 0
c           
c           rebuild index to variables
          do ivar = 1, nallvar
            indvar(iallVarpc(ivar)) = ivar
          enddo
c           
c           Loop on pieces in this group, and for each edge to a piece in a
c           lower group, add up the displacement across the edge
          do ivar = 1, nvar
            ipc = ivarpc(ivar)
            do lowup = 1, 2
              do ixy = 1, 2
                if (.not. includeEdge(lowup, ipc, ixy, iedge)) then
                  if (iedge .gt. 0) then
                    if (lowup.eq.1) then
                      ipclo=ipiecelower(iedge,ixy)
                    else
                      ipclo=ipieceupper(iedge,ixy)
                    endif                    
                    if (ivarGroup(indvar(ipclo)) .lt. igroup) then
                      dxgroup = dxgroup + h(1,3,ipc) - h(1,3,ipclo)
                      dygroup = dygroup + h(2,3,ipc) - h(2,3,ipclo)
                      ndxy = ndxy + 1
                    endif
                  endif
                endif
              enddo              
            enddo              
          enddo
c           
c           Adjust positions by a weighted fraction of the mean displacement
          if (ndxy .gt. 0) then
            dxgroup = dxgroup / ndxy
            dygroup = dygroup / ndxy
c            print *,'Shift relative to previous group', dxgroup,dygroup
c            print *,'adjusting this',-(dygroup * numPrev) / (nvar +numPrev),
c     &          '   this',(dygroup * nvar) / (nvar + numPrev)
            do ivar = 1, nallvar
              ipc=iallVarpc(ivar)
              if (ivarGroup(ivar) .lt. igroup) then
                h(1,3,ipc) = h(1,3,ipc) + (dxgroup * nvar) / (nvar + numPrev)
                h(2,3,ipc) = h(2,3,ipc) + (dygroup * nvar) / (nvar + numPrev)
              else if (ivarGroup(ivar) .eq. igroup) then
                h(1,3,ipc) = h(1,3,ipc) - (dxgroup * numPrev) / (nvar +numPrev)
                h(2,3,ipc) = h(2,3,ipc) - (dygroup * numPrev) / (nvar +numPrev)
              endif
            enddo
          endif
        endif
        numPrev = numPrev + nvar
      enddo
c       
c       compute and return the results 
c       
      do ivar=1,nallvar
        ipc=iallVarpc(ivar)
        call xfinvert(h(1,1,ipc),hinv(1,1,ipc))
c        write(*,'(i7,2i4,2f8.1)')ipc,1+(ixpclist(ipc)-minxpiece)/(nxin-nxoverlap),
c     &      1+(iypclist(ipc)-minypiece)/(nyin-nyoverlap), h(1,3,ipc),h(2,3,ipc)
        do ixy=1,2
          if (includeEdge(1, ipc,ixy, iedge)) then
            ipclo=ipiecelower(iedge,ixy)
            bdist=sqrt(dxgridmean(iedge,ixy)**2+
     &          dygridmean(iedge,ixy)**2)
            bsum=bsum+bdist
            adist=sqrt((idir*dxgridmean(iedge,ixy)+h(1,3,ipc)-h(1,3,ipclo))
     &          **2+(idir*dygridmean(iedge,ixy)+h(2,3,ipc)-h(2,3,ipclo))**2)
            asum=asum+adist
            amax=max(amax,adist)
            bmax=max(bmax,bdist)
            nsum=nsum+1
c            write(*,'(i7,2i4,f8.2)')ipc,ixy-1+(ixpclist(ipc)-minxpiece)/(nxin-nxoverlap),
c     &          2-ixy+(iypclist(ipc)-minypiece)/(nyin-nyoverlap), adist
          endif
        enddo
      enddo
      bavg=bsum/nsum
      aavg=asum/nsum
c       write(*,'(i3,a,2f6.2,a,2f6.2)')nsum, ' edges, mean, max '//
c     &    'displacement before:', bsum/nsum,bmax,', after:',asum/nsum, amax
c       
      return

      CONTAINS

c       Test for whether an edge should be included, lowup = 1/2 for lower/
c       upper edge, kpc is piece index, kxy is x/y direction, the edge number
c       if any is returned in ked
c
      logical function includeEdge(lowup, kpc, kxy, ked)
      integer*4 lowup, kpc, kxy, ked
      includeEdge = .false.
      if (lowup .eq. 1) then
        ked = iedgelower(kpc,kxy)
      else
        ked = iedgeupper(kpc,kxy)
      endif
      if (ked .eq. 0) return
      if (ifskipEdge(ked,kxy) .gt. 1) return
      includeEdge = .true.
      return
      end function includeEdge


c       Check for whether to add a piece to the current group
c
      subroutine checkGroup(kpc)
      integer*4 kpc
c      print *,'checking group of ',kpc, ivarGroup(indvar(kpc))
      if (ivarGroup(indvar(kpc)) .gt. 0) return
      ivarGroup(indvar(kpc)) = numGroups
      numToCheck = numToCheck + 1
      listCheck(numToCheck) = kpc
      return
      end subroutine checkGroup


c       Classify pieces into separate groups if any by following connections
c       between them
c
      subroutine sortVarsIntoGroups()
      numGroups = 0
      nextvar = 1
      do while (nextvar .le. nallvar)
c         
c         Look for next piece that is unclassified
        do while (nextvar .le. nallvar)
          if (ivarGroup(nextvar) .eq. 0) exit
          nextvar = nextvar + 1
        enddo
        if (nextvar .gt. nallvar) exit
c         
c         Start a new group and initialize check list
        numGroups = numGroups + 1
        ivarGroup(nextvar) = numGroups
        nextCheck = 1
        numToCheck = 1
        listCheck(1) = iallVarpc(nextvar)
c         
c         Loop on the check list, for next piece, check its 4 edges and
c         assign and add to check list other pieces not in group yet
        do while (nextCheck .le. numToCheck)
          ipc = listCheck(nextCheck)
          do ixy = 1, 2
            if (includeEdge(1, ipc, ixy, iedge))
     &          call checkGroup(ipiecelower(iedge, ixy))
            if (includeEdge(2, ipc, ixy, iedge))
     &          call checkGroup(ipieceupper(iedge, ixy))
          enddo
          nextCheck = nextCheck + 1
        enddo
        nextvar = nextvar + 1
      enddo
c      print *,'groups:',numGroups
c       
c       Have to make sure the groups are going to be done in a connected order
c       if there are more than 2
      do igroup = 1, numGroups - 2
        newGroup = 0
        do ivar = 1, nallVar
c           
c           Look at pieces in all the groups already done
          if (ivarGroup(ivar) .le. igroup) then
            ipc = iallVarpc(ivar)
c             
c             Look for an excluded edge to a piece in a higher group
            do ixy = 1, 2
              if (.not. includeEdge(1, ipc, ixy, iedge)) then
                if (iedge .gt. 0) then
                  ipclo=ipiecelower(iedge,ixy)
                  m = ivarGroup(indvar(ipclo))
                  if (m .gt. igroup) newGroup = m
                endif
              endif
              if (.not. includeEdge(2, ipc, ixy, iedge)) then
                if (iedge .gt. 0) then
                  ipclo=ipieceupper(iedge,ixy)
                  m = ivarGroup(indvar(ipclo))
                  if (m .gt. igroup) newGroup = m
                endif
              endif
            enddo
c             
c             Swap the group numbers of that group and the next one
            if (newGroup .gt. 0) then
c              print *,'swapping groups',igroup + 1,newGroup
              do i = 1, nallVar
                if (ivarGroup(i) .eq. igroup + 1) then
                  ivarGroup(i) = newGroup
                else if (ivarGroup(i) .eq. newGroup) then
                  ivarGroup(i) = igroup + 1
                endif
              enddo
              exit
            endif
          endif
        enddo
      enddo
      return
      end subroutine sortVarsIntoGroups

      end subroutine find_best_shifts


      subroutine findBestGradient(dxgridmean,dygridmean,idimEdge,idir,izsect,
     &    gradnew,rotnew)
      use blendvars
      implicit none
      integer*4 idir,izsect,idimEdge
      real*4 gradnew,rotnew
      real*4 dxgridmean(idimedge,2),dygridmean(idimedge,2)
c       
c       Stuff for amoeba: ftol2 and ptol2 are used the FIRST time
c       
      integer nvar
      parameter (nvar = 2)
      real*4 pp(nvar+1,nvar+1),yy(nvar+1),ptol(nvar), da(nvar)
      real*4 ptol1, ftol1,ptol2,ftol2,delfac,var(nvar)
      data da/0.5,0.2/
      integer*4 jmin, iter, i
      external gradfunc
c       
      integer*4 nedg, ifTrace, nTrial,izedge
      real*4 errMin
      common /funccom/nedg, ifTrace, nTrial, izedge, errMin
c       
      integer*4 ipc,ipclo,ixy,iedge

      ifTrace = 0
      ptol1 = 1.e-5
      ftol1 = 1.e-5
      ptol2 = 1.e-3
      ftol2 = 1.e-3
      delfac = 2.

      izedge = izsect
      nedg = 0
      do ipc=1,npclist
        if (izpclist(ipc).eq.izsect .and. (iedgelower(ipc,1).gt.0.or.
     &      iedgelower(ipc,2).gt.0)) then
          do ixy=1,2
            iedge=iedgelower(ipc,ixy)
            if (iedge.gt.0) then
              nedg=nedg+1
              ipclo=ipiecelower(iedge,ixy)
c               
c               Get the residual at the edge: the amount the upper piece
c               is still displaced away from alignment with lower
c               
              dxedge(iedge,ixy)=idir*dxgridmean(iedge,ixy)
              dyedge(iedge,ixy)=idir*dygridmean(iedge,ixy)
c               
c               Get center for gradient in each piece
c               
              if (focusAdjusted) then
                gradXcenLo(nedg) = nxin / 2.
                gradYcenLo(nedg) = nyin / 2.
                gradXcenHi(nedg) = nxin / 2.
                gradYcenHi(nedg) = nyin / 2.
              else
                gradXcenLo(nedg) = (minxpiece + nxpieces * (nxin - nxoverlap)
     &              + nxoverlap) / 2. - ixpclist(ipclo)
                gradYcenLo(nedg) = (minypiece + nypieces * (nyin - nyoverlap)
     &              + nyoverlap) / 2. - iypclist(ipclo)
                gradXcenHi(nedg) = (minxpiece + nxpieces * (nxin - nxoverlap)
     &              + nxoverlap) / 2. - ixpclist(ipc)
                gradYcenHi(nedg) = (minypiece + nypieces * (nyin - nyoverlap)
     &              + nyoverlap) / 2. - iypclist(ipc)
              endif
c               
c               Get center point of overlap zone in each piece
c               
              if (ixy .eq. 1) then
                overXcenLo(nedg) = nxin - nxoverlap/2
                overXcenHi(nedg) = nxoverlap/2
                overYcenLo(nedg) = nyin/2
                overYcenHi(nedg) = nyin/2
              else
                overYcenLo(nedg) = nyin - nyoverlap/2
                overYcenHi(nedg) = nyoverlap/2
                overXcenLo(nedg) = nxin/2
                overXcenHi(nedg) = nxin/2
              endif

c              write(*, '(i2,2i4,2f6.1, 8f7.0)')ixy,ipclo,ipc,dxedge(iedge,ixy),
c     &            dyedge(iedge,ixy), overXcenLo(nedg),overYcenLo(nedg),
c     &            overXcenHi(nedg),overYcenHi(nedg),gradXcenLo(nedg),
c     &            gradYcenLo(nedg),gradXcenHi(nedg),gradYcenHi(nedg)
            endif
          enddo
        endif
      enddo             

c       
c       set up for minimization
c       
      errMin = 1.e30
      nTrial = 0
      var(1) = 0.
      var(2) = 0.
      call amoebaInit(pp, yy, nvar + 1, nvar, delfac, ptol2, var, da, 
     &    gradfunc,ptol)
      call amoeba(pp,yy,nvar+1,nvar,ftol2,gradfunc,iter,ptol,jmin)
c       
c       per Press et al. recommendation, just restart at current location
c       
      do i=1,nvar
        var(i)=pp(jmin,i)
      enddo
      call amoebaInit(pp, yy, nvar + 1, nvar, delfac / 4., ptol1, var, da, 
     &    gradfunc,ptol)
      call amoeba(pp,yy,nvar+1,nvar,ftol1,gradfunc,iter,ptol,jmin)
c       
c       recover result
c       
      do i = 1, nvar
        var(i) = pp(jmin, i)
      enddo
      call gradfunc(var, errMin)
      write(*,73)var(1),var(2),errMin
73    format(' Implied incremental gradient:',2f9.4,'  mean error:',f10.4)
      call flush(6)
      gradnew = var(1)
      rotnew = var(2)
      return
      end


      subroutine gradfunc(p, funcErr)
      use blendvars
      implicit none
      real*4 p(*), funcErr

      integer*4 nedg, ifTrace, nTrial,izedge
      real*4 errMin
      common /funccom/nedg, ifTrace, nTrial, izedge, errMin
c       
      real*4 tiltang,errSum,dxlo,dxhi,dylo,dyhi
      real*4 bmean, bmax, aftmean, aftmax
      integer*4 ied, ixy, iedge, ipc
      character*1 starout

      tiltang = tiltAngles(min(ilistz, numAngles))
      nTrial = nTrial + 1
      errSum =0.

      ied = 0
      do ipc=1,npclist
        if (izpclist(ipc).eq.izedge .and. (iedgelower(ipc,1).gt.0.or.
     &      iedgelower(ipc,2).gt.0)) then
          do ixy=1,2
            iedge=iedgelower(ipc,ixy)
            if (iedge.gt.0) then
              ied = ied + 1
              call magGradientShift(overXcenLo(ied), overYcenLo(ied),
     &            nxin, nyin, gradXcenLo(ied), gradYcenLo(ied),
     &            pixelMagGrad, axisRot, tiltang, p(1), p(2), dxlo, dylo)
              call magGradientShift(overXcenHi(ied), overYcenHi(ied),
     &            nxin, nyin, gradXcenHi(ied), gradYcenHi(ied),
     &            pixelMagGrad, axisRot, tiltang, p(1), p(2), dxhi, dyhi)
c               
c               Point moves by negative of shift to get to undistorted image
c               Displacement changes by negative of upper shift and positive
c               of  lower shift
c               
              dxadj(iedge,ixy) = dxedge(iedge,ixy) + dxlo - dxhi
              dyadj(iedge,ixy) = dyedge(iedge,ixy) + dylo - dyhi
            endif
          enddo
        endif
      enddo

      call find_best_shifts(dxadj, dyadj, limedge, 1, izedge, htmp,
     &    iedge, bmean, bmax, aftmean, aftmax)

      funcErr = aftmean
      if (ifTrace .gt. 0) then
        starout=' '
        if(funcErr.lt.errMin)then
          starout='*'
          errMin=funcErr
        endif
        if(iftrace.gt.1.or.starout.eq.'*')
     &      write(*,72)starout,nTrial,funcErr, (p(ied), ied = 1,2)
72      format(1x,a1,i4,f15.5, 2f9.4)
        call flush(6)
      endif
c       
c       Amoeba flails on hard zeros so add something (no longer, 6/17/06)
c       
      funcErr = funcErr
      return
      end


c       findEdgeToUse looks up an edge to use in place of the given edge
c       if Z limits are defined on the edges to use.  It returns the original
c       edge number if no limits have been set, or if no substitte edge should
c       be used.  It returns 0 if a different Z level should be used but no
c       edge exists at it.
c
      subroutine findEdgeToUse(iedge, ixy, iuse)
      use blendvars
      implicit none
      integer*4 iedge, iuse, ixy, izlow, izhigh, ipclo,ixfrm,iyfrm,izuse,i
      iuse = iedge
      if (numUseEdge .eq. 0 .and. izUseDefLow .lt. 0) return
c       
c       Find frame number of piece then look up frame in the use list
      ipclo = ipiecelower(iedge, ixy)
      ixfrm=1+(ixpclist(ipclo)-minxpiece)/(nxin-nxoverlap)
      iyfrm=1+(iypclist(ipclo)-minypiece)/(nyin-nyoverlap)
      izlow = -1
      do i = 1, numUseEdge
        if (ixFrmUseEdge(i) .eq. ixfrm .and. iyFrmUseEdge(i) .eq. iyfrm .and.
     &      ixy .eq. ixyUseEdge(i)) then
          izlow = izLowUse(i)
          izhigh = izHighUse(i)
        endif
      enddo
c       
c       If frame not found, use the default or skip out if none
c       Then find out if there is another Z to use or not
      if (izlow .lt. 0) then
        if (izUseDefLow .lt. 0) return
        izlow = izUseDefLow
        izhigh = izUseDefHigh
      endif
      if (izpclist(ipclo) .lt. izlow) then
        izuse = izlow
      else if (izpclist(ipclo) .gt. izhigh) then
        izuse = izhigh
      else
        return
      endif
c       
c       Seek these coordinates on that Z with an edge above
      do i = 1, npclist
        if (izpclist(i) .eq. izuse .and. ixpclist(i) .eq. ixpclist(ipclo) .and.
     &      iypclist(i) .eq. iypclist(ipclo) .and. iedgeUpper(i,ixy) .gt. 0)
     &      then
          iuse = iedgeUpper(i,ixy)
          return
        endif
      enddo
      iuse = 0
      return
      end


c       getDataLimits returns the limits of actual data (as opposed to gray
c       fill data) in the dimension given by IXY (1 for X, 2 for Y) and for
c       an edge on the side given by LOHI (1 for lower edge, 2 for upper edge).
c       It returns coordinates numbered from 0 in limitLo and limitHi, looking
c       them up in the limDataLo and limDataHi arrays or finding the values and
c       storing them in those arrays when done.
c
      subroutine getDataLimits(ipc, ixy, lohi, limitLo, limitHi)
      use blendvars
      implicit none
      integer*4 ipc, ixy, limitLo, limitHi, ind, incPix, incLine, numPix, line
      integer*4 lineEnd,lineStart,idir,i, lineBase, lineGood, maxSame,numSame
      integer*4 limInd, lohi, iPixStr, iPixEnd
      real*4 value
c       
      limInd = limDataInd(ipc)
      if (limInd .le. 0) then
        limitLo = 0
        limitHi = nxin - 1
        if (ixy .eq. 2) limitHi = nyin - 1
        return
      endif
      if (limDataLo(limInd, ixy, lohi) .ge. 0 .and.
     &    limDataHi(limInd, ixy, lohi) .ge. 0) then
        limitLo = limDataLo(limInd, ixy, lohi)
        limitHi = limDataHi(limInd, ixy, lohi)
        return
      endif
      call shuffler(ipc, ind)
      
      lineEnd = 0
      if (ixy .eq. 1) then
        incPix = nxin
        incLine = 1
        numPix = min(nyin, (3 * nyoverlap) / 2)
        iPixEnd = nyin - 1
        lineStart = nxin - 1
      else
        incPix = 1
        incLine = nxin
        numPix = min(nxin, (3 * nxoverlap) / 2)
        iPixEnd = nxin - 1
        lineStart = nyin - 1
      endif
      if (lohi .eq. 1) then
        iPixStr = 0
        iPixEnd = numPix - 1
      else
        iPixStr = iPixEnd + 1 - numPix
      endif
      maxSame = numPix / 4
      do idir = -1,1,2
c         
c         First find out if first line has a common value
        i = iPixStr + 1
        lineBase = ind + lineStart * incLine
        value = array(lineBase + iPixStr * incPix)
        do while (i .le. iPixEnd)
          if (array(lineBase + i * incPix) .ne. value) exit
          i = i + 1
        enddo
        if (i .le. iPixEnd) then
          lineGood = lineStart
        else
c         
c           If it made it to the end, next search for line with less than 
c           maximum number of pixels at this value
          lineGood = -1
          line = lineStart + idir
          do while (lineGood .lt. 0 .and. idir * (line - lineEnd) .lt. 0)
            i = iPixStr
            numSame = 0
            lineBase = ind + line * incLine
            do while (i .le. iPixEnd .and. numSame .lt. maxSame)
              if (array(lineBase + i * incPix) .eq. value) numSame = numSame +1
              i = i + 1
            enddo
            if (numSame .lt. maxSame) lineGood = line
            line = line + idir
          enddo
        endif
c         
c         If go to end, set it to next to last line, then save limit
        if (lineGood .lt. 0) lineGood = lineEnd - idir
        
        if (idir .eq. -1) then
          limitHi = lineGood
        else
          limitLo = lineGood
        endif
        lineEnd = lineStart
        lineStart = 0
      enddo
      limDataLo(limInd, ixy, lohi) = limitLo
      limDataHi(limInd, ixy, lohi) = limitHi
      return
      end


c       iwrBinned writes the data in ARRAY to unit IUNIT, with binning
c       given by IBINNING, using BRRAY as a scratch line.  The data in ARRAY
c       are NY lines of length NX.  The output will consist of NYOUT lines
c       of length NXOUT.  IXST and IYST are possible starting pixels, where
c       negative numbers are used to indicate non-existent pixels.
c       The routine also maintains the min and max density in DMIN and DMAX
c       and adds to the sum of densities in DSUM8.
c       
      subroutine iwrBinned(iunit, array, brray, nx, nxout, ixst, ny, nyout,
     &    iyst, iBinning, dmin, dmax, dsum8)
      implicit none
      integer*4 nx, nxout, ixst, ny, nyout, iyst, iBinning, iunit
      real*4 array(nx,ny),brray(*),sum,dmin,dmax
      integer*4 ix, iy, jx, jy, jxbase, jybase,nsum
      real*8 dsum8
c       
      if (iBinning .eq. 1) then
        do iy = 1, nyout
          do ix = 1, nxout
            dmin = min(dmin, array(ix,iy))
            dmax = max(dmax, array(ix,iy))
            dsum8 = dsum8 + array(ix,iy)
          enddo
          call parWrtLin(iunit, array(1,iy))
        enddo
        return
      endif

      do iy = 1, nyout
        jybase = (iy - 1) * iBinning + iyst
        do ix = 1, nxout
          jxbase = (ix - 1) * iBinning + ixst
          sum = 0.
          if (ix .eq. 1 .or. ix .eq. nxout .or. iy .eq. 1 .or. iy .eq. nyout)
     &        then
            nsum = 0
            do jy = max(1, jybase + 1), min(ny ,jybase + iBinning)
              do jx = max(1, jxbase + 1), min(nx, jxbase + iBinning)
                sum = sum + array(jx, jy)
                nsum = nsum + 1
              enddo
            enddo
            brray(ix) = sum / nsum
          else
            do jy = jybase + 1, jybase + iBinning
              do jx = jxbase + 1, jxbase + iBinning
                sum = sum + array(jx, jy)
              enddo
            enddo
            brray(ix) = sum / iBinning**2
          endif
          dmin = min(dmin, brray(ix))
          dmax = max(dmax, brray(ix))
          dsum8 = dsum8 + brray(ix)
        enddo
        call parWrtLin(iunit, brray)
      enddo
      return
      end


c       getExtraIndents computes a border or indent for correlations and
c       edge functions when there are distortion corrections, based upon the
c       maximum  distortion vector along any edge of the two pieces, whose
c       numbers are given by IPCLOW and IPCUP.  IXY specifies the direction
c       of the overlap zone.  The indents in X and Y are returned in
c       delIndent.
c       
      subroutine getExtraIndents(ipclow, ipcup, ixy, delIndent)
      use blendvars
      implicit none
      integer*4 ipclow, ipcup, ixy, ix, memlow, memup, iy
      real*4 delIndent(2)
c       
      delIndent(1) = 0.
      delIndent(2) = 0.
      if (.not. doFields) return
      memlow = memIndex(ipclow)
      memup = memIndex(ipcup)
c       
c       The undistorted image moves in the direction opposite to the
c       field vector, so positive vectors at the right edge of the lower
c       piece move the border in to left and require more indent in
c       short direction.
c       
      if (ixy .eq. 1) then
        do iy = 1, nyField
          delIndent(1) = max(delIndent(1), fieldDx(nxField, iy, memlow),
     &        -fieldDx(1, iy, memup))
        enddo
        delIndent(2) = max(0., -fieldDy(nxField, 1, memlow),
     &      -fieldDy(1, 1, memup), fieldDy(nxField, nyField, memlow),
     &      fieldDy(1, nyField, memup))
      else
        do ix = 1, nxField
          delIndent(1) = max(delIndent(1), fieldDy(ix, nyField, memlow),
     &        -fieldDy(ix, 1, memup))
        enddo
        delIndent(2) = max(0., -fieldDx(1, nyField, memlow),
     &      -fieldDx(1, 1, memup), fieldDx(nxField, nyField, memlow),
     &      fieldDx(nxField, 1, memup))
      endif
      return
      end

c       IBINPAK packs a portion of ARRAY into BRRAY with binning given by 
c       NBIN.  MX, MY are the dimensions of ARRAY, NX1, NX2, NY1, and NY2
c       are starting and ending indexes (numbered from 0) from which to take
c       the data
c
      subroutine ibinpak(brray, array, mx, my, nx1, nx2, ny1, ny2, nbin)
      implicit none
      integer*4 mx, my, nx1, nx2, ny1, ny2, nbin
      real*4 brray(*), array(mx, my), sum
      integer*4 nx2adj, ny2adj, ix, iy, ipx, ipy, iout

      if (nbin .eq. 1) then
        call irepak(brray,array,mx,my,nx1,nx2,ny1,ny2)
        return
      endif
      nx2adj = nbin * ((nx2 + 1 - nx1) / nbin - 1) + nx1
      ny2adj = nbin * ((ny2 + 1 - ny1) / nbin - 1) + ny1
      iout = 1
      do iy = ny1, ny2adj, nbin
        do ix = nx1, nx2adj, nbin
          sum = 0.
          do ipy = 1, nbin
            do ipx = 1, nbin
              sum = sum + array(ipx + ix, ipy + iy)
            enddo
          enddo
          brray(iout) = sum / nbin**2
          iout = iout + 1
        enddo
      enddo
      return
      end


c       Reads a model of edges to exclude, analyzes it and marks edges
c       for exclusion.  FILNAM has the name of the model, EDGEDISPX and
c       EDGEDISPY are the edge displacements (zero unless read from file) with
c       direst dimension IDIMEDGE, findEFforAdjusted is a flag that edge
c       functions should be found for excluded edges that have nonzero
c       displacements, and numSkip is returned with the number of edges skipped
c
      subroutine readExclusionModel(filnam, edgedispx, edgedispy,idimedge,
     &    ifUseAdjusted, mapAllPc, nxmap, nymap, minzpc, numSkip)
      use blendvars
      implicit none
      include 'smallmodel.inc'
      character*(*) filnam
      integer*4 nxmap, nymap, minzpc, mapAllPc(nxmap,nymap,*)
      integer*4 ixy, ied, ipc, iobj, ipt, ipnt,numEFonly,idimedge, numSkip
      integer*4 ixpc, iypc, lenx, leny, numNear, ifUseAdjusted, ixframe,iyframe
      integer*4 ixright, iytop, izpc
      real*4 edgedispx(idimedge,2), edgedispy(idimedge,2), vertex(4,2)
      logical exist, readSmallMod, inside
c
      exist=readSmallMod(filnam)
      if (.not.exist) call exitError('READING EDGE EXCLUSION MODEL FILE')
      call scaleModelToImage(1,0)
      numEFonly = 0
      numSkip = 0
      numnear = 0
c       
c       Loop on the edges, make a quadrangle for area nearest to each
      do ixy = 1, 2
        do ied = 1, nedge(ixy)
          ipc = ipieceupper(ied,ixy)
          ixpc = ixpclist(ipc)
          iypc = iypclist(ipc)
          izpc = izpclist(ipc)+1-minzpc
          lenx = nxin - nxoverlap
          leny = nyin - nyoverlap
          ixframe = 1 + (ixpc - minxpiece) / lenx
          iyframe = 1 + (iypc - minypiece) / leny
          ixright = ixpc + lenx
          iytop = iypc + leny
c           
c           WARNING: This code tracks how 3dmod lays out data when displaying
c           a montage: it copies each entire piece into the buffer in Z order
c           A piece extends farther in one direction if it is either the last
c           piece or it is laid down after the next in that direction
          if (ixframe .eq. nxpieces) then
            ixright = ixpc + nxin
          else if (mapAllPc(ixframe+1, iyframe, izpc) .lt. ipc) then
            ixright = ixpc + nxin
          endif
          if (iyframe .eq. nypieces) then
            iytop = iypc + nyin
          else if (mapAllPc(ixframe, iyframe+1, izpc) .lt. ipc) then
            iytop = iypc + nyin
          endif
c           
c           A piece starts farther in one direction if the piece before it
c           is laid down after this piece
          if (ixframe .gt. 1) then
            if (mapAllPc(ixframe-1, iyframe, izpc) .gt. ipc)
     &          ixpc = ixpc + nxoverlap
          endif
          if (iyframe .gt. 1) then
            if (mapAllPc(ixframe, iyframe-1, izpc) .gt. ipc)
     &          iypc = iypc + nyoverlap
          endif
          lenx = ixright - ixpc
          leny = iytop - iypc
c           
c           This puts the vertices on the edge at its visible ends and one 
c           vertex in the middle of this visible piece; the other vertex is 
c           just a fixed distance into the piece below
          vertex(1,1) = ixpc + lenx / 2.
          vertex(1,2) = iypc + leny / 2.
          vertex(2,1) = ixpc
          vertex(2,2) = iypc
          if (ixy .eq. 1) then
            vertex(3,1) = ixpc - (nxin - nxoverlap) / 2.
            vertex(3,2) = vertex(1,2)
            vertex(4,1) = vertex(2,1)
            vertex(4,2) = vertex(2,2) + leny
          else
            vertex(3,1) = vertex(1,1)
            vertex(3,2) = iypc - (nyin - nyoverlap) / 2.
            vertex(4,1) = vertex(2,1) + lenx
            vertex(4,2) = vertex(2,2)
          endif
c          print *,'edge',ixy,ixpc,iypc,int(vertex(4,1)),int(vertex(4,2))
c           
c           test each model point for being inside this quadrangle
          do iobj = 1, max_mod_obj
            do ipt = 1, npt_in_obj(iobj)
              ipnt = abs(object(ipt + ibase_obj(iobj)))
              if (nint(p_coord(3,ipnt)) .eq. izpclist(ipc)) then
                if (inside(vertex(1,1), vertex(1,2), 4, p_coord(1,ipnt),
     &              p_coord(2,ipnt)) .and. ifskipEdge(ied,ixy) .eq. 0) then
c                  write(*,'(3i3,10f6.0)')ied,ixy,ipc,p_coord(1,ipnt),
c     &                p_coord(2,ipnt),((vertex(i,j),j=1,2),i=1,4)
                  numNear = numNear + 1
                  ifskipEdge(ied,ixy) = 2
                  if ((edgedispx(ied,ixy) .ne. 0. .or.
     &                edgedispy(ied,ixy).ne.0.) .and. ifUseAdjusted.gt.0) then
                    ifskipEdge(ied,ixy) = 1
                    if (ifUseAdjusted .gt. 1) ifskipEdge(ied,ixy) = 0
                    numEFonly = numEFonly + 1
                  endif
                  if (ifskipEdge(ied,ixy) .gt. 0) numSkip = numSkip + 1
                endif
              endif
            enddo
          enddo
        enddo
      enddo
      if (numNear .gt. 0) then
        if (numEFonly .eq. 0) then
          write(*,'(/,i7,a,/,a)')numSkip,' edges will be given zero edge '//
     &        'functions and', '   excluded when solving for shifts'
        else if (ifUseAdjusted .gt. 1) then
          write(*,'(/,i7,a,i7,a,/,a)')numSkip,
     &      ' edges will be given zero edge functions but', numEFonly,
     &      ' others have','  non-zero displacements and edge functions '//
     &        'will be found for them'
        else
          write(*,'(/,i7,a,i7,a,/,a)')numSkip,
     &      ' edges will be given zero edge functions but', numEFonly,
     &      ' of them have','  non-zero displacements and will be '//
     &      'included when solving for shifts'
        endif
      endif
      if (n_point .gt. numNear)
     &    write(*,'(/,a,i7,a,i7,a)')'WARNING: only',numSkip,' of the ',n_point,
     &      ' model points were near an edge'
      return
      end


c       dumpEdge writes a padded image or correlation to a file.  The
c       image in CRRAY, NXDIM is the X array dimension and NXPAD and NYPAD
c       are image sizes.  IXY is 1 for X or 2 for Y edge.
c       
      subroutine dumpedge(crray,nxdim,nxpad,nypad,ixy,ifcorr)
      use blendvars
      implicit none
      integer maxline
      parameter (maxline=4096)
      integer*4 nxdim,nxpad,nypad,ixy,ifcorr
      real*4 crray(nxdim,nypad)
      real*4 title(20), scale, dmin, dmax, dmt, bline(maxline)
      integer*4 kxyz(3), ix, iy
c       
      if (ifDumpXY(ixy) .lt. 0 .or. nxpad .gt. maxline) return
      if (ifDumpXY(ixy) .eq. 0) then
        nzOutXY(ixy) = 0
        nxOutXY(ixy) = nxpad
        nyOutXY(ixy) = nypad
        kxyz(1) = nxpad
        kxyz(2) = nypad
        kxyz(3) = 0
        call icrhdr(2+ixy, kxyz, kxyz, 2, title, 0)
        ifDumpXY(ixy) = 1
      endif
      call imposn(2 + ixy, nzOutXY(ixy), 0)
      nzOutXY(ixy) = nzOutXY(ixy) + 1
      call ialsiz_sam_cel(2 + ixy, nxOutXY(ixy),nyOutXY(ixy), nzOutXY(ixy))

      call iclden(crray,nxdim,nypad,1,nxpad,1,nypad,dmin,dmax,dmt)
      scale = 255. / (dmax - dmin)
      do iy = 1, nyOutXY(ixy)
        if (iy .le. nypad) then
          if (ifcorr .eq. 0) then
            do ix = 1, nxOutXY(ixy)
              bline(ix) = scale *(crray(min(ix,nxpad),iy) - dmin)
            enddo
          else
            do ix = 1, nxOutXY(ixy)
              bline(ix) = scale *(crray(min(mod(ix+nxpad/2-1,nxpad)+1,nxpad),
     &            min(mod(iy+nypad/2-1,nypad)+1, nypad)) - dmin)
            enddo
          endif
        endif
        call iwrlin(2+ixy, bline)
      enddo
      if (ifcorr .ne. 0) then
        ix = (ixpclist(ipcBelowEdge)-minxpiece)/(nxin-nxoverlap)
        iy = (iypclist(ipcBelowEdge)-minypiece)/(nyin-nyoverlap)
        if (ixy .eq. 1) ix = iy * (nxpieces - 1) + ix + 1
        if (ixy .eq. 2) ix = ix * (nypieces - 1) + iy + 1
        write (*,'(1x,a,i4,a,i5)')char(ixy+ichar('W'))//' edge',ix,
     &      ' corr at Z',nzOutXY(ixy)
        call flush(6)
      endif
c       
      call iwrhdr(2+ixy,title,-1,0.,255.,128.)
      return
      end

