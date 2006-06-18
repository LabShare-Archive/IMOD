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
c       PEAKFIND
c       FIND_BEST_SHIFTS
c       findBestGradient
c       IWRBINNED
c       GETEXTRAINDENTS
c       DUMPEDGE
c       
c       
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       Revision 3.19  2006/02/27 15:20:20  mast
c       g77 wanted find_best_shift called with an equivalenced real*8 array
c
c       Revision 3.18  2006/02/26 18:29:50  mast
c       Converted to using double precision for solving shift equations and 
c       only used as much of the array as necessary for this.
c
c       Revision 3.17  2006/02/26 06:04:40  mast
c       Fixed countedges to go to the right piece when there are h transforms
c
c       Revision 3.16  2006/02/06 21:51:04  mast
c       Fixed findBestGradient to solve for shifts at each trial gradient
c
c       Revision 3.15  2006/01/16 03:16:26  mast
c       Changed message for implied gradients
c       
c       Revision 3.14  2005/11/09 05:56:47  mast
c       Added parameters for correlation control, and edge dumping
c       
c       Revision 3.13  2005/08/22 16:19:59  mast
c       Preliminary - finding gradients from displacements
c       
c       Revision 3.12  2005/08/22 16:15:59  mast
c       
c       Revision 3.11  2005/08/20 05:10:48  mast
c       Excluded a border region from correlations with distortion
c       corrections
c       
c       Revision 3.10  2005/07/24 17:33:15  mast
c       Increased allowed overlap in xcorredge up to 600 pixels but kept
c       arrays on stack
c       
c       Revision 3.9  2005/06/03 19:39:04  mast
c       Added routine for writing binned output
c       
c       Revision 3.8  2005/03/18 23:38:30  mast
c       Improved error message from read_list
c       
c       Revision 3.7  2005/02/28 22:13:27  mast
c       Commented out edge vector summary
c       
c       Revision 3.6  2005/02/28 21:15:07  mast
c       Changes for distortion and mag gradient correction and cubic and
c       linear interpolation
c       
c       Revision 3.5  2004/09/01 20:27:38  mast
c       Fixed bug in testing if piece list input entered
c       
c       Revision 3.4  2003/12/12 20:47:42  mast
c       Moved FINDEDGEFUNC, SETGRIDCHARS, and LOCALMEAN to edgesubs.f
c       
c       Revision 3.3  2003/08/09 23:21:59  mast
c       Changes for PIP input
c       
c       Revision 3.2  2003/06/20 20:18:48  mast
c       Standardized error exits and increased limits for correlation area
c       
c       Revision 3.1  2002/08/19 04:27:43  mast
c       Changed to use blend.inc.  Made declarations for implicit none in
c       all routines that used the include file.  Changed DOEDGE to use
c       ARRAY from common, and made FIND_BEST_SHIFTS get its big array as
c       an argument then invalidate the part of the array that it uses.
c       
c       
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
      integer*4 lnblnk
c       
c       get file name and open file
c       
      if (pipinput) then
        if (PipGetString('PieceListInput', filnam) .ne. 0) call errorexit
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
      do while(lenact.gt.0.and.(dummy(lenact:lenact).eq.' '.or.
     &    dummy(lenact:lenact).eq.char(0)))
        lenact=lenact-1
      enddo
      if(lenact.eq.0)go to 12
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
     &    filnam(1:lnblnk(filnam)),' - ', errmess(ierr),npclist+1
      close(3)
      call exit(1)
      end



c       function ONEINTRP used interpolation in a real array CRRAY
c       (dimensions NX,NY) to find the pixel value of the point at
c       coordinates (X,Y).  Coordinates run from 0 to NX-1.  A real value
c       is returned.  If the pixel is not within the array, DMEAN is returned
c       (from value in common).  IZPC is the piece number (numbered from 1)
c       The interpolation order is set from the value in common.
c       
      real*4 function oneintrp(crray,nx,ny,x,y,izpc)
c       
      implicit none
      include 'blend.inc'
      integer*4 nx, ny,izpc
      real*4 x,y
      real*4 crray(nx,ny)
      integer*4 ixp,iyp,ixpp1,ixpm1,iypp1,iypm1,ixpp2,iypp2
      real*4 dx,dy,xp,yp,v2,v4,v6,v8,v5,a,b,c,d,vmin,vmax
      real*4 dxm1,dxdxm1,fx1,fx2,fx3,fx4,dym1,dydym1,v1,v3
c       
      oneintrp=dmean
      xp=x+1.
      yp=y+1.
      if (doFields) then
        call interpolateGrid(xp, yp, fieldDx(1,1,memIndex(izpc)),
     &      fieldDy(1,1,memIndex(izpc)), lmField, nxField, nyField,
     &      ixFieldstrt, xFieldIntrv, iyFieldStrt, yFieldIntrv, dx, dy)
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
c       filled with DMEAN from common.  IZPC is the piece number.
c       
      subroutine fastinterp(drray,nxd,nyd, crray,nxc,nyc,indxlo,
     &    indxhi,indylo, indyhi,newpcxll,amat,fdx, fdy,izpc)
c       
      implicit none
      include 'blend.inc'
      integer*4 nxd,nyd,nxc,nyc,indxlo, indxhi,indylo, indyhi
          integer*4 newpcxll,izpc
          real*4 drray(nxd,nyd)
          real*4 crray(nxc,nyc)
          real*4 amat(2,2),dx,dy,fdx,fdy,xbase,ybase
          integer*4 indy,iyout,indx,ixout,ixp,iyp,ixpp1,ixpm1,iypp1,iypm1
          integer*4 ixpp2,iypp2
          real*4 pixval,xp,yp,v2,v4,v6,v8,v5,a,b,c,d,vmin,vmax
          real*4 dxm1,dxdxm1,fx1,fx2,fx3,fx4,dym1,dydym1,v1,v3
c           
          do indy=indylo,indyhi
            iyout=indy+1-indylo
            xbase = amat(1,2)*indy+fdx
            ybase = amat(2,2)*indy+fdy
            if (interpOrder .le. 1) then
c               
c               Linear interpolation
c               
              do indx=indxlo,indxhi
                ixout=indx+1-newpcxll
                pixval=dmean
                xp=amat(1,1)*indx+xbase
                yp=amat(2,1)*indx+ybase
                if (doFields) then
                  call interpolateGrid(xp, yp, fieldDx(1,1,memIndex(izpc)),
     &                fieldDy(1,1,memIndex(izpc)), lmField, nxField, nyField,
     &                ixFieldstrt, xFieldIntrv, iyFieldStrt, yFieldIntrv, dx, dy)
                  xp = xp + dx
                  yp = yp + dy
                endif
                IXP = XP
                IYP = YP
                IF (IXP .ge. 1 .and. IXP .lt. NXC .and. IYP .ge. 1 .and.
     &              IYP .lt. NYC) then
                  DX = XP - IXP
                  DY = YP - IYP
                  IXPP1 = IXP + 1
                  IYPP1 = IYP + 1
                  pixval = (1. - dy) * ((1. - dx) * crray(ixp, iyp) +
     &                dx * crray(ixpp1, iyp)) +
     &                dy * ((1. - dx) * crray(ixp, iypp1) +
     &                dx * crray(ixpp1, iypp1))
                endif
                drray(ixout,iyout)=pixval             
              enddo
c               
            elseif (interpOrder .eq. 2) then
c               
c               Old quadratic interpolation
c               
              do indx=indxlo,indxhi
                ixout=indx+1-newpcxll
                pixval=dmean
                xp=amat(1,1)*indx+xbase
                yp=amat(2,1)*indx+ybase
                if (doFields) then
                  call interpolateGrid(xp, yp, fieldDx(1,1,memIndex(izpc)),
     &                fieldDy(1,1,memIndex(izpc)), lmField, nxField, nyField,
     &                ixFieldstrt, xFieldIntrv, iyFieldStrt, yFieldIntrv, dx, dy)
                  xp = xp + dx
                  yp = yp + dy
                endif
                ixp=nint(xp)
                iyp=nint(yp)
                if(ixp.lt.1.or.ixp.gt.nxc.or.iyp.lt.1.or.iyp.gt.nyc)
     &              go to 80
C                 
C                 Do quadratic interpolation
C                 
                DX = XP - IXP
                DY = YP - IYP
                v5=crray(ixp,iyp)
c                 
c                 but if on an integer boundary already, done
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
C                 Set up terms for quadratic interpolation
C                 
                V2 = CRRAY(IXP, IYPM1)
                V4 = CRRAY(IXPM1, IYP)
                V6 = CRRAY(IXPP1, IYP)
                V8 = CRRAY(IXP, IYPP1)
c                 
c                 find min and max of all 5 points
c                 
                vmax=max(v2,v4,v5,v6,v8)
                vmin=min(v2,v4,v5,v6,v8)
C                 
                A = (V6 + V4)*.5 - V5
                B = (V8 + V2)*.5 - V5
                C = (V6 - V4)*.5
                D = (V8 - V2)*.5
C                 
c                 limit the new density to between min and max of original points
c                 
                pixval = max(vmin,min(vmax,
     &              A*DX*DX + B*DY*DY + C*DX + D*DY + V5))
80              drray(ixout,iyout)=pixval
              enddo
            else
c               
c               cubic interpolation
c               
              do indx=indxlo,indxhi
                ixout=indx+1-newpcxll
                pixval=dmean
                xp=amat(1,1)*indx+xbase
                yp=amat(2,1)*indx+ybase
                if (doFields) then
                  call interpolateGrid(xp, yp, fieldDx(1,1,memIndex(izpc)),
     &                fieldDy(1,1,memIndex(izpc)), lmField, nxField, nyField,
     &                ixFieldstrt, xFieldIntrv, iyFieldStrt, yFieldIntrv, dx, dy)
                  xp = xp + dx
                  yp = yp + dy
                endif
                IXP = XP
                IYP = YP
                IF (IXP .ge. 2 .and. IXP .lt. NXC - 1 .and. IYP .ge. 2 .and.
     &              IYP .lt. NYC - 1) then

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
     &                fx3*crray(ixpp1,iypm1)+fx4*crray(ixpp2,iypm1)
                  v2=fx1*crray(ixpm1,iyp)+fx2*crray(ixp,iyp)+
     &                fx3*crray(ixpp1,iyp)+fx4*crray(ixpp2,iyp)
                  v3=fx1*crray(ixpm1,iypp1)+fx2*crray(ixp,iypp1)+
     &                fx3*crray(ixpp1,iypp1)+fx4*crray(ixpp2,iypp1)
                  v4=fx1*crray(ixpm1,iypp2)+fx2*crray(ixp,iypp2)+
     &                fx3*crray(ixpp1,iypp2)+fx4*crray(ixpp2,iypp2)
                  pixval=-dym1*dydym1*v1+(1.+dy**2*(dy-2.))*v2+
     &                dy*(1.-dydym1)*v3 +dy*dydym1*v4
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
      implicit none
      include 'blend.inc'
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
      implicit none
      include 'blend.inc'
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
     &    norder,nskip,docross,xcreadin,xclegacy,edgedispx,edgedispy)
c       
      implicit none
      include 'blend.inc'
c       real*4 array(*)
      integer*4 nfit(2),nskip(2)
      logical docross,xcreadin,xclegacy
      integer*4 iedge,ixy,norder
      real*4 sdcrit,devcrit
c       
      logical edgedone(limedge,2)
      real*4 edgedispx(limedge,2),edgedispy(limedge,2)
c       
      integer limpneg
      parameter (limpneg=20)
      integer*4 multcoord(limpneg),multedge(limpneg),multmp(limpneg)
     &    ,mcotmp(limpneg),igrstr(2),igrofs(2)
      real*4 dxgrid(ixgdim,iygdim),dygrid(ixgdim,iygdim)
      real*4 ddengrid(ixgdim,iygdim),sdgrid(ixgdim,iygdim)
c       
      integer*4 intxgrid,intygrid,nmult,intscan,ipclo,ipcup,ipc,mltco
      integer*4 i,j,itmp,midcoord,mindiff,imult,imid,middone,indlow
      integer*4 indup,ixdisp,iydisp,ixdispmid,iydispmid,lastedge
      integer*4 lastxdisp,lastydisp,idiff,jedge,nxgr,nygr,ix,iy,indentXcorr
      real*4 xdisp,ydisp,theta,dxedge,dyedge,dxmid,dymid,xdispl,ydispl
      real*4 costh,sinth,xrel,yrel,thetamid,delIndent(2)
      integer*4 memlow,memup,indentUse(2)
      real*4 cosd,sind
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
              indentXcorr = 0
              if (delIndent(ixy) .gt. 0.)
     &            indentXcorr = int(delIndent(ixy)) + 1
              call xcorredge(array(indlow),array(indup),
     &            ixy,xdisp,ydisp,xclegacy,indentXcorr)
              edgedispx(jedge,ixy)=xdisp
              edgedispy(jedge,ixy)=ydisp
            endif
            ixdisp=nint(xdisp)
            iydisp=nint(ydisp)
c             write(*,'(1x,a,2i4,a,2i5)') char(ixy+ichar('W'))//' edge, pieces'
c             &       ,ipiecelower(jedge,ixy),ipieceupper(jedge,ixy),
c             &           '  ixydisp:',ixdisp,iydisp
          endif
          ixdispmid=ixdisp
          iydispmid=iydisp
c           
        else
          if(imult.eq. middone+1)then
c             
            theta=thetamid                      !at midway point, restore
            dxedge=dxmid                        !values from first (middle)
            dyedge=dymid                        !edge
            lastedge=multedge(1)
            lastxdisp=ixdispmid
            lastydisp=iydispmid
          else
            call edge_to_rotrans(dxgrid,dygrid,ixgdim,iygdim,nxgr,
     &          nygr,intxgrid,intygrid,theta,dxedge,dyedge)
            if(imult.eq.2)then
              thetamid=theta                    !if that was first edge, save
              dxmid=dxedge                      !the value
              dymid=dyedge
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
          xrel=xdispl*costh - ydispl*sinth + dxedge - xdispl
          yrel=xdispl*sinth + ydispl*costh + dyedge - ydispl
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
        call setgridchars(nxyzin,noverlap,iboxsiz,indentUse,intgrid,
     &      ixy,ixdisp,iydisp,nxgr,nygr,igrstr,igrofs)
        if (nxgr .gt. ixgdim .or. nygr .gt. iygdim) call errorexit(
     &      'TOO MANY GRID POINTS FOR ARRAYS, TRY INCREASING GridSpacing')
        lastxdisp=ixdisp
        lastydisp=iydisp
c         
c         write(*,'(1x,a,2i4,a,2i4,a,2i5,a,2i4)')
c         &           char(ixy+ichar('W'))//' edge, pieces'
c         &           ,ipiecelower(jedge,ixy),ipieceupper(jedge,ixy),
c         &           '  ngrid:',nxgr,nygr,'  start lower:',igrstr,
c         &           '  upper:',igrofs
        call findedgefunc(array(indlow),array(indup),nxin,nyin,
     &      igrstr(1),igrstr(2),igrofs(1),igrofs(2),nxgr,nygr,
     &      intxgrid,intygrid,iboxsiz(ixy),iboxsiz(3-ixy),intscan,
     &      dxgrid, dygrid,sdgrid, ddengrid,ixgdim,iygdim)
c         
        call smoothgrid(dxgrid,dygrid,sdgrid,ddengrid,ixgdim,
     &      iygdim,nxgr,nygr,sdcrit, devcrit,nfit(ixy),nfit(3-ixy),
     &      norder, nskip(ixy),nskip(3-ixy))
c         
        write(iunedge(ixy),rec=jedge+1)nxgr,nygr,(igrstr(i),igrofs(i)
     &      ,i=1,2) ,((dxgrid(ix,iy),dygrid(ix,iy),
     &      ddengrid(ix,iy),ix=1,nxgr),iy=1,nygr)
c         
c$$$      xrel = 0.
c$$$      yrel = 0.
c$$$      do ix = 1,nxgr
c$$$      do iy = 1,nygr
c$$$      costh = sqrt(dxgrid(ix,iy)**2 + dygrid(ix,iy)**2)
c$$$      xrel = xrel + costh
c$$$      yrel = max(yrel, costh)
c$$$      enddo
c$$$      enddo
c$$$      write(*,'(1x,a,2i4,a,2f6.2)')
c$$$      &           char(ixy+ichar('W'))//' edge, pieces'
c$$$      &           ,ipiecelower(jedge,ixy),ipieceupper(jedge,ixy),
c$$$      &           '  mean, max vector:',xrel/(nxgr*nygr), yrel

        edgedone(jedge,ixy)=.true.
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
      subroutine countedges(indx,indy,xg,yg)
c       
      implicit none
      integer*4 indx,indy
      real*4 xg,yg
c       
      include 'blend.inc'
c       
      logical edgeonlist,needcheck(5,2),ngframe
      real*4 xycur(2)
      integer*4 ixframe,iyframe,ipc,ixfrm,iyfrm,minxframe,minyframe
      integer*4 indinp,newedge,newpiece,iflo,listno,ixy,i, idSearch
      real*4 xtmp,xframe,yframe,ytmp,xbak,ybak,distmin,xttmp,dist
c       
      numpieces=0
      numedges(1)=0
      numedges(2)=0
      idSearch = 1
c       
c       get frame # that it is nominally in: actual one or nearby valid frame
c       
      xg=indx
      yg=indy
      if(dogxforms)then
        xtmp=xg
        xg=ginv(1,1)*xtmp+ginv(1,2)*yg+ginv(1,3)
        yg=ginv(2,1)*xtmp+ginv(2,2)*yg+ginv(2,3)
      endif
      xframe=(xg-minxpiece-nxoverlap/2)/(nxin-nxoverlap)
      yframe=(yg-minypiece-nyoverlap/2)/(nyin-nyoverlap)
      ixframe=xframe+1.                         !truncation gives proper frame
      iyframe=yframe+1.
      ngframe=ixframe.lt.1.or.ixframe.gt.nxpieces.or.
     &    iyframe.lt.1.or.iyframe.gt.nypieces.or.
     &    mappiece(max(1,ixframe),max(1,iyframe)).eq.0
      if(multng)then
c         
c         if there are multineg h's, need to make sure point is actually in
c         frame, but if frame no good, switch to nearest frame first
c         
        if (ngframe) call findNearestPiece(ixframe, iyframe)
        ipc=mappiece(ixframe,iyframe)
        xtmp=xg-ixpclist(ipc)
        ytmp=yg-iypclist(ipc)
        xbak=hinv(1,1,ipc)*xtmp+hinv(1,2,ipc)*ytmp +hinv(1,3,ipc)
        ybak=hinv(2,1,ipc)*xtmp+hinv(2,2,ipc)*ytmp +hinv(2,3,ipc)
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
          if (ixframe.lt.1.or.ixframe.gt.nxpieces.or.
     &        iyframe.lt.1.or.iyframe.gt.nypieces.or.
     &        mappiece(max(1,ixframe),max(1,iyframe)).eq.0) then
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
              xtmp=xg-ixpclist(ipc)
              ytmp=yg-iypclist(ipc)
              if(multng)then
                xttmp=xtmp
                xtmp=hinv(1,1,ipc)*xttmp+hinv(1,2,ipc)*ytmp +hinv(1,3,ipc)
                ytmp=hinv(2,1,ipc)*xttmp+hinv(2,2,ipc)*ytmp +hinv(2,3,ipc)
              endif
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
        if(distmin.eq.1.e10)return
        ixframe=minxframe
        iyframe=minyframe
      endif
c       
c       initialize list of pieces with this piece # on it, then start looping
c       over the pieces present in the list
c       
      numpieces=1
      inpiece(1)=mappiece(ixframe,iyframe)
      indinp=1
      needcheck(1,1)=.true.
      needcheck(1,2)=.true.
      do while (indinp.le.numpieces)
c         
c         come into this loop looking at a piece onlist; need to get true
c         coordinates in piece and see if point is near/in an edge to another
c         Start by translating into coordinates in piece
c         
        ipc=inpiece(indinp)
        xycur(1)=xg-ixpclist(ipc)
        xycur(2)=yg-iypclist(ipc)
c         
c         if there are multineg h's, need to compute (again!) the
c         location within the piece
c         
        if(multng)then
          xtmp=xycur(1)
          xycur(1)=hinv(1,1,ipc)*xtmp+hinv(1,2,ipc)*xycur(2) +hinv(1,3,ipc)
          xycur(2)=hinv(2,1,ipc)*xtmp+hinv(2,2,ipc)*xycur(2) +hinv(2,3,ipc)
        endif
        xinpiece(indinp)=xycur(1)
        yinpiece(indinp)=xycur(2)
c         
c         check the x and y directions to see if point is near an edge
c         
        do ixy=1,2
          if(needcheck(indinp,ixy))then
            newedge=0
            if(xycur(ixy).lt.edgelonear(ixy).and.
     &          iedgelower(ipc,ixy).gt.0)then
              newedge=iedgelower(ipc,ixy)
              newpiece=ipiecelower(newedge,ixy)
              iflo=1
            endif
            if(xycur(ixy).gt.edgehinear(ixy).and.
     &          iedgeupper(ipc,ixy).gt.0)then
              newedge=iedgeupper(ipc,ixy)
              newpiece=ipieceupper(newedge,ixy)
              iflo=0
            endif
c             
c             if either check picked up a new edge, see if edge is on list
c             already
c             
            if(newedge.ne.0)then
              edgeonlist=.false.
              do i=1,numedges(ixy)
                edgeonlist=edgeonlist.or.(inedge(i,ixy).eq.newedge)
              enddo
c               
c               if not, add it, and the implied piece, to list
c               
              if(.not.edgeonlist)then
                listno=0
                do i=1,numpieces
                  if(newpiece.eq.inpiece(i))listno=i
                enddo
                if(listno.eq.0)then
c                   
c                   but if adding a new piece, check for point actually in
c                   piece first
c                   
                  xbak=xg-ixpclist(newpiece)
                  ybak=yg-iypclist(newpiece)
                  if(multng)then
                    xtmp=xbak
                    xbak=hinv(1,1,newpiece)*xtmp+ hinv(1,2,newpiece)*ybak
     &                  +hinv(1,3,newpiece)
                    ybak=hinv(2,1,newpiece)*xtmp+ hinv(2,2,newpiece)*ybak
     &                  +hinv(2,3,newpiece)
                  endif
                  if(xbak.ge.0.and.xbak.le.nxin-1.and.
     &                ybak.ge.0.and.ybak.le.nyin-1)then
                    numpieces=numpieces+1
                    inpiece(numpieces)=newpiece
                    needcheck(numpieces,ixy)=.false.
                    needcheck(numpieces,3-ixy)=.true.
                    listno=numpieces
                  endif
                endif
c                 
c                 add edge to list only if legal piece found
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
        indinp=indinp+1
      enddo
      return
      end


c       initNearList sets up an ordered list of dx, dy values to nearby pieces 
c       up to the maximum distance in maxDistNear
c
      subroutine initNearList()
      implicit none
      include 'blend.inc'
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
      implicit none
      include 'blend.inc'
      integer*4 ixFrame, iyFrame, i, ixnew, iynew
      ixFrame = max(1, min(nxpieces, ixFrame))
      iyFrame = max(1, min(nypieces, iyFrame))
      if (mappiece(ixFrame, iyFrame) .ne. 0) return
      do i = 1, numPcNear
        ixnew = ixFrame + idxPcNear(i)
        iynew = iyFrame + idyPcNear(i)
        if (ixnew .ge. 1 .and. ixnew .le. nxpieces .and. iynew .ge. 1 .and. 
     &      iynew .le. nypieces .and. mappiece(ixnew, iynew) .ne. 0) then
          ixFrame = ixnew
          iyFrame = iynew
          return
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
      implicit none
      real*4 x1,x2,y1,y2,dden
      integer*4 indedg
      include 'blend.inc'
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


      subroutine xcorredge(crray,drray,ixy,xdisp,ydisp, legacy, indentXC)
      implicit none
      include 'blend.inc'
      real*4 crray(*),drray(*),xdisp,ydisp
      integer*4 indentXC,ixy
      logical legacy
      integer*4 nxybox(2),ind0(2),ind1(2),idispl(2), nxybord(2)
      real*4 ctf(8193),rdispl(2)
      real*4 overfrac,delta,xpeak,ypeak,peak,sdmin,ddenmin
      integer*4 indentSD,niter,limstep,iyx,nxpad,nypad,nxtap, indentUse
      integer*4 nytap,jx,ixdispl,iydispl,i,nExtra(2)
      integer*4 niceframe

      indentSD=5                                !indent for sdsearch
      overfrac=0.9                              !fraction of overlap to use
      niter=4                                   !iterations for sdsearch
      limstep=10                                !limiting distance
c       
c       find size and limits of box in overlap zone to cut out
c       
      iyx=3-ixy
      indentUse = min(indentXC, (noverlap(ixy) - 8) / 2)
      indentSD = indentSD + indentUse
      nxybox(ixy)=noverlap(ixy) - indentUse * 2
      nxybox(iyx)=min(nxyzin(iyx), int(aspectmax*noverlap(ixy)))
c       nxybox(iyx)=nxyzin(iyx)
      nExtra(iyx)=0
      nExtra(ixy) = 2 * (nint(extraWidth * nxybox(ixy)) / 2)
      nxybox(ixy) = nxybox(ixy) + nExtra(ixy)
      ind0(iyx)=nxyzin(iyx)/2 - nxybox(iyx)/2 
      ind1(iyx)=nxyzin(iyx)/2 + nxybox(iyx)/2 - 1
      ind0(ixy)=nxyzin(ixy) - noverlap(ixy) + indentUse - nExtra(ixy)
      ind1(ixy)=nxyzin(ixy) - 1 - indentUse
c       
c       get the padded size and the taper extents
c       Limit the long dimension padding to twice the default short dim
c       padding
c       
      nxybord(ixy)=max(5,nint(padFrac*nxybox(ixy)))
      nxybord(iyx)=min(max(5,nint(padFrac*nxybox(iyx))), 
     &    max(5, nint(0.45 * 2 * nxybox(ixy))))
c       nxybord(iyx)=max(5,nint(padFrac*nxybox(iyx)))
      nxpad=niceframe(nxybox(1)+2*nxybord(1),2,19)
      nypad=niceframe(nxybox(2)+2*nxybord(2),2,19)
      nxtap=max(5,nint(taperFrac*nxybox(1)))
      nytap=max(5,nint(taperFrac*nxybox(2)))

      if(nxybox(1)*nxybox(2).gt.maxbsiz.or.nxpad*nypad.gt.idimc)call
     &    errorexit('Overlap too big for correlation arrays, reduce '//
     &    'padding or aspect ratio')
c       
c       get the first image, lower piece
c       
      call irepak(brray, crray,nxin,nyin,ind0(1),ind1(1),ind0(2),
     &    ind1(2))
      call taperinpad(brray,nxybox(1),nxybox(2),xcray,nxpad+2,nxpad,
     &    nypad,nxtap,nytap)
      call meanzero(xcray,nxpad+2,nxpad,nypad)
      call dumpedge(xcray,nxpad+2,nxpad,nypad,ixy,0)
      call todfft(xcray,nxpad,nypad,0)
c       
c       get the second image, upper piece
c       
      ind0(ixy)=indentUse
      ind1(ixy)=noverlap(ixy) - 1 - indentUse + nExtra(ixy)
c       
      call irepak(brray, drray,nxin,nyin,ind0(1),ind1(1),ind0(2),
     &    ind1(2))
      call taperinpad(brray,nxybox(1),nxybox(2),xdray,nxpad+2,nxpad,
     &    nypad,nxtap,nytap)
      call meanzero(xdray,nxpad+2,nxpad,nypad)
      call dumpedge(xdray,nxpad+2,nxpad,nypad,ixy,0)
      call todfft(xdray,nxpad,nypad,0)
c       
c       multiply xcray by complex conjugate of xdray, put back in xcray
c       
      do jx=1,nypad*(nxpad+2)/2
        xcray(jx)=xcray(jx)*conjg(xdray(jx))
      enddo

      call setctfwsr(sigma1,sigma2,radius1,radius2,ctf,nxpad,nypad,delta)
c       
      if(delta.ne.0.)call filterpart(xcray,xcray,nxpad,nypad,ctf,delta)
      call todfft(xcray,nxpad,nypad,1)
      call peakfind(xcray,nxpad+2,nypad,xpeak,ypeak,peak)
      call dumpedge(xcray,nxpad+2,nxpad,nypad,ixy,1)
c       
c       return the amount to shift upper to align it to lower (verified)
c       
      xdisp=xpeak - nExtra(1)
      ydisp=ypeak - nExtra(2)
c       write(*,'(2f8.2,2f8.2)')xpeak,ypeak,xdisp,ydisp
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
c       write(*,'(2f8.2,2f8.2)')xpeak,ypeak,xdisp,ydisp
      return
      end



c       PEAKFIND finds the coordinates of the the absolute peak, XPEAK, YPEAK
c       in the array ARRAY, which is dimensioned to nx+2 by ny.
c       
      subroutine peakfind(array,nxplus,nyrot,xpeak,ypeak,peak)
      real*4 array(nxplus,nyrot),amat(9)
      nxrot=nxplus-2
c       
c       find peak
c       
      peak=-1.e30
      do iy=1,nyrot
        do ix=1,nxrot
          if(array(ix,iy).gt.peak)then
            peak=array(ix,iy)
            ixpeak=ix
            iypeak=iy
          endif
        enddo
      enddo
c       print *,ixpeak,iypeak
c       
c       simply fit a parabola to the two adjacent points in X or Y
c       
      cx=0.
      y1=array(indmap(ixpeak-1,nxrot),iypeak)
      y2=peak
      y3=array(indmap(ixpeak+1,nxrot),iypeak)
      denom=2.*(y1+y3-2.*y2)
      if(abs(denom).gt.-1.e6)cx=(y1-y3)/denom
      if(abs(cx).gt.0.5)cx=sign(0.5,cx)
c       print *,'X',y1,y2,y3,cx
      cy=0.
      y1=array(ixpeak,indmap(iypeak-1,nyrot))
      y3=array(ixpeak,indmap(iypeak+1,nyrot))
      denom=2.*(y1+y3-2.*y2)
      if(abs(denom).gt.-1.e6)cy=(y1-y3)/denom
      if(abs(cy).gt.0.5)cy=sign(0.5,cy)
c       print *,'Y',y1,y2,y3,cy
c       
c       return adjusted pixel coordinate minus 1
c       
      xpeak=ixpeak+cx-1.
      ypeak=iypeak+cy-1.
      if(xpeak.gt.nxrot/2)xpeak=xpeak-nxrot
      if(ypeak.gt.nyrot/2)ypeak=ypeak-nyrot
c       print *,xpeak,ypeak
      return
      end

c$$$    
c$$$    
c$$$    
c$$$    c         PEAKFIND finds the coordinates of the the absolute peak, XPEAK, YPEAK
c$$$    c         in the array ARRAY, which is dimensioned to nx+2 by ny.
c$$$    c
c$$$    subroutine peakfind(array,nxplus,nyrot,xpeak,ypeak,peak)
c$$$    real*4 array(nxplus,nyrot)
c$$$    nxrot=nxplus-2
c$$$    c         
c$$$    c         find peak
c$$$    c
c$$$    peak=-1.e30
c$$$    do iy=1,nyrot
c$$$    do ix=1,nxrot
c$$$    if(array(ix,iy).gt.peak)then
c$$$    peak=array(ix,iy)
c$$$    ixpeak=ix
c$$$    iypeak=iy
c$$$    endif
c$$$    enddo
c$$$    enddo
c$$$    c       print *,ixpeak,iypeak
c$$$    c         
c$$$    c         return adjusted pixel coordinate minus 1
c$$$    c
c$$$    xpeak=ixpeak-1.
c$$$    ypeak=iypeak-1.
c$$$    if(xpeak.gt.nxrot/2)xpeak=xpeak-nxrot
c$$$    if(ypeak.gt.nyrot/2)ypeak=ypeak-nyrot
c$$$    c       print *,xpeak,ypeak
c$$$    return
c$$$    end

c       DNM 8/18/02: add array argument, remove hinv as argument and
c       access it through common

      subroutine find_best_shifts(a,maxvar,dxgridmean,dygridmean,idir,izsect,h,
     &    nsum,bavg,bmax,aavg,amax)

      implicit none
      integer*4 idir,izsect,nsum,maxvar
      real*4 bavg,bmax,aavg,amax
      include 'blend.inc'
c       
      real*4 h(2,3,*)
      real*4 dxgridmean(limedge,2),dygridmean(limedge,2)
      integer*4 indvar(limnpc)
c       
c       parameter (limvar=400)
      real*8 a(maxvar,maxvar),b(limvar,2)
      integer*4 ivarpc(limvar)
c       
      integer*4 nvar,ipc,ivar,m,ixy,iedge,neighpc,neighvar,ipclo,i
      integer*4 noverwrote
      real*4 asum,bsum,xsum,ysum,bdist,adist
c       
c       The data coming in are the displacements of upper piece from
c       being in alignment with the lower piece if idir = 1, or the
c       shift needed to align upper piece with lower if idir = -1
c       
c       build list of variables
c       
      nvar=0
      do ipc=1,npclist
        if(izpclist(ipc).eq.izsect)then
          call xfunit(h(1,1,ipc),1.)
          call xfunit(hinv(1,1,ipc),1.)
          indvar(ipc)=0
          if(iedgelower(ipc,1).gt.0.or.
     &        iedgelower(ipc,2).gt.0.or.
     &        iedgeupper(ipc,1).gt.0.or.
     &        iedgeupper(ipc,2).gt.0)then
            nvar=nvar+1
            if (nvar.gt.limvar .or. nvar*maxvar.gt.maxsiz / 2)call errorexit(
     &          'TOO MANY PIECES FOR ARRAYS IN FIND_BEST_SHIFTS')
            ivarpc(nvar)=ipc
            indvar(ipc)=nvar
          endif
        endif
      enddo
      nsum=0
      bsum=0.
      bmax=0.
      asum=0.
      amax=0.
      bavg=0.
      aavg=0.
      if(nvar.eq.1)return
c       
c       build matrix of simultaneous equations for minimization solution
c       
      do ivar=1,nvar-1
        ipc=ivarpc(ivar)
        do m=1,nvar-1
          a(ivar,m)=0.
          b(ivar,1)=0.
          b(ivar,2)=0.
        enddo
c         
        do ixy=1,2
          iedge=iedgelower(ipc,ixy)
          if(iedge.gt.0)then
            a(ivar,ivar)=a(ivar,ivar)+1
            neighpc=ipiecelower(iedge,ixy)
            neighvar=indvar(neighpc)
c             
c             for a regular neighbor, enter a -1 in its term; but for the
c             last variable being eliminated, enter a +1 for ALL other
c             variables instead
c             
            if(neighvar.ne.nvar)then
              a(ivar,neighvar)=a(ivar,neighvar)-1
            else
              do m=1,nvar-1
                a(ivar,m)=a(ivar,m)+1
              enddo
            endif
c             
c             when this piece is an upper piece, subtract displacements from
c             constant term
c             
            b(ivar,1)=b(ivar,1)-idir*dxgridmean(iedge,ixy)
            b(ivar,2)=b(ivar,2)-idir*dygridmean(iedge,ixy)
          endif
c           
          iedge=iedgeupper(ipc,ixy)
          if(iedge.gt.0)then
            a(ivar,ivar)=a(ivar,ivar)+1
            neighpc=ipieceupper(iedge,ixy)
            neighvar=indvar(neighpc)
            if(neighvar.ne.nvar)then
              a(ivar,neighvar)=a(ivar,neighvar)-1
            else
              do m=1,nvar-1
                a(ivar,m)=a(ivar,m)+1
              enddo
            endif
c             
c             when a lower piece, add displacements to constant terms
c             
            b(ivar,1)=b(ivar,1)+idir*dxgridmean(iedge,ixy)
            b(ivar,2)=b(ivar,2)+idir*dygridmean(iedge,ixy)
          endif
        enddo
      enddo
c       
c       solve the equations, take the b values as dx and dy; compute the
c       sum to get the shift for the final piece
c       
c       write(*,'(9i5)')(ivarpc(i),i=1,nvar)
c       write(*,'(8f7.1)')((a(i,j),i=1,nvar-1),j=1,nvar-1)
c       write(*,'(8f9.2)')((b(i,j),i=1,nvar-1),j=1,2)
      call gaussjd(a,nvar-1,maxvar,b,2,limvar,2)
c       write(*,'(8f9.2)')((b(i,j),i=1,nvar-1),j=1,2)
      xsum=0.
      ysum=0.
      do i=1,nvar-1
        h(1,3,ivarpc(i))=b(i,1)
        h(2,3,ivarpc(i))=b(i,2)
        xsum=xsum+b(i,1)
        ysum=ysum+b(i,2)
      enddo
      h(1,3,ivarpc(nvar))=-xsum
      h(2,3,ivarpc(nvar))=-ysum
      do i=1,nvar
        call xfinvert(h(1,1,ivarpc(i)),hinv(1,1,ivarpc(i)))
      enddo
c       
c       compute and return the results 
c       
      do ivar=1,nvar
        ipc=ivarpc(ivar)
        do ixy=1,2
          iedge=iedgelower(ipc,ixy)
          if(iedge.gt.0)then
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
          endif
        enddo
      enddo
      bavg=bsum/nsum
      aavg=asum/nsum
c       write(*,'(i3,a,2f6.2,a,2f6.2)')nsum, ' edges, mean, max '//
c       &           'displacement before:', bsum/nsum,bmax,', after:',asum/nsum,
c       &           amax
c       
c       DNM 8/18/02: invalidate pieces in memory for part of array that
c       was used
c       
      noverwrote = (2 * maxvar * nvar + npixin - 1) / npixin
      do i = 1,min(noverwrote,memlim)
        if (izmemlist(i) .gt. 0) memIndex(izmemlist(i)) = -1
        izmemlist(i) = -1
        lastused(i) = 0
      enddo
c       
      return
      end


      subroutine findBestGradient(dxgridmean,dygridmean,idir,izsect,
     &    gradnew,rotnew)
      implicit none
      include 'blend.inc'
      integer*4 idir,izsect
      real*4 gradnew,rotnew
      real*4 dxgridmean(limedge,2),dygridmean(limedge,2),bavg,bmax
c       
c       Stuff for amoeba: ftol2 and ptol2 are used the FIRST time
c       
      integer nvar
      parameter (nvar = 2)
      real*4 pp(nvar+1,nvar+1),yy(nvar+1),ptmp(nvar),ptol(nvar), da(nvar)
      real*4 ptol1, ftol1,ptol2,ftol2,delfac,var(nvar)
      data da/0.5,0.2/
      integer*4 jmin, iter, i, j
      external gradfunc
c       
      integer*4 nedg, ifTrace, nTrial,izedge
      real*4 dxedge(limedge,2),dyedge(limedge,2),errMin
      real*4 gradXcenLo(limvar),gradXcenHi(limvar)
      real*4 gradYcenLo(limvar),gradYcenHi(limvar)
      real*4 overXcenLo(limvar),overXcenHi(limvar)
      real*4 overYcenLo(limvar),overYcenHi(limvar)
      common /funccom/nedg, ifTrace, nTrial, izedge, errMin, dxedge, dyedge,
     &    gradXcenLo, gradXcenHi, gradYcenLo, gradYcenHi, overXcenLo,
     &    overXcenHi, overYcenLo, overYcenHi
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
      implicit none
      real*4 p(*), funcErr
      include 'blend.inc'

      integer*4 nedg, ifTrace, nTrial,izedge
      real*4 dxedge(limedge,2),dyedge(limedge,2),errMin
      real*4 gradXcenLo(limvar),gradXcenHi(limvar)
      real*4 gradYcenLo(limvar),gradYcenHi(limvar)
      real*4 overXcenLo(limvar),overXcenHi(limvar)
      real*4 overYcenLo(limvar),overYcenHi(limvar)
      common /funccom/nedg, ifTrace, nTrial, izedge, errMin, dxedge, dyedge,
     &    gradXcenLo, gradXcenHi, gradYcenLo, gradYcenHi, overXcenLo,
     &    overXcenHi, overYcenLo, overYcenHi
c       
      real*4 h(2,3,limnpc),tiltang,errSum,dxlo,dxhi,dylo,dyhi
      real*4 dxadj(limedge,2),dyadj(limedge,2),bmean, bmax, aftmean, aftmax
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

      call find_best_shifts(array8, nxpieces * nypieces, dxadj, dyadj,
     &    1, izedge, h, iedge, bmean, bmax, aftmean, aftmax)

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
        enddo
        call iwrsecl(iunit, array, nyout)
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
        call iwrlin(iunit, brray)
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
      implicit none
      include 'blend.inc'
      integer*4 ipclow, ipcup, ixy, ind, ix, memlow, memup, iy
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

c       dumpEdge writes a padded image or correlation to a file.  The
c       image in CRRAY, NXDIM is the X array dimension and NXPAD and NYPAD
c       are image sizes.  IXY is 1 for X or 2 for Y edge.
c       
      subroutine dumpedge(crray,nxdim,nxpad,nypad,ixy,ifcorr)
      implicit none
      include 'blend.inc'
      integer*4 nxdim,nxpad,nypad,ixy,ifcorr
      real*4 crray(nxdim,nypad)
      real*4 title(20), scale, dmin, dmax, dmt
      integer*4 kxyz(3), ix, iy
c       
      if (ifDumpXY(ixy) .lt. 0) return
      if (ifDumpXY(ixy) .eq. 0) then
        nzOutXY(ixy) = 0
        kxyz(1) = nxpad
        kxyz(2) = nypad
        kxyz(3) = 0
        call icrhdr(2+ixy, kxyz, kxyz, 2, title, 0)
        ifDumpXY(ixy) = 1
      endif
      call imposn(2 + ixy, nzOutXY(ixy), 0)
      nzOutXY(ixy) = nzOutXY(ixy) + 1
      call ialsiz_sam_cel(2 + ixy, nxpad,nypad, nzOutXY(ixy))

      call iclden(crray,nxdim,nypad,1,nxpad,1,nypad,dmin,dmax,dmt)
      scale = 255. / (dmax - dmin)
      do iy = 1, nypad
        if (ifcorr .eq. 0) then
          do ix = 1, nxpad
            brray(ix) = scale *(crray(ix,iy) - dmin)
          enddo
        else
          do ix = 1, nxpad
            brray(ix) = scale *(crray(mod(ix+nxpad/2-1,nxpad)+1,
     &          mod(iy+nypad/2-1,nypad)+1) - dmin)
          enddo
        endif
        call iwrlin(2+ixy, brray)
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
