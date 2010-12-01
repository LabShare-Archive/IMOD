c       edgesubs.f has subroutines needed by blendmont and by finddistort:
c       
c       FINDEDGEFUNC
c       SETGRIDCHARS
c       LOCALMEAN
c
c       $Id$
c       Log at end of file

c       FINDEDGEFUNC finds the edge function relating a piece in ARRAY to a
c       piece in BRRAY (dimensions NX by NY).  I[XY]GRIDSTR is the coordinate
c       of the lower-left corner of the function grid in the lower piece (in
c       ARRAY).  I[XY]OFSET is the ofset to the corresponding point in the
c       upper piece.  NXGRID, NYGRID specify the # of grid points in X and Y;
c       INTXGRID, INTYGRID are the pixel intervals between grid points;
c       IXBOXSIZ and IYBOXSIZ are the dimensions of the box for image
c       matching.  At each grid position, the displacements are returned in
c       DXGRID, DYGRID, the SD value of the image match in SDGRID, and the
c       mean density difference in DDENGRID (all dimension IXGDIM x IYGDIM).
c       
      subroutine findedgefunc(array,brray,nx,ny,ixgridstr,iygridstr,
     &    ixofset,iyofset,nxgrid,nygrid,intxgrid,intygrid,ixboxsiz,
     &    iyboxsiz,intscan,dxgrid,dygrid,sdgrid,ddengrid,ixgdim,
     &    iygdim)
c       
      implicit none
      real*4 array(*),brray(*)
      real*4 dxgrid(ixgdim,iygdim),dygrid(ixgdim,iygdim)
      real*4 sdgrid(ixgdim,iygdim),ddengrid(ixgdim,iygdim)
      integer*4 nx,ny,ixgridstr,iygridstr,ixofset,iyofset,nxgrid,nygrid
      integer*4 intxgrid,intygrid,ixboxsiz,intscan,iyboxsiz,ixgdim,iygdim
      integer*4 ix,iy,idxbase,idybase,ixgrdcen,iygrdcen,ixdir,ixloopst
      integer*4 ixloopnd, ixloop, iouter, iydir, iyloopst, iyloopnd,iyloop
      integer*4 ixgrid,iygrid,ixbox0,ixbox1,iybox0,iybox1,nsum,idx0,idx1
      integer*4 idy0,idy1,idxmin,idymin,nsteps,limstep,inner
      real*4 dxmean,dymean,ddenmean,dxgr,dygr,sdmin,ddenmin
c       
c       intercept case of no overlap and make a null grid
c       
      if(nxgrid.le.0.or.nygrid.eq.0)then
        nxgrid=max(1,nxgrid)
        nygrid=max(1,nygrid)
        do iy=1,nygrid
          do ix=1,nxgrid
            dxgrid(ix,iy)=0.
            dygrid(ix,iy)=0.
            sdgrid(ix,iy)=-1.
            ddengrid(ix,iy)=0.
          enddo
        enddo
        return
      endif
c       
c       set up grid arrays
c       
      do iy=1,nygrid
        do ix=1,nxgrid
          sdgrid(ix,iy)=-1
        enddo
      enddo
      idxbase=ixofset-ixgridstr
      idybase=iyofset-iygridstr
c       
c       start from the inside out: so need the center location of grid
c       
      ixgrdcen=nxgrid/2
      iygrdcen=nygrid/2
c       
c       set up loops from center out
c       i.e. outer loop for long dimension from center to end then from
c       center to start
      ixdir=1
      if (nxgrid .gt. nygrid) then
        ixloopst=ixgrdcen+1
        ixloopnd=nxgrid
      else
        ixloopst=iygrdcen+1
        ixloopnd=nygrid
      endif
      do ixloop=1,2
        do iouter=ixloopst,ixloopnd,ixdir
c           
c           then inner loop for short dimension from center to end then from
c           center to start
          iydir=1
          if (nxgrid .gt. nygrid) then
            iyloopst=iygrdcen+1
            iyloopnd=nygrid
          else
            iyloopst=ixgrdcen+1
            iyloopnd=nxgrid
          endif
          do iyloop=1,2
            do inner=iyloopst,iyloopnd,iydir
              if (nxgrid .gt. nygrid) then
                ixgrid = iouter
                iygrid = inner
              else
                ixgrid = inner
                iygrid = iouter
              endif
c               
c               to get starting dx, dy, search for and average from neighbors
c               

              ixbox0=max(1, ixgridstr+(ixgrid-1)*intxgrid-ixboxsiz/2)
              ixbox1=min(nx-1, ixbox0+ixboxsiz-1)
              iybox0=max(1, iygridstr+(iygrid-1)*intygrid-iyboxsiz/2)
              iybox1=min(ny-1, iybox0+iyboxsiz-1)
              call localmean(dxgrid,dygrid,sdgrid,ddengrid,ixgdim,
     &            iygdim,nxgrid, nygrid, ixgrid,iygrid, dxmean ,
     &            dymean,ddenmean,nsum)
c               
c               if only one neighbor (or none), do complete scan
c               
              if(nsum.le.1)then
                idx0=-intscan
                idx1=intscan
                idy0=-intscan
                idy1=intscan
                call sdintscan(array,brray,nx,ny,ixbox0,iybox0,ixbox1,
     &              iybox1, idx0+idxbase,idy0+idybase,idx1+idxbase,
     &              idy1+idybase, sdmin,ddenmin,idxmin,idymin)
                dxgr=idxmin
                dygr=idymin
              else
c                 
c                 otherwise take starting point as average
c                 
                nsteps=4
                dxgr=dxmean+idxbase
                dygr=dymean+idybase
              endif
c               
c               now do the real search
c               
              nsteps=4
              limstep=6
              call bigsearch(array,brray,nx,ny,ixbox0,iybox0,ixbox1
     &            ,iybox1,dxgr,dygr, sdgrid(ixgrid,iygrid),ddengrid(
     &            ixgrid,iygrid),nsteps,limstep)
              dxgrid(ixgrid,iygrid)=dxgr-idxbase
              dygrid(ixgrid,iygrid)=dygr-idybase
c              write(*,102)ixbox0,iybox0,dxgrid(ixgrid,iygrid)
c     &            ,dygrid(ixgrid,iygrid),sdgrid(ixgrid,iygrid)
c     &            ,'  is the minimum'
c102           format(2i5,2f7.2,f10.4,a)
            enddo
            iydir=-1
            iyloopnd=1
            if (nxgrid .gt. nygrid) then
              iyloopst=iygrdcen
            else
              iyloopst=ixgrdcen
            endif
          enddo
        enddo
        ixdir=-1
        ixloopnd=1
        if (nxgrid .gt. nygrid) then
          ixloopst=ixgrdcen
        else
          ixloopst=iygrdcen
        endif
      enddo
      return
      end


c       SETGRIDCHARS determines the location and size of the grid for an edge
c       function.  NXY and NOVERLAP has the frame dimensions and overlap
c       between frames.  IBOXSIZ, INDENT, and INTGRID specify the box size
c       for matching images, minimum indentation from edge of image, and the
c       grid interval; element (1) is for the short direction of the edge and
c       (2) is for the long direction.  IXY is 1 for an X, 2 for Y edge.
c       IXDISPL, IYDISPL specify the displacement from perfect alignment
c       already expected at the edge.  It returns the size of the grid in
c       NXGRID and NYGRID, the pixel coordinate of the lower left corner of
c       the grid in the lower frame in IGRIDSTR, and the offset to the
c       corresponding place in the upper frame in IOFSET.
c       
      subroutine setgridchars(nxy,noverlap,iboxsiz,indent,intgrid,
     &    ixy,ixdispl,iydispl,limitLo,limitHi,nxgrid,nygrid,igridstr,iofset)
c       
      implicit none
      integer*4 nxy(*),noverlap(*),igridstr(*),iofset(*) !indexed by x-y
      integer*4 iboxsiz(*),indent(*),intgrid(*) !indexed by short-long
      integer*4 ngrid(2),idispl(2),len(2),jndent(2),limlen(2)
      integer*4 ixy,ixdispl,iydispl,limitLo,limitHi,nxgrid,nygrid
      integer*4 iyx,isl,indentUse,nextent,lapcen,ihafgrid,i
c       
c       12/98 change -i[xy]displ to +i[xy]displ
      if(ixy.eq.1)then
        idispl(1)=nxy(1)-noverlap(1)+ixdispl
        idispl(2)=iydispl
      else
        idispl(1)=ixdispl
        idispl(2)=nxy(2)-noverlap(2)+iydispl
      endif
      iyx=3-ixy
c       
c       get length of overlap in short direction: limit to standard length
c       
      len(ixy)=min(noverlap(ixy),nxy(ixy)-idispl(ixy))
      limlen(ixy) = len(ixy)
c       length of overlap in long direction: reduce by displacement
      len(iyx)=nxy(iyx)-abs(idispl(iyx))
      limlen(iyx) = len(iyx)
c       
c       Get limited length for getting extent if limitHi is nonzero
      if (limitHi .ne. 0) limlen(iyx) = max(iboxsiz(iyx),
     &    (limitHi + 1 - limitLo) - abs(idispl(iyx)))
c       print *,limitLo, limitHi, limlen(iyx)
c       
c       calculate extent usable within box, # gridpoints, indent to 1st point
c       but keep extent from getting negative
c       
      isl=ixy                                   !index to short-long variables
      do i=1,2
        indentUse = min(indent(isl), (limlen(i)-iboxsiz(isl)) / 2)
        nextent=limlen(i)-iboxsiz(isl)- 2 * indentUse
        ngrid(i)=1+nextent/intgrid(isl)
        jndent(i)=indentUse + (iboxsiz(isl)+mod(nextent,intgrid(isl)))/2
        isl=iyx
      enddo
      if (limitHi .ne. 0 .and. limitHi .ge. limitLo)
     &    jndent(iyx) = jndent(iyx) + limitLo
c       
c       calculate coordinates of grid within each frame
c       
      do i=1,2
        lapcen=(nxy(i)-idispl(i))/2             !offset to center of overlap
        ihafgrid=jndent(i)-len(i)/2             !from center to start of grid
        iofset(i)=lapcen+ihafgrid               !offset in frame 2
        igridstr(i)=(nxy(i)-lapcen)+ihafgrid    !offset in frame 1
      enddo
c       
      nxgrid=ngrid(1)
      nygrid=ngrid(2)
      return
      end


c       LOCALMEAN computes the local mean value of DXGRID and DYGRID around
c       the location IX,IY from positions that are possible (within 1 to
c       NXGRID, 1 to NYGRID) and have a non-negative SDGRID.
c       
      subroutine localmean(dxgrid,dygrid,sdgrid,ddengrid,ixgdim,
     &    iygdim,nxgrid,nygrid,ix,iy ,dxmean,dymean,ddenmean,nsum)
      implicit none
      real*4 dxgrid(ixgdim,iygdim),dygrid(ixgdim,iygdim)
      real*4 sdgrid(ixgdim,iygdim),ddengrid(ixgdim,iygdim)
      integer*4 ixgdim,iygdim,nxgrid,nygrid,ix,iy,nsum
      real*4 dxmean,dymean,ddenmean
      real*4 dxsum,dysum,densum
      integer*4 i, j
      nsum=0
      dxsum=0.
      dysum=0.
      densum=0.
      do i=ix-1,ix+1
        do j=iy-1,iy+1
          if((i.ne.ix.or.j.ne.iy).and.i.ge.1.and.i.le.nxgrid.and.
     &        j.ge.1.and.j.le.nygrid)then
            if(sdgrid(i,j).ge.0.)then
              dxsum=dxsum+dxgrid(i,j)
              dysum=dysum+dygrid(i,j)
              densum=densum+ddengrid(i,j)
              nsum=nsum+1
            endif         
          endif           
        enddo
      enddo
      if(nsum.eq.0)return
      dxmean=dxsum/nsum
      dymean=dysum/nsum
      ddenmean=densum/nsum
      return
      end

c       
c       $Log$
c       Revision 3.5  2010/09/23 04:59:10  mast
c       Made it go along short dimension in inner loop
c
c       Revision 3.4  2007/04/10 15:50:33  mast
c       Modified setgridchars to take data limits in the long dimension
c       
c       Revision 3.3  2005/08/22 16:17:56  mast
c       Bad message in last checkin
c       
c       Revision 3.2  2005/08/22 16:15:59  mast
c       Prevented negative extents when distortion limits size of field
c       
c       Revision 3.1  2003/12/12 20:46:48  mast
c       Split off from bsubs.f for finddistort to use
c       
