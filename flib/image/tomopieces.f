* * * * * * TOMOPIECES * * * * * *
c	  
c       TOMOPIECES figures out how to chop up a tomogram into pieces so that
c       the Fourier transforms of each piece can be done in memory.
c       
c       See manual page for more details.
c	  
c       
c       $Id$
c       Log at end of file
c
      implicit none
      integer limpart
      parameter (limpart=2000)
      character*80 filout
      integer*4 nxyz(3)
      real*4 megaVox/10/                        !MAXIMUM MEGAVOXELS
      integer*4 izbackst(limpart),izbacknd(limpart),izoutst(limpart),
     &    izoutnd(limpart)
      integer*4 iybackst(limpart),iybacknd(limpart),iyoutst(limpart),
     &    iyoutnd(limpart)
      integer*4 ixbackst(limpart),ixbacknd(limpart),ixoutst(limpart),
     &    ixoutnd(limpart)
      logical*4 toobig, noFFT
      integer*4 minOverlap, nPadX, nPadY, nPadZ, maxPieceX, maxPieceY
      integer*4 maxPieceZ, maxLayerPieces, ierr, nx, ny, nz, nxp, nyp, nzp
      integer*4 nxextr, nyextr, nzextr, nxout, nyout, nzout, ix, iy, iz, i
      real*4 perim, perimin, piecePerim, piecePerimin
      integer*4 nxpmin, nypmin, nzpmin, nout, limY
      integer*4 padNiceIfFFT

      logical pipinput
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetFloat,PipGetInteger,PipGetLogical
      integer*4 PipGetInOutFile
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  tomopieces
c       
      integer numOptions
      parameter (numOptions = 11)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'tomogram:TomogramOrSizeXYZ:CH:@megavox:MegaVoxels:F:@'//
     &    'xpad:XPadding:I:@ypad:YPadding:I:@zpad:ZPadding:I:@'//
     &    'xmaxpiece:XMaximumPieces:I:@ymaxpiece:YMaximumPieces:I:@'//
     &    'zmaxpiece:ZMaximumPieces:I:@'//
     &    'minoverlap:MinimumOverlap:I:@param:ParameterFile:PF:@'//
     &    'help:usage:B:'
c       
      minoverlap=4                              !MINIMUM OVERLAP TO OUTPUT
      npadx=8                                   !PADDING/TAPER EXTENT IN X
      npadz=8                                   !PADDING/TAPER EXTENT IN Z
      npady=4                                   !PADDING IN Y
      maxpiecex=0                               !MAX PIECES IN X
      maxpiecey=0                               !MAX PIECES IN X
      maxpiecez=-1                              !MAX PIECES IN X
      maxLayerPieces = 19
      noFFT = .false.
c       
c       Pip startup: set error, parse options, do help output, get the
c       one obligatory argument to get size
c       But turn off the entry printing first!
      call PipEnableEntryOutput(0)
      call PipReadOrParseOptions(options, numOptions, 'tomopieces',
     &    'ERROR: TOMOPIECES - ', .false., 1, 1, 0, numOptArg,
     &    numNonOptArg)
      call get_nxyz(.true., 'TomogramOrSizeXYZ', 'TOMOPIECES', 1, nxyz)

      ierr = PipGetFloat('MegaVoxels', megaVox)
      ierr = PipGetInteger('MinimumOverlap', minOverlap)
      ierr = PipGetInteger('XPadding', npadx)
      ierr = PipGetInteger('YPadding', npady)
      ierr = PipGetInteger('ZPadding', npadz)
      ierr = PipGetInteger('XMaximumPieces', maxPieceX)
      ierr = PipGetInteger('YMaximumPieces', maxPieceY)
      ierr = PipGetInteger('ZMaximumPieces', maxPieceZ)
      ierr = PipGetLogical('NoFFTSizes', noFFT)
      call PipDone()
c       
      nx=nxyz(1)
      ny=nxyz(2)
      nz=nxyz(3)
c       
c       Set defaults very big if no maxima entered - but if X and Y maxes
c       are still 0, set up for 19 pieces on a layer
c       Also, if one is zero and one is 1, set up for the 19-piece limit
c       
      if (maxPieceX .eq. 0 .and. (maxPieceY .eq. 0 .or. maxPieceY.eq.1)) then
        maxPieceX = maxLayerPieces
      else if (maxPieceX .eq. 1 .and. maxPieceY .eq. 0) then
        maxPieceY = maxLayerPieces
      else
        if (maxPieceX .le. 0) maxPieceX = nx / 2 - 1
        if (maxPieceY .le. 0) maxPieceY = ny / 2 - 1
      endif
      if (maxPieceZ .lt. 0) maxPieceZ = nz / 2 - 1
c       
c       loop on the possible pieces in X; for each one, get the padded size
c       
      perimin = (10. * nx) * ny * nz
      piecePerimin = perimin
      nxpmin = 0
      do nxp=1,maxpiecex
        nxextr=(nx+(nxp-1)*minoverlap + nxp-1)/nxp
        nxout = padNiceIfFFT(nxextr, npadx, noFFT)
c         
c         loop on the possible pieces in Y
c         
        limY = maxPieceY
        if (limY .eq. 0) limy = maxLayerPieces / nxp

        do nyp = 1, limY
          nyextr=(ny+(nyp-1)*minoverlap + nyp-1)/nyp
          nyout = padNiceIfFFT(nyextr, npady, noFFT)
          nzp=1
          toobig=.true.
c           
c           then loop on pieces in Z, get padded size and compute padded
c           volume size - until it is no longer too big for maxmem
c           
          do while(nzp.le. maxPieceZ .and.toobig)
            nzextr=(nz+(nzp-1)*minoverlap + nzp-1)/nzp
            nzout = padNiceIfFFT(nzextr, npadz, noFFT)
            if(float(nxout*nyout)*nzout.gt.megaVox * 1.e6)then
              nzp=nzp+1
            else
              toobig=.false.
            endif
          enddo
c           
c           compute perimeter of pieces and keep track of minimum
c           If total perimeter is equal, favor one with minimum individual
c           perimeter (at the expense of more pieces)
c           Of the equivalent sets when nx = nz, this favors the ones with
c           fewer X pieces
c           
          perim=float(nx*ny)*nzp + float(nx*nz)*nyp + float(ny*nz)*nxp
          piecePerim = nxout * nyout + nxout * nzout + nyout * nzout
c           print *,nxp, nyp, nzp, perim, piecePerim, nxout, nyout, nzout
          if (.not.toobig .and. (perim.lt.perimin .or.
     &        (perim .eq. perimin .and. piecePerim .lt. piecePerimin))) then
            perimin=perim
            piecePerimin = piecePerim
            nxpmin=nxp
            nypmin = nyp
            nzpmin=nzp
          endif
        enddo
      enddo

      if (nxpmin .eq. 0) call exitError(
     &    'PIECES ARE ALL TOO LARGE WITH GIVEN MAXIMUM NUMBERS')
      nxp=nxpmin
      nzp=nzpmin
      nyp = nypmin
c       
c       get the starting and ending limits to extract and coordinates
c       for getting back from the padded volume
      call getRanges(nx, nxp, minoverlap, npadx, ixoutst, ixoutnd,
     &    ixbackst, ixbacknd, noFFT)
      call getRanges(ny, nyp, minoverlap, npady, iyoutst, iyoutnd,
     &    iybackst, iybacknd, noFFT)
      call getRanges(nz, nzp, minoverlap, npadz, izoutst, izoutnd,
     &    izbackst, izbacknd, noFFT)
c       
      write(*,101)nxp,nyp,nzp
101   format(3i4)
      do iz=1,nzp
        do iy=1,nyp
          do ix=1,nxp
            call rangeout(ixoutst(ix),ixoutnd(ix),',',filout,nout)
            call rangeadd(iyoutst(iy),filout,nout)
            call rangeadd(iyoutnd(iy),filout,nout)
            call rangeadd(izoutst(iz),filout,nout)
            call rangeadd(izoutnd(iz),filout,nout)
            write(*,102)filout(1:nout)
102         format(a)
          enddo
        enddo
      enddo
      do i=1,nxp
        call rangeout(ixbackst(i),ixbacknd(i),',',filout,nout)
        write(*,102)filout(1:nout)
      enddo
      do i=1,nyp
        call rangeout(iybackst(i),iybacknd(i),',',filout,nout)
        write(*,102)filout(1:nout)
      enddo
      do i=1,nzp
        call rangeout(izbackst(i),izbacknd(i),',',filout,nout)
        write(*,102)filout(1:nout)
      enddo
c       
      call exit(0)
      end

c       get size to extract, and size of padded output, and offset for
c       getting back from the padded volume
c       
      subroutine getRanges(nx, nxp, minoverlap, npadx, ixoutst, ixoutnd,
     &    ixbackst, ixbacknd, noFFT)
      implicit none
      integer*4 nx, nxp, minoverlap, npadx
      integer*4 ixoutst(*), ixoutnd(*), ixbackst(*), ixbacknd(*)
      integer*4 nxextr, nxout, nxbackofs, laptot, lapbase, lapextra, ip, lap
      integer*4 lapbot, laptop, padNiceIfFFT
      logical*4 noFFT
c       
      nxextr=(nx+(nxp-1)*minoverlap + nxp-1)/nxp
      nxout=padNiceIfFFT(nxextr, npadx, noFFT)
      nxbackofs=(nxout-nxextr)/2
c       
c       divide the total overlap into nearly equal parts
c       
      laptot=nxextr*nxp-nx
      lapbase=laptot/max(1,nxp-1)
      lapextra=mod(laptot,max(1,nxp-1))
      ixoutst(1)=0
      ixbackst(1)=nxbackofs
c       
c       get coordinates to extract, and coordinates for reassembly
c       
      do ip=1,nxp
        ixoutnd(ip)=ixoutst(ip)+nxextr-1
        if(ip.lt.nxp)then
          lap=lapbase
          if(ip.le.lapextra)lap=lap+1
          laptop=lap/2
          lapbot=lap-laptop
          ixoutst(ip+1)=ixoutst(ip)+nxextr-lap
          ixbacknd(ip)=nxbackofs+nxextr-1-laptop
          ixbackst(ip+1)=nxbackofs+lapbot
        else
          ixbacknd(ip)=nxbackofs+nxextr-1
        endif
      enddo
      return
      end

      integer*4 function padNiceIfFFT(nxextr, npadx, noFFT)
      implicit none
      integer*4 nxextr, npadx
      logical*4 noFFT
      integer*4 niceframe
      if (noFFT) then
        padNiceIfFFT = nxextr + 2 * npadx
      else
        padNiceIfFFT=niceframe(2*((nxextr+1)/2+npadx),2,19)
      endif
      return
      end
 
      subroutine rangeout(izst,iznd,link,buf,nout)
      character*(*) buf
      character*1 link
      call int_iwrite(buf,izst,nout)
      buf(nout+1:nout+1)=link
      call int_iwrite(buf(nout+2:),iznd,nadd)
      nout=nout+nadd+1
      return
      end

      subroutine rangeadd(iznd,buf,nout)
      character*(*) buf
      buf(nout+1:nout+1)=','
      call int_iwrite(buf(nout+2:),iznd,nadd)
      nout=nout+nadd+1
      return
      end
c
c       $Log$
c       Revision 3.3  2010/01/08 19:06:52  mast
c       Added nofft option
c
c       Revision 3.2  2009/12/28 20:39:08  mast
c       Suppress entry output
c
c       Revision 3.1  2004/06/14 19:20:16  mast
c       Converted to PIP input, made it able to divide in Y dimension also,
c       and added options for controlling the maximum number of pieces
c	
