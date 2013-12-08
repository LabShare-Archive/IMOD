      Program   EXTSTACK

c       -------------------------------------------------------------------
c       --- This program will extract subsections from a symmetrical    ---
c       --- object and create a stack of normalized subsections as the  ---
c       --- output. It works best when used upon a symmetrical object   ---
c       --- such as an axoneme cross-section.                           ---
c       --- The input requirements are:                                 ---
c       ---     1) An image file containing 1 or more sections          ---
c       ---     2) A point file ( corresponding to (1) ) containing     ---
c       ---        a point at the center of every subsection to be      ---
c       ---        extracted.                                           ---
c       ---     3) A point file ( corresponding to (1) ) containing     ---
c       ---        all of the points used to determine the section      ---
c       ---        center.                                              ---
c       --- The program requires that the dimensions of the subsection  ---
c       --- box be give as the edge lengths of the box.  An additional  ---
c       --- subsection rotation may be specified.  The output will be   ---
c       --- an image file containing all of the normalized subsections. ---
c       --- Written: Feb 6, 1989 - sjm                                  ---
c       --- Updates:                                                    ---
c       ---     21-feb-1989 sjm : User now creates two point files:     ---
c       ---                       1) A list of points to extract, and   ---
c       ---                       2) A list of all points in a section  ---
c       ---                          used for center determination.     ---
c       ---                       This allows pre-processing by only    ---
c       ---                       selecting good objects for (1), yet   ---
c       ---                       retains center information in (2).    ---
c       -------------------------------------------------------------------
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       Revision 3.2  2005/02/11 01:42:32  mast
c       Warning cleanup: implicit declarations, main return type, parentheses, etc.
c       
c       Revision 3.1  2003/03/14 01:07:52  mast
c       Add linear argument for interpolation call
c       


      parameter ( maxarr=2100*2100 )            !--- Maximum array size ---
      parameter ( maxpts=10000 )                !--- Maximum number of pts ---

c       --- Working variables ---
      integer x(maxpts), y(maxpts), z(maxpts), plen, nlen,
     &    xref, yref, zref, ztmp
      real      xtmp(maxpts), ytmp(maxpts), amat(2,2),
     &    xcen(maxpts), ycen(maxpts), ccnt(maxpts)
      logical done
      character*80 imgfil, omgfil, reffil, extfil
      
c       --- Image file variables ---
      character dat*9, tim*8
c       
c       7/7/00 CER: remove the encode's; titlech is the temp space
c       
      character*80 titlech
      real      array(maxarr), brray(maxarr), trray(maxarr)
      real      dmax, dmin, title(20), cell(6)

      integer   nxyza(3), mxyza(3), nxyzsta(3)
      integer   nxyzb(3), mxyzb(3), nxyzstb(3)
      equivalence (nxyza,nxa)
      equivalence (nxyzb,nxb)
      common//nxa,nya,nza,nxb,nyb,nzb
      data nxyzsta / 0,0,0 /
      data nxyzstb / 0,0,0 /
      data cell    / 0.,0.,0.,90.,90.,90. /


c       ------------------------------------
c       --- Get the parameters and files ---

      write ( *, * ) ' Input image file name : '
      read  ( *, 1000 ) imgfil
      write ( *, * ) ' Output image file name : '
      read  ( *, 1000 ) omgfil
      write ( *, * ) ' Reference (complete) point file name : '
      read  ( *, 1000 ) reffil
      write ( *, '( a, $ )' ) ' Expected number of ref. pts / section :'
      read  ( *, * ) expref
      write ( *, * ) ' Extraction point file name : '
      read  ( *, 1000 ) extfil
      write ( *, * ) ' '
      write ( *, * ) ' Subsection box is centered on the distal end of'
      write ( *, * ) ' a line (A) from the image center to the subsection'
      write ( *, * ) ' point.'
      write ( *, * ) ' '
      write ( *, * ) ' Length of box edge parallel to line A (pixels) : '
      read  ( *, * ) plen
      write ( *, * ) ' Length of box edge normal to line A (pixels) : '
      read  ( *, * ) nlen
      write ( *, * ) ' '
      addangle = 0.0
      write ( *, * ) ' ( A < 0.0 rotates clockwise, A > 0.0 ccw )'
      write ( *, * ) ' Additional angle to rotate subsections [0.0] : '
      read  ( *, * ) addangle

      call imopen(1,imgfil,'ro')
      call imopen(2,omgfil,'new')
      open ( unit=10, file=extfil, status='old' )
      open ( unit=20, file=reffil, status='old' )

      call irdhdr(1,nxyza,mxyza,mode,dmin,dmax,dmean) !---Get Image Header---
      call itrhdr(2,1)
      nxb = nlen
      nyb = plen
      nzb = 1
                                                !--- Transfer header with array ---
      call ialsiz(2,nxyzb,nxyzstb)              !--- dimensions of subsection box ---


c       -------------------------------
c       --- Read in the point files ---

c       --- Read reference point file ---
      done = .false.
      maxsec = 0
      nrefpts = 0
      do i=1,maxpts
        ccnt(i)=0.
        xcen(i)=0.
        ycen(i)=0.
      enddo
      do while ( .not. done )
        read ( 20, *, end=100 ) xref, yref, zref
        ztmp = zref + 1
        xcen(ztmp) = xcen(ztmp) + float(xref)
        ycen(ztmp) = ycen(ztmp) + float(yref)
        ccnt(ztmp) = ccnt(ztmp) + 1.0
        maxsec = max(maxsec,ztmp)
        nrefpts = nrefpts + 1
        if ( nrefpts .gt. 10000 ) done = .true.
      end do
100   do ztmp = 1 , maxsec
        if ( ccnt(ztmp) .gt. 0.0 ) then
          if ( ccnt(ztmp) .ne. expref ) then
            write ( *, '( 3( a, i4 ) )' )
     &          ' ERROR: Section ', (ztmp-1), ' -- expected ',
     &          int(expref), ' reference points, found ',
     &          int(ccnt(ztmp))
            call exit(0)
          end if
          xcen(ztmp) = xcen(ztmp) / ccnt(ztmp)
          ycen(ztmp) = ycen(ztmp) / ccnt(ztmp)
        end if
      end do

c       --- Read extraction point file ---
      done = .false.
      npts = 0
      maxsec = 0
      do while ( .not. done )
        read ( 10, *, end=200 ) x(npts+1), y(npts+1), z(npts+1)
        maxsec = max(maxsec,z(npts+1))
        npts = npts + 1
        if ( npts .gt. 10000 ) done = .true.
      end do


c       -------------------------------
c       --- Extract the subsections ---
      
200   nsec = 0
      dmintot = 1.0e5
      dmaxtot = -1.0e5
      dmeantot = 0.0

      do iz = 0 , maxsec                        !--- Loop over all the sections ---
        call irdsec(1,array,*300)               !--- Read a section ---
        nlpts = 0

c         --- set up temporary arrays packed with the point data ---
c         --- for the current section                             ---
        do np = 1 , npts
          if ( z(np) .eq. iz ) then
            nlpts = nlpts + 1
            xtmp(nlpts) = float(x(np))
            ytmp(nlpts) = float(y(np))
          end if
        end do

c         --- if the section has point data, then extract subsections ---
        if ( nlpts .gt. 0 ) then

c           --- Loop over the subsection points for each extraction ---
          do np = 1 , nlpts
            scale = 1.0
            xc = nxa / 2.0
            yc = nya / 2.0
            opp = xtmp(np) - xcen(iz+1)
            adj = ytmp(np) - ycen(iz+1)
            if ( (opp.ne.0.0) .and. (adj.ne.0.0) ) then
              theta = atan2d(opp,adj) - 180.0 + addangle !---Rotation---
            else
              theta = addangle
            end if
            amat(1,1) = cosd(theta)             !---Transform---
            amat(1,2) =-sind(theta)
            amat(2,1) = sind(theta)
            amat(2,2) = cosd(theta)
            xt = -( amat(1,1)*(xtmp(np)-xc) +   !---Translate---
     &          amat(1,2)*(ytmp(np)-yc) )
            yt = -( amat(2,1)*(xtmp(np)-xc) +
     &          amat(2,2)*(ytmp(np)-yc) )
            nxb = nlen
            nyb = plen

c             --- Extract the subsection using calculated parameters ---
            call cubinterp( array, brray, nxa, nya, nxb, nyb, amat,
     &          xc, yc, xt, yt, scale, dmean, 0)

            call iclden( brray, nxb, nyb, 1, nxb, 1, nyb,
     &          dmint, dmaxt, dmeant )
            dmintot = min(dmintot,dmint)
            dmaxtot = max(dmaxtot,dmaxt)
            dmeantot = dmeantot + dmeant
            write ( *, 2000 ) nsec              !---Write the new subsection---
            nsec = nsec + 1
            call iwrsec(2,brray)
          end do                                !---End of subsection loop---
        end if
      end do                                    !---End of main section loop---


c       ---------------------------------
c       --- Write out the header info ---

      nxyzb(3) = nsec                           !--- New number of sections ---
      mxyzb(3) = nsec
      cell(1) = float(nxb)
      cell(2) = float(nyb)
      cell(3) = float(nzb)
      call ialsam(2,nxyzb)
      call ialcel(2,cell)
      call ialsiz(2,nxyzb,nxyzstb)

      call date(dat)
      call time(tim)
c       
c       7/7/00 CER: remove the encodes
c       
c       encode ( 80, 3000, title ) dat, tim
      write(titlech,3000) dat,tim
      read(titlech,'(20a4)')(title(kti),kti=1,20)

      dmeantot = dmeantot / nsec

      call iwrhdr(2,title,1,dmintot,dmaxtot,dmeantot)

      call imclose(2)

300   call exit(0)


1000  format ( a50 )
2000  format ( ' Writing new section # ', i4 )
3000  format ( 'EXTSTACK: Extract subsections from a stack.',
     &    9x, a9, 2x, a8 )


      end
