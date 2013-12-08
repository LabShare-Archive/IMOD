*       * * * * EXTPOSITION * * * * *
c       
c       This program produces a list of position numbers for portions of
c       images extracted by the program EXTSTACK.  It is a quick hack of
c       that program.  It requires the two point files used by EXTSTACK,
c       the reference point file and the extraction point file.  In addition,
c       it requires a file of points specifying the location of position 1
c       on whichever sections that can be determined.  Also, one must
c       specify whether position numbers increase in the counter-clockwise
c       or clockwise direction.
c       
c       If there is a position 1 point for a particular section, then the
c       program assigns appropriate position numbers to all of the
c       extraction points on that section; otherwise it assigns a position
c       number of zero. 
c       
c       David Mastronarde 12/1/92  from EXTSTACK by sjm

      parameter ( maxpts=10000 )                !--- Maximum number of pts ---

c       --- Working variables ---
      integer x(maxpts), y(maxpts), z(maxpts), plen, nlen,
     &    xref(maxpts), yref(maxpts), zref(maxpts), ztmp,
     &    xpos(maxpts), ypos(maxpts), zpos(maxpts)
      real      xtmp(maxpts), ytmp(maxpts), amat(2,2),
     &    xcen(maxpts), ycen(maxpts), ccnt(maxpts)
      integer indref(50)
      real angref(50)
      logical done
      character*80 omgfil, reffil, extfil, posfil
      

c       ------------------------------------
c       --- Get the parameters and files ---

      write ( *, * ) ' Reference (complete) point file name : '
      read  ( *, 1000 ) reffil
      write ( *, '( a, $ )' ) ' Expected number of ref. pts / section :'
      read  ( *, * ) expref
      write ( *, * ) ' Extraction point file name : '
      read  ( *, 1000 ) extfil
      write ( *, * ) ' Position 1 point file name : '
      read  ( *, 1000 ) posfil
      write ( *, * ) ' Name of output file for list of positions : '
      read  ( *, 1000 ) omgfil
      write ( *, '(a,$)' ) ' 0 or 1 to count positions'//
     &    ' clockclockwise or clockwise : '
      read  ( *, * ) ifclock

      open ( unit=10, file=extfil, status='old' )
      open ( unit=20, file=reffil, status='old' )
      open ( unit=21, file=posfil, status='old' )
      call dopen(2,omgfil,'new','f')

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
        i=nrefpts+1
        read ( 20, *, end=100 ) xref(i), yref(i), zref(i)
        ztmp = zref(i) + 1
        xcen(ztmp) = xcen(ztmp) + float(xref(i))
        ycen(ztmp) = ycen(ztmp) + float(yref(i))
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

c       read position 1 point file
      done = .false.
      npos = 0
      do while ( .not. done )
        read ( 21, *, end=150 ) xpos(npos+1), ypos(npos+1), zpos(npos+1)
        npos = npos + 1
        if ( npos .gt. 10000 ) done = .true.
      end do

c       --- Read extraction point file ---
150   done = .false.
      npts = 0
      maxsec = 0
      do while ( .not. done )
        read ( 10, *, end=200 ) x(npts+1), y(npts+1), z(npts+1)
        maxsec = max(maxsec,z(npts+1))
        npts = npts + 1
        if ( npts .gt. 10000 ) done = .true.
      end do


c       -------------------------------
c       --- Find the ---
      
200   nsec = 0
      do iz = 0 , maxsec                        !--- Loop over all the sections ---
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

c         --- if the section has point data, then analyse reference points
        if ( nlpts .gt. 0 ) then
c           
c           find position 0 point
          indpos=0
          do ipos=1,npos
            if(zpos(ipos).eq.iz)indpos=ipos
          enddo
c           
c           make list of angles of reference points in section
c           
          ind=0
          do iref=1,nrefpts
            if(zref(iref).eq.iz)then
              ind=ind+1
              angref(ind)=atan2d(yref(iref)-ycen(iz+1),
     &            xref(iref)-xcen(iz+1))
              indref(ind)=iref
            endif
          enddo
c           
c           order the angles
c           
          do i=1,ind-1
            do j=i+1,ind
              if(angref(j).lt.angref(i))then
                angtmp=angref(i)
                angref(i)=angref(j)
                angref(j)=angtmp
                indtmp=indref(i)
                indref(i)=indref(j)
                indref(j)=indtmp
              endif
            enddo
          enddo
c           
c           find point closest to position point for
c           
          indmin=0
          if(indpos.gt.0)then
            distmin=1.e10
            do i=1,ind
              dist=sqrt(float((xref(indref(i))-xpos(indpos))**2 +
     &            (yref(indref(i))-ypos(indpos))**2))
              if(dist.lt.distmin)then
                distmin=dist
                indmin=i
              endif
            enddo
          endif
c           
c           
c           --- Loop over the subsection points for each extraction ---
          do np = 1 , nlpts
            if(indmin.ne.0)then
              distmin=1.e10
              do i=1,ind
                dist=(xtmp(np)-xref(indref(i)))**2+
     &              (ytmp(np)-yref(indref(i)))**2
                if(dist.lt.distmin)then
                  distmin=dist
                  ipmin=i
                endif
              enddo
              itype=ipmin+1-indmin
              if(itype.le.0)itype=itype+ind
              if(ifclock.ne.0)then
                itype=2+ind-itype
                if(itype.gt.ind)itype=1
              endif
            else
              itype=0
            endif
            write(2,'(i2)')itype
          end do                                !---End of subsection loop---
        end if
      end do                                    !---End of main section loop---

      close(2)

300   call exit(0)


1000  format ( a50 )

      end
