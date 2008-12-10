c       REPACKSEED is a companion program for the shell script Transferfid.
c       It is used to give the user a list of how points correspond between
c       the original fiducial model and the new seed model, and to repack
c       the seed model to eliminate empty contours.  See man page for details.
c       
c       $Id$
c       Log at end of file
c       
      implicit none
      integer idim
      parameter (idim=10000)
      include 'model.inc'
      character*320 fidname,seedname,outfile,xyzname,matchname
      integer*4 ifid_in_a(idim),maplist(idim)
      integer*4 ixyzcont(idim),ixyzpnt(idim),ixyzobj(idim)
      integer*4 iobj,nmap,noriginal,i,imodobj,imodcont,nxyz,nmaplen, ierr
      integer*4 izOrig, izTrans, ifBtoA, indab, max_obj_orig, ibase, ipt, ip
      real*4 dum, xOrig(idim), yOrig(idim), ximscale, yimscale, zimscale
      character*1 abtext(2)/'A','B'/
      integer*4 lnblnk, getImodScales
      logical*4 readw_or_imod
c       
      write(*,'(1x,a,$)')'Name of fiducial file from first axis: '
      read(5,101)fidname
101   format(a)
      write(*,'(1x,a)')'Enter name of file with X/Y/Z coordinates,'
     &    //' or Return if none available'
      read(5,101)xyzname
      write(*,'(1x,a,$)')'Name of input file with new seed model: '
      read(5,101)seedname
      write(*,'(1x,a,$)')'Name of output file for packed model: '
      read(5,101)outfile
      write(*,'(1x,a,$)')'Name of output file for matching coordinates,'//
     &    ' or Return for none: '
      read(5,101)matchname
      write(*,'(1x,a,$)')'Section numbers in original and second series,'//
     &    ' 1 for B to A: '
      read(5,*)izOrig,izTrans,ifBtoA
      indab = 1
      if (ifBtoA .ne. 0) indab = 2
c       
c       read original file
c       
      if (.not.readw_or_imod(fidname))then
        print *
        print *,'ERROR: REPACKSEED - reading original fiducial file',
     &      fidname
        call exit(1)
      endif
      call scale_model(0)
c       
c       read xyz file if any
c       
      nxyz = 0
      if (xyzname .ne. ' ') then
        call dopen(1, xyzname, 'ro', 'f')
10      read(1,*,end=20)ixyzpnt(nxyz+1), dum, dum, dum,
     &      ixyzobj(nxyz+1), ixyzcont(nxyz+1)
        nxyz = nxyz + 1
        go to 10
20      continue
      endif
      close(1)
c       
c       Keep track of original object numbers
c       
      noriginal=0
      max_obj_orig = max_mod_obj
      do iobj=1,max_mod_obj
        xOrig(iobj) = 0.
        yOrig(iobj) = 0.
        ibase = ibase_obj(iobj)
        do ipt = 1, npt_in_obj(iobj)
          ip = object(ibase + ipt)
          if (nint(p_coord(3, ip) - izOrig) .eq. 0) then
            xOrig(iobj) = p_coord(1, ip)
            yOrig(iobj) = p_coord(2, ip)
          endif
        enddo
          
        if (nxyz .eq. 0) then
c           
c           if no xyz file available, have to assume that every contour with
c           at least one point will be a fiducial
c           
          if(npt_in_obj(iobj).gt.0)then
            noriginal=noriginal+1
            ifid_in_a(iobj)=noriginal
          else
            ifid_in_a(iobj)=0
          endif
        else
c           
c           look for object in xyz list, take number if found
c           
          call objtocont(iobj,obj_color,imodobj,imodcont)
          ifid_in_a(iobj)=0
          do i = 1, nxyz
            if (ixyzobj(i).eq.imodobj .and. ixyzcont(i) .eq. imodcont)
     &          ifid_in_a(iobj) = ixyzpnt(i)
          enddo
        endif
      enddo
c       
c       read new seed model
c       
      if (.not.readw_or_imod(seedname))then
        print *
        print *,'ERROR: REPACKSEED - reading new seed file',
     &      seedname
        call exit(1)
      endif
      call scale_model(0)
      if (matchname .ne. ' ') then
        call dopen(1, matchname, 'new', 'f')
        ierr = getImodScales(ximscale, yimscale, zimscale)
        if (ierr .eq. 0) then
          write(1, '(3i6,f12.3)') izOrig, izTrans, ifBtoA, ximscale
        else
          write(1, '(3i6)') izOrig, izTrans, ifBtoA
        endif
      endif
c       
c       pack objects down and accumulate map list
c       
      nmap=0
      do iobj=1,max_mod_obj
        if (npt_in_obj(iobj).gt.0)then
          if (matchname .ne. ' ' .and. xOrig(iobj) .ne. 0. .and. yOrig(iobj)
     &        .ne. 0 .and. iobj .le. max_obj_orig) then
            ip = object(ibase_obj(iobj) + 1)
            write(1, 107) xOrig(iobj), yOrig(iobj), p_coord(1,ip),p_coord(2,ip)
107         format(4f12.3)
          endif
          nmap=nmap+1
          maplist(nmap)=ifid_in_a(iobj)
          npt_in_obj(nmap)=npt_in_obj(iobj)
          ibase_obj(nmap)=ibase_obj(iobj)
          obj_color(1,nmap)=obj_color(1,iobj)
          obj_color(2,nmap)=obj_color(2,iobj)
        endif
      enddo

      do iobj=nmap+1,max_mod_obj
        npt_in_obj(iobj)=0
      enddo
      max_mod_obj=nmap
c       
c       output the model and the map list
c       
      call scale_model(1)
      call write_wmod(outfile)
      if (matchname .ne. ' ') then
        write(*,108)
108     format(//,' The correspondence between points is as follows',/)
        close(1)
      else
        write(*,102)
102     format(//,' Make the following entries to Setupcombine or ',
     &      'Solvematch to describe how ',/,
     &      ' fiducials correspond between the first and second axes',/)
      endif
      call int_iwrite(outfile, nmap, nmaplen)

      if (ifBtoA .eq. 0) then
        write(*,109)abtext(indab)
109     format(' Points in ',a,': ',$)
        call wrlist(maplist,nmap)
        write(*,110)abtext(3-indab), outfile(1:nmaplen)
110     format(' Points in ',a,': 1-',a)
      else
        write(*,110)abtext(3-indab), outfile(1:nmaplen)
        write(*,109)abtext(indab)
        call wrlist(maplist,nmap)
      endif
      
      if (matchname .ne. ' ') then
        write(*,111)matchname(1:lnblnk(matchname))
111     format(/,' Solvematch will be able to match up points using ',
     &      'data in ',a,/' regardless of how well points track or ',
     &      'whether you add or delete points')
      else
        if (nxyz.eq.0)then
          write(*,103)abtext(indab)
103       format(/,' These lists may be wrong if they include a contour',
     &        ' in ',a,' that will not',/,' correspond to a fiducial ',
     &        'point (e.g., if it has only one point)')
        endif
        write(*,105)
105     format(/,' These lists may be thrown off if you delete a contour',
     &      ' in either set',/,' and will be thrown off if one of the',
     &      ' seed points fails to track')
      endif
      call exit(0)
      end
c       
c       $Log$
c       Revision 3.4  2006/05/04 23:11:23  mast
c       Added option to create file of matching coordinates
c
c       Revision 3.3  2003/05/20 21:41:27  mast
c       Fixed output to work with no spaces in output lists
c	
c       Revision 3.2  2002/07/28 20:28:30  mast
c       Added better analysis of mapping based on fiducial coordinate file
c       from Tiltalign
c	
c       Revision 3.1  2002/05/23 04:42:15  mast
c       Creation of program
c	
