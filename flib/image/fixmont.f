*       * * * * * FIXMONT.FOR * * * * * *
*       
c       FIXMONT will substitute sections from one montage file into another,
c       with a linear scaling of the intensities in the sections being
c       substituted.  It can also be used with ordinary image stacks by
c       making dummy piece lists for each of the stacks.
c       
c       Entries to the program:
c       
c       Name of image file to insert corrected sections into
c       Name of list of piece coordinates for file to be corrected
c       Name of image file with sections to be inserted into other file
c       Name of list of piece coordinates for file with corrections
c       
c       List of section numbers to take from the correction file and
c       substitute into the file to be corrected.  Enter / to take all
c       sections; ranges may be entered (e.g., 9-11,14,7)
c       
c       A pair of intensity scaling factors for each section; intensities
c       will be multiplied by the first factor, then the second factor will
c       be added.  Enter / for factors of 1,0, which will leave intensities
c       unchanged.
c       
c       The same image file (and the same piece list file) may be used
c       for both entries.
c       
c       David Mastronarde   5/7/92
*       
      parameter (idim=3100,lmsec=1000,limpcl=50000)
      COMMON //NX,NY,NZ
C       
      DIMENSION NXYZ(3),MXYZ(3),ARRAY(idim*idim),TITLE(20),
     &    NXYZ2(3),MXYZ2(3)
C       
      CHARACTER*80 FILE1,FILE2,pifil1,pifil2
C       
      EQUIVALENCE (NX,NXYZ)
C       
      integer*4 listz(lmsec)
     &    ,ixpclist1(limpcl),iypclist1(limpcl) ,izpclist1(limpcl)
     &    ,ixpclist2(limpcl),iypclist2(limpcl),izpclist2(limpcl)
      real*4 scmult(lmsec),scadd(lmsec)
      character dat*9,tim*8
c       
c       7/7/00 CER: remove the encode's; titlech is the temp space
c       
      character*80 titlech
c       
      write(*,'(1x,a,$)')'Name of image file to correct: '
      READ(5,101)FILE1
      CALL IMOPEN(1,FILE1,'OLD')
      CALL IRDHDR(1,NXYZ,MXYZ,MODE,DMIN,DMAX,DMEAN)
      IF ((NX*NY.GT.idim*idim)) STOP 'INPUT IMAGE TOO LARGE.'
c       
      write(*,'(1x,a,$)')
     &    'Name of file with list of piece coordinates: '
      READ(5,101)pifil1
101   FORMAT(A)
      call read_piece_list(pifil1,ixpclist1,iypclist1,
     &    izpclist1,npclist1)
c       
      write(*,'(1x,a,$)')
     &    'Name of image file with corrections: '
      READ(5,101)FILE2
      if(file2.eq.file1)then
        iuncor=1
      else
        iuncor=2
        CALL IMOPEN(2,FILE2,'RO')
      endif
      CALL IRDHDR(iuncor,NXYZ2,MXYZ2,MODE2,DMIN2,DMAX2,DMEAN2)
      IF (NXYZ2(1).NE.NX.OR.NXYZ2(2).NE.NY)
     &    STOP 'INPUT IMAGE SIZES DO NOT MATCH.'
c       
      write(*,'(1x,a,$)')
     &    'Name of file with list of piece coordinates: '
      READ(5,101)pifil2
      call read_piece_list(pifil2,ixpclist2,iypclist2,
     &    izpclist2,npclist2)
      call fill_listz(izpclist2,npclist2,listz,nlistz)
c       
      print *,'Enter list of sections to read from correction'//
     &    ' file (/ for all; ranges OK)'
      call rdlist(5,listz,nlistz)
c       
      do ilis=1,nlistz
        scmult(ilis)=1.0
        scadd(ilis)=0.0
      enddo
      print *,'For each section, enter intensity scaling factors',
     &    ' to multiply by',
     &    ' then add (or / for 1,0 to leave all sections unscaled)'
      read(5,*)(scmult(i),scadd(i),i=1,nlistz)
c       
      ipow=0
      if(mode.eq.0.or.(mode.ge.9.and.mode.le.15))then
        ipow=max(8,mode)
        valmax=2**ipow
      endif
      nreplace=0
      do ilis=1,nlistz
        iz=listz(ilis)
        scmul=scmult(ilis)
        scad=scadd(ilis)
        ifanypc=0
        do ipc2=1,npclist2
          if(izpclist2(ipc2).eq.iz)then
            do ipc1=1,npclist1
              if(izpclist1(ipc1).eq.iz.and.
     &            ixpclist1(ipc1).eq.ixpclist2(ipc2).and.
     &            iypclist1(ipc1).eq.iypclist2(ipc2))then
                ifanypc=1
                call imposn(iuncor,ipc2-1,0)
                call irdsec(iuncor,array,*99)
                if(ipow.eq.0)then
                  do i=1,nx*ny
                    array(i)=array(i)*scmul+scad
                  enddo
                else
                  do i=1,nx*ny
                    array(i)=max(0.,min(valmax,array(i)*scmul+scad))
                  enddo
                endif
                call iclden(array,nx,ny,1,nx,1,ny,dmin2,dmax2,dmean2)
                dmin=min(dmin,dmin2)
                dmax=max(dmax,dmax2)
                call imposn(1,ipc1-1,0)
                call iwrsec(1,array)
              endif
            enddo
          endif
        enddo
        if(ifanypc.eq.1)nreplace=nreplace+1
      enddo
      call time(tim)
      call date(dat)
c       
c       7/7/00 CER: remove the encodes
c       
c       ENCODE(80,301,TITLE)nreplace,dat,tim
      write(titlech,301) nreplace,dat,tim
      read(titlech,'(20a4)')(TITLE(kti),kti=1,20)
301   FORMAT('FIXMONT:',i4,' sections replaced',t57,a9,2x,a8)
      CALL IWRHDR(1,TITLE,1,DMIN,DMAX,DMEAN)
      CALL IMCLOSE(1)
      if(iuncor.eq.2)call imclose(iuncor)
C       
      STOP
99    STOP 'FIXMONT READ ERROR'
      END
