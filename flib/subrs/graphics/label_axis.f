c       LABEL_AXIS will label an axis with quality characters 
c       XLO, YLO are the starting coordinates of the axis, RANGE is the
c       length of the axis, SCALE is the factor relating user units to
c       inches, NTICKS is the number of division of a linear axis or the
c       number of ticks on a logarithmic axis, TICKS is an array with the
c       values at which there are ticks for a log axis, IFLOG is 1 if the
c       the axis is logarithmic, ifyax is 0 for an X axis, 1 for a left Y
c       axis, or -1 for a right Y axis.
c
c       $Id$
c       
      subroutine label_axis(xlo,ylo,range,scale,nticks,ticks,iflog,
     &    ifyax)
      real*4 ticks(*)
      dimension ipos(31),lenlab(31)
      character*8 labx(31)
      character*6 axname
      character*80 line
      character*160 pwrstr
      character char
      save nlabs,nlines
      data nlabs/0/,nlines/0/
c       
c       modified for PS - Postscript
      data pwxupi/107./
c       
      if(ifyax.gt.0)then
        axname='left'
      elseif(ifyax.lt.0)then
        axname='right'
      else
        axname='bottom'
      endif
c       
      write(*,'(1x,a,a,a,$)') '# of numeric and # of text labels for '
     &    ,axname, ' axis: '
      read(5,*) nlabs,nlines
      if(nlabs.gt.0)then
        write(*,'(1x,a,$)')'Starting tick, '//
     &      '# ticks between labels (or 0,0 to enter list): '
        read(5,*)ipos(1),intpos
        if(intpos.eq.0)then
          write(*,'(1x,a,$)')'Enter #''s of ticks to put labels at: '
          read(5,*)(ipos(i),i=1,nlabs)
        else
          do 10 i=2,nlabs
            ipos(i)=ipos(i-1)+intpos
10        continue
        endif
        write(*,*)'Enter labels separated by commas or spaces'
        read(5,'(a)')line
        do 30 i=1,nlabs
30        labx(i)='        '
          li=1
          do 40 i=1,nlabs
            ls=li
35          char=line(li:li)
            li=li+1
            if(char.ne.','.and.char.ne.' '.and.ichar(char).ne.9)go to 35
            jl=0
            do 38 j=ls,li-2
              jl=jl+1
              labx(i)(jl:jl)=line(j:j)
38          continue 
            lenlab(i)=jl
40        continue
c           
          write(*,'(1x,a,$)')'Label size and separation from axis: '
          read(5,*)size,ofsline
          jsize=size*pwxupi
c           
c           modification for PS - Postscript
          do 50 ilab=1,nlabs
            if(iflog.eq.0)then
              ofset=(ipos(ilab)-1)*range/nticks
            else
              ofset=scale*alog10(ticks(ipos(ilab))/ticks(1))
            endif
            if(ifyax.gt.0)then
              call psWriteText(xlo-ofsline,ylo+ofset,labx(ilab)(1:lenlab(ilab)),
     &            jsize,0,1)
            elseif(ifyax.lt.0)then
              call psWriteText(xlo+ofsline,ylo+ofset,labx(ilab)(1:lenlab(ilab)),
     &            jsize,0,-1)
            else
              call psWriteText(xlo+ofset,ylo-ofsline-size/2.,labx(ilab)(1:lenlab(ilab)),
     &            jsize,0,0)
            endif
50        continue
        endif
c         
        cenofs=range/2.
        if(iflog.ne.0.and.nticks.gt.0)
     &      cenofs=0.5*scale*alog10(ticks(nticks)/ticks(1))
c         
        do 60 ilin=1,nlines
          write(*,'(1x,a,$)') 'Text size, separation from axis, '//
     &        'center offset along axis: '
          read(5,*)size,ofsline,ofset
          jsize=size*pwxupi
          write(*,*)'Enter text label'
          read(5,'(a)')line
          if(ifyax.gt.0)then
            call psWriteText(xlo-ofsline-size/2.,ylo+cenofs+ofset,trim(line),
     &          jsize,90,0)
          elseif(ifyax.lt.0)then
            call psWriteText(xlo+ofsline+size/2,ylo+cenofs+ofset,trim(line),
     &          jsize,90,0)
          else
            call psWriteText(xlo+cenofs+ofset,ylo-ofsline-size/2.,trim(line),
     &          jsize,0,0)
          endif
60      continue
        return
        end
