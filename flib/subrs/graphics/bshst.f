*       * * * * BSHST * * * * *
c       
c       Subroutine BSHST plots histograms of values belonging to one or more
c       groups, on a graphics device, on the terminal, and via NCAR graphics.
c       
c       Values that must be entered:
c       
c       First indicate what you want to do with the subroutine:
c       .  0 to skip right through the subroutine
c       .  1 to look at histograms
c       .  2 to compute means and SD's and return from the subroutine
c       .  209 to plot out the current contents of the metacode file and
c       .     return from the subroutine.
c       .  -123 to terminate the program
c       
c       The program will then tell you the minimum and maximum values and the
c       .  total range.  If your values are logarithms, it also tells you
c       .  the minimum and maximum of the actual values.
c       
c       Next enter the value at the bottom of the lowest bin, the bin
c       .  width, and the number of bins.  The lowest value must not be
c       .  greater than the minimum value which the program has just informed
c       .  you of.  If your values are logarithms, then your lowest value
c       .  should be an actual (linear) value rather than the logarithm, but
c       .  the bin width must still be specified in log units.
c       .  NOTE:  if you enter 0,0,N you will get N bins that span the
c       .  range of the data.
c       .  NOTE 2: Enter a negative bin width to do kernel smoothing with a
c       .  triweight kernel whose half-width h is the negative of the entered
c       .  value.  In this case, the "number of bins" determines the width
c       .  of the graph, and a non-integer value can be usefully entered.
c       
c       The program tells you the number of counts in the biggest bin.
c       
c       Next enter either:
c       .  To get a plot on the graphics device, enter the desired count for
c       .     the full scale of the Y-axis, which should be bigger than the
c       .     highest bin, or
c       .  0 for a histogram on the terminal, or
c       .  -1 for a histogram on the terminal with bin labels
c       
c       IF you selected a terminal plot, the plot appears and you must enter
c       .  Return to continue
c       
c       Next enter an option to control output:
c       .  0 for none of these options
c       .  1 to type the value out on the terminal
c       .  -1 to write the values into a file
c       .  n+1 to draw n lines on a hard-copy plot; e.g. 3 for 2 lines.
c       
c       IF you elect to write the data to a file, next enter a file name.
c       .  If the file already exists, the data will be appended to the file.
c       
c       At this point a histogram appears on the graphics device unless you
c       .  already had it on the terminal.  The program tells you the range
c       .  of values corresponding to the X axis of the histogram.
c       
c       Next select one of these options:
c       .  0 to return to calling program
c       .  10 to loop back and and new lowest value, bin width, # of bins
c       .  1-3 for plots on X/Y plotter (obsolete)
c       .  4 or 5 for laser printer plot on upper or lower half of page
c       .  11-16 for laser printer plots in one of 6 positions (11-13 is
c       .     left column top to bottom, 14-16 is right column)
c       .  6 for laser printer plot with complete control of size, position,
c       .     and other parameters
c       .  7 for plot with complete control of parameters, and some groups
c       .     plotted below the X-axis
c       .  Enter the negative of a plot number to get a histogram without
c       .     a symbol for each data point.
c       
c       IF you selected a plot other than 7 and the "full scale count"
c       .  entered above is less than the number of counts in the highest
c       .  bin, the program next asks you to enter the full scale count again
c       
c       IF you selected plot 7, the program requests the following:
c       
c       .  The number of groups to place below the X-axis
c       
c       .  The group numbers of those groups (groups are numbered
c       .     sequentially from 1, regardless of the "type" values that were
c       .     used to assign values to different groups)
c       
c       .  The program next informs you of the number of counts in the
c       .     biggest bins above and below the X-axis and wants you to enter
c       .     full scale counts for the Y-axis above and below the X-axis
c       
c       IF you selected 6 or 7, next enter the following:
c       
c       .  Size of plot area in X and Y, and lower left X and Y coordinates,
c       .     (in inches), and number of ticks along the X and Y axis.
c       .     Enter the negative of the lower Y coordinate to offset the
c       .     horizontal grid lines from the edge of the graphing area.
c       .     For a linear axis, the "number of ticks" is actually the
c       .     number of divisions along the axis; but for a logarithmic
c       .     axis, it is the number of tick marks, which is the number of
c       .     divisions plus 1.  Sorry.
c       .     Enter negative the number of ticks to get unidirectional ticks.
c       
c       .  Symbol size, tick length (inches), line thickness for the axes,
c       .     for the histogram, and for the symbols (small integers), and 1
c       .     for a box (axes on the top and right).  If you selected
c       .     unidirectional ticks, positive and negative tick lengths give
c       .     ticks toward the inside and outside of the box, respectively.
c       
c       .  IF you entered a negative lower Y coordinate, enter the amount to
c       .     offset the horizontal grid lines in the Y direction
c       
c       .  1 for a new page, or 0 to plot on the same page as previous graphs
c       
c       IF you are plotting and the values are logarithms, enter the actual
c       .  values (not the logarithms) at which X ticks should be drawn
c       
c       IF you are plotting and said that you wanted to draw some lines,
c       .  next enter the specification for each line.  The format is:
c       .     Slope (Y/X), Y-intercept, starting and ending X coordinate (all
c       .     in your units, not inches), line thickness (small integer), and
c       .     0,0 for a solid line or length of dash and length between
c       .     dashes for a dashed line.  If you enter the negative of line
c       .     thickness, then X and Y are inverted, so the entries are the
c       .     X/Y slope, the X-intercept, and the starting and ending Y
c       .     coordinates.
c       
c       IF you selected graph 6 or 7, then you can next make many entries to
c       .  label the axes and add other symbols and lines to the graph.  The
c       .  FIRST time that you encounter these options (via BSPLT or BSHST),
c       .  you will be asked if you want regular characters or the simpler,
c       .  sans serif characters (enter 1 for the latter).
c       
c       First, for the X axis, enter:
c       
c       .  # of ticks to label with numeric labels, and number of lines of
c       .      text labels.
c       
c       .  IF you specified a nonzero number of ticks to label, next enter:
c       
c       .    If the ticks are to be labeled at regular intervals, enter the
c       .        number of the first tick to label (first tick is #1) and
c       .        the interval between labeled ticks (e.g. 2 for every other
c       .        tick or enter 0,0 to specify a list of ticks to label.
c       
c       .        IF you entered 0,0, next enter the #'s of the ticks to label
c       
c       .    Labels for the ticks, in one line, separated by commas or spaces
c       
c       .    Numeric label size and separation from axis, in inches
c       
c       .  IF you specified a nonzero number of text labels, next enter for
c       .      each label in turn:
c       
c       .     Text label size, separation from axis, and offset along axis
c       .          between center of axis and center of text.
c       
c       .     Text label
c       
c       Next enter these parameters in the same order for the Y axis
c       
c       Finally, the program calls the IMMISC subroutine.  Here, coordinates
c       .  may be specified in one of three ways independently for each of
c       .  the entries to the program.  The coordinates may be in "user"
c       .  units (the units of the numbers being graphed), in absolute units
c       .  of inches on the plotter page, or in units relative to the frame
c       .  of the graph (e.g. 0.1,0.9 for a position in the upper left
c       .  corner, or 0.5,1.1 for a position centered above the graph frame).
c       
c       .  Enter the number of text labels, # of letters in circles,
c       .      # of symbols in boxes, and # of dashed or solid lines to draw
c       
c       .  IF you entered a non-zero # of text labels, then for each, enter:
c       
c       .      X and Y position, and 0 for user or 1 for absolute or -1 for
c       .          relative units 
c       
c       .      0 to center, -1 to left justify, or 1 to right justify the
c       .          character string on this position
c       
c       .      Size of characters in inches, and orientation angle in
c       .          degrees (usually 0 or 90)
c       
c       .      Text, on one line
c       
c       .  IF you entered a non-zero # of circled letters, then for each:
c       
c       .      X and Y position, and 0 for user or 1 for absolute or -1 for
c       .          relative units 
c       
c       .      Diameter of circle in inches, line thickness
c       
c       .      Size of character in inches
c       
c       .      Letter (upper case)
c       
c       .  IF you entered a non-zero # of boxed symbols, then for each:
c       
c       .      X and Y position, and 0 for user or 1 for absolute or -1 for
c       .          relative units 
c       
c       .      Symbol type (or 0 for no symbol), symbol size in inches, 
c       .          symbol thickness (a small integer), box size in inches
c       .          (or 0 for no box), box line thickness
c       
c       .  IF you entered a non-zero # of lines, then for each, enter:
c       
c       .      0 for user, 1 for absolute, or -1 for relative units
c       
c       .      A line specification as described above, in the units just
c       .          indicated
c       
c       NOTE FOR SIMPLEST USE: if you just want standard graohs, enter:
c       .  1 for histograms
c       .  Lowest value, bin width, # of bins
c       .  Full scale count
c       .  0 for no type out
c       .  0 return, 10 replot, or 4 or 5 or 11-16 for laser plots
c       .  1 for new page as needed
c       
      subroutine bshst(namx,xx,ngx,nx,nsymb,ngrps,iz,jz,irec,col
     1    ,iflog)
      dimension namx(*),xx(*),ngx(*),nsymb(*),iz(*),jz(*)
     1    ,irec(*),col(*),xtick(310),xls(6),yls(6),iginv(30)
      common /hstp/ifgnh
      character*80 namef
      character*240 line
      character*1 hisymb(22)
      character*22 pausymb
      logical b3dxor
      save xloin,dxin,nbins,ifplt,ifnewp,fsc,iskp,binsin
      data iskp/0/
      data hisymb/'-','|','<','>','/','\\','X','+','O','@','v','^'
     1    ,'3','4','5','6','7','8','9',' ',' ',' '/
      data xls,yls/0.,0.,0.,4.,4.,4.,5.2,2.6,0.,5.2,2.6,0./
      nkpts=200
      call minmax(xx,nx,xmin,xmax)
      xrange=xmax-xmin
      iftyp=0
      write(*,'('' histograms (1), means only (-1), skip (0),'//
     &    ' exit (-123), plot out (209): '',$)')
      read(5,*)iskp
      if(iskp.eq.209.or.iskp.eq.208)then
        call pltout(209-iskp)
        return
      endif
      if(iskp.eq.-123)then
        call scrnClose
        call psExit()
      endif
      if(iskp)26,82,11
11    write(*,101)xmin,xmax,xrange
101   format(' min, max:',2f10.3,',  range:',f10.3)
      if(iflog.ne.0)then
        xlnmn=10.**xmin
        xlnmx=10.**xmax
        write(*,'('' linear values:'',2f10.3)')xlnmn,xlnmx
      endif
14    write(*,'('' lowest value (linear even if log), bin width or -h,'//
     &    ' # of bins: '',$)')
      read(5,*)xloin,dxin,binsin
      nbins=binsin
      xlo=xloin
      dx=abs(dxin)
      if(dx.eq.0.)then
        if(nbins.lt.5)go to 14
        dx=xrange/(nbins-3)
        xlo=xmin-1.5*dx
      else
        if(iflog.ne.0)xlo=alog10(xloin)
        if(xlo.gt.xmin.or.dx.le.0.or.nbins.le.0)go to 14
      endif
      xhi=xlo+dx*nbins
      if(dxin.lt.0.)xhi=xlo+dx*binsin
      do 18 i=1,nbins
        jz(i)=0
18    continue
c       
c       DNM 5/11/01: add a bit to prevent horrible variability in rounding
c       on PC when data are an even multiple of bin size
c       
      ibmax=0
      do i=1,nx
        ibin=(xx(i)-xlo)/dx+1.0001
        if(ibin.gt.nbins)ibin=nbins
        iz(i)=ibin
        jz(ibin)=jz(ibin)+1
        ibmax=max(ibmax,jz(ibin))
      enddo
      write(*,*) 'highest bin is',ibmax
      write(*,'('' full scale count (0 terminal plot, -1 labels also, -2 counts): '',$)')
      read(5,*)fsc
      call scrnErase(-1)
      if(fsc.le.0.)then
        line(1:1)=' '
        do ibin=1,nbins
          xlin=xlo+dx*(ibin-1)
          linept=1
          if(fsc.lt.0)then
            write(line(2:16),'(f11.4,'':   '')')xlin
            linept=16
          endif
          if(fsc.eq.-2)then
            write(line(17:20),'(i4)')jz(ibin)
            linept=20
          else
            do ng=1,ngrps
              do i=1,nx
                if(ngx(i).eq.ng.and.iz(i).eq.ibin.and.linept.lt.240)then
                  linept=linept+1
                  line(linept:linept)=hisymb(max(1,nsymb(ngx(i))))
c                   &               call chrout(ichar(hisymb(max(1,nsymb(ngx(i))))))
                endif
              enddo
            enddo
          endif
          write(*,'(a)')line(1:linept)
        enddo
        do i=1,22
          pausymb(i:i)=hisymb(i)
        enddo
        call mypause(pausymb)
      else
        call scrnGridLine(10,10,0,100,10)
        xscal=1000/nbins
        yscal=1000/fsc
        idx=xscal
        call scrnGridLine(10,10,idx,0,nbins)
      endif
      do i=1,nbins
        jz(i)=0
      enddo
      write(*,'('' Draw n lines (n+1), type out (1), output to file (-1), or not'//
     &    ' (0): '',$)')
      read(5,*)iftyp
      nlines=0
      if(iftyp.gt.1)then
        nlines=iftyp-1
        iftyp=0
      endif
      iout=6
      if(iftyp.lt.0)then
24      iout=8
        write(*,*) 'enter output file name'
        call flnam(namef,0,'0')
        close(8)
        open(8,file=namef,err=24,status='unknown')
25      read(8,'(a4)',end=26)namef
        go to 25
      endif
26    do 60 ng=1,ngrps
        sx=0.
        sxs=0.
        np=0
        do 50 i=1,nx
          if(ngx(i).eq.ng)then
            sx=sx+xx(i)
            sxs=sxs+xx(i)**2
            np=np+1
            if(iftyp.ne.0)then
              if(ifgnh.eq.0)then
                write(iout,103)ng,namx(i),irec(i),xx(i),col(i)
              else
                write(iout,203)ng,xx(i)
              endif
            endif
103         format(i3,2i7,2f10.3)
203         format(i3,f10.3)
            if(iskp.ge.0.and.fsc.gt.0..and.dxin.ge.0.)then
              ibin=iz(i)
              jz(ibin)=jz(ibin)+1
              ix=xscal*(ibin-0.5)+10
              iy=yscal*(jz(ibin)-0.5)+10
              isymbt=nsymb(ng)
              if(isymbt.lt.0)isymbt=-i
              call scrnSymbol(ix,iy,isymbt)
            endif
          endif
50      continue
        if(dxin.lt.0.and.fsc.gt.0.)then
          do ixk=0,nkpts
            xk=xlo+(ixk/float(nkpts))*(xhi-xlo)
            ix=xscal*(ixk/float(nkpts))*nbins+10
            ysum=0.
            do i=1,nx
              if(ngx(i).le.ng.and.abs(xx(i)-xk).lt.dx)then
                ysum=ysum+(1-((xx(i)-xk)/dx)**2)**3
              endif
            enddo
            ysum=ysum*35./(32.)
            iy=yscal*ysum+10
            if(ixk.eq.0)call scrnMoveAbs(ix,iy)
            call scrnVectAbs(ix,iy)
          enddo
        endif
        if(np.gt.0)then
          xav=sx/np
          xsd=0.
          if(np.gt.1)xsd=sqrt((sxs-np*xav**2)/(np-1))
          sem=xsd/sqrt(1.*np)
          write(*,105)ng,xav,xsd,sem,np
105       format(' group',i3,'  avg =',f10.3,',  sd =',f10.3,',  sem='
     1        ,f10.3,',  n=',i4)
          if(iskp.ge.0.and.fsc.gt.0..and.dxin.ge.0.)then
            call scrnMoveAbs(10,10)
            ix=10
            do 56 ib=1,nbins
              iy=yscal*jz(ib)+10
              call scrnVectAbs(ix,iy)
              ix=xscal*ib+10
              call scrnVectAbs(ix,iy)
56          continue
            call scrnVectAbs(ix,10)
          endif
        endif
60    continue
      call scrnUpdate(1)
      if(iskp.lt.0)go to 82
      write(*,109)xlo,xhi
109   format(' x from',f9.2,' to',f9.2)
      write(*,'('' 4-7 or 11-16 for plot (-# for no points),'//
     &    ' return (0), redisplay (10): '',$)')
      read(5,*)ifplt
      ifcrv=0
      if(iabs(ifplt).gt.100)then
        ifcrv=1
        ifplt=ifplt-isign(100,ifplt)
      endif
      if(abs(ifplt).le.3)return
      if(ifplt.eq.10)go to 11
      iaplt=iabs(ifplt)
      if(abs(fsc).lt.ibmax.and.iaplt.ne.7)then
        write(*,'('' highest bin is'',i4,'', enter full scale count: '',$)') ibmax
        read(5,*)fsc
      elseif(iaplt.eq.7)then
        write(*,'(1x,a,$)')'# of groups to put below axis: '
        read(5,*)ngbelow
        write(*,'(1x,a,$)')'Group #''s: '
        read(5,*)(iginv(i),i=1,ngbelow)
        do 66 nupdown=1,2
          maxup=maxbin
          maxbin=0
          do 61 i=1,nbins
            jz(i)=0
61        continue
          do 64 i=1,nx
            ifbelow=0
            do 63 jb=1,ngbelow
              if(ngx(i).eq.iginv(jb))ifbelow=1
63          continue
            if(b3dxor(nupdown.eq.1, ifbelow.eq.1))then
              jz(iz(i))=jz(iz(i))+1
              maxbin=max(maxbin,jz(iz(i)))
            endif
64        continue
66      continue
        write(*,'('' highest bins up & down are'',2i4'//
     &      ','', enter full scale counts: '',$)')maxup,maxbin
        read(5,*)fscup,fscdown
      endif
      ifimg=iaplt-3
      defscl=1.
      if(ifimg.gt.0)then
        call psSetup(1,wthinch,c2,c3,0)
        defscl=0.74*wthinch/7.5
        iaplt=iaplt-3
      endif
      ntx=10
      nty=10
      xran=6*defscl
      yran=4*defscl
      xl=0
      yl=0
      symwid=0.08
      tiksiz=0.05
      ithsym=1
      ithgrd=1
      ithhis=1
      ifbox=0
      xaxofs=0.
      nhists=1
      if(iaplt.gt.7)then
        xran=3.*wthinch/7.5
        yran=2.*wthinch/7.5
        xl=xls(iaplt-7)*wthinch/7.5
        yl=yls(iaplt-7)*wthinch/7.5
      elseif(iaplt.lt.2)then
        yl=5*defscl
      elseif(iaplt.gt.2)then
        write(*,'('' X and Y size, lower left X and Y, # ticks X and Y: '',$)')
        read(5,*)xran,yran,xl,yl,ntx,nty
        if (ifimg.gt.0)then
          write(*,'('' symbol and tick size, grid, histogram and symbol'//
     &        ' thickness, 1 for box: '',$)')
          read(5,*)symwid,tiksiz,ithgrd,ithhis,ithsym,ifbox
        endif
        if(yl.lt.0.)then
          yl=-yl
          write(*,'(1x,a,$)')'Amount to offset x axis in y: '
          read(5,*)xaxofs
        endif
      endif
      if(ifimg.gt.0)then
        write(*,'('' new page (0 or 1: , gives'',i2,'')?: '',$)')ifnewp
        read(5,*)ifnewp
        if(ifnewp.gt.0)call psFrame()
        call psSymSize(symwid)
        yscal=yran/abs(fsc)
        xll=xl+0.1
        yll=yl+0.1
        iabntx=iabs(ntx)
        call psSetup(ithgrd,c1,c2,c3,0)
        xaxisy=yll-xaxofs
        ylorig=yll
        if(iflog.eq.0)then
          call psGridLine(xll,xaxisy,xran,0.,ntx,tiksiz)
          if(ifbox.ne.0)
     &        call psGridLine(xll,yll+yran+xaxofs,xran,0.,ntx,-tiksiz)
        else
          write(*,'(i3,'' tick values: '',$)')iabntx
          read(5,*)(xtick(i),i=1,iabntx)
          xscl=xran/(xhi-xlo)
          call psLogGrid(xll,xaxisy,xscl,0.,xtick,ntx,tiksiz)
          if(ifbox.ne.0)call psLogGrid
     &        (xll,yll+yran+xaxofs,xscl,0.,xtick,ntx,-tiksiz)
        endif
        xscal=xran/nbins
        call psGridLine(xll,yll,0.,yran,nty,tiksiz)
        if(ifbox.ne.0)call psGridLine(xll+xran,yll,0.,yran,nty,-tiksiz)
        if(iaplt.eq.4)then
          yscal=yran/(fscup+fscdown)
          yll=yll+yscal*fscdown
          call psMoveAbs(xll,yll)
          call psVectAbs(xll+xran,yll)
          nhists=2
        endif
      else
        continue
      endif
      do 801 nupdown=1,nhists
        if(nupdown.eq.2)yscal=-yscal
        do 62 i=1,nbins
          jz(i)=0
62      continue
        do 80 ng=1,ngrps
          ifbelow=0
          np=0
          call psSetup(ithsym,c1,upi,c3,0)
          adjthk=0.5*(ithhis-1)/upi
          if(iaplt.eq.4)then
            do 65 jb=1,ngbelow
              if(ng.eq.iginv(jb))ifbelow=1
65          continue
          endif
          if(b3dxor(nupdown.eq.1, ifbelow.eq.1))then
            do 70 i=1,nx
              if(ngx(i).eq.ng)then
                np=np+1
                ibin=iz(i)
                jz(ibin)=jz(ibin)+1
                if(ifplt.ge.0)then
                  rx=xscal*(ibin-0.5)+xll+adjthk
                  ry=yscal*(jz(ibin)-0.5)+yll+adjthk
                  isymbt=nsymb(ng)
                  if(ifimg.gt.0.and.isymbt.eq.-2)then
                    yfilhi=yscal*jz(ibin)+yll
                    yfillo=yfilhi-yscal
                    xfillo=xscal*(ibin-1)+xll
                    call psSetup(1,c1,upi,c3,0)
                    nfill=xscal*upi
                    do 68 ifill=0,nfill
                      xfil=xfillo+ifill/upi
                      call psMoveAbs(xfil,yfillo)
                      call psVectAbs(xfil,yfilhi)
68                  continue 
                  elseif(dxin.ge.0.)then
                    if(isymbt.lt.0)isymbt=-i
                    if(ifimg.gt.0)call psSymbol(rx,ry,isymbt)
                  endif
                endif
              endif
70          continue
          endif
          if(np.gt.0)then
            if(dxin.lt.0.)then
              call psSetup(ithhis,c1,c2,c3,0)

              do ixk=0,nkpts
                xk=xlo+(ixk/float(nkpts))*(xhi-xlo)
                rx=xscal*(ixk/float(nkpts))*nbins+xll
                ysum=0.
                do i=1,nx
                  if(ngx(i).le.ng.and.abs(xx(i)-xk).lt.dx)then
                    ifbelow=0
                    if(iaplt.eq.4)then
                      do jb=1,ngbelow
                        if(ngx(i).eq.iginv(jb))ifbelow=1
                      enddo
                    endif
                    if(b3dxor(nupdown.eq.1, ifbelow.eq.1))then
                      ysum=ysum+(1-((xx(i)-xk)/dx)**2)**3
                    endif
                  endif
                enddo
                ysum=ysum*35./(32.)
                ry=yscal*ysum+yll
                if(ixk.eq.0)call psMoveAbs(rx,ry)
                call psVectAbs(rx,ry)
              enddo
            elseif(ifimg.gt.0)then
              call psSetup(ithhis,c1,c2,c3,0)
              call psMoveAbs(xll,yll)
              rx=xll
              do 72 ib=1,nbins
                ry=yscal*jz(ib)+yll
                call psVectAbs(rx,ry)
                rx=xscal*ib+xll
                call psVectAbs(rx,ry)
72            continue
              call psVectAbs(rx,yll)
            else
              continue
            endif
          endif
80      continue
801   continue 
      do 805 i=1,nlines
        call psDashInteractive(xscal/dx,xlo,xll,abs(yscal),0.,yll)
805   continue
      if(ifimg.gt.0 .and. iaplt.gt.2 .and. iaplt.lt.7)then
        call label_axis(xll,xaxisy,xran,xran/(xhi-xlo),abs(ntx),
     &      xtick,iflog,0)
        call label_axis(xll,ylorig,yran,abs(yscal),abs(nty), xtick,0,1)
        call psMiscItems(xscal/dx,xlo,xll,xran,abs(yscal),0.,ylorig,yran)
      endif
c       
      if(ifcrv.eq.0)go to 81
      ithcrv=ithhis
      ncrvs=0
      inst=1
95    itycrv=col(inst)+0.0001
      if(itycrv.eq.0)go to 97
      ncrvs=ncrvs+1
      iz(ncrvs)=inst
      inst=inst+5
      if(itycrv.gt.1)inst=inst+4
      go to 95
97    if(ncrvs.eq.0)go to 81
87    write(*,'('' curve # to plot, 1 if sum (-1 this curve range'//
     &    ' only), thickness: '',$)')
      read(5,*)ncrplt,ifsum,ithcrv
      if(ncrplt.le.0.or.ncrplt.gt.ncrvs)go to 81
      if(ifsum.ne.0)then
        write(*,'('' # of paired curve: '',$)')
        read(5,*)ncrpar
      endif
      inst=iz(ncrplt)
      call psSetup(ithcrv,c1,c2,c3,0)
90    itycrv=col(inst)+0.0001
89    go to (91,93),itycrv
91    slope=col(inst+1)
      bint=col(inst+2)
      xcrvlo=amax1(xlo,col(inst+3))
      xcrvhi=amin1(xhi,col(inst+4))
      rx=xscal*(xcrvlo-xlo)/dx+xll
      yact=slope*xcrvlo+bint
      ry=dx*yscal*yact+yll
      call psMoveAbs(rx,ry)
      rx=xscal*(xcrvhi-xlo)/dx+xll
      yact=slope*xcrvhi+bint
      ry=dx*yscal*yact+yll
      call psVectAbs(rx,ry)
      go to 87
93    xcrvlo=amax1(xlo,col(inst+7))
      xcrvhi=amin1(xhi,col(inst+8))
      if(ifsum.gt.0)xcrvhi=amin1(xhi,col(iz(ncrpar)+8))
      dideal=.02*dx/xscal
      ndx=(xcrvhi-xcrvlo)/dideal
      drx=(xcrvhi-xcrvlo)/ndx
      do 96 i=0,ndx
        realx=xcrvlo+i*drx
        call pearev(col,inst,realx,yact)
        if(ifsum.ne.0)then
          call pearev(col,iz(ncrpar),realx,yact2)
          yact=yact+yact2
        endif
        rx=xscal*(realx-xlo)/dx+xll
        ry=dx*yscal*yact+yll
        if(i.eq.0)call psMoveAbs(rx,ry)
        if(i.ne.0)call psVectAbs(rx,ry)
96    continue
      go to 87
81    if(abs(ifplt).le.10)go to 82
      ifplt=ifplt+isign(1,ifplt)
      ifnewp=0
      if(abs(ifplt).le.16)go to 82
      ifnewp=1
      ifplt=isign(11,ifplt)
c       82      if(ifimg.gt.0)call flushb
82    return
      end

      subroutine pearev(col,inst,realx,yact)
      dimension col(100)
93    a1=col(inst+1)
      a2=col(inst+2)
      q1=col(inst+3)
      q2=col(inst+4)
      crvscl=col(inst+6)
      ex=realx+col(inst+5)
      yact=0.
      if(ex.le.-0.99999*a1.or.ex.ge.0.99999*a2)return
      if(q1.gt.0.) yact=crvscl*((1.+ex/a1)**q1)*((1.-ex/a2)**q2)
      if(q1.lt.0.)yact=crvscl*exp(q1*ex**2)
      return
      end

      subroutine gnhst(xx,ngx,nx,nsymb,ngrps,iflog)
      dimension xx(*),ngx(*),nsymb(*),iz(100000),jz(5000)
     1    ,namx(1),col(1),irec(1)
      common /hstp/ifgnh
      data ifgnh/0/
      ifgnh=1
      call bshst(namx,xx,ngx,nx,nsymb,ngrps,iz,jz,irec,col,iflog)
      return
      end

c       block data
c       common /hstp/ifgnh
c       data ifgnh/0/
c       end
