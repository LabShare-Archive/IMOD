*       * * * * BSPLT * * * * *
c       
c       Subroutine BSPLT allows one to plot one variable against another,
c       with graphical display on either a graphics device, if available, or
c       on the terminal (ANSI standard/VT100 class), and hard copy output via
c       postscript plotting. See man page for details.
c       
c       $Id$
c       
      subroutine bsplt(namx,xx,yy,ngx,nx,nsymb,ngrps,irecx,irecy,colx,coly,iflogx,iflogy)
      character*80 keys(8), xaxisLabel
      common /pltp/igenPltType,ifNoTerm,ifConnect,numKeys, keys, xaxisLabel
      dimension irecx(*),irecy(*),colx(*),coly(*),namx(*)
      dimension xx(*),ngx(*),nsymb(*),yy(*),xtick(310),ytick(310)
      dimension xls(4),yls(4)
      character*80 namfl
      character*1 hisymb(19)
      logical bygrup,docnct
      data hisymb/'-','|','<','>','/','\\','X','+','O','@','v','^'
     &    ,'3','4','5','6','7','8','9'/
      data xls,yls/0.,0.,5.2,5.2,5.2,0.,5.2,0./
      data ifterm/0/, iftyp/0/, ifplt/0/
      save ifterm,iftyp,ifplt
c       
      call minmax(xx,nx,xmin,xmax)
      call minmax(yy,nx,ymin,ymax)
      if(igenPltType.lt.0)then
        ifnonzer=0
        do i=1,nx
          ymin=min(ymin,yy(i)-colx(i))
          ymax=max(ymax,yy(i)+colx(i))
          if(colx(i).ne.0.)ifnonzer=1
        enddo
      endif
      ifterm=0
      if (ifNoTerm .eq. 0) then
        write(*,'('' 0 for graphics plot, 1 for plot on terminal: '',$)')
        read(5,*)ifterm
      endif
      if(ifterm.gt.0)then
        call chrout(27)
        call chrout(ichar('['))
        call chrout(ichar('2'))
        call chrout(ichar('J'))
        call setpos(21,0)
      endif
      call dsaxes(xmin,xmax,ymin,ymax,xsc,xad,ysc,yad,dx,xlo,dy,ylo)
      if(ifterm.eq.0)then
        do i=1,nx
          ng=ngx(i)
          ix=xsc*xx(i)+xad
          iy=ysc*yy(i)+yad
          isymbt=nsymb(ng)
c           
c           If the symbol type is 0, draw connected lines instead
          if(igenPltType.lt.0.or.isymbt.eq.0.or. ifConnect .ne. 0) then
            if (i.gt.1.and.ngx(i).eq.ngx(max(1,i-1))) then
              call va(ix,iy)
            else
              call ma(ix,iy)
            endif
          endif                
          if(isymbt.lt.0)isymbt=-i
          call scpnt(ix,iy,isymbt)
          if(igenPltType.lt.0)then
            idy=ysc*colx(i)
            call ma(ix,iy+idy)
            call va(ix,iy-idy)
            call ma(ix,iy)
          endif
        enddo
c         
c         Output symbols and keys on right
        do i = 1, min(numKeys, ngrps)
          ix = (xlo + 10. * dx) * xsc + xad + 30
          iy = (ylo + (10.5 - i) * dy) * ysc + yad
          if (nsymb(i) > 0) call scpnt(ix, iy, nsymb(i))
          call plax_next_text_align(14)
          call p_sctext(1, 8, 8, 241, ix + 20, iy, keys(i))
        enddo
        if (xaxisLabel .ne. ' ') then
          call plax_next_text_align(3)
          call p_sctext(1, 8, 8, 241, nint((xlo + 5. * dx) * xsc + xad), 2, xaxisLabel)
        endif
        numKeys = 0
        xaxisLabel = ' '
        call updat(1)
      elseif(ifterm.gt.0)then
        do i=1,nx
          jcol=7.9999*(xx(i)-xlo)/dx
          if(igenPltType.lt.0)then
            irow=2.09995*(10.-(yy(i)+colx(i)-ylo)/dy)
            call setpos(irow,jcol)
            call chrout(ichar('-'))
            irow=2.09995*(10.-(yy(i)-colx(i)-ylo)/dy)
            call setpos(irow,jcol)
            call chrout(ichar('-'))
          endif
          irow=2.09995*(10.-(yy(i)-ylo)/dy)
          call setpos(irow,jcol)
          call chrout(ichar(hisymb(max(1,nsymb(ngx(i))))))
        enddo
        call setpos(22,65)
        do i=1,12
          call chrout(ichar(hisymb(i)))
        enddo
      endif

      write(*,'(1x,a,$)')'draw n lines (n+1), type (1), to file (-1), connect '//
     &    '(-thick-1), no (0): '
      read(5,*)iftyp
      nlines=0
      ifcnct=0
      if(iftyp.gt.1) nlines=iftyp-1
      if(iftyp.lt.-1) ifcnct=-1-iftyp
c       if not doing errors, default is to connect all, and addition of
c       100 indicates connect by group; but if doing errors, default is
c       to do by group only, and addition of 100 is needed to specify
c       connect all
      bygrup=(ifcnct.gt.100.and.igenPltType.ge.0).or.
     &    (ifcnct.gt.0.and.ifcnct.lt.100.and.igenPltType.lt.0)
      ifcnct=mod(ifcnct,100)
      if(iabs(iftyp).gt.1)iftyp=0
      iout=6
      if(iftyp)24,26,26
24    iout=8
      write(*,*) 'enter output file name'
      call flnam(namfl,0,'0')
      open(8,file=namfl,err=24,status='unknown')
25    read(8,'(a4)',end=26)namfl
      go to 25
26    call xyset(1)
      write(*,'('' plot # (6-10), - # to set new x/y range, or no plot (0): '',$)')
      read(5,*)ifplt
      if(ifplt.eq.0.and.iftyp.eq.0)go to 60
      iaplt=iabs(ifplt)
      ifimg=iaplt-5
      defscl=1.
      if(ifimg.gt.0)then
        call imset(1,wthinch,c2,c3,0)
        defscl=0.74*wthinch/7.5
        iaplt=iaplt-5
      endif
      ntx=10
      nty=10
      xgrofs=0.
      ygrofs=0.
      xhi=xlo+10.*dx
      yhi=ylo+10.*dy
      xran=4.7*defscl
      yran=4.7*defscl
      symwid=.1
      errlen=0.8*symwid
      tiksiz=0.05
      ithsym=1
      ithgrd=1
      ifbox=0
      if(abs(ifplt).le.5)go to 60
      if(ifplt.lt.0)then
        write(*,101)xmin,xmax,ymin,ymax
101     format(' min and max values of x:',2f10.3,',  y:',2f10.3)
        if(iflogx.ne.0)then
          xlnmn=10.**xmin
          xlnmx=10.**xmax
          write(*,235)xlnmn,xlnmx
235       format(' linear limits of x:',2f10.3)
        endif
        if(iflogy.ne.0)then
          ylnmn=10.**ymin
          ylnmx=10.**ymax
          write(*,236)ylnmn,ylnmx
236       format(' linear limits of y:',2f10.3)
        endif
        write(*,'('' lower and upper limits of x, of y, # ticks x and y: '',$)')
        read(5,*)xlo,xhi,ylo,yhi,ntx,nty
        if(ifimg.gt.0)then
          write(*,'('' tick and symbol size, grid and symbol thickness, 1 for box: '',$)')
          read(5,*)tiksiz,symwid,ithgrd,ithsym,ifbox
          if(igenPltType.lt.0.and.ifnonzer.ne.0)then
            write(*,'(1x,a,$)') 'Length of ticks at ends of error bars (inches): '
            read(5,*)errlen
          endif
        endif
      endif
      if(iaplt.ge.5)then
        write(*,'('' X and Y size, lower left X and Y (- to offset grids): '',$)')
        read(5,*)xran,yran,xll,yll
        if(xll.lt.0.)then
          write(*,'('' grid offset in x: '',$)')
          read(5,*)xgrofs
          xll=-xll
        endif
        if(yll.lt.0.)then
          write(*,'('' grid offset in y: '',$)')
          read(5,*)ygrofs
          yll=-yll
        endif
        if(nsymb(1).eq.-2)then
          frcwid=0.
          frchit=0.
          frctic=0.
          write(*,'(1x,a,$)')'fraction box width, height, tick size: '
          read(5,*)frcwid,frchit,frctic
          call fbxset(frcwid,frchit,frctic)
        endif
      else
        xll=xls(iaplt)*defscl
        yll=yls(iaplt)*defscl
      endif
      if(igenPltType.eq.2)then
        tukwid=0.4
        tuktic=0.1
        tukgap=0.
        ithtuk=1
        write(*,'(1x,a,$)')'Tukey box width, tick size, tick-symbol gap, line thickness: '
        read(5,*)tukwid,tuktic,tukgap,ithtuk
      endif
      ixlo=10+102.*xll
      iylo=10+102.*yll
      xad=ixlo
      yad=iylo
      iabntx=iabs(ntx)
      iabnty=iabs(nty)
      ixran=102.*xran
      if(ifimg.gt.0)then
        call symsiz(symwid)
        call imset(ithgrd,c1,c2,c3,0)
        write(*,'('' new page (0 or 1)?: '',$)')
        read(5,*)ifnewp
        if(ifnewp.ne.0)call frame()
        xad=xll+0.1
        yad=yll+0.1
      endif
      if(iflogx.eq.0.or.ifplt.gt.0)then
        if(ifimg.gt.0)then
          xscal=xran/(xhi-xlo)
          call imgrid(xad,yad-ygrofs,xran,0.,ntx,tiksiz)
          if(ifbox.ne.0) call imgrid(xad,yad+yran+ygrofs,xran,0.,ntx,-tiksiz)
        else
          idx=ixran/ntx
          ixran=idx*ntx
          xscal=ixran/(xhi-xlo)
          call xygrd(ixlo,iylo,idx,0,ntx)
        endif
      else
        xlo=alog10(xlo)
        xhi=alog10(xhi)
        write(*,'(i3,'' x ticks: '',$)')iabntx
        read(5,*)(xtick(i),i=1,iabntx)
        if(ifimg.gt.0)then
          xscal=xran/(xhi-xlo)
          call imlgrd(xad,yad-ygrofs,xscal,0.,xtick,ntx,tiksiz)
          if(ifbox.ne.0) call imlgrd(xad,yad+yran+ygrofs,xscal,0., xtick,ntx,-tiksiz)
        else
          xscal=ixran/(xhi-xlo)
          call logax(ixlo,iylo,xscal,0.,xtick,ntx)
        endif
      endif
      iyran=102.*yran   
      if(iflogy.eq.0.or.ifplt.ge.0)then
        if(ifimg.gt.0)then
          yscal=yran/(yhi-ylo)
          call imgrid(xad-xgrofs,yad,0.,yran,nty,tiksiz)
          if(ifbox.ne.0) call imgrid(xad+xran+xgrofs,yad,0., yran,nty,-tiksiz)
        else
          idy=iyran/nty
          iyran=idy*nty
          yscal=iyran/(yhi-ylo)
          call xygrd(ixlo,iylo,0,idy,nty)
        endif
      else
        ylo=alog10(ylo)
        yhi=alog10(yhi)
        write(*,'(i3,'' y ticks: '',$)')iabnty
        read(5,*)(ytick(i),i=1,iabnty)
        if(ifimg.gt.0)then
          yscal=yran/(yhi-ylo)
          call imlgrd(xad-xgrofs,yad,0.,yscal,ytick,nty,tiksiz)
          if(ifbox.ne.0) call imlgrd(xad+xran+xgrofs,yad,0.,yscal, ytick,nty,-tiksiz)
        else
          yscal=iyran/(yhi-ylo)
          call logax(ixlo,iylo,0.,yscal,ytick,nty)
        endif
      endif
60    if(ifimg.gt.0)call imset(ithsym,c1,upi,c3,0)
      do ng=1,ngrps
        sx=0.
        sy=0.
        sxsq=0.
        sysq=0.
        sxy=0.
        nn=0
        do i=1,nx
          if(ngx(i).ne.ng) cycle
          if(iftyp.ne.0)then
            if(igenPltType.eq.0)then
              write(iout,103)ng,namx(i),irecx(i),xx(i),colx(i),irecy(i),yy(i),coly(i)
            elseif(igenPltType.gt.0)then
              write(iout,203)ng,xx(i),yy(i)
            else
              write(iout,203)ng,xx(i),yy(i),colx(i)
            endif
          endif
103       format(i3,2i7,2f10.3,i10,2f10.3)
203       format(i3,3f10.3)
          sx=sx+xx(i)
          sy=sy+yy(i)
          sxy=sxy+xx(i)*yy(i)
          sxsq=sxsq+xx(i)**2
          sysq=sysq+yy(i)**2
          nn=nn+1
          if(igenPltType.eq.2)colx(nn)=yy(i)        
        enddo
        if(nn.le.1) cycle
        rnum=nn*sxy-sx*sy
        den=nn*sxsq-sx**2
        radic=den*(nn*sysq-sy**2)
        if(radic.le.0.)go to 71
        rr=rnum/sqrt(radic)
        aa=(sy*sxsq-sx*sxy)/den
        bb=rnum/den
        se=0.
        term=(sysq-aa*sy-bb*sxy)
        if(nn.gt.2.and.term.ge.0.)se=sqrt(term/(nn-2))
        sa=se*sqrt(1./nn+(sx**2/nn)/den)
        sb=se/sqrt(den/nn)
        write(*,105)ng,nn,rr,aa,bb,sa,sb
105     format(' grp',i3,', n=',i4,', r=',f6.3,', a=',f10.3,', b='
     &      ,f10.3,', sa=',f9.3,', sb=',f9.3)
71      if(igenPltType.eq.2.and.nn.ge.2.and.ifimg.gt.0)then
c           
c           order values in colx
c           
          do i=1,nn-1
            do j=i+1,nn
              if(colx(j).lt.colx(i))then
                xtmp=colx(i)
                colx(i)=colx(j)
                colx(j)=xtmp
              endif
            enddo
          enddo
c           
c           get percentiles and draw box
c           
          call imset(ithtuk,c1,c2,c3,0)
          tukadj=(ithtuk-1)/upi
          p10=pctile(colx,nn,0.10)
          p25=pctile(colx,nn,0.25)
          p50=pctile(colx,nn,0.50)
          p75=pctile(colx,nn,0.75)
          p90=pctile(colx,nn,0.90)
          write(*,106)ng,nn,p10,p25,p50,p75,p90
106       format(' grp',i3,', n=',i4,',  10,25,50,75,90%s:',5f9.3)
          y10=yscal*(p10-ylo)+yad-tukadj
          y25=yscal*(p25-ylo)+yad-tukadj
          y50=yscal*(p50-ylo)+yad-tukadj
          y75=yscal*(p75-ylo)+yad-tukadj
          y90=yscal*(p90-ylo)+yad-tukadj
          rx=xscal*(sx/nn-xlo)+xad
          xcen=rx-tukadj
          xlft=xcen-tukwid/2.
          xrt=xcen+tukwid/2.
          ticlft=xcen-tuktic/2.
          ticrt=xcen+tuktic/2.
          call imma(xlft,y50)
          call imva(xrt,y50)
          call imma(xlft,y25)
          call imva(xrt,y25)
          call imva(xrt,y75)
          call imva(xlft,y75)
          call imva(xlft,y25)
          call imma(xcen,y25)
          call imva(xcen,y10)
          call imma(xcen,y75)
          call imva(xcen,y90)
          call imma(ticlft,y10)
          call imva(ticrt,y10)
          call imma(ticlft,y90)
          call imva(ticrt,y90)
          call imset(ithsym,c1,c2,c3,0)
c           
c           plot points outside 10/90 plus gap
c           
          do i=1,nn
            yt=max(ylo,min(yhi,colx(i)))
            ry=yscal*(yt-ylo)+yad
            if(ry.lt.y10-tukgap.or.ry.gt.y90+tukgap) call imsymb(rx,ry,nsymb(ng))
          enddo
        endif
      enddo
      if(abs(ifplt).le.5)return
      if(igenPltType.eq.2)go to 82
      conadj=(ifcnct-1)/upi
      nglas=-1
      do i=1,nx
        xt=max(xlo,min(xhi,xx(i)))
        yt=max(ylo,min(yhi,yy(i)))
        docnct=(ifcnct.gt.0).and.(i.gt.1).and.(.not.bygrup.or.ngx(i) .eq.nglas)
        nglas=ngx(i)
        rxlas=rxcon
        rylas=rycon
        rx=xscal*(xt-xlo)+xad
        ry=yscal*(yt-ylo)+yad
        rxcon=rx-conadj
        rycon=ry-conadj
        if(docnct)then
          call imset(ifcnct,c1,c2,c3,0)
          cutdist=1.1*symwid
          fracut=0.
          distot=sqrt((rxcon-rxlas)**2+(rycon-rylas)**2)
          if(isymbt.ne.0.and.distot.gt.1.e-4)fracut=cutdist/distot
          if(fracut.le.0.45)then
            xcut=fracut*(rxcon-rxlas)
            ycut=fracut*(rycon-rylas)
            call imma(rxlas+xcut,rylas+ycut)
            call imva(rxcon-xcut,rycon-ycut)
          endif
          call imset(ithsym,c1,c2,c3,0)
        endif
        isymbt=nsymb(ngx(i))
        if(isymbt.eq.-2.and.ifimg.gt.0)then
          call fracbx(rx,ry,colx(i))
        else
          if(isymbt.lt.0)isymbt=-i
          if(ifimg.le.0)call symbl(int(rx),int(ry),isymbt)
          if(ifimg.gt.0)call imsymb(rx,ry,isymbt)
        endif
        if(ifimg.gt.0.and.igenPltType.lt.0.and.ifnonzer.ne.0)then
          ypos=yscal*(max(ylo,min(yhi,yy(i)+colx(i)))-ylo)+yad-conadj
          yneg=yscal*(max(ylo,min(yhi,yy(i)-colx(i)))-ylo)+yad-conadj
          if(ifcnct.gt.0)call imset(ifcnct,c1,c2,c3,0)
          call imma(rxcon-errlen/2.,yneg)
          call imva(rxcon+errlen/2.,yneg)
          call imma(rxcon-errlen/2.,ypos)
          call imva(rxcon+errlen/2.,ypos)
          call imma(rxcon,yneg)
          call imva(rxcon,ypos)
          if(docnct)call imset(ithsym,c1,c2,c3,0)
        endif
      enddo
82    call penup(1)
      do il=1,nlines
        call imdash(xscal,xlo,xad,yscal,ylo,yad)
      enddo
      if(ifplt.lt.0.and.ifimg.gt.0) then
        call label_axis(xad,yad-ygrofs,xran,xscal,abs(ntx),xtick,iflogx,0)
        call label_axis(xad-xgrofs,yad,yran,yscal,abs(nty),ytick,iflogy,1)
        call immisc(xscal,xlo,xad,xran,yscal,ylo,yad,yran)
      endif
c         if(ifimg.gt.0)call flushb
      return
      end

      subroutine gnplt(xx,yy,ngx,nx,nsymb,ngrps,iflogx,iflogy)
      dimension xx(*),ngx(*),nsymb(*),yy(*)
      dimension irecx(1),irecy(1),colx(1),coly(1),namx(1)
      common /pltp/igenPltType,ifNoTerm,ifConnect,numKeys
      data igenPltType/0/
      data ifNoTerm/0/
      data ifConnect/0/
      data numKeys/0/
      igenPltType=1
      call bsplt(namx,xx,yy,ngx,nx,nsymb,ngrps,irecx,irecy,colx,coly,iflogx,iflogy)
      return
      end

      subroutine errplt(xx,yy,ngx,nx,nsymb,ngrps,colx,iflogx,iflogy)
      dimension xx(*),ngx(*),nsymb(*),yy(*),colx(*)
      dimension irecx(1),irecy(1),coly(1),namx(1)
      common /pltp/igenPltType
      igenPltType=-1
      call bsplt(namx,xx,yy,ngx,nx,nsymb,ngrps,irecx,irecy,colx,coly,iflogx,iflogy)
      return
      end

      subroutine boxplt(xx,yy,ngx,nx,nsymb,ngrps,colx,iflogx,iflogy)
      dimension xx(*),ngx(*),nsymb(*),yy(*),colx(*)
      dimension irecx(1),irecy(1),coly(1),namx(1)
      common /pltp/igenPltType
      igenPltType=2
      call bsplt(namx,xx,yy,ngx,nx,nsymb,ngrps,irecx,irecy,colx,coly,iflogx,iflogy)
      return
      end

c       block data
c       common /pltp/ igenPltType
c       data igenPltType/0/
c       end


      subroutine setpos(irow,jcol)
      call chrout(27)
      call chrout(ichar('['))
      idig1=(irow+1)/10
      if(idig1.gt.0)call chrout(idig1+48)
      call chrout(48+(irow+1-10*idig1))
      call chrout(ichar(';'))
      idig2=(jcol+1)/100
      idig1=(jcol+1-100*idig2)/10
      if(idig2.gt.0)call chrout(idig2+48)
      if(idig1.gt.0.or.idig2.gt.0)call chrout(idig1+48)
      call chrout(48+(jcol+1-10*idig1-100*idig2))
      call chrout(ichar('H'))
      return
      end       



      function pctile(x,n,p)
      real*4 x(*)
      v=n*p+0.5
      v=max(1.,min(float(n),v))
      iv=min(v,n-1.)
      f=v-iv
      pctile=(1.-f)*x(iv)+f*x(iv+1)
c       write(*,'(f6.0,f9.3)')100.*p,pctile
      return
      end
