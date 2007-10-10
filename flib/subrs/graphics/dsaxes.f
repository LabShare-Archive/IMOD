c       DSAXES receives the range of X and Y values, determines "nice" limits
c       of X and Y that contain those values, draws axes with rudimentary
c       labels on a graphics device, and returns scaling factors to the
c       calling program.
c       
c       Coordinates adjusted slightly for plotting in an X window.
c       
      SUBROUTINE DSAXES(XMIN,XMAX,YMIN,YMAX,XSCAL,XADD,YSCAL,YADD,
     &    DX,XLO,DY,YLO)
      CALL ERASE(-1)
      CALL AXSUB(XMIN,XMAX,DX,XLO,LFTX,IRTX,'x')

c       It was at 25 and 945
      do i = 0, 5
        ival = lftx + nint(i * (irtx - lftx) / 5.)
        CALL MA(35 + i * 184,5)
        CALL LABEL(ival,5)
      enddo
      CALL AXSUB(YMIN,YMAX,DY,YLO,LFTX,IRTX,'y')

c       This one was at 30 and 1000
      do i = 0, 5
        ival = lftx + nint(i * (irtx - lftx) / 5.)
        CALL MA(3, 30 + i * 194)
        CALL LABEL(ival,5)
      enddo
      CALL DSGRD(90,40,92,0,10)
      CALL DSGRD(1010,40,0,97,10)
      CALL DSGRD(1010,1010,-92,0,10)
      CALL DSGRD(90,1010,0,-97,10)
      CALL UPDAT
      XSCAL=92./DX
      YSCAL=97./DY
      XADD=90-XLO*XSCAL
      YADD=40-YLO*YSCAL
      RETURN
      end

      subroutine AXSUB(XMIN,XMAX,DX,XLO,LFT,IRT,NAME)
      character name
      CALL SCALE(XMIN,XMAX,DX,XLO)
      XHI=10.*DX+XLO
      IPOWR=3-IFIX(ALOG10(AMAX1(ABS(XLO),ABS(XHI))))
      LFT=XLO*10.**IPOWR+SIGN(0.5,XLO)
      IRT=XHI*10.**IPOWR+SIGN(0.5,XHI)
      WRITE(*,30)NAME,XLO,XHI,DX
30    FORMAT(1X,A2,' from',F12.5,' to',F12.5,' at',F11.4,' /div.')
      RETURN
      END
