*       * * * * * CONTOURMOD * * * * * *
c       
c       CONTOURMOD will generate contour maps of selected sections in an
c       image file and place them into a WIMP model that can then be
c       displayed on the images.  You can produce contours at regularly
c       spaced intervals or at individually specified levels.  Each contour
c       level may have a different color.
c       
c       The contours are lines connecting points spaced about 1 pixel apart
c       or less; thus a complete set of contour lines for one image can
c       have quite a large number of points, especially if the image is
c       large or has a lot of fine detail.  The program will give an error
c       message and stop adding points to the model if no further points can
c       fit in the model storage arrays.  If this happens when contouring
c       several sections, you need to make more than one model file.  If it
c       happens when contouring a single image, the image is too large or too
c       detailed and needs to be reduced in size or filtered.
c       
c       Entries to the program are:
c       
c       Name of file with images to contour
c       
c       Name of model file to place contours into
c       
c       List of sections to contour, or / for all sections in the image file.
c       Ranges may be entered, e.g. 0-2,5,7
c       
c       X and Y scaling factors to apply to the model relative to the image
c       pixels, or / for 1,1.  If you are going to display the contours on
c       an image that has been expanded by a certain factor with a
c       transformation, it is recommended that you generate the contours on
c       the original rather than the expanded image, so as to avoid getting
c       a contour model with a vast number of points.  In this case, you
c       would enter that expansion factor as the X and Y scaling factors, and
c       the contour model would fit the expanded images.
c       
c       Enter a positive number of contours for regularly spaced contours,
c       or the negative of the number of contours if you want to specify
c       each contour level individually.
c       
c       IF you entered a positive number greater than 1, next enter the
c       lowest and highest contour levels desired.
c       
c       IF you entered 0 or 1, instead enter the single contour level
c       desired.
c       
c       IF you entered a negative number, instead enter all of the desired
c       contour levels.
c       
c       Finally, enter an intensity level or color (0 to 255) for each
c       contour level.  If you might need to change the colors or turn any
c       contours off inside WIMP, it is recommended that you give each level
c       a unique color value.  For example, if you want several bright white
c       or several black lines, make them 240, 239, 238, etc or 0, 1, 2, etc.
c       
c       David Mastronarde  12/1/90, using CONTUR package from Tim Baker
c       
      parameter (maxarr=8200*8200,maxcolrow=210000,maxsec=10000,
     &    maxtour=1000)
c       
      include 'model.inc'
c       
      real*4 array(maxarr),xp(maxcolrow),yp(maxcolrow)
      integer*2 img(maxarr)
      integer*4 nxyz(3),mxyz(3),listz(maxsec),icoltour(maxtour)
      real*4 tour(maxtour)
      equivalence (nxyz,nx)
      character*160 imagefile,modelfile
      common /veccom/nx,ny,nz,icolor,izcur,xofset,yofset,sclx,scly
c       
      write(*,'(1x,a,$)')'Name of image file to contour: '
      read(5,100)imagefile
100   format(a)
      call imopen(1,imagefile,'ro')
      call irdhdr(1,nxyz,mxyz,mode,dmin,dmax,dmean)
      if (nx * ny .gt. maxarr) then
        print *,'ERROR: contourmod - image too large for arrays'
        call exit(1)
      endif
c       
      write(*,'(1x,a,$)')'Name of model file to place contours in: '
      read(5,100)modelfile
c       
      nlistz=nz
      do i=1,nz
        listz(i)=i-1
      enddo
c       
      print *,'Enter list of sections to contour (ranges are OK;',
     &    ' / for all sections)'
      call rdlist(5,listz,nlistz)
c       
      sclx=1.
      scly=1.
      write(*,'(1x,a,$)')'X and Y scaling factors (/ for 1,1): '
      read(5,*)sclx,scly
      xofset=0.+0.5*sclx
      yofset=0.+0.5*scly
c       
      write(*,'(1x,a,/,a,/,a,$)')'For regularly spaced contours,'//
     &    ' enter the number of contours that you want;',
     &    ' For irregularly spaced contours (at individually'//
     &    ' specified levels),',
     &    '  enter the negative of the number of contours: '
      read(5,*)ntour
c       
      if(ntour.gt.1)then
        write(*,'(1x,a,$)')'Lowest and highest contour levels: '
        read(5,*)tourlo,tourhi
        tourdiff=(tourhi-tourlo)/(ntour-1)
        do i=1,ntour
          tour(i)=tourlo+(i-1)*tourdiff
        enddo
      elseif(ntour.lt.0)then
        ntour=-ntour
        write(*,'(1x,a,$)')'Individual contour levels: '
        read(5,*)(tour(i),i=1,ntour)
      else
        ntour=1
        write(*,'(1x,a,$)')'Contour level: '
        read(5,*)tour(1)
      endif
c       
      print *,'Enter an intensity/color for each contour'
      read(5,*)(icoltour(i),i=1,ntour)
c       
c       zero out the model
c       
      n_object=0
      n_point=0
      n_clabel=0
c       
c       loop on sections
c       
      do ilist=1,nlistz
        izcur=listz(ilist)
        if(izcur.ge.0.and.izcur.lt.nz)then
          print *,'. . . Contouring section #',izcur
          call imposn(1,izcur,0)
          call irdsec(1,array,*99)
c           
c           loop on contours.  The CONTUR program assumes the image was
c           padded on top and right, so just tell it nx-1 and ny-1
c           
          do itour=1,ntour
            icolor=icoltour(itour)
            call contur_a(array,img,xp,yp,nx-1,ny-1,sclx,scly,
     &          tour(itour))
          enddo
        endif
      enddo
c       
c       write model
c       
      max_mod_obj=n_object
      print *,n_point,' points in',n_object,' objects written in model'
      call write_wmod(modelfile)
      call exit(0)
99    stop 'ERROR READING IMAGE FILE'
      end


      subroutine vecplt(xplot,yplot,itype)
      include 'model.inc'
      common /veccom/nx,ny,nz,icolor,izcur,xofset,yofset,sclx,scly
      logical toomuch/.false./
c       
c       write(*,'(i3,2f8.2)')itype,xplot,yplot
      if(toomuch)return
c       
c       if new object (itype=3) then increment object # and set up new
c       object
c       
      if(itype.eq.3)then
c         
        if(n_object.lt.max_obj_num-1)then
          n_object=n_object+1
          obj_color(1,n_object)=1
          obj_color(2,n_object)=icolor
          ibase_obj(n_object)=n_point
          npt_in_obj(n_object)=0
        else
          print *,'TOO MANY OBJECTS TO FIT IN MODEL'
          toomuch=.true.
          return
        endif
      endif
c       
c       now add point to object
c       
      if(n_point.lt.max_pt-3)then
        n_point=n_point+1
        npt_in_obj(n_object)=npt_in_obj(n_object)+1
        object(n_point)=n_point
        p_coord(1,n_point)=xplot+xofset
        p_coord(2,n_point)=yplot+yofset
        p_coord(3,n_point)=izcur
        pt_label(n_point)=0
      else
        print *,'TOO MANY POINTS TO FIT IN MODEL'
        toomuch=.true.
        return
      endif
      return
      end

C       **************************************************************************
C       *                                                                        *
C       *                     *****     CONTUR.FOR     *****                     *
C       *                                                                        *
C       *    THIS CONTAINS A SERIES OF SUBROUTINES USED FOR CONTOURING "MAPS"    *
C       *                                                                        *
C       *    THE SUBROUTINE NAMES USED HERE ARE CHANGED FROM THE ORIGINAL NAMES: *
C       *                                                                        *
C       *        NEW NAME   ORIGINAL NAME                                        *
C       *        --------   -------------                                        *
C       *        CONTUR_A      AUX095                                            *
C       *        CONTUR_B      AUX098                                            *
C       *        CONTUR_C      AUX096                                            *
C       *        CONTUR_D      AUX097                                            *
C       *                                                                        *
C       *    THESE SUBROUTINES ARE CURRENTLY USED IN THE PROGRAMS EMMAPDSP,      *
C       *     EMMAPENV AND EMFFTLAT.                                             *
C       **************************************************************************
C       
      SUBROUTINE CONTUR_A (MAP,IMG,XP,YP,NCOL,NROW,SCLX,SCLY,TOUR)
C       
      IMPLICIT NONE
C       
      COMMON    /COM031/ IRP,JRP,ISP,JSP,NNX,NNY
      INTEGER*4 NCOL, NROW, IRP, JRP, ISP, JSP, NNX, NNY, I, J
      INTEGER*4 ITEMP, JTEMP, II, JJ, CONTUR_B, PRINT
      INTEGER*2 IMG(NCOL+1,NROW+1)
      REAL*4    MAP(NCOL+1,NROW+1), XP(NCOL+1), YP(NROW+1)
      REAL*4    SCLX, SCLY, TOUR
C       ----------------------------------------------------------------------------
C       
      PRINT = -1
      DO I = 1,(NCOL+1)
        XP(I) = I-1
      ENDDO
      DO I = 1,(NROW+1)
        YP(I) = I-1
      ENDDO
      NNX  = NCOL+1
      NNY  = NROW+1
      DO J = 1,(NROW+1)                         ! ZERO "IMG" ARRAY
        DO I = 1,(NCOL+1)
          IMG(I,J) = 0
        ENDDO
      ENDDO
C       
      DO I = 2,NNX
        JRP = 1
        JSP = 1
        IF(MAP(I,1).GT.TOUR) THEN
          J = I-1
          IF(MAP(J,1).LE.TOUR) THEN
            IRP = I
            ISP = J
            CALL CONTUR_C (MAP,IMG,XP,YP,NCOL,NROW,PRINT,SCLX,SCLY,
     &          TOUR)
            PRINT = -1
          ENDIF
        ENDIF
      ENDDO
C       
      DO I = 2,NNY
        ISP = NNX
        IRP = NNX
        IF(MAP(NNX,I).GT.TOUR) THEN
          J = I-1
          IF(MAP(NNX,J).LE.TOUR) THEN
            JRP = I
            JSP = J
            CALL CONTUR_C (MAP,IMG,XP,YP,NCOL,NROW,PRINT,SCLX,SCLY,
     &          TOUR)
            PRINT = -1
          ENDIF
        ENDIF
      ENDDO
C       
      ITEMP = NNX-1
      DO I = 1,ITEMP
        JRP = NNY
        JSP = NNY
        II  = NNX-I
        IF(MAP(II,NNY).GT.TOUR) THEN
          J = II+1
          IF(MAP(J,NNY).LE.TOUR) THEN
            IRP = II
            ISP = J
            CALL CONTUR_C (MAP,IMG,XP,YP,NCOL,NROW,PRINT,SCLX,SCLY,
     &          TOUR)
            PRINT = -1
          ENDIF
        ENDIF
      ENDDO
C       
      JTEMP = NNY-1
      DO I = 1,JTEMP
        IRP = 1
        ISP = 1
        II = NNY-I
        IF(MAP(1,II).GT.TOUR) THEN
          J = II+1
          IF(MAP(1,J).LE.TOUR) THEN
            JRP = II
            JSP = J
            CALL CONTUR_C (MAP,IMG,XP,YP,NCOL,NROW,PRINT,SCLX,SCLY,
     &          TOUR)
            PRINT = -1
          ENDIF
        ENDIF
      ENDDO
C       
      DO I = 2,JTEMP
        DO J = 2,NNX
          JJ = J-1
          IF(MAP(J,I).GT.TOUR) THEN
            IF(MAP(JJ,I).LE.TOUR) THEN
              IF(CONTUR_B(IMG,NCOL,NROW,J,I,0).NE.1) THEN
                IRP = J
                ISP = JJ
                JRP = I
                JSP = I
                CALL CONTUR_C (MAP,IMG,XP,YP,NCOL,NROW,PRINT,SCLX,
     &              SCLY,TOUR)
                PRINT = -1
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDDO
C       
      RETURN
      END
C       *********************************************************************
      FUNCTION CONTUR_B (IMG, NCOL, NROW, NI, NJ, IND)
C       
      IMPLICIT NONE
C       
      INTEGER*4 CONTUR_B, NI, NJ, IND, NCOL, NROW
      INTEGER*2 IMG(NCOL+1,NROW+1), I, J, ITEMP
C       ----------------------------------------------------------------------------
C       
      ITEMP    = 0
      CONTUR_B = 0
      IF(IMG(NI,NJ).EQ.1) THEN
        ITEMP    = 1
        CONTUR_B = 1
      ENDIF
      IF(IND.NE.0) IMG(NI,NJ) = 1
C       
      RETURN
      END
C       ****************************************************************************
      SUBROUTINE CONTUR_C (MAP,IMG,XP,YP,NCOL,NROW,PRINT,SCLX,SCLY,
     &    TOUR)
C       
      IMPLICIT NONE
C       
      COMMON    /COM031/ IRP,JRP,ISP,JSP,NNX,NNY
      INTEGER*4 IT(9), JT(9), LOCATE, CONTUR_B, PRINT, NCOL, NROW
      INTEGER*4 IRP, JRP, ISP, JSP, NNX, NNY, IN, JN, INN, JNN
      INTEGER*2 IMG(NCOL+1,NROW+1)
      REAL*4    MAP(NCOL+1,NROW+1), XP(NCOL+1), YP(NROW+1)
      REAL*4    TOUR, ZPP, SCLX, SCLY, VX, VY, HTM
      LOGICAL   DTEST(9)
      DATA      IT / 0,1,1, 0,9,0,-1,-1,0/
      DATA      JT /-1,0,0,-1,9,1, 0, 0,1/
      DATA      DTEST /.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE.,
     1    .TRUE.,.FALSE.,.TRUE.,.FALSE./
C       ----------------------------------------------------------------------------
C       
      GO TO 200
100   IRP = IN
      JRP = JN
200   CALL CONTUR_D (XP(IRP),YP(JRP),MAP(IRP,JRP),XP(ISP),YP(JSP),
     1    MAP(ISP,JSP), PRINT, SCLX, SCLY, TOUR)
      PRINT = 1
      LOCATE = 3*(JRP-JSP)+IRP-ISP+5
      IN     = ISP+IT(LOCATE)
      JN     = JSP+JT(LOCATE)
      IF(IN.LE.NNX.AND.IN.GE.1.AND.JN.LE.NNY.AND.JN.GE.1) GO TO 300
      RETURN
C       
300   IF(LOCATE.EQ.6) THEN
        IF(CONTUR_B(IMG,NCOL,NROW,IRP,JRP,1).EQ.1) RETURN
      ENDIF
C       
      IF(.NOT.DTEST(LOCATE)) THEN
        ZPP = MAP(IN,JN)
        IF(ZPP.GT.TOUR) GO TO 100
        ISP = IN
        JSP = JN
        GO TO 200
      ENDIF
C       
      VX     = (XP(IRP)+XP(IN))*0.5
      VY     = (YP(JRP)+YP(JN))*0.5
      LOCATE = 3*(JRP-JN)+IRP-IN+5
      INN    = IN+IT(LOCATE)
      JNN    = JN+JT(LOCATE)
      HTM    = (MAP(IRP,JRP)+MAP(ISP,JSP)+MAP(IN,JN)+MAP(INN,JNN))/4.0
      IF(HTM.GT.TOUR) GO TO 800
      CALL CONTUR_D (XP(IRP), YP(JRP), MAP(IRP,JRP), VX, VY, HTM,
     &    PRINT, SCLX, SCLY, TOUR)
      IF(MAP(INN,JNN).LE.TOUR) THEN
        ISP = INN
        JSP = JNN
        GO TO 200
      ENDIF
C       
      CALL CONTUR_D (XP(INN), YP(JNN), MAP(INN,JNN), VX, VY, HTM,
     &    PRINT, SCLX, SCLY, TOUR)
      IF(MAP(IN,JN).LE.TOUR) THEN
        IRP = INN
        JRP = JNN
        ISP = IN
        JSP = JN
        GO TO 200
      ENDIF
      CALL CONTUR_D (XP(IN), YP(JN), MAP(IN,JN), VX, VY, HTM,
     &    PRINT, SCLX, SCLY, TOUR)
      GO TO 100
800   CONTINUE
      CALL CONTUR_D (VX, VY, HTM, XP(ISP), YP(JSP), MAP(ISP,JSP),
     &    PRINT, SCLX, SCLY, TOUR)
      IF(MAP(IN,JN).GT.TOUR) GO TO 100
      CALL CONTUR_D (VX, VY, HTM, XP(IN), YP(JN), MAP(IN,JN),
     &    PRINT, SCLX, SCLY, TOUR)
      IF(MAP(INN,JNN).GT.TOUR) GO TO 900
      CALL CONTUR_D (VX, VY, HTM, XP(INN), YP(JNN), MAP(INN,JNN),
     &    PRINT, SCLX, SCLY, TOUR)
      ISP = INN
      JSP = JNN
      GO TO 200
900   ISP = IN
      JSP = JN
      IRP = INN
      JRP = JNN
      GO TO 200
C       
      END
C       ****************************************************************************
      SUBROUTINE CONTUR_D (XR,YR,HR,XS,YS,HS,IPLT,SCLX,SCLY,TOUR)
C       
      IMPLICIT NONE
C       
      INTEGER*4 IPLT
      REAL*4    XR, YR, XS, YS, XPLOT, YPLOT, FRAC, HR, HS
      REAL*4    TOUR, SCLX, SCLY
C       ----------------------------------------------------------------------------
C       
      FRAC  = (HR-TOUR)/(HR-HS)
      XPLOT = SCLX*(XR-FRAC*(XR-XS))
      YPLOT = SCLY*(YR-FRAC*(YR-YS))
      IF(IPLT.EQ.1) THEN
        CALL VECPLT (XPLOT, YPLOT, 2)
      ELSE
        CALL VECPLT (XPLOT, YPLOT, 3)
      ENDIF
C       
      RETURN
      END
