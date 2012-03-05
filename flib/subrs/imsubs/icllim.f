C       *ICLLIM
C       
C       Subroutine to aid with selecting a region of an image.
C       This routine will ask the user to enter region limits
C       (0,0 = lower left corner) and will return pixel coords
C       for bounds. The deafult input / will return full image
C       limits. All requests are truncated to actual image boundary.
C       For MODE 3,4 images, values refer to complex values.
C       Returned pixel coordinates are RELATIVE to start of image.
C       ie. XYZmin = 0 means @ whatever start point is on file.
C       
C       IXYZMIN(3)      Lower pixel limits
C       IXYZMAX(3)      Upper pixel limits
C       MXYZ(3)         Number of pixels
C       
      SUBROUTINE ICLLIM(ISTREAM,IXYZMIN,IXYZMAX,MXYZ)
      use imsubs
C       
      DIMENSION IXYZMIN(3),IXYZMAX(3),MXYZ(3)
C       
      J = LSTREAM(ISTREAM)
      CALL ZERO(IXYZMIN,NBW3)
      CALL ZERO(IXYZMAX,NBW3)
      CALL MOVE(MXYZ,NCRS(1,J),NBW3)
      IXYZMAX(3) = MXYZ(3) - 1
C       
      IF (MXYZ(3) .EQ. 1) THEN
        WRITE(6,1000) MXYZ(1),MXYZ(2)
1000    FORMAT(/,' Number of points (X,Y)= ',2I5,/'$Enter Limits',
     .      ' (Xmin,max,Ymin,max) [ (0,0) is lower-left corner ] : ')
        READ(5,*,END=10) (IXYZMIN(K),IXYZMAX(K),K=1,2)
      ELSE
        WRITE(6,1100) MXYZ
1100    FORMAT(/,' Number of points (X,Y,Z)= ',3I5,/,' Enter Limits',
     .      ' (Xmin,max,Ymin,max,Zmin,max) [ (0,0) is',
     .      '  lower-left corner ] : ')
        READ(5,*,END=10) (IXYZMIN(K),IXYZMAX(K),K=1,3)
      END IF
C       
C       If min,max both= 0 then use full range (except in Z),else make sure in range
C       
10    DO 100 K = 1,3
        IMIN = 0                                !for relative start
        IMAX = IMIN + MXYZ(K) - 1
        IF (IXYZMIN(K).EQ.0 .AND. IXYZMAX(K).EQ.0 .AND. K.NE.3) THEN
          IXYZMIN(K) = IMIN
          IXYZMAX(K) = IMAX
        ELSE
          IF (IXYZMIN(K) .LT. IMIN) IXYZMIN(K) = IMIN
          IF (IXYZMAX(K) .GT. IMAX) IXYZMAX(K) = IMAX
        END IF
        MXYZ(K) = IXYZMAX(K) - IXYZMIN(K) + 1
100   CONTINUE
C       
      RETURN
      END
c       
