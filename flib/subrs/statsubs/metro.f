C       METRO -- VARIABLE METRIC MINIMIZATION W/ FLETCHER'S SWITCHING METHOD
C       
C       REV 1.0         10/1/79 -- JLC
c       
c       $Id$
c       
c       $Log$
c       Revision 3.2  2005/03/28 23:00:42  mast
c       Interpreted a negative GHG as non-positive-definite error
c       
c       Revision 3.1  2004/09/16 16:12:57  mast
c       Changed do loop indentation, declared for implicit none
c       
c       ! Implements variable metric minimization using Fletcher's switching
c       algorithm.
c       ^  [n]: Number of variables
c       ^  [x]: Solution vector of the N variables, suitably initialized
c               and returned with the solution
c       ^  [funct]: the function to evaluate the error, called as
c       ^           call funct(n, x, f, g)
c       ^  [f]: Returned with the error
c       ^  [g]: Array for gradients; must be at least [n] in size
c       ^  [factor]: Initial step size
c       ^  [eps]: Epsilon for convergence test
c       ^  [limitin]: Maximum number of cycles; use a negative value to 
c       ^             suppress output of errors every 10 cycles
c       ^  [ier]: Error code, returned with 0 for success, 1 for DG > 0,
c       ^         2 for linear search lost, 3 for non-positive definite matrix,
c       ^         or 4 for cycle limit reached
c       ^  [h]: Scratch array for Hessian matrix, must be at least N**2+3N
c       ^  [kount]: The number of cycles to convergence
C       !
      subroutine metro(n, x, funct, f, g, factor, eps, limitin, ier, h, kount)
      implicit none
      real*4 X(*),G(*),H(*)
      real*4 factor, eps, f
      integer*4 n, limitin,limit,ier,kount
      external funct
      DOUBLE PRECISION DGAM,DD,S,AK,YA,YB,GA,GB,Z,W,NUJ,NUK
      DOUBLE PRECISION HG,GHG,HGGH,DG,DOT,MU
      integer*4 j, k, ibDirVec, ibHG, ibArg, ibGrad
      real*4 step, gg, p, delta, sig, posdef
      MU = 1.D-04
C       
C       SCRATCH VECTOR STORAGE IN H:
C       H(1 -- N**2)            - COVARIANCE MATRIX
C       H(N**2+1 -- N**2+N)     - OLD ARGUMENT VECTOR
C       H(N**2+N+1 -- N**2+2N)  - OLD GRADIENT VECTOR
C       H(N**2+2N+1 -- N**2+3N) - DIRECTION VECTOR, 
C       OVERWRITTEN W HG WHEN UPDATING MA
C       
c       2/8/07: removed est, it was assigned to step just before factor is

C       EVAL INITIAL ARG, GENERATE IDENTITY MATRIX SCALED BY NORM GRAD

      STEP=FACTOR
      ibArg=N**2
      ibGrad=n**2+n
      ibDirVec=n**2+2*n
      ibHG = ibDirVec
      IER=0
      KOUNT=0
      limit=abs(limitin)
      CALL FUNCT (N,X,F,G)
1     GG=DOT(G,G,N)
      DO J=1,N
        DO  K=1,N
          H((J-1)*N+K)=0.
        enddo
        H((J-1)*N+J)=1./SQRT(GG/N)
      enddo
C       
C       ......................................   MAIN ITERATION LOOP ..............
C       
C       SAVE OLD F,X,G
21    KOUNT=KOUNT+1
C       
      IF(limitin.gt.0.and.MOD(KOUNT,10).EQ.0)WRITE(6,777)KOUNT,F
777   FORMAT(T48,'Cycle',I5,T65,E14.7)
C       
      DO J=1,N
        H(ibArg+J)=X(J)
        H(ibGrad+J)=G(J)
C         
C         COMPUTE DIRECTION VECTOR
        P=0.
        DO K=1,N
          P=P-H((J-1)*N+K)*G(K)
        enddo
        H(ibDirVec+J)=P
      enddo
C       
C       COMPUTE COMPONENT OF NEW GRADIENT ALONG SEARCH VECTOR
      YB=F
      GG=DOT(G,G,N)
      GB=DOT(H(ibDirVec+1),G,N)
      IF (GG.EQ.0.) GO TO 500                   !CONVERGED
      IF (GB.GE.0.D0) GO TO 1                   !CAN'T REDUCE F ALONG THIS LINE --
                                                !RETRY STEEPEST
C       
C       ------------------------------   EXTRAPOLATE -------------------------------
      YA=YB
      GA=GB
      DO  J=1,N
        X(J)=X(J)+STEP*H(ibDirVec+J)
      enddo
      CALL FUNCT (N,X,F,G)
      YB=F
      GB=DOT(H(ibDirVec+1),G,N)
      DG=0.D0
      DO J=1,N
        DG=DG+(X(J)-H(ibArg+J))*H(ibGrad+J)
      enddo
      IF (DG.GE.0.D0) GO TO 600                 !SOMETHING'S FLAKY
      IF ((YB-YA)/DG.GE.MU) STEP=FACTOR         !GOOD IMPROVEMENT -- 
                                                !USE FULL SHIFT NEXT
      IF ((YB-YA)/DG.GE.MU) GO TO 100           !IMPROVEMENT W/OUT LINEAR 
                                                !SEARCH WAS SATISFA
C       
C       NOT ENOUGH IMPROVEMENT -- TRY CUBIC INTERPOLATION IF STEP WAS TOO LARGE
      IF (GB.LT.0..AND.YB.LT.YA) GO TO 95
      S=STEP
      Z=3.D0*(YA-YB)/S+GA+GB
      W=DSQRT(Z**2-GA*GB)
      AK=S*(GB+W-Z)/(GB-GA+2.D0*W)
      IF (DABS(AK).GT.S) AK=AK*0.90D0*S/DABS(AK)
      DO J=1,N
        X(J)=X(J)-AK*H(ibDirVec+J)
      enddo
      CALL FUNCT (N,X,F,G)
      IF (F.GT.YA.OR.F.GT.YB) GO TO 95          !INTERPOLATION FAILED
C       
C       DECREASE STEP SIZE NEXT TIME
      STEP=STEP*DMAX1(1.-AK,1.D-1)
      GO TO 100
C       
C       TRY STEPSIZE OF 0.1**N IF NOTHING ELSE WORKS
95    CALL MOVE (X,H(ibArg+1),4*N)              !RESTORE OLD ARGUMENT
      CALL MOVE (G,H(ibGrad+1),4*N)             !RESTORE OLD GRADIENT
      STEP=0.1*STEP
      IF (STEP/FACTOR.LT.1.E-06) GO TO 700      !ERROR
      GO TO 21                                  !TRY IT
C       -------------------------- END EXTRAPOLATION -----------------------------
C       
C       CHECK CONVERGENCE
100   DELTA=0.
      DGAM=0.D0
      DG=0.D0
      DO J=1,N
        DG=DG+(X(J)-H(ibArg+J))*H(ibGrad+J)
        DGAM=DGAM+(X(J)-H(ibArg+J))*(G(J)-H(ibGrad+J))
        DELTA=DELTA+(X(J)-H(ibArg+J))**2
      enddo
      IF (SQRT(DELTA).LE.EPS) RETURN
      IF (DG.GE.0.D0) GO TO 600                 !ERROR
      IF (KOUNT.GE.LIMIT) GO TO 750
C       
C       UPDATE COVARIANCE MATRIX ACCORDING TO SWITCHING ALGORITHM OF FLETCHER
      GHG=0.D0
      POSDEF=0.
      DO J=1,N
        SIG=0.
        HG=0.D0
        DO K=1,N
          SIG=SIG+H((J-1)*N+K)*G(K)
          HG=HG+H((J-1)*N+K)*(G(K)-H(ibGrad+K))
        enddo
        H(ibHG+J)=HG
        GHG=GHG+(G(J)-H(ibGrad+J))*HG
        POSDEF=POSDEF+G(J)*SIG
      enddo
      IF (POSDEF.LT.0. .or. GHG.LT.0.) GO TO 800 !ERROR
C       
C       H0 ALGORITHM
      DO J=1,N
        DO K=1,N
          DD=(X(J)-H(ibArg+J))*(X(K)-H(ibArg+K))
          HGGH=H(ibHG+J)*H(ibHG+K)
          H((J-1)*N+K)=H((J-1)*N+K)+DD/DGAM-HGGH/GHG
        enddo
      enddo
C       
C       H1 ALGORITHM
      IF (DGAM.LT.GHG) GO TO 21
      DO J=1,N
        NUJ=DSQRT(GHG)*((X(J)-H(ibArg+J))/DGAM-H(ibHG+J)/GHG)
        DO K=1,N
          NUK=DSQRT(GHG)*((X(K)-H(ibArg+K))/DGAM-
     &        H(ibHG+K)/GHG)
          H((J-1)*N+K)=H((J-1)*N+K)+NUJ*NUK
        enddo
      enddo
      GO TO 21                                  !CONTINUE W NEXT DIRECTION
C       ..................................... END MAIN LOOP .....................
C       
C       DONE -- SET ERROR CODE AND RETURN
500   RETURN                                    !NORMAL RETURN
600   IER=1                                     !DG > 0
      RETURN
700   IER=2                                     !LINEAR SEARCH LOST
      RETURN
750   IER=3                                     !KOUNT HAS REACHED LIMIT
      RETURN
800   IER=4                                     !MATRIX NON-POSITIVE DEFINITE
      RETURN
      END
C**     DOT
      DOUBLE PRECISION FUNCTION DOT(A,B,N)
      implicit none
      real*4 A(*),B(*), C
      integer*4 n, j
      DOUBLE PRECISION AD,BD
C       
      DOT = 0.0
      DO J = 1,N
        AD = A(J)
        BD = B(J)
        DOT = DOT + AD*BD
      enddo
C       
      C= DOT
      DOT = C
      RETURN
      END
