c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$

c	  from Press et al, Numerical Recipes, P.292-293.  Lower case added
c	  here.

	SUBROUTINE AMOEBA(P,Y,MP,NP,NDIM,FTOL,FUNK,ITER,ptol,ilo)
	PARAMETER (NMAX=20,ALPHA=1.0,BETA=0.5,GAMMA=2.0,ITMAX=500)
	DIMENSION P(MP,NP),Y(MP),PR(NMAX),PRR(NMAX),PBAR(NMAX),ptol(mp)
	external funk
	MPTS=NDIM+1
	ITER=0
1	ILO=1
	IF(Y(1).GT.Y(2))THEN
	  IHI=1
	  INHI=2
	ELSE
	  IHI=2
	  INHI=1
	ENDIF
	DO 11 I=1,MPTS
	  IF(Y(I).LT.Y(ILO)) ILO=I
	  IF(Y(I).GT.Y(IHI))THEN
	    INHI=IHI
	    IHI=I
	  ELSEIF(Y(I).GT.Y(INHI))THEN
	    IF(I.NE.IHI) INHI=I
	  ENDIF
11	CONTINUE
C	  
c	  check if each point is within certain distance of lowest
c	  
	ifnear=0
	do 112 i=1,mpts
	  do 111 j=1,ndim
	    if(abs(p(i,j)-p(ilo,j)).ge.ptol(j))go to 113
111	  continue
112	continue
c	print *,'All within limits'
	return
113	continue 
c
	RTOL=2.*ABS(Y(IHI)-Y(ILO))/(ABS(Y(IHI))+ABS(Y(ILO)))
	IF(RTOL.LT.FTOL)RETURN
	IF(ITER.EQ.ITMAX) PRINT *,'Amoeba exceeding maximum iterations'
	ITER=ITER+1
	DO 12 J=1,NDIM
	  PBAR(J)=0.
12	CONTINUE
	DO 14 I=1,MPTS
	  IF(I.NE.IHI)THEN
	    DO 13 J=1,NDIM
	      PBAR(J)=PBAR(J)+P(I,J)
13	    CONTINUE
	  ENDIF
14	CONTINUE
	DO 15 J=1,NDIM
	  PBAR(J)=PBAR(J)/NDIM
	  PR(J)=(1.+ALPHA)*PBAR(J)-ALPHA*P(IHI,J)
15	CONTINUE
	YPR=FUNK(PR)
	IF(YPR.LE.Y(ILO))THEN
	  DO 16 J=1,NDIM
	    PRR(J)=GAMMA*PR(J)+(1.-GAMMA)*PBAR(J)
16	  CONTINUE
	  YPRR=FUNK(PRR)
	  IF(YPRR.LT.Y(ILO))THEN
	    DO 17 J=1,NDIM
	      P(IHI,J)=PRR(J)
17	    CONTINUE
	    Y(IHI)=YPRR
	  ELSE
	    DO 18 J=1,NDIM
	      P(IHI,J)=PR(J)
18	    CONTINUE
	    Y(IHI)=YPR
	  ENDIF
	ELSEIF(YPR.GE.Y(INHI))THEN
	  IF(YPR.LT.Y(IHI))THEN
	    DO 19 J=1,NDIM
	      P(IHI,J)=PR(J)
19	    CONTINUE
	    Y(IHI)=YPR
	  ENDIF
	  DO 21 J=1,NDIM
	    PRR(J)=BETA*P(IHI,J)+(1.-BETA)*PBAR(J)
21	  CONTINUE
	  YPRR=FUNK(PRR)
	  IF(YPRR.LT.Y(IHI))THEN
	    DO 22 J=1,NDIM
	      P(IHI,J)=PRR(J)
22	    CONTINUE
	    Y(IHI)=YPRR
	  ELSE
	    DO 24 I=1,MPTS
	      IF(I.NE.ILO)THEN
		DO 23 J=1,NDIM
		  PR(J)=0.5*(P(I,J)+P(ILO,J))
		  P(I,J)=PR(J)
23		CONTINUE
		Y(I)=FUNK(PR)
	      ENDIF
24	    CONTINUE
	  ENDIF
	ELSE
	  DO 25 J=1,NDIM
	    P(IHI,J)=PR(J)
25	  CONTINUE
	  Y(IHI)=YPR
	ENDIF
	GO TO 1
	END
