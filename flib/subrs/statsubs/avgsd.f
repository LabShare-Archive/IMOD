c	  AVGSD calculates the mean AVG, standard deviation SD, and standard
c	  error of mean SEM, from the N values in array X
c
	subroutine avgsd(x,n,avg,sd,sem)
	dimension x(*)
	sx=0.
	sxsq=0.
	do i=1,n
	  sx=sx+x(i)
	enddo
	avg=sx/n
	sx=0.
	do i=1,n
	  d=x(i)-avg
	  sx=sx+d
	  sxsq=sxsq+d**2
	enddo
	call sums_to_avgsd(sx,sxsq,n,avnew,sd)
	avg=avg+avnew
	sem=0.
	if(n.gt.0)sem=sd/sqrt(float(n))
	return
	end



c	  SUMS_TO_AVGSD computes a mean AVG and standard deviation SD from the
c	  sum of values SX, sum of squares SXSQ, and number of value N.
c	  It will not generate any division by 0 errors.
c
	subroutine sums_to_avgsd(sx,sxsq,n,avg,sd)
	avg=0.
	sd=0.
	if(n.le.0)return
	avg=sx/n
	if(n.gt.1)sd=sqrt(max(0.,(sxsq-n*avg**2)/(n-1.)))
	return
	end

c	  SUMS_TO_AVGSD8 computes a mean AVG and standard deviation SD from the
c	  sum of values SX8, sum of squares SXSQ8, and number of value N, where
c	  SX8 and SXSQ8 are real*8.
c	  It will not generate any division by 0 errors.
c
	subroutine sums_to_avgsd8(sx8,sxsq8,n,avg,sd)
	real*8 sx8,sxsq8,avg8
	avg=0.
	sd=0.
	if(n.le.0)return
	avg8=sx8/n
	avg = avg8
	if(n.gt.1)sd=sqrt(max(0.,(sxsq8-n*avg8**2)/(n-1.)))
	return
	end
