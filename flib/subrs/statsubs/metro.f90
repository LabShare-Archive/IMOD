! metro -- VARIABLE METRIC MINIMIZATION w / FLETCHER'S SWITCHING METHOD
!
! REV 1.0         10/1/79 -- JLC
!
! $Id$
!
! Notes from November 2013:
! This is based on Fletcher, R. A new approach to variable metric algorithms.
! Computer Journal, 13:317-322, 1970.
! Variables were renamed to match that better; "delX" is the delta there
! Two big problems were: 1) the old f value was not restored along with the vector and
! gradient when cutting the step size by 10, allowing bad tests of improvement and 
! movement away from the minimum.  2) Error versus termination tests were such that 
! when starting at a previous solution, there was a high risk of tripping error 1 or 2.
! Thus the delta length is tested before taking one of these error returns.
! All vector products are now calculated with items to be multiplied converted to double
! precision, so a double precision product is added into the sum.
!
! ! Implements variable metric minimization using Fletcher's switching
! algorithm.
! ^  [n]: Number of variables
! ^  [X]: Solution vector of the n variables, suitably initialized
! and returned with the solution
! ^  [funct]: the function to evaluate the error, called as
! ^           call funct(n, X, f, G)
! ^  [f]: Returned with the error
! ^  [G]: Array for gradients; must be at least [n] in size
! ^  [stepInitial]: Initial step size
! ^  [epsilon]: Epsilon for convergence test
! ^  [limitIn]: Maximum number of cycles; use a negative value to
! ^             suppress output of errors every 10 cycles
! ^  [ier]: Error code, returned with 0 for success, 1 for dg > 0,
! ^         2 for linear search lost, 3 for non - positive definite matrix,
! ^         or 4 for cycle limit reached
! ^  [H]: Scratch array for Hessian matrix, must be at least n**2 + 3n
! ^  [numIter]: The number of cycles to convergence
! ^  [rmsScale]: Scale factor for error printout to be an RMS error
! !
subroutine metro(n, X, funct, f, G, stepInitial, epsilon, limitIn, ier, H, numIter, &
    rmsScale)
  implicit none
  real*4 X(*), G(*), H(*)
  real*4 stepInitial, epsilon, f, rmsScale
  integer*4 n, limitIn, iterLimit, ier, numIter
  logical cutBy10, reinitialize
  external funct
  double precision delXGamma, delXdelX, stepDbl, backoffStep, fOld, fNew, dirVecDotGold
  double precision dirVecDotGnew, z, w, nuj, nuk, deltaX, deltaG, delXlength, hdbl
  double precision HdotGamma, gammaHGamma, HGGH, delXdotG, mu, sig, posdef, p
  integer*4 j, k, ibDirVec, ibHGamma, ibArg, ibGrad
  real*4 step, GdotG
  double precision dotProduct
  mu = 1.d-04
  !
  ! SCRATCH VECTOR STORAGE IN H:
  ! H(1 -- n**2)            - COVARIANCE MATRIX
  ! H(n**2 + 1 -- n**2 + n)     - OLD ARGUMENT VECTOR
  ! H(n**2 + n + 1 -- n**2 + 2n)  - OLD GRADIENT VECTOR
  ! H(n**2 + 2n + 1 -- n**2 + 3n) - DIRECTION VECTOR,
  ! OVERWRITTEN w HG WHEN UPDATING MA
  !
  ! 2/8/07: removed est, it was assigned to step just before factor is

  ! Eval initial arg, generate identity matrix scaled by norm grad

  step = stepInitial
  ibArg = n**2
  ibGrad = n**2 + n
  ibDirVec = n**2 + 2 * n
  ibHGamma = ibDirVec
  ier = 0
  numIter = 0
  iterLimit = abs(limitIn)
  call funct (n, X, f, G)
  reinitialize = .true.
  !
  ! Iteration loop
  do while (numIter <= iterLimit)
    if (reinitialize) then
      ! print *,'Initializing'
      GdotG = dotProduct(G, G, n)
      H(1:n*n) = 0.
      do j = 1, n
        H((j - 1) * n + j) = 1. / sqrt(GdotG / n)
      enddo
    endif
    numIter = numIter + 1
    !
    if (limitIn > 0 .and. mod(numIter, 10) == 0) then
      if (rmsScale == 0.) write(6, 777) numIter, f
777   format(T48,'Cycle',I5,T65,E14.7)
      if (rmsScale .ne. 0.) write(6, 778) numIter, sqrt(f * rmsScale)
778   format(T48,'Cycle',I5,T61,F14.6)
    endif
    !
    ! Save old f, X, G
    H(ibArg + 1 : ibArg + n) = X(1:n)
    H(ibGrad + 1 : ibGrad + n) = G(1:n)
    !
    ! Compute direction vector
    do j = 1, n
      H(ibDirVec + j) = -dotProduct(H((j - 1) * n + 1), G, n)
    enddo
    !
    ! Compute component of new gradient along search vector
    fNew = f
    GdotG = dotProduct(G, G, n)
    dirVecDotGnew = dotProduct(H(ibDirVec + 1), G, n)
    ! write (*,'(a,2f20.10)')'GdotG',GdotG
    !
    ! CONVERGED: normal return (of limited value, you will never get exactly 0)
    if (GdotG == 0.) return
    !
    ! Can't reduce f along this line -- retry steepest
    reinitialize = dirVecDotGnew >= 0.D0
    if (reinitialize) cycle
    !
    ! ------------------------------   EXTRAPOLATE -------------------------------
    fOld = fNew
    dirVecDotGold = dirVecDotGnew
    X(1:n) = X(1:n) + step * H(ibDirVec + 1 : ibDirVec + n)
    call funct (n, X, f, G)
    fNew = f
    dirVecDotGnew = dotProduct(H(ibDirVec + 1), G, n)
    delXdotG = 0.D0
    do j = 1, n
      deltaX = X(j) - H(ibArg + j)
      delXdotG = delXdotG + deltaX * H(ibGrad + j)
    enddo
    ! write (*,'(a,2f20.15)')'delXdotG',delXdotG
    !
    ! Something's flaky: dg >= 0.  But it is OK if vector length is very small
    if (delXdotG >= 0.D0) then
      delXlength = deltaXlength()
      ! write(*,'(a,2f15.12)')'DG > 0, xlength', delXlength,epsilon
      if (delXlength < epsilon / 5.) return
      ier = 1
      return
    endif

    if ((fNew - fOld) / delXdotG >= mu) then
      !
      ! Good improvement w/out linear search, use full shift next time
      step = stepInitial
      ! write(*,'(a,f12.9)')'Restoring initial step', step
    else
      !
      ! not enough improvement
      cutBy10 = dirVecDotGnew < 0. .and. fNew < fOld
      if (.not. cutBy10) then
        !
        ! try cubic interpolation if step was too large and pass initial test
        stepDbl = step
        z = 3.D0 * (fOld - fNew) / stepDbl + dirVecDotGold + dirVecDotGnew
        w = dsqrt(z**2 - dirVecDotGold * dirVecDotGnew)
        backoffStep = stepDbl * (dirVecDotGnew + w - z) /  &
            (dirVecDotGnew - dirVecDotGold + 2.D0 * w)
        if (DABS(backoffStep) > stepDbl)  &
            backoffStep = backoffStep * 0.90D0 * stepDbl /  DABS(backoffStep)
        ! write(*,'(a,f12.9)')'backing off step by',backoffStep
        X(1:n) = X(1:n) - backoffStep * H(ibDirVec + 1 : ibDirVec + n)
        call funct (n, X, f, G)
        cutBy10 = f > fOld .or. f > fNew        !interpolation failed
      endif
      !
      if (cutBy10) then
        !
        ! try stepsize OF 0.1**n if either test fails, restore last position
        step = 0.1 * step
        if (step / stepInitial < 1.E-06) then
          !
          ! DMN 11/27/13: if step is too small, just terminate if delta X is small enough
          delXlength = deltaXlength()
          ! write(*,'(a,2f15.12)')'step too small, xlength', delXlength,epsilon
          if (delXlength < epsilon) return
          ier = 2                           ! Linear search lost
          return
        endif
        !
        ! DNM 11/27/13:
        ! Restore the old f too so that tests and interpolation are correct in next round
        X(1:n) = H(ibArg + 1:ibArg + n)
        G(1:n) = H(ibGrad + 1:ibGrad + n)
        f = fOld
        ! write(*,'(a,f12.9)')'restored previous arg/grad; cut step by 10 to ', step
        cycle                               !try it
      else
        !
        ! Otherwise decrease step size next time.  This would make sense if the backoff
        ! was a factor and not the step itself...
        step = step * dmax1(1. - backoffStep, 1.d-1)
        ! write(*,'(a,f12.9)')'Cut step by 1 - backoff to', step
      endif
    endif
    ! -------------------------- End extrapolation -----------------------------
    !
    ! Check convergence
    delXlength = 0.
    delXGamma = 0.D0
    delXdotG = 0.D0
    do j = 1, n
      deltaX = X(j) - H(ibArg + j)
      deltaG = G(j) - H(ibGrad + j)
      delXdotG = delXdotG + deltaX * H(ibGrad + j)
      delXGamma = delXGamma + deltaX * deltaG
      delXlength = delXlength + deltaX**2
    enddo
    !
    ! Normal return
    if (sqrt(delXlength) <= epsilon) return
    !
    ! ERROR: dg >= 0 even if length is big
    if (delXdotG >= 0.D0) then
      ! write(*,'(a,2f15.9)')'DG > 0, xlength', sqrt(delXlength),epsilon
      ier = 1
      return
    endif
    ! numIter has reached limit
    if (numIter >= iterLimit) then
      ier = 3
      return
    endif
    !
    ! Update covariance matrix according to switching algorithm of Fletcher
    gammaHGamma = 0.D0
    posdef = 0.D0
    do j = 1, n
      sig = 0.D0
      HdotGamma = 0.D0
      do k = 1, n
        Hdbl = H((j - 1) * n + k)
        sig = sig + Hdbl * G(k)
        HdotGamma = HdotGamma + Hdbl * (G(k) - H(ibGrad + k))
      enddo
      H(ibHGamma + j) = HdotGamma
      gammaHGamma = gammaHGamma + (G(j) - H(ibGrad + j)) * HdotGamma
      posdef = posdef + G(j) * sig
    enddo
    !
    ! ERROR: matrix non-positive definite
    if (posdef < 0. .or. gammaHGamma < 0.) then
      ier = 4
      return
    endif
    !
    ! H0 algorithm
    do j = 1, n
      do k = 1, n
        delXdelX = (X(j) - H(ibArg + j)) * (X(k) - H(ibArg + k))
        HGGH = H(ibHGamma + j) * H(ibHGamma + k)
        H((j - 1) * n + k) = H((j - 1) * n + k) + delXdelX / delXGamma - HGGH /gammaHGamma
      enddo
    enddo
    !
    ! H1 algorithm
    if (delXGamma < gammaHGamma) cycle
    do j = 1, n
      nuj = dsqrt(gammaHGamma) * ((X(j) - H(ibArg + j)) / delXGamma - H(ibHGamma + j) / &
          gammaHGamma)
      do k = 1, n
        nuk = dsqrt(gammaHGamma) * ((X(k) - H(ibArg + k)) / delXGamma - &
            H(ibHGamma + k) / gammaHGamma)
        H((j - 1) * n + k) = H((j - 1) * n + k) + nuj * nuk
      enddo
    enddo
  enddo                                     ! Continue with next direction
  ier = 3                    ! numIter has reached limit after a cycle statement
  return

CONTAINS

  ! DNM 11/27/13: Compute the argument vector change in full precision
  double precision function deltaXlength()
    deltaXlength = 0.
    do j = 1, n
      deltaX = X(j) - H(ibArg + j)
      deltaXlength = deltaXlength + deltaX**2
    enddo
    deltaXlength = sqrt(deltaXlength)
  end function deltaXlength

end subroutine metro


! **     DOT PRODUCT
double precision function dotProduct(A, b, n)
  implicit none
  real*4 A(*), b(*)
  integer*4 n, j
  double precision ad, bd
  !
  dotProduct = 0.0
  do j = 1, n
    ad = A(j)
    bd = b(j)
    dotProduct = dotProduct + ad * bd
  enddo
  
  ! DNM 11/27/13: get rid of assignment to float then back
  return
end function dotProduct
