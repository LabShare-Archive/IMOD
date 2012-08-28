! metro -- VARIABLE METRIC MINIMIZATION w / FLETCHER'S SWITCHING METHOD
!
! REV 1.0         10/1/79 -- JLC
!
! $Id$
!
! ! Implements variable metric minimization using Fletcher'S switching
! algorithm.
! ^  [n]: Number of variables
! ^  [X]: Solution vector of the n variables, suitably initialized
! and returned with the solution
! ^  [funct]: the function to evaluate the error, called as
! ^           call funct(n, X, f, G)
! ^  [f]: Returned with the error
! ^  [G]: Array for gradients; must be at least [n] in size
! ^  [factor]: Initial step size
! ^  [eps]: Epsilon for convergence test
! ^  [limitIn]: Maximum number of cycles; use a negative value to
! ^             suppress output of errors every 10 cycles
! ^  [ier]: Error code, returned with 0 for success, 1 for dg > 0,
! ^         2 for linear search lost, 3 for non - positive definite matrix,
! ^         or 4 for cycle limit reached
! ^  [H]: Scratch array for Hessian matrix, must be at least n**2 + 3n
! ^  [kount]: The number of cycles to convergence
! ^  [rmsScale]: Scale factor for error printout to be an RMS error
! !
subroutine metro(n, X, funct, f, G, factor, eps, limitIn, ier, H, kount, rmsScale)
  implicit none
  real*4 X(*), G(*), H(*)
  real*4 factor, eps, f, rmsScale
  integer*4 n, limitIn, limit, ier, kount
  logical cutBy10, reinitialize
  external funct
  double precision dgam, dd, S, ak, ya, yb, ga, gb, z, w, nuj, nuk
  double precision HG, GHG, HGGH, dg, dot, mu
  integer*4 j, k, ibDirVec, ibHG, ibArg, ibGrad
  real*4 step, GG, p, delta, sig, posdef
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

  step = factor
  ibArg = n**2
  ibGrad = n**2 + n
  ibDirVec = n**2 + 2 * n
  ibHG = ibDirVec
  ier = 0
  kount = 0
  limit = abs(limitIn)
  call funct (n, X, f, G)
  reinitialize = .true.
  !
  ! Iteration loop
  do while (kount <= limit)
    if (reinitialize) then
      GG = dot(G, G, n)
      do j = 1, n
        do  k = 1, n
          H((j - 1) * n + k) = 0.
        enddo
        H((j - 1) * n + j) = 1. / sqrt(GG / n)
      enddo
    endif
    !
    ! Save old f, X, G
    kount = kount + 1
    !
    if (limitIn > 0 .and. mod(kount, 10) == 0) then
      if (rmsScale == 0.) write(6, 777) kount, f
777   format(T48,'Cycle',I5,T65,E14.7)
      if (rmsScale .ne. 0.) write(6, 778) kount, sqrt(f * rmsScale)
778   format(T48,'Cycle',I5,T61,F14.6)
    endif
    !
    do j = 1, n
      H(ibArg + j) = X(j)
      H(ibGrad + j) = G(j)
      !
      ! Compute direction vector
      p = 0.
      do k = 1, n
        p = p - H((j - 1) * n + k) * G(k)
      enddo
      H(ibDirVec + j) = p
    enddo
    !
    ! Compute component of new gradient along search vector
    yb = f
    GG = dot(G, G, n)
    gb = dot(H(ibDirVec + 1), G, n)
    !
    ! CONVERGED: normal return
    if (GG == 0.) return
    !
    ! Can't reduce f along this line -- retry steepest
    reinitialize = gb >= 0.D0
    if (reinitialize) cycle
    !
    ! ------------------------------   EXTRAPOLATE -------------------------------
    ya = yb
    ga = gb
    do  j = 1, n
      X(j) = X(j) + step * H(ibDirVec + j)
    enddo
    call funct (n, X, f, G)
    yb = f
    gb = dot(H(ibDirVec + 1), G, n)
    dg = 0.D0
    do j = 1, n
      dg = dg + (X(j) - H(ibArg + j)) * H(ibGrad + j)
    enddo
    !
    ! Something'S flaky: dg >= 0
    if (dg >= 0.D0) then
      ier = 1
      return
    endif

    if ((yb - ya) / dg >= mu) then
      !
      ! Good improvement w/out linear search, use full shift next time
      step = factor
    else
      !
      ! not enough improvement
      cutBy10 = gb < 0. .AND. yb < ya
      if (.not. cutBy10) then
        !
        ! try cubic interpolation if step was too large and pass initial test
        S = step
        z = 3.D0 * (ya - yb) / S + ga + gb
        w = dsqrt(z**2 - ga * gb)
        ak = S * (gb + w - z) / (gb - ga + 2.D0 * w)
        if (DABS(ak) > S) ak = ak * 0.90D0 * S / DABS(ak)
        do j = 1, n
          X(j) = X(j) - ak * H(ibDirVec + j)
        enddo
        call funct (n, X, f, G)
        cutBy10 = f > ya .OR. f > yb        !interpolation failed
      endif
      !
      if (cutBy10) then
        !
        ! try stepsize OF 0.1**n if either test fails
        X(1:n) = H(ibArg + 1:ibArg + n)
        G(1:n) = H(ibGrad + 1:ibGrad + n)
        step = 0.1 * step
        if (step / factor < 1.E-06) then
          ier = 2                           ! Linear search lost
          return
        endif
        cycle                               !try it
      else
        !
        ! Otherwise decrease step size next time
        step = step * dmax1(1. - ak, 1.d-1)
      endif
    endif
    ! -------------------------- End extrapolation -----------------------------
    !
    ! Check convergence
    delta = 0.
    dgam = 0.D0
    dg = 0.D0
    do j = 1, n
      dg = dg + (X(j) - H(ibArg + j)) * H(ibGrad + j)
      dgam = dgam + (X(j) - H(ibArg + j)) * (G(j) - H(ibGrad + j))
      delta = delta + (X(j) - H(ibArg + j))**2
    enddo
    !
    ! Normal return
    if (sqrt(delta) <= eps) return
    !
    ! ERROR: dg >= 0
    if (dg >= 0.D0) then
      ier = 1
      return
    endif
    ! kount has reached limit
    if (kount >= limit) then
      ier = 3
      return
    endif
    !
    ! Update covariance matrix according to switching algorithm of Fletcher
    GHG = 0.D0
    posdef = 0.
    do j = 1, n
      sig = 0.
      HG = 0.D0
      do k = 1, n
        sig = sig + H((j - 1) * n + k) * G(k)
        HG = HG + H((j - 1) * n + k) * (G(k) - H(ibGrad + k))
      enddo
      H(ibHG + j) = HG
      GHG = GHG + (G(j) - H(ibGrad + j)) * HG
      posdef = posdef + G(j) * sig
    enddo
    !
    ! ERROR: matrix non-positive definite
    if (posdef < 0. .or. GHG < 0.) then
      ier = 4
      return
    endif
    !
    ! H0 algorithm
    do j = 1, n
      do k = 1, n
        dd = (X(j) - H(ibArg + j)) * (X(k) - H(ibArg + k))
        HGGH = H(ibHG + j) * H(ibHG + k)
        H((j - 1) * n + k) = H((j - 1) * n + k) + dd / dgam - HGGH / GHG
      enddo
    enddo
    !
    ! H1 algorithm
    if (dgam < GHG) cycle
    do j = 1, n
      nuj = dsqrt(GHG) * ((X(j) - H(ibArg + j)) / dgam - H(ibHG + j) / GHG)
      do k = 1, n
        nuk = dsqrt(GHG) * ((X(k) - H(ibArg + k)) / dgam - &
            H(ibHG + k) / GHG)
        H((j - 1) * n + k) = H((j - 1) * n + k) + nuj * nuk
      enddo
    enddo
  enddo                                     ! Continue with next direction
  ier = 3                    ! kount has reached limit (but this was caught above)
  return
end subroutine metro

! **     DOT PRODUCT
double precision function dot(A, b, n)
  implicit none
  real*4 A(*), b(*), c
  integer*4 n, j
  double precision ad, bd
  !
  dot = 0.0
  do j = 1, n
    ad = A(j)
    bd = b(j)
    dot = dot + ad * bd
  enddo
  ! This seems to survive optimization and reduces a final RMS error of ~0.3 by ~0.000002
  c = dot
  dot = c
  return
end function dot
