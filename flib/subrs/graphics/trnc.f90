real*4 function trnc(xx)
  use psparams
  real*4 xx, safe2(3) /.35, 1.2, 2.65/
  integer*4 idot
  idot = nint(xx * unitsPerInch)
  trnc = (3 * (idot / 3) + safe2(mod(idot, 3) + 1)) / unitsPerInch
! trnc=(ifix((xx+0.0001)*unitsPerInch)) /unitsPerInch+safe
  return
end function trnc

