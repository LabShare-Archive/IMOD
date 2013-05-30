module plotvars
  integer LIM_KEYS, LIM_COLORS
  parameter (LIM_KEYS = 10, LIM_COLORS = 40)
  character*80 keys(LIM_KEYS), xaxisLabel/' '/
  integer*4 igenPltType/0/, ifNoTerm/0/, ifConnect/0/, numKeys/0/, numColors/0/
  integer*4 icolors(6, LIM_COLORS)
  real*4 symConnectGap/1.1/
end module plotvars

subroutine lookupColorIndex(icolumn, igroup, index)
  use plotvars
  implicit none
  integer*4 igroup, index, icolumn
  do index = 1, numColors
    if (igroup == icolors(icolumn, index)) return
  enddo
  index = -1
  return
end subroutine lookupColorIndex

