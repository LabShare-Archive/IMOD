module plotvars
  integer LIM_KEYS, LIM_COLORS
  parameter (LIM_KEYS = 8, LIM_COLORS = 40)
  character*80 keys(LIM_KEYS), xaxisLabel/' '/
  integer*4 igenPltType/0/, ifNoTerm/0/, ifConnect/0/, numKeys/0/, numColors/0/
  integer*4 icolors(4, LIM_COLORS)
end module plotvars
