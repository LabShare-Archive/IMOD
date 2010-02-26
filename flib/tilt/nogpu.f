c       NOGPU.F
c       Has stubs to allow compilation when no CUDA  is available
c
      integer*4 function gpuAvailable(nGPU, fmemory, ifdebug)
      gpuAvailable = 0
      return
      end

      integer*4 function gpuallocarrays(iwidth, nyout, nxprj2, nyprj, nplanes,
     &    nviews, numWarps, numDelz, nfilt, nreproj, npl1, npl2)
      gpuallocarrays =1
      return
      end

      integer*4 function gpubpnox(slice, flines, sbeta, cbeta, nxprj,
     &    xcenin, xcen, ycen, edgefill)
      gpubpnox = 1
      return
      end

      integer*4 function gpushiftproj(numPlanes, lsliceStart, loadStart)
      gpushiftproj = 1
      return
      end

      integer*4 function gpuloadproj(lines, numPlanes, lsliceStart, loadStart)
      gpuloadproj = 1
      return
      end

      integer*4 function gpubpxtilt(slice, sbeta, cbeta, salpha, calpha,
     &    xzfac, yzfac, nxprj, nyprj, xcenin, xcen, ycen,
     &    lslice, slicen, edgefill) 
      gpubpxtilt = 1
      return
      end

      integer*4 function gpubplocal(slice, lslice, nxwarp, nywarp, ixswarp,
     &    iyswarp, idxwarp, idywarp, nxprj, xcen, xcenin, delxx, ycen, slicen,
     &    edgefill)
      gpubplocal = 1
      return
      end

      integer*4 function gpuloadfilter(flines)
      gpuloadfilter = 1
      return
      end

      integer*4 function gpuloadlocals(packed, numWarps)
      gpuloadlocals = 1
      return
      end

      integer*4 function gpufilterlines(flines, lslice, iset)
      gpufilterlines = 1
      return
      end

      integer*4 function gpureproject(flines, sbeta, cbeta, salpha,  calpha,
     &    xzfac, yzfac, delz, lsStart, lsEnd, ithick, xcen, xcenPdelxx,
     &    minXreproj,  xprjOffset, ycen, minYreproj, yprjOffset, slicen,
     &    ifalpha,  pmean)
      gpureproject = 1
      return
      end

      integer*4 function gpureprojoneslice(slice, flines, sbeta, cbeta,
     &    ycen, numproj, pmean)
      gpureprojoneslice =1 
      return
      end

      integer*4 function gpureprojlocal(flines, sbeta, cbeta, salpha, calpha,
     &    xzfac, yzfac, nxwarp, nywarp, ixswarp,  iyswarp, idxwarp, idywarp,
     &    warpDelz, nWarpDelz,  dxWarpDelz,xprojMin,xprojMax, lsStart, lsEnd,
     &    ithick, iview, xcen, xcenin, delxx,  minXload, xprjOffset, ycenAdj,
     &    yprjOffset, slicen, pmean);
      gpureprojlocal = 1
      return
      end

      subroutine gpuDone()
      return
      end
