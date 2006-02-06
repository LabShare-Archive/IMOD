      integer limsec, limopt
      parameter (limsec = 1000, limopt = 20)
      real*4 pixelMagGrad, axisRot, tiltAngle,errMin,sectErrs(limsec)
      real*4 dmagStart, drotStart, tiltOffStart
      real*4 sectDmag(limsec),sectDrot(limsec)
      integer*4 inputBinning,ifTrace,nTrial,nError,izsect(limsec),nvar
      integer*4 numBlendOpt
      logical focusAdjusted
      character*80 rootname,imageName,plName,distName,gradName,tmpRoot
      character*240 sectionList
      character*120 angleFile
      character*40 blendOption(limopt)
      common /funccom/pixelMagGrad, axisRot, tiltAngle,inputBinning,
     &    focusAdjusted,errMin,ifTrace,nTrial,nError,sectErrs,izsect,nvar,
     &    rootname, tmpRoot,imageName,plName, distName,gradName, sectionList,
     &    angleFile,dmagStart, drotStart, tiltOffStart,sectDmag,sectDrot,
     &    blendOption,numBlendOpt
