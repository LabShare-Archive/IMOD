c       SHUFFLER manages the input sections in the big array ARRAY.
c       IZWANT specifies the piece # that is wanted (numbered from 1, not 0).
c       The routine looks to see if it is already in memory; if not, it loads
c       the section, replacing the frame that has not been used in the
c       longest time, and returns the index of the frame's start in INDEX
c       
c       $Id$
c       
      subroutine shuffler(izwant,index)
c       
      use blendvars
      implicit none
c       
c       real*4 array(*)
      integer*4 minused,i,index,izwant,ioldest, indRead,j, magUse, iangUse
      real*4 xcen, ycen, amat(2,2)
c       
c       if the section's entry in memIndex exists, return index
c       
      i = memIndex(izwant)
      if (i .gt. 0) then
        index=(i-1)*npixin+1
        jusecount=jusecount+1
        lastused(i)=jusecount
        return
      endif
c       
c       Otherwise look for oldest used or unused slot
c       
      minused=jusecount+1
      do i=1,maxload
        if(minused.gt.lastused(i))then
          minused=lastused(i)
          ioldest=i
        endif
      enddo
c       
c       Assign to that slot, clear out previous allocation there
c       
      index=(ioldest-1)*npixin+1
      jusecount=jusecount+1
      if (izmemlist(ioldest) .gt. 0) memIndex(izmemlist(ioldest)) = -1
      lastused(ioldest)=jusecount
      izmemlist(ioldest)=izwant
      memIndex(izwant) = ioldest
c       print *,'reading section',izwant-1,'  index',index
      call imposn(1,izwant-1,0)                 !yes izwant starts at 1
c       
      indRead = index
      if (doFields .and. doingEdgeFunc) indRead = maxload * npixin + 1
      call irdsec(1,array(indRead),*99)
c       
      if (doFields) then
        if (doMagGrad) then
          magUse = min(ilistz, numMagGrad)
          iangUse = min(ilistz, numAngles)
c           
c           Get center of tilt as center of frame or montage
c           
          if (focusAdjusted) then
            xcen = nxin / 2.
            ycen = nyin / 2.
          else
            xcen = (minxpiece + nxpieces * (nxin - nxoverlap) + nxoverlap) /
     &          2. - ixpclist(izwant)
            ycen = (minypiece + nypieces * (nyin - nyoverlap) + nyoverlap) /
     &          2. - iypclist(izwant)
          endif
c           
c           add mag gradient to distortion or make field up
c           
          if (undistort) then
            call addMagGradField(distDx, distDy, fieldDx(1,1,ioldest),
     &          fieldDy(1,1, ioldest), lmField, nxin, nyin, nxField, nyField, xFieldStrt,
     &          yFieldStrt, xFieldIntrv, yFieldIntrv, xcen, ycen, pixelMagGrad, axisRot,
     &          tiltAngles(iangUse), dmagPerUm(magUse), rotPerUm(magUse))
          else
            call makeMagGradField(distDx, distDy, fieldDx(1,1,ioldest),
     &          fieldDy(1,1, ioldest), lmField, nxin, nyin, nxField, nyField, xFieldStrt,
     &          yFieldStrt, xFieldIntrv, yFieldIntrv, xcen, ycen, pixelMagGrad, axisRot,
     &          tiltAngles(iangUse), dmagPerUm(magUse), rotPerUm(magUse))
          endif
        else
c           
c           Just copy field if distortion only
c           
          do i = 1, nxField
            do j = 1, nyField
              fieldDx(i,j,ioldest) = distDx(i,j)
              fieldDy(i,j,ioldest) = distDy(i,j)
            enddo
          enddo
        endif
c         
c         Undistort the piece for computing edge functions
c         
        if (doingEdgeFunc) then
          amat(1,1) = 1.
          amat(2,2) = 1.
          amat(1,2) = 0.
          amat(2,1) = 0.
          call warpInterp(array(indRead),array(index),nxin, nyin, nxin, nyin, amat,
     &        nxin/2. ,nyin/2., 0.,0.,1.,dmean, 1, 0, fieldDx(1,1,ioldest),
     &        fieldDy(1,1,ioldest), lmField, nxField, nyField, xFieldStrt, yFieldStrt,
     &        xFieldIntrv, yFieldIntrv)
        endif
      endif
      return
99    call exitError('READING FILE')
      end


c       CLEARSHUFFLE initializes or clears the memory allocation
c       
      subroutine clearShuffle()
      use blendvars
      implicit none
      integer*4 i

      jusecount=0
      do i = 1, memlim
        izmemlist(i) = -1
        lastused(i) = 0
      enddo
      do i = 1,limnpc
        memIndex(i) = -1
      enddo
      return
      end
