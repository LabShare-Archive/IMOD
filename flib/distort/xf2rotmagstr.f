c       XF2ROTMAGSTR
c       A simple program to convert transforms to rotation-mag-stretch
c
      implicit none
      integer maxSect
      character*120 strFile
      parameter (maxSect = 10000)
      real*4 xform(2,3,maxSect)
      integer*4 nxfRead, i
      real*4 theta, smag, str, phi, smagMean
c       
      call setExitPrefix('ERROR: XF2ROTMAGSTR -')
      call getinout(1, strFile, strFile)
      call dopen(1, strFile, 'ro', 'f')
      call xfrdall(1, xform, nxfRead, *98)
      if (nxfRead .gt. maxSect)
     &    call exitError('TOO MANY TRANSFORMS IN FILE FOR ARRAY')
      do i =1 , nxfRead
        call amat_to_rotmagstr(xform(1,1,i), theta, smag, str, phi)
        smagMean = smag * sqrt(abs(str))
        write(*,110)i, theta, smag, str, phi, smagMean
110     format(i5, ': rot=',f8.2,', mag=',f7.4,
     &      ', str=',f7.4,' on',f7.1,' axis',
     &      ', Mean mag=',f7.4)
      enddo
      call exit(0)
98    call exitError('READING TRANSFORMS')
      end
