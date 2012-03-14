c       function NICEFRAME returns NUM if it is even and has no prime factor 
c       greater than LIMIT, or adds IDNUM to NUM until it reaches a number
c       with this tractable property
c       
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
      function niceframe(num,idnum,limit)
      numin=num+mod(num,2)
10    numtmp=numin
      do ifac=2,limit
        do while (mod(numtmp,ifac).eq.0)
          numtmp=numtmp/ifac
        enddo
      enddo
      if(numtmp.gt.1)then
        numin=numin+idnum
        go to 10
      endif
      niceframe=numin
      return
      end
