


c	  function NICEFRAME returns NUM if it has no prime factor greater
c	  than LIMIT, or adds IDNUM to NUM until it reaches a number with this
c	  tractable property
c
	function niceframe(num,idnum,limit)
	numin=num
10	numtmp=numin
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
