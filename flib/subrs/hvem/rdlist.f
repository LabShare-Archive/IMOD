c	  RDLIST reads a line from unit IUNit specifying ranges of values and
c	  returns a list of all of the values (NLIST values in array LIST).
c	  E.g. 1-5,7,9,11,15-20.  Numbers separated by dashed are replaced by
c	  all of the numbers in the range.  NUmbers need not be in any order,
c	  and backward ranges (10-5) are handled.  Any characters besides
c	  digits are valid separators. A / at the beginning of the line will
c	  return an unmodified list. Negative numbers can be entered provided
c	  that the minus sign immediately precedes the number.  E.g.: -3 - -1
c	  or -3--1 will give -3,-2,-1; -3, -1,1 or -3,-1,1 will give -3,-1,1.
c
c	  $Author$
c
c	  $Date$
c
c	  $Revision$
c
c	  $Log$
c
        subroutine rdlist(iunit,list,nlist)
c	  
	implicit none
        integer*4 list(*),iunit,nlist
        character*1024 line
        read(iunit,'(a)')line
	call parselist(line,list,nlist)
	return
	end


	subroutine parselist(line,list,nlist)
	implicit none
	character*(*) line
        integer*4 list(*),nlist
        character*10 intern
        character next
        logical dashlast,negnum,gotcomma
	integer*4 nchars,ind,lastnum,numst,loopst,number,idir,i
c
        dashlast=.false.
	negnum=.false.
	gotcomma=.false.
        nchars=len(line)
	if(line(1:1).eq.'/')return
        nlist=0
        ind=1
        lastnum=0
c
c	  find next digit and look for '-', but terminate on non -,space
c
10      next=line(ind:ind)
        if(next.ge.'0'.and.next.le.'9')go to 14
	if(next.ne.','.and.next.ne.' '.and.next.ne.'-')go to 20
	if(next.eq.',')gotcomma=.true.
        if(next.eq.'-')then
	  if(dashlast.or.ind.eq.1.or.gotcomma)then
	    negnum=.true.
	  else
	    dashlast=.true.
	  endif
	endif
        ind=ind+1
        if(ind.le.nchars)go to 10
        go to 20
c
c	  got a digit: save ind, find next non-digit
c
14      numst=ind
16      if(ind.ge.nchars)go to 18
        ind=ind+1
        next=line(ind:ind)
        if(next.ge.'0'.and.next.le.'9')go to 16
c
c	  move number right-justified into intern and read it
c
18      intern='         '
	if(negnum)numst=numst-1
        intern(11+numst-ind:10)=line(numst:ind-1)
        read(intern,'(i10)')number
c
c	  set up loop to add to list
c
        loopst=number
        idir=1
        if(dashlast)then
          if(lastnum.gt.number)idir=-1
          loopst=lastnum+idir
        endif
        do 24 i=loopst,number,idir
          nlist=nlist+1
          list(nlist)=i
24      continue
        lastnum=number
	negnum=.false.
        dashlast=.false.
	gotcomma=.false.
        if(ind.le.nchars)go to 10
20      return
        end
