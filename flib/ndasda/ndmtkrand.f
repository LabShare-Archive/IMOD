c       RANDORDER returns in LIST a list of numbers from 1 to N in random
c       order.
c       
      subroutine randorder(list,n)
      integer*4 list(*)
      integer*2 lstmp(50000)
      character*8 jtime
      call time(jtime)
      iseed=2*((ichar(jtime(8:8))+128*(ichar(jtime(7:7))+
     &    128*(ichar(jtime(5:5))+128*ichar(jtime(4:4)))))/2)+1
c       
c       set up a temporary list of the values
c       
      do i=1,n
        lstmp(i)=i
      enddo
      ngot=0                                    !# values gotten so far
      do while(ngot.lt.n)
        ntmp=n-ngot                             !# left to get
        nlim=min(ngot+(ntmp+1)/2,n)             !get half of what's left
        do while(ngot.lt.nlim)
          x=ran(iseed)
          x=ran(iseed)
          x=ran(iseed)
          x=ran(iseed)
          num=min(ntmp,ifix(n*ran(iseed)+1.))
          if(lstmp(num).ne.0)then               !if number not gotten yet
            ngot=ngot+1                         !put it on list
            list(ngot)=lstmp(num)
            lstmp(num)=0                        !mark as gotten
          endif
        enddo
c         
c         compact temporary list of ones not gotten yet
c         
        inew=0
        do i=1,ntmp
          if(lstmp(i).ne.0)then
            inew=inew+1
            lstmp(inew)=lstmp(i)
          endif
        enddo
      enddo
c       write(*,'(20i4)')(list(i),i=1,n)
      return
      end
      


c       SHUFFLE randomly shuffles the N values in LIST
c       
      subroutine shuffle(list,n)
      integer*4 list(*)
      parameter (limpnts=50000)
      integer*4 ltmp(limpnts)
      call randorder(ltmp,n)
      do i=1,n
        ltmp(i)=list(ltmp(i))
      enddo
      do i=1,n
        list(i)=ltmp(i)
      enddo
      return
      end


c       CHANGE_TYPE changes a specified fraction of points of one type
c       into another type.  NCHANGE specifies the number of types to change,
c       ITYFROM has the types to change, ITYTO has the types to convert them
c       into, and CHNGFRAC has the fraction of ones to convert.
c       
      subroutine change_type(itype,npnts,ityfrom,ityto,chngfrac,
     &    nchange)
c       
      integer*4 itype(*),ityfrom(*),ityto(*)
      real*4 chngfrac(*)
      parameter (limpnts=50000)
      integer*4 ltmp(limpnts),index(limpnts)
c       
c       loop through the changes
c       
      do ichg=1,nchange
        nfrom=0
c         
c         build index to points of the type to be changed
c         
        do i=1,npnts
          if(itype(i).eq.ityfrom(ichg))then
            nfrom=nfrom+1
            index(nfrom)=i
          endif
        enddo
c         
c         get shuffled list of that number of points
c         
        nconvert=nint(chngfrac(ichg)*nfrom)
        call randorder(ltmp,nfrom)
c         
c         convert ones on the list up to the desired fraction
c         
        do i=1,nconvert
          itype(index(ltmp(i)))=ityto(ichg)
        enddo
      enddo
      return
      end


