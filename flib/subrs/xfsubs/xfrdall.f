c routine to read all elements in file into list
        subroutine xfrdall(nunit,flist,nlist,*)
        real*4 flist(2,3,*)
        nlist=0
10      nt=nlist+1
        call xfread(nunit,flist(1,1,nt),*20,*96)
        nlist=nt
        go to 10
20      return
96      return 1
        end
