c   simple routine to repack an image from a portion of a 2-d array
c   sequentially into a 1-d array (which can be the same array)
c   this allows one to replace time-consuming calls to iwrpas with a
c   call to this then a quick call to iwrsec
c   brray is the repacked array, everything else follows definition of iwrpas
c
        subroutine irepak(brray,array,mx,my,nx1,nx2,ny1,ny2)
        dimension brray(*),array(mx,my)
        ind=1
        do iy=ny1+1,ny2+1
          do ix=nx1+1,nx2+1
            brray(ind)=array(ix,iy)
            ind=ind+1
          enddo
        enddo
        return
        end
