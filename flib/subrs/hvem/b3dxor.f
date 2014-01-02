c       B3DXOR returns the exclusive or of its two arguments
c       
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       
      logical function b3dxor(a, b)
      logical a, b
      b3dxor = (a .and. .not.b) .or. (b .and. .not.a)
      return
      end
