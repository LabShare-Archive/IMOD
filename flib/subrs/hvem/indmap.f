c       INDMAP returns IX if IX is between 1 and NX, but when IX is wrapped
c       below 1 or past NX, it adjusts it by NX to be between 1 and NX

      function indmap(ix,nx)
      indmap=ix
      if(ix.lt.1)indmap=ix+nx
      if(ix.gt.nx)indmap=ix-nx
      return
      end
