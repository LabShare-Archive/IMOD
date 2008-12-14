c***    PROC_MODEL processes the model data, 
c       sorts out the model points that are to be included in
c       the analysis, and converts the coordinates to "index" coordinates
c       with the origin at the center of the section
c       
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       Revision 3.2  2006/06/29 04:53:31  mast
c       Set up to use small model
c	
c       Revision 3.1  2005/04/07 03:56:31  mast
c       New version with local tracking, new mapping, outliers, etc.
c	
c       
      subroutine proc_model(xcen,ycen,xdelt,ydelt,xorig,yorig,scalexy,
     &    nviewtot,mininview,izcur,nzlocal,listobj,ninlist,ninview,
     &    ivorig, xx,yy,isecview,maxprojpt,maxreal,irealstr,iobjali,
     &    nview, nprojpt,nrealpt)
c       
      implicit none
      include 'smallmodel.inc'
      real*4 xx(*),yy(*)
      integer*4 isecview(*),irealstr(*),listobj(*)
      integer*4 ninview(*),ivorig(*),iobjali(*)
      real*4 xcen, ycen, xdelt, ydelt,xorig,yorig,scalexy
      integer*4 nviewtot,mininview,izcur,nzlocal,ninlist,nview, nprojpt
      integer*4 nrealpt,maxprojpt,maxreal
      integer*4 izst,iznd,izstmin,izndmax,i,l,iobject,ibase,iz,ipt
      integer*4 ntry,itry,izsttry,izndtry
      integer*4 needdo,ninobj,nlegal,maxview,minview,minViewsPresent
      real*4 adjNpnts, adjMaxpt
      minViewsPresent = 3
c       
c       find z limits based on nzlocal
c       
      if(nzlocal.eq.0.or.nzlocal.ge.nviewtot)then
        izst=1
        iznd=nviewtot
      else
c         
c	  count number of points on each view
c         
        izstmin=max(1,izcur-nzlocal)
        izndmax=min(nviewtot,izcur+nzlocal)
        do i=izstmin,izndmax
          ninview(i)=0
        enddo
        do l=1,ninlist
          iobject=listobj(l)
          ibase=ibase_obj(iobject)
          do ipt=1,npt_in_obj(iobject)
            iz=nint(p_coord(3,object(ipt+ibase)))+1
            if(iz.ge.izstmin.and.iz.le.izndmax)ninview(iz)=ninview(iz)+1
          enddo
        enddo
c         
c         Force the current view to be included if it has points
        if (ninview(izcur) .gt. 0) izstmin = max(1,izcur + 1 -nzlocal)
c         
c         find placement of the window of views that maximizes an adjusted
c         total number of points, where views within the range that would
c         be equally spaced around the current one are given a 10% premium
c         
        ntry=izndmax+2-izstmin-nzlocal
c	  print *,izcur,nzlocal,izstmin,izndmax,ntry
        adjMaxpt=-1
        do itry=1,ntry
          adjNpnts=0
          izsttry=izstmin+itry-1
          izndtry=min(nviewtot, izsttry+nzlocal-1)
          do i=izsttry,izndtry
            adjNpnts = adjNpnts + ninview(i)
            if (abs(i - izcur) .lt. nzlocal / 2) 
     &          adjNpnts = adjNpnts + 0.1 * ninview(i)
          enddo
c	    print *,izsttry,izndtry,adjNpnts
          if(adjNpnts .gt. adjMaxpt)then
            adjMaxpt = adjNpnts
            izst=izsttry
            iznd=izndtry
          endif
        enddo
      endif
c       
c       scan to get number of points on each view; eliminate consideration
c       of views with not enough points, and objects with less than 2 points
c       on the legal views; reiterate until all counted points come up above
c       the minimum
c       
      do i=1,nviewtot
        ivorig(i)=0
        ninview(i)=0
      enddo
      do i=izst,iznd
        ivorig(i)=2*mininview
      enddo
      needdo=1
c       
      do while(needdo.eq.1)
        do i=izst,iznd
          ninview(i)=0
        enddo
        do l=1,ninlist
          iobject=listobj(l)
          ninobj=npt_in_obj(iobject)
          if(ninobj.ge.minViewsPresent)then
            ibase=ibase_obj(iobject)
            nlegal=0
            do ipt=1,ninobj
              iz=nint(p_coord(3,object(ipt+ibase)))+1
              if(ivorig(iz).ge.mininview)nlegal=nlegal+1
            enddo
            if(nlegal.ge.minViewsPresent)then
              do ipt=1,ninobj
                iz=nint(p_coord(3,object(ipt+ibase)))+1
                if(ivorig(iz).ge.mininview)ninview(iz)=ninview(iz)+1
              enddo
            endif
          endif
        enddo
        needdo=0
        do i=izst,iznd
          if(ninview(i).lt.mininview.and.ninview(i).gt.0)needdo=1
          ivorig(i)=ninview(i)
        enddo
      enddo
c       
c       find number of views with enough points, build cross-indexes
c       
      nview=0
      maxview=0
      minview=nviewtot
      do i=izst,iznd
        if(ninview(i).ne.0)then
          nview=nview+1
          ninview(i)=nview
          ivorig(nview)=i
          minview=min(minview,i)
          maxview=max(maxview,i)
        endif
      enddo
c	print *,'views',minview,' to',maxview
c       
c       go through model finding objects with more than one point in the
c       proper z range, and convert to index coordinates, origin at center
c       
      nprojpt=0
      nrealpt=0
      do l=1,ninlist
        iobject=listobj(l)
        ninobj=npt_in_obj(iobject)
        ibase=ibase_obj(iobject)
        if (ninobj .ge. minViewsPresent) then
          nlegal=0
          do ipt=1,ninobj
            iz=nint(p_coord(3,object(ipt+ibase)))+1
            if(ninview(iz).gt.0)nlegal=nlegal+1
          enddo
          if(nlegal.ge. minViewsPresent)then
            nrealpt=nrealpt+1
            if(nrealpt.gt.maxreal)call errorexit(
     &          'TOO MANY FIDUCIALS FOR ARRAYS' , 0)
            irealstr(nrealpt)=nprojpt+1
            iobjali(nrealpt)=iobject
            do ipt=1,ninobj			!loop on points
c               
c		find out if the z coordinate of this point is on the list
c               
              iz=nint(p_coord(3,object(ipt+ibase)))+1
              if(ninview(iz).gt.0)then
c                 
c		  if so, add index coordinates to list
c                 
                nprojpt=nprojpt+1
                if(nprojpt.gt.maxprojpt)call errorexit(
     &              'TOO MANY PROJECTION POINTS FOR ARRAYS', 0)
                xx(nprojpt)=((p_coord(1,object(ipt+ibase))+xorig)/
     &              xdelt -xcen)/scalexy
                yy(nprojpt)=((p_coord(2,object(ipt+ibase))+yorig)/
     &              ydelt -ycen)/scalexy
                isecview(nprojpt)=ninview(iz)
              endif
            enddo
          endif
        endif
      enddo
      irealstr(nrealpt+1)=nprojpt+1		!for convenient looping
      return
      end
