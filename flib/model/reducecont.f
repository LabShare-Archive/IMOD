*       * * * * * * REDUCECONT * * * * * * *
c       
c       This program will reduce the number of points in model contours to
c       the minimum consistent with a defined maximum change in the model,
c       the tolerance value.  Each of the original points in the model will
c       be within the tolerance distance of the line segments connecting the
c       final, reduced set of points.  Some smoothing is also done, and a
c       point will be replaced by a smoothed point if the smoothed point is
c       within the tolerance distance from the original point.
c       
c       See man page for more details.
c       
c       $Author$
c       
c       $Date$
c       
c       $Revision$
c       
c       $Log$
c       Revision 3.2  2005/05/31 00:55:37  mast
c       Fixed scaling direction
c       
c       Revision 3.1  2005/05/24 18:01:07  mast
c       Modified to use partial loading mode and converted to PIP
c       
c       
c       David Mastronarde, 9/8/97
c       
      implicit none
      integer limpath,limflags
      include 'model.inc'
      parameter (limpath=50000,limflags=5000)
C       
      REAL*4 xt(limpath),yt(limpath),xx(limpath),yy(limpath)
      integer*4 idum(limpath),jdum(limpath),iobjdo(limflags),iflags(limflags)
C       
      CHARACTER*120 FILin,filout
      integer*4 nfit, iorder,ierr,nobjdo,nobjTot,iflag,indy,indz,ifflip
      real*4 tol, xyscal,zscal,xofs,yofs,zofs
      integer*4 newtot,npold,ncontTot,imodObj,ifonlist,ichek,i,iobj,ninobj
      integer*4 ipnt,ibase,izcont,nContInObj,nBefore,nAfter
      logical readw_or_imod,coplanar,getModelObjectRange
      integer getimodhead,getimodflags,getImodObjSize
c       
      integer*4 numOptArg, numNonOptArg
      integer*4 PipGetTwoIntegers
      integer*4 PipGetFloat,PipGetString
      integer*4 PipGetInOutFile
c       
c       fallbacks from ../../manpages/autodoc2man -2 2  reducecont
c       
      integer numOptions
      parameter (numOptions = 6)
      character*(40 * numOptions) options(1)
      options(1) =
     &    'input:InputFile:FN:@output:OutputFile:FN:@'//
     &    'objects:ObjectsToReduce:LI:@tolerance:Tolerance:F:@'//
     &    'smoothing:SmoothingPointsAndOrder:IP:@help:usage:B:'
c       
      nfit=5
      iorder=2
      tol=0.25
      nobjdo = 0
c       
c       Pip startup: set error, parse options, check help
c       
      call PipReadOrParseOptions(options, numOptions, 'reducecont',
     &    'ERROR: REDUCECONT - ', .false., 2, 1, 1, numOptArg,
     &    numNonOptArg)
c       
      if (PipGetInOutFile('InputFile', 1, ' ', filin) .ne. 0)
     &    call errorexit('NO INPUT FILE SPECIFIED')
c       
      if (PipGetInOutFile('OutputFile', 2, ' ', filout) .ne. 0)
     &    call errorexit('NO OUTPUT FILE SPECIFIED')
c       
      ierr = PipGetFloat('Tolerance', tol)
      ierr = PipGetTwoIntegers('SmoothingPointsAndOrder', nfit, iorder)
      if (tol .le. 0) call errorexit('ZERO TOLERANCE WILL HAVE NO EFFECT')
      if (nfit .gt. 100 .or. iorder .gt. 5) call errorexit(
     &    'NUMBERS OF POINTS OR ORDER FOR SMOOTHING TOO HIGH')
c       
      call imodPartialMode(1)
      if (.not.readw_or_imod(filin))call errorexit('READING MODEL')
c       
      if (PipGetString('ObjectsToReduce', filin) .eq. 0)
     &    call parselist(filin, iobjdo, nobjdo)
      if (nobjdo .gt. limflags) call errorexit(
     &    'TOO MANY OBJECTS IN LIST FOR ARRAYS')

      call PipDone()
      ierr=getimodhead(xyscal,zscal,xofs,yofs,zofs,ifflip)
c       
c       Get object flags
c       
      do i=1,limflags
        iflags(i)=0
      enddo
      ierr=getimodflags(iflags,limflags)
      if(ierr.ne.0)print *,'Error getting object types, assuming',
     &    ' all are closed contours'
      nobjTot = getImodObjSize()

c       print *,n_point,' points,',max_mod_obj,' contours in input file'
c       
      indy=2
      indz=3
      if (ifflip.ne.0)then
        indy=3
        indz=2
      endif
      newtot=0
      npold=0
c       
c       loop on model objects and decide whether to load
c       
      do imodObj = 1, nobjTot
c         
c         is IMOD object on list
c         
        ifonlist=0
        if(nobjdo.eq.0)then
          ifonlist=1
        else
          do ichek=1,nobjdo
            if(imodobj.eq.iobjdo(ichek))ifonlist=1
          enddo
        endif
c         
c         require not scattered points
c         
        iflag = 0
        if (imodobj .le. limflags) iflag = mod(iflags(imodobj), 4)
        if (ifonlist .eq. 1 .and. iflag .lt. 2) then
          nContInObj = 0
          nBefore = 0
          nAfter = 0
c           
          if (.not.getModelObjectRange(imodObj, imodObj)) then
            print *
            print *, 'ERROR: REDUCECONT - LOADING DATA FOR OBJECT #',imodobj
            call exit(1)
          endif
          call scale_model(0)
c           
c           Loop on contours, which are in this object
c           
          do iobj=1,max_mod_obj
            ninobj=npt_in_obj(iobj)
            ibase=ibase_obj(iobj)
            coplanar=.true.
            i=2
            izcont=nint(p_coord(indz,ibase+1))
            do while(i.le.ninobj.and.coplanar)
              coplanar=nint(p_coord(indz,object(i+ibase))).eq.izcont
              i=i+1
            enddo
            
            if (ninobj.gt.2 .and. ninobj.le.limpath .and. coplanar) then
              nBefore = nBefore + ninobj
              nContInObj = nContInObj + 1
              do i=1,ninobj
                ipnt=object(i+ibase)
                xx(i)=p_coord(1,ipnt)
                yy(i)=p_coord(indy,ipnt)
              enddo
              call reducepts(xx,yy,ninobj,iorder,nfit,tol,xt,yt,idum,jdum)
              do i=1,ninobj
                ipnt=object(i+ibase)
                p_coord(1,ipnt)=xx(i)
                p_coord(indy,ipnt)=yy(i)
              enddo
c               print *,imodobj,iobj,' reduced from',npt_in_obj(iobj),' to',
c               &                   ninobj
              npt_in_obj(iobj)=ninobj
              nAfter = nAfter + ninobj
            endif
          enddo
          npold=npold+nBefore
          newtot=newtot+nAfter
          ncontTot = ncontTot + nContInObj
c           
          call scale_model(1)
          call putModelObjects()
          if (nContInobj .gt. 0)
     &        write(*,101)imodObj, nContInObj,nBefore,nAfter
101       format('Object',i4,':',i7,' contours reduced from',i8,' to',i8,
     &        ' points')
        endif
      enddo
c       
      n_point = -1
      call write_wmod(filout)
      write(*,102)nContTot,npold,newtot
102   format('     Total:',i7,' contours reduced from',i8,' to',i8,
     &    ' points')
      call exit(0)
      end


c       REDUCEPTS reduces a set of points by smoothing and selecting a
c       minimal subset of the points.  Each of the original points will be
c       within a tolerance TOL of the segments defined by the new set of
c       points.
c       XX, YY are coordinates of the points
c       NPTS is the number of points
c       INORDER is the order of a polynomial to be fit to successive sets of
c       points for smoothing; INFIT is the number of points to fit.  No
c       smoothing is used if either value is zero.
c       TOL is the tolerance
c       XS, YS are scratch real*4 arrays, NEXTPT and MINSEG are scratch
c       integer*4 arrays which should be at least as large as the original
c       number of points
c       The reduced set of points and number of points are returned in
c       XX, YY and NPTS.
c       
      subroutine reducepts(xx,yy,npts,inorder,infit,tol,xs,ys,nextpt,
     &    minseg)
      parameter (limfit=30,limord=10)
      real*4 xx(*),yy(*),xs(*),ys(*)
      integer*4 nextpt(*),minseg(*)
      real*4 xt(limfit),yt(limfit),slop(limord)
      errlim=1.e-6
c       
      nfit=min(infit,limfit)
      iorder=min(inorder,limord)
      do icen=1,npts
c         
c         copy data into xs,ys arrays
c         
        xs(icen)=xx(icen)
        ys(icen)=yy(icen)
        if(iorder.gt.0.and.nfit.gt.iorder.and.nfit.le.npts.and.
     &      icen.gt.1.and.icen.lt.npts)then
c           
c           try to smooth data if iorder/nfit are set
c           
          ist=max(1,icen-nfit/2)
          ind=min(npts,ist+nfit-1)
          ist=ind+1-nfit
          dx=xx(ind)-xx(ist)
          dy=yy(ind)-yy(ist)
          dlen=sqrt(dx**2+dy**2)
          xmid=0.5*(xx(ind)+xx(ist))
          ymid=0.5*(yy(ind)+yy(ist))
          if(dlen.gt.1.)then
c             
c             if the segment spanned by nfit points is long enough, rotate
c             it about its midpoint so that it is horizontal
c             
            sinth=-dy/dlen
            costh=dx/dlen
            do in=ist,ind
              io=in+1-ist
              xt(io)=costh*(xx(in)-xmid)-sinth*(yy(in)-ymid)
              yt(io)=sinth*(xx(in)-xmid)+costh*(yy(in)-ymid)
              if(in.eq.icen)jcen=io
            enddo
c             
c             fit to rotated points
c             
            call localpolyfit(xt,yt,nfit,iorder,slop,bint)
            ycen=bint
            do i=1,iorder
              ycen=ycen+slop(i)*xt(jcen)**i
            enddo
            if(abs(ycen-yt(jcen)).lt.tol)then
c               
c               if the fitted point is within tolerance of the actual value
c               replace the actual value in the xs,ys arrays with the
c               back-rotated fitted position
c               
              xs(icen)=costh*xt(jcen)+sinth*ycen+xmid
              ys(icen)=-sinth*xt(jcen)+costh*ycen+ymid
            endif
          endif
        endif
      enddo
c       
c       moving from right to left, look at possible segments from a given
c       point going to right.  A segment is possible if all intervening
c       points are within TOL of the segment.  From the given point, find
c       the possible segment that involves the fewest segments to get to
c       the right end.  Keep track of the endpoint of the best segment in
c       NEXTPT and the total number of segments in MINSEG
c       
      minseg(npts)=0
      minseg(npts-1)=1
      nextpt(npts)=npts+1
      nextpt(npts-1)=npts
      tolsq=tol**2
c       
c       Stop looking at longer segments after finding NOUTLIM segments that
c       are not possible
c       
      noutlim=3
      do left=npts-2,1,-1
c         
c         set left edge of segment
c         
        minseg(left)=minseg(left+1)+1
        nextpt(left)=left+1
        irt=left+2
        x1=xs(left)
        y1=ys(left)
        nout=0
        do while(irt.le.npts.and.nout.lt.noutlim)
          if(1+minseg(irt).lt.minseg(left))then
c             
c             look at a segment only if it will give a better path: set
c             right edge
c             
            ifout=0
            x2=xs(irt)
            y2=ys(irt)
            dx=x2-x1
            dy=y2-y1
            denom=dx**2+dy**2
            its=left+1
            do while(its.lt.irt.and.ifout.eq.0)
c               
c               check distance of points from segment until one falls out
c               
              dx0=xx(its)-x1
              dy0=yy(its)-y1
              if(denom.lt.1.e-5)then
                distsq=dx0**2+dy0**2
              else
                tmin=(dx0*dx+dy0*dy)/denom
                distsq=(tmin*dx-dx0)**2+(tmin*dy-dy0)**2
              endif
              if(distsq.ge.tolsq)ifout=1
              its=its+1
            enddo
c             
c             if its an OK segment, set it up as new minimum
c             
            if(ifout.eq.0)then
              minseg(left)=1+minseg(irt)
              nextpt(left)=irt
            else
              nout=nout+1
            endif
          endif
          irt=irt+1
        enddo
      enddo
c       
c       when we get to the left edge, the minimal path is available by 
c       following the chain of NEXTPT values
c       
      npo=1
      ipo=1
      do while(nextpt(ipo).le.npts)
        ipo=nextpt(ipo)
        npo=npo+1
        xx(npo)=xs(ipo)
        yy(npo)=ys(ipo)
      enddo
c       print *,npts,npo
      npts=npo
      return
      end


      subroutine errorexit(message)
      character*(*) message
      print *
      print *,'ERROR: REDUCECONT - ',message
      call exit(1)
      end
