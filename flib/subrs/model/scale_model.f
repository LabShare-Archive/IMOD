c       
c       $Id$
c       
c       $Log$
c       Revision 3.1  2005/01/07 15:54:49  mast
c       Addition to library
c	!
c       Will undo or redo the scaling from image index coordinates to model
c       coordinates performed by imodel_fwrap, using the data in the model
c       header about the image file that the model was last loaded on.  ^
c       Set [idir] to 0 to undo the scaling, for working in index coordinates ^
c       Set [idir] to 1 to redo the scaling before saving the model back out.
c       !
      subroutine scale_model(idir)
      implicit none
      include 'model.inc'
      integer getimodhead,getimodscales
      integer*4 ierr, ierr2, ifflip, idir, i
      real*4 ximscale, yimscale, zimscale,xyscal,zscale,xofs,yofs,zofs
c       
      ierr=getimodhead(xyscal,zscale,xofs,yofs,zofs,ifflip)
      ierr2 = getimodscales(ximscale, yimscale, zimscale)
      if (ierr .ne. 0 .or. ierr2 .ne. 0) then
        print *,'ERROR: SCALE_MODEL: getting model header'
        call exit(1)
      endif
      if (idir .eq. 0) then
c	  
c         shift the data to index coordinates before working with it
c         
        do i=1,n_point
          p_coord(1,i)=(p_coord(1,i)-xofs) / ximscale
          p_coord(2,i)=(p_coord(2,i)-yofs) / yimscale
          p_coord(3,i)=(p_coord(3,i)-zofs) / zimscale
        enddo
      else
c	  
c         shift the data back for saving
c         
        do i=1,n_point
          p_coord(1,i)=ximscale*p_coord(1,i)+xofs
          p_coord(2,i)=yimscale*p_coord(2,i)+yofs
          p_coord(3,i)=zimscale*p_coord(3,i)+zofs
        enddo
      endif
      return
      end

c       !
c       Will undo or redo the scaling from image index coordinates to model
c       coordinates performed by imodel_fwrap, using the header information of
c       the image file open on unit [iunit].  The resulting index coordinates
c       will fit those of this image rather than the image file the model was
c       last loaded on. ^
c       Set [idir] to 0 to undo the scaling, for working in index coordinates ^
c       Set [idir] to 1 to redo the scaling before saving the model back out.
c       !
      subroutine scaleModelToImage(iunit, idir)
      implicit none
      include 'model.inc'
      integer*4  idir, i, iunit, j, imodHasImageRef, putImageRef
      real*4 orig(3), delt(3)
c       
c
      call irtorg(iunit, orig(1), orig(2), orig(3))
      call irtdel(iunit, delt)

      if (idir .eq. 0) then
c	  
c         shift the data to index coordinates before working with it, but only if the
c         it got shifted in the first place
        if (imodHasImageRef() .le. 0) return
c
        do i=1,n_point
          do j = 1, 3
            p_coord(j,i)=(p_coord(j,i) + orig(j)) / delt(j)
          enddo
        enddo
      else
c	  
c         shift the data back for saving, but first set an image ref if there is none 
        if (imodHasImageRef() .le. 0) i = putImageRef(delt, orig)
        do i=1,n_point
          do j = 1, 3
            p_coord(j,i)=p_coord(j,i) * delt(j) - orig(j)
          enddo
        enddo
      endif
      return
      end
