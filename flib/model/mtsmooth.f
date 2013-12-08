*       * * * * MTSMOOTH * * * * *
c       
c       MTSMOOTH will take a model of tracked microtubules and smooth the
c       trajectory of each MT independently.  It will make a much nicer
c       model for presentation than any kind of transformations will.
c       However, it will not necessarily preserve the spacing between MTs on
c       any given section.  Such a smoothed model should thus NOT be used for
c       any kind of analysis that depends on the separations between MTs.
c       
c       The program operates by considering each point of each MT in turn.
c       It fits a polynomial to the nearest N points to that point in the MT,
c       where N is a number specified by the user.  (More precisely, it fits
c       a polynomial to X coordinates as a function of Z, and a separate
c       polynomial to the Y coordinates as a function of Z.)  It replaces the
c       given point with the coordinates of the fitted polynomial at that
c       position in Z.  The order of the polynomials (1 for a straight line,
c       2 for a parabola, etc.,) is also chosen by the user.
c       
c       Entries to the program:
c       
c       Name of input model file 
c       
c       Name of output model file
c       
c       Number of points to fit polynomials to, and the order of the
c       polynomials.  Fitting to 5 or 7 points, with an order of 2, is
c       recommended.
c       
c       Minimum object length for fitting polynomials of the chosen order;
c       objects shorter than this will be fit with a simple straight line.
c       A value of 5 is recommended for this parameter.
c       
c       David Mastronarde  1/13/93
c       
      parameter (limsec=1000)
      include 'model.inc'
      real*4 xmt(limsec),ymt(limsec),zmt(limsec),zfit(1000),slop(50)
c       
      character*80 modelfile,newmodel
      logical exist,readw_or_imod
c       
91    write(*,'(1x,a,$)')'Name of input model file: '
      read(*,'(a)')modelfile
c       
c       read in the model
c       
75    exist=readw_or_imod(modelfile)
      if(.not.exist)go to 91
c       
      write(*,'(1x,a,$)')'Name of output model file: '
      read(*,'(a)')newmodel
c       
      write(*,'(1x,a,$)')
     &    'Number of points to fit to, order of polynomial to fit: '
      read(5,*)nfitpt,iorder
      write(*,'(1x,a,$)')'Minimum number of points required to '//
     &    'fit polynomial rather than line: '
      read(5,*)minfit
c       
      do iobj=1,max_mod_obj
        ninobj=npt_in_obj(iobj)
        if(ninobj.gt.2)then
          ibase=ibase_obj(iobj)
          nzchange=0
          do ind=1,ninobj
            ipt=abs(object(ibase+ind))
            xmt(ind)=p_coord(1,ipt)
            ymt(ind)=p_coord(2,ipt)
            zmt(ind)=p_coord(3,ipt)
            if(ind.gt.1)then
              if(zmt(ind).ne.zmt(ind-1))nzchange=nzchange+1
            endif
          enddo
          if(nzchange.ge.ninobj-2)then
            do ind=1,ninobj
              ipt=abs(object(ibase+ind))
              jfhi=min(ind+nfitpt/2,ninobj)
              jflo=max(1,jfhi+1-nfitpt)
              jfhi=min(ninobj,jflo+nfitpt-1)
              nfit=jfhi+1-jflo
              iord=iorder
              if(nfit.lt.minfit)iord=1
              iz=1
              do jf=jflo,jfhi
                zfit(iz)=zmt(jf)-zmt(ind)
                iz=iz+1
              enddo
              call polyfit(zfit,xmt(jflo),nfit,iord,slop,bint)
              p_coord(1,ipt)=bint
              call polyfit(zfit,ymt(jflo),nfit,iord,slop,bint)
              p_coord(2,ipt)=bint
            enddo
          endif
        endif
      enddo
c       
      call write_wmod(newmodel)
      call exit(0)
      end
