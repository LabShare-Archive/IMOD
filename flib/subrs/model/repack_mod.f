c       REPACK_MOD was a subroutine to repack a WIMP model.  It packs all
c       true point coordinates downward into contiguous space in the P_COORD
c       array, and changes all of the references to those points
c       appropriately in the OBJECT array.  Point marks, text labels, and
c       branch points should all be handled properly.
c       
c       7/6/05: this was creating a stack size too large on the SGI with
c       the new model size.  It was called only immediately after reading
c       or immediately before writing models and was thus a complete no-op
c       since an IMOD model read in is contiguous, and a model being written
c       will have no gaps.  Callers should remove the call on the next
c       available oportunity:
c       beadtrack/beadtrack.f:  call repack_mod
c       beadtrack/beadtrack.f:  call repack_mod
c       model/clipmodel.f:        call repack_mod
c       model/joinmodel.f:      call repack_mod
c       model/joinmodel.f:      call repack_mod
c       model/mtmodel.f:          call repack_mod
c       model/reducemtmod.f:    call repack_mod
c       
c       It had separate real and int arrays size of max_pt - if it were ever
c       needed it could be rewritten to use much less memory.

      subroutine repack_mod
      return
      end

