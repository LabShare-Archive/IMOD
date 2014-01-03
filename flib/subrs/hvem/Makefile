include ../../configure

FFLAGS = $(LIBFFLAGS)

O        = $(OBJEXT)
OBJS = dopen.$(O) getinout.$(O) mypause.$(O) \
 rdlist.$(O) wrlist.$(O) frefor.$(O) copy_to_backup.$(O) niceframe.$(O) \
 get_tilt_angles.$(O) int_iwrite.$(O) inv_matrix.$(O) xfmult3d.$(O) \
 xfcopy3d.$(O) xfinv3d.$(O) indmap.$(O) ialsiz_sam_cel.$(O) get_nxyz.$(O) \
 get_extra_header_pieces.$(O) get_extra_header_tilts.$(O) temp_filename.$(O) \
 taperinvol.$(O) setctf.$(O) parse_input_params.$(O) parallel_write.$(O) \
 getbinnedsize.$(O) set_projection_rays.$(O) objtocont.$(O) b3ddate.$(O) b3dxor.$(O) \
 inside.$(O)


all: $(OBJS)

clean:
	$(FIND) . -type f -name "*.$(OBJEXT)" -exec rm "{}" \;
	$(FIND) . -type f -name "*.mod" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
