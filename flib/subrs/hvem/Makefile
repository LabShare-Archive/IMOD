include ../../configure
#
# 7/18/00 CER move strupcase to imsubs2
#    (strupcase.o was in the OBJS list below)
#
FFLAGS = $(LIBFFLAGS)

O        = $(OBJEXT)
OBJS = concat.$(O) dopen.$(O) getinout.$(O) mypause.$(O) convexbound.$(O) \
 rdlist.$(O) wrlist.$(O) frefor.$(O) copy_to_backup.$(O) niceframe.$(O) \
 get_tilt_angles.$(O) int_iwrite.$(O) inv_matrix.$(O) xfmult3d.$(O) \
 xfcopy3d.$(O) xfinv3d.$(O) taperoutpad.$(O) taperinpad.$(O) padfill.$(O) \
 meanzero.$(O) setctf.$(O) indmap.$(O) ialsiz_sam_cel.$(O) get_nxyz.$(O) \
 get_extra_header_pieces.$(O) get_extra_header_tilts.$(O) temp_filename.$(O) \
 taperinvol.$(O) reduce_by_binning.$(O) parse_input_params.$(O)


all: $(OBJS)

clean:
	$(FIND) . -type f -name "*.o*" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
