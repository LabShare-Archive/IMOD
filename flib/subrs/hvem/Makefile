include ../../configure
#
# 7/18/00 CER move strupcase to imsubs2
#    (strupcase.o was in the OBJS list below)
#
OBJS = concat.o dopen.o getinout.o mypause.o\
 rdlist.o wrlist.o frefor.o copy_to_backup.o niceframe.o\
 get_tilt_angles.o int_iwrite.o inv_matrix.o xfmult3d.o xfcopy3d.o xfinv3d.o\
 taperoutpad.o taperinpad.o padfill.o meanzero.o setctf.o indmap.o\
 ialsiz_sam_cel.o get_extra_header_pieces.o get_extra_header_tilts.o\
 temp_filename.o taperinvol.o


all: $(OBJS)

clean:
	$(FIND) . -type f -name "*.o" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
