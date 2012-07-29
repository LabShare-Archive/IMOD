include ../../configure

FFLAGS = $(LIBFFLAGS)

O        = $(OBJEXT)
OBJS = obj_manager.$(O) read_mod.$(O) readw_or_imod.$(O) repack_mod.$(O) \
 write_wmod.$(O) iobjfromcont.$(O) scale_model.$(O)

all: $(OBJS)

obj_manager.f:	../model/obj_manager.f
	sed '/model.inc/s//smallmodel.inc/' ../model/obj_manager.f \
	> obj_manager.f

read_mod.f:	../model/read_mod.f
	sed '/model.inc/s//smallmodel.inc/' ../model/read_mod.f > read_mod.f


readw_or_imod.f:	../model/readw_or_imod.f
	sed -e '/model.inc/s//smallmodel.inc/' \
	-e '/readw_or_imod/s//readsmallmod/' ../model/readw_or_imod.f \
	> readw_or_imod.f


repack_mod.f:	../model/repack_mod.f
	sed '/model.inc/s//smallmodel.inc/' ../model/repack_mod.f \
	> repack_mod.f


write_wmod.f:	../model/write_wmod.f
	sed '/model.inc/s//smallmodel.inc/' ../model/write_wmod.f \
	> write_wmod.f


iobjfromcont.f:	../model/iobjfromcont.f
	sed '/model.inc/s//smallmodel.inc/' ../model/iobjfromcont.f \
	> iobjfromcont.f

scale_model.f:	../model/scale_model.f
	sed '/model.inc/s//smallmodel.inc/' ../model/scale_model.f \
	> scale_model.f

clean:
	$(FIND) . -type f -name "*.$(OBJEXT)" -exec rm "{}" \;
	$(FIND) . -type f -name "*.f*" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;

obj_manager.$(O):	$(INCDIR)/smallmodel.inc
read_mod.$(O):	$(INCDIR)/smallmodel.inc
readw_or_imod.$(O):	$(INCDIR)/smallmodel.inc
repack_mod.$(O):	$(INCDIR)/smallmodel.inc
write_wmod.$(O):	$(INCDIR)/smallmodel.inc
iobjfromcont.$(O):	$(INCDIR)/smallmodel.inc
scale_model.$(O):	$(INCDIR)/smallmodel.inc
