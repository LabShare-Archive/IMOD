include ../../configure

FFLAGS = $(LIBFFLAGS)

O        = $(OBJEXT)
OBJS = obj_manager.$(O) read_mod.$(O) readw_or_imod.$(O) repack_mod.$(O) \
 store_mod.$(O) write_wmod.$(O) iobjfromcont.$(O) scale_model.$(O)


all: $(OBJS)

clean:
	$(FIND) . -type f -name "*.$(OBJEXT)" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;

obj_manager.$(O):	$(INCDIR)/model.inc
read_mod.$(O):	$(INCDIR)/model.inc
readw_or_imod.$(O):	$(INCDIR)/model.inc
repack_mod.$(O):	$(INCDIR)/model.inc
store_mod.$(O):	$(INCDIR)/model.inc
write_wmod.$(O):	$(INCDIR)/model.inc
iobjfromcont.$(O):	$(INCDIR)/model.inc
objtocont.$(O):	$(INCDIR)/model.inc
scale_model.$(O):	$(INCDIR)/model.inc
