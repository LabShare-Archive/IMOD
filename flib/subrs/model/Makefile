include ../../configure

OBJS = obj_manager.o read_mod.o readw_or_imod.o repack_mod.o store_mod.o\
 write_wmod.o iobjfromcont.o objtocont.o

###################################
# These objects are obsolete.
# Use the libimod library instead.
IMOD_OBJS = imodf.o rdimod.o imodslib.o

all: $(OBJS)

clean:
	$(FIND) . -type f -name "*.o" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;

obj_manager.o:	$(INCDIR)model.inc
read_mod.o:	$(INCDIR)model.inc
readw_or_imod.o:	$(INCDIR)model.inc
repack_mod.o:	$(INCDIR)model.inc
store_mod.o:	$(INCDIR)model.inc
write_wmod.o:	$(INCDIR)model.inc
iobjfromcont.o:	$(INCDIR)model.inc
objtocont.o:	$(INCDIR)model.inc
