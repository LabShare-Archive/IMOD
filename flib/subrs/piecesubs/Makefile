include ../../configure

OBJS = checklist.o fill_listz.o lookup_piece.o read_piece_list.o

all: $(OBJS)


clean:
	$(FIND) . -type f -name "*.o" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;

