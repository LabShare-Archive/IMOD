#
# Makefile for libdia.a
# 
include ../configure

LIB      = libdia.$(LIBEXT)
INCLUDES = $(INCDIR)dia.h $(INCDIR)diaP.h
OBJS     = dia_abort.o dia_init.o dia_ask.o dia_puts.o dia_gets.o dia_msg.o \
dia_choice.o dia_file.o dia_int.o dia_color.o dia_fact.o xcramp.o \
dia_sgicolor.o dia_sgcolor.o dialog.o dia_act.o \
dia_menu.o diaListPane.o dia_print.o

all : $(LIB)

$(LIB) : $(OBJS)
	$(FIND) . -type f -name $(LIB) -exec rm "{}" \;
	$(MAKELIB) $(LIB) $(OBJS)
	$(RANLIB) $(LIB)
	$(CP) $(LIB) $(LIBDIR)


install : $(LIB)
	$(CP) $(LIB) $(INSTLIBDIR)

clean :
	$(FIND) . -type f -name "*.o"   -exec rm "{}" \;
	$(FIND) . -type f -name $(LIB)  -exec rm "{}" \;
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
	$(FIND) . -type f -name test    -exec rm "{}" \;

work : clean
	touch $(OBJS)
	touch $(LIB)

test : test.o $(LIB)
# 8/1/00 CER... what is -rpath??  what is -lPW???
#	$(CC) -o test test.o -rpath . -L. -ldia -lXm -lXt -lX11 -lXext -lPW
	$(CC) -o test test.o $(LDFLAGS) -ldia -lXm -lXt -lX11 -lXext 


$(OBJS) : $(INCLUDES)
xcramp.o: $(INCDIR)xcramp.h
sgi_visual.o: $(INCDIR)sgi_visual.h
diaListPane.o: $(INCDIR)diaListPane.h
