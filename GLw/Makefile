# Makefile for OpenGL widgets

# NOTE: widget code is from SGI.  See any of the .c or .h files for the
# complete copyright.  Mesa's GNU copyright DOES NOT apply to this widget
# code.

# see the file Makefile.X11 for the original and CER's changes to make
# this work when first building for RH 7.0
#
#  $Author$
#
#  $Date$
#
#  $Revision$
#
#  $Log$

include ../configure

LIB = libGLw.a
INCDIRS = -I. -I/usr/include -I/usr/X11R6/include   
LDFLAGS= -L/usr/X11R6/lib -L/usr/lib -lGLU -lGL\
         -lXm -lXext -lXmu -lXi -lX11 -lm

SOURCES = GLwDrawA.c GLwMDrawA.c

OBJECTS = GLwDrawA.o  GLwMDrawA.o


all:  $(LIB)

$(LIB): 
	$(CC) -c  $(SOURCES) $(INCDIRS) $(CFLAGS)  $<
	ar ruv $(LIB) $(OBJECTS)
	ranlib $(LIB)

install : $(LIB)
	if (! (-e $(GLW_INSTINCDIR)))  mkdir $(GLW_INSTINCDIR)
	$(CP) $(LIB) $(GLW_INSTLIBDIR)
	$(CP) *.h $(GLW_INSTINCDIR)

clean :
	$(FIND) . -type f -name "*.o"   -exec rm "{}" \;
	$(FIND) . -type f -name $(LIB)  -exec rm "{}" \;
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
