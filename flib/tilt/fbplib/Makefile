#     MAKEFILE FOR Fbplib 
#
#  Subroutines in this library were written by Kristian Sandberg
#
include ../../configure

#
#
OBJS     = fbp.o angsum.o angsum0.o fbpt2f.o fbpt2f0.o fbpini.o angsumi.o \
 angsumini.o fbpt2fi.o fbpt2fini.o makefilter.o der_par.o trig_ang.o \
 openfile.o slicecopy.o opt2mass.o angread.o pad.o diagmult.o f2t.o crop.o \
 writeslice.o freadbin.o nice_pad.o fbpneed.o
#
#
LIBRARY  =  ../libfbp.a  
#
#
all:	ALWAYS $(OBJS)  
	ar cr $(LIBRARY) $(OBJS)
	
#
clean:
	-\rm -f *.o *.a *~

ALWAYS:
