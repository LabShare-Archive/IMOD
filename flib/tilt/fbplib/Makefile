#     MAKEFILE FOR Fbplib 
#
#  Subroutines in this library were written by Kristian Sandberg
#
include ../../configure

O        = $(OBJEXT)

#
#
OBJS     = fbp.$(O) angsum.$(O) angsum0.$(O) fbpt2f.$(O) fbpt2f0.$(O) \
 fbpini.$(O) angsumi.$(O) angsumini.$(O) fbpt2fi.$(O) fbpt2fini.$(O) \
 makefilter.$(O) der_par.$(O) trig_ang.$(O) openfile.$(O) slicecopy.$(O) \
 opt2mass.$(O) angread.$(O) pad.$(O) diagmult.$(O) f2t.$(O) crop.$(O) \
 writeslice.$(O) freadbin.$(O) nice_pad.$(O) fbpneed.$(O)
#
#
LIBRARY  =  ../libfbp.a  
#
#
all:	ALWAYS $(OBJS)  
	$(MAKESTATICLIB)$(LIBRARY) $(OBJS)
	$(STATICRANLIB) $(LIBRARY)

#
clean:
	-\rm -f *.o* *.a *~

ALWAYS:
