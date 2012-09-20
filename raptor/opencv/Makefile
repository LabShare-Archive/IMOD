include ../../configure

O        = $(OBJEXT)
LIBRARY  = ../libopencv.$(STATICEXT)

all: $(LIBRARY)

OBJS = cvaccum.$(O) cvsumpixels.$(O) cvtables.$(O) cvtemplmatch.$(O) \
 cvutils.$(O) cxalloc.$(O) cxarithm.$(O) cxarray.$(O) cxconvert.$(O) \
 cxcopy.$(O) cxdatastructs.$(O) cxdxt.$(O) cxerror.$(O) cxlut.$(O) \
 cxmathfuncs.$(O) cxmatmul.$(O) cxmatrix.$(O) cxmean.$(O) cxmeansdv.$(O) \
 cxminmaxloc.$(O) cxnorm.$(O) cxrand.$(O) cxsumpixels.$(O) cxsvd.$(O) \
 cxtables.$(O) cxutils.$(O)



$(LIBRARY): $(OBJS)
	$(MAKESTATICLIB)$(LIBRARY) $(OBJS)
	$(STATICRANLIB) $(LIBRARY)

clean:
	-\rm -f *.$(OBJEXT) *.a *.lib *~

$(OBJS): *.h *.hpp

