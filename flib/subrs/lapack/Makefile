include ../../configure

#######################################################################
#  This is the makefile to create a library for LAPACK.
#  The files are organized as follows:
#       ALLAUX -- Auxiliary routines called from all precisions
#       SCLAUX -- Auxiliary routines called from both REAL and COMPLEX
#       DZLAUX -- Auxiliary routines called from both DOUBLE PRECISION
#                 and COMPLEX*16
#       SLASRC -- Single precision real LAPACK routines
#       CLASRC -- Single precision complex LAPACK routines
#       DLASRC -- Double precision real LAPACK routines
#       ZLASRC -- Double precision complex LAPACK routines
#
#  The library can be set up to include routines for any combination
#  of the four precisions.  To create or add to the library, enter make
#  followed by one or more of the precisions desired.  Some examples:
#       make single
#       make single complex
#       make single double complex complex16
#  Alternatively, the command
#       make
#  without any arguments creates a library of all four precisions.
#  The library is called
#       lapack.a
#  and is created at the next higher directory level.
#
#  To remove the object files after the library is created, enter
#       make clean
#  On some systems, you can force the source files to be recompiled by
#  entering (for example)
#       make single FRC=FRC
#
#  ***Note***
#  The functions lsame, second, dsecnd, slamch, and dlamch may have
#  to be installed before compiling the library.  Refer to the
#  installation guide, LAPACK Working Note 41, for instructions.
#
#######################################################################
CFLAGS = $(LIBCFLAGS)
FFLAGS = $(LIBFFLAGS)

O        = $(OBJEXT)

all: double
 
ALLAUX = ilaenv.$(O) ieeeck.$(O) lsame.$(O) lsamen.$(O) xerbla.$(O)

DZLAUX = \
   dbdsdc.$(O) \
   dbdsqr.$(O) ddisna.$(O) dlabad.$(O) dlacpy.$(O) dladiv.$(O) dlae2.$(O)  dlaebz.$(O) \
   dlaed0.$(O) dlaed1.$(O) dlaed2.$(O) dlaed3.$(O) dlaed4.$(O) dlaed5.$(O) dlaed6.$(O) \
   dlaed7.$(O) dlaed8.$(O) dlaed9.$(O) dlaeda.$(O) dlaev2.$(O) dlagtf.$(O) \
   dlagts.$(O) dlamch.$(O) dlamrg.$(O) dlanst.$(O) \
   dlapy2.$(O) dlapy3.$(O) dlarnv.$(O) \
   dlarrb.$(O) dlarre.$(O) dlarrf.$(O) \
   dlartg.$(O) dlaruv.$(O) dlas2.$(O)  dlascl.$(O) \
   dlasd0.$(O) dlasd1.$(O) dlasd2.$(O) dlasd3.$(O) dlasd4.$(O) dlasd5.$(O) dlasd6.$(O) \
   dlasd7.$(O) dlasd8.$(O) dlasd9.$(O) dlasda.$(O) dlasdq.$(O) dlasdt.$(O) \
   dlaset.$(O) dlasq1.$(O) dlasq2.$(O) dlasq3.$(O) dlasq4.$(O) dlasq5.$(O) dlasq6.$(O) \
   dlasr.$(O)  dlasrt.$(O) dlassq.$(O) dlasv2.$(O) dpttrf.$(O) dstebz.$(O) dstedc.$(O) \
   dsteqr.$(O) dsterf.$(O) dsecnd.$(O)

DLASRC = \
   dgelss.$(O) dgeqrf.$(O) dlange.$(O) dlarfb.$(O) dlarft.$(O) dorgbr.$(O) dorglq.$(O) \
   dorgqr.$(O) dormbr.$(O) dormlq.$(O) dormqr.$(O) drscl.$(O) dbdsqr.$(O) dlabad.$(O) \
   dlacpy.$(O) dlamch.$(O) dlartg.$(O) dlas2.$(O) dlascl.$(O) dlaset.$(O) dlasq1.$(O) \
   dlasq2.$(O) dlasq3.$(O) dlasq4.$(O) dlasq5.$(O) dlasq6.$(O) dlasr.$(O) dlasrt.$(O) \
   dlassq.$(O) dlasv2.$(O) ilaenv.$(O) ieeeck.$(O) lsame.$(O) xerbla.$(O) dgebrd.$(O) \
   dgelqf.$(O) dgeqr2.$(O) dlabrd.$(O) dlarf.$(O) dlarfg.$(O) dorg2r.$(O) dorgl2.$(O) \
   dorm2r.$(O) dorml2.$(O) dlapy2.$(O) dgebd2.$(O) dgelq2.$(O) dsysv.$(O) dsytrf.$(O) \
   dsytrs.$(O) dsytf2.$(O) dlasyf.$(O) dspsv.$(O) dsptrf.$(O) dsptrs.$(O) \
   dsyev.$(O) dlansy.$(O) dsytrd.$(O) dorgtr.$(O) dlatrd.$(O) dsytd2.$(O) dorgql.$(O) \
   dorg2l.$(O)

double: $(DLASRC) $(ALLAUX) $(DZLAUX)

clean:
	$(FIND) . -type f -name "*.$(OBJEXT)" -exec rm "{}" \;
	$(FIND) . -type f -name "*~" -exec rm "{}" \;
	$(FIND) . -type f -name "#*#" -exec rm "{}" \;
