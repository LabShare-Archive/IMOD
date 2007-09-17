include ../make.inc

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
 
ALLAUX = ilaenv.o ieeeck.o lsame.o lsamen.o xerbla.o

DZLAUX = \
   dbdsdc.o \
   dbdsqr.o ddisna.o dlabad.o dlacpy.o dladiv.o dlae2.o  dlaebz.o \
   dlaed0.o dlaed1.o dlaed2.o dlaed3.o dlaed4.o dlaed5.o dlaed6.o \
   dlaed7.o dlaed8.o dlaed9.o dlaeda.o dlaev2.o dlagtf.o \
   dlagts.o dlamch.o dlamrg.o dlanst.o \
   dlapy2.o dlapy3.o dlarnv.o \
   dlarrb.o dlarre.o dlarrf.o \
   dlartg.o dlaruv.o dlas2.o  dlascl.o \
   dlasd0.o dlasd1.o dlasd2.o dlasd3.o dlasd4.o dlasd5.o dlasd6.o \
   dlasd7.o dlasd8.o dlasd9.o dlasda.o dlasdq.o dlasdt.o \
   dlaset.o dlasq1.o dlasq2.o dlasq3.o dlasq4.o dlasq5.o dlasq6.o \
   dlasr.o  dlasrt.o dlassq.o dlasv2.o dpttrf.o dstebz.o dstedc.o \
   dsteqr.o dsterf.o dsecnd.o

DLASRC = \
   dgelss.o dgeqrf.o dlange.o dlarfb.o dlarft.o dorgbr.o dorglq.o \
   dorgqr.o dormbr.o dormlq.o dormqr.o drscl.o dbdsqr.o dlabad.o \
   dlacpy.o dlamch.o dlartg.o dlas2.o dlascl.o dlaset.o dlasq1.o \
   dlasq2.o dlasq3.o dlasq4.o dlasq5.o dlasq6.o dlasr.o dlasrt.o \
   dlassq.o dlasv2.o ilaenv.o ieeeck.o lsame.o xerbla.o dgebrd.o \
   dgelqf.o dgeqr2.o dlabrd.o dlarf.o dlarfg.o dorg2r.o dorgl2.o \
   dorm2r.o dorml2.o dlapy2.o dgebd2.o dgelq2.o

double: $(DLASRC) $(ALLAUX) $(DZLAUX)
	$(ARCH) $(ARCHFLAGS) ../$(LAPACKLIB) $(DLASRC) $(ALLAUX) \
	$(DZLAUX)
	$(RANLIB) ../$(LAPACKLIB)


$(ALLAUX): $(FRC)
$(SCLAUX): $(FRC)
$(DZLAUX): $(FRC)
$(SLASRC): $(FRC)
$(CLASRC): $(FRC)
$(DLASRC): $(FRC)
$(ZLASRC): $(FRC)

FRC:
	@FRC=$(FRC)

clean:
	rm -f *.o

slamch.o: slamch.f
	$(FORTRAN) $(NOOPT) -c $<
dlamch.o: dlamch.f
	$(FORTRAN) $(NOOPT) -c $<

.f.o: 
	$(FORTRAN) $(OPTS) -c $<
