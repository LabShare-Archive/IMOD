include ../../configure

O        = $(OBJEXT)
LIBRARY  = ../libcsparse.$(STATICEXT)

all: $(LIBRARY)

CS = cs_add.$(O) cs_amd.$(O) cs_chol.$(O) cs_cholsol.$(O) cs_counts.$(O) \
 cs_cumsum.$(O) cs_droptol.$(O) cs_dropzeros.$(O) cs_dupl.$(O) cs_entry.$(O) \
 cs_etree.$(O) cs_fkeep.$(O) cs_gaxpy.$(O) cs_happly.$(O) cs_house.$(O) \
 cs_ipvec.$(O) cs_lsolve.$(O) cs_ltsolve.$(O) cs_lu.$(O) cs_lusol.$(O) \
 cs_util.$(O) cs_multiply.$(O) cs_permute.$(O) cs_pinv.$(O) cs_post.$(O) \
 cs_pvec.$(O) cs_qr.$(O) cs_qrsol.$(O) cs_scatter.$(O) cs_schol.$(O) \
 cs_sqr.$(O) cs_symperm.$(O) cs_tdfs.$(O) cs_malloc.$(O) cs_transpose.$(O) \
 cs_compress.$(O) cs_usolve.$(O) cs_utsolve.$(O) cs_scc.$(O) cs_maxtrans.$(O) \
 cs_dmperm.$(O) cs_updown.$(O) cs_print.$(O) cs_norm.$(O) cs_load.$(O) \
 cs_dfs.$(O) cs_reach.$(O) cs_spsolve.$(O) cs_ereach.$(O) cs_leaf.$(O) \
 cs_randperm.$(O)

$(LIBRARY): $(CS)
	$(MAKESTATICLIB)$(LIBRARY) $(CS)
	$(STATICRANLIB) $(LIBRARY)

clean:
	-\rm -f *.$(OBJEXT) *.a *.lib *~

$(CS): cs.h

