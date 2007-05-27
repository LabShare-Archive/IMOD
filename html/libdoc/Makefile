include ../../configure

# Make doc files and place them in help directory
all :
	mkdir -p ../libhelp
	$(CP) libimod.html libiimod.html ../libhelp 
	../../sourcedoc/$(SOURCEDOC_EXE) -d ../../libimod imodel.html \
	  ../libhelp/imodel.html
	../../sourcedoc/$(SOURCEDOC_EXE) -d ../../libimod iobj.html \
	  ../libhelp/iobj.html
	../../sourcedoc/$(SOURCEDOC_EXE) -d ../../libimod icont.html \
	  ../libhelp/icont.html
	../../sourcedoc/$(SOURCEDOC_EXE) -d ../../libimod ipoint.html \
	  ../libhelp/ipoint.html
	../../sourcedoc/$(SOURCEDOC_EXE) -d ../../libimod imesh.html \
	  ../libhelp/imesh.html
	../../sourcedoc/$(SOURCEDOC_EXE) -d ../../libimod imodel_files.html \
	  ../libhelp/imodel_files.html
	../../sourcedoc/$(SOURCEDOC_EXE) -d ../../libimod ilist.html \
	  ../libhelp/ilist.html
	../../sourcedoc/$(SOURCEDOC_EXE) -d ../../libimod istore.html \
	  ../libhelp/istore.html
	../../sourcedoc/$(SOURCEDOC_EXE) -d ../../libimod imat.html \
	  ../libhelp/imat.html
	../../sourcedoc/$(SOURCEDOC_EXE) -d ../../libimod mod_fwrap.html \
	  ../libhelp/mod_fwrap.html
	../../sourcedoc/$(SOURCEDOC_EXE) -d ../../libimod b3dutil.html \
	  ../libhelp/b3dutil.html
	../../sourcedoc/$(SOURCEDOC_EXE) -d ../../libimod autodoc.html \
	  ../libhelp/autodoc.html
	../../sourcedoc/$(SOURCEDOC_EXE) -d ../../libiimod mrcfiles.html \
	  ../libhelp/mrcfiles.html
	../../sourcedoc/$(SOURCEDOC_EXE) -d ../../libiimod mrcslice.html \
	  ../libhelp/mrcslice.html
	../../sourcedoc/$(SOURCEDOC_EXE) -d ../../libiimod sliceproc.html \
	  ../libhelp/sliceproc.html
	../../sourcedoc/$(SOURCEDOC_EXE) -d ../../libmesh libmesh.html \
	  ../libhelp/libmesh.html
	../../sourcedoc/$(SOURCEDOC_EXE) -d ../../libcfft libifft.html \
	  ../libhelp/libifft.html

# For testing
#	../../sourcedoc/$(SOURCEDOC_EXE) testin.html testout.html
#	../../sourcedoc/$(SOURCEDOC_EXE) -f testfin.html testfout.html

clean :
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
