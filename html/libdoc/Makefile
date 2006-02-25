include ../../configure

# Make doc files and place them in help directory
all :
	mkdir -p ../libhelp
	$(CP) libimod.html ../libhelp 
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

# For testing
#	../../sourcedoc/$(SOURCEDOC_EXE) testin.html testout.html
#	../../sourcedoc/$(SOURCEDOC_EXE) -f testfin.html testfout.html

clean :
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
