include ../../configure

# Make doc files and place them in help directory
all :
	mkdir -p ../libhelp
	../../sourcedoc/$(SOURCEDOC_EXE) -d ../../libimod libimod.html ../libhelp/libimod.html
# For testing
#	../../sourcedoc/$(SOURCEDOC_EXE) testin.html testout.html
#	../../sourcedoc/$(SOURCEDOC_EXE) -f testfin.html testfout.html

# Remove output files from help directory
clean :
	$(FIND) ../libhelp -type f -name "*"  -exec rm "{}" \;
	$(FIND) . -type f -name "*~"    -exec rm "{}" \;
	$(FIND) . -type f -name "#*#"   -exec rm "{}" \;
