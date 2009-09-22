all: single dual single-test-gui dual-test-gui single-fidless single-montage dual-montage single-montage-test-gui dual-montage-test-gui join join-test-processes nad
recon: single dual single-test-gui dual-test-gui single-fidless single-montage dual-montage single-montage-test-gui dual-montage-test-gui
build: single dual single-fidless single-montage dual-montage join nad
bc: single-test-gui dual-test-gui dual-montage-test-gui
temp: 

single: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single
dual: dummy
	$(IMOD_UITEST_SCRIPT)/uitest dual
single-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-test-gui
dual-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest dual-test-gui
single-fidless: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-fidless
single-montage: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-montage
dual-montage: dummy
	$(IMOD_UITEST_SCRIPT)/uitest dual-montage
single-montage-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-montage-test-gui
dual-montage-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest dual-montage-test-gui
join: dummy
	$(IMOD_UITEST_SCRIPT)/uitest join
join-test-processes: dummy
	$(IMOD_UITEST_SCRIPT)/uitest join-test-processes
nad: dummy
	$(IMOD_UITEST_SCRIPT)/uitest nad
dummy:
	
