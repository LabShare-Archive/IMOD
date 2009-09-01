all: single dual join single-montage dual-montage join-test-processes single-test-gui dual-test-gui single-montage-test-gui dual-montage-test-gui single-fidless nad

single: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single
dual: dummy
	$(IMOD_UITEST_SCRIPT)/uitest dual
join: dummy
	$(IMOD_UITEST_SCRIPT)/uitest join
single-montage: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-montage
dual-montage: dummy
	$(IMOD_UITEST_SCRIPT)/uitest dual-montage
join-test-processes: dummy
	$(IMOD_UITEST_SCRIPT)/uitest join-test-processes
single-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-test-gui
dual-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest dual-test-gui
single-montage-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-montage-test-gui
dual-montage-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest dual-montage-test-gui
single-fidless: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-fidless
nad: dummy
	$(IMOD_UITEST_SCRIPT)/uitest nad
dummy:
	
