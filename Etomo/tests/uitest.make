all: gpu-test-gui single-test-gui peet-test-gui join-test-gui serial-sections-test-gui nad-test-gui generic-test-gui flatten-volume-test-gui dual-test-gui serial-sections-montage-test-gui single-montage-test-gui single-patch-tracking-test-gui single-fidless-test-gui single-simple-align-test-gui dual-montage-test-gui dual-montage-patch-tracking-test-gui
all-pt: single-patch-tracking-test-gui single-patch-tracking dual-montage-patch-tracking
all-peet: peet peet-test-gui
build: single dual single-montage dual-montage join nad peet generic flatten-volume
recon: single dual single-test-gui dual-test-gui single-fidless single-simple-align single-patch-tracking-test-gui single-patch-tracking single-montage dual-montage single-montage-test-gui single-montage-patch-tracking dual-montage-test-gui dual-montage-patch-tracking

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
single-fidless-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-fidless-test-gui
single-simple-align: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-simple-align
single-simple-align-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-simple-align-test-gui
single-patch-tracking-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-patch-tracking-test-gui
single-patch-tracking: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-patch-tracking
single-montage: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-montage
dual-montage: dummy
	$(IMOD_UITEST_SCRIPT)/uitest dual-montage
single-montage-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-montage-test-gui
single-montage-patch-tracking: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-montage-patch-tracking
dual-montage-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest dual-montage-test-gui
dual-montage-patch-tracking: dummy
	$(IMOD_UITEST_SCRIPT)/uitest dual-montage-patch-tracking
dual-montage-patch-tracking-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest dual-montage-patch-tracking-test-gui
join: dummy
	$(IMOD_UITEST_SCRIPT)/uitest join
join-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest join-test-gui
serial-sections-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest serial-sections-test-gui
serial-sections-montage-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest serial-sections-montage-test-gui
nad: dummy
	$(IMOD_UITEST_SCRIPT)/uitest nad
nad-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest nad-test-gui
peet: dummy
	$(IMOD_UITEST_SCRIPT)/uitest peet
peet-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest peet-test-gui
generic: dummy
	$(IMOD_UITEST_SCRIPT)/uitest generic
generic-test-gui: dummy
	 $(IMOD_UITEST_SCRIPT)/uitest generic-test-gui
flatten-volume: dummy
	$(IMOD_UITEST_SCRIPT)/uitest flatten-volume
flatten-volume-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest flatten-volume-test-gui
gpu-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest gpu-test-gui
dummy:
	
