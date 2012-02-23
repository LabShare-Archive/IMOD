all: peet-test-gui single-test-gui single-patch-tracking-test-gui single-fidless single-simple-align dual-test-gui dual-testBB-test-gui single-montage-test-gui dual-montage-test-gui dual-montage-patch-tracking join-test-gui nad generic flatten-volume-test-gui
sub: peet peet-test-gui single-patch-tracking-test-gui single-fidless single-simple-align dual-test-gui single-montage-test-gui join-test-gui nad generic flatten-volume-test-gui

all-pt: single-patch-tracking-test-gui single-patch-tracking dual-montage-patch-tracking
all-peet: peet peet-test-gui

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
single-simple-align: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-simple-align
single-patch-tracking-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-patch-tracking-test-gui
single-patch-tracking: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-patch-tracking
dual-testBB-test-gui:
	$(IMOD_UITEST_SCRIPT)/uitest dual-testBB-test-gui
single-montage: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-montage
dual-montage: dummy
	$(IMOD_UITEST_SCRIPT)/uitest dual-montage
single-montage-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-montage-test-gui
dual-montage-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest dual-montage-test-gui
dual-montage-patch-tracking: dummy
	$(IMOD_UITEST_SCRIPT)/uitest dual-montage-patch-tracking
single-testmontage-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest single-testmontage-test-gui
dual-testmontage-patch-tracking: dummy
	$(IMOD_UITEST_SCRIPT)/uitest dual-testmontage-patch-tracking
join: dummy
	$(IMOD_UITEST_SCRIPT)/uitest join
join-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest join-test-gui
join-testunicross-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest join-testunicross-test-gui
nad: dummy
	$(IMOD_UITEST_SCRIPT)/uitest nad
nad-testunicross: dummy
	$(IMOD_UITEST_SCRIPT)/uitest nad-testunicross
peet: dummy
	$(IMOD_UITEST_SCRIPT)/uitest peet
peet-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest peet-test-gui
generic: dummy
	$(IMOD_UITEST_SCRIPT)/uitest generic
flatten-volume: dummy
	$(IMOD_UITEST_SCRIPT)/uitest flatten-volume
flatten-volume-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest flatten-volume-test-gui
dummy:
	
