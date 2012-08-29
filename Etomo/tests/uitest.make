all: single-test-gui single-patch-tracking-test-gui single-fidless single-simple-align dual-test-gui single-montage-test-gui dual-montage-test-gui dual-montage-patch-tracking peet-test-gui join-test-gui serial-sections-test-gui serial-sections-montage-test-gui nad generic flatten-volume-test-gui gpu-test-gui
sub: gpu-test-gui flatten-volume-test-gui generic nad serial-sections-montage-test-gui serial-sections-test-gui join-test-gui peet-test-gui single-montage-test-gui dual-test-gui single-simple-align single-fidless single-patch-tracking-test-gui   

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
gpu-test-gui: dummy
	$(IMOD_UITEST_SCRIPT)/uitest gpu-test-gui
dummy:
	
