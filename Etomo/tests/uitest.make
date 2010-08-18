all: single dual single-test-gui dual-test-gui single-fidless single-simple-align single-patch-tracking-test-gui single-montage dual-montage single-montage-test-gui dual-montage-test-gui dual-montage-patch-tracking join join-test-gui generic nad peet peet-test-gui flatten-volume flatten-volume-test-gui
small: single dual single-montage join generic nad peet
small-no-peet: single dual single-montage join generic nad
build: single dual single-fidless single-simple-align single-montage dual-montage join generic nad peet
test-gui: single-test-gui dual-test-gui single-montage-test-gui dual-montage-test-gui join-test-gui peet-test-gui
edf: single dual single-test-gui dual-test-gui single-fidless single-simple-align single-montage dual-montage single-montage-test-gui dual-montage-test-gui
ejf: join join-test-gui
epp: generic nad
epe: peet peet-test-gui
bb: single dual single-test-gui dual-test-gui single-fidless single-simple-align single-montage dual-montage single-montage-test-gui dual-montage-test-gui generic
midzone2: single-montage dual-montage single-montage-test-gui dual-montage-test-gui
unicross: join join-test-gui nad
pi: peet peet-test-gui
temp: single-patch-tracking-test-gui single-montage dual-montage single-montage-test-gui dual-montage-test-gui dual-montage-patch-tracking join join-test-gui generic nad peet peet-test-gui flatten-volume flatten-volume-test-gui

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
dummy:
	
