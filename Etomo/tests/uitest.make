all: single dual join single-montage join-test-processes single-test-gui dual-test-gui single-fidless

single:
	$(IMOD_UITEST_SCRIPT)/uitest single
dual:
	$(IMOD_UITEST_SCRIPT)/uitest dual
join:
	$(IMOD_UITEST_SCRIPT)/uitest join
single-montage:
	$(IMOD_UITEST_SCRIPT)/uitest single-montage
dual-montage:
	$(IMOD_UITEST_SCRIPT)/uitest dual-montage
join-test-processes:
	$(IMOD_UITEST_SCRIPT)/uitest join-test-processes
single-test-gui:
	$(IMOD_UITEST_SCRIPT)/uitest single-test-gui
dual-test-gui:
	$(IMOD_UITEST_SCRIPT)/uitest dual-test-gui
single-fidless:
	$(IMOD_UITEST_SCRIPT)/uitest single-fidless
