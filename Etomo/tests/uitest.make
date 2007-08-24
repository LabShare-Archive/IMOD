all:	test build

test:	test-dual test-single

build:	build-dual build-single

build-fidless:	build-dual-fidless

test-dual:
	$(IMOD_UITEST_SCRIPT)/uitest dual
test-single:
	$(IMOD_UITEST_SCRIPT)/uitest single
build-dual:
	$(IMOD_UITEST_SCRIPT)/uitest build-dual
build-single:
	$(IMOD_UITEST_SCRIPT)/uitest build-single
build-dual-fidless:
	$(IMOD_UITEST_SCRIPT)/uitest build-dual-fidless
build-dual-montage:
	$(IMOD_UITEST_SCRIPT)/uitest build-dual-montage
