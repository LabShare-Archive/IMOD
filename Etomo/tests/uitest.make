all:	test build

test:	test-dual test-single

build:	build-dual build-single

build-fidless:	build-dual-fidless

test-dual:
	which uitest
	uitest dual
test-single:
	uitest single
build-dual:
	uitest build-dual
build-single:
	uitest build-single
build-dual-fidless:
	uitest build-dual-fidless
build-dual-montage:
	uitest build-dual-montage
