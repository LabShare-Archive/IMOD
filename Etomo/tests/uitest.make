SCRIPT_PATH = ~sueh/workspace/Etomo/scripts

all:	test build

test:	test-dual test-single

build:	build-dual build-single

build-fidless:	build-dual-fidless

test-dual:
	$(SCRIPT_PATH)/uitest dual
test-single:
	$(SCRIPT_PATH)/uitest single
build-dual:
	$(SCRIPT_PATH)/uitest build-dual
build-single:
	$(SCRIPT_PATH)/uitest build-single
build-dual-fidless:
	$(SCRIPT_PATH)/uitest build-dual-fidless