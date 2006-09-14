SCRIPT_PATH = ~sueh/workspace/Etomo/scripts

all:	dual single build_build build_single

test:	dual single

build:	build_dual build_single

dual:
	$(SCRIPT_PATH)/uitest dual
single:
	$(SCRIPT_PATH)/uitest single
build-dual:
	$(SCRIPT_PATH)/uitest build-dual
build-single:
	$(SCRIPT_PATH)/uitest build-single
