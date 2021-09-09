PLATFORM ?= musl64
STRIP = strip
PKG ?= periodic-client-exe
COMPILER = ghc8107

ifeq ($(PLATFORM),aarch64-multiplatform-musl)
STRIP = aarch64-unknown-linux-musl-strip
else
ifeq ($(PLATFORM),muslpi)
STRIP = armv6l-unknown-linux-musleabihf-strip
COMPILER = ghc884
else

endif

endif

all: package

dist/$(PLATFORM):
	mkdir -p $@

dist/$(PLATFORM)/%: dist/$(PLATFORM)
	nix-build -A projectCross.$(PLATFORM).hsPkgs.$(PKG).components.exes.$(shell basename $@) --argstr compiler-nix-name $(COMPILER)
	cp -f result/bin/$(shell basename $@) $@
	chmod +w $@
	nix-shell --run "$(STRIP) -s $@" --argstr compiler-nix-name $(COMPILER) --arg crossPlatforms "ps: with ps; [$(PLATFORM)]"
	chmod -w $@

periodic:
	PKG=periodic-client-exe make dist/$(PLATFORM)/$@

periodic-run:
	PKG=periodic-client-exe make dist/$(PLATFORM)/$@

periodic-http-bridge:
	PKG=periodic-client-exe make dist/$(PLATFORM)/$@

periodicd:
	PKG=periodic-server make dist/$(PLATFORM)/$@

package: periodic periodic-run periodic-http-bridge periodicd
	cd dist/$(PLATFORM) && tar cjvf ../periodic-linux-$(PLATFORM).tar.bz2 periodic*


plan-sha256:
	nix-build -A plan-nix.passthru.calculateMaterializedSha | bash

materialized:
	rm -r nix/materialized
	nix-build 2>&1 | grep -om1 '/nix/store/.*-updateMaterialized' | bash

clean:
	rm -rf dist

help:
	@echo make PLATFORM=muslpi
	@echo make PLATFORM=musl64
	@echo make PLATFORM=aarch64-multiplatform-musl
	@echo make clean
	@echo make plan-sha256
	@echo make materialized
