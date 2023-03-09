PLATFORM ?= musl64
STRIP = strip
PKG ?= periodic-client-exe
COMPILER = ghc925

ifeq ($(PLATFORM),aarch64-multiplatform-musl)
STRIP = aarch64-linux-gnu-strip
else
ifeq ($(PLATFORM),muslpi)
STRIP = armv6l-unknown-linux-musleabihf-strip
COMPILER = ghc884
else

endif

endif

OUT = periodic periodic-run periodic-http-bridge periodicd
BUNDLE_BIN = dist/bundle/bin
BUNDLE_LIB = dist/bundle/lib/periodic
BUNDLE_EXEC_PATH = @executable_path/../lib/periodic
BUNDLE = dylibbundler -b -d $(BUNDLE_LIB) -p '$(BUNDLE_EXEC_PATH)' -of
BUNDLE_BINS = $(foreach var,$(OUT),dist/bundle/bin/$(var))

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

package: $(OUT)
	cd dist/$(PLATFORM) && tar cjvf ../periodic-linux-$(PLATFORM).tar.bz2 periodic*


plan-sha256:
	nix-build -A plan-nix.passthru.calculateMaterializedSha | bash

materialized:
	rm -r nix/materialized
	nix-build 2>&1 | grep -om1 '/nix/store/.*-updateMaterialized' | bash

dist/bundle/bin/%: bin/%
	@mkdir -p dist/bundle/bin
	@mkdir -p dist/bundle/lib/periodic
	cp $< $@
	nix-shell -p macdylibbundler --run "$(BUNDLE) -x $@"
	echo sudo xattr -d com.apple.quarantine $< >> dist/bundle/install.sh

macos-build:
	stack install --local-bin-path bin

macos-install:
	echo '#!/usr/bin/env bash' > dist/bundle/install.sh

macos-bundle: macos-install macos-build $(BUNDLE_BINS)
	cd dist/bundle && find lib -type f | while read F; do echo sudo xattr -d com.apple.quarantine $$F >> install.sh; done
	chmod +x dist/bundle/install.sh
	cd dist/bundle && tar cjvf ../macos-bundle.tar.bz2 .

clean:
	rm -rf dist

help:
	@echo make PLATFORM=muslpi
	@echo make PLATFORM=musl64
	@echo make PLATFORM=aarch64-multiplatform-musl
	@echo make clean
	@echo make plan-sha256
	@echo make materialized
