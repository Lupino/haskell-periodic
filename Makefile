PLATFORM ?= musl64
STRIP = strip
PKG ?= periodic-client-exe

ifeq ($(PLATFORM),aarch64-multiplatform-musl)
STRIP = aarch64-linux-gnu-strip
COMPILER ?= ghc984
else
ifeq ($(PLATFORM),muslpi)
STRIP = armv6l-unknown-linux-musleabihf-strip
COMPILER ?= ghc884
else
COMPILER ?= ghc984
endif
endif

OUT = periodic periodic-mcp periodic-run periodic-run-pipe periodic-http-bridge periodicd
BUNDLE_BIN = dist/bundle/bin
BUNDLE_LIB = dist/bundle/lib/periodic
BUNDLE_EXEC_PATH = @executable_path/../lib/periodic
BUNDLE = dylibbundler -b -d $(BUNDLE_LIB) -p '$(BUNDLE_EXEC_PATH)' -of
BUNDLE_BINS = $(foreach var,$(OUT),dist/bundle/bin/$(var))

all: package

dist/$(PLATFORM):
	mkdir -p $@

dist/$(PLATFORM)/%: dist/$(PLATFORM)
	nix-build -A projectCross.$(PLATFORM).hsPkgs.$(PKG).components.exes.$(shell basename $@) --argstr compiler-nix-name $(COMPILER) # --arg enableProfiling true
	cp -f result/bin/$(shell basename $@) $@
	chmod +w $@
	nix-shell --run "$(STRIP) -s $@" --argstr compiler-nix-name $(COMPILER) --arg crossPlatforms "ps: with ps; [$(PLATFORM)]"
	chmod -w $@

periodic:
	PKG=periodic-client-exe make dist/$(PLATFORM)/$@

periodic-mcp:
	PKG=periodic-client-exe make dist/$(PLATFORM)/$@

periodic-run:
	PKG=periodic-client-exe make dist/$(PLATFORM)/$@

periodic-run-pipe:
	PKG=periodic-client-exe make dist/$(PLATFORM)/$@

periodic-http-bridge:
	PKG=periodic-client-exe make dist/$(PLATFORM)/$@

periodicd:
	PKG=periodic-server make dist/$(PLATFORM)/$@

package: $(OUT)
	cd dist/$(PLATFORM) && tar cjvf ../periodic-linux-$(PLATFORM).tar.bz2 periodic*

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

update-sha256:
	gawk -f nix/update-sha256.awk cabal.project > nix/sha256map.nix

clean:
	rm -rf dist

help:
	@echo make PLATFORM=muslpi
	@echo make PLATFORM=musl64
	@echo make PLATFORM=aarch64-multiplatform-musl
	@echo make clean
	@echo make update-sha256
