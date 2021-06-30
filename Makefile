PLATFORM ?= musl64
STRIP = strip
PKG ?= periodic-client-exe

ifeq ($(PLATFORM),aarch64-multiplatform-musl)
STRIP = aarch64-unknown-linux-musl-strip
else

endif

all: package

$(PLATFORM):
	mkdir -p $@

$(PLATFORM)/%: $(PLATFORM)
	nix-build -A projectCross.$(PLATFORM).hsPkgs.$(PKG).components.exes.$(shell basename $@)
	cp -f result/bin/$(shell basename $@) $@
	chmod +w $@
	nix-shell --run "$(STRIP) -s $@"
	chmod -w $@

periodic:
	PKG=periodic-client-exe make $(PLATFORM)/$@

periodic-run:
	PKG=periodic-client-exe make $(PLATFORM)/$@

periodic-http-bridge:
	PKG=periodic-client-exe make $(PLATFORM)/$@

periodicd:
	PKG=periodic-server make $(PLATFORM)/$@

package: periodic periodic-run periodic-http-bridge periodicd
	cd $(PLATFORM) && tar cjvf ../periodic-linux-$(PLATFORM).tar.bz2 periodic*

clean:
	rm -rf $(PLATFORM)
