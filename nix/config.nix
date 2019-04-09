{ nixpkgs ? import <nixpkgs> {}, static ? false}:

let
  pkgs = if static then nixpkgs.pkgsMusl
                   else nixpkgs.pkgs;
  libffi = pkgs.libffi.overrideDerivation (attrs: {
    configureFlags = [
    "--with-gcc-arch=generic" # no detection of -march= or -mtune=
    "--enable-pax_emutramp"
    "--enable-static"
    ];
  });
  configureFlags = if static then [
    "--ghc-option=-optl=-static"
    "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
    "--extra-lib-dirs=${pkgs.zlib.static}/lib"
    "--extra-lib-dirs=${libffi}/lib"
  ] else [];
in {
  configureFlags = configureFlags;
  pkgs = pkgs;
}
