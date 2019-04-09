{ nixpkgs ? import <nixpkgs> {}, static ? false, compiler ? "default" }:

let
  pkgs = if static && !nixpkgs.stdenv.isDarwin
            then nixpkgs.pkgsMusl
            else nixpkgs.pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  periodic-common = haskellPackages.callPackage ./nix/periodic-common.nix { };

  periodic-client0 = haskellPackages.callPackage ./nix/periodic-client.nix {
    inherit periodic-common;
  };
  periodicd0 = haskellPackages.callPackage ./nix/periodicd.nix {
    inherit periodic-common;
  };

  libffi = pkgs.libffi.overrideDerivation (attrs: {
    configureFlags = [
      "--with-gcc-arch=generic" # no detection of -march= or -mtune=
      "--enable-pax_emutramp"
      "--enable-static"
    ];
  });

  periodic-client = if static then periodic-client0.overrideDerivation (attrs: {
    configureFlags = [
      "--ghc-option=-optl=-static"
      "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
      "--extra-lib-dirs=${pkgs.zlib.static}/lib"
      "--extra-lib-dirs=${libffi}/lib"
    ];
  }) else periodic-client0;

  periodicd = if static then periodicd0.overrideDerivation (attrs: {
    configureFlags = [
      "--ghc-option=-optl=-static"
      "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
      "--extra-lib-dirs=${pkgs.zlib.static}/lib"
      "--extra-lib-dirs=${libffi}/lib"
    ];
  }) else periodicd0;

in {
  inherit periodic-client;
  inherit periodicd;
}
