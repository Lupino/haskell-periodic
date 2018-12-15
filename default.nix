{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let
  pkgs = nixpkgs.pkgsMusl;
  # inherit (nixpkgs) pkgs;

  configureFlags = [
    "--ghc-option=-optl=-static"
    "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
    "--extra-lib-dirs=${pkgs.zlib.static}/lib"
  ] ++ pkgs.lib.optionals (false) [
    "--disable-executable-stripping"
  ];

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  periodic-common = haskellPackages.callPackage ./nix/periodic-common.nix {};

in {
  periodic-client = haskellPackages.callPackage ./nix/periodic-client.nix {
    inherit periodic-common;
    inherit configureFlags;
  };
  periodicd = haskellPackages.callPackage ./nix/periodicd.nix {
    inherit periodic-common;
    inherit configureFlags;
  };
}
