{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let
  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  periodic-common = haskellPackages.callPackage ./nix/periodic-common.nix {};

in {
  periodic-client = haskellPackages.callPackage ./nix/periodic-client.nix {
    inherit periodic-common;
  };
  periodicd = haskellPackages.callPackage ./nix/periodicd.nix {
    inherit periodic-common;
  };
}
