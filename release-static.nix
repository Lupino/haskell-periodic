let
  # We are using lts-15.13 stack resolver which uses ghc883 (cf https://www.stackage.org/lts-15.13)
  compiler = "ghc884";

  # pin nixpkgs for reproducible build
  nixpkgsVersion = import nix/nixpkgs-version.nix;
  nixpkgs =
    builtins.fetchTarball {
      url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsVersion.rev}.tar.gz";
      sha256 = nixpkgsVersion.tarballHash;
    };


  # overlays define packages we need to build our project
  allOverlays = import nix/overlays;
  overlays = [
    allOverlays.gitignore # helper to use gitignoreSource
    (allOverlays.haskell-packages { inherit compiler; })
  ];

  pkgs = import nixpkgs { inherit overlays; };

  # We define our packages by giving them names and a list of source files
  periodic-common = {
    name = "periodic-common";
    src = pkgs.lib.sourceFilesBySuffices (pkgs.gitignoreSource ./periodic-common)[ ".cabal" ".hs" "LICENSE" ];
  };
  periodic-client = {
    name = "periodic-client";
    src = pkgs.lib.sourceFilesBySuffices (pkgs.gitignoreSource ./periodic-client)[ ".cabal" ".hs" "LICENSE" ];
  };
  periodic-client-exe = {
    name = "periodic-client-exe";
    src = pkgs.lib.sourceFilesBySuffices (pkgs.gitignoreSource ./periodic-client-exe)[ ".cabal" ".hs" "LICENSE" ];
  };
  periodic-server = {
    name = "periodic-server";
    src = pkgs.lib.sourceFilesBySuffices (pkgs.gitignoreSource ./periodic-server)[ ".cabal" ".hs" "LICENSE" ];
  };

  # Some patches are unfortunately necessary to work with libpq
  patches = pkgs.callPackage nix/patches {};

  lib = pkgs.haskell.lib;

  # call our script which add our packages to nh2/static-haskell-nix project
  staticHaskellPackage = import nix/static-haskell-package.nix { inherit nixpkgs compiler patches allOverlays; } periodic-common periodic-client periodic-server periodic-client-exe;
in
rec {
  inherit nixpkgs pkgs;

  # if instead we want to generated a fully static executable we need:
  periodic-server-static = lib.justStaticExecutables (lib.dontCheck staticHaskellPackage.periodic-server);
  periodic-client-exe-static = lib.justStaticExecutables (lib.dontCheck staticHaskellPackage.periodic-client-exe);
}
