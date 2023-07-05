{ compiler-nix-name ? "ghc945" }:
let
  # Read in the Niv sources
  sources = import ./nix/sources.nix {};
  # If ./nix/sources.nix file is not found run:
  #   niv init
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Fetch the haskell.nix commit we have pinned with Niv
  haskellNix = import sources.haskellNix { };
  # If haskellNix is not found run:
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  overlays = haskellNix.overlays ++ [
    (self: super: {
        postgresql = super.postgresql.overrideAttrs (_: { doCheck = false;});
    })
  ];

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    sources.nixpkgs
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    (haskellNix.nixpkgsArgs // { inherit overlays; });
in pkgs.haskell-nix.cabalProject {
    # 'cleanGit' cleans a source directory based on the files known by git
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      src = ./.;
      name = "haskell-periodic";
    };
    index-state = "2023-07-04T00:00:00Z";
    index-sha256 = "43fcfb7e9eda6411c7ff71ba0bb70b784b46e3f179c8756c82d6b7bdf2439237";
    plan-sha256 = if compiler-nix-name == "ghc945" then "1ml0x6bnm85sa1zg81gkda299nzn3y1ndqmxxpsbszh68zj3p841" else null;
    materialized = if compiler-nix-name == "ghc945" then ./nix/materialized else null;
    # Specify the GHC version to use.
    compiler-nix-name = compiler-nix-name;
    modules = [(
       {pkgs, ...}: {
         packages.periodic-server.configureFlags = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isMusl [
           "--ghc-option=-optl=-lssl"
           "--ghc-option=-optl=-lcrypto"
           "--ghc-option=-optl=-lpgport"
           "--ghc-option=-optl=-lpgcommon"
           "--ghc-option=-optl=-L${pkgs.postgresql.lib.out}/lib"
           "--ghc-option=-optl=-L${pkgs.openssl.out}/lib"
         ];
      })];
  }
