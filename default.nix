{ compiler-nix-name ? "ghc948" }:
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
    index-state = "2024-06-30T00:00:00Z";
    index-sha256 = "5b1b317db9c6c4626fbe7a827a2b51f9fba30edd0c594e11abbd83c4edc45fb0";
    sha256map = import ./nix/sha256map.nix;
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
