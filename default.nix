{ compiler-nix-name ? "ghc8105" }:
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
  pkgs = import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    sources.nixpkgs
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    haskellNix.nixpkgsArgs;
in pkgs.haskell-nix.cabalProject {
    # 'cleanGit' cleans a source directory based on the files known by git
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      src = ./.;
      name = "haskell-periodic";
    };
    index-state = "2021-06-30T00:00:00Z";
    index-sha256 = "0f6213f13984148dbf6ad865576e3a9ebb330751b30b49a7f6e02697865cbb01";
    plan-sha256 = if compiler-nix-name == "ghc8105" then "0lc2ypkkzwc6nf0s8zpgxz9zn8wdmnww3q8rp1h5zak9q2yxk8sb" else null;
    materialized = if compiler-nix-name == "ghc8105" then ./nix/materialized else null;
    # Specify the GHC version to use.
    compiler-nix-name = compiler-nix-name;
    modules = [(
       {pkgs, ...}: {
         packages.periodic-server.configureFlags = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isMusl [
           "--ghc-option=-optl=-lssl"
           "--ghc-option=-optl=-lcrypto"
           "--ghc-option=-optl=-L${pkgs.openssl.out}/lib"
         ];
      })];
  }
