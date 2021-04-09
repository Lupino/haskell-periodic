{ compiler ? "ghc8104" }:

let
  # overlays define packages we need to build our project
  pkgs = import <nixpkgs> {};
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;
  allOverlays = import nix/overlays;
  extraOverrides = final: prev:
    rec {
      periodic-common = prev.callCabal2nix "periodic-common" (gitIgnore [./.gitignore] ./periodic-common) {};
      periodic-server = prev.callCabal2nix "periodic-server" (gitIgnore [./.gitignore] ./periodic-server) {};
      periodic-client = prev.callCabal2nix "periodic-client" (gitIgnore [./.gitignore] ./periodic-client) {};
      periodic-client-exe = prev.callCabal2nix "periodic-client-exe" (gitIgnore [./.gitignore] ./periodic-client-exe) {};
    };
  overlays = [
    allOverlays.gitignore # helper to use gitignoreSource
    (allOverlays.haskell-packages { inherit compiler extraOverrides; })
  ];

  normalPkgs = import <nixpkgs> {inherit overlays;};

  haskellPackages = if compiler == "default"
                       then normalPkgs.haskellPackages
                       else normalPkgs.haskell.packages.${compiler};


in {
  inherit normalPkgs;
  periodic-server = haskellPackages.periodic-server;
  periodic-client-exe = haskellPackages.periodic-client-exe;
  shell = haskellPackages.shellFor {
    packages = p: [p.periodic-client-exe p.periodic-server];
    buildInputs = with normalPkgs; [
      haskellPackages.cabal-install
    ];
  };
}
