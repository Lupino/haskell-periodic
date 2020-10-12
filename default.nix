{ compiler ? "default" }:

let
  pkgs = import <nixpkgs> { inherit config; };
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;

  metroRepo = pkgs.fetchFromGitHub {
    owner = "Lupino";
    repo = "metro";
    rev = "ce81fc9b0da294e597b55dfaad199d9fa47b8c1d";
    sha256 = "1l2isigv5qkg8ps7izzkcrh5hkwmpvsxvqsfbvycsacwhnmin9jq";
  };

  config = {
    packageOverrides = super: let self = super.pkgs; in rec {
      haskell = super.haskell // {
        packageOverrides = self: super: {
          periodic-common = super.callCabal2nix "periodic-common" (gitIgnore [./.gitignore] ./periodic-common) {};
          periodic-server = super.callCabal2nix "periodic-server" (gitIgnore [./.gitignore] ./periodic-server) {};
          periodic-client = super.callCabal2nix "periodic-client" (gitIgnore [./.gitignore] ./periodic-client) {};
          periodic-client-exe = super.callCabal2nix "periodic-client-exe" (gitIgnore [./.gitignore] ./periodic-client-exe) {};
          metro = super.callCabal2nix "metro" (metroRepo) {};
          metro-socket = super.callCabal2nix "metro-socket" ((metroRepo) + /metro-socket) {};
          metro-transport-tls = super.callCabal2nix "metro-transport-tls" ((metroRepo) + /metro-transport-tls) {};
          metro-transport-websockets = super.callCabal2nix "metro-transport-websockets" ((metroRepo) + /metro-transport-websockets) {};
          metro-transport-xor = super.callCabal2nix "metro-transport-xor" ((metroRepo) + /metro-transport-xor) {};
        };
      };
    };
  };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};


in {
  inherit pkgs;
  periodic-server = haskellPackages.periodic-server;
  periodic-client-exe = haskellPackages.periodic-client-exe;
  shell = haskellPackages.shellFor {
    packages = p: [p.periodic-client-exe p.periodic-server];
    buildInputs = with pkgs; [
      haskellPackages.cabal-install
    ];
  };
}
