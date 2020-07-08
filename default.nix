{ compiler ? "default" }:

let
  pkgs = import <nixpkgs> { inherit config; };
  gitIgnore = pkgs.nix-gitignore.gitignoreSourcePure;

  config = {
    packageOverrides = super: let self = super.pkgs; in rec {
      haskell = super.haskell // {
        packageOverrides = self: super: {
          periodic-common = super.callCabal2nix "periodic-common" (gitIgnore [./.gitignore] ./periodic-common) {};
          periodic-server = super.callCabal2nix "periodic-server" (gitIgnore [./.gitignore] ./periodic-server) {};
          periodic-client = super.callCabal2nix "periodic-client" (gitIgnore [./.gitignore] ./periodic-client) {};
          periodic-client-exe = super.callCabal2nix "periodic-client-exe" (gitIgnore [./.gitignore] ./periodic-client-exe) {};
          metro = super.callCabal2nix "metro" (
            pkgs.fetchFromGitHub {
              owner = "Lupino";
              repo = "metro";
              rev = "f0f3eed1e792ffe624e32c477778a42e444ff472";
              sha256 = "00fvkk4hyvqkd8dipmc69mc94raapbv0b3lxnhi618myr6lb64si";
            }) {};
          metro-socket = super.callCabal2nix "metro-socket" ((
            pkgs.fetchFromGitHub {
              owner = "Lupino";
              repo = "metro";
              rev = "f0f3eed1e792ffe624e32c477778a42e444ff472";
              sha256 = "00fvkk4hyvqkd8dipmc69mc94raapbv0b3lxnhi618myr6lb64si";
            }) + /metro-socket) {};
          metro-transport-tls = super.callCabal2nix "metro-transport-tls" ((
            pkgs.fetchFromGitHub {
              owner = "Lupino";
              repo = "metro";
              rev = "f0f3eed1e792ffe624e32c477778a42e444ff472";
              sha256 = "00fvkk4hyvqkd8dipmc69mc94raapbv0b3lxnhi618myr6lb64si";
            }) + /metro-transport-tls) {};
          metro-transport-websockets = super.callCabal2nix "metro-transport-websockets" ((
            pkgs.fetchFromGitHub {
              owner = "Lupino";
              repo = "metro";
              rev = "f0f3eed1e792ffe624e32c477778a42e444ff472";
              sha256 = "00fvkk4hyvqkd8dipmc69mc94raapbv0b3lxnhi618myr6lb64si";
            }) + /metro-transport-websockets) {};
          metro-transport-xor = super.callCabal2nix "metro-transport-xor" ((
            pkgs.fetchFromGitHub {
              owner = "Lupino";
              repo = "metro";
              rev = "f0f3eed1e792ffe624e32c477778a42e444ff472";
              sha256 = "00fvkk4hyvqkd8dipmc69mc94raapbv0b3lxnhi618myr6lb64si";
            }) + /metro-transport-xor) {};
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
