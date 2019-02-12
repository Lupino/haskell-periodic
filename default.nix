{ static ? false, compiler ? "default" }:

let
  config = import ./nix/config.nix {static=static;};

  pkgs = config.pkgs;
  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};
  periodic-common = haskellPackages.callPackage ./nix/periodic-common.nix {
    crc =
      let crc = haskellPackages.crc;
      in pkgs.haskell.lib.dontCheck crc;
  };

in {
  periodic-client = haskellPackages.callPackage ./nix/periodic-client.nix {
    inherit periodic-common;
    static = static;
  };
  periodicd = haskellPackages.callPackage ./nix/periodicd.nix {
    inherit periodic-common;
    static = static;
  };
}
