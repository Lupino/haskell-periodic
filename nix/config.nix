{ nixpkgs ? import <nixpkgs> {}, static ? false}:

let
  pkgs = if static then nixpkgs.pkgsMusl
                   else nixpkgs.pkgs;
  configureFlags = if static then [
    "--ghc-option=-optl=-static"
    "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
    "--extra-lib-dirs=${pkgs.zlib.static}/lib"
  ] else [];
in {
  configureFlags = configureFlags;
  pkgs = pkgs;
}
