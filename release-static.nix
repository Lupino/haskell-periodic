# Run using:
#
#     $(nix-build --no-link -A fullBuildScript)
{
  stack2nix-output-path ? "custom-stack2nix-output.nix",
}:
let
  compiler = "ghc865"; # matching stack.yaml

  # Pin static-haskell-nix version.
  static-haskell-nix =
      # Update this hash to use a different `static-haskell-nix` version:
      fetchTarball https://github.com/nh2/static-haskell-nix/archive/d1b20f35ec7d3761e59bd323bbe0cca23b3dfc82.tar.gz;

  # Pin nixpkgs version
  # By default to the one `static-haskell-nix` provides, but you may also give
  # your own as long as it has the necessary patches, using e.g.
  pkgs = import "${static-haskell-nix}/nixpkgs.nix";

  stack2nix-script = import "${static-haskell-nix}/static-stack2nix-builder/stack2nix-script.nix" {
    inherit pkgs;
    stack-project-dir = toString ./.; # where stack.yaml is
    hackageSnapshot = "2020-03-20T00:00:00Z"; # pins e.g. extra-deps without hashes or revisions
  };

  periodicd-builder = import "${static-haskell-nix}/static-stack2nix-builder/default.nix" {
    normalPkgs = pkgs;
    cabalPackageName = "periodicd";
    inherit compiler stack2nix-output-path;
    # disableOptimization = true; # for compile speed
  };

  periodic-client-exe-builder = import "${static-haskell-nix}/static-stack2nix-builder/default.nix" {
    normalPkgs = pkgs;
    cabalPackageName = "periodic-client-exe";
    inherit compiler stack2nix-output-path;
    # disableOptimization = true; # for compile speed
  };

  # Full invocation, including pinning `nix` version itself.
  fullBuildScript = pkgs.writeScript "stack2nix-and-build-script.sh" ''
    #!/usr/bin/env bash
    set -eu -o pipefail
    STACK2NIX_OUTPUT_PATH=$(${stack2nix-script})
    export NIX_PATH=nixpkgs=${pkgs.path}
    ${pkgs.nix}/bin/nix-build -A periodicd -A periodic-client-exe --argstr stack2nix-output-path "$STACK2NIX_OUTPUT_PATH" "$@"
  '';

  periodicd =
    periodicd-builder.haskell-static-nix_output.pkgs.staticHaskellHelpers.addStaticLinkerFlagsWithPkgconfig
      periodicd-builder.static_package
      (with periodicd-builder.haskell-static-nix_output.pkgs; [ openssl postgresql ])
      "--libs libpq";

in
  {
    periodic-client-exe = periodic-client-exe-builder.static_package;
    inherit periodicd;
    inherit fullBuildScript;
    # For debugging:
    inherit stack2nix-script;
    inherit periodic-client-exe-builder;
    inherit periodicd-builder;
  }
