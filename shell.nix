{ compiler ? "default" }:
(import ./default.nix { inherit compiler; }).shell
