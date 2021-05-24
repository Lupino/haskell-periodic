{ compiler, extraOverrides ? (final: prev: { }) }:

self: super:
let
  metroRepo = super.fetchFromGitHub {
    owner = "Lupino";
    repo = "metro";
    rev = "84fc5a4b4513cdb8bad6df2182097bcc7bc12f93";
    sha256 = "0iw0vlzl98fc9iq4p893v1k3a1d86cpyvdzmqmkva5dgry6s8n3n";
  };
  overrides =
    final: prev:
    rec {
      metro = prev.callCabal2nix "metro" (metroRepo) {};
      metro-socket = prev.callCabal2nix "metro-socket" ((metroRepo) + /metro-socket) {};
      metro-transport-tls = prev.callCabal2nix "metro-transport-tls" ((metroRepo) + /metro-transport-tls) {};
      metro-transport-websockets = prev.callCabal2nix "metro-transport-websockets" ((metroRepo) + /metro-transport-websockets) {};
      metro-transport-xor = prev.callCabal2nix "metro-transport-xor" ((metroRepo) + /metro-transport-xor) {};
      map-io = prev.callCabal2nix "map-io" ((metroRepo) + /map-io) {};
    } // extraOverrides final prev;
in
{
  haskell =
    super.haskell // {
      packages = super.haskell.packages // {
        "${compiler}" =
          super.haskell.packages."${compiler}".override { inherit overrides; };
      };
    };
}
