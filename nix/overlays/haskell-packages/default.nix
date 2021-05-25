{ compiler, extraOverrides ? (final: prev: { }) }:

self: super:
let
  metroRepo = super.fetchFromGitHub {
    owner = "Lupino";
    repo = "metro";
    rev = "3cfb06e859df5e0e77e39a103967d14d382dec88";
    sha256 = "07h95bm84fchsnszfsnr0l6npwkvk9vi43hppi1iblsbcm6pnxs3";
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
