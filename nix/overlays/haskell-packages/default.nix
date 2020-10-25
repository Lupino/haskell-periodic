{ compiler, extraOverrides ? (final: prev: { }) }:

self: super:
let
  overrides =
    final: prev:
    rec {
      metro = prev.callHackageDirect
        {
          pkg = "metro";
          ver = "0.1.0.4";
          sha256 = "13pbzly15gwgsr7sybbilivmmmc4yhl2rq590npld9p9mngy9hg2";
        } { };
      metro-socket = prev.callHackageDirect
        {
          pkg = "metro-socket";
          ver = "0.1.0.0";
          sha256 = "1mc4rb8mqavc07xvhkhkygvyal1prqc98mdwyrf9mfmnzsdy8ag1";
        } { };
      metro-transport-tls = prev.callHackageDirect
        {
          pkg = "metro-transport-tls";
          ver = "0.1.0.0";
          sha256 = "1prx6005m8422i5n601vdhbl7c2wygds6va5lq893rqdb364i26m";
        } { };
      metro-transport-websockets = prev.callHackageDirect
        {
          pkg = "metro-transport-websockets";
          ver = "0.1.0.0";
          sha256 = "0b5hg74lpyihn7516a9ja3jbrrwhh1fhidvi39zn1hrh50jhbh43";
        } { };
      metro-transport-xor = prev.callHackageDirect
        {
          pkg = "metro-transport-xor";
          ver = "0.1.0.0";
          sha256 = "1hl07x98sl6vx7xyf8wbsa2km37zngdpbrj1g0lg8bp9rk91fcmr";
        } { };
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
