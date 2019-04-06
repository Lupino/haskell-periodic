{ mkDerivation, async, base, binary, byteable, bytestring
, direct-sqlite, directory, filepath, hslogger
, mtl, network, periodic-common, unliftio
, psqueues, stdenv, stm, transformers
, unordered-containers, static ? false
}:
let config = import ./config.nix {static = static;};
in mkDerivation {
  pname = "periodicd";
  version = "1.1.4.0";
  src = ../periodic-server;
  isLibrary = true;
  isExecutable = true;
  enableSharedExecutables = false;
  enableSharedLibraries = false;
  libraryHaskellDepends = [
    async base binary byteable bytestring direct-sqlite directory
    filepath hslogger mtl network unliftio
    periodic-common psqueues stm transformers
    unordered-containers
  ];
  executableHaskellDepends = [ base bytestring periodic-common ];
  homepage = "https://github.com/Lupino/haskell-periodic#readme";
  license = stdenv.lib.licenses.bsd3;
  configureFlags = config.configureFlags;
}
