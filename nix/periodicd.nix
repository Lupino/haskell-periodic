{ mkDerivation, async, base, binary, byteable, bytestring
, direct-sqlite, directory, exceptions, filepath, hslogger
, lifted-async, monad-control, mtl, network, periodic-common
, psqueues, stdenv, stm, transformers, transformers-base
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
    exceptions filepath hslogger lifted-async monad-control mtl network
    periodic-common psqueues stm transformers transformers-base
    unordered-containers
  ];
  executableHaskellDepends = [ base bytestring periodic-common ];
  homepage = "https://github.com/Lupino/haskell-periodic#readme";
  license = stdenv.lib.licenses.bsd3;
  configureFlags = config.configureFlags;
}
