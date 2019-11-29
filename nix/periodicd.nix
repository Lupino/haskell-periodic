{ mkDerivation, async, base, binary, byteable, bytestring
, direct-sqlite, filepath, hslogger, mtl, network, periodic-common
, psqueues, stdenv, stm, transformers, unliftio
, unordered-containers
}:
mkDerivation {
  pname = "periodicd";
  version = "1.1.5.6";
  src = ../periodic-server;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base binary byteable bytestring direct-sqlite filepath
    hslogger mtl network periodic-common psqueues stm transformers
    unliftio unordered-containers
  ];
  executableHaskellDepends = [
    base bytestring periodic-common unliftio
  ];
  homepage = "https://github.com/Lupino/haskell-periodic#readme";
  license = stdenv.lib.licenses.bsd3;
}
