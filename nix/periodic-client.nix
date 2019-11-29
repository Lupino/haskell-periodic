{ mkDerivation, base, binary, boxes, byteable, bytestring
, data-default-class, deepseq, hslogger, http-types
, periodic-common, process, resource-pool, scotty, stdenv
, streaming-commons, text, transformers, unix-time, unliftio, warp
}:
mkDerivation {
  pname = "periodic-client";
  version = "1.1.5.6";
  src = ../periodic-client;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base byteable bytestring hslogger periodic-common resource-pool
    transformers unliftio
  ];
  executableHaskellDepends = [
    base binary boxes bytestring data-default-class deepseq http-types
    periodic-common process scotty streaming-commons text unix-time
    unliftio warp
  ];
  homepage = "https://github.com/Lupino/haskell-periodic#readme";
  license = stdenv.lib.licenses.bsd3;
}
