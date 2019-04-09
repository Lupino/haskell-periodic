{ mkDerivation, base, binary, boxes, byteable, bytestring
, data-default-class, hslogger, http-types, periodic-common
, process-extras, resource-pool, scotty, stdenv, streaming-commons
, text, transformers, unix-time, unliftio, warp
}:
mkDerivation {
  pname = "periodic-client";
  version = "1.1.5.0";
  src = ../periodic-client;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base byteable bytestring hslogger periodic-common resource-pool
    transformers unliftio
  ];
  executableHaskellDepends = [
    base binary boxes bytestring data-default-class http-types
    periodic-common process-extras scotty streaming-commons text
    unix-time warp
  ];
  homepage = "https://github.com/Lupino/haskell-periodic#readme";
  license = stdenv.lib.licenses.bsd3;
}
