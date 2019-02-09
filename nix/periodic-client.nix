{ mkDerivation, base, binary, boxes, byteable, bytestring
, data-default-class, exceptions, hslogger, http-types
, lifted-async, lifted-base, monad-control, periodic-common
, process-extras, resource-pool, scotty, stdenv, streaming-commons
, text, transformers, unix-time, warp, static ? false
}:
let config = import ./config.nix {static = static;};
in mkDerivation {
  pname = "periodic-client";
  version = "1.1.3.0";
  src = ../periodic-client;
  isLibrary = true;
  isExecutable = true;
  enableSharedExecutables = false;
  enableSharedLibraries = false;
  libraryHaskellDepends = [
    base byteable bytestring exceptions hslogger lifted-async
    lifted-base monad-control periodic-common resource-pool
    transformers
  ];
  executableHaskellDepends = [
    base binary boxes bytestring data-default-class http-types
    periodic-common process-extras scotty streaming-commons text
    unix-time warp
  ];
  homepage = "https://github.com/Lupino/haskell-periodic#readme";
  license = stdenv.lib.licenses.bsd3;
  configureFlags = config.configureFlags;
}
