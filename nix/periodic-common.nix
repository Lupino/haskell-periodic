{ mkDerivation, base, binary, byteable, bytestring
, data-default-class, directory, entropy, exceptions, hashable
, hslogger, monad-control, mtl, network, pem, stdenv, stm, text
, tls, transformers, transformers-base, unix-time, crc
, unordered-containers, websockets, x509, x509-store
, x509-validation
}:
mkDerivation {
  pname = "periodic-common";
  version = "1.1.3.0";
  src = ../periodic-common;
  libraryHaskellDepends = [
    base binary byteable bytestring data-default-class directory
    entropy exceptions hashable hslogger monad-control mtl network pem
    stm text tls transformers transformers-base unix-time crc
    unordered-containers websockets x509 x509-store x509-validation
  ];
  homepage = "https://github.com/Lupino/haskell-periodic#readme";
  license = stdenv.lib.licenses.bsd3;
}
