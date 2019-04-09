{ mkDerivation, base, binary, byteable, bytestring
, data-default-class, directory, entropy, hashable, hslogger, mtl
, network, pem, stdenv, stm, text, tls, transformers, unix-time
, unliftio, unordered-containers, vector, websockets, x509
, x509-store, x509-validation
}:
mkDerivation {
  pname = "periodic-common";
  version = "1.1.5.0";
  src = ../periodic-common;
  libraryHaskellDepends = [
    base binary byteable bytestring data-default-class directory
    entropy hashable hslogger mtl network pem stm text tls transformers
    unix-time unliftio unordered-containers vector websockets x509
    x509-store x509-validation
  ];
  homepage = "https://github.com/Lupino/haskell-periodic#readme";
  license = stdenv.lib.licenses.bsd3;
}
