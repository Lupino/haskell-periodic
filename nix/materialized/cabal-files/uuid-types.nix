{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.12";
      identifier = { name = "uuid-types"; version = "1.0.5.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2017-2018 Herbert Valerio Riedel\n(c) 2008-2014 Antoine Latter";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Antoine Latter";
      homepage = "https://github.com/haskell-hvr/uuid";
      url = "";
      synopsis = "Type definitions for Universally Unique Identifiers";
      description = "This library contains type definitions for\n<https://en.wikipedia.org/wiki/UUID Universally Unique Identifiers (UUID)>\n(as specified in\n<http://tools.ietf.org/html/rfc4122 RFC 4122>)\nand basic conversion functions.\n\nSee also the <https://hackage.haskell.org/package/uuid 'uuid' package>\nproviding a high-level API for managing the different UUID versions.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ];
        buildable = true;
        };
      tests = {
        "testuuid" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."uuid-types" or (errorHandler.buildDepError "uuid-types"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.4")) (hsPkgs."ghc-byteorder" or (errorHandler.buildDepError "ghc-byteorder"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/uuid-types-1.0.5.1.tar.gz";
      sha256 = "0bec6d6982b3c92bfa5eab1d213be2d4b6696b9a2c3a1f1f05812dc3762dca2c";
      });
    }) // {
    package-description-override = "cabal-version:      1.12\nname:               uuid-types\nversion:            1.0.5.1\ncopyright:\n  (c) 2017-2018 Herbert Valerio Riedel\n  (c) 2008-2014 Antoine Latter\n\nauthor:             Antoine Latter\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\nlicense:            BSD3\nlicense-file:       LICENSE\ncategory:           Data\nbuild-type:         Simple\ntested-with:\n  GHC ==7.4.2\n   || ==7.6.3\n   || ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.4\n   || ==8.8.4\n   || ==8.10.7\n   || ==9.0.2\n   || ==9.2.8\n   || ==9.4.7\n   || ==9.6.3\n   || ==9.8.1\n\nsynopsis:           Type definitions for Universally Unique Identifiers\ndescription:\n  This library contains type definitions for\n  <https://en.wikipedia.org/wiki/UUID Universally Unique Identifiers (UUID)>\n  (as specified in\n  <http://tools.ietf.org/html/rfc4122 RFC 4122>)\n  and basic conversion functions.\n  .\n  See also the <https://hackage.haskell.org/package/uuid 'uuid' package>\n  providing a high-level API for managing the different UUID versions.\n\nhomepage:           https://github.com/haskell-hvr/uuid\nbug-reports:        https://github.com/haskell-hvr/uuid/issues\nextra-source-files: ChangeLog.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell-hvr/uuid.git\n  subdir:   uuid-types\n\nlibrary\n  build-depends:\n      base              >=4.5     && <5\n    , binary            >=0.5.1.0 && <0.9\n    , bytestring        >=0.9.2.1 && <0.13\n    , deepseq           >=1.3.0.0 && <1.6\n    , hashable          >=1.2.7.0 && <1.5\n    , random            >=1.1     && <1.3\n    , template-haskell  >=2.7.0.0 && <2.22\n    , text              >=1.2.3.0 && <1.3  || >=2.0 && <2.2\n\n  exposed-modules:  Data.UUID.Types\n\n  -- Exposed for companion projects; *NOT* part of the official API:\n  exposed-modules:\n    Data.UUID.Types.Internal\n    Data.UUID.Types.Internal.Builder\n\n  default-language: Haskell2010\n  other-extensions:\n    DeriveDataTypeable\n    TypeFamilies\n\n  ghc-options:      -Wall\n  hs-source-dirs:   src\n\ntest-suite testuuid\n  type:             exitcode-stdio-1.0\n  main-is:          TestUUID.hs\n  hs-source-dirs:   tests\n  default-language: Haskell2010\n  other-extensions: ViewPatterns\n  ghc-options:      -Wall\n\n  -- inherited constraints\n  build-depends:\n      base\n    , binary\n    , bytestring\n    , template-haskell\n    , uuid-types\n\n  -- deps w/o inherited constraints\n  build-depends:\n      QuickCheck        >=2.14.2  && <2.15\n    , tasty             >=1.4.0.1 && <1.6\n    , tasty-hunit       >=0.10    && <0.11\n    , tasty-quickcheck  >=0.10    && <0.11\n\n  if !impl(ghc >=8.4)\n    build-depends: ghc-byteorder >=4.11 && <4.12\n";
    }