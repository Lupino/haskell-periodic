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
      specVersion = "2.4";
      identifier = { name = "resource-pool"; version = "0.4.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "andrzej@rybczak.net";
      author = "Andrzej Rybczak, Bryan O'Sullivan";
      homepage = "";
      url = "";
      synopsis = "A high-performance striped resource pooling implementation";
      description = "A high-performance striped pooling abstraction for managing\nflexibly-sized collections of resources such as database\nconnections.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/resource-pool-0.4.0.0.tar.gz";
      sha256 = "8c0d783e3e75788d65f79157309132999f1bbb70684bacc2ea5dd18f904ae9b1";
      });
    }) // {
    package-description-override = "cabal-version:       2.4\nbuild-type:          Simple\nname:                resource-pool\nversion:             0.4.0.0\nlicense:             BSD-3-Clause\nlicense-file:        LICENSE\ncategory:            Data, Database, Network\nmaintainer:          andrzej@rybczak.net\nauthor:              Andrzej Rybczak, Bryan O'Sullivan\n\nsynopsis:            A high-performance striped resource pooling implementation\n\ndescription: A high-performance striped pooling abstraction for managing\n             flexibly-sized collections of resources such as database\n             connections.\n\ntested-with: GHC ==8.4.4 || ==8.6.5 || ==8.8.4 || ==8.10.7 || ==9.0.2 || ==9.2.5\n              || ==9.4.3\n\nextra-doc-files:\n  CHANGELOG.md\n  README.md\n\nbug-reports: https://github.com/scrive/pool/issues\nsource-repository head\n  type:     git\n  location: https://github.com/scrive/pool.git\n\nlibrary\n  hs-source-dirs:  src\n\n  exposed-modules: Data.Pool\n                   Data.Pool.Internal\n                   Data.Pool.Introspection\n\n  build-depends: base >= 4.11 && < 5\n               , hashable >= 1.1.0.0\n               , primitive >= 0.7\n               , time\n\n  ghc-options: -Wall -Wcompat\n\n  default-language: Haskell2010\n\n  default-extensions: DeriveGeneric\n                    , LambdaCase\n                    , RankNTypes\n                    , TypeApplications\n";
    }