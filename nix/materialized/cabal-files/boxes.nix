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
      specVersion = "1.10";
      identifier = { name = "boxes"; version = "0.1.5"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "David Feuer <David.Feuer@gmail.com>";
      author = "Brent Yorgey";
      homepage = "";
      url = "";
      synopsis = "2D text pretty-printing library";
      description = "A pretty-printing library for laying out text in\ntwo dimensions, using a simple box model.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."split" or (errorHandler.buildDepError "split"))
          ];
        buildable = true;
        };
      tests = {
        "test-boxes" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."split" or (errorHandler.buildDepError "split"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/boxes-0.1.5.tar.gz";
      sha256 = "38e1782e8a458f342a0acbb74af8f55cb120756bc3af7ee7220d955812af56c3";
      });
    }) // {
    package-description-override = "name:                boxes\nversion:             0.1.5\nsynopsis:            2D text pretty-printing library\ndescription:         A pretty-printing library for laying out text in\n                     two dimensions, using a simple box model.\ncategory:            Text\nlicense:             BSD3\nlicense-file:        LICENSE\nextra-source-files:  CHANGES, README.md, include/boxes.h\n\nauthor:              Brent Yorgey\nmaintainer:          David Feuer <David.Feuer@gmail.com>\nbuild-type:          Simple\ncabal-version:       >= 1.9.2\n-- Minimum Cabal version supporting test suites\n\nlibrary\n  build-depends:     base >= 3 && < 5, split >=0.2 && <0.3\n  exposed-modules:   Text.PrettyPrint.Boxes\n  extensions:        CPP\n  include-dirs:      include\n\nTest-Suite test-boxes\n  type:              exitcode-stdio-1.0\n  main-is:           Text/PrettyPrint/Tests.hs\n  build-depends:     base >= 3 && < 5, split >=0.2 && <0.3, QuickCheck\n  extensions:        CPP\n  include-dirs:      include\n  cpp-options:       -DTESTING\n  other-modules:     Text.PrettyPrint.Boxes\n-- Export some internals so the tests can get at them\n\nsource-repository head\n  type: git\n  location: https://github.com/treeowl/boxes.git\n";
    }