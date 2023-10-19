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
      identifier = { name = "time-manager"; version = "0.0.1"; };
      license = "MIT";
      copyright = "";
      maintainer = "kazu@iij.ad.jp";
      author = "Michael Snoyman and Kazu Yamamoto";
      homepage = "http://github.com/yesodweb/wai";
      url = "";
      synopsis = "Scalable timer";
      description = "Scalable timer functions provided by a timer manager.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/time-manager-0.0.1.tar.gz";
      sha256 = "07492523fabc69b8ec308f6276cc93df3dd060c91f23df2ac8a1f56d331ea05b";
      });
    }) // {
    package-description-override = "Name:                time-manager\nVersion:             0.0.1\nSynopsis:            Scalable timer\nLicense:             MIT\nLicense-file:        LICENSE\nAuthor:              Michael Snoyman and Kazu Yamamoto\nMaintainer:          kazu@iij.ad.jp\nHomepage:            http://github.com/yesodweb/wai\nCategory:            System\nBuild-Type:          Simple\nCabal-Version:       >=1.10\nStability:           Stable\nDescription:         Scalable timer functions provided by a timer manager.\n\nLibrary\n  Build-Depends:     base                      >= 4.12       && < 5\n                   , auto-update\n                   , unliftio\n  Default-Language:  Haskell2010\n  Exposed-modules:   System.TimeManager\n  Ghc-Options:       -Wall\n";
    }