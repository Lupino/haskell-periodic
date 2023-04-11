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
      specVersion = "1.8";
      identifier = { name = "time-manager"; version = "0.0.0"; };
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
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/time-manager-0.0.0.tar.gz";
      sha256 = "90a616ed20b2119bb64f78f84230b6798cde22a35e87bc8d9ee08cdf1d90fcdb";
      });
    }) // {
    package-description-override = "Name:                time-manager\r\nVersion:             0.0.0\r\nx-revision: 1\r\nSynopsis:            Scalable timer\r\nLicense:             MIT\r\nLicense-file:        LICENSE\r\nAuthor:              Michael Snoyman and Kazu Yamamoto\r\nMaintainer:          kazu@iij.ad.jp\r\nHomepage:            http://github.com/yesodweb/wai\r\nCategory:            System\r\nBuild-Type:          Simple\r\nCabal-Version:       >=1.8\r\nStability:           Stable\r\nDescription:         Scalable timer functions provided by a timer manager.\r\n\r\nLibrary\r\n  Build-Depends:     base                      >= 4.8        && < 5\r\n                   , auto-update               >= 0.1.3\r\n  Exposed-modules:   System.TimeManager\r\n  Ghc-Options:       -Wall\r\n";
    }