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
      identifier = { name = "easy-file"; version = "0.2.3"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Kazu Yamamoto <kazu@iij.ad.jp>";
      homepage = "http://github.com/kazu-yamamoto/easy-file";
      url = "";
      synopsis = "Cross-platform File handling";
      description = "Cross-platform File handling for Unix\\/Mac\\/Windows";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        buildable = true;
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."easy-file" or (errorHandler.buildDepError "easy-file"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/easy-file-0.2.3.tar.gz";
      sha256 = "0437d8c2a5efe4bd703f2627495f28092f0256b3d2dab191c9d08a6351074cc9";
      });
    }) // {
    package-description-override = "Cabal-Version:          >= 1.10\nName:                   easy-file\nVersion:                0.2.3\nAuthor:                 Kazu Yamamoto <kazu@iij.ad.jp>\nMaintainer:             Kazu Yamamoto <kazu@iij.ad.jp>\nLicense:                BSD3\nLicense-File:           LICENSE\nSynopsis:               Cross-platform File handling\nDescription:            Cross-platform File handling for Unix\\/Mac\\/Windows\nHomepage:               http://github.com/kazu-yamamoto/easy-file\nCategory:               System\nBuild-Type:             Simple\n\nTested-With:\n  GHC == 9.6.0\n  GHC == 9.4.4\n  GHC == 9.2.7\n  GHC == 9.0.2\n  GHC == 8.10.7\n  GHC == 8.8.4\n  GHC == 8.6.5\n  GHC == 8.4.4\n  GHC == 8.2.2\n  GHC == 8.0.2\n  GHC == 7.10.3\n  GHC == 7.8.4\n  GHC == 7.6.3\n  GHC == 7.4.2\n  GHC == 7.0.4\n\nLibrary\n  Exposed-Modules:      System.EasyFile\n  Other-Modules:        System.EasyFile.FilePath\n                        System.EasyFile.Directory\n                        System.EasyFile.Missing\n  Build-Depends:\n      base >= 4 && < 5\n    , directory\n    , filepath\n    , time\n\n  if os(windows)\n    Build-Depends:      Win32\n  else\n    Build-Depends:      unix\n\n  Default-Language:     Haskell2010\n  GHC-Options:          -Wall\n  if impl(ghc >= 8)\n    GHC-Options:        -Wcompat\n\n\nTest-Suite test\n  Type:                 exitcode-stdio-1.0\n  Hs-Source-Dirs:       test\n  Main-Is:              Test.hs\n  Build-Depends:\n      base\n    , easy-file\n    , tasty\n    , tasty-hunit\n  Default-Language:     Haskell2010\n\nSource-Repository head\n  Type:                 git\n  Location:             https://github.com/kazu-yamamoto/easy-file.git\n";
    }