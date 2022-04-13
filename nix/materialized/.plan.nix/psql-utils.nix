{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "psql-utils"; version = "0.2.0.1"; };
      license = "BSD-3-Clause";
      copyright = "MIT";
      maintainer = "lmjubuntu@gmail.com";
      author = "Li Meng Jun";
      homepage = "https://github.com/Lupino/yuntan-common/tree/master/psql-utils#readme";
      url = "";
      synopsis = "PostgreSQL Simple util tools.";
      description = "An easy way to use postgresql-simple library.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."resource-pool" or (errorHandler.buildDepError "resource-pool"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."postgresql-simple" or (errorHandler.buildDepError "postgresql-simple"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ];
        buildable = true;
        modules = [ "Database/PSQL/Config" "Database/PSQL/Types" ];
        hsSourceDirs = [ "src" ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "1";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "1";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/psql-utils; echo source root reset to $sourceRoot";
    }