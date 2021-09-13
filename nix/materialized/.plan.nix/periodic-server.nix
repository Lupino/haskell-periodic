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
      identifier = { name = "periodic-server"; version = "1.1.7.3"; };
      license = "BSD-3-Clause";
      copyright = "MIT";
      maintainer = "lmjubuntu@gmail.com";
      author = "Li Meng Jun";
      homepage = "https://github.com/Lupino/haskell-periodic/tree/master/periodic-server#readme";
      url = "";
      synopsis = "Periodic task system haskell server";
      description = "Periodic task system haskell server and command peridoicd.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.md" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."periodic-common" or (errorHandler.buildDepError "periodic-common"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hslogger" or (errorHandler.buildDepError "hslogger"))
          (hsPkgs."byteable" or (errorHandler.buildDepError "byteable"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."direct-sqlite" or (errorHandler.buildDepError "direct-sqlite"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."metro" or (errorHandler.buildDepError "metro"))
          (hsPkgs."map-io" or (errorHandler.buildDepError "map-io"))
          (hsPkgs."entropy" or (errorHandler.buildDepError "entropy"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."metro-socket" or (errorHandler.buildDepError "metro-socket"))
          (hsPkgs."psql-utils" or (errorHandler.buildDepError "psql-utils"))
          (hsPkgs."unix-time" or (errorHandler.buildDepError "unix-time"))
          ];
        buildable = true;
        modules = [
          "Periodic/Server/Scheduler"
          "Periodic/Server/GrabQueue"
          "Periodic/Server/FuncStat"
          "Periodic/Server/Persist"
          "Periodic/Server/Persist/SQLite"
          "Periodic/Server/Persist/PSQL"
          "Periodic/Server/Persist/Memory"
          "Periodic/Server/Persist/Cache"
          "Periodic/Server/Client"
          "Periodic/Server/Types"
          "Periodic/Server/Hook"
          "Periodic/Server"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "periodicd" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."periodic-server" or (errorHandler.buildDepError "periodic-server"))
            (hsPkgs."periodic-common" or (errorHandler.buildDepError "periodic-common"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."hslogger" or (errorHandler.buildDepError "hslogger"))
            (hsPkgs."metro" or (errorHandler.buildDepError "metro"))
            (hsPkgs."metro-socket" or (errorHandler.buildDepError "metro-socket"))
            (hsPkgs."metro-transport-xor" or (errorHandler.buildDepError "metro-transport-xor"))
            (hsPkgs."metro-transport-tls" or (errorHandler.buildDepError "metro-transport-tls"))
            (hsPkgs."metro-transport-websockets" or (errorHandler.buildDepError "metro-transport-websockets"))
            ];
          buildable = true;
          modules = [ "Paths_periodic_server" ];
          hsSourceDirs = [ "app" ];
          mainPath = [ "periodicd.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../periodic-server; }