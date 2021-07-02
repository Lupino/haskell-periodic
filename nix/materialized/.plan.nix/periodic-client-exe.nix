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
      identifier = { name = "periodic-client-exe"; version = "1.1.7.3"; };
      license = "BSD-3-Clause";
      copyright = "MIT";
      maintainer = "lmjubuntu@gmail.com";
      author = "Li Meng Jun";
      homepage = "https://github.com/Lupino/haskell-periodic/tree/master/periodic-client-exe#readme";
      url = "";
      synopsis = "Periodic task system haskell client executables";
      description = "Periodic task system haskell client executables.";
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
      exes = {
        "periodic" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."periodic-client" or (errorHandler.buildDepError "periodic-client"))
            (hsPkgs."periodic-common" or (errorHandler.buildDepError "periodic-common"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."boxes" or (errorHandler.buildDepError "boxes"))
            (hsPkgs."unix-time" or (errorHandler.buildDepError "unix-time"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."metro" or (errorHandler.buildDepError "metro"))
            (hsPkgs."metro-socket" or (errorHandler.buildDepError "metro-socket"))
            (hsPkgs."metro-transport-xor" or (errorHandler.buildDepError "metro-transport-xor"))
            (hsPkgs."metro-transport-tls" or (errorHandler.buildDepError "metro-transport-tls"))
            (hsPkgs."metro-transport-websockets" or (errorHandler.buildDepError "metro-transport-websockets"))
            ];
          buildable = true;
          modules = [ "Paths_periodic_client_exe" ];
          hsSourceDirs = [ "app" ];
          mainPath = [ "periodic.hs" ];
          };
        "periodic-run" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."periodic-client" or (errorHandler.buildDepError "periodic-client"))
            (hsPkgs."periodic-common" or (errorHandler.buildDepError "periodic-common"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."metro" or (errorHandler.buildDepError "metro"))
            (hsPkgs."metro-socket" or (errorHandler.buildDepError "metro-socket"))
            (hsPkgs."metro-transport-xor" or (errorHandler.buildDepError "metro-transport-xor"))
            (hsPkgs."metro-transport-tls" or (errorHandler.buildDepError "metro-transport-tls"))
            (hsPkgs."metro-transport-websockets" or (errorHandler.buildDepError "metro-transport-websockets"))
            ];
          buildable = true;
          modules = [ "Paths_periodic_client_exe" ];
          hsSourceDirs = [ "app" ];
          mainPath = [ "periodic-run.hs" ];
          };
        "periodic-http-bridge" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."periodic-client" or (errorHandler.buildDepError "periodic-client"))
            (hsPkgs."periodic-common" or (errorHandler.buildDepError "periodic-common"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."scotty" or (errorHandler.buildDepError "scotty"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."metro" or (errorHandler.buildDepError "metro"))
            (hsPkgs."metro-socket" or (errorHandler.buildDepError "metro-socket"))
            (hsPkgs."metro-transport-xor" or (errorHandler.buildDepError "metro-transport-xor"))
            (hsPkgs."metro-transport-tls" or (errorHandler.buildDepError "metro-transport-tls"))
            (hsPkgs."metro-transport-websockets" or (errorHandler.buildDepError "metro-transport-websockets"))
            (hsPkgs."websockets" or (errorHandler.buildDepError "websockets"))
            ];
          buildable = true;
          modules = [ "Paths_periodic_client_exe" ];
          hsSourceDirs = [ "app" ];
          mainPath = [ "periodic-http-bridge.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../periodic-client-exe; }