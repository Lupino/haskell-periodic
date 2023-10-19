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
      identifier = { name = "network-bsd"; version = "2.8.1.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto, Herbert Valerio Riedel";
      author = "";
      homepage = "https://github.com/haskell/network-bsd";
      url = "";
      synopsis = "POSIX network database (<netdb.h>) API";
      description = "This package provides Haskell bindings to the\nthe [POSIX network database (netdb.h) API](http://pubs.opengroup.org/onlinepubs/009696699/basedefs/netdb.h.html).\n\n=== Relationship to the @network@ package\n\nThe @network@ package version 2.* series provides \"Network.BSD\" but\nit is removed starting with @network@ version 3.0.\n\nThis package provides the \"Network.BSD\" module split off from the\n<https://hackage.haskell.org/package/network network package>.\n\nIf in addition to the @network@'s modules also \"Network.BSD\" is\nnecessary, add @network-bsd@ to your dependencies like so:\n\n> library\n>     build-depends: network     >= 2.7 && < 3.2\n>                  , network-bsd >= 2.7 && < 2.9\n\nI.e. you can control the version of the @network@ package\nindependently.\n\n__NOTE__: Starting with @network-bsd-2.8.1.0@ the APIs of @network@\nand @network-bsd@ evolve differently, and consequently the\nversioning doesn't match up anymore! Moreover, also starting with\nversion @network-bsd-2.8.1.0@ this package requires @network >= 3@\nin order to avoid module name clashes with @network < 3@'s\n\"Network.BSD\" module.\n\nHowever, @network-bsd-2.7.0.0@ and @network-bsd-2.8.0.0@ passes thru\nthe \"Network.BSD\" module from @network-2.7.*@ and @network-2.8.*@\nrespectively in a non-clashing way via Cabal's\n<https://www.haskell.org/cabal/users-guide/developing-packages.html#pkg-field-library-reexported-modules reexported-modules>\nfeature while ensuring a well-defined\n<https://pvp.haskell.org/ API versioning> of the observable API of\n@network-bsd@. This is why the example above supporting a wide range\nof @network@ versions works by including version 2.7.0.0 in the\nrequired version range of @network-bsd@.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          ];
        build-tools = [
          (hsPkgs.buildPackages.hsc2hs.components.exes.hsc2hs or (pkgs.buildPackages.hsc2hs or (errorHandler.buildToolDepError "hsc2hs:hsc2hs")))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/network-bsd-2.8.1.0.tar.gz";
      sha256 = "d94961ca15c42c798d19cde540ec12b25cc43435fb95e682399d6c1a02022d4e";
      });
    }) // {
    package-description-override = "cabal-version:  1.12\nname:           network-bsd\nversion:        2.8.1.0\nx-revision:     5\nlicense:        BSD3\nlicense-file:   LICENSE\nmaintainer:     Kazu Yamamoto, Herbert Valerio Riedel\nsynopsis:       POSIX network database (<netdb.h>) API\ndescription:\n  This package provides Haskell bindings to the\n  the [POSIX network database (netdb.h) API](http://pubs.opengroup.org/onlinepubs/009696699/basedefs/netdb.h.html).\n  .\n  === Relationship to the @network@ package\n  .\n  The @network@ package version 2.* series provides \"Network.BSD\" but\n  it is removed starting with @network@ version 3.0.\n  .\n  This package provides the \"Network.BSD\" module split off from the\n  <https://hackage.haskell.org/package/network network package>.\n  .\n  If in addition to the @network@'s modules also \"Network.BSD\" is\n  necessary, add @network-bsd@ to your dependencies like so:\n  .\n  > library\n  >     build-depends: network     >= 2.7 && < 3.2\n  >                  , network-bsd >= 2.7 && < 2.9\n  .\n  I.e. you can control the version of the @network@ package\n  independently.\n  .\n  __NOTE__: Starting with @network-bsd-2.8.1.0@ the APIs of @network@\n  and @network-bsd@ evolve differently, and consequently the\n  versioning doesn't match up anymore! Moreover, also starting with\n  version @network-bsd-2.8.1.0@ this package requires @network >= 3@\n  in order to avoid module name clashes with @network < 3@'s\n  \"Network.BSD\" module.\n  .\n  However, @network-bsd-2.7.0.0@ and @network-bsd-2.8.0.0@ passes thru\n  the \"Network.BSD\" module from @network-2.7.*@ and @network-2.8.*@\n  respectively in a non-clashing way via Cabal's\n  <https://www.haskell.org/cabal/users-guide/developing-packages.html#pkg-field-library-reexported-modules reexported-modules>\n  feature while ensuring a well-defined\n  <https://pvp.haskell.org/ API versioning> of the observable API of\n  @network-bsd@. This is why the example above supporting a wide range\n  of @network@ versions works by including version 2.7.0.0 in the\n  required version range of @network-bsd@.\n\ncategory:       Network\nbuild-type:     Simple\nhomepage:       https://github.com/haskell/network-bsd\nbug-reports:    https://github.com/haskell/network-bsd/issues\n\ntested-with:\n  GHC == 9.6.2\n  GHC == 9.4.5\n  GHC == 9.2.8\n  GHC == 9.0.2\n  GHC == 8.10.7\n  GHC == 8.8.4\n  GHC == 8.6.5\n  GHC == 8.4.4\n  GHC == 8.2.2\n  GHC == 8.0.2\n  GHC == 7.10.3\n  GHC == 7.8.4\n\nextra-source-files: CHANGELOG.md\n\nlibrary\n  default-language: Haskell2010\n  other-extensions: CPP, NondecreasingIndentation\n  exposed-modules: Network.BSD\n  build-depends: base    >= 4.7 && < 5\n               , deepseq >= 1.3.0.0 && < 1.6\n               -- NOTES on `network` depdendency\n               --\n               -- `network-bsd` heavily relies on `network`'s\n               -- exported CPP headers!  make sure that `network`\n               -- doesn't inadvertently break/change its CPP\n               -- headers before advertising support for new\n               -- versions of `network`.\n               --\n               -- Moreover, `network-bsd` reexports entities from\n               -- `network` so we need to be very conservative\n               -- with the bounds to avoid leaking through API\n               -- changes that aren't reflected in `network-bsd`'s\n               -- API version.\n               , network (>= 3.0.0.0 && < 3.0.2)\n                      || (>= 3.1.0.0 && < 3.2)\n  build-tools: hsc2hs >= 0.67 && < 0.69\n  ghc-options: -Wall\n\n  if os(windows) && impl(ghc >= 7.10)\n    -- See https://github.com/haskell/network/pull/362\n    cpp-options: -D_WIN32_WINNT=0x0600\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell/network-bsd.git\n";
    }