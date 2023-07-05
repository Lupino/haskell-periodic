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
    flags = { tagged = true; };
    package = {
      specVersion = "1.12";
      identifier = { name = "assoc"; version = "1.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      homepage = "";
      url = "";
      synopsis = "swap and assoc: Symmetric and Semigroupy Bifunctors";
      description = "Provides generalisations of\n@swap :: (a,b) -> (b,a)@ and\n@assoc :: ((a,b),c) -> (a,(b,c))@\nto\n@Bifunctor@s supporting similar operations (e.g. @Either@, @These@).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (flags.tagged) (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."bifunctor-classes-compat" or (errorHandler.buildDepError "bifunctor-classes-compat"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/assoc-1.1.tar.gz";
      sha256 = "7aa2e6548b3d9d49a286ac20639479aaf6c47a1446113ed784d98737c5f60df4";
      });
    }) // {
    package-description-override = "cabal-version:      1.12\nname:               assoc\nversion:            1.1\nlicense:            BSD3\nlicense-file:       LICENSE\nsynopsis:           swap and assoc: Symmetric and Semigroupy Bifunctors\ncategory:           Data\ndescription:\n  Provides generalisations of\n  @swap :: (a,b) -> (b,a)@ and\n  @assoc :: ((a,b),c) -> (a,(b,c))@\n  to\n  @Bifunctor@s supporting similar operations (e.g. @Either@, @These@).\n\nauthor:             Oleg Grenrus <oleg.grenrus@iki.fi>\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\nbuild-type:         Simple\nextra-source-files: CHANGELOG.md\ntested-with:\n    GHC ==7.0.4\n     || ==7.2.2\n     || ==7.4.2\n     || ==7.6.3\n     || ==7.8.4\n     || ==7.10.3\n     || ==8.0.2\n     || ==8.2.2\n     || ==8.4.4\n     || ==8.6.5\n     || ==8.8.4\n     || ==8.10.7\n     || ==9.0.2\n     || ==9.2.7\n     || ==9.4.4\n     || ==9.6.1\n  , GHCJS ==8.4\n\nsource-repository head\n  type:     git\n  location: https://github.com/phadej/assoc.git\n\nflag tagged\n  default:     True\n  manual:      True\n  description:\n    You can disable the use of the `tagged` package using `-f-tagged`.\n    .\n    Disabing this is an unsupported configuration, but it may be useful for accelerating builds in sandboxes for expert users.\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs:   src\n  build-depends:    base >=4.3 && <4.19\n\n  if flag(tagged)\n    build-depends: tagged >=0.8.6 && <0.9\n\n  if !impl(ghc >=8.0)\n    build-depends: bifunctor-classes-compat >=0.1 && <0.2\n\n  exposed-modules:\n    Data.Bifunctor.Assoc\n    Data.Bifunctor.Swap\n\n  other-extensions: TypeFamilies\n";
    }