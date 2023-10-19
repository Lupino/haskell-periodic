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
      identifier = { name = "these"; version = "1.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "C. McCann, Oleg Grenrus";
      homepage = "https://github.com/haskellari/these";
      url = "";
      synopsis = "An either-or-both data type.";
      description = "This package provides a data type @These a b@ which can hold a value of either\ntype or values of each type. This is usually thought of as an \"inclusive or\"\ntype (contrasting @Either a b@ as \"exclusive or\") or as an \"outer join\" type\n(contrasting @(a, b)@ as \"inner join\").\n\n@\ndata These a b = This a | That b | These a b\n@\n\nSince version 1, this package was split into parts:\n\n* <https://hackage.haskell.org/package/semialign semialign> For @Align@ and @Zip@ type-classes.\n\n* <https://hackage.haskell.org/package/semialign-indexed semialign-indexed> For @SemialignWithIndex@ class, providing @ialignWith@ and @izipWith@.\n\n* <https://hackage.haskell.org/package/these-lens these-lens> For lens combinators.\n\n* <http://hackage.haskell.org/package/monad-chronicle monad-chronicle> For transformers variant of @These@.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."assoc" or (errorHandler.buildDepError "assoc"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.5") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "9.6")) (hsPkgs."foldable1-classes-compat" or (errorHandler.buildDepError "foldable1-classes-compat"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.2")) (hsPkgs."bifunctor-classes-compat" or (errorHandler.buildDepError "bifunctor-classes-compat"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) ([
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "7.10") (hsPkgs."transformers" or (errorHandler.buildDepError "transformers")));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/these-1.2.tar.gz";
      sha256 = "a8ed7174b7f790764360f48aab72fea382a6093ed369c7bc6881fae1abacd4b8";
      });
    }) // {
    package-description-override = "cabal-version:      >=1.10\nname:               these\nversion:            1.2\nx-revision:         1\nsynopsis:           An either-or-both data type.\nhomepage:           https://github.com/haskellari/these\nlicense:            BSD3\nlicense-file:       LICENSE\nauthor:             C. McCann, Oleg Grenrus\nmaintainer:         Oleg Grenrus <oleg.grenrus@iki.fi>\ncategory:           Data, These\nbuild-type:         Simple\nextra-source-files: CHANGELOG.md\ndescription:\n  This package provides a data type @These a b@ which can hold a value of either\n  type or values of each type. This is usually thought of as an \"inclusive or\"\n  type (contrasting @Either a b@ as \"exclusive or\") or as an \"outer join\" type\n  (contrasting @(a, b)@ as \"inner join\").\n  .\n  @\n  data These a b = This a | That b | These a b\n  @\n  .\n  Since version 1, this package was split into parts:\n  .\n  * <https://hackage.haskell.org/package/semialign semialign> For @Align@ and @Zip@ type-classes.\n  .\n  * <https://hackage.haskell.org/package/semialign-indexed semialign-indexed> For @SemialignWithIndex@ class, providing @ialignWith@ and @izipWith@.\n  .\n  * <https://hackage.haskell.org/package/these-lens these-lens> For lens combinators.\n  .\n  * <http://hackage.haskell.org/package/monad-chronicle monad-chronicle> For transformers variant of @These@.\n\ntested-with:\n    GHC ==7.4.2\n     || ==7.6.3\n     || ==7.8.4\n     || ==7.10.3\n     || ==8.0.2\n     || ==8.2.2\n     || ==8.4.4\n     || ==8.6.5\n     || ==8.8.4\n     || ==8.10.7\n     || ==9.0.2\n     || ==9.2.8\n     || ==9.4.7\n     || ==9.6.3\n     || ==9.8.1\n  , GHCJS ==8.4\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskellari/these.git\n  subdir:   these\n\nlibrary\n  default-language:         Haskell2010\n  ghc-options:              -Wall\n\n  if impl(ghc >=8.0)\n    ghc-options: -Wno-trustworthy-safe\n\n  hs-source-dirs:           src\n  exposed-modules:\n    Data.Functor.These\n    Data.These\n    Data.These.Combinators\n\n  -- ghc boot libs\n  build-depends:\n      base     >=4.5.1.0 && <4.20\n    , binary   >=0.5.1.0 && <0.10\n    , deepseq  >=1.3.0.0 && <1.6\n\n  -- other dependencies\n  -- note: we need to depend on assoc-1.1 to be sure that\n  -- Bifunctor type class comes from bifunctor-classes-compat\n  build-depends:\n      assoc     >=1.1     && <1.2\n    , hashable  >=1.2.7.0 && <1.5\n\n  if impl(ghc <7.5)\n    build-depends: ghc-prim\n\n  if !impl(ghc >=9.6)\n    build-depends: foldable1-classes-compat >=0.1 && <0.2\n\n  if !impl(ghc >=8.2)\n    build-depends: bifunctor-classes-compat >=0.1 && <0.2\n\n  if !impl(ghc >=8.0)\n    build-depends:\n        semigroups           >=0.18.5  && <0.21\n      , transformers         >=0.3.0.0 && <0.7\n      , transformers-compat  >=0.6.5   && <0.8\n\n    -- Ensure Data.Functor.Classes is always available\n    if impl(ghc >=7.10)\n      build-depends: transformers >=0.4.2.0\n\n  x-docspec-extra-packages: lens\n";
    }