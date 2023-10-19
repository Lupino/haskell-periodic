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
    flags = {
      boundschecks = true;
      unsafechecks = false;
      internalchecks = false;
      wall = false;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "vector"; version = "0.13.1.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) Roman Leshchinskiy 2008-2012,\nAlexey Kuleshevich 2020-2022,\nAleksey Khudyakov 2020-2022,\nAndrew Lelechenko 2020-2022";
      maintainer = "Haskell Libraries Team <libraries@haskell.org>\nAlexey Kuleshevich <alexey@kuleshevi.ch>,\nAleksey Khudyakov <alexey.skladnoy@gmail.com>,\nAndrew Lelechenko <andrew.lelechenko@gmail.com>";
      author = "Roman Leshchinskiy <rl@cse.unsw.edu.au>";
      homepage = "https://github.com/haskell/vector";
      url = "";
      synopsis = "Efficient Arrays";
      description = "\nAn efficient implementation of @Int@-indexed arrays (both mutable\nand immutable), with a powerful loop optimisation framework .\n\nIt is structured as follows:\n\n[\"Data.Vector\"] Boxed vectors of arbitrary types.\n\n[\"Data.Vector.Unboxed\"] Unboxed vectors with an adaptive\nrepresentation based on data type families.\n\n[\"Data.Vector.Storable\"] Unboxed vectors of 'Storable' types.\n\n[\"Data.Vector.Primitive\"] Unboxed vectors of primitive types as\ndefined by the @primitive@ package. \"Data.Vector.Unboxed\" is more\nflexible at no performance cost.\n\n[\"Data.Vector.Generic\"] Generic interface to the vector types.\n\nThere is also a (draft) tutorial on common uses of vector.\n\n* <http://haskell.org/haskellwiki/Numeric_Haskell:_A_Vector_Tutorial>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."vector-stream" or (errorHandler.buildDepError "vector-stream"))
          ];
        buildable = true;
        };
      tests = {
        "vector-tests-O0" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        "vector-tests-O2" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        "vector-doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = (if compiler.isGhc && (compiler.version).lt "8.6"
            then false
            else true) && (if compiler.isGhc && (compiler.version).ge "8.10" && (compiler.isGhc && (compiler.version).lt "8.11")
            then false
            else true) && (if compiler.isGhc && (compiler.version).ge "9.0" && (compiler.isGhc && (compiler.version).lt "9.1")
            then false
            else true) && (if compiler.isGhc && (compiler.version).ge "9.2" && (compiler.isGhc && (compiler.version).lt "9.2.3")
            then false
            else true);
          };
        "vector-inspection" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-inspection-testing" or (errorHandler.buildDepError "tasty-inspection-testing"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "algorithms" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-bench" or (errorHandler.buildDepError "tasty-bench"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/vector-0.13.1.0.tar.gz";
      sha256 = "63f272279eab8ab9411a0fffb1252ac309b297313f8e33be9ebbc2f981edecee";
      });
    }) // {
    package-description-override = "Name:           vector\nVersion:        0.13.1.0\n-- don't forget to update the changelog file!\nLicense:        BSD3\nLicense-File:   LICENSE\nAuthor:         Roman Leshchinskiy <rl@cse.unsw.edu.au>\nMaintainer:     Haskell Libraries Team <libraries@haskell.org>\n                Alexey Kuleshevich <alexey@kuleshevi.ch>,\n                Aleksey Khudyakov <alexey.skladnoy@gmail.com>,\n                Andrew Lelechenko <andrew.lelechenko@gmail.com>\nCopyright:      (c) Roman Leshchinskiy 2008-2012,\n                    Alexey Kuleshevich 2020-2022,\n                    Aleksey Khudyakov 2020-2022,\n                    Andrew Lelechenko 2020-2022\n\nHomepage:       https://github.com/haskell/vector\nBug-Reports:    https://github.com/haskell/vector/issues\nCategory:       Data, Data Structures\nSynopsis:       Efficient Arrays\nDescription:\n        .\n        An efficient implementation of @Int@-indexed arrays (both mutable\n        and immutable), with a powerful loop optimisation framework .\n        .\n        It is structured as follows:\n        .\n        [\"Data.Vector\"] Boxed vectors of arbitrary types.\n        .\n        [\"Data.Vector.Unboxed\"] Unboxed vectors with an adaptive\n        representation based on data type families.\n        .\n        [\"Data.Vector.Storable\"] Unboxed vectors of 'Storable' types.\n        .\n        [\"Data.Vector.Primitive\"] Unboxed vectors of primitive types as\n        defined by the @primitive@ package. \"Data.Vector.Unboxed\" is more\n        flexible at no performance cost.\n        .\n        [\"Data.Vector.Generic\"] Generic interface to the vector types.\n        .\n        There is also a (draft) tutorial on common uses of vector.\n        .\n        * <http://haskell.org/haskellwiki/Numeric_Haskell:_A_Vector_Tutorial>\n\nTested-With:\n  GHC == 8.0.2,\n  GHC == 8.2.2,\n  GHC == 8.4.4,\n  GHC == 8.6.5,\n  GHC == 8.8.4,\n  GHC == 8.10.7,\n  GHC == 9.0.2,\n  GHC == 9.2.8,\n  GHC == 9.4.6,\n  GHC == 9.6.2\n  \n\nCabal-Version:  >= 1.10\nBuild-Type:     Simple\n\nExtra-Source-Files:\n      changelog.md\n      README.md\n      tests/LICENSE\n      tests/Setup.hs\n      tests/Main.hs\n      internal/GenUnboxTuple.hs\n      internal/unbox-tuple-instances\n\nFlag BoundsChecks\n  Description: Enable bounds checking\n  Default: True\n  Manual: True\n\nFlag UnsafeChecks\n  Description: Enable bounds checking in unsafe operations at the cost of a\n               significant performance penalty\n  Default: False\n  Manual: True\n\nFlag InternalChecks\n  Description: Enable internal consistency checks at the cost of a\n               significant performance penalty\n  Default: False\n  Manual: True\n\nFlag Wall\n  Description: Enable all -Wall warnings\n  Default: False\n  Manual: True\n\n\nLibrary\n  Default-Language: Haskell2010\n  Other-Extensions:\n        BangPatterns\n        CPP\n        DeriveDataTypeable\n        ExistentialQuantification\n        FlexibleContexts\n        FlexibleInstances\n        GADTs\n        KindSignatures\n        MagicHash\n        MultiParamTypeClasses\n        RankNTypes\n        ScopedTypeVariables\n        StandaloneDeriving\n        TypeFamilies\n\n  Exposed-Modules:\n        Data.Vector.Internal.Check\n\n        Data.Vector.Fusion.Util\n        Data.Vector.Fusion.Stream.Monadic\n        Data.Vector.Fusion.Bundle.Size\n        Data.Vector.Fusion.Bundle.Monadic\n        Data.Vector.Fusion.Bundle\n\n        Data.Vector.Generic.Mutable.Base\n        Data.Vector.Generic.Mutable\n        Data.Vector.Generic.Base\n        Data.Vector.Generic.New\n        Data.Vector.Generic\n\n        Data.Vector.Primitive.Mutable\n        Data.Vector.Primitive\n\n        Data.Vector.Storable.Internal\n        Data.Vector.Storable.Mutable\n        Data.Vector.Storable\n\n        Data.Vector.Unboxed.Base\n        Data.Vector.Unboxed.Mutable\n        Data.Vector.Unboxed\n\n        Data.Vector.Mutable\n        Data.Vector\n\n  Hs-Source-Dirs:\n        src\n\n  Include-Dirs:\n        include, internal\n\n  Install-Includes:\n        vector.h\n\n  Build-Depends: base >= 4.9 && < 4.20\n               , primitive >= 0.6.4.0 && < 0.10\n               , deepseq >= 1.1 && < 1.6\n               , vector-stream >= 0.1 && < 0.2\n\n  Ghc-Options: -O2 -Wall\n\n  if !flag(Wall)\n    Ghc-Options: -fno-warn-orphans\n\n    if impl(ghc >= 8.0) && impl(ghc < 8.1)\n      Ghc-Options:   -Wno-redundant-constraints\n\n  if flag(BoundsChecks)\n    cpp-options: -DVECTOR_BOUNDS_CHECKS\n\n  if flag(UnsafeChecks)\n    cpp-options: -DVECTOR_UNSAFE_CHECKS\n\n  if flag(InternalChecks)\n    cpp-options: -DVECTOR_INTERNAL_CHECKS\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell/vector.git\n  subdir:   vector\n\n\ntest-suite vector-tests-O0\n  Default-Language: Haskell2010\n  type: exitcode-stdio-1.0\n  Main-Is:  Main.hs\n\n  other-modules: Boilerplater\n                 Tests.Bundle\n                 Tests.Move\n                 Tests.Vector\n                 Tests.Vector.Property\n                 Tests.Vector.Boxed\n                 Tests.Vector.Storable\n                 Tests.Vector.Primitive\n                 Tests.Vector.Unboxed\n                 Tests.Vector.UnitTests\n                 Utilities\n\n  hs-source-dirs: tests\n  Build-Depends: base >= 4.5 && < 5, template-haskell, base-orphans >= 0.6, vector,\n                 primitive, random,\n                 QuickCheck >= 2.9 && < 2.15, HUnit, tasty,\n                 tasty-hunit, tasty-quickcheck,\n                 transformers >= 0.2.0.0\n\n  default-extensions: CPP,\n              ScopedTypeVariables,\n              PatternGuards,\n              MultiParamTypeClasses,\n              FlexibleContexts,\n              RankNTypes,\n              TypeSynonymInstances,\n              TypeFamilies,\n              TemplateHaskell\n\n  Ghc-Options: -O0 -threaded\n  Ghc-Options: -Wall\n\n  if !flag(Wall)\n    Ghc-Options: -fno-warn-orphans -fno-warn-missing-signatures\n    if impl(ghc >= 8.0) && impl(ghc < 8.1)\n      Ghc-Options: -Wno-redundant-constraints\n\n\ntest-suite vector-tests-O2\n  Default-Language: Haskell2010\n  type: exitcode-stdio-1.0\n  Main-Is:  Main.hs\n\n  other-modules: Boilerplater\n                 Tests.Bundle\n                 Tests.Move\n                 Tests.Vector\n                 Tests.Vector.Property\n                 Tests.Vector.Boxed\n                 Tests.Vector.Storable\n                 Tests.Vector.Primitive\n                 Tests.Vector.Unboxed\n                 Tests.Vector.UnitTests\n                 Utilities\n\n  hs-source-dirs: tests\n  Build-Depends: base >= 4.5 && < 5, template-haskell, base-orphans >= 0.6, vector,\n                 primitive, random,\n                 QuickCheck >= 2.9 && < 2.15, HUnit, tasty,\n                 tasty-hunit, tasty-quickcheck,\n                 transformers >= 0.2.0.0\n\n  default-extensions: CPP,\n              ScopedTypeVariables,\n              PatternGuards,\n              MultiParamTypeClasses,\n              FlexibleContexts,\n              RankNTypes,\n              TypeSynonymInstances,\n              TypeFamilies,\n              TemplateHaskell\n\n  Ghc-Options: -Wall\n  Ghc-Options:  -O2 -threaded\n  if !flag(Wall)\n    Ghc-Options: -fno-warn-orphans -fno-warn-missing-signatures\n    if impl(ghc >= 8.0) && impl(ghc < 8.1)\n      Ghc-Options: -Wno-redundant-constraints\n\ntest-suite vector-doctest\n  type:             exitcode-stdio-1.0\n  main-is:          doctests.hs\n  hs-source-dirs:   tests\n  default-language: Haskell2010\n  -- Older GHC don't support DerivingVia\n  if impl(ghc < 8.6)\n    buildable: False\n  -- GHC 8.10 fails to run doctests for some reason\n  if impl(ghc >= 8.10) && impl(ghc < 8.11)\n    buildable: False\n  -- GHC 9.0 fails to run doctests for some reason too\n  if impl(ghc >= 9.0) && impl(ghc < 9.1)\n    buildable: False\n  -- And GHC 9.2 too\n  if impl(ghc >= 9.2) && impl(ghc < 9.2.3)\n    buildable: False\n  if impl(ghc >= 9.2.3) && impl(ghc < 9.3)\n    buildable: True\n  build-depends:\n        base      -any\n      , doctest   >=0.15 && <0.23\n      , primitive >= 0.6.4.0 && < 0.10\n      , vector    -any\n\ntest-suite vector-inspection\n  type:             exitcode-stdio-1.0\n  hs-source-dirs:   tests-inspect\n  Ghc-Options:      -Wall\n  main-is:          main.hs\n  default-language: Haskell2010\n  Other-modules:    Inspect\n  if impl(ghc >= 8.6)\n    Other-modules:  Inspect.DerivingVia\n                    Inspect.DerivingVia.OtherFoo\n  build-depends:\n        base                     -any\n      , primitive                >= 0.6.4.0 && < 0.10\n      , vector                   -any\n      , tasty\n      , tasty-inspection-testing >= 0.1\n\nbenchmark algorithms\n  type:             exitcode-stdio-1.0\n  main-is:          Main.hs\n  hs-source-dirs:   benchmarks\n  default-language: Haskell2010\n\n  build-depends:\n        base >= 2 && < 5\n      , random >= 1.2\n      , tasty\n      , tasty-bench >= 0.2.1\n      , vector\n\n  ghc-options: -O2\n\n  other-modules:\n        Algo.MutableSet\n        Algo.ListRank\n        Algo.Rootfix\n        Algo.Leaffix\n        Algo.AwShCC\n        Algo.HybCC\n        Algo.Quickhull\n        Algo.Spectral\n        Algo.Tridiag\n        Algo.FindIndexR\n        TestData.ParenTree\n        TestData.Graph\n";
    }