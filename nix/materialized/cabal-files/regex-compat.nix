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
      identifier = { name = "regex-compat"; version = "0.95.2.1"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (c) 2006, Christopher Kuklewicz";
      maintainer = "Andreas Abel";
      author = "Christopher Kuklewicz";
      homepage = "https://wiki.haskell.org/Regular_expressions";
      url = "";
      synopsis = "Replaces/enhances \"Text.Regex\"";
      description = "One module compat layer over <//hackage.haskell.org/package/regex-posix regex-posix> to replace \"Text.Regex\".\n\nSee also <https://wiki.haskell.org/Regular_expressions> for more information.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."regex-base" or (errorHandler.buildDepError "regex-base"))
          (hsPkgs."regex-posix" or (errorHandler.buildDepError "regex-posix"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/regex-compat-0.95.2.1.tar.gz";
      sha256 = "8f7b386c72fd605a292edfb809b8620245b4a3ab7af192ad79e36778596e7947";
      });
    }) // {
    package-description-override = "cabal-version:          1.12\nname:                   regex-compat\nversion:                0.95.2.1\nx-revision:             2\n\nbuild-type:             Simple\nlicense:                BSD3\nlicense-file:           LICENSE\ncopyright:              Copyright (c) 2006, Christopher Kuklewicz\nauthor:                 Christopher Kuklewicz\nmaintainer:             Andreas Abel\nhomepage:               https://wiki.haskell.org/Regular_expressions\nbug-reports:            https://github.com/haskell-hvr/regex-compat/issues\nsynopsis:               Replaces/enhances \"Text.Regex\"\ncategory:               Text\ndescription:\n  One module compat layer over <//hackage.haskell.org/package/regex-posix regex-posix> to replace \"Text.Regex\".\n  .\n  See also <https://wiki.haskell.org/Regular_expressions> for more information.\n\nextra-source-files:\n  ChangeLog.md\n\ntested-with:\n  GHC == 9.4.1\n  GHC == 9.2.2\n  GHC == 9.0.2\n  GHC == 8.10.7\n  GHC == 8.8.4\n  GHC == 8.6.5\n  GHC == 8.4.4\n  GHC == 8.2.2\n  GHC == 8.0.2\n  GHC == 7.10.3\n  GHC == 7.8.4\n  GHC == 7.6.3\n  GHC == 7.4.2\n  GHC == 7.2.2\n  GHC == 7.0.4\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell-hvr/regex-compat.git\n\nsource-repository this\n  type:     git\n  location: https://github.com/haskell-hvr/regex-compat.git\n  tag:      v0.95.2.1-r2\n\nlibrary\n  exposed-modules: Text.Regex\n\n  build-depends: base        >= 4.3 && < 5\n               , regex-base  == 0.94.*\n               , regex-posix == 0.96.*\n               , array       >= 0.3 && < 0.6\n\n  default-language: Haskell2010\n  default-extensions: MultiParamTypeClasses, FunctionalDependencies\n\n  if impl(ghc >= 7.2)\n    default-extensions: Trustworthy\n\n  ghc-options:            -Wall\n";
    }