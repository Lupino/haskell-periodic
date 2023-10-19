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
      identifier = { name = "scotty"; version = "0.12.1"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2012-Present Andrew Farmer";
      maintainer = "Andrew Farmer <xichekolas@gmail.com>";
      author = "Andrew Farmer <xichekolas@gmail.com>";
      homepage = "https://github.com/scotty-web/scotty";
      url = "";
      synopsis = "Haskell web framework inspired by Ruby's Sinatra, using WAI and Warp";
      description = "A Haskell web framework inspired by Ruby's Sinatra, using WAI and Warp.\n\n@\n&#123;-&#35; LANGUAGE OverloadedStrings &#35;-&#125;\n\nimport Web.Scotty\n\nimport Data.Monoid (mconcat)\n\nmain = scotty 3000 $\n&#32;&#32;get &#34;/:word&#34; $ do\n&#32;&#32;&#32;&#32;beam <- param &#34;word&#34;\n&#32;&#32;&#32;&#32;html $ mconcat [&#34;&#60;h1&#62;Scotty, &#34;, beam, &#34; me up!&#60;/h1&#62;&#34;]\n@\n\n\nScotty is the cheap and cheerful way to write RESTful, declarative web applications.\n\n* A page is as simple as defining the verb, url pattern, and Text content.\n\n* It is template-language agnostic. Anything that returns a Text value will do.\n\n* Conforms to WAI Application interface.\n\n* Uses very fast Warp webserver by default.\n\nAs for the name: Sinatra + Warp = Scotty.\n\n[WAI] <http://hackage.haskell.org/package/wai>\n\n[Warp] <http://hackage.haskell.org/package/warp>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-compat-batteries" or (errorHandler.buildDepError "base-compat-batteries"))
          (hsPkgs."blaze-builder" or (errorHandler.buildDepError "blaze-builder"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."monad-control" or (errorHandler.buildDepError "monad-control"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."regex-compat" or (errorHandler.buildDepError "regex-compat"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-base" or (errorHandler.buildDepError "transformers-base"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
          (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
          (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."fail" or (errorHandler.buildDepError "fail"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.10") (hsPkgs."nats" or (errorHandler.buildDepError "nats"));
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."hspec-wai" or (errorHandler.buildDepError "hspec-wai"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."lifted-base" or (errorHandler.buildDepError "lifted-base"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."scotty" or (errorHandler.buildDepError "scotty"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "weigh" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."scotty" or (errorHandler.buildDepError "scotty"))
            (hsPkgs."lucid" or (errorHandler.buildDepError "lucid"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
            (hsPkgs."weigh" or (errorHandler.buildDepError "weigh"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/scotty-0.12.1.tar.gz";
      sha256 = "1e2a33ff37765d9e1de7fc7b8f20a75b91b008ada60c1e445df06c7fc614db4a";
      });
    }) // {
    package-description-override = "Name:                scotty\r\nVersion:             0.12.1\r\nx-revision: 2\r\nSynopsis:            Haskell web framework inspired by Ruby's Sinatra, using WAI and Warp\r\nHomepage:            https://github.com/scotty-web/scotty\r\nBug-reports:         https://github.com/scotty-web/scotty/issues\r\nLicense:             BSD3\r\nLicense-file:        LICENSE\r\nAuthor:              Andrew Farmer <xichekolas@gmail.com>\r\nMaintainer:          Andrew Farmer <xichekolas@gmail.com>\r\nCopyright:           (c) 2012-Present Andrew Farmer\r\nCategory:            Web\r\nStability:           experimental\r\nBuild-type:          Simple\r\nCabal-version:       >= 1.10\r\nDescription:\r\n  A Haskell web framework inspired by Ruby's Sinatra, using WAI and Warp.\r\n  .\r\n  @\r\n    &#123;-&#35; LANGUAGE OverloadedStrings &#35;-&#125;\r\n    .\r\n    import Web.Scotty\r\n    .\r\n    import Data.Monoid (mconcat)\r\n    .\r\n    main = scotty 3000 $\r\n    &#32;&#32;get &#34;/:word&#34; $ do\r\n    &#32;&#32;&#32;&#32;beam <- param &#34;word&#34;\r\n    &#32;&#32;&#32;&#32;html $ mconcat [&#34;&#60;h1&#62;Scotty, &#34;, beam, &#34; me up!&#60;/h1&#62;&#34;]\r\n  @\r\n  .\r\n  .\r\n  Scotty is the cheap and cheerful way to write RESTful, declarative web applications.\r\n  .\r\n    * A page is as simple as defining the verb, url pattern, and Text content.\r\n  .\r\n    * It is template-language agnostic. Anything that returns a Text value will do.\r\n  .\r\n    * Conforms to WAI Application interface.\r\n  .\r\n    * Uses very fast Warp webserver by default.\r\n  .\r\n  As for the name: Sinatra + Warp = Scotty.\r\n  .\r\n  [WAI] <http://hackage.haskell.org/package/wai>\r\n  .\r\n  [Warp] <http://hackage.haskell.org/package/warp>\r\ntested-with:         GHC == 7.6.3\r\n                   , GHC == 7.8.4\r\n                   , GHC == 7.10.3\r\n                   , GHC == 8.0.2\r\n                   , GHC == 8.2.2\r\n                   , GHC == 8.4.4\r\n                   , GHC == 8.6.5\r\n                   , GHC == 8.8.4\r\n                   , GHC == 8.10.4\r\n                   , GHC == 9.0.1\r\nExtra-source-files:\r\n    README.md\r\n    changelog.md\r\n    examples/404.html\r\n    examples/LICENSE\r\n    examples/*.hs\r\n    examples/static/jquery.js\r\n    examples/static/jquery-json.js\r\n    examples/uploads/.keep\r\n\r\nLibrary\r\n  Exposed-modules:     Web.Scotty\r\n                       Web.Scotty.Trans\r\n                       Web.Scotty.Internal.Types\r\n  other-modules:       Web.Scotty.Action\r\n                       Web.Scotty.Route\r\n                       Web.Scotty.Util\r\n  default-language:    Haskell2010\r\n  build-depends:       aeson                 >= 0.6.2.1  && < 2.3,\r\n                       base                  >= 4.6      && < 5,\r\n                       base-compat-batteries >= 0.10     && < 0.14,\r\n                       blaze-builder         >= 0.3.3.0  && < 0.5,\r\n                       bytestring            >= 0.10.0.2 && < 0.12,\r\n                       case-insensitive      >= 1.0.0.1  && < 1.3,\r\n                       data-default-class    >= 0.0.1    && < 0.2,\r\n                       exceptions            >= 0.7      && < 0.11,\r\n                       http-types            >= 0.9.1    && < 0.13,\r\n                       monad-control         >= 1.0.0.3  && < 1.1,\r\n                       mtl                   >= 2.1.2    && < 2.4,\r\n                       network               >= 2.6.0.2  && < 3.2,\r\n                       regex-compat          >= 0.95.1   && < 0.96,\r\n                       text                  >= 0.11.3.1 && < 2.1,\r\n                       transformers          >= 0.3.0.0  && < 0.7,\r\n                       transformers-base     >= 0.4.1    && < 0.5,\r\n                       transformers-compat   >= 0.4      && < 0.8,\r\n                       wai                   >= 3.0.0    && < 3.3,\r\n                       wai-extra             >= 3.0.0    && < 3.2,\r\n                       warp                  >= 3.0.13   && < 3.4\r\n\r\n  if impl(ghc < 8.0)\r\n    build-depends:     fail\r\n\r\n  if impl(ghc < 7.10)\r\n    build-depends:     nats                  >= 0.1      && < 2\r\n\r\n  GHC-options: -Wall -fno-warn-orphans\r\n\r\ntest-suite spec\r\n  main-is:             Spec.hs\r\n  other-modules:       Web.ScottySpec\r\n  type:                exitcode-stdio-1.0\r\n  default-language:    Haskell2010\r\n  hs-source-dirs:      test\r\n  build-depends:       async,\r\n                       base,\r\n                       bytestring,\r\n                       data-default-class,\r\n                       directory,\r\n                       hspec == 2.*,\r\n                       hspec-wai >= 0.6.3,\r\n                       http-types,\r\n                       lifted-base,\r\n                       network,\r\n                       scotty,\r\n                       text,\r\n                       wai\r\n  build-tool-depends:  hspec-discover:hspec-discover == 2.*\r\n  GHC-options:         -Wall -threaded -fno-warn-orphans\r\n\r\nbenchmark weigh\r\n  main-is:             Main.hs\r\n  type:                exitcode-stdio-1.0\r\n  default-language:    Haskell2010\r\n  hs-source-dirs:      bench\r\n  build-depends:       base,\r\n                       scotty,\r\n                       lucid,\r\n                       bytestring,\r\n                       mtl,\r\n                       text,\r\n                       transformers,\r\n                       data-default-class,\r\n                       weigh == 0.0.16\r\n  GHC-options:         -Wall -O2 -threaded\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/scotty-web/scotty.git\r\n";
    }