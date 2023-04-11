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
    flags = { build-example = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "wai-extra"; version = "3.1.13.0"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman";
      homepage = "http://github.com/yesodweb/wai";
      url = "";
      synopsis = "Provides some basic WAI handlers and middleware.";
      description = "Provides basic WAI handler and middleware functionality:\n\n* WAI Testing Framework\n\nHspec testing facilities and helpers for WAI.\n\n* Event Source/Event Stream\n\nSend server events to the client. Compatible with the JavaScript\nEventSource API.\n\n* Accept Override\n\nOverride the Accept header in a request. Special handling for the\n_accept query parameter (which is used throughout WAI override the\nAccept header).\n\n* Add Headers\n\nWAI Middleware for adding arbitrary headers to an HTTP request.\n\n* Clean Path\n\nClean a request path to a canonical form.\n\n* Combine Headers\n\nCombine duplicate headers into one.\n\n* GZip Compression\n\nNegotiate HTTP payload gzip compression.\n\n* Health check endpoint\n\nAdd an empty health check endpoint.\n\n* HTTP Basic Authentication\n\nWAI Basic Authentication Middleware which uses Authorization header.\n\n* JSONP\n\n\\\"JSON with Padding\\\" middleware. Automatic wrapping of JSON\nresponses to convert into JSONP.\n\n* Method Override / Post\n\nAllows overriding of the HTTP request method via the _method query string\nparameter.\n\n* Request Logging\n\nRequest logging middleware for development and production environments\n\n* Request Rewrite\n\nRewrite request path info based on a custom conversion rules.\n\n* Select\n\nDynamically choose between Middlewares.\n\n* Stream Files\n\nConvert ResponseFile type responses into ResponseStream type.\n\n* Virtual Host\n\nRedirect incoming requests to a new host based on custom rules.\n\n\nAPI docs and the README are available at <http://www.stackage.org/package/wai-extra>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."base64-bytestring" or (errorHandler.buildDepError "base64-bytestring"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."call-stack" or (errorHandler.buildDepError "call-stack"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
          (hsPkgs."data-default-class" or (errorHandler.buildDepError "data-default-class"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."fast-logger" or (errorHandler.buildDepError "fast-logger"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."vault" or (errorHandler.buildDepError "vault"))
          (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
          (hsPkgs."wai-logger" or (errorHandler.buildDepError "wai-logger"))
          (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
          (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
        buildable = true;
        };
      exes = {
        "example" = {
          depends = (pkgs.lib).optionals (flags.build-example) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            ];
          buildable = if flags.build-example then true else false;
          };
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cookie" or (errorHandler.buildDepError "cookie"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."fast-logger" or (errorHandler.buildDepError "fast-logger"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/wai-extra-3.1.13.0.tar.gz";
      sha256 = "df0d2d275bc3c888fae9cf525496140c707cbccfdf013a36dc00e7a94ac38cc0";
      });
    }) // {
    package-description-override = "Name:                wai-extra\r\nVersion:             3.1.13.0\r\nx-revision: 1\r\nSynopsis:            Provides some basic WAI handlers and middleware.\r\ndescription:\r\n  Provides basic WAI handler and middleware functionality:\r\n  .\r\n  * WAI Testing Framework\r\n  .\r\n  Hspec testing facilities and helpers for WAI.\r\n  .\r\n  * Event Source/Event Stream\r\n  .\r\n  Send server events to the client. Compatible with the JavaScript\r\n  EventSource API.\r\n  .\r\n  * Accept Override\r\n  .\r\n  Override the Accept header in a request. Special handling for the\r\n  _accept query parameter (which is used throughout WAI override the\r\n  Accept header).\r\n  .\r\n  * Add Headers\r\n  .\r\n  WAI Middleware for adding arbitrary headers to an HTTP request.\r\n  .\r\n  * Clean Path\r\n  .\r\n  Clean a request path to a canonical form.\r\n  .\r\n  * Combine Headers\r\n  .\r\n  Combine duplicate headers into one.\r\n  .\r\n  * GZip Compression\r\n  .\r\n  Negotiate HTTP payload gzip compression.\r\n  .\r\n  * Health check endpoint\r\n  .\r\n  Add an empty health check endpoint.\r\n  .\r\n  * HTTP Basic Authentication\r\n  .\r\n  WAI Basic Authentication Middleware which uses Authorization header.\r\n  .\r\n  * JSONP\r\n  .\r\n  \\\"JSON with Padding\\\" middleware. Automatic wrapping of JSON\r\n  responses to convert into JSONP.\r\n  .\r\n  * Method Override / Post\r\n  .\r\n  Allows overriding of the HTTP request method via the _method query string\r\n  parameter.\r\n  .\r\n  * Request Logging\r\n  .\r\n  Request logging middleware for development and production environments\r\n  .\r\n  * Request Rewrite\r\n  .\r\n  Rewrite request path info based on a custom conversion rules.\r\n  .\r\n  * Select\r\n  .\r\n  Dynamically choose between Middlewares.\r\n  .\r\n  * Stream Files\r\n  .\r\n  Convert ResponseFile type responses into ResponseStream type.\r\n  .\r\n  * Virtual Host\r\n  .\r\n  Redirect incoming requests to a new host based on custom rules.\r\n  .\r\n  .\r\n  API docs and the README are available at <http://www.stackage.org/package/wai-extra>.\r\n\r\nLicense:             MIT\r\nLicense-file:        LICENSE\r\nAuthor:              Michael Snoyman\r\nMaintainer:          michael@snoyman.com\r\nHomepage:            http://github.com/yesodweb/wai\r\nCategory:            Web\r\nBuild-Type:          Simple\r\nCabal-Version:       >=1.10\r\nStability:           Stable\r\nextra-source-files:\r\n  test/requests/dalvik-request\r\n  test/json\r\n  test/json.gz\r\n  test/noprecompress\r\n  test/test.html\r\n  test/sample.hs\r\n  ChangeLog.md\r\n  README.md\r\n\r\nflag build-example\r\n  description:        Build example executable.\r\n  manual:             True\r\n  default:            False\r\n\r\nLibrary\r\n  Build-Depends:     base                      >= 4.12 && < 5\r\n                   , aeson\r\n                   , ansi-terminal             >= 0.4\r\n                   , base64-bytestring\r\n                   , bytestring                >= 0.10.4\r\n                   , call-stack\r\n                   , case-insensitive          >= 0.2\r\n                   , containers\r\n                   , cookie\r\n                   , data-default-class\r\n                   , directory                 >= 1.2.7.0\r\n                   , fast-logger               >= 2.4.5\r\n                   , http-types                >= 0.7\r\n                   , HUnit\r\n                   , iproute                   >= 1.7.8\r\n                   , network                   >= 2.6.1.0\r\n                   , resourcet                 >= 0.4.6    && < 1.4\r\n                   , streaming-commons         >= 0.2\r\n                   , text                      >= 0.7\r\n                   , time                      >= 1.1.4\r\n                   , transformers              >= 0.2.2\r\n                   , vault\r\n                   , wai                       >= 3.2.2.1  && < 3.3\r\n                   , wai-logger                >= 2.3.7\r\n                   , warp                      >= 3.3.22\r\n                   , word8\r\n\r\n  if os(windows)\r\n      cpp-options:   -DWINDOWS\r\n  else\r\n      build-depends: unix\r\n\r\n  default-extensions:        OverloadedStrings\r\n\r\n  Exposed-modules:   Network.Wai.EventSource\r\n                     Network.Wai.EventSource.EventStream\r\n                     Network.Wai.Handler.CGI\r\n                     Network.Wai.Handler.SCGI\r\n                     Network.Wai.Header\r\n                     Network.Wai.Middleware.AcceptOverride\r\n                     Network.Wai.Middleware.AddHeaders\r\n                     Network.Wai.Middleware.Approot\r\n                     Network.Wai.Middleware.Autohead\r\n                     Network.Wai.Middleware.CleanPath\r\n                     Network.Wai.Middleware.CombineHeaders\r\n                     Network.Wai.Middleware.ForceDomain\r\n                     Network.Wai.Middleware.ForceSSL\r\n                     Network.Wai.Middleware.Gzip\r\n                     Network.Wai.Middleware.HealthCheckEndpoint\r\n                     Network.Wai.Middleware.HttpAuth\r\n                     Network.Wai.Middleware.Jsonp\r\n                     Network.Wai.Middleware.Local\r\n                     Network.Wai.Middleware.MethodOverride\r\n                     Network.Wai.Middleware.MethodOverridePost\r\n                     Network.Wai.Middleware.RealIp\r\n                     Network.Wai.Middleware.RequestLogger\r\n                     Network.Wai.Middleware.RequestLogger.JSON\r\n                     Network.Wai.Middleware.RequestSizeLimit\r\n                     Network.Wai.Middleware.RequestSizeLimit.Internal\r\n                     Network.Wai.Middleware.Rewrite\r\n                     Network.Wai.Middleware.Routed\r\n                     Network.Wai.Middleware.Select\r\n                     Network.Wai.Middleware.StreamFile\r\n                     Network.Wai.Middleware.StripHeaders\r\n                     Network.Wai.Middleware.Timeout\r\n                     Network.Wai.Middleware.Vhost\r\n                     Network.Wai.Parse\r\n                     Network.Wai.Request\r\n                     Network.Wai.Test\r\n                     Network.Wai.Test.Internal\r\n                     Network.Wai.UrlMap\r\n  other-modules:     Network.Wai.Middleware.RequestLogger.Internal\r\n                     Network.Wai.Util\r\n  default-language:          Haskell2010\r\n  ghc-options:       -Wall\r\n\r\nexecutable example\r\n  hs-source-dirs:      example\r\n  main-is:             Main.hs\r\n  ghc-options:         -threaded -rtsopts -with-rtsopts=-N -Wall\r\n  if flag(build-example)\r\n    build-depends:     base\r\n                     , bytestring\r\n                     , http-types\r\n                     , time\r\n                     , wai\r\n                     , wai-extra\r\n                     , warp\r\n  else\r\n    buildable: False\r\n  default-language:    Haskell2010\r\n\r\ntest-suite spec\r\n    type:            exitcode-stdio-1.0\r\n    hs-source-dirs:  test\r\n    main-is:         Spec.hs\r\n    other-modules:   Network.Wai.Middleware.ApprootSpec\r\n                     Network.Wai.Middleware.CombineHeadersSpec\r\n                     Network.Wai.Middleware.ForceSSLSpec\r\n                     Network.Wai.Middleware.RealIpSpec\r\n                     Network.Wai.Middleware.RequestSizeLimitSpec\r\n                     Network.Wai.Middleware.RoutedSpec\r\n                     Network.Wai.Middleware.SelectSpec\r\n                     Network.Wai.Middleware.StripHeadersSpec\r\n                     Network.Wai.Middleware.TimeoutSpec\r\n                     Network.Wai.ParseSpec\r\n                     Network.Wai.RequestSpec\r\n                     Network.Wai.TestSpec\r\n                     WaiExtraSpec\r\n    build-tool-depends: hspec-discover:hspec-discover\r\n    build-depends:   base                      >= 4        && < 5\r\n                   , aeson\r\n                   , bytestring\r\n                   , cookie\r\n                   , case-insensitive\r\n                   , directory\r\n                   , fast-logger\r\n                   , hspec >= 1.3\r\n                   , http-types\r\n                   , HUnit\r\n                   , iproute\r\n                   , resourcet\r\n                   , temporary\r\n                   , text\r\n                   , time\r\n                   , wai-extra\r\n                   , wai\r\n                   , warp\r\n                   , zlib\r\n    ghc-options:     -Wall\r\n    default-language:          Haskell2010\r\n\r\n    if os(windows)\r\n        cpp-options:   -DWINDOWS\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/yesodweb/wai.git\r\n";
    }