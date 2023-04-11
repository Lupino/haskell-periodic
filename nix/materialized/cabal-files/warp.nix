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
      network-bytestring = false;
      allow-sendfilefd = true;
      warp-debug = false;
      x509 = true;
      };
    package = {
      specVersion = "1.10";
      identifier = { name = "warp"; version = "3.3.23"; };
      license = "MIT";
      copyright = "";
      maintainer = "michael@snoyman.com";
      author = "Michael Snoyman, Kazu Yamamoto, Matt Brown";
      homepage = "http://github.com/yesodweb/wai";
      url = "";
      synopsis = "A fast, light-weight web server for WAI applications.";
      description = "HTTP\\/1.0, HTTP\\/1.1 and HTTP\\/2 are supported.\nFor HTTP\\/2,  Warp supports direct and ALPN (in TLS)\nbut not upgrade.\nAPI docs and the README are available at\n<http://www.stackage.org/package/warp>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
          (hsPkgs."bsb-http-chunked" or (errorHandler.buildDepError "bsb-http-chunked"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."http-date" or (errorHandler.buildDepError "http-date"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."http2" or (errorHandler.buildDepError "http2"))
          (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
          (hsPkgs."recv" or (errorHandler.buildDepError "recv"))
          (hsPkgs."simple-sendfile" or (errorHandler.buildDepError "simple-sendfile"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time-manager" or (errorHandler.buildDepError "time-manager"))
          (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
          (hsPkgs."vault" or (errorHandler.buildDepError "vault"))
          (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
          (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          ] ++ (pkgs.lib).optional (flags.x509) (hsPkgs."x509" or (errorHandler.buildDepError "x509"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (if flags.network-bytestring
          then [
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-bytestring" or (errorHandler.buildDepError "network-bytestring"))
            ]
          else [
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            ])) ++ (if system.isWindows
          then [ (hsPkgs."time" or (errorHandler.buildDepError "time")) ]
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        buildable = true;
        };
      tests = {
        "doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            ];
          buildable = false;
          };
        "spec" = {
          depends = (([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
            (hsPkgs."bsb-http-chunked" or (errorHandler.buildDepError "bsb-http-chunked"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."case-insensitive" or (errorHandler.buildDepError "case-insensitive"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."http-client" or (errorHandler.buildDepError "http-client"))
            (hsPkgs."http-date" or (errorHandler.buildDepError "http-date"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."http2" or (errorHandler.buildDepError "http2"))
            (hsPkgs."iproute" or (errorHandler.buildDepError "iproute"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."recv" or (errorHandler.buildDepError "recv"))
            (hsPkgs."simple-sendfile" or (errorHandler.buildDepError "simple-sendfile"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."streaming-commons" or (errorHandler.buildDepError "streaming-commons"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."time-manager" or (errorHandler.buildDepError "time-manager"))
            (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
            (hsPkgs."vault" or (errorHandler.buildDepError "vault"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."word8" or (errorHandler.buildDepError "word8"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            ] ++ (pkgs.lib).optional (flags.x509) (hsPkgs."x509" or (errorHandler.buildDepError "x509"))) ++ (pkgs.lib).optionals (compiler.isGhc && (compiler.version).lt "8") [
            (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ]) ++ (if system.isWindows
            then [ (hsPkgs."time" or (errorHandler.buildDepError "time")) ]
            else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "parser" = {
          depends = ((([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."auto-update" or (errorHandler.buildDepError "auto-update"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."http-date" or (errorHandler.buildDepError "http-date"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."recv" or (errorHandler.buildDepError "recv"))
            (hsPkgs."time-manager" or (errorHandler.buildDepError "time-manager"))
            (hsPkgs."unix-compat" or (errorHandler.buildDepError "unix-compat"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            ] ++ (pkgs.lib).optional (flags.x509) (hsPkgs."x509" or (errorHandler.buildDepError "x509"))) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional ((system.isLinux || system.isFreebsd || system.isOsx) && flags.allow-sendfilefd) (hsPkgs."unix" or (errorHandler.buildDepError "unix"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."time" or (errorHandler.buildDepError "time"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/warp-3.3.23.tar.gz";
      sha256 = "bad666f7c8557f5b1d3c5f367cff7115d2627bbd69859e319d2fff823f3b3978";
      });
    }) // {
    package-description-override = "Name:                warp\r\nVersion:             3.3.23\r\nx-revision: 1\r\nSynopsis:            A fast, light-weight web server for WAI applications.\r\nLicense:             MIT\r\nLicense-file:        LICENSE\r\nAuthor:              Michael Snoyman, Kazu Yamamoto, Matt Brown\r\nMaintainer:          michael@snoyman.com\r\nHomepage:            http://github.com/yesodweb/wai\r\nCategory:            Web, Yesod\r\nBuild-Type:          Simple\r\nCabal-Version:       >= 1.10\r\nStability:           Stable\r\ndescription:         HTTP\\/1.0, HTTP\\/1.1 and HTTP\\/2 are supported.\r\n                     For HTTP\\/2,  Warp supports direct and ALPN (in TLS)\r\n                     but not upgrade.\r\n                     API docs and the README are available at\r\n                     <http://www.stackage.org/package/warp>.\r\nextra-source-files:  attic/hex\r\n                     ChangeLog.md\r\n                     README.md\r\n                     test/head-response\r\n                     test/inputFile\r\n\r\nFlag network-bytestring\r\n    Default: False\r\n\r\nFlag allow-sendfilefd\r\n    Description: Allow use of sendfileFd (not available on GNU/kFreeBSD)\r\n    Default:     True\r\n\r\nFlag warp-debug\r\n    Description: print debug output. not suitable for production\r\n    Default:     False\r\n\r\nFlag x509\r\n    Description: Adds a dependency on the x509 library to enable getting TLS client certificates.\r\n    Default:     True\r\n\r\nLibrary\r\n  Build-Depends:     base                      >= 4.12       && < 5\r\n                   , array\r\n                   , auto-update               >= 0.1.3    && < 0.2\r\n                   , bsb-http-chunked                         < 0.1\r\n                   , bytestring                >= 0.9.1.4\r\n                   , case-insensitive          >= 0.2\r\n                   , containers\r\n                   , ghc-prim\r\n                   , hashable\r\n                   , http-date\r\n                   , http-types                >= 0.12\r\n                   , http2                     >= 3.0      && < 5\r\n                   , iproute                   >= 1.3.1\r\n                   , recv                                     < 0.1.0\r\n                   , simple-sendfile           >= 0.2.7    && < 0.3\r\n                   , stm                       >= 2.3\r\n                   , streaming-commons         >= 0.1.10\r\n                   , text\r\n                   , time-manager\r\n                   , unix-compat               >= 0.2\r\n                   , vault                     >= 0.3\r\n                   , wai                       >= 3.2      && < 3.3\r\n                   , word8\r\n                   , unliftio\r\n  if flag(x509)\r\n      Build-Depends: x509\r\n  if impl(ghc < 8)\r\n      Build-Depends: semigroups\r\n  if flag(network-bytestring)\r\n      Build-Depends: network                   >= 2.2.1.5  && < 2.2.3\r\n                   , network-bytestring        >= 0.1.3    && < 0.1.4\r\n  else\r\n      Build-Depends: network               >= 2.3\r\n  Exposed-modules:   Network.Wai.Handler.Warp\r\n                     Network.Wai.Handler.Warp.Internal\r\n  Other-modules:     Network.Wai.Handler.Warp.Buffer\r\n                     Network.Wai.Handler.Warp.Conduit\r\n                     Network.Wai.Handler.Warp.Counter\r\n                     Network.Wai.Handler.Warp.Date\r\n                     Network.Wai.Handler.Warp.FdCache\r\n                     Network.Wai.Handler.Warp.File\r\n                     Network.Wai.Handler.Warp.FileInfoCache\r\n                     Network.Wai.Handler.Warp.HashMap\r\n                     Network.Wai.Handler.Warp.HTTP1\r\n                     Network.Wai.Handler.Warp.HTTP2\r\n                     Network.Wai.Handler.Warp.HTTP2.File\r\n                     Network.Wai.Handler.Warp.HTTP2.PushPromise\r\n                     Network.Wai.Handler.Warp.HTTP2.Request\r\n                     Network.Wai.Handler.Warp.HTTP2.Response\r\n                     Network.Wai.Handler.Warp.HTTP2.Types\r\n                     Network.Wai.Handler.Warp.Header\r\n                     Network.Wai.Handler.Warp.IO\r\n                     Network.Wai.Handler.Warp.Imports\r\n                     Network.Wai.Handler.Warp.PackInt\r\n                     Network.Wai.Handler.Warp.ReadInt\r\n                     Network.Wai.Handler.Warp.Request\r\n                     Network.Wai.Handler.Warp.RequestHeader\r\n                     Network.Wai.Handler.Warp.Response\r\n                     Network.Wai.Handler.Warp.ResponseHeader\r\n                     Network.Wai.Handler.Warp.Run\r\n                     Network.Wai.Handler.Warp.SendFile\r\n                     Network.Wai.Handler.Warp.Settings\r\n                     Network.Wai.Handler.Warp.Types\r\n                     Network.Wai.Handler.Warp.Windows\r\n                     Network.Wai.Handler.Warp.WithApplication\r\n                     Paths_warp\r\n  Ghc-Options:       -Wall\r\n\r\n  if flag(warp-debug)\r\n      Cpp-Options:   -DWARP_DEBUG\r\n  if (os(linux) || os(freebsd) || os(darwin)) && flag(allow-sendfilefd)\r\n      Cpp-Options:   -DSENDFILEFD\r\n  if os(windows)\r\n      Cpp-Options:   -DWINDOWS\r\n      Build-Depends: time\r\n  else\r\n      Build-Depends: unix\r\n      Other-modules: Network.Wai.Handler.Warp.MultiMap\r\n  if impl(ghc >= 8)\r\n      Default-Extensions:  Strict StrictData\r\n  Default-Language:     Haskell2010\r\n\r\nTest-Suite doctest\r\n  buildable:            False\r\n  Type:                 exitcode-stdio-1.0\r\n  HS-Source-Dirs:       test\r\n  Ghc-Options:          -threaded -Wall\r\n  Main-Is:              doctests.hs\r\n  Build-Depends:        base >= 4.8 && < 5\r\n                      , doctest >= 0.10.1\r\n  if os(windows)\r\n    Buildable: False\r\n  if impl(ghc >= 8)\r\n      Default-Extensions:  Strict StrictData\r\n  Default-Language:     Haskell2010\r\n\r\nTest-Suite spec\r\n    Main-Is:         Spec.hs\r\n    Other-modules:   ConduitSpec\r\n                     ExceptionSpec\r\n                     FdCacheSpec\r\n                     FileSpec\r\n                     ReadIntSpec\r\n                     RequestSpec\r\n                     ResponseHeaderSpec\r\n                     ResponseSpec\r\n                     RunSpec\r\n                     SendFileSpec\r\n                     WithApplicationSpec\r\n                     HTTP\r\n                     Network.Wai.Handler.Warp\r\n                     Network.Wai.Handler.Warp.Buffer\r\n                     Network.Wai.Handler.Warp.Conduit\r\n                     Network.Wai.Handler.Warp.Counter\r\n                     Network.Wai.Handler.Warp.Date\r\n                     Network.Wai.Handler.Warp.FdCache\r\n                     Network.Wai.Handler.Warp.File\r\n                     Network.Wai.Handler.Warp.FileInfoCache\r\n                     Network.Wai.Handler.Warp.HTTP1\r\n                     Network.Wai.Handler.Warp.HTTP2\r\n                     Network.Wai.Handler.Warp.HTTP2.File\r\n                     Network.Wai.Handler.Warp.HTTP2.PushPromise\r\n                     Network.Wai.Handler.Warp.HTTP2.Request\r\n                     Network.Wai.Handler.Warp.HTTP2.Response\r\n                     Network.Wai.Handler.Warp.HTTP2.Types\r\n                     Network.Wai.Handler.Warp.HashMap\r\n                     Network.Wai.Handler.Warp.Header\r\n                     Network.Wai.Handler.Warp.IO\r\n                     Network.Wai.Handler.Warp.Imports\r\n                     Network.Wai.Handler.Warp.MultiMap\r\n                     Network.Wai.Handler.Warp.PackInt\r\n                     Network.Wai.Handler.Warp.ReadInt\r\n                     Network.Wai.Handler.Warp.Request\r\n                     Network.Wai.Handler.Warp.RequestHeader\r\n                     Network.Wai.Handler.Warp.Response\r\n                     Network.Wai.Handler.Warp.ResponseHeader\r\n                     Network.Wai.Handler.Warp.Run\r\n                     Network.Wai.Handler.Warp.SendFile\r\n                     Network.Wai.Handler.Warp.Settings\r\n                     Network.Wai.Handler.Warp.Types\r\n                     Network.Wai.Handler.Warp.Windows\r\n                     Network.Wai.Handler.Warp.WithApplication\r\n                     Paths_warp\r\n\r\n    Hs-Source-Dirs:  test, .\r\n    Type:            exitcode-stdio-1.0\r\n\r\n    Ghc-Options:     -Wall -threaded\r\n    Build-Tool-Depends: hspec-discover:hspec-discover\r\n    Build-Depends:   base >= 4.8 && < 5\r\n                   , QuickCheck\r\n                   , array\r\n                   , auto-update\r\n                   , bsb-http-chunked                         < 0.1\r\n                   , bytestring                >= 0.9.1.4\r\n                   , case-insensitive          >= 0.2\r\n                   , containers\r\n                   , directory\r\n                   , ghc-prim\r\n                   , hashable\r\n                   , hspec                     >= 1.3\r\n                   , http-client\r\n                   , http-date\r\n                   , http-types                >= 0.12\r\n                   , http2                     >= 3.0      && < 5\r\n                   , iproute                   >= 1.3.1\r\n                   , network\r\n                   , process\r\n                   , recv\r\n                   , simple-sendfile           >= 0.2.4    && < 0.3\r\n                   , stm                       >= 2.3\r\n                   , streaming-commons         >= 0.1.10\r\n                   , text\r\n                   , time-manager\r\n                   , unix-compat               >= 0.2\r\n                   , vault\r\n                   , wai                       >= 3.2      && < 3.3\r\n                   , word8\r\n                   , unliftio\r\n  if flag(x509)\r\n      Build-Depends: x509\r\n  if impl(ghc < 8)\r\n      Build-Depends: semigroups\r\n                   , transformers\r\n\r\n  if (os(linux) || os(freebsd) || os(darwin)) && flag(allow-sendfilefd)\r\n      Cpp-Options:   -DSENDFILEFD\r\n  if os(windows)\r\n      Cpp-Options:   -DWINDOWS\r\n      Build-Depends: time\r\n  else\r\n      Build-Depends: unix\r\n      Other-modules: Network.Wai.Handler.Warp.MultiMap\r\n  if impl(ghc >= 8)\r\n      Default-Extensions:  Strict StrictData\r\n  Default-Language:     Haskell2010\r\n\r\nBenchmark parser\r\n    Type:           exitcode-stdio-1.0\r\n    Main-Is:        Parser.hs\r\n    other-modules:  Network.Wai.Handler.Warp.Date\r\n                    Network.Wai.Handler.Warp.FdCache\r\n                    Network.Wai.Handler.Warp.FileInfoCache\r\n                    Network.Wai.Handler.Warp.HashMap\r\n                    Network.Wai.Handler.Warp.Imports\r\n                    Network.Wai.Handler.Warp.MultiMap\r\n                    Network.Wai.Handler.Warp.Types\r\n    HS-Source-Dirs: bench .\r\n    Build-Depends:  base >= 4.8 && < 5\r\n                  , auto-update\r\n                  , bytestring\r\n                  , containers\r\n                  , gauge\r\n                  , hashable\r\n                  , http-date\r\n                  , http-types\r\n                  , network\r\n                  , network\r\n                  , recv\r\n                  , time-manager\r\n                  , unix-compat\r\n                  , unliftio\r\n  if flag(x509)\r\n      Build-Depends: x509\r\n  if impl(ghc < 8)\r\n      Build-Depends: semigroups\r\n\r\n  if (os(linux) || os(freebsd) || os(darwin)) && flag(allow-sendfilefd)\r\n    Cpp-Options:   -DSENDFILEFD\r\n    Build-Depends: unix\r\n  if os(windows)\r\n    Cpp-Options:   -DWINDOWS\r\n    Build-Depends: time\r\n  if impl(ghc >= 8)\r\n      Default-Extensions:  Strict StrictData\r\n  Default-Language:     Haskell2010\r\n\r\nSource-Repository head\r\n  Type:     git\r\n  Location: git://github.com/yesodweb/wai.git\r\n";
    }