-- | Helpers for setting up a tls connection with @tls@ package,
-- for further customization, please refer to @tls@ package.
--
-- Note, functions in this module will throw error if can't load certificates or CA store.
--
{-# LANGUAGE OverloadedStrings #-}
module Periodic.Transport.TLSSetting
  (
    -- * Make TLS settings
    makeClientParams
  , makeClientParams'
  , makeServerParams
  , makeServerParams'
  ) where

import qualified Data.ByteString            as B (empty, readFile)
import           Data.Default.Class         (def)
import qualified Data.PEM                   as X509 (pemContent, pemParseBS)
import qualified Data.X509                  as X509 (CertificateChain (..),
                                                     decodeSignedCertificate)
import qualified Data.X509.CertificateStore as X509 (CertificateStore,
                                                     makeCertificateStore)
import qualified Data.X509.Validation       as X509 (ServiceID, validateDefault)
import qualified Network.TLS                as TLS
import qualified Network.TLS.Extra          as TLS (ciphersuite_strong)



makeCAStore :: FilePath -> IO X509.CertificateStore
makeCAStore fp = do
  bs <- B.readFile fp
  let Right pems = X509.pemParseBS bs
  case mapM (X509.decodeSignedCertificate . X509.pemContent) pems of
    Right cas -> return (X509.makeCertificateStore cas)
    Left err  -> error err

-- | make a simple tls 'TLS.ClientParams' that will validate server and use tls connection
-- without providing client's own certificate. suitable for connecting server which don't
-- validate clients.
--
-- we defer setting of 'TLS.clientServerIdentification' to connecting phase.
--
-- Note, tls's default validating method require server has v3 certificate.
-- you can use openssl's V3 extension to issue such a certificate. or change 'TLS.ClientParams'
-- before connecting.
--
makeClientParams :: FilePath          -- ^ trusted certificates.
                 -> X509.ServiceID
                 -> IO TLS.ClientParams
makeClientParams tca servid = do
  caStore <- makeCAStore tca
  return (TLS.defaultParamsClient "" B.empty)
    { TLS.clientSupported = def { TLS.supportedCiphers = TLS.ciphersuite_strong }
    , TLS.clientServerIdentification = servid
    , TLS.clientShared    = def
      { TLS.sharedCAStore         = caStore
      , TLS.sharedValidationCache = def
      }
    }

-- | make a simple tls 'TLS.ClientParams' that will validate server and use tls connection
-- while providing client's own certificate as well. suitable for connecting server which
-- validate clients.
--
-- Also only accept v3 certificate.
--
makeClientParams' :: FilePath       -- ^ public certificate (X.509 format).
                  -> [FilePath]     -- ^ chain certificates (X.509 format).
                                    --   the root of your certificate chain should be
                                    --   already trusted by server, or tls will fail.
                  -> FilePath       -- ^ private key associated.
                  -> FilePath       -- ^ trusted certificates.
                  -> X509.ServiceID
                  -> IO TLS.ClientParams
makeClientParams' pub certs priv tca servid = do
  p <- makeClientParams tca servid
  c <- TLS.credentialLoadX509Chain pub certs priv
  case c of
    Right c' ->
      return p
        { TLS.clientShared = (TLS.clientShared p)
          {
            TLS.sharedCredentials = TLS.Credentials [c']
          }
        , TLS.clientHooks = (TLS.clientHooks p)
          {
            TLS.onCertificateRequest = const . return $ Just c'
          }
        }
    Left err -> error err

-- | make a simple tls 'TLS.ServerParams' without validating client's certificate.
--
makeServerParams :: FilePath        -- ^ public certificate (X.509 format).
                 -> [FilePath]      -- ^ chain certificates (X.509 format).
                                    --   the root of your certificate chain should be
                                    --   already trusted by client, or tls will fail.
                 -> FilePath        -- ^ private key associated.
                 -> IO TLS.ServerParams
makeServerParams pub certs priv = do
  c <- TLS.credentialLoadX509Chain pub certs priv
  case c of
    Right c'@(X509.CertificateChain c'', _) ->
      return def
        { TLS.serverCACertificates =  c''
        , TLS.serverShared = def
          {
            TLS.sharedCredentials = TLS.Credentials [c']
          }
        , TLS.serverSupported = def { TLS.supportedCiphers = TLS.ciphersuite_strong }
        }
    Left err -> error err

-- | make a tls 'TLS.ServerParams' that also validating client's certificate.
--
makeServerParams' :: FilePath       -- ^ public certificate (X.509 format).
                  -> [FilePath]     -- ^ chain certificates (X.509 format).
                  -> FilePath       -- ^ private key associated.
                  -> FilePath       -- ^ server will use these certificates to validate clients.
                  -> X509.ServiceID
                  -> IO TLS.ServerParams
makeServerParams' pub certs priv tca servid = do
  caStore <- makeCAStore tca
  p <- makeServerParams pub certs priv
  return p
    { TLS.serverWantClientCert = True
    , TLS.serverShared = (TLS.serverShared p)
      {   TLS.sharedCAStore = caStore
      }
    , TLS.serverHooks = def
      { TLS.onClientCertificate = \chain -> do
        errs <- X509.validateDefault caStore def servid chain
        case errs of
          [] -> return TLS.CertificateUsageAccept
          xs -> return . TLS.CertificateUsageReject . TLS.CertificateRejectOther $ show xs
      }
    }
