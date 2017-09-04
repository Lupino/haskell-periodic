{-# LANGUAGE ScopedTypeVariables #-}

-- | This module provides convenience functions for interfacing @tls@.
--
-- This module is intended to be imported @qualified@, e.g.:
--
module Periodic.Transport.TLS
  (
    makeTLSTransport
    -- * re-export
  , module Periodic.Transport.TLSSetting
  ) where

import           Control.Exception             (SomeException, bracketOnError,
                                                catch)
import qualified Data.ByteString.Char8         as B (append, length, null, pack)
import qualified Data.ByteString.Lazy          as BL (fromStrict)
import           Network.TLS                   (Context, TLSParams)
import qualified Network.TLS                   as TLS
import           Periodic.Transport            (Transport (..))
import           Periodic.Transport.TLSSetting


tLsToTransport :: TLS.Context -> Transport
tLsToTransport ctx = Transport { recvData = const $ TLS.recvData ctx
                               , sendData = TLS.sendData ctx . BL.fromStrict
                               , close    = closeTLS ctx
                               }

transportBackend :: Transport -> TLS.Backend
transportBackend transport = TLS.Backend
  { TLS.backendFlush = return ()
  , TLS.backendClose = close transport
  , TLS.backendSend = sendData transport
  , TLS.backendRecv = recvData'
  }

  where recvData' nbytes = do
         s <- recvData transport nbytes
         if loadMore nbytes s then do
                              s' <- recvData' (nbytes - B.length s)
                              return $ s `B.append` s'
                              else return s

        loadMore nbytes bs | B.null bs = False
                           | B.length bs < nbytes = True
                           | otherwise = False


-- | Close a TLS 'Context' and its underlying socket.
--
closeTLS :: Context -> IO ()
closeTLS ctx = (TLS.bye ctx >> TLS.contextClose ctx) -- sometimes socket was closed before 'TLS.bye'
    `catch` (\(_::SomeException) -> return ())   -- so we catch the 'Broken pipe' error here


-- | Convenience function for initiating an TLS transport
--
-- This operation may throw 'TLS.TLSException' on failure.
--
makeTLSTransport :: TLSParams params => params -> Transport -> IO Transport
makeTLSTransport prms trp =
  bracketOnError (TLS.contextNew (transportBackend trp) prms) closeTLS $ \ctx -> do
    TLS.handshake ctx
    return $ tLsToTransport ctx
