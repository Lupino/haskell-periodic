module Periodic.Server.GrabQueue
  ( GrabQueue
  , newGrabQueue
  , pushAgent
  , popAgentSTM
  , popAgentList
  ) where


import           Data.ByteString (ByteString)
import           Data.IOMap      (IOMap)
import qualified Data.IOMap      as IOMap
import qualified Data.IOMap.STM  as IOMapS
import           Data.List       (nub)
import           Periodic.Types  (FuncName)
import           UnliftIO        (MonadIO, STM, retrySTM)

newtype GrabQueue = GrabQueue (IOMap FuncName [ByteString])

newGrabQueue :: MonadIO m => m GrabQueue
newGrabQueue = GrabQueue <$> IOMap.empty

pushAgent :: MonadIO m => GrabQueue -> [FuncName] -> ByteString -> m ()
pushAgent (GrabQueue q) funcList k =
  mapM_ (\fn -> IOMap.insertLookupWithKey f fn [k] q) funcList
  where f _ nv ov = nub $ nv ++ ov

popAgentSTM :: GrabQueue -> FuncName -> STM ByteString
popAgentSTM (GrabQueue q) fn = do
  ks <- IOMapS.insertLookupWithKey f fn [] q
  case ks of
    Nothing    -> retrySTM
    Just []    -> retrySTM
    Just (x:_) -> return x

  where f _ _ []     = []
        f _ _ (_:xs) = xs

popAgentList :: MonadIO m => GrabQueue -> FuncName -> m [ByteString]
popAgentList (GrabQueue q) fn = do
  ks <- IOMap.insertLookupWithKey f fn [] q
  case ks of
    Nothing -> pure []
    Just xs -> pure xs
  where f _ _ _ = []
