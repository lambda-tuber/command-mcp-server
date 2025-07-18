{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module CMS.Domain.Service.Core where

import Control.Monad.Logger
import Data.Conduit
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Lens
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import Control.Monad.Trans.State.Lazy
import qualified Data.Text as T
import Control.Monad.Except

import qualified CMS.Domain.Model.Type as DM
import qualified CMS.Domain.Model.Constant as DM

import CMS.Domain.Service.Type
import CMS.Domain.Service.TH
import CMS.Domain.Service.State.Start()
import CMS.Domain.Service.State.Run()
import CMS.Domain.Service.State.Stop()

-- |
--
funcTH_transit


-- |
--
app :: AppContext ()
app = do
  $logDebugS DM._LOGTAG "app called."
  runConduit pipeline
  where
    pipeline :: ConduitM () Void AppContext ()
    pipeline = src .| work .| sink

---------------------------------------------------------------------------------
-- |
--
src :: ConduitT () DM.McpRequest AppContext ()
src = lift go >>= yield >> src
  where
    go :: AppContext DM.McpRequest
    go = do
      queue <- view DM.requestQueueDomainData <$> lift ask
      req <- liftIO $ STM.atomically $ STM.readTQueue queue
      $logDebugS DM._LOGTAG $ T.pack $ "src: req: " ++ show req
      return req

---------------------------------------------------------------------------------
-- |
--
work :: ConduitT DM.McpRequest EventW AppContext ()
work = await >>= \case
  Just reqBS -> flip catchError errHdl $
    lift (go reqBS) >>= yield >> work
  Nothing -> do
    $logWarnS DM._LOGTAG "work: await returns nothing. skip."
    work
  where
    errHdl :: String -> ConduitT DM.McpRequest EventW AppContext ()
    errHdl msg = do
      $logWarnS DM._LOGTAG $ T.pack $ "work: exception occurred. skip. " ++ msg
      work


    go :: DM.McpRequest -> AppContext EventW
    go req = req2ev req

-- |
--
req2ev :: DM.McpRequest -> AppContext EventW
req2ev (DM.McpInitializeRequest dat) = pure . EventW . InitEvent . InitEventData $ dat
req2ev (DM.McpInitializedNotification dat) = pure . EventW . InitializedEvent . InitializedEventData $ dat
req2ev (DM.McpToolsListRequest dat) = pure . EventW . ToolsListEvent . ToolsListEventData $ dat
req2ev (DM.McpToolsCallRequest dat) = pure . EventW . ToolsCallEvent . ToolsCallEventData $ dat
-- req2ev x = throwError $ "unsupported request: " ++ show x


---------------------------------------------------------------------------------
-- |
--
sink :: ConduitT EventW Void AppContext ()
sink = await >>= \case
  Just ev -> flip catchError errHdl $ do
    lift (go ev) >> sink
  Nothing -> do
    $logWarnS DM._LOGTAG "sink: await returns nothing. skip."
    sink

  where
    errHdl :: String -> ConduitT EventW Void AppContext ()
    errHdl msg = do
      $logWarnS DM._LOGTAG $ T.pack $ "sink: exception occurred. skip. " ++ msg
      sink
      
    go :: EventW -> AppContext ()
    go ev = get >>= flip actionSW ev >>= \case
      Nothing -> return ()
      Just st -> transit st
