{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module CMS.UI.Request.Core where

import Control.Monad.Logger
import Data.Conduit
import qualified Data.ByteString.Lazy as B
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Lens
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import qualified Data.Text as T
--import Text.Parsec
import Data.Aeson
import Control.Monad.Except

import qualified CMS.Domain.Model.Type as DM
import qualified CMS.Domain.Model.Utility as DM
import qualified CMS.Domain.Model.Constant as DM

import CMS.UI.Request.Type
import CMS.UI.Request.Utility

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
src :: ConduitT () B.ByteString AppContext ()
src = lift go >>= yield >> src
  where
    go :: AppContext B.ByteString
    go = do
      hdl <- view inputHandleAppData <$> ask
      bs <- readLineL hdl
      $logDebugS DM._LOGTAG $ T.pack $ "src: " ++ DM.lbs2str bs
      return bs

---------------------------------------------------------------------------------
-- |
--
work :: ConduitT B.ByteString DM.JsonRpcRequest AppContext ()
work = await >>= \case
  Just reqBS -> flip catchError errHdl $ do
    lift (go reqBS) >>= yield >> work
  Nothing  -> do
    $logWarnS DM._LOGTAG "work: await returns nothing. skip."
    work
    
  where
    errHdl :: String -> ConduitT B.ByteString DM.JsonRpcRequest AppContext ()
    errHdl msg = do
      $logWarnS DM._LOGTAG $ T.pack $ "work: exception. skip. " ++ msg
      work

    go :: B.ByteString -> AppContext DM.JsonRpcRequest
    go = liftEither . eitherDecode

---------------------------------------------------------------------------------
-- |
--
sink :: ConduitT DM.JsonRpcRequest Void AppContext ()
sink = await >>= \case
  Just req -> flip catchError errHdl $ do
    lift (go req) >> sink
  Nothing  -> do
    $logWarnS DM._LOGTAG "sink: await returns nothing. skip."
    sink

  where
    errHdl :: String -> ConduitT DM.JsonRpcRequest Void AppContext ()
    errHdl msg = do
      $logWarnS DM._LOGTAG $ T.pack $ "sink: exception skip. " ++ msg
      sink

    go :: DM.JsonRpcRequest -> AppContext ()
    go req = do
      let method  = req^.DM.methodJsonRpcRequest
          rawJson = req^.DM.paramsJsonRpcRequest
      reqDat <- decodeReq method rawJson req
      enq reqDat

-- |
--
enq :: DM.McpRequest -> AppContext ()
enq req = do
  queue <- view DM.requestQueueDomainData <$> lift ask
  liftIO $ STM.atomically $ STM.writeTQueue queue req

-- |
--
-- {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26","capabilities":{"roots":{"listChanged":true}},"clientInfo":{"name":"Visual Studio Code","version":"1.100.2"}}} @(cms-ui-request-0.1.0.0-d3d6f94ec8cad8f11e6c82557d1e8927ebf7b47db964c68a0c1101f4c8759996:CMS.UI.Request.Core src/CMS/UI/Request/Core.hs:47:8)
-- {"method":"notifications/initialized","jsonrpc":"2.0"} @(cms-ui-request-0.1.0.0-d3d6f94ec8cad8f11e6c82557d1e8927ebf7b47db964c68a0c1101f4c8759996:CMS.UI.Request.Core src/CMS/UI/Request/Core.hs:47:8)
-- {"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}} @(cms-ui-request-0.1.0.0-d3d6f94ec8cad8f11e6c82557d1e8927ebf7b47db964c68a0c1101f4c8759996:CMS.UI.Request.Core src/CMS/UI/Request/Core.hs:47:8)
-- {"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"df","arguments":{"arg1":""},"_meta":{"progressToken":"df68feeb-d5b0-4807-abac-ad2eb5d628bd"}}} @(cms-ui-request-0.1.0.0-d3d6f94ec8cad8f11e6c82557d1e8927ebf7b47db964c68a0c1101f4c8759996:CMS.UI.Request.Core src/CMS/UI/Request/Core.hs:47:8)
--
decodeReq :: String -> Maybe DM.RawJsonByteString -> DM.JsonRpcRequest -> AppContext DM.McpRequest
decodeReq "initialize" (Just (DM.RawJsonByteString rawJson)) req = DM.McpInitializeRequest . DM.McpInitializeRequestData req <$> (liftEither (eitherDecode rawJson))
decodeReq "notifications/initialized" Nothing req = pure . DM.McpInitializedNotification . DM.McpInitializedNotificationData $ req
decodeReq "tools/list" _ req = pure . DM.McpToolsListRequest . DM.McpToolsListRequestData $ req
decodeReq "tools/call" (Just (DM.RawJsonByteString rawJson)) req = DM.McpToolsCallRequest . DM.McpToolsCallRequestData req <$> (liftEither (eitherDecode rawJson))
decodeReq _ _ req = throwError $ "unsupported method: " ++ show req

