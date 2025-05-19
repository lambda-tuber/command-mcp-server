{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module CMS.Domain.Service.State.Run where


import System.IO
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Logger
import qualified Data.Text as T
import Control.Lens
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import System.FilePath
import System.Exit

import qualified CMS.Domain.Model.Type as DM
import qualified CMS.Domain.Model.Constant as DM

import CMS.Domain.Service.Type
import CMS.Domain.Service.TH
import CMS.Domain.Service.Constant
import qualified CMS.Domain.Service.Utility as U



instanceTH_IAppState ''RunStateData

-- |
--
instance IStateActivity RunStateData EntryEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "Run entry called."
    return Nothing

-- |
--
instance IStateActivity RunStateData ExitEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "Run exit called."
    return Nothing

-- |
--
instance IStateActivity RunStateData TransitEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity RunStateData InitEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity RunStateData LaunchEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "Run launch called."
    return Nothing

-- |
--
instance IStateActivity RunStateData DisconnectEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "Run discoonect called."
    return $ Just RunToStop

-- |
--
instance IStateActivity RunStateData TerminateEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity RunStateData InitializedEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity RunStateData ToolsListEventData where
  action s (ToolsListEvent r@(ToolsListEventData dat)) = do
    $logDebugS DM._LOGTAG "Run ToolsListEvent called."
    $logDebugS DM._LOGTAG (T.pack (show s))
    $logDebugS DM._LOGTAG (T.pack (show r))

    scriptsDir <- view DM.scriptsDirDomainData <$> lift ask

    let toolFile = scriptsDir </> _TOOLS_LIST_FILE
    cont <- U.readFile toolFile
    let result = DM.McpToolsListResponseResult $ DM.RawJsonByteString cont
        jsonRpc = dat^.DM.jsonrpcMcpToolsListRequestData
        resDat = DM.McpToolsListResponseData jsonRpc result
        res = DM.McpToolsListResponse resDat

    $logDebugS DM._LOGTAG $ T.pack $ show res

    queue <- view DM.responseQueueDomainData <$> lift ask
    liftIO $ STM.atomically $ STM.writeTQueue queue res

    return noStateTransition

-- |
--
instance IStateActivity RunStateData ToolsCallEventData where
  action _ (ToolsCallEvent (ToolsCallEventData dat)) = do
    $logDebugS DM._LOGTAG "Run ToolsCallEvent called."

    resQ <- view DM.responseQueueDomainData <$> lift ask
    cmdQ <- view DM.commandQueueDomainData <$> lift ask

    let cmdDat = DM.SystemCommandData {
                   DM._nameSystemCommandData = dat^.DM.paramsMcpToolsCallRequestData^.DM.nameMcpToolsCallRequestDataParams
                 , DM._argumentsSystemCommandData = dat^.DM.paramsMcpToolsCallRequestData^.DM.argumentsMcpToolsCallRequestDataParams
                 , DM._callbackSystemCommandData = callback resQ
                 }
        cmd = DM.SystemCommand cmdDat

    liftIO $ STM.atomically $ STM.writeTQueue cmdQ cmd

    return noStateTransition
    
    where
      -- |
      --
      callback :: STM.TQueue DM.McpResponse -> DM.SystemCommandCallback ()
      callback resQ code outStr errStr = do
        hPutStrLn stderr $ "[INFO] CMS.Domain.Service.State.Run.callback called."

        let jsonRpc = dat^.DM.jsonrpcMcpToolsCallRequestData
            content = [ DM.McpToolsCallResponseResultContent "text" outStr
                      , DM.McpToolsCallResponseResultContent "text" errStr
                      ]
            result = DM.McpToolsCallResponseResult {
                       DM._contentMcpToolsCallResponseResult = content
                     , DM._isErrorMcpToolsCallResponseResult = (ExitSuccess /= code)
                     }
            resDat = DM.McpToolsCallResponseData jsonRpc result
            res = DM.McpToolsCallResponse resDat

        STM.atomically $ STM.writeTQueue resQ res

        hPutStrLn stderr $ "[INFO] CMS.Domain.Service.State.Run.callback end."
