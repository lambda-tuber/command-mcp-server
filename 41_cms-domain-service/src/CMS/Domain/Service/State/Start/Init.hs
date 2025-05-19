{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module CMS.Domain.Service.State.Start.Init where

import Data.Default
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Lens
import qualified Data.Text as T
import qualified Control.Concurrent.STM as STM

import qualified CMS.Domain.Model.Type as DM
import qualified CMS.Domain.Model.Constant as DM

import CMS.Domain.Service.Type


-- |
--
instance IStateActivity StartStateData InitEventData where
  action s (InitEvent r@(InitEventData dat)) = do
    $logDebugS DM._LOGTAG "Start init called."
    $logDebugS DM._LOGTAG (T.pack (show s))
    $logDebugS DM._LOGTAG (T.pack (show r))

    let jsonRpc = dat^.DM.jsonrpcMcpInitializeRequestData
        resDat  = DM.McpInitializeResponseData jsonRpc def
        res     = DM.McpInitializeResponse resDat

    $logDebugS DM._LOGTAG $ T.pack $ show res

    queue <- view DM.responseQueueDomainData <$> lift ask
    liftIO $ STM.atomically $ STM.writeTQueue queue res

    return Nothing

