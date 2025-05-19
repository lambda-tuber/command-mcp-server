{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module CMS.Domain.Service.State.Start where

import Control.Monad.Logger

import qualified CMS.Domain.Model.Constant as DM

import CMS.Domain.Service.Type
import CMS.Domain.Service.TH
import CMS.Domain.Service.State.Start.Init()
import CMS.Domain.Service.State.Start.Launch()
import CMS.Domain.Service.State.Start.Disconnect()
import CMS.Domain.Service.State.Start.Terminate()

{-
-- |
--
instance IAppState StartStateData where
  actionS s (EventW r@EntryEvent{})      = action s r
  actionS s (EventW r@ExitEvent{})       = action s r
  actionS s (EventW r@TransitEvent{})    = action s r
  actionS s (EventW r@InitEvent{})       = action s r
  actionS s (EventW r@LaunchEvent{})     = action s r
  actionS s (EventW r@DisconnectEvent{}) = action s r
  actionS s (EventW r@TerminateEvent{})  = action s r
-}
instanceTH_IAppState ''StartStateData

-- |
--
instance IStateActivity StartStateData EntryEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "Start entry called."
    return Nothing

-- |
--
instance IStateActivity StartStateData ExitEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "Start exit called."
    return Nothing

-- |
--
instance IStateActivity StartStateData TransitEventData
  -- @see default implementation in Type module.


-- |
--
instance IStateActivity StartStateData InitializedEventData where
  action _ _ = do
    $logDebugS DM._LOGTAG "initialized called."
    return $ Just StartToRun

-- |
--
instance IStateActivity StartStateData ToolsListEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StartStateData ToolsCallEventData
  -- @see default implementation in Type module.
