{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CMS.Domain.Service.State.Start.Disconnect where

import CMS.Domain.Service.Type

-- |
--
instance IStateActivity StartStateData DisconnectEventData
  -- @see default implementation in Type module.

