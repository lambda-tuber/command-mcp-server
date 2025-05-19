{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CMS.Domain.Service.State.Start.Terminate where

import CMS.Domain.Service.Type


-- |
--
instance IStateActivity StartStateData TerminateEventData
  -- @see default implementation in Type module.

