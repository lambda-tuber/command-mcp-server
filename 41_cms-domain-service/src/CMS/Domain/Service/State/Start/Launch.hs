{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CMS.Domain.Service.State.Start.Launch where

import CMS.Domain.Service.Type


-- |
--
instance IStateActivity StartStateData LaunchEventData
  -- @see default implementation in Type module.

