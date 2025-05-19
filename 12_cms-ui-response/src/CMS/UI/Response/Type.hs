{-# LANGUAGE TemplateHaskell #-}

module  CMS.UI.Response.Type where

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Except
import Data.Default
import System.IO
import Control.Lens

import qualified CMS.Domain.Model.Type as DM


data AppData = AppData {
               _outputHandleAppData :: Handle  
             }

makeLenses ''AppData

instance Default AppData where
  def = AppData {
          _outputHandleAppData = stdout
        }

-- |
--
type AppContext = ReaderT AppData (ReaderT DM.DomainData (ExceptT DM.ErrorData (LoggingT IO)))
