{-# LANGUAGE TemplateHaskell #-}

module  CMS.Infra.Type where

import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Except
import System.IO
import Control.Lens

import qualified CMS.Domain.Model.Type as DM


data AppData = AppData {
               _inputHandleAppData :: Handle 
             }

makeLenses ''AppData

defaultAppData :: IO AppData
defaultAppData = do
  return AppData {
           _inputHandleAppData = stdin
         } 

-- |
--
type AppContext = ReaderT AppData (ReaderT DM.DomainData (ExceptT DM.ErrorData (LoggingT IO)))

-- |
--
type IOTask = IO

