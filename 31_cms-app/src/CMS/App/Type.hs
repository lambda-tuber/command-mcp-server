{-# LANGUAGE TemplateHaskell #-}


module CMS.App.Type where

import Data.Data
import Data.Default
import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Except

import qualified CMS.Domain.Model.Type as DM


--------------------------------------------------------------------------------
-- |
--
data ArgData = ArgData {
    _yamlArgData :: Maybe FilePath
  } deriving (Data, Show, Read, Eq)

makeLenses ''ArgData
$(deriveJSON
  defaultOptions {
      fieldLabelModifier = tail . reverse . drop (length "ArgData") . reverse
    }
  ''ArgData)

instance Default ArgData where
  def = ArgData {
        _yamlArgData = Nothing
      }


--------------------------------------------------------------------------------
-- |
--
data ConfigData = ConfigData {
    _logDirConfigData :: Maybe FilePath
  , _logLevelConfigData :: LogLevel
  , _scriptsDirConfigData :: FilePath
  } deriving (Show, Read, Eq)

makeLenses ''ConfigData
$(deriveJSON
  defaultOptions {
      fieldLabelModifier = tail . reverse . drop (length "ConfigData") . reverse
    }
  ''ConfigData)

instance Default ConfigData where
  def = ConfigData {
        _logDirConfigData  = Nothing
      , _logLevelConfigData = LevelDebug
      , _scriptsDirConfigData = "./scripts"
      }


--------------------------------------------------------------------------------
-- |
--
data AppData = AppData {
               _appsAppData :: [DM.DomainContext ()]
             }
instance Default AppData where
  def = AppData {
          _appsAppData = []
        }

makeLenses ''AppData

-- |
--
type AppContext = ReaderT AppData (ReaderT DM.DomainData (ExceptT DM.ErrorData (LoggingT IO)))
