{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module CMS.Infra.Core where

import System.IO
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Lens
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import Data.Conduit
import Control.Concurrent.Async
import qualified Data.Text as T
import Control.Monad.Except
import System.Process
import System.FilePath
import Data.Aeson

import qualified Control.Concurrent as C

import qualified CMS.Domain.Model.Type as DM
import qualified CMS.Domain.Model.Constant as DM

import CMS.Infra.Type
import CMS.Infra.Utility


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
src :: ConduitT () DM.Command AppContext ()
src = lift go >>= yield >> src
  where
    go :: AppContext DM.Command
    go = do
      queue <- view DM.commandQueueDomainData <$> lift ask
      liftIO $ STM.atomically $ STM.readTQueue queue

---------------------------------------------------------------------------------
-- |
--
work :: ConduitT DM.Command (IOTask ()) AppContext ()
work = await >>= \case
  Just cmd -> flip catchError errHdl $ do
    lift (go cmd) >>= yield >> work
  Nothing -> do
    $logWarnS DM._LOGTAG "work: await returns nothing. skip."
    work

  where
    errHdl :: String -> ConduitT DM.Command (IOTask ()) AppContext ()
    errHdl msg = do
      $logWarnS DM._LOGTAG $ T.pack $ "work: exception occurred. skip. " ++ msg
      work

    go :: DM.Command -> AppContext (IOTask ())
    go (DM.InitializeCommand dat) = return $ task dat
    go (DM.SystemCommand dat) = systemTask dat
    go _ = throwError $ "work: unsupported command. "

    task :: DM.InitializeCommandData -> IOTask ()
    task dat = do
      let params = dat^.DM.paramsInitializeCommandData
          callback = dat^.DM.callbackInitializeCommandData

      hPutStrLn stderr $ "[INFO] CMS.Infra.Core.work.task start. wait " ++ show params ++ " sec."
      C.threadDelay (params * 1000 * 1000)
      hPutStrLn stderr "[INFO] CMS.Infra.Core.work.task end."
      callback params


-- |
--
systemTask :: DM.SystemCommandData -> AppContext (IOTask ())
systemTask dat = do
  scriptsDir <- view DM.scriptsDirDomainData <$> lift ask

  let name = dat^.DM.nameSystemCommandData
      callback = dat^.DM.callbackSystemCommandData
      argsBS = DM.unRawJsonByteString $ dat^.DM.argumentsSystemCommandData
  
  args <- liftEither $ eitherDecode $ argsBS

  let cmd = scriptsDir </> name ++ ".sh" ++ " " ++ (args^.DM.argsMcpToolsCallRequestDataParamsSystemArgs)

  $logDebugS DM._LOGTAG $ T.pack $ "systemTask: system cmd. " ++ cmd
  return $ systemTaskIO cmd callback

-- |
--
systemTaskIO :: String -> DM.SystemCommandCallback () -> IOTask ()
systemTaskIO cmd callback = do
  hPutStrLn stderr $ "[INFO] CMS.Infra.Core.work.systemTask run. " ++ cmd

  (code, out, err) <- readCreateProcessWithExitCode (shell cmd) ""

  callback code out err

  hPutStrLn stderr "[INFO] CMS.Infra.Core.work.systemTask end."

---------------------------------------------------------------------------------
-- |
--
sink :: ConduitT (IOTask ()) Void AppContext ()
sink = await >>= \case
  Just req -> flip catchError errHdl $ do
    lift (go req) >> sink
  Nothing -> do
    $logWarnS DM._LOGTAG "sink: await returns nothing. skip."
    sink

  where
    errHdl :: String -> ConduitT (IOTask ()) Void AppContext ()
    errHdl msg = do
      $logWarnS DM._LOGTAG $ T.pack $ "sink: exception occurred. skip. " ++ msg
      sink

    go :: (IO ()) -> AppContext ()
    go t = do
      $logDebugS DM._LOGTAG "sink: start async."
      _ <- liftIOE $ async t
      $logDebugS DM._LOGTAG "sink: end async."
      return ()


