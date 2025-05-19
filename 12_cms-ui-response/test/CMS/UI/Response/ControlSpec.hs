{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module CMS.UI.Response.ControlSpec (spec) where

import Test.Hspec
import Data.Default
import Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import System.IO
import System.Posix.IO
import Control.Lens

import qualified CMS.Domain.Model.Type as DM
import qualified CMS.UI.Response.Control as SUT
import qualified CMS.UI.Response.Type as SUT

-- |
--
data SpecContext = SpecContext {
                   _handlePairSpecContext :: (Handle, Handle) 
                 , _domainDataSpecContext :: DM.DomainData
                 , _appDataSpecContext :: SUT.AppData
                 }

makeLenses ''SpecContext

defaultSpecContext :: IO SpecContext
defaultSpecContext = do
  domDat <- DM.defaultDomainData
  let appDat = def
  return SpecContext {
           _handlePairSpecContext = (stdin, stdout) 
         , _domainDataSpecContext = domDat
         , _appDataSpecContext    = appDat
         }

-- |
--
spec :: Spec
spec = do
  runIO $ putStrLn "Start Spec."
  beforeAll setUpOnce $ 
    afterAll tearDownOnce . 
      beforeWith setUp . 
        after tearDown $ run

-- |
--
setUpOnce :: IO SpecContext
setUpOnce = do
  putStrLn "[INFO] すべての試験開始前に1回だけ実施"
  defaultSpecContext

-- |
--
tearDownOnce :: SpecContext -> IO ()
tearDownOnce _ = do
  putStrLn "[INFO] すべての試験終了後に1回だけ実施"

-- |
--
setUp :: SpecContext -> IO SpecContext
setUp ctx = do
  putStrLn "[INFO] 各試験の開始前に実施"

  (readFd, writeFd) <- createPipe
  readH  <- fdToHandle readFd
  writeH <- fdToHandle writeFd
  hSetBuffering readH NoBuffering
  hSetBuffering writeH NoBuffering

  domDat <- DM.defaultDomainData
  let appDat = ctx^.appDataSpecContext
  return ctx {
                _handlePairSpecContext = (readH, writeH)
              , _domainDataSpecContext = domDat
              , _appDataSpecContext    = appDat {SUT._outputHandleAppData = writeH}
              }

-- |
--
tearDown :: SpecContext -> IO ()
tearDown ctx = do
  putStrLn "[INFO] 各試験の終了後に実施"
  hClose $ fst $ ctx^.handlePairSpecContext
  hClose $ snd $ ctx^.handlePairSpecContext

-- |
--
run :: SpecWith SpecContext
run = do
  describe "runApp" $ do
    context "when AppData default" $ do
      it "should be run" $ \ctx -> do 
        putStrLn "[INFO] 1件目の試験を実施"

        let readH  = fst $ ctx^.handlePairSpecContext
            domDat = ctx^.domainDataSpecContext
            appDat = ctx^.appDataSpecContext
            queue   = domDat^.DM.responseQueueDomainData
            expect = DM.McpInitializeResponse def 
            
        thId <- async $ SUT.runWithAppData appDat domDat

        STM.atomically $ STM.writeTQueue queue expect

        actual <- hGetLine readH
        putStrLn $ show actual
        -- actual `shouldBe` expect

        cancel thId
      
