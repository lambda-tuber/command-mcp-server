{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module CMS.Domain.Service.ControlSpec (spec) where

import Test.Hspec
import Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import Control.Lens
import Data.Default

import qualified CMS.Domain.Model.Type as DM
import qualified CMS.Domain.Service.Control as SUT
import qualified CMS.Domain.Service.Type as SUT


-- |
--
data SpecContext = SpecContext {
                   _domainDataSpecContext :: DM.DomainData
                 , _appDataSpecContext :: SUT.AppStateW
                 }

makeLenses ''SpecContext

defaultSpecContext :: IO SpecContext
defaultSpecContext = do
  domDat <- DM.defaultDomainData
  let appDat = SUT.AppStateW SUT.StartState
  return SpecContext {
           _domainDataSpecContext = domDat
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

  domDat <- DM.defaultDomainData
  return ctx {  _domainDataSpecContext = domDat
              , _appDataSpecContext    = SUT.AppStateW SUT.StartState
              }

-- |
--
tearDown :: SpecContext -> IO ()
tearDown _ = do
  putStrLn "[INFO] 各試験の終了後に実施"

-- |
--
run :: SpecWith SpecContext
run = do
  describe "runApp" $ do
    context "when AppData default" $ do
      it "should be run" $ \ctx -> do 
        putStrLn "[INFO] 1件目の試験を実施"

        let domDat = ctx^.domainDataSpecContext
            appDat = ctx^.appDataSpecContext
            reqQ   = domDat^.DM.requestQueueDomainData
            resQ   = domDat^.DM.responseQueueDomainData
            args   = DM.McpInitializeRequest def
            expect = 7
            
        thId <- async $ SUT.runWithAppData appDat domDat

        STM.atomically $ STM.writeTQueue reqQ args

        actual <- STM.atomically $ STM.readTQueue resQ
        putStrLn $ show actual
        -- actual `shouldBe` expect

        cancel thId

{-
      it "should be 11" $ \ctx -> do 
        putStrLn "[INFO] 1件目の試験を実施"

        let domDat = ctx^.domainDataSpecContext
            appDat = ctx^.appDataSpecContext
            reqQ   = domDat^.DM.requestQueueDomainData
            resQ   = domDat^.DM.responseQueueDomainData
            cmdQ   = domDat^.DM.commandQueueDomainData
            args   = "11"
            expect = 11
            
        thId <- async $ SUT.runWithAppData appDat domDat

        STM.atomically $ STM.writeTQueue reqQ args

        (DM.InitializeCommand cmdDat) <- STM.atomically $ STM.readTQueue cmdQ
        let callback = cmdDat^.DM.callbackInitializeCommandData
        callback 11

        actual <- STM.atomically $ STM.readTQueue resQ
        actual `shouldBe` expect

        cancel thId
-}
