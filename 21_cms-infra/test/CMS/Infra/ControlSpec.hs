{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module CMS.Infra.ControlSpec (spec) where

import Test.Hspec
import Control.Concurrent.Async
import qualified Control.Concurrent.STM as STM
import Control.Lens

import qualified CMS.Domain.Model.Type as DM
import qualified CMS.Infra.Control as SUT
import qualified CMS.Infra.Type as SUT

-- |
--
data SpecContext = SpecContext {
                   _domainDataSpecContext :: DM.DomainData
                 , _appDataSpecContext :: SUT.AppData
                 }

makeLenses ''SpecContext

defaultSpecContext :: IO SpecContext
defaultSpecContext = do
  domDat <- DM.defaultDomainData
  appDat <- SUT.defaultAppData
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
  appDat <- SUT.defaultAppData
  return ctx {
               _domainDataSpecContext = domDat
             , _appDataSpecContext    = appDat
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
      it "should be 10" $ \ctx -> do 
        putStrLn "[INFO] 1件目の試験を実施"

        let domDat = ctx^.domainDataSpecContext
            appDat = ctx^.appDataSpecContext
            cmdQ   = domDat^.DM.commandQueueDomainData
            resQ   = domDat^.DM.responseQueueDomainData
            argDat = DM.InitializeCommandData 5 (callback resQ)
            args   = DM.InitializeCommand argDat
            expect = 5
            
        thId <- async $ SUT.runWithAppData appDat domDat

        STM.atomically $ STM.writeTQueue cmdQ args

        actual <- STM.atomically $ STM.readTQueue resQ
        actual `shouldBe` expect

        cancel thId

-- |
--
callback :: STM.TQueue Int -> Int -> IO ()
callback queue x = do
  STM.atomically $ STM.writeTQueue queue x
