{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module CMS.App.ControlSpec (spec) where

import Test.Hspec
import Data.Default
import Control.Lens
import qualified Control.Concurrent as C

import qualified CMS.Domain.Model.Type as DM
import qualified CMS.App.Control as SUT

-- |
--
data SpecContext = SpecContext {}

makeLenses ''SpecContext

defaultSpecContext :: IO SpecContext
defaultSpecContext = do
  return SpecContext {}

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
setUp _ = do
  putStrLn "[INFO] 各試験の開始前に実施"
  defaultSpecContext

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
      it "should be 10" $ \_ -> do 
        putStrLn "[INFO] 1件目の試験を実施"

        let args = def
            apps = [sample1, sample2]
            
        SUT.run args apps


  where 
    sample1 :: DM.DomainContext ()
    sample1 _ = do
      putStrLn $ "[INFO] sample1 called."
      C.threadDelay (2 * 1000 * 1000)  -- 100万マイクロ秒 = 1秒
      putStrLn $ "[INFO] sample1 called. end."

    sample2 :: DM.DomainContext ()
    sample2 _ = do
      putStrLn $ "[INFO]   sample2 called."
      C.threadDelay (3 * 1000 * 1000)  -- 100万マイクロ秒 = 1秒
      putStrLn $ "[INFO]   sample2 called. end."
