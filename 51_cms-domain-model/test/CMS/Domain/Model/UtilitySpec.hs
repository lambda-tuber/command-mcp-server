{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultilineStrings #-}

module CMS.Domain.Model.UtilitySpec (spec) where

import Test.Hspec
import Control.Lens
import Data.Aeson

import qualified CMS.Domain.Model.Utility as SUT
import qualified CMS.Domain.Model.Type as SUT


-- |
--
data SpecContext = SpecContext {
                 }

makeLenses ''SpecContext

defaultSpecContext :: IO SpecContext
defaultSpecContext = do
  return SpecContext {
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
setUp _ = do
  putStrLn "[INFO] 各試験の開始前に実施"

  ctx <- defaultSpecContext
  return ctx

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

        let expect = "abc"
            actual = SUT.lbs2str . SUT.str2lbs $ expect

        actual `shouldBe` expect

      it "should be json" $ \_ -> do 
        putStrLn "[INFO] 2件目の試験を実施"

        let jsonStr = """
                      {"jsonrpc":"2.0","id":1,"method":"initialize","params":{"capabilities":{"roots":{"listChanged":true}},"clientInfo":{"name":"Visual Studio Code","version":"1.99.2"},"protocolVersion":"2024-11-05"}}
                      """
            Right jsonDat = (eitherDecode jsonStr) :: Either String SUT.JsonRpcRequest

        let actual = encode jsonDat

        actual `shouldBe` jsonStr


