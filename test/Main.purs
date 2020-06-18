module Main where

import Prelude

import Data.Either (either)
import Data.HttpTypes.V000 (Exchange(..), Method(..), Request(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff as Effect
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Simple.JSON (E, readJSON)
import Test.Spec (before, describe, it)
import Test.Spec.Assertions (shouldEqual, fail)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)


readExchange :: String -> Effect.Aff (E Exchange)
readExchange t = do
  rtf <- liftEffect $ readTextFile UTF8 t
  pure $ readJSON rtf

main ∷ Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "stripe" do
    before (readExchange "examples/0.json") $ do
      it "has method get" $ \exch -> do
        --getExchangeObjectExchange oai `shouldBe` "3.0.0"
        either (\e -> fail (show e)) (\(Exchange { request: Request { method }}) -> method `shouldEqual` GET) exch
