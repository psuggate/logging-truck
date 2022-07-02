{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, NoMonomorphismRestriction,
             OverloadedStrings #-}

module Main where

import           Data.Aeson        as Aeson
import           Data.Event.Status
import           Relude
import           Test.Hspec

import           Data.Hashable
import           Web.HttpApiData   (toUrlPiece)


-- * Top-level tests
------------------------------------------------------------------------------
spec :: Spec
spec  = describe "Encoding and decoding of event-types" $ do

  context "JSON- and Hashable- conversions for status-event data types" $ do

    context "Tests for @MessageId@ data type" $ do
      let mid = "05558c29-e918-42ae-b66c-36551672a592" :: MessageId

      it "can JSON-encode a @MessageId@" $ do
        let str = show (getMessageId mid) :: Text
        Aeson.encode mid `shouldBe` Aeson.encode str
        Aeson.decode (Aeson.encode mid) `shouldBe` Just mid

      it "can hash a @MessageId@" $ do
        hash mid `shouldBe` hash (getMessageId mid)

      it "can Text-encode a @MessageId@ (to a UUID)" $ do
        toText mid `shouldBe` "05558c29-e918-42ae-b66c-36551672a592"

    context "Tests for @ServiceName@ data type" $ do
      let svc = "logging-service" :: ServiceName

      it "can JSON-encode a @ServiceName@" $ do
        Aeson.encode svc `shouldBe` Aeson.encode (getServiceName svc)
        Aeson.decode (Aeson.encode svc) `shouldBe` Just svc

      it "can hash a @ServiceName@" $ do
        hash svc `shouldBe` hash (getServiceName svc)

    context "Tests for @Platform@ data type" $ do
      let pla = "TESTING" :: Platform

      it "can JSON-encode a @Platform@" $ do
        Aeson.encode pla `shouldBe` Aeson.encode (getPlatform pla)
        Aeson.decode (Aeson.encode pla) `shouldBe` Just pla

      it "can hash a @Platform@" $ do
        hash pla `shouldBe` hash (getPlatform pla)

    context "Tests for @EventStatus@ data type" $ do
      let evt = "somewhat depressing" :: EventStatus

      it "can JSON-encode a @EventStatus@" $ do
        Aeson.encode evt `shouldBe` Aeson.encode (getEventStatus evt)
        Aeson.decode (Aeson.encode evt) `shouldBe` Just evt

      it "can hash an @EventStatus@" $ do
        hash evt `shouldBe` hash (getEventStatus evt)

    context "Tests for @Severity@ data type" $ do
      let sev = [Trace ..Fatal]
          str = map (encodeUtf8 . txt . txt) sev

      it "can JSON-encode each @Severity@" $ do
        map Aeson.encode sev `shouldBe` str
        catMaybes (Aeson.decode . Aeson.encode <$> sev) `shouldBe` sev

      it "can hash each @Severity@" $ do
        map hash sev `shouldBe` (hash . ("Severity" :: Text,) . fromEnum <$> sev)

      it "can URL-encode each @Severity@" $ do
        map toUrlPiece sev `shouldBe` map txt sev


-- * Helpers
------------------------------------------------------------------------------
txt :: Show a => a -> Text
txt  = show


-- * Test-runner
------------------------------------------------------------------------------
main :: IO ()
main  = hspec spec -- putStrLn "Test suite not yet implemented"
