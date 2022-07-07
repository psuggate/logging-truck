{-# LANGUAGE FlexibleContexts, NoImplicitPrelude, NoMonomorphismRestriction,
             OverloadedStrings #-}

module Main where

import           Data.Aeson        as Aeson
import           Data.Hashable
import           Relude
import           Test.Hspec
import qualified Text.Read         as Text (read)
import           Web.HttpApiData   (toUrlPiece)

------------------------------------------------------------------------------
-- | Module under test
import           Data.Event.Status


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

    context "Parsing-tests for @DateTime@ values" $ do
      let tim = "1.657102119810025e9" :: String
          (utc, dtu) = (Text.read str, DateTime utc)
          str = "2022-07-06 10:08:39.810025 UTC" :: String
          iso = "2022-07-06T10:08:39.810025Z" :: String

      it "can read a UTC time value" $ do
        parseUTC str `shouldBe` Just utc
        dateTime (toText str) `shouldBe` Just dtu

      it "can read an ISO8601 time value" $ do
        parseUTC iso `shouldBe` Just utc
        dateTime (toText iso) `shouldBe` Just dtu

      it "can read a POSIX-seconds time value" $ do
        parseUTC tim `shouldBe` Just utc
        dateTime (toText tim) `shouldBe` Just dtu

      it "can @show@ a @DateTime@ value" $ do
        show dtu `shouldBe` str

      it "can @read@ a @DateTime@ value" $ do
        Text.read str `shouldBe` dtu

      it "can JSON-decode @DateTime@ floating-point values" $ do
        let enc :: String -> Maybe DateTime
            enc  = Aeson.decode . encodeUtf8
        enc tim `shouldBe` Just dtu

      it "can JSON-decode @DateTime@ ISO8601 values" $ do
        Aeson.decode (Aeson.encode dtu) `shouldBe` Just dtu
        -- enc (show str) `shouldBe` Just dtu

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
          str = map (encodeUtf8 . txt . fromEnum) sev

      it "can @read@ and @show@ each @Severity@" $ do
        map (Text.read . show) sev `shouldBe` sev

      it "can JSON-encode each @Severity@" $ do
        map Aeson.encode sev `shouldBe` str
        catMaybes (Aeson.decode . Aeson.encode <$> sev) `shouldBe` sev

      it "can hash each @Severity@" $ do
        map hash sev `shouldBe` (hash . ("Severity" :: Text,) . fromEnum <$> sev)

      it "can URL-encode each @Severity@" $ do
        map toUrlPiece sev `shouldBe` map txt sev

      it "can JSON-decode string-representations of a @Severity@" $ do
        let ss = encodeUtf8 . txt . txt <$> sev
        catMaybes (Aeson.decode <$> ss) `shouldBe` sev


-- * Helpers
------------------------------------------------------------------------------
txt :: Show a => a -> Text
txt  = show


-- * Test-runner
------------------------------------------------------------------------------
main :: IO ()
main  = hspec spec -- putStrLn "Test suite not yet implemented"
