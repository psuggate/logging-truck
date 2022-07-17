{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings #-}

module Main where

import           Data.Aeson             as Aeson
import           Data.Event.Status.Util
import           Relude


-- * Settings & helpers
------------------------------------------------------------------------------
plat :: Platform
plat  = "localhost"

name :: ServiceName
name  = "logging-truck"

mesg :: IO StatusEvent
mesg  = messageR >>= toStatusEvent plat name


-- * Main entry-point
------------------------------------------------------------------------------
main :: IO ()
main  = do
  n <- getArgs >>= \case
    [] -> pure 10
    xs -> mapM_ putStrLn xs >> pure 10

  mapM_ (putTextLn . decodeUtf8 . Aeson.encode) =<< sequence (replicate n mesg)
