{-# LANGUAGE BangPatterns, DeriveGeneric, OverloadedStrings, TemplateHaskell #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

------------------------------------------------------------------------------
-- |
-- Module      : System.Logger
-- Copyright   : (c) 2022 Patrick Suggate
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Small layer on top of @fast-logger@ which adds log-levels and
-- timestamp support and not much more.
--
------------------------------------------------------------------------------

module System.Logger
  (
    -- * Settings
    Settings
  , defaultSettings
  , logLevel
  , logLevelOf
  , output
  , format
  , delimiter
  , readEnvironment
  , bufSize
  , name
  , renderer

  , setLogLevelOf
  , setName
  , setNetStrings
  , setRendererNetstr
  , setRendererDefault

    -- * Type definitions
  , Logger
  , settings
  , logger
  , getDate

  , HasSeverity (..)
  , Severity    (..)

  , Output      (..)
  , DateFormat  (..)
  , Renderer
  , iso8601UTC

    -- * Core API
  , new
  , create
  , level
  , flush
  , close
  , clone

    -- ** Logging
  , log
  , trace
  , debug
  , info
  , warn
  , err
  , fatal

  , module Export
  )
where

import           Control.Lens           (makeLenses, set, (%~), (^.))
import qualified Data.Map.Strict        as Map
import           Data.UnixTime
import           Relude                 hiding (trace)
import qualified System.Log.FastLogger  as FL
import           System.Logger.Message  as Export
import           System.Logger.Settings


-- * Top-level logger data types
------------------------------------------------------------------------------
data Logger
  = Logger
      { _logger   :: FL.LoggerSet
      , _settings :: Settings
      , _getDate  :: IO (Msg -> Msg)
      }
  deriving (Generic)


-- * Lenses & instances
------------------------------------------------------------------------------
makeLenses ''Logger

instance HasSeverity Logger where
  severity = settings . logLevel


-- * Logger API functions
------------------------------------------------------------------------------
-- | Create a new 'Logger' with the given 'Settings'.
-- Please note that the 'logLevel' can be dynamically adjusted by setting
-- the environment variable @LOG_LEVEL@ accordingly. Likewise the buffer
-- size can be dynamically set via @LOG_BUFFER@ and netstrings encoding
-- can be enabled with @LOG_NETSTR=True@.  **NOTE: If you do this any custom
-- renderers you may have passed with the settings will be overwritten!**
--
-- Since version 0.11 one can also use @LOG_LEVEL_MAP@ to specify log
-- levels per (named) logger. The syntax uses standard haskell syntax for
-- association lists of type @[(Text, Level)]@. For example:
--
-- If you want to ignore environment variables, call @setReadEnvironment False@ on the
-- 'Settings'.
--
-- @
-- $ LOG_LEVEL=Info LOG_LEVEL_MAP='[("foo", Warn), ("bar", Trace)]' cabal repl
-- > g1 <- new defaultSettings
-- > let g2 = clone (Just "foo") g1
-- > let g3 = clone (Just "bar") g1
-- > let g4 = clone (Just "xxx") g1
-- > logLevel (settings g1)
-- Info
-- > logLevel (settings g2)
-- Warn
-- > logLevel (settings g3)
-- Trace
-- > logLevel (settings g4)
-- Info
-- @
new :: MonadIO m => Settings -> m Logger
new s =
  liftIO $ do
    !n <- fmap (readNote "Invalid LOG_BUFFER") <$> maybeLookupEnv "LOG_BUFFER"
    !l <- fmap (readNote "Invalid LOG_LEVEL")  <$> maybeLookupEnv "LOG_LEVEL"
    !e <- fmap (readNote "Invalid LOG_NETSTR") <$> maybeLookupEnv "LOG_NETSTR"
    !m <- fromMaybe "[]" <$> maybeLookupEnv "LOG_LEVEL_MAP"
    let !k  = logLevelMap s `mergeWith` m
    let !s' = set logLevel (fromMaybe (s ^. logLevel) l)
            . maybe id (bool id setRendererNetstr) e
            . setLogLevelMap k
            $ s
    g <- fn (s^.output) (fromMaybe (s^.bufSize) n)
    Logger g s' <$> mkGetDate (s^.format)
  where
    maybeLookupEnv :: String -> IO (Maybe String)
    maybeLookupEnv key =
        if s^.readEnvironment
            then lookupEnv key
            else pure Nothing

    fn StdOut   = FL.newStdoutLoggerSet
    fn StdErr   = FL.newStderrLoggerSet
    fn (Path p) = flip FL.newFileLoggerSet p

    mkGetDate Nothing  = return (return id)
    mkGetDate (Just f) = return (msg . (display f) <$> getUnixTime)

    mergeWith m e = Map.fromList (readNote "Invalid LOG_LEVEL_MAP" e) `Map.union` m

    readNote :: Read a => String -> String -> a
    readNote m xs = case reads xs of
      [(a, "")] -> a
      _         -> error $ toText m

------------------------------------------------------------------------------
-- | Invokes 'new' with default settings and the given output as log sink.
create :: MonadIO m => Output -> m Logger
create  = new <<< set output `flip` defaultSettings

------------------------------------------------------------------------------
-- | Clone the given logger and optionally give it a name
-- (use @Nothing@ to clear).
--
-- If 'logLevelOf' returns a custom 'Level' for this name
-- then the cloned logger will use it for its log messages.
clone :: Maybe Text -> Logger -> Logger
clone mn = settings %~ setName mn . go
  where
    go s = case mn of
      Just n  -> s & logLevel %~ maybe id const (logLevelOf n s)
      Nothing -> s

------------------------------------------------------------------------------
-- | Force buffered bytes to output sink.
flush :: MonadIO m => Logger -> m ()
flush  = liftIO . FL.flushLogStr . _logger

------------------------------------------------------------------------------
-- | Closes the logger.
close :: MonadIO m => Logger -> m ()
close  = liftIO . FL.rmLoggerSet . _logger

------------------------------------------------------------------------------
-- | Inspect this logger's threshold.
level :: Logger -> Severity
level  = (^. logLevel) . _settings


-- ** Standard logging functions
------------------------------------------------------------------------------
-- | Logs a message with the given level if greater or equal to the
-- logger's threshold.
log :: MonadIO m => Logger -> Severity -> (Msg -> Msg) -> m ()
log g l m = unless (level g > l) $ putMsg g l m
{-# INLINE log #-}

------------------------------------------------------------------------------
-- | Abbreviation of 'log' using the corresponding log level.
trace, debug, info, warn, err, fatal
  :: MonadIO m
  => Logger
  -> (Msg -> Msg)
  -> m ()
trace g = log g Trace
debug g = log g Debug
info  g = log g Info
warn  g = log g Warn
err   g = log g Error
fatal g = log g Fatal
{-# INLINE trace #-}
{-# INLINE debug #-}
{-# INLINE info  #-}
{-# INLINE warn  #-}
{-# INLINE err   #-}
{-# INLINE fatal #-}


-- ** Emitter functions
------------------------------------------------------------------------------
putMsg :: MonadIO m => Logger -> Severity -> (Msg -> Msg) -> m ()
putMsg g l f = liftIO $ do
  d <- g ^. getDate
  let r = g ^. settings . renderer
  let x = g ^. settings . delimiter
  let s = g ^. settings . nameMsg
  let df = fromMaybe iso8601UTC $ g ^. settings . format
  let ll = g ^. settings . logLevel
  let m = render (r x df ll) (d . lmsg l . s . f)
  FL.pushLogStr (g ^. logger) (FL.toLogStr m)

lmsg :: Severity -> (Msg -> Msg)
lmsg Trace = msg (val "T")
lmsg Debug = msg (val "D")
lmsg Info  = msg (val "I")
lmsg Warn  = msg (val "W")
lmsg Error = msg (val "E")
lmsg Fatal = msg (val "F")
{-# INLINE lmsg #-}
