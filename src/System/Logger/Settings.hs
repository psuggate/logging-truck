{-# LANGUAGE DeriveAnyClass, DeriveGeneric, DerivingStrategies, DerivingVia,
             NoImplicitPrelude, OverloadedStrings, TemplateHaskell,
             TupleSections #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

module System.Logger.Settings
  (
    Settings
  , Output     (..)
  , DateFormat (..)
  , Renderer

  , HasSeverity (..)
  , Severity    (..)

    -- * Default settings
  , defaultSettings
  , iso8601UTC

    -- * Accessors for various settings
  , output
  , format
  , bufSize
  , delimiter
  , logLevel
  , levelMap
  , logLevelMap
  , logLevelOf
  , name
  , nameMsg
  , renderer
  , readEnvironment

    -- * Extra setters
  , setLogLevelMap
  , setLogLevelOf
  , setName
  , setNetStrings
  , setRendererNetstr
  , setRendererDefault
  )
where

import           Control.Lens            (Lens', makeLenses, over, set, (.~))
import           Data.Aeson              (FromJSON (..), ToJSON (..))
import qualified Data.Aeson.Types        as Aeson
import qualified Data.ByteString.Builder as B
import           Data.ByteString.Char8   (pack)
import qualified Data.Char               as Char
import           Data.Map.Strict         as Map
import           Data.MonoTraversable    (omap)
import qualified Data.OpenApi            as OpenAPI
import           Data.String
import           Data.UnixTime
import           Relude
import           System.Log.FastLogger   (defaultBufSize)
import           System.Logger.Message
import           Text.Printf
import           Web.HttpApiData         (FromHttpApiData (..),
                                          ToHttpApiData (..))


-- * Convenience function-families
------------------------------------------------------------------------------
-- | Accessors for the severity-level of logged messages & events.
class HasSeverity a where
  severity :: Lens' a Severity


-- * Data types for the logger settings
------------------------------------------------------------------------------
data Settings
  = Settings
      { _logLevel        :: !Severity
        -- ^ messages below this log level will be suppressed
      , _levelMap        :: !(Map Text Severity)
        -- ^ log level per named logger
      , _output          :: !Output
        -- ^ log sink
      , _format          :: !(Maybe DateFormat)
        -- ^ the timestamp format (use 'Nothing' to disable timestamps)
      , _delimiter       :: !ByteString
        -- ^ text to intersperse between fields of a log line
      , _bufSize         :: !Int
        -- ^ how many bytes to buffer before commiting to sink
      , _name            :: !(Maybe Text)
        -- ^ logger name
      , _nameMsg         :: !(Msg -> Msg)
      , _renderer        :: !Renderer
      , _readEnvironment :: !Bool
        -- ^ should 'new' check @LOG_*@ process environment settings?
      }

------------------------------------------------------------------------------
-- | How severe is the event that led to this message?
data Severity
  = Trace
  | Debug
  | Info
  | Warn
  | Error
  | Fatal
  deriving (Enum, Eq, Generic, Ord, Read, Show)
  deriving anyclass (NFData)

instance ToJSON Severity where
  toJSON = toJSON . fromEnum

instance FromJSON Severity where
  parseJSON v = case v of
    Aeson.Number _ -> toEnum <$> parseJSON v
    Aeson.String s ->
      let s' = toString s
          er = "can not parse a Severity from the given string: " <> s'
      in  maybe (fail er) pure $ readMaybe s' <|> toEnum `fmap` readMaybe s'
    invalid        -> Aeson.typeMismatch "Number or String" invalid

instance Hashable Severity where
  hashWithSalt s = hashWithSalt s . ("Severity" :: Text,) . fromEnum

instance ToHttpApiData Severity where
  toUrlPiece = show

instance FromHttpApiData Severity where
  parseUrlPiece t = case omap Char.toUpper t of
    "TRACE" -> Right Trace
    "DEBUG" -> Right Debug
    "INFO"  -> Right Info
    "WARN"  -> Right Warn
    "ERROR" -> Right Error
    "FATAL" -> Right Fatal
    _       -> Left . fromString $ printf "Unrecognised severity: %s" t

instance OpenAPI.ToSchema Severity
instance OpenAPI.ToParamSchema Severity

------------------------------------------------------------------------------
data Output
  = StdOut
  | StdErr
  | Path FilePath
  deriving (Eq, Generic, NFData, Ord, Show)

------------------------------------------------------------------------------
newtype DateFormat
  = DateFormat { display :: UnixTime -> ByteString }

instance IsString DateFormat where
  fromString = DateFormat . formatUnixTimeGMT . pack

------------------------------------------------------------------------------
-- | Take a custom separator, date format, log level of the event, and render
-- a list of log fields or messages into a builder.
type Renderer = ByteString -> DateFormat -> Severity -> [Element] -> B.Builder


-- * Lenses & instances
------------------------------------------------------------------------------
makeLenses ''Settings

------------------------------------------------------------------------------
instance HasSeverity Severity where
  severity = id

instance HasSeverity Settings where
  severity = logLevel


-- * Additional setters
------------------------------------------------------------------------------
-- | Whether to use <http://cr.yp.to/proto/netstrings.txt netstring>
-- encoding for log lines.
--
-- {#- DEPRECATED setNetStrings "Use setRendererNetstr or setRendererDefault instead" #-}
setNetStrings :: Bool -> Settings -> Settings
setNetStrings True  = set renderer $ \_ _ _ -> renderNetstr
setNetStrings False = set renderer $ \s _ _ -> renderDefault s

-- | Shortcut for calling 'setRenderer' with 'renderNetstr'.
setRendererNetstr :: Settings -> Settings
setRendererNetstr  = set renderer $ \_ _ _ -> renderNetstr

-- | Default rendering of log lines.
--
-- Uses the value of `delimiter` as a separator of fields and '=' between
-- field names and values.
setRendererDefault :: Settings -> Settings
setRendererDefault  = set renderer $ \s _ _ -> renderDefault s


-- * Log-level helpers
------------------------------------------------------------------------------
logLevelMap :: Settings -> Map Text Severity
logLevelMap  = _levelMap

-- | Log level of some named logger.
logLevelOf :: Text -> Settings -> Maybe Severity
logLevelOf x = Map.lookup x . _levelMap

-- | Specify a log level for the given named logger. When a logger is
-- 'clone'd and given a name, the 'logLevel' of the cloned logger will be
-- the provided here.
setLogLevelOf :: Text -> Severity -> Settings -> Settings
setLogLevelOf n = over levelMap . Map.insert n

setLogLevelMap :: Map Text Severity -> Settings -> Settings
setLogLevelMap  = set levelMap


-- ** Log-name helpers
------------------------------------------------------------------------------
setName :: Maybe Text -> Settings -> Settings
setName mn = name .~ mn >>> nameMsg .~ maybe id ("logger" .=) mn


-- ** Defaults
------------------------------------------------------------------------------
-- | ISO 8601 date-time format.
iso8601UTC :: DateFormat
iso8601UTC  = "%Y-%0m-%0dT%0H:%0M:%0SZ"

------------------------------------------------------------------------------
-- | Default settings:
--
--   * 'logLevel'        = 'Debug'
--
--   * 'output'          = 'StdOut'
--
--   * 'format'          = 'iso8601UTC'
--
--   * 'delimiter'       = \", \"
--
--   * 'netstrings'      = False
--
--   * 'bufSize'         = 'FL.defaultBufSize'
--
--   * 'name'            = Nothing
--
--   * 'readEnvironment' = True
--
defaultSettings :: Settings
defaultSettings  = Settings
  Debug
  Map.empty
  StdOut
  (Just iso8601UTC)
  ", "
  defaultBufSize
  Nothing
  id
  (\s _ _ -> renderDefault s)
  True
