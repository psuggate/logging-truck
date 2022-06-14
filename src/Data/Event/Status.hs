{-# LANGUAGE DeriveAnyClass, DerivingStrategies, FunctionalDependencies,
             MultiParamTypeClasses, NoImplicitPrelude, OverloadedStrings #-}

------------------------------------------------------------------------------
-- |
-- Module      : Data.Event.Status
-- Copyright   : (c) 2022 Patrick Suggate
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
-- Loggers for @StatusMessage@ events, and supporting various backends; e.g.,
-- 'fast-logger'.
--
-- == TODO
--  - there needs to be a method for flushing a logger's buffer, and closing
--    it's handle -- perhaps using 'conduit'?
--
------------------------------------------------------------------------------

module Data.Event.Status
  (
    HasDetailsOf (..)
  , HasSeverity (..)
  , HasStatusOf (..)
  , MonadLogger (..)

  , LogAction (..)
  , MessageId (..)
  , ServiceName (..)
  , Severity (..)

  , StatusMessage (..)
  , logEvent
  , eventMsg

  , Platform (..)
  , defaultPlatform
  , testing
  , development
  , staging
  , production

  , EventStatus (..)
  , resolved
  , unknown
  , running

  , StatusEvent (..)
  , newStatusEvent
  , toStatusEvent

  , stdoutLogger
  , stderrLogger
  , actionLogger

  , exampleStatusEvent
  -- , unsafeReadUUID
  )
where

import           Colog.Core              (LogAction (..))
import           Control.Lens            (Lens', lens, (.~), (?~), (^.))
import           Control.Monad.IO.Unlift
import           Data.Aeson              (FromJSON (..), Options (..),
                                          ToJSON (..), defaultOptions, encode,
                                          genericParseJSON, genericToJSON)
import qualified Data.List               as List
import           Data.Time.Clock         as Time (UTCTime, getCurrentTime)
import           Data.UUID               as UUID (UUID)
import qualified Data.UUID               as UUID (fromString)
import qualified Data.UUID.V4            as UUID
import           Relude
import           System.Logger           as Logger
import           System.Logger.Class     as Class (MonadLogger (..))
import qualified Text.Read               as Text (read)
import           Web.HttpApiData         (FromHttpApiData)


-- * Convenience function-families
------------------------------------------------------------------------------
-- | Accessors for the details of a data type.
class HasDetailsOf t a | t -> a where
  detailsOf :: Lens' t a

class HasStatusOf t a | t -> a where
  statusOf :: Lens' t a


-- * Data types for status updates and events
------------------------------------------------------------------------------
data StatusEvent
  = StatusEvent
      { statusEvent'id       :: !MessageId
      , statusEvent'datetime :: !UTCTime
      , statusEvent'platform :: !Platform
      , statusEvent'service  :: !ServiceName
      , statusEvent'severity :: !Severity
      , statusEvent'status   :: !(Maybe EventStatus)
      , statusEvent'message  :: !(Maybe Text)
      }
  deriving (Eq, Generic, Show)
  deriving anyclass (NFData)

instance ToJSON StatusEvent where
  toJSON = genericToJSON jsonOpts

instance FromJSON StatusEvent where
  parseJSON = genericParseJSON jsonOpts

instance HasStatusOf StatusEvent (Maybe EventStatus) where
  statusOf = lens statusEvent'status $ \r s -> r { statusEvent'status = s }

instance HasDetailsOf StatusEvent (Maybe Text) where
  detailsOf = lens statusEvent'message $ \r s -> r { statusEvent'message = s }

instance HasSeverity StatusEvent where
  severity = lens statusEvent'severity $ \r s -> r { statusEvent'severity = s }

------------------------------------------------------------------------------
instance ToText StatusEvent where
  toText :: StatusEvent -> Text
  toText  = decodeUtf8 . encode

------------------------------------------------------------------------------
newtype MessageId
  = MessageId { getMessageId :: UUID }
  deriving (Eq, Generic, Show)
  deriving newtype (FromHttpApiData, FromJSON, NFData, ToJSON)

instance IsString MessageId where
  fromString = MessageId . unsafeReadUUID "MessageId"

------------------------------------------------------------------------------
newtype ServiceName
  = ServiceName { getServiceName :: Text }
  deriving (Eq, Generic, Show)
  deriving newtype (FromHttpApiData, FromJSON, NFData, ToJSON)

instance IsString ServiceName where
  fromString = ServiceName . toText

------------------------------------------------------------------------------
newtype Platform
  = Platform { getPlatform :: Text }
  deriving (Eq, Generic, Show)
  deriving newtype (FromHttpApiData, FromJSON, NFData, ToJSON)

instance IsString Platform where
  fromString = Platform . toText


-- * Data types for status-events
------------------------------------------------------------------------------
data StatusMessage
  = StatusMessage
      { _levelOf :: !Severity
      , _status  :: !EventStatus
      , _details :: !Text
      }
  deriving (Eq, Generic, Show)
  deriving anyclass (NFData)

instance IsString StatusMessage where
  fromString = StatusMessage Debug unknown . toText

instance HasStatusOf StatusMessage EventStatus where
  statusOf = lens _status $ \r s -> r { _status = s }

instance HasDetailsOf StatusMessage Text where
  detailsOf = lens _details $ \r s -> r { _details = s }

instance HasSeverity StatusMessage where
  severity = lens _levelOf $ \r s -> r { _levelOf = s }

------------------------------------------------------------------------------
newtype EventStatus
  = EventStatus { getEventStatus :: Text }
  deriving (Eq, Generic, Show)
  deriving newtype (FromHttpApiData, FromJSON, NFData, ToJSON)

instance IsString EventStatus where
  fromString = EventStatus . toText

instance ToText EventStatus where
  toText = getEventStatus

instance ToBytes EventStatus where
  bytes = bytes . getEventStatus

------------------------------------------------------------------------------
resolved :: EventStatus
resolved  = EventStatus "resolved"

unknown :: EventStatus
unknown  = EventStatus "unknown"

running :: EventStatus
running  = EventStatus "running"


-- * Some standard loggers
------------------------------------------------------------------------------
-- | Log to 'stdout' with minimal formatting.
stdoutLogger :: MonadIO m => Severity -> m (LogAction IO StatusMessage)
stdoutLogger lvl = actionLogger lvl Logger.StdOut (Just "stdout")

-- | Log to 'stderr' with minimal formatting.
stderrLogger :: MonadIO m => Severity -> m (LogAction IO StatusMessage)
stderrLogger lvl = actionLogger lvl Logger.StdErr (Just "stderr")

------------------------------------------------------------------------------
-- | Create a 'tinylog' logger action.
actionLogger
  :: MonadIO m
  => Severity
  -> Logger.Output
  -> Maybe Text
  -> m (LogAction IO StatusMessage)
actionLogger l o n = do
  let s = Logger.defaultSettings
        & Logger.logLevel .~ l
        & Logger.output .~ o
  lgr <- Logger.new $ Logger.setName n s
  pure $ LogAction $ \x -> when (x ^. severity >= l) $ do
    Logger.log lgr (x ^. severity) (eventMsg x)


-- * Smart constructors
------------------------------------------------------------------------------
newStatusEvent :: Platform -> ServiceName -> Severity -> IO StatusEvent
newStatusEvent p s l = do
  u <- MessageId <$> UUID.nextRandom
  t <- Time.getCurrentTime
  pure $ StatusEvent u t p s l Nothing Nothing

------------------------------------------------------------------------------
-- | Default platform issuing the status events.
defaultPlatform :: Platform
defaultPlatform  = development

testing :: Platform
testing  = Platform "TESTING"

development :: Platform
development  = Platform "DEVELOPMENT"

staging :: Platform
staging  = Platform "STAGING"

production :: Platform
production  = Platform "PRODUCTION"


-- * Conversions and renderers
------------------------------------------------------------------------------
toStatusEvent
  :: MonadIO m
  => Platform
  -> ServiceName
  -> StatusMessage
  -> m StatusEvent
toStatusEvent p n (StatusMessage l s d) = do
  liftIO $ newStatusEvent p n l
    <&> statusOf  ?~ s
    <&> detailsOf ?~ d


-- * Helpers
------------------------------------------------------------------------------
-- | JSON encoding & decoding settings for the types defined in this module.
jsonOpts :: Options
jsonOpts  = defaultOptions
  { omitNothingFields = True
  , fieldLabelModifier = List.tail . List.dropWhile (/= '\'')
  }

------------------------------------------------------------------------------
-- | Helper-function for reading a UUID into a @newtype@.
--
--   NOTE: only a partial function, and is intended only for testing and
--     generating documentation.
--
unsafeReadUUID :: Text -> String -> UUID.UUID
unsafeReadUUID typ sval = error emsg `fromMaybe` UUID.fromString sval where
  emsg = "Failed to parse value (" <> fromString sval <> " :: " <> typ <> ")"

------------------------------------------------------------------------------
logEvent :: MonadLogger m => StatusMessage -> m ()
logEvent ev = Class.log (ev ^. severity) (eventMsg ev)

------------------------------------------------------------------------------
eventMsg :: StatusMessage -> Msg -> Msg
eventMsg (StatusMessage _ s d) = field "status" s . msg d


-- * Miscellaneous functions
------------------------------------------------------------------------------
exampleStatusEvent :: StatusEvent
exampleStatusEvent  = StatusEvent
  { statusEvent'id       = "b0614aaa-2daf-40a3-8fe5-5772bad038aa"
  , statusEvent'datetime = Text.read "2022-03-01 07:37:47 UTC"
  , statusEvent'platform = testing
  , statusEvent'service  = ServiceName "logging-truck (version: 0.1.0.3)"
  , statusEvent'severity = Fatal
  , statusEvent'status   = Just resolved
  , statusEvent'message  = Just "have a great weekend!"
  }
