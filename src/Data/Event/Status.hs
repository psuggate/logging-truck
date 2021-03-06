{-# LANGUAGE DeriveAnyClass, DerivingStrategies, DerivingVia, FlexibleContexts,
             FunctionalDependencies, MultiParamTypeClasses, NoImplicitPrelude,
             OverloadedStrings #-}

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

  , DateTime (..)
  , dateTime
  , parseUTC

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
  , starting
  , stopping

  , StatusEvent (..)
  , newStatusEvent
  , toStatusEvent

  , stdoutLogger
  , stderrLogger
  , actionLogger

  , exampleStatusEvent
  )
where

import           Colog.Core                   (LogAction (..))
import           Control.Lens                 (Lens', lens, mapped, (.~), (?~),
                                               (^.))
import           Control.Monad.IO.Unlift
import           Data.Aeson                   as Aeson
import qualified Data.Aeson.Types             as Aeson
import qualified Data.List                    as List
import           Data.OpenApi                 (ToParamSchema, ToSchema (..))
import qualified Data.OpenApi                 as OpenAPI
import qualified Data.OpenApi.Declare         as OpenAPI
import qualified Data.OpenApi.Internal.Schema as OpenAPI
import           Data.Scientific              (Scientific)
import           Data.Time.Clock              as Time (UTCTime, getCurrentTime)
import           Data.Time.Clock.System       (SystemTime (..), systemToUTCTime)
import qualified Data.Time.Format.ISO8601     as Time
import           Data.UUID                    as UUID (UUID)
import qualified Data.UUID                    as UUID (fromString, toText)
import qualified Data.UUID.V4                 as UUID
import qualified GHC.Generics                 as Generics
import           Relude
import           System.Logger                as Logger
import           System.Logger.Class          as Class (MonadLogger (..))
import           Text.Printf
import qualified Text.Read                    as Text (read)
import           Web.HttpApiData              (FromHttpApiData, ToHttpApiData)


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
      , statusEvent'datetime :: !DateTime
      , statusEvent'platform :: !Platform
      , statusEvent'service  :: !ServiceName
      , statusEvent'severity :: !Severity
      , statusEvent'status   :: !(Maybe EventStatus)
      , statusEvent'message  :: !(Maybe Text)
      }
  deriving (Eq, Generic, Show)
  deriving anyclass (NFData)

instance ToSchema StatusEvent where
  declareNamedSchema _ = openApiSchemaWith describe exampleStatusEvent where
    describe = "Information for a status-event record of the SixthSense platform"

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

deriving via UUID instance ToSchema MessageId
deriving via UUID instance ToParamSchema MessageId
deriving newtype  instance Hashable MessageId

instance IsString MessageId where
  fromString = MessageId . unsafeReadUUID "MessageId"

instance ToText MessageId where
  toText :: MessageId -> Text
  toText  = UUID.toText . getMessageId

------------------------------------------------------------------------------
newtype ServiceName
  = ServiceName { getServiceName :: Text }
  deriving (Eq, Generic, Show)
  deriving newtype (FromHttpApiData, FromJSON, NFData, ToJSON)

deriving via Text instance ToSchema ServiceName
deriving via Text instance ToParamSchema ServiceName
deriving newtype  instance Hashable ServiceName

instance IsString ServiceName where fromString = ServiceName . toText
instance ToText   ServiceName where toText = getServiceName

------------------------------------------------------------------------------
newtype Platform
  = Platform { getPlatform :: Text }
  deriving (Eq, Generic, Show)
  deriving newtype (FromHttpApiData, FromJSON, NFData, ToJSON)

deriving via Text instance ToSchema Platform
deriving via Text instance ToParamSchema Platform
deriving newtype  instance Hashable Platform

instance IsString Platform where fromString = Platform . toText
instance ToText   Platform where toText = getPlatform

------------------------------------------------------------------------------
-- | Wrapped @UTCTime@ so that different parsers can be used for different
--   Google services; e.g., BigQuery returns time-values as a stringified
--   floating-point representation (which does not automatically parse, when
--   using the 'time' library).
newtype DateTime
  = DateTime { unDateTime :: UTCTime }
  deriving (Eq, Generic, Ord)
  deriving newtype
    ( FromHttpApiData
    , Hashable
    , NFData
    , Read
    , Show
    , ToHttpApiData
    )

deriving via Text instance ToSchema DateTime
deriving via Text instance ToParamSchema DateTime

instance ToJSON DateTime where
  toJSON = toJSON . unDateTime

instance FromJSON DateTime where
  parseJSON xs = case xs of
    Number ss -> pure $! DateTime (utctime ss)
    String ts -> case dateTime ts of
      Just dt -> pure dt
      Nothing -> dtfail . fail $ printf "can not parse: %s" ts
    invalid   -> dtfail $ Aeson.typeMismatch "String or Number" invalid
    where
      dtfail = Aeson.prependFailure "parsing DateTime failed, "


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

deriving via Text instance ToSchema EventStatus
deriving via Text instance ToParamSchema EventStatus
deriving newtype  instance Hashable EventStatus

instance IsString EventStatus where fromString = EventStatus . toText
instance ToText   EventStatus where toText = getEventStatus
instance ToBytes  EventStatus where bytes = bytes . getEventStatus

------------------------------------------------------------------------------
resolved :: EventStatus
resolved  = EventStatus "resolved"

unknown :: EventStatus
unknown  = EventStatus "unknown"

------------------------------------------------------------------------------
-- | Service state/stage.
starting :: EventStatus
starting  = EventStatus "starting"

running :: EventStatus
running  = EventStatus "running"

stopping :: EventStatus
stopping  = EventStatus "stopping"


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
  t <- DateTime <$> Time.getCurrentTime
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

------------------------------------------------------------------------------
-- | Attempt to parse a @DateTime@ from the given string-value.
dateTime :: Text -> Maybe DateTime
dateTime  = fmap DateTime . parseUTC . toString

parseUTC :: String -> Maybe UTCTime
parseUTC ts = readMaybe ts
  <|> Time.iso8601ParseM ts
  <|> fmap utctime (readMaybe ts)

utctime :: Scientific -> UTCTime
utctime  = systemToUTCTime . systime
  where
    systime :: Scientific -> SystemTime
    systime x =
      let secs = floor x
          nano = round $ (x - fromIntegral secs) * 1e9
      in  MkSystemTime secs nano


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

------------------------------------------------------------------------------
openApiSchemaWith
  :: forall a. Typeable a
  => ToJSON a
  => Generic a
  => OpenAPI.GToSchema (Generics.Rep a)
  => Text
  -> a
  -> OpenAPI.Declare (OpenAPI.Definitions OpenAPI.Schema) OpenAPI.NamedSchema
openApiSchemaWith desc example =
  OpenAPI.genericDeclareNamedSchema openApiSchemaOptions proxy
    & mapped . OpenAPI.schema . OpenAPI.description ?~ desc
    & mapped . OpenAPI.schema . OpenAPI.example ?~ toJSON example
  where
    proxy = Proxy :: Proxy a

openApiSchemaOptions :: OpenAPI.SchemaOptions
openApiSchemaOptions  =
  let go xs = case List.elemIndex '\'' xs of
        Just i  -> List.drop (i+1) xs
        Nothing -> xs
  in  OpenAPI.defaultSchemaOptions { OpenAPI.fieldLabelModifier = go }
