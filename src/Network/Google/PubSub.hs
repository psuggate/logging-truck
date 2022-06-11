{-# LANGUAGE ConstraintKinds, DataKinds, DerivingStrategies, FlexibleContexts,
             FunctionalDependencies, MultiParamTypeClasses, NoImplicitPrelude,
             OverloadedStrings, PatternSynonyms, ScopedTypeVariables,
             TypeFamilies #-}

------------------------------------------------------------------------------
-- |
-- Module      : Network.Google.PubSub
-- Copyright   : (c) 2022 Patrick Suggate
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
------------------------------------------------------------------------------

module Network.Google.PubSub
  (
    module Export

  , type AllowPubSubRequest
  , type PubSubScopes
  , HasDetailsOf (..)
  , TopicName (..)

  , getTopic
  , topicList
  , publish

  , toMessage
  )
where

import           Control.Lens         (lens, (?~))
import           Control.Monad.Google as Export
import           Data.Aeson           as Aeson
import           Data.Event.Status    as Export (HasDetailsOf (..))
import           Data.Google.Types    as Export
import qualified Gogol                as Google
import qualified Gogol.Auth.Scope     as Google
import qualified Gogol.PubSub         as PubSub
import           Relude


-- * Type constraints
------------------------------------------------------------------------------
-- | Allow a Pub/Sub action to be performed if any of the required scopes are
--   present.
-- type AllowPubSubRequest scopes = Google.AllowRequest PubSub.PubSubProjectsTopicsPublish scopes
type AllowPubSubRequest scopes =
  ( Google.KnownScopes scopes
  , Google.SatisfyScope PubSubScopes scopes
  )

type PubSubScopes = '[PubSub.CloudPlatform'FullControl, PubSub.Pubsub'FullControl]

------------------------------------------------------------------------------
instance HasDetailsOf PubSub.PubsubMessage (Maybe ByteString) where
  detailsOf = lens f g
    where
      f = fmap fromBase64 . PubSub.data'
      g = \r s -> r { PubSub.data' = fmap Base64 s }


-- * Pub/Sub data types
------------------------------------------------------------------------------
-- | Wrapped so that type-inference can be used, when extracting topics from
--   compound data structures.
newtype TopicName
  = TopicName { getTopicName :: Text }
  deriving (Eq, Generic, NFData, Show)
  deriving newtype (FromJSON, ToJSON)

instance IsString TopicName where
  fromString = TopicName . toText

instance ToText TopicName where
  toText = getTopicName

instance HasPath TopicName where
  pathOf = ("topics/" <>) . getTopicName


-- * Pub/Sub top-level API
------------------------------------------------------------------------------
getTopic
  :: AllowPubSubRequest scopes
  => Project
  -> TopicName
  -> Google scopes PubSub.Topic
getTopic proj topic = Google $ do
  let treq = PubSub.newPubSubProjectsTopicsGet path
      path = pathOf proj <> "/" <> pathOf topic
  flip Google.send treq =<< ask

topicList
  :: AllowPubSubRequest scopes
  => Project
  -> Google scopes (Maybe [PubSub.Topic])
topicList proj = Google $ do
  env <- ask
  let path = pathOf proj
  PubSub.topics <$> env `Google.send` PubSub.newPubSubProjectsTopicsList path

------------------------------------------------------------------------------
-- | Send a message to the indicated Pub/Sub topic.
--
--   == TODO:
--    - use @KnownScopes@ (or whatever) for the set of supported scopes;
--    - extract the @Project@ from the @Env scopes@?
--
publish
  :: forall a. ToJSON a
  => Project
  -> TopicName
  -> a
  -> Google PubSubScopes (Maybe [Text])
publish proj topic payload = Google $ do
  env <- ask
  let path = pathOf proj <> "/" <> pathOf topic
      mesg = toMessage . toStrict . Aeson.encode
      preq = PubSub.newPublishRequest { PubSub.messages = Just [mesg payload] }
  PubSub.messageIds <$> env `Google.send` PubSub.newPubSubProjectsTopicsPublish preq path


-- * Conversions
------------------------------------------------------------------------------
toMessage :: ByteString -> PubSub.PubsubMessage
toMessage payload = PubSub.newPubsubMessage & detailsOf ?~ payload
