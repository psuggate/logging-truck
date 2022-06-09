{-# LANGUAGE ConstraintKinds, DataKinds, DerivingStrategies,
             DuplicateRecordFields, FlexibleContexts, FunctionalDependencies,
             MultiParamTypeClasses, OverloadedStrings, PatternSynonyms,
             ScopedTypeVariables, TypeFamilies #-}

------------------------------------------------------------------------------
-- |
-- Module      : Network.Google.BigQuery
-- Copyright   : (c) 2022 Patrick Suggate
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
------------------------------------------------------------------------------

module Network.Google.BigQuery
  (
    module Export

  , type AllowBigQueryRequest
  , type BigQueryScopes

  , createDataset
  )
where

import           Control.Lens         (lens, (?~))
import           Control.Monad.Google as Export
import           Data.Aeson           as Aeson
import           Data.Event.Status    as Export (HasDetailsOf (..))
import           Data.Google.Types    as Export
import           Dhall                (FromDhall)
import qualified Gogol                as Google
import qualified Gogol.Auth.Scope     as Google
import qualified Gogol.BigQuery       as BigQuery
import qualified Gogol.PubSub         as PubSub
import           Relude


-- * Type constraints
------------------------------------------------------------------------------
-- | Allow a Pub/Sub action to be performed if any of the required scopes are
--   present.
type AllowBigQueryRequest scopes =
  ( Google.KnownScopes scopes
  , Google.SatisfyScope BigQueryScopes scopes
  )

type BigQueryScopes
  = '[ BigQuery.CloudPlatform'FullControl
     , BigQuery.Bigquery'FullControl
     ]


-- * BigQuery top-level API
------------------------------------------------------------------------------
type DatasetId = Text

createDataset
  :: Project
  -> DatasetId
  -> Google BigQueryScopes BigQuery.Dataset
createDataset pid did = Google $ do
  env <- ask
  let dref = BigQuery.DatasetReference
        { BigQuery.datasetId = Just did
        , BigQuery.projectId = Just proj
        }
      dreq = BigQuery.newDataset
        { BigQuery.datasetReference = Just dref
        } :: BigQuery.Dataset
      proj = coerce pid
  env `Google.send` BigQuery.newBigQueryDatasetsInsert dreq proj
  -- BigQuery.id <$> env `Google.send` BigQuery.newBigQueryDatasetsInsert dreq proj

createTable
  :: Project
  -> DatasetId
  -> BigQuery.Table
  -> Google BigQueryScopes BigQuery.Table
createTable pid did tab = Google $ do
  pure undefined
