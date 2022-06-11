{-# LANGUAGE DuplicateRecordFields, NoImplicitPrelude #-}

module Network.Google.BigQuery.Dataset where

import           Control.Lens                  (lens, (?~))
import           Control.Monad.Google          as Export
import           Data.Aeson                    as Aeson
import           Data.Event.Status             as Export (HasDetailsOf (..))
import           Data.Google.Types             as Export
import qualified Gogol                         as Google
import qualified Gogol.Auth.Scope              as Google
import qualified Gogol.BigQuery                as BigQuery
import qualified Gogol.PubSub                  as PubSub
import           Network.Google.BigQuery.Types as Export
import           Relude


-- * BigQuery dataset top-level API
------------------------------------------------------------------------------
createDataset
  :: Project
  -> DatasetId
  -> Google BigQueryScopes BigQuery.Dataset
createDataset pid did = Google $ do
  env <- ask
  let dref = BigQuery.DatasetReference
        { BigQuery.datasetId = Just (coerce did)
        , BigQuery.projectId = Just proj
        }
      dreq = BigQuery.newDataset
        { BigQuery.datasetReference = Just dref
        } :: BigQuery.Dataset
      proj = coerce pid
  env `Google.send` BigQuery.newBigQueryDatasetsInsert dreq proj
  -- BigQuery.id <$> env `Google.send` BigQuery.newBigQueryDatasetsInsert dreq proj
