{-# LANGUAGE DataKinds, DerivingStrategies, FlexibleContexts, NoImplicitPrelude,
             OverloadedStrings, PatternSynonyms, RankNTypes,
             ScopedTypeVariables, TypeFamilies #-}

module Network.Google.BigQuery.Table where

import           Control.Lens                  (lens, (?~))
import           Control.Monad.Google
import           Control.Monad.Google          as Export
import           Data.Aeson                    as Aeson
import           Data.Event.Status             as Export (HasDetailsOf (..))
import           Data.Google.Types             as Export
import           Gogol                         (Env, HasEnv (..))
import qualified Gogol                         as Google
import qualified Gogol.Auth                    as Google
import qualified Gogol.Auth.Scope              as Google
import           Gogol.BigQuery
import qualified Gogol.BigQuery                as BigQuery
import           Network.Google.BigQuery.Types
import           Relude


-- * API
------------------------------------------------------------------------------
-- | Create a new table.
createTable
  :: Project
  -> DatasetId
  -> BigQuery.Table
  -> Google BigQueryScopes BigQuery.Table
createTable pid did tab = Google $ do
  pure undefined

{-- }
create :: HasProject e => DatasetId -> TableId -> GoogleT e BigQueryScopes Table
create did tid = withGoogleT createTable

-- lookup
-- update (via 'patch'?)
-- delete
-- list
--}
