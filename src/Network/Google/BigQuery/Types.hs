{-# LANGUAGE ConstraintKinds, DeriveAnyClass, DeriveGeneric, DerivingStrategies,
             GADTs, NoImplicitPrelude, OverloadedStrings #-}

module Network.Google.BigQuery.Types
  (
    module Network.Google.BigQuery.Types
  , module Export
  )
where

import           Data.Aeson        as Aeson
import           Data.Google.Types as Export (Project (..))
import qualified Data.List         as List
import qualified Gogol.Auth.Scope  as Google
import qualified Gogol.BigQuery    as BigQuery
import           Relude
import           Web.HttpApiData   (FromHttpApiData)


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


-- * Data types for BigQuery datasets and their tables
------------------------------------------------------------------------------
newtype DatasetId
  = DatasetId { getDatasetId :: Text }
  deriving newtype (FromHttpApiData, FromJSON, NFData, ToJSON)
  deriving (Eq, Generic, Show)

instance IsString DatasetId where
  fromString = DatasetId . toText

data Dataset
  = Dataset
      { dataset'name     :: Maybe Text
      , dataset'id       :: DatasetId
      , dataset'tables   :: [Table]
      , dataset'location :: Maybe Text
      , dataset'project  :: Project
      }
  deriving (Eq, Generic, NFData, Show)

instance FromJSON Dataset where parseJSON = genericParseJSON jsonOpts'
instance ToJSON   Dataset where toJSON = genericToJSON jsonOpts'

------------------------------------------------------------------------------
newtype TableId
  = TableId { getTableId :: Text }
  deriving newtype (FromHttpApiData, FromJSON, NFData, ToJSON)
  deriving (Eq, Generic, Show)

data Table
  = Table
      { table'id     :: TableId
      , table'schema :: Schema
      }
  deriving (Eq, Generic, NFData, Show)

instance FromJSON Table where parseJSON = genericParseJSON jsonOpts'
instance ToJSON   Table where toJSON = genericToJSON jsonOpts'


-- ** Schema definition data types
------------------------------------------------------------------------------
-- | Schema-definition data type, that supports just a (strict) subset of the
--   BigQuery Schema functionality.
data Schema
  = Schema
      { schema'name   :: Text
      , schema'fields :: [Field]
      }
  deriving (Eq, Generic, NFData, Show)

-- TODO: parse via the `gogol-bigquery` data type, and then extract the
--   desired fields?
instance FromJSON Schema where parseJSON = genericParseJSON jsonOpts'
instance ToJSON   Schema where toJSON = genericToJSON jsonOpts'

------------------------------------------------------------------------------
-- | Encoding of a subset of the BigQuery schema data type.
data Field
  = Field
      { field'name   :: Text
      , field'mode   :: Maybe FieldMode
      , field'type   :: FieldType
      , field'fields :: Maybe [Field]
      }
  deriving (Eq, Generic, NFData, Show)

{-- }
data Field
  = Column Text FieldMode FieldType
  | Struct Text FieldMode [Field]
  deriving (Eq, Generic, NFData, Show)
--}

-- TODO: parse via the `gogol-bigquery` data type, and then extract the
--   desired fields?
instance FromJSON Field where parseJSON = genericParseJSON jsonOpts'
instance ToJSON   Field where toJSON = genericToJSON jsonOpts'

------------------------------------------------------------------------------
data FieldMode
  = NULLABLE
  | REQUIRED
  | REPEATED
  deriving (Enum, Eq, Generic, NFData, Ord, Show)

deriving instance FromJSON FieldMode
deriving instance ToJSON   FieldMode

------------------------------------------------------------------------------
-- | Name of the (SQL) data type for data that is stored within the cells of
--   the corresponding column.
data FieldType
  = INT64
  | FLOAT64
  | NUMERIC
  | BOOL
  | STRING
  | BYTES
  | DATE
  | DATETIME
  | TIME
  | TIMESTAMP
  | STRUCT
  | RECORD
  deriving (Enum, Eq, Generic, NFData, Ord, Show)

deriving instance FromJSON FieldType
deriving instance ToJSON   FieldType


-- * Lenses & instances
------------------------------------------------------------------------------


-- * Helpers
------------------------------------------------------------------------------
jsonOpts' :: Options
jsonOpts'  = defaultOptions
  { omitNothingFields = True
  , fieldLabelModifier = List.tail . List.dropWhile (/= '\'')
  }
