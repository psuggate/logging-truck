{-# LANGUAGE DataKinds, DerivingStrategies, FlexibleContexts,
             FunctionalDependencies, MultiParamTypeClasses, NoImplicitPrelude,
             OverloadedStrings, PatternSynonyms, ScopedTypeVariables,
             TypeFamilies #-}

------------------------------------------------------------------------------
-- |
-- Module      : Data.Google.Types
-- Copyright   : (c) 2022 Patrick Suggate
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
------------------------------------------------------------------------------

module Data.Google.Types where
{-- }
  (
    module Export
  , HasPath (..)
  , HasProject (..)
  , Project (..)

  , Severity (..)
  , googleLogger
  , googleLogAction
  , stdoutLogger
  , stderrLogger
  , handleLogger

  , Base64 (..)
  , toBase64
  )
where

import           Control.Lens            (Lens', set, (^.))
import           Control.Monad.Google    as Export hiding (Env)
import           Control.Monad.IO.Unlift
import           Data.Aeson              (FromJSON, ToJSON)
import qualified Data.ByteString.Builder as B
import           Data.Event.Status       hiding (stderrLogger, stdoutLogger)
import           Gogol                   (Base64 (..))
import qualified Gogol.Internal.Logger   as Google
import           Relude
import           System.IO               (hSetBinaryMode)


-- * Convenience function-families
------------------------------------------------------------------------------
class HasPath t where
  pathOf :: t -> Text

class HasProject t a | t -> a where
  projectOf :: Lens' t a


-- * Core Google Cloud Platform (GCP) data types
------------------------------------------------------------------------------
newtype Project
  = Project { getProject :: Text }
  deriving (Eq, Generic, Show)
  deriving newtype (FromJSON, NFData, ToJSON)

instance IsString Project where
  fromString  = Project . toText

instance ToText Project where
  toText  = getProject

instance HasPath Project where
  pathOf  = mappend "projects/" . getProject

instance HasProject Project Project where
  projectOf  = id


-- * Some standard loggers
------------------------------------------------------------------------------
-- | Log to 'stdout' with minimal formatting.
stdoutLogger :: MonadIO m => Severity -> m (LogAction IO StatusMessage)
stdoutLogger  = handleLogger stdout

-- | Log to 'stderr' with minimal formatting.
stderrLogger :: MonadIO m => Severity -> m (LogAction IO StatusMessage)
stderrLogger  = handleLogger stderr

------------------------------------------------------------------------------
-- | Log to the given file handle, and with minimal formatting.
handleLogger
  :: MonadIO m
  => Handle
  -> Severity
  -> m (LogAction IO StatusMessage)
handleLogger h l = do
  liftIO $ do
    hSetBinaryMode h True
    hSetBuffering h LineBuffering
  pure $ LogAction $ \x -> when (x ^. severity >= l) $ do
    B.hPutBuilder h (Google.build $ x ^. detailsOf <> "\n")


-- ** Interop with Gogol
------------------------------------------------------------------------------
-- | Convert a @LogAction m StatusMessage@ logger into a form that is usable
--   with the `gogol` packages.
googleLogger
  :: MonadUnliftIO m
  => LogAction m StatusMessage
  -> m (Google.LogLevel -> B.Builder -> IO ())
googleLogger (LogAction action) = askRunInIO >>= \run -> do
  let go :: Google.Logger
      go l b = run $ action sm
        where
          l' = l ^. severity
          tx = decodeUtf8 (B.toLazyByteString b)
          sm = StatusMessage l' "Google-specific information" tx
  pure go

googleLogAction
  :: MonadIO m
  => Google.Logger
  -> LogAction m StatusMessage
googleLogAction f = LogAction $ \(StatusMessage l _ d) -> liftIO $ do
  f (set severity l Google.Debug) (Google.build d)


-- * Conversions
------------------------------------------------------------------------------
toBase64 :: Text -> Base64
toBase64  = Base64 . encodeUtf8
--}
