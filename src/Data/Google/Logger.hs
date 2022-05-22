{-# LANGUAGE FlexibleContexts, LambdaCase #-}

module Data.Google.Logger
  (
    module Export

  , makeLogger
  , translateSeverity
  )
where

import qualified Colog.Core              as Colog
import           Data.ByteString.Builder (Builder)
import           Gogol                   as Export (LogLevel (..), Logger)
import           Relude

-- import           Data.Google.Types
-- import qualified Gogol                   as Google


makeLogger
  :: (MonadReader e m, MonadIO m, Colog.HasLog e Builder IO)
  => LogLevel
  -> m (LogLevel -> Builder -> IO ())
makeLogger level = do
  action <- asks Colog.getLogAction
  pure $ \l b -> when (l >= level) $ action Colog.<& b
  -- pure $ \_ b -> action Colog.<& b

translateSeverity :: LogLevel -> Colog.Severity
translateSeverity  = \case
  Info  -> Colog.Info
  Error -> Colog.Error
  Debug -> Colog.Debug
  Trace -> Colog.Debug
