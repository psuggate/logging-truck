{-# LANGUAGE DataKinds, DerivingStrategies, FlexibleContexts, OverloadedStrings,
             PatternSynonyms, RankNTypes, ScopedTypeVariables, TypeFamilies #-}

------------------------------------------------------------------------------
-- |
-- Module      : Control.Monad.Google
-- Copyright   : (c) 2022 Patrick Suggate
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Patrick Suggate <patrick.suggate@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
------------------------------------------------------------------------------

module Control.Monad.Google
  (
    Google (..)
  , HasEnv (..)
  , Env

  , makeEnv
  , newGoogleEnv

  , runGoogle
  , withGoogle
  )
where

import           Control.Lens                 (lens, (.~))
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Gogol                        (Env, HasEnv (..))
import qualified Gogol                        as Google
import qualified Gogol.Auth                   as Google
import           Relude
import           System.Logger                (HasSeverity (..), Severity (..))

-- import           Control.Monad.Trans.Reader


-- * Google monad
------------------------------------------------------------------------------
newtype Google scopes a
  = Google { getGoogle :: ReaderT (Env scopes) (ResourceT IO) a }
  deriving newtype (Applicative, Functor, Monad, MonadReader (Env scopes))


-- * Instances
------------------------------------------------------------------------------
instance HasSeverity Google.LogLevel where
  severity  = lens g (const s) where
    g Google.Trace = Trace
    g Google.Debug = Debug
    g Google.Error = Error
    g Google.Info  = Info
    s Trace = Google.Trace
    s Debug = Google.Debug
    s Warn  = Google.Error
    s Error = Google.Error
    s Fatal = Google.Error
    s Info  = Google.Info


-- * Google environments
------------------------------------------------------------------------------
makeEnv
  :: forall m scopes. MonadIO m
  => Google.KnownScopes scopes
  => Severity
  -> m (Env scopes)
makeEnv level = liftIO $ do
  logger <- Google.newLogger (severity .~ level $ Google.Debug) stdout
  Google.newEnv
    <&> Google.envLogger .~ logger
    <&> Google.envScopes .~ (Proxy :: Proxy scopes)

newGoogleEnv
  :: forall m scopes. MonadIO m
  => Google.KnownScopes scopes
  => Google.Logger
  -> m (Env scopes)
newGoogleEnv logger = liftIO $ do
  Google.newEnv
    <&> Google.envLogger .~ logger
    <&> Google.envScopes .~ (Proxy :: Proxy scopes)


-- * Google evaluation
------------------------------------------------------------------------------
runGoogle
  :: forall scopes a. Google.KnownScopes scopes
  => Env scopes
  -> Google scopes a
  -> IO a
runGoogle env = runResourceT . flip runReaderT env . getGoogle

------------------------------------------------------------------------------
-- | Run the Google Cloud action within an appropriately-scoped context.
withGoogle
  :: forall scopes a. Google.KnownScopes scopes
  => Google scopes a
  -> IO a
withGoogle action = do
  lgr <- Google.newLogger Google.Debug stdout
  env <- Google.newEnv
    <&> (Google.envLogger .~ lgr)
      . (Google.envScopes .~ (Proxy :: Proxy scopes))
  runGoogle env action
