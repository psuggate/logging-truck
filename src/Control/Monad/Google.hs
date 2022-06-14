{-# LANGUAGE DataKinds, DerivingStrategies, FlexibleContexts, NoImplicitPrelude,
             OverloadedStrings, PatternSynonyms, RankNTypes,
             ScopedTypeVariables, TypeFamilies #-}

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

module Control.Monad.Google where
{-- }
  (
    Google (..)
  , GoogleT (..)

  , HasEnv (..)
  , Env
  , MonadUnliftIO

  , makeEnv
  , newGoogleEnv

  , runGoogle
  , runGoogleT
  , withGoogle
  )
where

import           Control.Lens                 (lens, (.~))
import           Control.Monad.IO.Unlift      (MonadUnliftIO)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           GHC.TypeLits                 (Symbol)
import           Gogol                        (Env, HasEnv (..))
import qualified Gogol                        as Google
import qualified Gogol.Auth                   as Google
import           Relude
import           System.Logger                (HasSeverity (..), Severity (..))


-- * Google monad
------------------------------------------------------------------------------
newtype Google scopes a
  = Google { getGoogle :: ReaderT (Env scopes) (ResourceT IO) a }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadIO
    , MonadReader (Env scopes)
    , MonadUnliftIO
    )

------------------------------------------------------------------------------
-- | Generalise the above monad to work with more environment-types, and over
--   more base-monads.
newtype GoogleT env (scopes :: [Symbol]) m a
  = GoogleT { getGoogleT :: ReaderT env (ResourceT m) a }
  deriving newtype (Applicative, Functor, Monad, MonadReader env)


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
-- | Evaluate the @GoogleT@-based action within the given context.
runGoogleT
  :: forall env scopes m a. MonadUnliftIO m
  => Google.KnownScopes scopes
  => Google.HasEnv scopes env
  => env
  -> GoogleT env scopes m a
  -> m a
runGoogleT env = runResourceT . flip runReaderT env . getGoogleT

------------------------------------------------------------------------------
-- | Evaluate the @Google@ action within the given context.
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

{-- }
------------------------------------------------------------------------------
-- | Run the Google Cloud action within an appropriately-scoped context.
withGoogleT
  :: forall env scopes m a. MonadUnliftIO m
  => Google.KnownScopes scopes
  => Google.HasEnv scopes env
  => GoogleT env scopes m a
  -> m a
withGoogleT action = do
  lgr <- Google.newLogger Google.Debug stdout
  env <- Google.newEnv
    <&> (Google.envLogger .~ lgr)
      . (Google.envScopes .~ (Proxy :: Proxy scopes))
  runGoogleT env action
--}
--}
