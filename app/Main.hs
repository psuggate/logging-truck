{-# LANGUAGE LambdaCase, NoImplicitPrelude, OverloadedStrings #-}

module Main where

import           Control.Lens                  (view)
import           Control.Monad.Google          as Google
import           Gogol                         (HasEnv (..))
import           Gogol.Compute.Metadata        (getProjectId)
import           Network.Google.BigQuery.Types (BigQueryScopes, Project (..))
import           Relude


googleProjectId :: Google BigQueryScopes Project
googleProjectId  = do
  liftIO (lookupEnv "GCLOUD_PROJECT") >>= \case
    Just pr -> pure $ Project $ fromString pr
    Nothing -> do
      man <- view envManager
      Project <$> getProjectId man


main :: IO ()
main  = do
  withGoogle $ do
    liftIO . print =<< googleProjectId
  putTextLn "todo ..."
