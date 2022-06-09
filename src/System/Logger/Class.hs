{-# LANGUAGE DerivingStrategies, FlexibleContexts, FunctionalDependencies,
             MultiParamTypeClasses, OverloadedStrings #-}

-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- | The 'MonadLogger' type-class and associated functions.
module System.Logger.Class
  (
    module Export

  , L.Settings
  , L.Renderer
  , L.defaultSettings
  , L.logLevel
  , L.output
  , L.format
  , L.delimiter
  , L.setNetStrings
  , L.bufSize
  , L.name
  , L.setName
  , L.renderer

  , L.Severity (..)
  , L.Output   (..)

  , L.DateFormat
  , L.iso8601UTC

  , L.Logger
  , L.new
  , L.create
  , L.level
  , L.flush
  , L.close
  , L.clone
  , L.settings

  , MonadLogger (..)
  , trace
  , debug
  , info
  , warn
  , err
  , fatal
  )
where

import           Relude                hiding (trace)
import           System.Logger         (Severity (..))
import qualified System.Logger         as L
import           System.Logger.Message as Export


-- * Convenience function-families
------------------------------------------------------------------------------
class Monad m => MonadLogger m where
  log :: Severity -> (Msg -> Msg) -> m ()


-- * Convenience functions
------------------------------------------------------------------------------
-- | Abbreviation for 'log' using the corresponding log level.
trace, debug, info, warn, err, fatal :: MonadLogger m => (Msg -> Msg) -> m ()
trace = System.Logger.Class.log Trace
debug = System.Logger.Class.log Debug
info  = System.Logger.Class.log Info
warn  = System.Logger.Class.log Warn
err   = System.Logger.Class.log Error
fatal = System.Logger.Class.log Fatal
