{-# LANGUAGE DeriveGeneric, NoImplicitPrelude #-}

------------------------------------------------------------------------------
-- |
-- Logger messages are emitted as JSON(L).
--
------------------------------------------------------------------------------

module System.Logger.JSON where

import           Data.Aeson as Aeson (ToJSON)
import           Relude


{-- }
instance Semigroup Msg where Msg xs <> Msg ys = Msg $ xs ++ ys
instance Monoid    Msg where mempty = Msg []


json :: ToJSON a => a -> Msg -> Msg
json  = (mappend .) . toMsg . toJSON

toMsg :: Value -> Msg
toMsg

logJSON :: (Monad m, ToJSON a) => Severity -> a -> m ()
logJSON l x = undefined

renderJSON :: Value -> Msg
renderJSON  =
--}
