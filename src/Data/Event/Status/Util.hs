{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, TemplateHaskell #-}

module Data.Event.Status.Util
  (
    module Export
  , messageR
  , sentence
  , someWords
  , randIntN
  , randFrom
  )
where

import           Data.Event.Status    as Export
import           Data.List            ((!!))
import qualified Data.Text            as Text
import qualified Data.Text.Manipulate as Text
import           Relude
import           System.Random        (randomIO)


-- * Utility functions
------------------------------------------------------------------------------
messageR :: IO StatusMessage
messageR  = StatusMessage <$> randFrom levels <*> randFrom states <*> sentence
  where
    levels = [Trace ..Fatal]
    states = [resolved, unknown, starting, running, stopping]
{-- }
messageR  = do
  let lev = [Trace ..Fatal]
      sts = [resolved, unknown, starting, running, stopping]
  sev <- (lev!!) <$> randIntN (length lev)
  st' <- (sts!!) <$> randIntN (length sts)
  StatusMessage sev st' <$> sentence
--}

------------------------------------------------------------------------------
-- | Assemble some words into a sentence.
sentence :: IO Text
sentence  = do
  let go 0 = pure []
      go n = do
        w <- (someWords!!) <$> randIntN l
        (w:) <$> go (n-1)
      l = length someWords
  s  <- (+) <$> succ `fmap` randIntN 6 <*> randIntN 6
  ws <- go s
  -- let cap (w:ws') = (Char.toUpper (Text.head w) `Text.cons` Text.tail w):ws'
  let cap :: [Text] -> [Text]
      cap (w:ws') = Text.upperHead w:ws'
      cap []      = []
  pure $ Text.unwords (cap ws) <> "."

someWords :: [Text]
someWords  = ["aardvark", "sailor", "running", "accentuates", "documentation"
             ,"implements", "tagline", "secure", "Spartacus", "jovial", "by"
             ,"entropy", "truck", "graphene", "holistically", "tennis", "oaf"
             ,"butchered", "talon", "variable", "encouraging", "downright"
             ,"pterodactyl", "dazzling", "below", "internecene", "sheepishly"
             ,"complicated", "extreme", "onomatopoeia", "valid", "asphalt"
             ,"stretched", "salty", "strapping", "femur", "oxygenate", "fresh"
             ]


-- * Miscellaneous functions
------------------------------------------------------------------------------
-- | Generate a random integer within [0, n).
randIntN :: Int -> IO Int
randIntN n = floor . (*fromIntegral n) <$> (randomIO :: IO Double)

randFrom :: [a] -> IO a
randFrom xs = (xs!!) <$> randIntN (length xs)
