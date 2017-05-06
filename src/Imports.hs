{-# LANGUAGE FlexibleInstances #-}

module Imports
  ( module Exports
  , module Imports
  ) where

import Protolude as Exports hiding ((<.>))

import Control.Monad as Exports hiding ((<$!>))

import Data.Char as Exports
import Data.List.Split as Exports
import Data.List.NonEmpty as Exports (NonEmpty)
import Data.String as Exports
import Data.Text as Exports (Text)
import Data.Time as Exports (Day, fromGregorian)
import Data.Hashable as Exports

import System.Directory as Exports
import System.FilePath as Exports
import System.Process as Exports

import Text.Pandoc as Exports hiding (Space, Format, Reader, Meta)

import qualified Data.Text as T


class Show a => Display a where
  displayTitle :: a -> Text
  displayTitle a = "Posts about " <> displayShow a <> "."

  displayDescription :: a -> Text
  displayDescription a = "Posts about " <> displayShow a <> "."

  -- | Turn the showable into a string, splitting it into it's word parts
  -- and joining them with a dash '-'. The result is all lower case.
  displayUrl :: a -> Text
  displayUrl a = case T.uncons (displayShow a) of
    Just (h, t) -> T.foldl' go (T.singleton (toLower h)) $ T.concatMap escape t
    Nothing -> undefined
    where
      go :: Text -> Char -> Text
      go t c = case () of
        _ | isUpper c && isLetter (T.last t) -> t <> "-" <> T.singleton (toLower c)
          | isUpper c -> T.snoc t (toLower c)
          | c == ' '  -> case T.last t of
              '-' -> t
              _ -> T.snoc t '-'
          | otherwise -> T.snoc t c

      escape c = case c of
        -- 'ä' -> "ae"
        -- 'ö' -> "oe"
        -- 'ü' -> "ue"
        -- 'Ä' -> "Ae"
        -- 'Ö' -> "Oe"
        -- 'Ü' -> "Ue"
        _ | isAlphaNum c || c == ' ' -> T.singleton c
        _ -> ""

  -- | Allows you to keep the default implementation of the
  -- functions above, while customizing the way your datatype is shown.
  -- It must guarantee to return non-empty text.
  displayShow :: a -> Text
  displayShow = show

instance Display Text where
  displayShow = T.map toLower
