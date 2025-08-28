{-# LANGUAGE FlexibleInstances #-}  -- (covers TypeSynonymInstances too)

module Shared.StringUtils where

import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import qualified Data.Text as T

class Trim a where
  trim      :: a -> a
  trimStart :: a -> a
  trimEnd   :: a -> a

-- String instance (no Text dependency at call sites)
instance Trim String where
  trim      = trimEnd . trimStart
  trimStart = dropWhile isSpace
  trimEnd   = dropWhileEnd isSpace

-- Text instance (delegates to Data.Text)
instance Trim T.Text where
  trim      = T.strip
  trimStart = T.stripStart
  trimEnd   = T.stripEnd