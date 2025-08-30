module Shared.CollectionUtils where

import qualified Data.Set as Set

distinct :: (Ord a) => [a] -> [a]
distinct = go Set.empty
  where
    go _ [] = []
    go seen (x:xs)
      | x `Set.member` seen = go seen xs
      | otherwise           = x : go (Set.insert x seen) xs