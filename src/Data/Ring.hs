module Data.Ring
  ( Ring(..)
  , zero
  , prod
  ) where

import Data.Group (Group(..))


class Group r => Ring r where
  (<.>) :: r -> r -> r
  unit :: r

prod :: Ring r => [r] -> r
prod = foldl (<.>) unit

zero :: Group g => g
zero = mempty
