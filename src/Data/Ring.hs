module Data.Ring
  ( Group(..)
  , Ring(..)
  , (~~)
  , zero
  , prod
  ) where

class Monoid g => Group g where
  neg :: g -> g

(~~) :: Group g => g -> g -> g
a ~~ b = a <> neg b

class Group r => Ring r where
  (<.>) :: r -> r -> r
  unit :: r

prod :: Ring r => [r] -> r
prod = foldl (<.>) unit

zero :: Group g => g
zero = mempty
