module Generators where

import Data.FiniteField (primes)
import Test.QuickCheck


newtype Prime = Prime Integer
  deriving (Show, Eq)

instance Arbitrary Prime where
  arbitrary = Prime <$> (elements $ take 1000 primes)
