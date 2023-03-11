{-# LANGUAGE DataKinds, GADTs, KindSignatures, ScopedTypeVariables #-}
module Generators where

import Data.FiniteField (Fin, fin, primes)
import Data.Kind
import Data.Proxy
import GHC.TypeLits
import Test.Hspec
import Test.QuickCheck


newtype Prime = Prime Integer
  deriving (Show, Eq, Ord)

testPrimeGenerator :: Spec
testPrimeGenerator = do
  describe "Test prime numbers generator." $ do
    it "Generated numbers are always positive." $
      property (\(Prime p) -> p > 0)
    it "Generated numbers are always prime." $
      property isPrime

instance Arbitrary Prime where
  arbitrary = Prime <$> (elements $ take 1000 primes)

choosePrime :: (Integer, Integer) -> Gen Prime
choosePrime (lo, hi) = Prime <$> (elements . takeWhile (<= hi) $ dropWhile (< lo) primes)

isPrime :: Prime -> Bool
isPrime (Prime i) = all (\n -> mod i n > 0) [2..pred i]

newtype RestrictionAlpha a = RestrictionAlpha a
  deriving (Show, Eq)

instance (Num a, Eq a, Arbitrary a) => Arbitrary (RestrictionAlpha a) where
  arbitrary = RestrictionAlpha <$> arbitrary `suchThat` (\x -> x /= 0 && x /= 1)

instance (Integral a, KnownNat m) => Arbitrary (Fin m a) where
  arbitrary =
    let proxy = Proxy :: Proxy m
        modulus = fromInteger $ natVal proxy in
    fin proxy . fromInteger <$> chooseInteger (0, pred modulus)
    
