{-# LANGUAGE DataKinds, GADTs, KindSignatures, ScopedTypeVariables, StandaloneDeriving #-}
module Data.FiniteField
  ( Fin
  , fin
  , finMod
  , primes )
where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Ratio (numerator, denominator)
import GHC.TypeLits (Nat, KnownNat, natVal)


data Fin :: Nat -> Type -> Type where
  Fin :: (KnownNat m, Integral a) => Proxy m -> a -> Fin m a

deriving instance Eq a => Eq (Fin m a)
deriving instance Ord a => Ord (Fin m a)

finMod :: KnownNat m => Fin m a -> Integer
finMod (Fin proxyM _) = natVal proxyM

instance Show a => Show (Fin m a) where
  show (Fin Proxy a) = show a

instance (Integral a, KnownNat m) => Num (Fin m a) where
  (Fin Proxy a) + (Fin proxy b) =
    Fin Proxy $ mod (a + b) (fromInteger $ natVal proxy)
  (Fin Proxy a) * (Fin proxy b) =
    Fin Proxy $ mod (a * b) (fromInteger $ natVal proxy)
  abs = id
  signum _ = Fin Proxy 1
  fromInteger i =
    Fin Proxy $ mod (fromInteger i) (fromInteger $ natVal (Proxy :: Proxy m))
  negate (Fin proxy n) =
    Fin Proxy $ mod (negate n) (fromInteger $ natVal proxy)

instance (Integral a, KnownNat m) => Real (Fin m a) where
  toRational (Fin Proxy n) = toRational n

instance (Integral a, KnownNat m) => Enum (Fin m a) where
  toEnum i =
    Fin Proxy $ mod (toEnum i) (fromInteger $ natVal (Proxy :: Proxy m))
  fromEnum (Fin Proxy n) = fromEnum n
  succ (Fin proxy n) =
    Fin Proxy $ mod (succ n) (fromInteger $ natVal proxy)
  pred (Fin proxy n) =
    Fin Proxy $ mod (pred n) (fromInteger $ natVal proxy)

instance (Integral a, KnownNat m) => Integral (Fin m a) where
  toInteger (Fin Proxy n) = toInteger n
  quotRem a (Fin proxy b)
    | gcd b m == 1 = (a * recip (Fin proxy b), Fin proxy 0)
    | otherwise = (Fin proxy 0, a)
    where
    m = fromInteger $ natVal proxy

-- Warning: inverse works under assumption that a and m are
-- relatively prime. In general this instance will work
-- reliably only for fields where m is prime.
instance (Integral a, KnownNat m) => Fractional (Fin m a) where
  recip (Fin Proxy 0) = error "Divide by zero"
  recip (Fin Proxy a) = Fin Proxy $ mod (a ^ (m - 2)) m
    where
    m = fromInteger $ (natVal :: Proxy m -> Integer) Proxy
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

fin :: (KnownNat m, Integral a) => Proxy m -> a -> Fin m a
fin proxy a = Fin Proxy $ mod a (fromInteger $ natVal proxy)

primes :: [Integer]
primes = genPrimes [2]
  where
  genPrimes :: [Integer] -> [Integer]
  genPrimes [] = 2 : genPrimes [2]
  genPrimes ps@(p : _) =
    let next = nextPrime ps $ succ p in
    next : genPrimes (next : ps)
  possibleFactors n = dropWhile (\f -> f * f > n)
  notFactor n f = mod n f /= 0
  nextPrime ps candidate
    | all (notFactor candidate) (possibleFactors candidate ps) = candidate
    | otherwise = nextPrime ps $ succ candidate
