{-# LANGUAGE DataKinds, GADTs, KindSignatures, ScopedTypeVariables #-}
module Encryption.Homomorphic
  ( Encrypted
  , SomeEncryption(..)
  , proxyEncryption
  , someEncryption
  , encrypt
  , generator
  , modulus
  , add
  , mul )
where

import Data.FiniteField (Fin, fin)
import Data.Kind (Type)
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (Nat, SomeNat(..), KnownNat, natVal, someNatVal)

{- This module provides irreversible homomorphic encryption of numbers
   under which arithmetic operations can still be performed and
   equality relations between unencrypted values still hold for their
   encrypted representations. -}
data Encrypted :: Nat -> Nat -> Type -> Type where
  Encrypted :: (Integral a, KnownNat g, KnownNat m) => Proxy g -> Proxy m -> Fin m a -> Encrypted g m a

data SomeEncryption :: Type -> Type where
  SomeEncryption :: (KnownNat g, KnownNat m) => (a -> Encrypted g m a) -> SomeEncryption a

generator :: Encrypted g m a -> Fin m a
generator (Encrypted g m _) = fin m . fromInteger $ natVal g

modulus :: Encrypted g m a -> Integer
modulus (Encrypted _ m _) = natVal m

proxyEncryption :: (Integral a, KnownNat g, KnownNat m) =>
                  Proxy g -> Proxy m -> SomeEncryption a
proxyEncryption proxyG proxyM = SomeEncryption (encrypt proxyG proxyM)

someEncryption :: Integral a => Integer -> Integer -> SomeEncryption a
someEncryption g m = case (someNatVal (abs g), someNatVal (abs m)) of
                       (Just (SomeNat proxyG), Just (SomeNat proxyM)) ->
                         proxyEncryption proxyG proxyM
                       (_, _) -> error "Failed to create encryption!"
                         
encrypt :: (Integral a, KnownNat g, KnownNat m) => Proxy g -> Proxy m -> a -> Encrypted g m a
encrypt (proxyG :: Proxy g) (proxyM :: Proxy m) a = Encrypted proxyG proxyM (g ^^ a)
  where
  g = fin proxyM . fromInteger $ natVal proxyG

instance Eq a => Eq (Encrypted g m a) where
  (Encrypted _ _ a) == (Encrypted _ _ b) = a == b

instance (Show a, KnownNat g, KnownNat m) => Show (Encrypted g m a) where
  show e@(Encrypted _ _ f) = "<" <> show f <> " mod " <> show (generator e) <> ">"

instance (Integral a, KnownNat g, KnownNat m) => Semigroup (Encrypted g m a) where
  (Encrypted proxyG proxyM a) <> (Encrypted _ _ b) = Encrypted proxyG proxyM (a * b)

instance (Integral a, KnownNat g, KnownNat m) => Monoid (Encrypted g m a) where
  mempty = Encrypted Proxy Proxy 1

-- This is hacky, sorry :D
instance (Num a) => Show (SomeEncryption a) where
  show (SomeEncryption enc) = "<encryption: g = " <> show g <> "; m = " <> show m <> ">"
    where
    Encrypted proxyG proxyM _ = enc 0
    g = natVal proxyG
    m = natVal proxyM

add :: (Integral a, KnownNat g, KnownNat m) => Encrypted g m a -> a -> Encrypted g m a
add a@(Encrypted proxyG proxyM _) b = a <> encrypt proxyG proxyM b

mul :: (Integral a, KnownNat g, KnownNat m) => Encrypted g m a -> a -> Encrypted g m a
mul (Encrypted proxyG proxyM a) b = Encrypted proxyG proxyM (a ^^ b)

