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
  , mul
  , Restricted
  , restrict
  , mulR
  , verifyR
  , withRestricted )
where

import Data.FiniteField (Fin, fin)
import Data.Kind (Type)
import Data.Polynomial.Encrypted (PolynomialEncryption(..), PolynomialRestrictedEncryption(..))
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
  show e@(Encrypted _ _ f) = "<" <> show f <> " mod " <> show (modulus e) <> ">"

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

instance (KnownNat g, KnownNat m) => PolynomialEncryption (Encrypted g m) where
  zero = Encrypted Proxy Proxy 1
  addEnc = (<>)
  mulEnc = mul

data Restricted :: Nat -> Nat -> Type -> Type where
  Restricted :: (KnownNat g, KnownNat m) =>
                Encrypted g m a -> Encrypted g m a -> Restricted g m a


-- Restrict an encrypted value by pairing it with another, shifted (multiplied)
-- by some alpha. Such restricted value behaves just as a simple Encrypted,
-- but performing any operation other than multiplication breaks the shifting
-- relation and can be detected by the verifier provided that he has access to
-- the original alpha.
restrict :: (Integral a, KnownNat g, KnownNat m) =>
            Encrypted g m a -> a -> Restricted g m a
restrict enc alpha = Restricted enc (mul enc alpha)

instance Eq a => Eq (Restricted g m a) where
  (Restricted a a') == (Restricted b b') = a == b && a' == b'

instance (Show a, KnownNat g, KnownNat m) => Show (Restricted g m a) where
  show (Restricted (Encrypted _ proxyM a) (Encrypted _ _ a')) =
    "<" <> show a <> ", " <> show a' <> " mod " <> show (natVal proxyM) <> ">"

-- Given the original alpha-shift, verify that the relation is preserved in
-- the given restricted value. If it is, it means that the original restricted
-- value was only ever multiplied; never added to.
verifyR :: (Integral a, KnownNat g, KnownNat m) => a -> Restricted g m a -> Bool
verifyR alpha (Restricted a a') = mul a alpha == a'

-- This function is useful in tests, so that we can perform arbitrary
-- operations on restricted values and show that they break the alpha-shift.
withRestricted :: (KnownNat g, KnownNat m) =>
                  (Encrypted g m a -> Encrypted g m a) -> Restricted g m a -> Restricted g m a
withRestricted f (Restricted a a') = Restricted (f a) (f a')

instance (KnownNat g, KnownNat m) =>
         PolynomialRestrictedEncryption (Restricted g m) where
  zeroR = Restricted zero zero
  addR (Restricted a a') (Restricted b b') = Restricted (a <> b) (a' <> b')
  mulR (Restricted a a') b = Restricted (mul a b) (mul a' b)
