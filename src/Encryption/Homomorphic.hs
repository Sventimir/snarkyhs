module Encryption.Homomorphic
  ( EncryptionSpec(..)
  , Encrypted
  , encModulus
  , encBase
  , encValue
  , encrypt
  , addEnc
  , mulEnc )
where

{- This module provides irreversible homomorphic encryption of numbers
   under which arithmetic operations can still be performed and
   equality relations between unencrypted values still hold for their
   encrypted representations. -}

data EncryptionSpec a = EncryptionSpec
  { modulus, base :: a }
  deriving Show

data Encrypted a = Encrypted
  { encModulus, encBase, encValue :: a }
  deriving Eq

encrypt :: Integral a => EncryptionSpec a -> a -> Encrypted a
encrypt spec a = Encrypted { encValue = (base spec ^ a) `mod` modulus spec
                           , encModulus = modulus spec
                           , encBase = base spec }

instance Show a => Show (Encrypted a) where
  show enc = "[" <> (show $ encValue enc) <> "]"


addEnc :: (Integral a, Num a) => Encrypted a -> a -> Encrypted a
addEnc e a = e { encValue = encValue e * (encBase e ^ a) `mod` encModulus e }

mulEnc :: (Integral a, Num a) => Encrypted a -> a -> Encrypted a
mulEnc e a = e { encValue = (encValue e ^ a) `mod` encModulus e }
