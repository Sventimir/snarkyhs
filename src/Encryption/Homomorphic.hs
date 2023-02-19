module Encryption.Homomorphic
  ( EncryptionSpec(..)
  , Encrypted
  , encModulus
  , encBase
  , encValue
  , encrypt
  , addEnc
  , addEncPure
  , mulEnc
  , EncryptedGen
  , encryptedGen
  , genList )
where

import Control.Monad.State.Lazy (StateT, MonadState(..), evalState)

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
  show enc = "<" <> (show $ encValue enc) <> ">"


addEncPure :: Integral a => Encrypted a -> a -> Encrypted a
addEncPure e a = e { encValue = encValue e * (encBase e ^ a) `mod` encModulus e }

-- Operation only makes sense if both encrypted values have the same base and modulus.
addEnc :: Integral a => Encrypted a -> Encrypted a -> Maybe (Encrypted a)
addEnc a b
  | encBase a == encBase b && encModulus a == encModulus b =
      Just (a { encValue = (encValue a * encValue b) `mod` encModulus a })
  | otherwise = Nothing

mulEnc :: Integral a => Encrypted a -> a -> Encrypted a
mulEnc e a = e { encValue = (encValue e ^ a) `mod` encModulus e }

-- generates encrypted values of type a
newtype EncryptedGen m a = EncryptedGen (StateT a m (Encrypted a))

encryptedGen :: (Integral a, Monad m) => EncryptionSpec a -> (a -> a) -> EncryptedGen m a
encryptedGen spec next = EncryptedGen generate
  where
  generate = do
    g <- get
    put (next g)
    return $ encrypt spec g

genList :: Integral a => EncryptionSpec a -> (a -> a) -> a -> [Encrypted a]
genList spec next = evalState generateList 
  where
  EncryptedGen gen = encryptedGen spec next
  generateList = do
    g <- gen
    (g :) <$> generateList
