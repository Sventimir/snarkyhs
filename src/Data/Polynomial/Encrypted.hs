{-# LANGUAGE ExplicitForAll, MultiParamTypeClasses, FunctionalDependencies #-}
module Data.Polynomial.Encrypted
  ( PolynomialEncryption(..)
  , PolynomialRestrictedEncryption(..)
  , evalEnc
  , evalR ) 
where

import Data.Polynomial (Polynomial(..))


class PolynomialEncryption e where
  zero :: Integral a => e a
  addEnc :: Integral a => e a -> e a -> e a
  mulEnc :: Integral a => e a -> a -> e a

class PolynomialRestrictedEncryption e where
  zeroR :: Integral a => e a
  addR :: Integral a => e a -> e a -> e a
  mulR :: Integral a => e a -> a -> e a


evalEnc :: (Integral a, PolynomialEncryption e) => Polynomial a -> [e a] -> e a
evalEnc p xs = foldl addEnc zero $ zipWith mulEnc xs (reverse $ coefficients p)

evalR :: (Integral a, PolynomialRestrictedEncryption e) =>
         Polynomial a -> [e a] -> e a
evalR p xs = foldl addR zeroR $ zipWith mulR xs (reverse $ coefficients p)
