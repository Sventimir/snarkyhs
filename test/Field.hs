{-# LANGUAGE DataKinds, GADTs, RankNTypes, ScopedTypeVariables, StandaloneDeriving  #-}
 module Field ( testField ) where

import Data.Proxy
import Data.FiniteField (Fin, fin, finMod)
import Generators
import GHC.TypeLits
import Test.Hspec
import Test.QuickCheck


-- For some random boundary m.
data F where
  F :: KnownNat m => Fin m Integer -> F

data F2 where
  F2 :: KnownNat m => Fin m Integer -> Fin m Integer -> F2

data F3 where
  F3 :: KnownNat m => Fin m Integer -> Fin m Integer -> Fin m Integer -> F3

data NonZeroF where
  NonZeroF :: KnownNat m => Fin m Integer -> NonZeroF


testField :: Spec
testField = do
  describe "Test finite field's structure." $ do
    it "All values are smaller than the modulus." $
      property alwaysSmallerThanModulus
    it "All values lesser than or equal to zero" $
      property alwaysGTEZero
  describe "Test algebraic properties of finite fields." $ do
    it "Field addition is associative." $
      property $ associative (+)
    it "Field addition is commutative." $
      property $ commutative (+)
    it "Zero is neutral element of addition." $
      property $ neutral 0 (+)
    it "Negation returns the additive inverse." $
      property $ properInverse 0 negate (+)
    it "Field multiplication is associative." $
      property $ associative (*)
    it "Field multiplication is commutative." $
      property $ commutative (*)
    it "One is neutral element of multiplication." $
      property $ neutral 1 (*)
    it "Any value multiplied by zero is zero." $
      property multZeroIsZero
    it "Inversion returns the multiplicative inverse." $
      property $ (properInverse 1 recip (*) . \(NonZeroF a) -> F a)
    it "Multiplication distributes over addition." $
      property $ distributive (+) (*)
    it "Reciprocal is its own reverse." $
      property recipSelfReverse
    it "Reciprocal is unique." $
      property recipUnique
    it "Any element is divisible by any other." $
      property anyDividesAny

alwaysSmallerThanModulus :: F -> Bool
alwaysSmallerThanModulus (F f) = toInteger f < finMod f

alwaysGTEZero :: F -> Bool
alwaysGTEZero (F f) = toInteger f >= 0

associative :: (forall m. KnownNat m => Fin m Integer -> Fin m Integer -> Fin m Integer) ->
               F3 -> Bool
associative op (F3 a b c) = (a `op` b) `op` c == a `op` (b `op` c)

commutative :: (forall m. KnownNat m => Fin m Integer -> Fin m Integer -> Fin m Integer) ->
               F2 -> Bool
commutative op (F2 a b) = a `op` b == b `op` a

neutral :: (forall m. KnownNat m => Fin m Integer) ->
           (forall m. KnownNat m => Fin m Integer -> Fin m Integer -> Fin m Integer) ->
           F -> Bool
neutral neutr op (F a) = a `op` neutr == a

multZeroIsZero :: F -> Bool
multZeroIsZero (F a) = a * 0 == 0

properInverse :: (forall m. KnownNat m => Fin m Integer) ->
                 (forall m. KnownNat m => Fin m Integer -> Fin m Integer) ->
                 (forall m. KnownNat m => Fin m Integer -> Fin m Integer -> Fin m Integer) ->
                 F -> Bool
properInverse neutr inv op (F a) = a `op` inv a == neutr

distributive :: (forall m. KnownNat m => Fin m Integer -> Fin m Integer -> Fin m Integer) ->
                (forall m. KnownNat m => Fin m Integer -> Fin m Integer -> Fin m Integer) ->
                F3 -> Bool
distributive add mul (F3 a b c) = a `mul` (b `add` c) == (a `mul` b) `add` (a `mul` c)

recipSelfReverse :: NonZeroF -> Bool
recipSelfReverse (NonZeroF a) = (recip $ recip a) == a

recipUnique :: F2 -> Bool
recipUnique (F2 a b)
  | b == 0 = True -- inverse fails for 0, but it's cumbersome to exclude it.
  | a * b == 1 = a == recip b
  | otherwise = a /= recip b

anyDividesAny :: F2 -> Bool
anyDividesAny (F2 a b)
  | b == 0 = True -- division by 0 is prohibited, but it's cumbersome to exclude it.
  | otherwise = let c = a / b in a == b * c


instance Arbitrary F where
  arbitrary = do
    Prime i <- arbitrary
    case someNatVal i of
      Nothing -> error "Finite field generator failed!"
      Just (SomeNat (proxy :: KnownNat m => Proxy m)) -> F . fin proxy <$> chooseInteger (0, pred i)

instance Arbitrary F2 where
  arbitrary = do
    Prime i <- arbitrary
    case someNatVal i of
      Nothing -> error "Finite field generator failed!"
      Just (SomeNat (proxy :: Proxy m)) -> do
        a <- chooseInteger (0, pred i)
        b <- chooseInteger (0, pred i)
        return $ F2 (fin proxy a) (fin proxy b)
    
instance Arbitrary F3 where
  arbitrary = do
    Prime i <- arbitrary
    case someNatVal i of
      Nothing -> error "Finite field generator failed!"
      Just (SomeNat (proxy :: Proxy m)) -> do
        a <- chooseInteger (0, pred i)
        b <- chooseInteger (0, pred i)
        c <- chooseInteger (0, pred i)
        return $ F3 (fin proxy a) (fin proxy b) (fin proxy c)

instance Arbitrary NonZeroF where
  arbitrary = do
    Prime i <- arbitrary
    case someNatVal i of
      Nothing -> error "Finite field generator failed!"
      Just (SomeNat (proxy :: Proxy m)) -> NonZeroF . fin proxy <$> chooseInteger (1, pred i)


deriving instance Show F
deriving instance Show F2
deriving instance Show F3
deriving instance Show NonZeroF
