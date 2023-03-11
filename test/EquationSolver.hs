{-# LANGUAGE GADTs, DataKinds, KindSignatures, ScopedTypeVariables, StandaloneDeriving #-}
module EquationSolver (testEquationSolver) where

import Data.FiniteField
import Data.LinEquationSystem
import Data.Kind
import Data.Proxy
import Generators
import GHC.TypeLits
import Test.Hspec
import Test.QuickCheck


data SolvableEqSystem a = SolvableEqSystem [a] (LinEq a)
  deriving Show

data FinSolvableEqSystem :: Type -> Type where
  FinSolvableEqSystem :: (KnownNat m, Integral a) =>
                         SolvableEqSystem (Fin m a) -> FinSolvableEqSystem a

testEquationSolver :: Spec
testEquationSolver = do
  describe "Test equation system solver with Rationals." $ do
    it "Check computed solutions." $
      property (verifyEqSysSolutions :: SolvableEqSystem Rational -> Bool)
  describe "Test equation system solver with finite fields." $ do
    it "Check computed solutions." $
      property verifyFinEqSystemSolutions


verifyEqSysSolutions :: (Eq a, Fractional a) => SolvableEqSystem a -> Bool
verifyEqSysSolutions (SolvableEqSystem solutions eqs) =
  Just solutions == solve eqs

verifyFinEqSystemSolutions :: FinSolvableEqSystem Integer -> Bool
verifyFinEqSystemSolutions (FinSolvableEqSystem sys) = verifyEqSysSolutions sys


instance (Eq a, Num a, Arbitrary a) => Arbitrary (SolvableEqSystem a) where
  arbitrary = do
    vars <- chooseInt (2, 10)
    solutions <- vector vars
    coeffs <- vectorOf vars $ vector vars `suchThat` any (/= 0)
    let withResults = map (\cs -> cs ++ [sum $ zipWith (*) cs solutions]) coeffs
    case fromCoefficients withResults of
      Nothing -> error "Failed to generate a solvable equation system!"
      Just sys -> return $ SolvableEqSystem solutions sys

instance (Eq a, Num a, Integral a) => Arbitrary (FinSolvableEqSystem a) where
  arbitrary = do
    Prime m <- arbitrary
    case someNatVal m of
      Nothing -> error "Failed to generate a finite field boundary!"
      Just (SomeNat (Proxy :: Proxy m)) -> do
        vars <- chooseInt (2, 10)
        solutions <- vector vars :: Gen [Fin m a]
        coeffs <- (vectorOf vars $ vector vars `suchThat` any (/= 0)) :: Gen [[Fin m a]]
        let withResults = map (\cs -> cs ++ [sum $ zipWith (*) cs solutions]) coeffs
        case fromCoefficients withResults of
          Nothing -> error "Failed to generate a solvable equation system!"
          Just sys -> return . FinSolvableEqSystem $ SolvableEqSystem solutions sys


instance Show a => Show (FinSolvableEqSystem a) where
  show (FinSolvableEqSystem (SolvableEqSystem solutions coefficients :: SolvableEqSystem (Fin m a))) =
    let m = natVal (Proxy :: Proxy m) in
      "SolvableEqSystem {Fin " <> show m <>
      "; solutions: " <> show solutions <>
      "; coefficients: " <> show coefficients <> "}"
