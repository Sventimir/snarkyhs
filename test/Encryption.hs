module Encryption (testEncryption) where

import Data.Polynomial
import Data.Polynomial.Encrypted
import Encryption.Homomorphic
import Test.QuickCheck
import Test.Hspec
import Generators
import Polynomial()

type Enc = SomeEncryption Integer
type Alpha = RestrictionAlpha Integer -- Needs to be different than 1.

testEncryption :: Spec
testEncryption = do
  describe "Test homomorphism." $ do
    it "Addition of pure values is homomorphic." $
      property (homomorphicUnderPureAddition :: (Enc, Integer, Integer) -> Bool)
    it "Addition of encrypted values is homomorphic." $
      property (homomorphicUnderEncryptedAddition :: (Enc, Integer, Integer) -> Bool)
    it "Multiplication is homomorphic" $
      property (homomorphicUnderMultiplication :: (Enc, Integer, Integer) -> Bool)
    it "Polynomial evaluation is homomorphic." $
      property (homomorphicPolynomialEvaluation :: (Enc, Polynomial Integer, Integer) -> Bool)
  describe "Test properties of restricted homomorphic encryption." $ do
    it "Alpha-shift is preserved on fresh restricted values." $
      property (alphaShiftInitiallyHolds :: (Enc, Integer, Alpha) -> Bool)
    it "Alpha-shift is preserved after multiplication" $ do
      property (alphaShiftPreservedOnMultiplication :: (Enc, Integer, Alpha, Integer) -> Bool)
    it "Alpha-shift is preserved when multiplication can be factored out." $
      property (alphaShiftDistributesOverAddition :: (Enc, Integer, Alpha, [Integer]) -> Bool)
    it "After evaluating a polynomial on restricted argument, alpha-shift still holds." $
      -- This is because multiplication each coefficitient by alpha can be factored
      -- out, effectively yielding multiplication of the whole result by alpha.
      property (alphaShiftPreservedAfterPolynomialEval :: (Enc, Polynomial Integer, Integer, Alpha) -> Bool)


homomorphicUnderPureAddition :: Integral a => (SomeEncryption a, a, a) -> Bool
homomorphicUnderPureAddition (SomeEncryption enc, a, b) =
  add (enc a) b == enc (a + b)

homomorphicUnderEncryptedAddition :: Integral a => (SomeEncryption a, a, a) -> Bool
homomorphicUnderEncryptedAddition (SomeEncryption enc, a, b) =
  enc a <> enc b == (enc (a + b))
  
homomorphicUnderMultiplication :: Integral a =>
                                  (SomeEncryption a, a, a) -> Bool
homomorphicUnderMultiplication (SomeEncryption enc, a, b) =
  mul (enc a) b == enc (a * b)

homomorphicPolynomialEvaluation :: Integral a =>
                                   (SomeEncryption a, Polynomial a, a) -> Bool
homomorphicPolynomialEvaluation (SomeEncryption enc, p, x) =
  enc (eval p x) == evalEnc p [ enc (x ^ n) | n <- [(0 :: Integer)..] ]

alphaShiftInitiallyHolds :: Integral a => (SomeEncryption a, a, RestrictionAlpha a) -> Bool
alphaShiftInitiallyHolds (SomeEncryption enc, a, RestrictionAlpha alpha) =
  let r = restrict (enc a) alpha in
    verifyR alpha r

alphaShiftPreservedOnMultiplication :: Integral a =>
                                       (SomeEncryption a, a, RestrictionAlpha a, a) -> Bool
alphaShiftPreservedOnMultiplication (SomeEncryption enc, a, RestrictionAlpha alpha, b) =
  let r = restrict (enc a) alpha in
    verifyR alpha (mulR r b)

alphaShiftDistributesOverAddition :: Integral a =>
                                     (SomeEncryption a, a, RestrictionAlpha a, [a]) -> Bool
alphaShiftDistributesOverAddition (SomeEncryption enc, x, RestrictionAlpha alpha, cs) =
  let r = restrict (enc x) alpha in
    verifyR alpha . foldl addR zeroR $ map (mulR r) cs

alphaShiftPreservedAfterPolynomialEval :: Integral a =>
                                          (SomeEncryption a, Polynomial a, a, RestrictionAlpha a) -> Bool
alphaShiftPreservedAfterPolynomialEval (SomeEncryption enc, p, x, RestrictionAlpha alpha) =
  verifyR alpha $ evalR p [ restrict (enc (x ^ n)) alpha | n <- [(0 :: Integer)..] ]

instance Integral a => Arbitrary (SomeEncryption a) where
  arbitrary = do
    Prime m <- choosePrime (3, 10000)
    Prime g <- choosePrime (2, pred m)
    return $ someEncryption g m
