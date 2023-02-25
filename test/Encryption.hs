module Encryption (testEncryption) where

import Data.Polynomial
-- import Data.Polynomial.Encrypted
import Encryption.Homomorphic
import Test.QuickCheck
import Test.Hspec
import Generators
import Polynomial

type Enc = SomeEncryption Integer

testEncryption :: Spec
testEncryption = do
  describe "Test homomorphism." $ do
    it "Addition of pure values is homomorphic." $
      property (homomorphicUnderPureAddition :: (Enc, Integer, Integer) -> Bool)
    it "Addition of encrypted values is homomorphic." $
      property (homomorphicUnderEncryptedAddition :: (Enc, Integer, Integer) -> Bool)
    it "Multiplication is homomorphic" $
      property (homomorphicUnderMultiplication :: (Enc, Integer, Integer) -> Bool)
    -- xit "Polynomial evaluation is homomorphic." $
    --   property (homomorphicPolynomialEvaluation :: (SomeEncryption Integer, , PInt) -> Bool)


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

-- homomorphicPolynomialEvaluation :: Integral a =>
--                                    (SomeEncryption a, Polynomial a, Positive a) -> Bool
-- homomorphicPolynomialEvaluation (enc, p, Positive x) =
--   Just (encrypt enc (eval p x)) == evalEnc (encrypt enc 0) p [ encrypt enc (x ^ n) | n <- [0..] ]
  

instance Integral a => Arbitrary (SomeEncryption a) where
  arbitrary = do
    Prime g <- arbitrary
    Prime m <- arbitrary
    return $ someEncryption g m
