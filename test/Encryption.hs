module Encryption where

import Encryption.Homomorphic
import Test.QuickCheck
import Test.Hspec

type PInt = Positive Integer

testEncryption :: Spec
testEncryption = do
  describe "Test homomorphism." $ do
    it "Addition is homomorphic." $
      property (homomorphicUnderAddition :: (EncryptionSpec Integer, PInt, PInt) -> Bool)
    it "Multiplication is homomorphic" $
      property (homomorphicUnderMultiplication :: (EncryptionSpec Integer, PInt, PInt) -> Bool)


homomorphicUnderAddition :: (Eq a, Integral a, Num a) => (EncryptionSpec a, Positive a, Positive a) -> Bool
homomorphicUnderAddition (enc, Positive a, Positive b) =
  addEnc (encrypt enc a) b == encrypt enc (a + b)

homomorphicUnderMultiplication :: (Eq a, Integral a, Num a) =>
                                  (EncryptionSpec a, Positive a, Positive a) -> Bool
homomorphicUnderMultiplication (enc, Positive a, Positive b) =
  mulEnc (encrypt enc a) b == encrypt enc (a * b)


instance (Ord a, Num a, Arbitrary a) => Arbitrary (EncryptionSpec a) where
  arbitrary = do
    b <- arbitrary `suchThat` (> 1)
    EncryptionSpec b <$> (arbitrary `suchThat` (> 1))
