module Encryption (testEncryption) where

import Encryption.Homomorphic
import Test.QuickCheck
import Test.Hspec

type PInt = Positive Integer

testEncryption :: Spec
testEncryption = do
  describe "Test homomorphism." $ do
    it "Addition of pure values is homomorphic." $
      property (homomorphicUnderPureAddition :: (EncryptionSpec Integer, PInt, PInt) -> Bool)
    it "Addition of encrypted values is homomorphic." $
      property (homomorphicUnderEncryptedAddition :: (EncryptionSpec Integer, PInt, PInt) -> Bool)
    it "Multiplication is homomorphic" $
      property (homomorphicUnderMultiplication :: (EncryptionSpec Integer, PInt, PInt) -> Bool)


homomorphicUnderPureAddition :: Integral a => (EncryptionSpec a, Positive a, Positive a) -> Bool
homomorphicUnderPureAddition (enc, Positive a, Positive b) =
  addEncPure (encrypt enc a) b == encrypt enc (a + b)

homomorphicUnderEncryptedAddition :: Integral a => (EncryptionSpec a, Positive a, Positive a) -> Bool
homomorphicUnderEncryptedAddition (enc, Positive a, Positive b) =
  addEnc (encrypt enc a) (encrypt enc b) == Just (encrypt enc (a + b))
  
homomorphicUnderMultiplication :: Integral a =>
                                  (EncryptionSpec a, Positive a, Positive a) -> Bool
homomorphicUnderMultiplication (enc, Positive a, Positive b) =
  mulEnc (encrypt enc a) b == encrypt enc (a * b)


instance (Ord a, Num a, Arbitrary a) => Arbitrary (EncryptionSpec a) where
  arbitrary = do
    b <- arbitrary `suchThat` (> 1)
    EncryptionSpec b <$> (arbitrary `suchThat` (> 1))
