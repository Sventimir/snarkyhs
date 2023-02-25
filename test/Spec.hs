import Encryption
import Field
import Polynomial
import Test.Hspec

main :: IO ()
main = hspec $ do
  testField
  testPolynomial
  testEncryption
