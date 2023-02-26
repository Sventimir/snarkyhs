import Encryption
import Field
import Generators
import Polynomial
import Test.Hspec

main :: IO ()
main = hspec $ do
  testPrimeGenerator
  testField
  testPolynomial
  testEncryption
