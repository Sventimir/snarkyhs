import Encryption
import Polynomial
import Test.Hspec

main :: IO ()
main = hspec $ do
  testPolynomial
  testEncryption
