import Encryption
import EquationSolver
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
  testEquationSolver
