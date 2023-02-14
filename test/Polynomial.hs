module Polynomial (testPolynomial) where

import Data.Group (Group(..))
import Data.List (sort)
import Data.Polynomial
import Data.Ring (Ring(..), zero) 
import Test.Hspec
import Test.QuickCheck hiding (scale)


type P = Polynomial Integer

testPolynomial :: Spec
testPolynomial = do
  describe "Test group properties." $ do
    it "Addition is associative." $
      property $ associative ((<>) :: P -> P -> P)
    it "Addition is commutative" $
      property $ commutative ((<>) :: P -> P -> P)
    it "Zero is neutral with respect to addition." $
      property $ neutral (<>) (zero :: P)
    it "Zero is neutral with respect to subtraction." $
     property $ neutral (~~) (zero :: P)
    it "Subtract self is zero." $
      property (\p -> p ~~ p == (zero :: P))
  describe "Test ring properties." $ do
    it "Multiplication is associative." $
      property $ associative ((<.>) :: P -> P -> P)
    it "Multiplication is commutative." $ do
      property $ commutative ((<.>) :: P -> P -> P)
    it "Unit is neutral with respect to multiplication." $
      property (neutral (<.>) (unit :: P))
    it "Scaling is multiplication by a constant polynomial." $
      property scaleIsMult
    it "Multiplication by zero is zero" $
      property (multZeroIsZero :: P -> Bool)
  describe "Test distributive laws." $ do
    it "Multiplication distributes over addition." $
      property $ distributive ((<>) :: P -> P -> P) (<.>)
    it "Multiplication distributes over subtraction." $
      property $ distributive ((~~) :: P -> P -> P) (<.>)
  describe "Test root properties." $ do
    it "Polynomial evaluates to 0 at each root." $
      property evalRootIsZero
    it "Polynomial has a number of roots equal to its degree." $
      property hasDegreeRoots
    it "Polynomial is monotonic beyond its roots." $
      property polynomialMonotonicBeyondRoots
  describe "Test division." $ do
    it "Result * divisor + the remainder gives the dividend." $
      property (divisionResultCheck :: P -> NonZeroPolynomial Integer -> Bool)
    it "Zero is neutral with respect to division" $
      property $ \p -> p </> (unit :: P) == (p, mempty)
    it "Division by itself returns unit." $
      property $ \(NonZeroPolynomial p) -> (p :: P) </> p == (unit, mempty)
    

associative :: Eq a => (a -> a -> a) -> (a, a, a) -> Bool
associative op (a, b, c) = ((a `op` b) `op` c) == (a `op` (b `op` c))

commutative :: Eq a => (a -> a -> a) -> (a, a) -> Bool
commutative op (a, b) = op a b == op b a

neutral :: Eq a => (a -> a -> a) -> a -> a -> Bool
neutral op neutr a = op a neutr == a

scaleIsMult :: (P, Integer) -> Bool
scaleIsMult (p, s) = scale s p == (p <.> fromCoefficients [s])

distributive :: Eq a => (a -> a -> a) -> (a -> a -> a) -> (a, a, a) -> Bool
distributive add mult (a, b, c) = a `mult` (b `add` c) == (a `mult` b) `add` (a `mult` c)

multZeroIsZero :: (Eq r, Ring r) => r -> Bool
multZeroIsZero r = r <.> zero == zero

evalRootIsZero :: (Integer, [Integer]) -> Bool
evalRootIsZero (scl, roots) = all evalRoot roots
  where
  p = fromRoots scl roots
  evalRoot r = eval p r == 0

hasDegreeRoots :: (NonZero Integer, [Integer]) -> Bool
hasDegreeRoots (NonZero scl, roots) = degree (fromRoots scl roots) == length roots

polynomialMonotonicBeyondRoots :: (NonZero Integer, NonEmptyList Integer, Positive Integer) -> Bool
polynomialMonotonicBeyondRoots (NonZero scl, NonEmpty rs, Positive eps)
  | leadingCoefficient > 0 && maxExptEven = eval p x1 > 0 && eval p x2 > 0
  | leadingCoefficient > 0                = eval p x1 < 0 && eval p x2 > 0
  | maxExptEven                           = eval p x1 < 0 && eval p x2 < 0
  | otherwise                             = eval p x1 > 0 && eval p x2 < 0
  where
  roots = sort rs
  p = fromRoots scl roots
  maxExptEven = even $ degree p
  leadingCoefficient = head $ coefficients p
  x1 = head roots - eps
  x2 = last roots + eps

divisionResultCheck :: Integral a => Polynomial a -> NonZeroPolynomial a -> Bool
divisionResultCheck a (NonZeroPolynomial b) =
  let (q, r) = a </> b in
    (q <.> b) <> r == a

instance (Eq a, Num a, Arbitrary a) => Arbitrary (Polynomial a) where
  arbitrary = fromCoefficients <$> arbitrary

newtype NonZeroPolynomial a = NonZeroPolynomial (Polynomial a)

instance (Eq a, Num a, Arbitrary a) => Arbitrary (NonZeroPolynomial a) where
  arbitrary =
    NonZeroPolynomial . fromCoefficients <$> arbitrary `suchThat` doesNotStartWithZero
    where
    doesNotStartWithZero [] = False
    doesNotStartWithZero (c : _) = c /= 0

instance (Num a, Ord a, Show a) => Show (NonZeroPolynomial a) where
  show (NonZeroPolynomial p) = show p
