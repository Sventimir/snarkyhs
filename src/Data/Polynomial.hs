module Data.Polynomial
  ( Polynomial
  , coefficients
  , eval
  , fromCoefficients
  , fromRoots
  , scale
  , degree )
where

import Data.Group (Group(..), Abelian)
import Data.Ring (Ring(..), prod)

data Polynomial a = Polynomial
  { coefficients :: [a] }
  deriving (Show)


eval :: Num a => Polynomial a -> a -> a
eval p x = fst . foldl addTerm (fromInteger 0, degree p) $ coefficients p
  where
  addTerm (total, expt) c = (total + c * (x ^ expt), expt - 1)

scale :: (Eq a, Num a) => Polynomial a -> a -> Polynomial a
scale _ 0 = Polynomial []
scale (Polynomial ps) s = Polynomial ((* s) <$> ps)

normalize :: (Eq a, Num a) => Polynomial a -> Polynomial a
normalize (Polynomial ps) = Polynomial $ dropWhile (== 0) ps

degree :: Polynomial a -> Int
degree (Polynomial []) = 0
degree (Polynomial cs) = length cs - 1

fromCoefficients :: (Eq a, Num a) => [a] -> Polynomial a
fromCoefficients cs = Polynomial $ dropWhile (== 0) cs

fromRoots :: (Eq a, Num a) => a -> [a] -> Polynomial a
fromRoots scl rs = normalize $ scale (prod ((\r -> Polynomial [1, -r]) <$> rs)) scl

instance (Eq a, Num a) => Eq (Polynomial a) where
  (Polynomial ps) == (Polynomial qs) = ps == qs

instance (Eq a, Num a) => Semigroup (Polynomial a) where
  (Polynomial ps) <> (Polynomial qs) = normalize . Polynomial
                                       . fmap (uncurry (+))
                                       $ equilong 0 ps qs

instance (Eq a, Num a) => Monoid (Polynomial a) where
  mempty = Polynomial []

instance (Eq a, Num a) => Group (Polynomial a) where
  invert (Polynomial ps) = Polynomial $ fmap negate ps

instance (Eq a, Num a) => Abelian (Polynomial a) where

instance (Eq a, Num a) => Ring (Polynomial a) where
  unit = Polynomial [1]
  (Polynomial ps) <.> (Polynomial qs) = normalize . Polynomial
                                        . foldl (zipCoeffsWith (+)) []
                                        . uncurry (multCoeffs [] (replicate deg 0) [])
                                        $ unzip coeffs
    where
    coeffs = equilong 0 ps qs
    deg = length coeffs - 1

zipCoeffsWith :: Num a => (a -> a -> a) -> [a] -> [a] -> [a]
zipCoeffsWith f = doZip []
  where
  doZip acc [] [] = reverse acc
  doZip acc (p : ps) [] = doZip (f p 0 : acc) ps []
  doZip acc [] (q : qs) = doZip (f 0 q : acc) [] qs
  doZip acc (p : ps) (q : qs) = doZip (f p q : acc) ps qs

multCoeffs :: Num a => [a] -> [a] -> [[a]] -> [a] -> [a] -> [[a]]
multCoeffs _ _ acc _ [] = acc
multCoeffs prefix suffix acc ps (q : qs) = multCoeffs
                                           (head suffix : prefix)
                                           (tail suffix)
                                           (prefix <> (fmap (* q) ps <> suffix) : acc)
                                           ps
                                           qs

equilong :: a -> [a] -> [a] -> [(a, a)]
equilong padding as bs = zip (pad as) (pad bs)
  where
  len = max (length as) (length bs)
  pad xs = replicate (len - length xs) padding <> xs
