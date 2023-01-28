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
eval p x = fst . foldl addTerm (fromInteger 0, 1) $ coefficients p
  where
  addTerm (total, xpow) c = (total + xpow * c, xpow * x)

scale :: Num a => Polynomial a -> a -> Polynomial a
scale (Polynomial ps) s = Polynomial ((* s) <$> ps)

normalize :: (Eq a, Num a) => Polynomial a -> Polynomial a
normalize (Polynomial ps) = Polynomial (reverse . dropWhile (== 0) $ reverse ps)

degree :: (Num a, Eq a )=> Polynomial a -> Int
degree (Polynomial []) = 0
degree (Polynomial cs) = length cs - 1

fromCoefficients :: (Eq a, Num a) => [a] -> Polynomial a
fromCoefficients cs = Polynomial (reverse $ dropWhile (== 0) cs)

fromRoots :: (Eq a, Num a) => a -> [a] -> Polynomial a
fromRoots scl rs = normalize $ scale (prod ((\r -> Polynomial [-r, 1]) <$> rs)) scl

instance (Eq a, Num a) => Eq (Polynomial a) where
  (Polynomial ps) == (Polynomial qs) =
    dropWhile (== 0) (reverse ps) == dropWhile (== 0) (reverse qs)

instance (Eq a, Num a) => Semigroup (Polynomial a) where
  (Polynomial ps) <> (Polynomial qs) = normalize . Polynomial $ zipCoeffsWith (+) ps qs

instance (Eq a, Num a) => Monoid (Polynomial a) where
  mempty = Polynomial []

instance (Eq a, Num a) => Group (Polynomial a) where
  invert (Polynomial ps) = Polynomial $ fmap negate ps

instance (Eq a, Num a) => Abelian (Polynomial a) where

instance (Eq a, Num a) => Ring (Polynomial a) where
  unit = Polynomial [1]
  (Polynomial ps) <.> (Polynomial qs) = normalize . Polynomial
                                        . foldl (zipCoeffsWith (+)) []
                                        $ multCoeffs [] [] ps qs

zipCoeffsWith :: Num a => (a -> a -> a) -> [a] -> [a] -> [a]
zipCoeffsWith f = doZip []
  where
  doZip acc [] [] = reverse acc
  doZip acc (p : ps) [] = doZip (f p 0 : acc) ps []
  doZip acc [] (q : qs) = doZip (f 0 q : acc) [] qs
  doZip acc (p : ps) (q : qs) = doZip (f p q : acc) ps qs

multCoeffs :: Num a => [a] -> [[a]] -> [a] -> [a] -> [[a]]
multCoeffs _ acc _ [] = acc
multCoeffs prefix acc ps (q : qs) =
  multCoeffs (0 : prefix) ((prefix <> fmap (* q) ps) : acc) ps qs
