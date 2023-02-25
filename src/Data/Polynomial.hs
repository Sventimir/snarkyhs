module Data.Polynomial
  ( Polynomial
  , coefficients
  , eval
  , fromCoefficients
  , fromRoots
  , scale
  , degree
  , (<//>) )
where

import Data.Ring (Group(..), Ring(..), prod)

data Polynomial a = Polynomial
  { coefficients :: [a] }


instance (Ord a, Num a, Show a) => Show (Polynomial a) where
  show p@(Polynomial xs) = joinXs (degree p) xs
    where
    showC c x = if abs c == 1 && x > 0 then "" else show (abs c)
    showX 0 = ""
    showX 1 = "x"
    showX ex = "x^" <> show ex
    showSign c
      | c < 0 = " - "
      | otherwise = " + "
    joinXs _ [] = "0"
    joinXs _ [c] = show c
    joinXs d (c : cs) = showC c d <> showX d <> joinRest (d - 1) cs
    joinRest _ [] = ""
    joinRest d (0 : cs) = joinRest (d - 1) cs
    joinRest d (c : cs) = showSign c <> showC c d <> showX d <> joinRest (d - 1) cs

eval :: Num a => Polynomial a -> a -> a
eval p x = fst . foldl addTerm (fromInteger 0, degree p) $ coefficients p
  where
  addTerm (total, expt) c = (total + c * (x ^ expt), expt - 1)

scale :: (Eq a, Num a) => a -> Polynomial a -> Polynomial a
scale 0 _ = Polynomial []
scale s (Polynomial ps) = Polynomial ((* s) <$> ps)

normalize :: (Eq a, Num a) => Polynomial a -> Polynomial a
normalize (Polynomial ps) = Polynomial $ dropWhile (== 0) ps

degree :: Polynomial a -> Int
degree (Polynomial []) = 0
degree (Polynomial cs) = length cs - 1

fromCoefficients :: (Eq a, Num a) => [a] -> Polynomial a
fromCoefficients cs = Polynomial $ dropWhile (== 0) cs

fromRoots :: (Eq a, Num a) => a -> [a] -> Polynomial a
fromRoots scl rs = normalize . scale scl $ prod ((\r -> Polynomial [1, -r]) <$> rs)

instance (Eq a, Num a) => Eq (Polynomial a) where
  (Polynomial ps) == (Polynomial qs) = ps == qs

instance (Eq a, Num a) => Semigroup (Polynomial a) where
  (Polynomial ps) <> (Polynomial qs) = normalize . Polynomial
                                       . fmap (uncurry (+))
                                       $ equilong 0 ps qs

instance (Eq a, Num a) => Monoid (Polynomial a) where
  mempty = Polynomial []

instance (Eq a, Num a) => Group (Polynomial a) where
  neg (Polynomial ps) = Polynomial $ fmap negate ps

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

(<//>) :: Integral a => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
_ <//> (Polynomial []) = error "Polynomial: division by 0 undefined."
p <//> divisor@(Polynomial ds) = longDivCheck mempty p
  where
  divisLen = length ds
  longDivCheck acc (Polynomial coeffs) =
    let degDiff = length coeffs - divisLen in
      if degDiff < 0
      then (normalize acc, Polynomial coeffs)
      else longDivStep degDiff acc coeffs
  longDivStep degDiff acc coeffs = 
    let (c, r) = divMod (head coeffs) (head ds)
        acc' = Polynomial (c : replicate degDiff 0)
        divis = divisor <.> scale (-1) acc'
        remainder = Polynomial coeffs <> divis in
      if r == 0
      then longDivCheck (acc <> acc') $ normalize remainder
      else (normalize (acc <> acc'), normalize remainder)
