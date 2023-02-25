module Properties where

import Data.Ring


associative :: Eq a => (a -> a -> a) -> (a, a, a) -> Bool
associative op (a, b, c) = ((a `op` b) `op` c) == (a `op` (b `op` c))

commutative :: Eq a => (a -> a -> a) -> (a, a) -> Bool
commutative op (a, b) = op a b == op b a

neutral :: Eq a => (a -> a -> a) -> a -> a -> Bool
neutral op neutr a = op a neutr == a

distributive :: Eq a => (a -> a -> a) -> (a -> a -> a) -> (a, a, a) -> Bool
distributive add mult (a, b, c) = a `mult` (b `add` c) == (a `mult` b) `add` (a `mult` c)

multZeroIsZero :: (Eq r, Ring r) => r -> Bool
multZeroIsZero r = r <.> zero == zero

mirror :: Eq a => a -> (a -> a) -> (a -> a -> a) -> a -> Bool
mirror neutr inv op a = a `op` inv a == neutr
