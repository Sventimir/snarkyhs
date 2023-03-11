module Data.LinEquationSystem
  ( LinEq
  , fromCoefficients
  , solve
  ) where


newtype LinEq a = LinEq [[a]]
  deriving Show

fromCoefficients :: [[a]] -> Maybe (LinEq a)
fromCoefficients eqs =
  let varCount = length eqs in
    if all (\eq -> length eq == varCount + 1) eqs
    then Just (LinEq eqs)
    else Nothing

solve :: (Eq a, Fractional a) => LinEq a -> Maybe [a]
solve (LinEq []) = Just []
solve (LinEq (eq : eqs)) = (reverse <$> eliminate 0 eq eqs) >>= substitute []

substitute :: (Eq a, Fractional a) => [a] -> [[a]] -> Maybe [a]
substitute solutions [] = Just solutions
substitute solutions (eq : eqs) =
  case dropWhile (== 0) eq of
    [] -> Nothing
    [_] -> Nothing
    (x : xs) ->
      let solution = (last xs - sum (zipWith (*) solutions (init xs))) / x in
        substitute (solution : solutions) eqs
        

eliminate :: (Eq a, Fractional a) => Int -> [a] -> [[a]] -> Maybe [[a]]
eliminate _ eq [] = Just [eq]
eliminate c eq eqs = do
  let (eq', eqs') = if any ((/= 0) . (!! c)) eqs
                    then avoidZeroInDiag c eq eqs
                    else (eq, eqs)
  elimed <- mapM (elimRow eq') eqs'
  case elimed of
    [] -> return [eq]
    (eq'' : eqs'') -> (eq' :) <$> eliminate (succ c) eq'' eqs''
  where
  elimRow eq1 eq2
    | eq1 !! c == 0 = Nothing
    | otherwise =
        let r = (eq2 !! c) / (eq1 !! c) in
          Just (zipWith (\a b -> b - a * r) eq1 eq2)

avoidZeroInDiag :: (Num a, Eq a) => Int -> [a] -> [[a]] -> ([a], [[a]])
avoidZeroInDiag _ eq [] = (eq, [])
avoidZeroInDiag c eq (eq' : eqs)
  | eq !! c == 0 = avoidZeroInDiag c eq' (eqs ++ [eq])
  | otherwise = (eq, eq' : eqs)
