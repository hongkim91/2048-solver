module Alphabeta where

class Node a where
  terminalNode :: a -> Bool
  maxChildren  :: a -> [a]
  minChildren  :: a -> [a]
  rank :: a -> Int

type Depth = Int
data Mode = Max | Min

alphabeta :: Node a => a -> Depth -> Int -> Int -> Mode -> Int
alphabeta node depth alpha beta mode =
  if depth == 0 || terminalNode node then rank node
  else
    case mode of
      Max -> maxSearch (maxChildren node) depth alpha beta
      Min -> minSearch (minChildren node) depth alpha beta

maxSearch :: Node a => [a] -> Depth -> Int -> Int -> Int
maxSearch (x:xs) depth alpha beta =
  let alpha' = max alpha (alphabeta x (depth-1) alpha beta Min) in
  if alpha' < beta
    then maxSearch xs depth alpha' beta
    else alpha'
maxSearch [] _ alpha _ = alpha

minSearch :: Node a => [a] -> Depth -> Int -> Int -> Int
minSearch (x:xs) depth alpha beta =
  let beta' = min beta (alphabeta x (depth-1) alpha beta Max) in
  if alpha < beta'
    then minSearch xs depth alpha beta'
    else beta'
minSearch [] _ _ beta = beta

-------------------------------------------------------------------------------
-- Monadic Attempt
-------------------------------------------------------------------------------
-- import Data.Monoid

-- data AB a = AB [a]

-- instance Monoid (AB a) where
--   mempty = AB []
--   (AB x) `mappend` (AB y) = AB $ x ++ y

-- instance Monad AB where
--   return x = AB [x]
--   (AB []) >>= _ = AB []
--   (AB xs) >>= f = maximumAB $ mconcat $ map f xs

-- maximumAB :: (Ord a) => AB a -> AB a
-- maximumAB (AB []) = error "maximum called on empty list"
-- maximumAB (AB xs) = AB $ [maximum xs]
