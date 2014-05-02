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
      Max -> foldr (\x alpha' -> max alpha' (search x depth alpha' beta Min)) alpha (maxChildren node)
      Min -> foldr (\x beta' -> min beta' (search x depth alpha beta' Max)) beta (minChildren node)

search :: Node a => a -> Depth -> Int -> Int -> Mode -> Int
search x depth alpha beta mode =
  if alpha < beta
    then alphabeta x (depth-1) alpha beta mode
    else beta

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
