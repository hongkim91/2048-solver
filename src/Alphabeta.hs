module Alphabeta where

class Node a where
  terminalNode :: a -> Bool
  maxChildren  :: a -> [a]
  minChildren  :: a -> [a]
  rank :: a -> Int

type Depth = Int
data Mode = Max | Min deriving (Eq)

alphabeta :: Node a => a -> Depth -> Int -> Int -> Mode -> Int
alphabeta node depth alpha beta mode
  | depth == 0 || terminalNode node = rank node
  | alpha >= beta = if mode == Max then beta else alpha
  | otherwise = case mode of
      Max -> foldr (\x alpha' -> max alpha' (alphabeta x (depth-1) alpha' beta Min)) alpha (maxChildren node)
      Min -> foldr (\x beta' -> min beta' (alphabeta x (depth-1) alpha beta' Max)) beta (minChildren node)

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
