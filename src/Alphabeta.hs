module Alphabeta where

class Node a where
  terminalNode :: a -> Bool
  maxChildren  :: a -> [a]
  minChildren  :: a -> [a]
  rank :: a -> Int

type Depth = Int
data Mode = Max | Min deriving (Eq)

-- naive brute force minimax
minimax :: Node a => a -> Depth -> Mode -> Int
minimax node depth mode =
  if depth == 0 || terminalNode node then rank node
  else case mode of
    Max -> foldr (\x m -> max m (minimax x (depth-1) Min)) (-10000) (maxChildren node)
    Min -> foldr (\x m -> min m (minimax x (depth-1) Max)) 10000 (minChildren node)

alphabeta :: Node a => a -> Depth -> Int -> Int -> Mode -> Int
alphabeta node depth alpha beta mode =
  if depth == 0 || terminalNode node then rank node
  else case mode of
    Max -> foldr (\x alpha' -> max alpha' (go x depth alpha' beta Min)) alpha (maxChildren node)
    Min -> foldr (\x beta' -> min beta' (go x depth alpha beta' Max)) beta (minChildren node)
  where go x d a b m
          | a < b = alphabeta x (d-1) a b m
          | otherwise = if mode == Min then b else a

-------------------------------------------------------------------------------
-- QuickCheck Properties
-------------------------------------------------------------------------------

--F2(p, alpha, beta) <= alpha if F(p) <= alpha,
--F2(p, alpha, beta) == F(p)  if alpha < F(p) < beta,
--F2(p, alpha, beta) >= beta  if F(p) >= beta.

data IntNode = IN Int deriving (Show)

instance Node IntNode where
  terminalNode (IN n) = n < 0
  maxChildren  (IN n) = []
  minChildren  (IN n) = []
  rank         (IN n) = n

instance Arbitrary IntNode where
  arbitrary = liftM IN arbitrary

prop_lessThanAlpha :: Node a => a -> Depth -> Int -> Int -> Property
prop_lessThanAlpha node depth alpha beta =
  (minimax node depth Min <= alpha) && (alpha < beta) ==>
  alphabeta node depth alpha beta Min <= alpha

prop_equalsRank :: Node a => a -> Depth -> Int -> Int -> Property
prop_equalsRank node depth alpha beta =
  (minimax node depth Min > alpha) && (minimax node depth Min < beta) && (alpha < beta) ==>
  alphabeta node depth alpha beta Min == minimax node depth Min

prop_greaterThanBeta :: Node a => a -> Depth -> Int -> Int -> Property
prop_greaterThanBeta node depth alpha beta =
  (minimax node depth Min >= beta) && (alpha < beta) ==>
  alphabeta node depth alpha beta Min >= beta
