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
maxSearch (b:bs) depth alpha beta =
  let alpha' = max alpha (alphabeta b (depth-1) alpha beta Min) in
  if alpha' < beta
    then maxSearch bs depth alpha' beta
    else alpha'
maxSearch [] _ alpha _ = alpha

minSearch :: Node a => [a] -> Depth -> Int -> Int -> Int
minSearch (b:bs) depth alpha beta =
  let beta' = min beta (alphabeta b (depth-1) alpha beta Max) in
  if alpha < beta'
    then minSearch bs depth alpha beta'
    else beta'
minSearch [] _ _ beta = beta
