{-# LANGUAGE OverloadedStrings #-}

-- | Quick crack at a minimal 2048 in haskell

module HS2048 where

import Control.Monad.Random
import Control.Monad.Writer
import Data.Maybe
import Data.List
import Data.Ord

import qualified Data.Text as T

import System.Console.Haskeline

import Control.Concurrent

import Test.HUnit hiding (Node)

type Cell  = Maybe Int
type Row   = [Cell]
type Board = [Row]
type Score = Int
data Direction = East | West | North | South deriving (Show, Eq)

data MoveOutcome = Lose | Win | Active | Invalid
data RoundResult = RoundResult Score MoveOutcome Board

showBoard :: Board -> String
showBoard = T.unpack . T.unlines . fmap formatRow
    where formatRow = T.intercalate "|" . fmap (T.center 4 ' ' . formatCell)
          formatCell (Just x) = T.pack $ show x
          formatCell _ = mempty

shiftRow :: Row -> Writer (Sum Score) Row
shiftRow row = liftM (++ nothings) $ sumPairs justs
    where (justs, nothings) = partition isJust row
          sumPairs (Just x:Just y:zs) | x == y = do
            let total = x + y
            tell $ Sum total
            rest <- sumPairs zs
            return $ Just total : rest ++ [Nothing]
          sumPairs (x:xs) = liftM (x :) $ sumPairs xs
          sumPairs [] = return []

shiftBoard :: Direction -> Board -> (Board, Sum Score)
shiftBoard direction = runWriter . case direction of
    West  -> goWest
    East  -> goEast
    North -> liftM transpose . goWest . transpose
    South -> liftM transpose . goEast . transpose
    where goWest = mapM shiftRow
          goEast = mapM $ liftM reverse . shiftRow . reverse

emptyBoard :: Int -> Board
emptyBoard n = replicate n $ replicate n Nothing

-- | coords of available spaces
available :: Board -> [(Int, Int)]
available = concat . zipWith (zip . repeat) [0..] . fmap (elemIndices Nothing)

--  ew
update :: Board -> (Int, Int) -> Cell -> Board
update board (x, y) val = newBoard
    where (rs, r:rs') = splitAt x board
          (cs, _:cs') = splitAt y r
          newRow = cs <> (val : cs')
          newBoard = rs <> (newRow : rs')

insertRandom :: MonadRandom m => Board -> m (Maybe Board)
insertRandom board
    | null holes = return Nothing
    | otherwise = do
        pos <- liftM (holes !!) $ getRandomR (0, length holes - 1)
        coin <- getRandomR (0 :: Float, 1)
        let newCell = Just $ if coin < 0.9 then 2 else 4
        return . Just $ update board pos newCell
    where holes = available board

winner :: Cell -> Board -> Bool
winner winning = elem winning . concat

gameRound :: MonadRandom m => Cell -> Direction -> Board -> m RoundResult
gameRound goal direction board =
    let (newBoard, Sum newPoints) =
            shiftBoard direction board
        result = RoundResult newPoints
        change = board /= newBoard
    in if not change
        then return $ if null $ available newBoard
            then result Lose newBoard
            else result Invalid board
        else if winner goal newBoard
            then return $ result Win newBoard
            else do
                randoBoard <- insertRandom newBoard
                case randoBoard of
                    Nothing -> return $ result Lose newBoard
                    Just b  -> return $ result Active b

runGame :: Cell -> Board -> Int -> InputT IO ()
runGame goal board score = do
    liftIO . putStrLn $ showBoard board
    liftIO $ putStrLn ""

    liftIO $ threadDelay $ 10^(5::Int)
    direction <- liftIO $ return $ nextDirection board

    RoundResult newPoints moveOutcome newBoard <-
        liftIO $ gameRound goal direction board
    let totalScore = newPoints + score
    case moveOutcome of
        Lose -> liftIO $
            putStrLn $ "You lose with " ++ show totalScore ++ " points."
        Win -> liftIO $
            putStrLn $ "You win with " ++ show totalScore ++ " points!"
        Active -> do
            liftIO $ do
                putStrLn $ "You earned " ++ show newPoints ++ " points."
                putStrLn $ "Total score is " ++ show totalScore ++ " points."
            runGame goal newBoard totalScore
        Invalid -> do
            liftIO $ putStrLn "Invalid move, try again."
            runGame goal newBoard totalScore

makeStartBoard :: MonadRandom m => Int -> m Board
makeStartBoard size = do
    Just board  <- insertRandom (emptyBoard size)
    Just board' <- insertRandom board
    return board'

main :: IO ()
main = do
    let size = 4
        goal = Just 2048

    startBoard <- makeStartBoard size
    putStrLn "Use 'w', 'a', 's', and 'd' to move."
    runInputT defaultSettings $ runGame goal startBoard 0

-------------------------------------------------------------------------------
-- AI
-------------------------------------------------------------------------------

-- Interface b/w 2048 and our AI
nextDirection :: Board -> Direction
nextDirection board =
  let newBoards = shiftAllDirections board in
  if null newBoards then North
  else
    fst $ maximumBy (comparing snd) $ map f newBoards
  where f (d,b) = (d, alphabeta b (searchDepth-1) 1000000 (-1000000) Min)
        searchDepth = 7

-- helpers
shiftAllDirections :: Board -> [(Direction, Board)]
shiftAllDirections board = do
  direction <- [North, East, South, West]
  let board' = fst $ shiftBoard direction board
  guard $ board /= board'
  return (direction, board')

insertAllPositions :: Board -> [Board]
insertAllPositions board = do
  pos <- available board
  coin <- [2, 4]
  return $ update board pos (Just coin)

boardValues :: Board -> [Int]
boardValues board = map fromJust $ filter isJust $ concat board

rowValues :: Row -> [Int]
rowValues row = map fromJust $ filter isJust row

-------------------------------------------------------------------------------
-- Alpha-beta Pruning
-------------------------------------------------------------------------------

-- Required game specific definitions
type Node = Board

terminalNode :: Node -> Bool
terminalNode node = null $ shiftAllDirections node

maxChildren :: Node -> [Node]
maxChildren node = map snd $ shiftAllDirections node

minChildren :: Node -> [Node]
minChildren = insertAllPositions

rank :: Node -> Int
rank board = maximum [(monotonicity board 0 0), (monotonicity (reverse board) 0 0)] 
    + maximum [monotonicity (transpose board) 0 0, monotonicity (reverse (transpose board)) 0 0] 
    + smoothness board
        

--maximum $ boardValues node

-- Generic algorithm
type Depth = Int
data Mode = Max | Min

alphabeta :: Node -> Depth -> Int -> Int -> Mode -> Int
alphabeta node depth alpha beta mode =
  if depth == 0 || terminalNode node then rank node
  else
    case mode of
      Max -> maxSearch (maxChildren node) depth alpha beta
      Min -> minSearch (minChildren node) depth alpha beta

maxSearch :: [Node] -> Depth -> Int -> Int -> Int
maxSearch (b:bs) depth alpha beta =
  let alpha' = max alpha (alphabeta b (depth-1) alpha beta Min) in
  if alpha' < beta
    then maxSearch bs depth alpha' beta
    else alpha'
maxSearch [] _ alpha _ = alpha

minSearch :: [Node] -> Depth -> Int -> Int -> Int
minSearch (b:bs) depth alpha beta =
  let beta' = min beta (alphabeta b (depth-1) alpha beta Max) in
  if alpha < beta'
    then minSearch bs depth alpha beta'
    else beta'
minSearch [] _ _ beta = beta

-------------------------------------------------------------------------------
-- STATIC EVALUATION FUNCTIONS
-------------------------------------------------------------------------------
-- heuristics considered: monotonicity, smoothness, open tiles, corners are largest

evalCorners :: Board -> Int
evalCorners b = sum (map evalRowCorner b) + sum (map evalRowCorner (transpose b))

evalRowCorner :: Row -> Int
evalRowCorner l = case elemIndex (maximum l) l of
                    Nothing -> 0 -- should never happen
                    Just x  -> if x == 0 || x == length l - 1
                               then 10000
                               else 0

evalFreeTiles :: Board -> Int
evalFreeTiles board = sum $ map evalFreeRow board

-- 20000 value is arbitrary score for open cell
evalFreeRow :: Row -> Int
evalFreeRow (c:cs) = case c of
                        Nothing -> 10 + evalFreeRow cs
                        Just _  -> evalFreeRow cs
evalFreeRow []     = 0


{-
Hey, so for smoothness, the way it's implemented 
in the JavaScript version is it's given a point (x,y) in the board,
you traverse in all available directions until you meet a non-empty
cell and subtract the absolute difference of your current
and next cell to the smoothness rank. I think the problem I'm running into
is column traversal (since a board is a list of rows), so I tried
a workaround using transposes, but I think the approach is definitely
flawed.

Weights for the original js version:
Monotonicity: 1.0
Smoothness: 0.1
Freetiles: 2.7 (but they took the log values of the empty tiles for some reason)

-}

smoothness :: Board -> Int
smoothness board = sum (map f board ) 
  + sum (map f (transpose board))
  where f r = rowSmoothness r 0 1

rowSmoothness :: Row -> Int -> Int -> Int
rowSmoothness row strt nxt = let l = length row in
                                if strt < l && nxt < l then
                                case (row !! strt, row !! nxt) of
                                  (Just x, Just y) -> abs (x - y) - rowSmoothness row (strt + 1) (nxt + 1) 
                                  (Just _, Nothing) -> rowSmoothness row strt (nxt+1)
                                  (Nothing, _) -> rowSmoothness row (strt + 1) (nxt + 1)
                                else 0


monotonicity :: Board -> Int -> Int -> Int
monotonicity (r:rs) decr incr = 
  monotonicity rs (decr + fst row) (incr + snd row)
  where row = rowMonotonicity (rowValues r) 0 0
        --h = if not (null (rowValues r )) then head (rowValues r) else 0
monotonicity [] decr incr   = maximum [incr, decr]

rowMonotonicity :: [Int] -> Int -> Int -> (Int, Int)
rowMonotonicity (x:y:xs) decr incr
  | x > y = rowMonotonicity (y:xs) (decr + y - x) incr
  | x < y = rowMonotonicity (y:xs) decr (incr + x - y)
  | otherwise = rowMonotonicity (y:xs) decr incr
rowMonotonicity _ decr incr = (decr, incr)


