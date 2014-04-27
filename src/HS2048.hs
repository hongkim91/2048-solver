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
  where f (d,b) = (d, alphabeta b (searchDepth-1) (-10000) (-10000) Min)
        searchDepth = 5

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
minChildren node = insertAllPositions node

rank :: Node -> Int
rank node = maximum $ boardValues node

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
evalCorners b = (evalRowCorner $ head b) + (evalRowCorner $ last b)

evalRowCorner :: Row -> Int
evalRowCorner l = case elemIndex (maximum l) l of
                    Nothing -> 0 -- should never happen
                    Just x  -> if x == 0 || x == length l - 1
                               then 20000
                               else 0

evalFreeTiles :: Board -> Int
evalFreeTiles board = sum $ map evalFreeRow board

-- 20000 value is arbitrary score for open cell
evalFreeRow :: Row -> Int
evalFreeRow (c:cs) = case c of
                        Nothing -> 20000 + evalFreeRow cs
                        Just _  -> evalFreeRow cs
evalFreeRow []     = 0

evalSmoothness :: Board -> Int
evalSmoothness board = undefined

evalListSmoothness :: [Int] -> Int
evalListSmoothness = undefined

smoothTests :: Test
smoothTests = TestList [
    "General smoothness test." ~: evalSmoothness (emptyBoard 4) ~?= 0
    , "Test2" ~: evalSmoothness (emptyBoard 4) ~?= 1
    ]

evalMonotonicity :: Board -> Int
evalMonotonicity = undefined

-- kinda hacky, should handle taking the two sums within a helper function
evalListMonotonicity :: [Int] -> Int -> Int -> Int
evalListMonotonicity (x:y:xs) decr incr
    | x > y     = evalListMonotonicity (y:xs) (decr + y) incr
    | x < y     = evalListMonotonicity (y:xs) decr (incr + y)
    | otherwise = evalListMonotonicity (y:xs) decr incr
evalListMonotonicity _ decr incr = maximum [decr, incr]

monotonTest1 :: Test
monotonTest1 = TestList [
    "List increasing" ~: evalListMonotonicity [ 4, 6, 8, 10] 4 4 ~?= 28
    , "List decreasing" ~: evalListMonotonicity [ 8, 4, 2,  1] 8 8 ~?= 15
    , "List same" ~: evalListMonotonicity [ 2, 2, 2, 2] 0 0 ~?= 0
    , "Not monotonic" ~: evalListMonotonicity [ 2, 1, 2,  1] 2 2 ~?= 4
    ]
