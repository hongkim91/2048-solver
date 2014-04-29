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

-- import Test.HUnit hiding (Node)
import Alphabeta

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
    liftIO $ do
        putStrLn $ "  M: " ++ show (monotonicity board) ++
                   "  S: " ++ show (smoothness board) ++
                   "  F: " ++ show (freeTiles board)
    liftIO $ putStrLn ""
    liftIO $ putStrLn ""

    _ <- getInputChar "Press any key to continue"
    -- liftIO $ threadDelay $ 10^(4::Int)
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
    putStrLn ""
    putStrLn ""
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
  where f (d,b) = (d, alphabeta (BN b) (searchDepth-1) (-10000) (-10000) Min)
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

rowValues :: Row -> [Int]
rowValues row = map fromJust $ filter isJust row

-------------------------------------------------------------------------------
-- Alpha-beta Pruning
-------------------------------------------------------------------------------

data BoardNode = BN Board

-- Required game specific definitions
instance Node BoardNode where
  terminalNode (BN board) = null $ shiftAllDirections board
  maxChildren  (BN board) = map (BN . snd) $ shiftAllDirections board
  minChildren  (BN board) = map BN $ insertAllPositions board
  rank         (BN board) = monotonicity board +
                            smoothness board +
                            freeTiles board

-------------------------------------------------------------------------------
-- STATIC EVALUATION FUNCTIONS
-------------------------------------------------------------------------------

-- coefficients for heuristics: monoticity, smoothness, and # of free tiles
mWeight, sWeight, fWeight :: Int
mWeight = 10
sWeight = 1
fWeight = 27

freeTiles :: Board -> Int
freeTiles board = round $ (fWeight*) $ log' $ sum $ map (sum.(map free)) board
  where free Nothing = 1
        free (Just _)  = 0
        log' x = log' $ (fromIntegral (x :: Integer) :: Double)

evalTerimate :: Board -> Int
evalTerimate board = if null (shiftAllDirections board) then -10000 else 0

normalize :: Int -> Int
normalize x = round $ log (fromIntegral x :: Double) / log 2

normalizeBoard :: Board -> Board
normalizeBoard = map (map normalizeMaybe)
  where normalizeMaybe (Just x) = Just $ normalize x
        normalizeMaybe Nothing = Nothing

convertBoard :: Board -> [[Int]]
convertBoard board = map rowValues $ normalizeBoard board

-- monotonicity of the board in both direcitons
monotonicity :: Board -> Int
monotonicity board = (mWeight*) $ sum $ map (max'.boardMonotonicity) [board', board'T]
  where board' = convertBoard board
        board'T = convertBoard $ transpose board
        max' (x,y) = max x y

-- monotonicity of the board in one direction (left/right)
boardMonotonicity :: [[Int]] -> (Int, Int)
boardMonotonicity b = foldr (\r p -> sumPairs (rowMonotonicity r 0 0) p) (0,0) b
  where sumPairs (x1,y1) (x2,y2) = (x1+x2,y1+y2)

-- increasing/decreasing nature of one row
rowMonotonicity :: [Int] -> Int -> Int -> (Int, Int)
rowMonotonicity (x:y:xs) decr incr
  | x > y = rowMonotonicity (y:xs) (decr + y - x) incr
  | x < y = rowMonotonicity (y:xs) decr (incr + x - y)
  | otherwise = rowMonotonicity (y:xs) decr incr
rowMonotonicity _ decr incr = (decr, incr)

smoothness :: Board -> Int
smoothness board = (sWeight*) $ go board' + go board'T
  where go b = sum $ map evalRow b
        board' = convertBoard board
        board'T = convertBoard $ transpose board
        evalRow (x:y:xs) = evalRow (y:xs) - (abs (x-y))
        evalRow _ = 0

sample :: Board
sample = toBoard [[0,0,0,0],
                  [0,0,0,0],
                  [2,0,4,0],
                  [8,2,0,0]]
  where toBoard = map (map toMaybe)
        toMaybe x = if x > 0 then Just x else Nothing
