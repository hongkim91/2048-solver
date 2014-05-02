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

-- import Control.Concurrent
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

    -- _ <- getInputChar "Press any key to continue"
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
        goal = Just 16384

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
  where f (d,b) = (d, alphabeta (BN b) (searchDepth-1) (-10000) (10000) Min)
        searchDepth = 5

-- Required game specific definitions
data BoardNode = BN Board

instance Node BoardNode where
  terminalNode (BN board) = null $ shiftAllDirections board
  maxChildren  (BN board) = map (BN . snd) $ shiftAllDirections board
  minChildren  (BN board) = map BN $ insertAllPositions board
  rank         (BN board) = monotonicity board +
                            smoothness board +
                            freeTiles board +
                            maxValue board +
                            terminate board

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

-------------------------------------------------------------------------------
-- STATIC EVALUATION FUNCTIONS
-------------------------------------------------------------------------------

-- coefficients for heuristics: monoticity, smoothness, and # of free tiles
monoWeight, sWeight, maxWeight :: Int
fWeight :: Double

monoWeight = 10
sWeight = 1
fWeight = 27
maxWeight = 10

-- monotonicity of the board in both direcitons
monotonicity :: Board -> Int
monotonicity board = (monoWeight*) $ sum $ map go [board, transpose board]
  where go = boardMonotonicity . takeValues . normalizeBoard

-- monotonicity of the board in one direction (left/right)
boardMonotonicity :: [[Int]] -> Int
boardMonotonicity b = max' $ foldr combine (0,0) b
  where combine r = sumPairs (rowMonotonicity r)
        sumPairs (x1,y1) (x2,y2) = (x1+x2,y1+y2)
        max' (x,y) = max x y

-- increasing/decreasing nature of one row
rowMonotonicity :: [Int] -> (Int,Int)
rowMonotonicity = foldPair combine (0,0)
  where combine x y (dec, inc)
          | x > y = (dec - abs (x - y), inc)
          | x < y = (dec, inc - abs (x - y))
          | otherwise = (dec, inc)

smoothness :: Board -> Int
smoothness board = (sWeight*) $ sum $ map go [board, transpose board]
  where go b = sum $ map evalRow $ takeValues $ normalizeBoard b
        evalRow = foldPair (\x y b -> b - abs (x-y)) 0

freeTiles :: Board -> Int
freeTiles board = round $ (fWeight*) $ log' $ sum $ concat $ mapBoard free board
  where free Nothing = 1
        free (Just _) = 0
        log' :: Integer -> Double
        log' x = if x == 0 then 0 else log $ fromIntegral x

maxValue :: Board -> Int
maxValue = (maxWeight*) . maximum . concat . takeValues . normalizeBoard

terminate :: Board -> Int
terminate board = if null (shiftAllDirections board) then -10000 else 0

-- helpers
foldPair :: (a -> a -> b -> b) -> b -> [a] -> b
foldPair f b (x:y:xs) = f x y $ foldPair f b (y:xs)
foldPair _ b _ = b

mapBoard :: (Cell -> a) -> Board -> [[a]]
mapBoard = map.map

takeValues :: Board -> [[Int]]
takeValues = map catMaybes

normalizeBoard :: Board -> Board
normalizeBoard = mapBoard $ fmap normalize
  where normalize x = round $ log (fromIntegral x :: Double) / log 2

-- testing
sample :: Board
sample = toBoard [[0,0,0,0],
                  [0,0,0,0],
                  [2,0,4,0],
                  [8,2,0,0]]
  where toBoard = map (map toMaybe)
        toMaybe x = if x > 0 then Just x else Nothing
