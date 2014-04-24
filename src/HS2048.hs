{-# LANGUAGE OverloadedStrings #-}

-- | Quick crack at a minimal 2048 in haskell

module HS2048 where

import Control.Monad.Random
import Control.Monad.Writer
import Data.Maybe
import Data.List

import qualified Data.Text as T

import System.Console.Haskeline

import Control.Concurrent

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
    -- input <- getInputChar "wasd: "
    liftIO $ putStrLn ""
    
    -- let direction = case input of
    --      Just 'w' -> Just North
    --      Just 'a' -> Just West
    --      Just 's' -> Just South
    --      Just 'd' -> Just East
    --      _ -> Nothing

    liftIO $ threadDelay $ 10^(5::Int)
    direction <- nextDirection board

    case direction of
        Nothing ->
            runGame goal board score
        Just dir -> do
            RoundResult newPoints moveOutcome newBoard <-
                liftIO $ gameRound goal dir board
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

nextDirection :: Board -> InputT IO (Maybe Direction)
nextDirection _ = do
  idx <- lift $ randomRIO (0, 3)
  return $ Just $ [North, East, South, West] !! idx
