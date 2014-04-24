> module Main where

> import HS2048 hiding (main)
> import Control.Monad
> import Control.Monad.Logic
> import Control.Monad.Random

Backtracking search for winning moves, maintaining rng state. Winning games can be replayed given the same seed.

> goal :: Maybe Score
> boardSize, seed :: Int
> goal      = Just 2048
> boardSize = 4
> seed      = 666

List of moves for every winning game given a board and a source of random numbers:

> win :: MonadRandom m => [Direction] -> Board -> LogicT m [Direction]
> win moves board = do
>     direction <- msum $ fmap return [North, East, South, West]
>     RoundResult _ moveOutcome newBoard <- lift $ gameRound goal direction board
>     case moveOutcome of
>         Active  -> win (direction : moves) newBoard
>         Lose    -> mzero
>         Invalid -> mzero
>         Win     -> return $ direction : moves

Throw two random tiles on the board and win:

> solutions :: MonadRandom m => LogicT m [Direction]
> solutions = lift (makeStartBoard boardSize) >>= win []

Print moves for first 10 winning games:

> main :: IO ()
> main = do
>     winners <- return $ evalRand (observeAllT solutions) $ mkStdGen seed
>     mapM_ print $ take 10 winners
