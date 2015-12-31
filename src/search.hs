module Search where

import Data.Sequence (Seq)
import Data.Foldable
import Data.List (zip, sortBy)
import Data.Ord (comparing)
import Debug.Trace (traceShow)

import ChessData
import Moves
import Board
import Eval


data GameTree = GameTree
                { state::GameState,
                  nodes::[GameTree]
                } deriving Show

-- Find the best move (according to the evaluation) function
-- to a given depth
searchToDepth :: Int -> GameState -> GameState
searchToDepth depth st = snd bestMove
    where
      bestMove              = maximumBy (comparing fst) moveScores
      moveScores            = [(-(minimax (depth-1) c), state c) | c <- children] :: [(Int, GameState)]
      (GameTree _ children) = createTree st

minimax :: Int -> GameTree -> Int
minimax depth (GameTree st children)
  | depth == 0 = score st
  | otherwise  = if null scores
                 then -1000
                 else maximum scores
  where
    scores = map (negate.(minimax (depth-1))) children

createTree :: GameState -> GameTree
createTree state = GameTree state $ children state

children :: GameState -> [GameTree]
children g = map (\x -> GameTree x $ children x) $ toList (genMoves g)

-- Get a single best move at a given depth 
getBestMove :: GameState -> GameState
getBestMove state = snd $ foldl compGameState (minBound, initialState) scores
  where
    scores :: Seq (Int, GameState)
    scores = fmap (\x -> (score x, x)) moves
    moves = genMoves state

compGameState :: (Int, GameState) -> (Int, GameState) -> (Int, GameState)
compGameState (a, g) (b, g') = if a > b then (a, g) else (b, g')
