module Perft where

import Data.Sequence as S

import ChessData
import Moves

type Depth = Int

-- Run perft tests on a specific state
-- Takes a state, and a depth, and returns the amount of
-- moves per depth
perft :: Depth -> GameState -> [(Depth, Int)]
perft d g = perft' 1 d g
  where
    perft' :: Depth -> Depth -> GameState -> [(Depth, Int)]
    perft' c d g
      | c == d = [(c, S.length $ movesAtDepth c g)]
      | d == 0 || c == 0 = [(0, 1)]
      | otherwise = (c, S.length $ movesAtDepth c g) : perft' (c+1) d g

-- Run a single depth of divide
divide :: Depth -> Seq GameState -> Seq (Maybe Move, Int)
divide d gs = fmap (\x -> (move x, snd $ last $ perft (d-1) x)) gs

movesAtDepth :: Depth -> GameState -> Seq GameState
movesAtDepth 1 g = genMoves g
movesAtDepth n g = concatSeq genMoves $ movesAtDepth (n-1) g

