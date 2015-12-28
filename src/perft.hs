module Perft where

import Data.Sequence

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
    perft' c d g = if c /= d
                   then (c, Data.Sequence.length $ movesAtDepth c g) : perft' (c+1) d g
                   else [(c, Data.Sequence.length $ movesAtDepth c g)]

movesAtDepth :: Depth -> GameState -> Seq GameState
movesAtDepth 1 g = genMoves g
movesAtDepth n g = concatSeq genMoves $ movesAtDepth (n-1) g

