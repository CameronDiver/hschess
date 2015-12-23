module Eval where

import Data.Array

import ChessData

-- Board evaluation functions
evalBoard :: Board -> Int
evalBoard board =  sum $ [pieceScoreWithCol $ board ! (x, y) | x <- [0..7], y <- [0..7]]

-- Piece evaluation functions
pieceScoreWithCol :: Piece -> Int
pieceScoreWithCol Empty = 0
pieceScoreWithCol p@(Piece ptype col)
  | col == White = pieceScore p
  | otherwise    = -pieceScore p

pieceScore :: Piece -> Int
pieceScore (Piece ptype _) = case ptype of
  Pawn   -> 1
  Knight -> 3
  Bishop -> 3
  Queen  -> 9
  Rook   -> 5
  King   -> infinity

-- Close enough...
infinity :: Int
infinity = 10000
