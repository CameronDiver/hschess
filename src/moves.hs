 module Moves where

import Debug.Trace (traceShow)

import ChessData
import Board

moveVectors :: PieceType -> [(Int, Int)]
moveVectors Pawn   = undefined
moveVectors Knight = [(x, y) | x <- [1, 2, -1, -2], y <- [1, 2, -1, -2], x/=y, x/=(-y)]
moveVectors Rook   = [(1,0), (0,1), (-1, 0), (0, -1)]
moveVectors Bishop = [(1, 1), (-1, -1), (-1, 1), (1, -1)]
moveVectors Queen  = [(x, y) | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0)]
moveVectors King   = moveVectors Queen

-- Can this piece slide more than one square?
sliderPiece :: PieceType -> Bool
sliderPiece ptype =
  case ptype of
    Rook   -> True
    Bishop -> True
    Queen  -> True
    _      -> False



genMoves :: Board -> (Int, Int) -> [Board]
genMoves board pos = traceShow ends $ undefined
  where
    piece = pieceAt board pos
    moves = moveVectors $ ptype piece
    ends  = filter (isEmpty board) $ filter legalPos $ map (addPos pos) moves


addPos :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPos (a, b) (c, d) = (a+c, b+d)

-- Just do bounds checking for the time being
legalPos :: (Int, Int) -> Bool
legalPos (a, b) = a >= 0 && a <= 7 && b >= 0 && b <= 7

-- Is that position on the board empty?
isEmpty :: Board -> (Int, Int) -> Bool
isEmpty board pos = pieceAt board pos == Empty

isOppositeColour :: Board -> Colour -> (Int, Int) -> Bool
isOppositeColour board col pos = (colour (pieceAt board pos)) /= col
