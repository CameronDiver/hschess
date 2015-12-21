 module Moves where

import Debug.Trace (traceShow)

import ChessData
import Board

-- Return move vectors for each piece type
moveVectors :: Piece -> [(Int, Int)]
moveVectors (Piece ptype clr) =
	case (ptype, clr) of
		(Pawn, Black)	-> [(y, 1) | y <- [0,1,-1]]
		(Pawn, White)	-> [(y, -1) | y <- [0,1,-1]]
		(Knight, _)		-> [(x, y) | x <- [1, 2, -1, -2], y <- [1, 2, -1, -2], x/=y, x/=(-y)]
		(Rook, _)		-> [(1,0), (0,1), (-1, 0), (0, -1)]
		(Bishop, _)		-> [(1, 1), (-1, -1), (-1, 1), (1, -1)]
		(Queen, _)		-> [(x, y) | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0)]
		(King, _)		-> [(x, y) | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0)]

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
    col   = colour piece
    moves = moveVectors $ piece
    ends  = filter (legalBoardPos board col) $ filter legalPos $ map (addPos pos) moves


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

legalBoardPos :: Board -> Colour -> (Int, Int) -> Bool
legalBoardPos board col pos = isEmpty board pos || isOppositeColour board col pos
