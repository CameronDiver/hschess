 module Moves where

import Debug.Trace (traceShow)

import ChessData
import Board

-- Return move vectors for each piece type
moveVectors :: Piece -> [Square]
moveVectors (Piece ptype clr) =
  case (ptype, clr) of
    (Pawn, Black)   -> [(1, y) | y <- [0,1,-1]]
    (Pawn, White)   -> [(-1, y) | y <- [0,1,-1]]
    (Knight, _)     -> [(x, y) | x <- [1, 2, -1, -2], y <- [1, 2, -1, -2], x/=y, x/=(-y)]
    (Rook, _)       -> [(1,0), (0,1), (-1, 0), (0, -1)]
    (Bishop, _)     -> [(1, 1), (-1, -1), (-1, 1), (1, -1)]
    (Queen, _)      -> [(x, y) | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0)]
    (King, _)       -> [(x, y) | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0)]

-- Can this piece slide more than one square?
sliderPiece :: PieceType -> Bool
sliderPiece ptype =
  case ptype of
    Rook   -> True
    Bishop -> True
    Queen  -> True
    _      -> False


-- Generate pseudo-legal moves, that is, moves that are legal but may
-- leave the player in check which is not legal.
genMoves :: Board -> Square -> [Board]
genMoves board pos = map (movePiece board pos) pslegal
  where
    piece = pieceAt board pos
    col   = colour piece
    vectors = moveVectors $ piece
    pslegal = filter (legalBoardPos board col) $ filter legalPos $ map (addPos pos) vectors
    -- TODO: sliding


addPos :: Square -> Square -> Square
addPos (a, b) (c, d) = (a+c, b+d)

-- Just do bounds checking for the time being
legalPos :: Square -> Bool
legalPos (a, b) = a >= 0 && a <= 7 && b >= 0 && b <= 7

-- Is that position on the board empty?
isEmpty :: Board -> Square -> Bool
isEmpty board pos = pieceAt board pos == Empty

isOppositeColour :: Board -> Colour -> Square -> Bool
isOppositeColour board col pos = (colour (pieceAt board pos)) /= col

legalBoardPos :: Board -> Colour -> Square -> Bool
legalBoardPos board col pos = isEmpty board pos || isOppositeColour board col pos
