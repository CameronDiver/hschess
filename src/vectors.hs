module Vectors where

import Data.Sequence
import Data.Array

import ChessData
import Board

pawnVectors :: Square -> Colour -> Seq Square
pawnVectors (_,y) White = if y == 6       -- If the pawn is stll on the home row
                          then empty |> (0,-1) |> (0,-2)
                          else empty |> (0,-1)
pawnVectors (_,y) Black = if y == 1
                          then empty |> (0,1) |> (0,2)
                          else empty |> (0,1)

pawnAttackVectors :: Colour -> Square -> Seq Square
pawnAttackVectors White sq = fmap (addPos sq) $ empty |> (-1,-1) |> (1, -1)
pawnAttackVectors Black sq = fmap (addPos sq) $ empty |> (1, 1) |> (-1, 1)


-- Return move vectors for each piece type
-- FIXME: moveVectors need only take a piece type, as the only thing that
-- colour matters for is the pawn, and that is handled elsewhere
moveVectors :: Piece -> [Square]
moveVectors (Piece ptype clr) =
  case (ptype, clr) of
    (Pawn, _)   ->  undefined
    (Knight, _) -> [(x, y) | x <- [1, 2, -1, -2], y <- [1, 2, -1, -2], x/=y, x/=(-y)]
    (Rook, _)   -> [(1,0), (0,1), (-1, 0), (0, -1)]
    (Bishop, _) -> [(1, 1), (-1, -1), (-1, 1), (1, -1)]
    (Queen, _)  -> [(x, y) | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0)]
    (King, _)   -> [(x, y) | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0)]

-- Can this piece slide more than one square?
sliderPiece :: PieceType -> Bool
sliderPiece ptype =
  case ptype of
    Rook   -> True
    Bishop -> True
    Queen  -> True
    _      -> False


addPos :: Square -> Square -> Square
addPos (a, b) (c, d) = (a+c, b+d)

-- Is that position on the board empty?
isEmpty :: Board -> Square -> Bool
isEmpty board pos = pieceAt board pos == Empty

isOppositeColour :: Board -> Colour -> Square -> Bool
isOppositeColour board col pos = isOppositeColour' col (pieceAt board pos)
  where
    isOppositeColour' :: Colour -> Piece -> Bool
    isOppositeColour' _ Empty         = False
    isOppositeColour' c (Piece _ col) = c /= col

-- List of legal square positions on the chess board
legalSquares :: [Square]
legalSquares = [(x,y) | x <- [0..7], y <- [0..7]]

-- Just do bounds checking for the time being
legalPos :: Square -> Bool
legalPos (a, b) = a >= 0 && a <= 7 && b >= 0 && b <= 7

-- Check that a square is in a legal place on the board, and that place
-- is either an opposite piece (not a king) or empty
checkSq :: Board -> Colour -> Square -> Bool
checkSq b col sq = legalPos sq && (isOppositeColour b col sq || isEmpty b sq)


slide :: Board -> Square -> Colour -> (Int, Int) -> Seq Square
slide b' sq col vec = fromList $ sliderPiecePos b' sq col vec

-- if legal position and isnt empty and is opposite colour then can move up to that square
-- if legal position and isnt empty and is same colour then can move till just before that square
-- if legal position and is empty then can move into and past that point
-- if not legal position then cannnot move into that square
sliderPiecePos :: Board -> Square -> Colour -> Square -> [Square]
sliderPiecePos b'@(Board board) (a, b) col (c, d)
  | nextSqLegal && isNotEmpty && opCol = [nextSq]
  | nextSqLegal && isNotEmpty          = []
  | nextSqLegal                        = nextSq : sliderPiecePos b' nextSq col (c, d)
  | otherwise                          = []
  where
    nextSq      = (a+c, b+d)
    isNotEmpty  = not (isEmpty b' nextSq)
    legSquares  = map fst $ assocs board
    nextSqLegal = nextSq `elem` legSquares
    opCol       = isOppositeColour b' col nextSq

