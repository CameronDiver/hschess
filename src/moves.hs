 module Moves where

import Debug.Trace (traceShow)
import Data.List (intersect)
import qualified Data.Array as A

import ChessData
import Board
import Eval

-- Return move vectors for each piece type
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
    isNotEmpty  = (not (isEmpty b' nextSq))
    legSquares  = (map fst (A.assocs board))
    nextSqLegal = nextSq `elem` legSquares
    opCol       = (isOppositeColour b' col nextSq)

-- Generate pseudo-legal moves, that is, moves that are legal but may
-- leave the player in check which is not legal.
genMoves :: GameState -> [GameState]
genMoves g@(GameState b' stm c ep clk s) =
  concatMap gen pieceSqs
  where
    gen :: (Square, Piece) -> [GameState]
    gen (sq, p) = genMoves' g sq p
    pieceSqs    = piecesByColour b' stm

-- Generate moves for a certain piece on a given square
genMoves' :: GameState -> Square -> Piece -> [GameState]
genMoves' g@(GameState b' _ c _ ep _) sq p@(Piece ptype _)
  | sliderPiece ptype = sliderMoveGen g sq p
  | otherwise         = nonSliderMoveGen g sq p

-- Generate moves for pieces that slide
sliderMoveGen :: GameState -> Square -> Piece -> [GameState]
sliderMoveGen (GameState b' stm c ep clk s) sq p@(Piece _ col) =
  map (makeState (opposite stm) c ep (clk + 1)) moves
  where
    moves = (applyMoves b' sq) (concatMap (sliderPiecePos b' sq col) (moveVectors p))

-- Generate moves for pieces that don't slide
-- TODO: Make an exception for Pawn, King and Rook to handle enPassent and Castling
nonSliderMoveGen :: GameState -> Square -> Piece -> [GameState]
nonSliderMoveGen g sq p@(Piece Pawn _) = genPawnMoves g sq
nonSliderMoveGen  (GameState b' stm c ep clk s) sq p@(Piece _ col) =
  map (makeState (opposite stm) c ep (clk + 1)) moves
  where
    moves        = applyMoves b' sq (filter (legalBoardPos b' col) $ intersect legalSquares vectors)
    vectors      = map (addPos sq) (moveVectors p)

-- TODO: EnPassent
genPawnMoves :: GameState -> Square -> [GameState]
genPawnMoves g@(GameState b' stm c ep clk s) sq@(x,y) =
  map (makeState (opposite stm) c ep (clk + 1)) moves
  where
    vectors = (map (addPos sq) $ pawnVectors sq stm) ++ captMoves
    moves   = applyMoves b' sq (filter (legalBoardPos b' stm) $ intersect legalSquares vectors)
    captMoves = generatePawnCaptures g sq

pawnVectors :: Square -> Colour -> [Square]
pawnVectors (x,_) White = if x == 6             -- If the pawn is stll on the home row
                          then [(-1,0), (-2,0)] -- x and y are mixed up here, investigate...
                          else [(-1,0)]
pawnVectors (x,_) Black = if x == 1
                          then [(1,0), (2,0)]
                          else [(1,0)]

generatePawnCaptures :: GameState -> Square -> [Square]
generatePawnCaptures g@(GameState b' stm c ep clk s) sq = filter legalPos (avail $ vectors stm)
  where
    vectors White = map (addPos sq) [(-1,-1), (-1, 1)]
    vectors Black = map (addPos sq) [(1, 1), (1, -1)]
    avail [] = []
    avail (psq:psqs) = if isOppositeColour b' stm psq -- keep the available potential squares
                     then psq : avail psqs
                     else avail psqs

legalSquares :: [Square]
legalSquares = [(x,y) | x <- [0..7], y <- [0..7]]

-- A helper function which builds up a GameState with the board as the last arg
makeState :: Colour -> CastlingRights -> Maybe Square -> Int -> Board -> GameState
makeState c cr ep clk b = GameState b c cr ep clk (evalBoard b)

-- Apply a list of moves to the board
applyMoves :: Board -> Square -> [Square]  -> [Board]
applyMoves b pos = map $ movePiece b pos

addPos :: Square -> Square -> Square
addPos (a, b) (c, d) = (a+c, b+d)

-- Just do bounds checking for the time being
legalPos :: Square -> Bool
legalPos (a, b) = a >= 0 && a <= 7 && b >= 0 && b <= 7

-- Is that position on the board empty?
isEmpty :: Board -> Square -> Bool
isEmpty board pos = pieceAt board pos == Empty

isOppositeColour :: Board -> Colour -> Square -> Bool
isOppositeColour board col pos = isOppositeColour' col (pieceAt board pos)
  where
    isOppositeColour' :: Colour -> Piece -> Bool
    isOppositeColour' _ Empty         = False
    isOppositeColour' c (Piece _ col) = c /= col

legalBoardPos :: Board -> Colour -> Square -> Bool
legalBoardPos board col pos = isEmpty board pos || isOppositeColour board col pos

