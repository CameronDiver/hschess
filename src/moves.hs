 module Moves where

import Debug.Trace (traceShow)
import Data.List (intersect)
import qualified Data.Array as A
import Data.Sequence

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


isOppositeColour :: Board -> Colour -> Square -> Bool
isOppositeColour board col pos = isOppositeColour' col (pieceAt board pos)
  where
    isOppositeColour' :: Colour -> Piece -> Bool
    isOppositeColour' _ Empty         = False
    isOppositeColour' c (Piece _ col) = c /= col

-- List of legal square positions on the chess board
legalSquares :: [Square]
legalSquares = [(x,y) | x <- [0..7], y <- [0..7]]

-- A helper function which builds up a GameState with the board as the last arg
-- It also swaps the stm
makeState :: Colour -> CastlingRights -> Maybe Square -> Int -> Board -> GameState
makeState c cr ep clk b = GameState b (opposite c) cr ep clk (evalBoard b)

addPos :: Square -> Square -> Square
addPos (a, b) (c, d) = (a+c, b+d)

-- Just do bounds checking for the time being
legalPos :: Square -> Bool
legalPos (a, b) = a >= 0 && a <= 7 && b >= 0 && b <= 7

-- Is that position on the board empty?
isEmpty :: Board -> Square -> Bool
isEmpty board pos = pieceAt board pos == Empty

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

pawnVectors :: Square -> Colour -> Seq Square
pawnVectors (x,_) White = if x == 6                        -- If the pawn is stll on the home row
                          then empty |> (-1,0) |> (-2,0) -- x and y are mixed up here, investigate...
                          else empty |> (-1,0)
pawnVectors (x,_) Black = if x == 1
                          then empty |> (1,0) |> (2,0)
                          else empty |> (1,0)


-- Generate all pseudo-legal moves from a given board position and SideToMove
genMoves :: GameState -> Seq GameState
genMoves g@(GameState b' stm c ep clk s) = concatSeq (stdMoves g) pieces
  where
    pieces = fromList $ piecesByColour b' stm


-- Get all standard moves, that is moves which aren't enpassent, castling, or pawn
-- moves
stdMoves :: GameState -> (Square, Piece) -> Seq GameState
stdMoves g@(GameState b' stm c ep clk s) fromPiece@(_, (Piece ptype _))=
  if sliderPiece ptype
  then genSliderMoves g fromPiece
  else genNonSliderMoves g fromPiece

-- Generate the slider moves for pieces
genSliderMoves :: GameState -> (Square, Piece) -> Seq GameState
genSliderMoves g@(GameState b' stm c ep clk s) fromPiece@(sq, p@(Piece ptype _)) =
  fmap (makeState stm c ep clk) $ applyMoves b' sq (concatSeq (slide b' sq stm) vects)
  where
    vects = fromList $ moveVectors p

genNonSliderMoves :: GameState -> (Square, Piece) -> Seq GameState
genNonSliderMoves g@(GameState b' stm c ep clk s) fromPiece@(sq, p@(Piece ptype _))
  | ptype == Pawn = genPawnMoves g sq
  | otherwise     = fmap (makeState stm c ep clk) $
    applyMoves b' sq $ Data.Sequence.filter (checkSq b') (fmap (addPos sq) $ fromList $ moveVectors p)
  where
    checkSq :: Board -> Square -> Bool
    checkSq b' sq
      | legalPos sq = if isOppositeColour b' stm sq || isEmpty b' sq
                      then True else False
      | otherwise   = False


slide :: Board -> Square -> Colour -> (Int, Int) -> Seq Square
slide b' sq col vec = fromList $ sliderPiecePos b' sq col vec

-- Apply a list of moves to a board
applyMoves :: Board -> Square -> Seq Square -> Seq Board
applyMoves b' sq = fmap $ movePiece b' sq

-- Helper function for sequence
concatSeq :: (a -> Seq b) -> Seq a -> Seq b
concatSeq = foldMap


genPawnMoves :: GameState -> Square -> Seq GameState
genPawnMoves g@(GameState b' stm c ep clk s) sq =
  fmap (makeState stm c ep clk) $ applyMoves b' sq $ (stdPawnMoves >< pawnCaptures)
  where
    stdPawnMoves = fmap (addPos sq) $ pawnVectors sq stm
    pawnCaptures = genPawnCaptures g sq


genPawnCaptures :: GameState -> Square -> Seq Square
genPawnCaptures g@(GameState b' stm c ep clk s) sq = avail (Data.Sequence.filter legalPos $ vectors stm)
  where
    vectors White = fmap (addPos sq) $ empty |> (-1,-1) |> (-1, 1)
    vectors Black = fmap (addPos sq) $ empty |> (1, 1) |>  (1, -1)
    avail :: Seq Square -> Seq Square
    avail sqs = Data.Sequence.filter (isOppositeColour b' stm) sqs -- keep the available potential squares
