 module Moves where

import Debug.Trace (traceShow)
import Data.List (intersect)
import Data.Maybe
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

addPos :: Square -> Square -> Square
addPos (a, b) (c, d) = (a+c, b+d)

-- Just do bounds checking for the time being
legalPos :: Square -> Bool
legalPos (a, b) = a >= 0 && a <= 7 && b >= 0 && b <= 7

-- Is that position on the board empty?
isEmpty :: Board -> Square -> Bool
isEmpty board pos = pieceAt board pos == Empty

-- Check that a square is in a legal place on the board, and that place
-- is either an opposite piece (not a king) or empty
checkSq :: Board -> Colour -> Square -> Bool
checkSq b col sq = legalPos sq && (isOppositeColour b col sq || isEmpty b sq)

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
pawnVectors (_,y) White = if y == 6       -- If the pawn is stll on the home row
                          then empty |> (0,-1) |> (0,-2)
                          else empty |> (0,-1)
pawnVectors (_,y) Black = if y == 1
                          then empty |> (0,1) |> (0,2)
                          else empty |> (0,1)


-- Generate all pseudo-legal moves from a given board position and SideToMove
genMoves :: GameState -> Seq GameState
genMoves g@(GameState b' stm c ep clk s) = (concatSeq (stdMoves g) pieces) >< enPassents
  where
    pieces = fromList $ piecesByColour b' stm
    enPassents = genEnPassentMoves g


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
  fmap (makeMove g) moves
  where
    vects = fromList $ concatMap (sliderPiecePos b' sq stm) $ moveVectors p
    moves = fmap (\x -> Move sq x p Nothing) vects

genNonSliderMoves :: GameState -> (Square, Piece) -> Seq GameState
genNonSliderMoves g@(GameState b' stm c ep clk s) fromPiece@(sq, p@(Piece ptype _))
  | ptype == Pawn = genPawnMoves g sq
  | otherwise     = fmap (makeMove g) moves
  where
    moves         = fmap (\x -> Move sq x p Nothing) legalMoves
    legalMoves    = Data.Sequence.filter (checkSq b' stm) possibleMoves
    possibleMoves = fmap (addPos sq) $ fromList $ moveVectors p

slide :: Board -> Square -> Colour -> (Int, Int) -> Seq Square
slide b' sq col vec = fromList $ sliderPiecePos b' sq col vec

-- Helper function for sequence
concatSeq :: (a -> Seq b) -> Seq a -> Seq b
concatSeq = foldMap


genPawnMoves :: GameState -> Square -> Seq GameState
genPawnMoves g@(GameState b' stm c ep clk s) sq = fmap (makeMove g) moves
  where -- TODO: Handle promotion
    moves        =fmap (\x -> Move sq x (Piece Pawn stm) Nothing) (stdPawnMoves >< pawnCaptures)
    stdPawnMoves = Data.Sequence.filter checkSqPawn $ fmap (addPos sq) $ pawnVectors sq stm
    pawnCaptures = genPawnCaptures g sq
    checkSqPawn psq = legalPos psq && isEmpty b' psq


genPawnCaptures :: GameState -> Square -> Seq Square
genPawnCaptures g@(GameState b' stm c ep clk s) sq = avail (Data.Sequence.filter legalPos $ vectors stm)
  where
    vectors White = fmap (addPos sq) $ empty |> (-1,-1) |> (1, -1)
    vectors Black = fmap (addPos sq) $ empty |> (1, 1) |> (-1, 1)
    avail :: Seq Square -> Seq Square
    avail sqs = Data.Sequence.filter (isOppositeColour b' stm) sqs -- keep the available potential squares


genEnPassentMoves :: GameState -> Seq GameState
genEnPassentMoves g@(GameState b' stm c ep clk s)
  | isNothing ep = empty
  | otherwise    = fromList $ fmap (\x -> makeMove g (Move (fst x) epSq (snd x) Nothing)) candidates
    where
      epSq      = fromJust ep
      attackSqs =  Prelude.filter legalPos $ map (addPos epSq) $ if stm == White
                                                                 then [(1, 1), (-1, 1)]
                                                                 else [(1, -1), (-1, -1)]
      candidates = Prelude.filter (\x -> snd x /= Empty && (ptype $ snd x) == Pawn)
        $ map (\x -> (x, pieceAt b' x)) attackSqs

-- A helper function which will generate a new state, from move information.
-- It will also reverse the side to move, and setup castling and enpassent rights,
-- and also increment the half-move clock
makeMove :: GameState -> Move -> GameState
makeMove g@(GameState b' stm c ep clk s) move@(Move fromSq toSq (Piece ptype _) promo)
  | isJust $ enPassentSq move = makeEnPassentMove g move
  | otherwise                 = makeGeneralMove g move

-- When making an en passent move, the pawn moves normally, but an extra piece
-- must be removed
makeEnPassentMove :: GameState -> Move -> GameState
makeEnPassentMove g@(GameState b' stm c ep clk s) (Move fromSq toSq@(x,y) (Piece ptype _) _)
  = do
    let b1 = movePiece b' fromSq toSq                            -- Move the attacking piece
    let b2 = setBoardCell b1 pos Empty                           -- Take the enPassent victim
    GameState b2 (opposite stm) c Nothing (clk+1) $ evalState stm b2 -- Return a new state
      where
        pos | stm == White = (x, y+1)
            | otherwise    = (x, y-1)

makeGeneralMove :: GameState -> Move -> GameState
makeGeneralMove g@(GameState b' stm c ep clk s) move@(Move fromSq toSq@(x,y) (Piece ptype _) _)
  = GameState board next castling ep nclk s
  where
    board    = movePiece b' fromSq toSq
    next     = opposite stm
    castling = [] -- This is not a castling move (handled elsewhere)
    ep       = enPassentSq move
    nclk     = clk + 1
    s        = evalState next board

enPassentSq :: Move -> Maybe Square
enPassentSq (Move (x,y) (_,y') (Piece ptype _) _)
  | ptype == Pawn = if abs (y - y') == 2
                    then Just (x, (y+y') `quot` 2 )
                    else Nothing
  | otherwise     = Nothing
