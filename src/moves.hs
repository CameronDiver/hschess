 module Moves where

import Debug.Trace (traceShow)
import Data.List (intersect)
import Data.Maybe
import Control.Applicative
import qualified Data.Array as A
import Data.Sequence as S

import ChessData
import Board
import Vectors
import Eval
import Query

-- Generate all pseudo-legal moves from a given board position and SideToMove
genMoves :: GameState -> Seq GameState
genMoves g@(GameState b' stm c ep clk s) = S.filter (not.isInCheck) pslegal
  where
    pieces = fromList $ piecesByColour b' stm
    enPassents = genEnPassentMoves g
    pslegal = concatSeq (stdMoves g) pieces >< enPassents


-- Get all standard moves, that is moves which aren't enpassent, castling, or pawn
-- moves
stdMoves :: GameState -> (Square, Piece) -> Seq GameState
stdMoves g@(GameState b' stm c ep clk s) fromPiece@(_, Piece ptype _)=
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
    legalMoves    = S.filter (checkSq b' stm) possibleMoves
    possibleMoves = fmap (addPos sq) $ fromList $ moveVectors p


-- Helper function for sequence
concatSeq :: (a -> Seq b) -> Seq a -> Seq b
concatSeq = foldMap

genPawnMoves :: GameState -> Square -> Seq GameState
genPawnMoves g@(GameState b' stm c ep clk s) sq = fmap (makeMove g) moves
  where -- TODO: Handle promotion
    moves        = fmap (\x -> Move sq x (Piece Pawn stm) Nothing)
      (stdPawnMoves >< pawnCaptures)
    stdPawnMoves    = S.filter checkSqPawn (addPos sq <$> pawnVectors sq stm)
    pawnCaptures    = genPawnCaptures g sq
    checkSqPawn psq = legalPos psq && isEmpty b' psq


genPawnCaptures :: GameState -> Square -> Seq Square
genPawnCaptures g@(GameState b' stm c ep clk s) sq
  = avail (S.filter legalPos $ pawnAttackVectors stm sq)
  where
    -- keep the available potential squares
    avail :: Seq Square -> Seq Square
    avail = S.filter (isOppositeColour b' stm)


genEnPassentMoves :: GameState -> Seq GameState
genEnPassentMoves g@(GameState b' stm c ep clk s)
  | isNothing ep = S.empty
  | otherwise    = fromList $ fmap (\x -> makeMove g (Move (fst x) epSq (snd x) Nothing)) candidates
    where
      epSq      = fromJust ep
      attackSqs =  Prelude.filter legalPos $ map (addPos epSq) $ if stm == White
                                                                 then [(1, 1), (-1, 1)]
                                                                 else [(1, -1), (-1, -1)]
      candidates = Prelude.filter (\x -> snd x /= Empty && ptype (snd x) == Pawn)
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
    -- Move the attacking piece
    let b1 = movePiece b' fromSq toSq
    -- Take the enPassent victim
    let b2 = setBoardCell b1 pos Empty
    -- Return a new state
    GameState b2 (opposite stm) c Nothing (clk+1) $ evalState stm b2 
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
