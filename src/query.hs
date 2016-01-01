module Query where

import Data.Maybe
import Data.Foldable
import qualified Data.Sequence as S

import ChessData
import Board
import Vectors

-- Check if a specific square is being attacked at depth 1 by the given colour
isSqAttacked :: GameState -> Square -> Colour -> Bool
isSqAttacked g sq c = sliders || pawns || nsliders
  where
    pieces = piecesByColour (board g) c
    sliders = or $ sliderPieceAttacking' g (excludeNonSliders pieces) sq
    pawns = pawnAttacking pieces c sq
    nsliders = or $ nonSliderPieceAttacking' sq $ excludeSliders (excludePiece Pawn pieces)

-- Does the current game state have the king sq attacked?
isInCheck :: GameState -> Bool
isInCheck g@(GameState b stm _ _ _ _ _) = isSqAttacked g sq stm
  where
    (Just (sq,_)) = find ((== King).ptype.snd) (piecesByColour b (opposite stm))

-- Get the attack vectors for a pawn, and see if any pawn attacks the current sq
pawnAttacking :: [(Square, Piece)] -> Colour -> Square -> Bool
pawnAttacking pieces col sq = isJust p1 || isJust p2
  where
    pawns         = filterPieces Pawn pieces
    (sq1:sq2:_)   = toList $ pawnAttackVectors (opposite col) sq
    p1            = lookup sq1 pawns
    p2            = lookup sq2 pawns
    negateY (x,y) = (x, -y)

nonSliderPieceAttacking' :: Square -> [(Square, Piece)] -> [Bool]
nonSliderPieceAttacking' sq pcs =
  map (\(_,Piece t c) -> nonSliderPieceAttacking t pcs sq) pcs

nonSliderPieceAttacking :: PieceType -> [(Square, Piece)] -> Square -> Bool
nonSliderPieceAttacking ptype' pieces sq = any isJust candidates
  where
    ps = filterPieces ptype' pieces
    vs = map (addPos sq) (toList $ moveVectors (Piece ptype' White))
    candidates = map (`lookup` ps) vs

sliderPieceAttacking' :: GameState -> [(Square,Piece)] -> Square -> [Bool]
sliderPieceAttacking' g pcs sq =
  map (\(_,p) -> sliderPieceAttacking g p pcs sq) pcs

sliderPieceAttacking :: GameState -> Piece -> [(Square, Piece)] -> Square -> Bool
sliderPieceAttacking (GameState b _ _ _ _ _ _) p@(Piece ptype col) pieces sq =
  any isJust candidates
  where
    ps         = filterPieces ptype pieces
    vs         = concatMap (toList.slide b sq (opposite col)) $ moveVectors p
    candidates = map (`lookup` ps) vs

filterPieces :: PieceType -> [(a, Piece)] -> [(a, Piece)]
filterPieces ptype' = filter ((== ptype').ptype.snd)

excludePiece :: PieceType -> [(a, Piece)] -> [(a, Piece)]
excludePiece ptype' = filter ((/= ptype').ptype.snd)

excludeNonSliders :: [(a, Piece)] -> [(a, Piece)]
excludeNonSliders = filter (sliderPiece.ptype.snd)

excludeSliders :: [(a, Piece)] -> [(a, Piece)]
excludeSliders = filter (not.sliderPiece.ptype.snd)
