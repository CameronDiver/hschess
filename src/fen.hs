module FEN where

import Data.Char
import Debug.Trace (traceShow)

import ChessData

-- TODO: Use attoparsec!

listFromFEN :: String -> [[Piece]]
listFromFEN fen = reverse $ listFromFEN' fen [[]]

listFromFEN' :: String -> [[Piece]] -> [[Piece]]
listFromFEN' [] current = current -- handle all input domain
listFromFEN' (f:en) current@(r:rs) =
  case f of
    -- space is end of FEN placement string
    ' ' -> []
    '/' -> next $ [] : current
    '8' -> next $ (replicate 8 Empty):rs
    i | i > '0' && i < '8' -> next $ ((replicate (repeat :: Int) Empty) ++ r) : rs
    _ -> this $ pieceFromChar f
  where
    next = listFromFEN' en
    this piece = next $ ((piece):r):rs
    repeat = read [f] :: Int


-- Helper function which is essentially just a partial part of the switch above
-- TODO: seperate the repeated code into a single function
pieceFromChar :: Char -> Piece
pieceFromChar c =
  case c of
    -- black piece chars
    'p' -> (Piece Pawn Black)
    'n' -> (Piece Knight Black)
    'r' -> (Piece Rook Black)
    'b' -> (Piece Bishop Black)
    'q' -> (Piece Queen Black)
    'k' -> (Piece King Black)
    -- white pieces
    'P' -> (Piece Pawn White)
    'N' -> (Piece Knight White)
    'R' -> (Piece Rook White)
    'B' -> (Piece Bishop White)
    'Q' -> (Piece Queen White)
    'K' -> (Piece King White)
    _ -> undefined

