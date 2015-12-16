module ChessData where

-- The piece types
data PieceType = Pawn
               | Knight
               | Bishop
               | Rook
               | Queen
               | King
               deriving ( Show, Eq )

-- Fundamental colours
data Colour = White
            | Black
            deriving ( Show, Eq )

-- Combination of piece and it's colour
data Piece = Piece PieceType Colour
           | Empty
           deriving ( Show, Eq)

-- A lookup type
type Square = (Int, Int)

