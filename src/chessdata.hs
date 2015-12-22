module ChessData where

import Data.Array

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
data Piece = Piece { ptype::PieceType, colour::Colour }
           | Empty
           deriving ( Show, Eq)

-- A lookup type
type Square = (Int, Int)

-- The board types
type Position = String
type Board = Array Square Piece

-- Represent casting rights
data CastlingRight = WhiteKingSide | WhiteQueenSide | BlackKingSide | BlackQueenSide
                   deriving Show

type CastlingRights = [CastlingRight]

-- A structure to hold the current state of the game
data GameState =
  GameState { board :: Board,             -- The position
              sideToMove :: Colour,       -- Which side to move
              castling :: CastlingRights, -- The available castling rights
              enPassant :: Square,        -- Target square of available enPassent
              halfmoveClock :: Int,       -- Increments once per player turn
              fullMoveClock :: Int,       -- Increments once per W&B turn
              score :: Int                -- Result of the evaluation function
            }

