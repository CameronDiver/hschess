module ChessData where

import Data.Array
import Data.Maybe
import Data.List (intersperse, intercalate)
import Data.Char (toUpper, ord, chr)


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

opposite :: Colour -> Colour
opposite White = Black
opposite Black = White

-- Combination of piece and it's colour
data Piece = Piece { ptype::PieceType, colour::Colour }
           | Empty
           deriving ( Show, Eq)

-- A lookup type
type Square = (Int, Int)

-- The board types
type Position = String
newtype Board = Board ( Array Square Piece )

-- A structure to hold information about a move
data Move = Move { fromSq::Square,        -- The square the piece is moving from
                   toSq::Square,          -- The destination sq
                   piece::Piece,          -- The piece moving
                   promotion::Maybe Piece -- Is this a pawn promotion?
                                          -- and if so what is the toPiece
                 }

instance Show Move where
  show (Move fsq tsq _ _) = squareToPosition fsq ++ squareToPosition tsq

-- Board printing
-- Print board
instance Show Board where
  show (Board b) = showBoard $ Board b

-- Pretty print a board
showBoard :: Board -> String
showBoard (Board b) = "\n" ++ intercalate "\n"
  (map showLine [0..7]) ++ "\n   - - - - - - - -\n   a b c d e f g h\n"
  where
    showLine :: Int -> String
    showLine i   =  (show (8-i)) ++ "| " ++
      intersperse ' ' (map showPiece (map (getPiece i) [j | j <- [0..7]]))
    getPiece i j = b ! (j, i)


-- Make a character from a piece, taking into account colour
showPiece :: Piece -> Char
showPiece (Piece ptype col)
  | col == White  = toUpper $ showPiece' ptype
  | otherwise     = showPiece' ptype
showPiece Empty   = '.'

showPiece' :: PieceType -> Char
showPiece' ptype =
  case ptype of
    Pawn   -> 'p'
    Knight -> 'n'
    Bishop -> 'b'
    Rook   -> 'r'
    Queen  -> 'q'
    King   -> 'k'


-- Represent casting rights
data CastlingRight = WhiteKingSide | WhiteQueenSide | BlackKingSide | BlackQueenSide
                   deriving Show

type CastlingRights = [CastlingRight]

-- A structure to hold the current state of the game
data GameState =
  GameState { board :: Board,             -- The position
              sideToMove :: Colour,       -- Which side to move
              castling :: CastlingRights, -- The available castling rights
              enPassant :: Maybe Square,  -- Target square of available enPassent
              halfMoveClock :: Int,       -- Increments once per player turn
              score :: Int,               -- Result of the evaluation function
              move :: Maybe Move          -- The move which led to this position
            }

instance Show GameState where
  show (GameState b stm c ep clk s mv) =
    show b ++ "\nSide to Move: " ++ show stm
    ++ "\nCastling Rights: " ++ show c
    ++ "\nEn passent info: " ++ show ep
    ++ "\nHalf-Move Count: " ++ show clk
    ++ "\nBoard eval: " ++ show s
    ++ "\nLast move: " ++ move
    where
      move | isJust mv = show $ fromJust mv
           | otherwise = "-"

positionToSquare :: Position -> Square
positionToSquare (a:b:[]) = (fileToN a, rowToN $ read [b])
  where
    fileToN f            = (ord f) - (ord 'a')
    rowToN n             = 7 - (n - 1)
positionToSquare _        = undefined

squareToPosition :: Square -> Position
squareToPosition (x,y) = chr ((ord 'a') + x) : show (8-y)
