module Board where

import Data.Array
import Data.List ( intersperse, intercalate)
import Data.Char

import ChessData

type Board = [[Piece]]
type Row = [Piece]

showBoard :: Board -> String
showBoard board = (intercalate "\n" $
                   map drawRow $ zip board (reverse [0..7]))
                  ++ "\n  a b c d e f g h"

drawRow :: (Row, Int) -> String
drawRow (row, n) =
  (show $ rowToNum n) ++ " " ++ intersperse ' ' [showPiece x | x <- row]

rowToNum :: Int -> Int
rowToNum n = 1 + n


square :: Board -> (Int, Int) -> Piece
square (b:bs) (x, y) =
  if y > 0 then square bs (x, y-1)
  else if x > 0 then square bs (x-1, y)
       else undefined -- TODO

-- Make a character from a piece, taking into account colour
showPiece :: Piece -> Char
showPiece (Piece ptype col)
  | col == White = toUpper $ showPiece' ptype
  | otherwise = showPiece' ptype
showPiece Empty = '.'

showPiece' :: PieceType -> Char
showPiece' ptype =
  case ptype of
  Pawn -> 'p'
  Knight -> 'n'
  Bishop -> 'b'
  Rook -> 'r'
  Queen -> 'q'
  King -> 'k'

makeBoardFEN :: String -> Board
makeBoardFEN _ = undefined

makeBoard :: Board
makeBoard =
  reverse
  [backRowForColour White,
   pawnRowForColour White,
   replicate 8 Empty,
   replicate 8 Empty,
   replicate 8 Empty,
   replicate 8 Empty,
   pawnRowForColour Black,
   backRowForColour Black]


pawnRowForColour :: Colour -> [Piece]
pawnRowForColour c =
  replicate 8 (Piece Pawn c)

backRowForColour :: Colour -> [Piece]
backRowForColour c =
  (Piece Rook c) : (Piece Knight c) : (Piece Bishop c) :
   (Piece Queen c) : (Piece King c) : (Piece Bishop c) :
  (Piece Knight c) : [(Piece Rook c)]

