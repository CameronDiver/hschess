module Board where

import Data.Array
import Data.List (intersperse, intercalate)
import Data.Char (toUpper, ord)
import Debug.Trace (traceShow)

import ChessData
import FEN

initialBoard :: Board
initialBoard = listArray ((0,0), (7,7)) initialBoardList
  where
    initialBoardList =    backRowForColour Black ++
                          pawnRowForColour Black ++
                          replicate 8 Empty ++
                          replicate 8 Empty ++
                          replicate 8 Empty ++
                          replicate 8 Empty ++
                          pawnRowForColour White ++
                          backRowForColour White

pawnRowForColour :: Colour -> [Piece]
pawnRowForColour c = replicate 8 (Piece Pawn c)

backRowForColour :: Colour -> [Piece]
backRowForColour c = (Piece Rook c) : (Piece Knight c) : (Piece Bishop c) :
                     (Piece Queen c) : (Piece King c) : (Piece Bishop c) :
                     (Piece Knight c) : [(Piece Rook c)]


showBoard :: Board -> String
showBoard b = intercalate "\n"
  (map showLine [0..7]) ++ "\n   - - - - - - - -\n   a b c d e f g h"
  where
    showLine :: Int -> String
    showLine i   =  (show (8-i)) ++ "| " ++
      intersperse ' ' (map showPiece (map (getPiece i) [j | j <- [0..7]]))
    getPiece i j = b ! (i, j)


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

setBoardCell :: Board -> Square -> Piece -> Board
setBoardCell board pos p = board // [(pos, p)]

pieceAt :: Board -> Square -> Piece
pieceAt board pos = board ! pos

positionToTuple :: Position -> Square
positionToTuple (a:b:[]) = (fileToN a, rowToN $ read [b])
  where
    fileToN f            = (ord f) - (ord 'a')
    rowToN n             = n - 1
positionToTuple _        = undefined


tupleToPosition :: Square -> Position
tupleToPosition (a, b) = (toEnum (fromEnum 'a' + (7 - b))) : show (a + 1)


movePieceByPos :: Board -> Position -> Position -> Board
movePieceByPos board p1 p2 = movePiece board (positionToTuple p1) (positionToTuple p2)


movePiece :: Board -> Square -> Square -> Board
movePiece board p1 p2 = do
  let p = pieceAt board p1
  let b = setBoardCell board p1 Empty
  setBoardCell b p2 p

piecesByColour :: Board -> Colour -> [(Square, Piece)]
piecesByColour board col = filter (isCol board col) [((x, y), board ! (x,y)) | x <- [0..7], y <- [0..7]]
  where
    isCol :: Board -> Colour -> (Square, Piece) -> Bool
    isCol b c (_, Empty)                = False
    isCol b c (_, (Piece ptype colour)) = colour == c

