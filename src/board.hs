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


boardFromFEN :: String -> Board
boardFromFEN fen = listArray ((0,0), (7,7)) $ concat $ listFromFEN fen

pawnRowForColour :: Colour -> [Piece]
pawnRowForColour c = replicate 8 (Piece Pawn c)

backRowForColour :: Colour -> [Piece]
backRowForColour c = (Piece Rook c) : (Piece Knight c) : (Piece Bishop c) :
                     (Piece Queen c) : (Piece King c) : (Piece Bishop c) :
                     (Piece Knight c) : [(Piece Rook c)]


showBoard :: Board -> String
showBoard b = intercalate "\n"
              [intersperse ' ' $ map showPiece [b ! (x, y) | y <- [0..7]] | x <- [0..7]]

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

setBoardCell :: Board -> (Int, Int) -> Piece -> Board
setBoardCell board pos p = board // [(pos, p)]

pieceAt :: Board -> (Int, Int) -> Piece
pieceAt board pos = board ! pos

positionToTuple :: Position -> (Int, Int)
positionToTuple (a:b:[]) = (fileToN a, rowToN $ read [b])
  where
    fileToN f = (ord f) - (ord 'a')
    rowToN n = n - 1
positionToTuple _ = undefined


tupleToPosition :: (Int, Int) -> Position
tupleToPosition (a, b) = (toEnum (fromEnum 'a' + (7 - b))) : show (a + 1)


movePieceByPos :: Board -> Position -> Position -> Board
movePieceByPos board p1 p2 = movePiece board (positionToTuple p1) (positionToTuple p2)


movePiece :: Board -> (Int, Int) -> (Int, Int) -> Board
movePiece board p1 p2 = do
  let p = pieceAt board p1
  let b = setBoardCell board p1 Empty
  setBoardCell b p2 p

piecesByColour :: Board -> Colour -> [((Int, Int), Piece)]
piecesByColour board col = filter (isCol board col) [((x, y), board ! (x,y)) | x <- [0..7], y <- [0..7]]
  where
    isCol :: Board -> Colour -> ((Int, Int), Piece) -> Bool
    isCol b c (_, Empty) = False
    isCol b c (_, (Piece ptype colour)) = colour == c

