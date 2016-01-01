module Board where

import Data.Array
import Data.Char (toUpper)
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T

import ChessData
import FEN

initialBoard :: Board
initialBoard = Board $ listArray ((0,0), (7,7)) initialBoardList
  where
    initialBoardList =    backRowForColour Black ++
                          pawnRowForColour Black ++
                          replicate 8 Empty ++
                          replicate 8 Empty ++
                          replicate 8 Empty ++
                          replicate 8 Empty ++
                          pawnRowForColour White ++
                          backRowForColour White

initialState :: GameState
initialState = state
  where
    (Right state) = P.parseOnly gameFromFEN (T.pack "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")

stateFromFEN :: String -> GameState
stateFromFEN fen = state
  where
    (Right state) = P.parseOnly gameFromFEN $ T.pack fen

pawnRowForColour :: Colour -> [Piece]
pawnRowForColour c = replicate 8 (Piece Pawn c)

backRowForColour :: Colour -> [Piece]
backRowForColour c = (Piece Rook c) : (Piece Knight c) : (Piece Bishop c) :
                     (Piece Queen c) : (Piece King c) : (Piece Bishop c) :
                     (Piece Knight c) : [(Piece Rook c)]

setBoardCell :: Board -> Square -> Piece -> Board
setBoardCell (Board board) pos p = Board $ board // [(pos, p)]

pieceAt :: Board -> Square -> Piece
pieceAt (Board board) pos = board ! pos

tupleToPosition :: Square -> Position
tupleToPosition (a, b) = (toEnum (fromEnum 'a' + (7 - b))) : show (a + 1)


movePieceByPos :: Board -> Position -> Position -> Board
movePieceByPos board p1 p2 = movePiece board (positionToSquare p1) (positionToSquare p2)


movePiece :: Board -> Square -> Square -> Board
movePiece board p1 p2 = do
  let p = pieceAt board p1
  let b = setBoardCell board p1 Empty
  setBoardCell b p2 p

piecesByColour :: Board -> Colour -> [(Square, Piece)]
piecesByColour (Board board) col = filter (isCol col) [((x, y), board ! (x,y)) | x <- [0..7], y <- [0..7]]
  where
    isCol :: Colour -> (Square, Piece) -> Bool
    isCol c (_, Empty)                = False
    isCol c (_, (Piece ptype colour)) = colour == c


