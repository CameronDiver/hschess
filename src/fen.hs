module FEN where

import Data.Char
import qualified Data.Attoparsec.Text as P
import qualified Data.Text as T
import Data.Array
import Data.Word
import Debug.Trace (traceShow)

import ChessData
import Eval

gameFromFEN :: P.Parser GameState
gameFromFEN =
  do
    pos       <- P.takeWhile notSpace -- Get the fen position string
    let board = boardFromPieceList $ reverse $ map reverse $ listFromFEN' (T.unpack pos) [[]]
    P.skipSpace
    stm       <- P.takeWhile notSpace -- Get the side to move
    P.skipSpace
    castling  <- P.takeWhile notSpace -- Get the castling rights
    P.skipSpace
    enpassent <- P.takeWhile notSpace -- Get the enpassent square
    P.skipSpace
    halfClock <- P.decimal              -- half move clock
    return $ GameState {
      board = board,
      sideToMove = (stmToCol $ T.unpack stm),
      castling = (strToRights $ T.unpack castling),
      enPassant = (strToEnPassent $ T.unpack enpassent),
      halfMoveClock = halfClock,
      score = evalBoard board
      }

notSpace :: Char -> Bool
notSpace c = c /=  ' '

-- Given a side to move char, return the correct colour symbol
stmToCol :: String -> Colour
stmToCol "w" = White
stmToCol "b" = Black
stmToCol "W" = White
stmToCol "B" = Black
stmToCol a   = traceShow a $ undefined

-- Given a string of castling rights, return the list of symbolic rights
strToRights :: String -> CastlingRights
strToRights []     = []
strToRights "-"    = []
strToRights (c:cs) = charToRight c : strToRights cs
  where
    charToRight :: Char -> CastlingRight
    charToRight 'k' = BlackKingSide
    charToRight 'q' = BlackQueenSide
    charToRight 'K' = WhiteKingSide
    charToRight 'Q' = WhiteQueenSide

strToEnPassent :: String -> Maybe Square
strToEnPassent "-" = Nothing
strToEnPassent pos = Just $ positionToSquare pos

-- Given the initial part of a FEN string, return the board position
--Note:  needs to be reversed
listFromFEN' :: String -> [[Piece]] -> [[Piece]]
listFromFEN' [] current            = current -- handle all input domain
listFromFEN' (f:en) current@(r:rs) =
  case f of
    -- space is end of FEN placement string
    ' '                    -> []
    '/'                    -> next $ [] : current
    '8'                    -> next $ (replicate 8 Empty):rs
    i | i > '0' && i < '8' -> next $ ((replicate (repeat :: Int) Empty) ++ r) : rs
    _                      -> this $ pieceFromChar f
  where
    next       = listFromFEN' en
    this piece = next $ ((piece):r):rs
    repeat     = read [f] :: Int


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

boardFromPieceList :: [[Piece]] -> Board
boardFromPieceList l = Board (listArray ((0,0), (7,7)) $ concat l)


positionToSquare :: Position -> Square
positionToSquare (a:b:[]) = (fileToN a, rowToN $ read [b])
  where
    fileToN f            = (ord f) - (ord 'a')
    rowToN n             = n - 1
positionToSquare _        = undefined
