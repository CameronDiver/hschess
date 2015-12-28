import Data.Array

import ChessData
import Board
import Eval
import Moves
import Perft

main :: IO ()
main =
  do
    mapM_ print (perft 5 initialState)
    return ()
