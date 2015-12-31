import Data.Array

import ChessData
import Board
import Eval
import Moves
import Perft
import Search

main :: IO ()
main =
  do
    let state = stateFromFEN "3r1k2/8/2p5/8/3N4/8/8/4K3 w - - 0 1"
    print state
    print $ searchToDepth 1 state
    print $ searchToDepth 2 state
    print $ searchToDepth 3 state
    print $ searchToDepth 4 state
    --mapM_ print (perft 5 initialState)
    return ()


