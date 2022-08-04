module Chess.Program where

import Control.Arrow ((>>>))

import Chess.Base
import Chess.Board
import Chess.Move  (move)



moves = move WPawn   G2 G3 >>>
        move BKnight B8 C6 >>>
        move WBishop F1 H3 >>>
        move BPawn   D7 D5


result = moves defaultBoard
