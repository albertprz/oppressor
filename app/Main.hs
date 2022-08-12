module Main where

import Control.Arrow ((>>>))

import Chess.Base
import Chess.Board


opening = move WPawn   E2 E4 >>>
          move BPawn   C7 C5 >>>
          move WKnight G1 F3 >>>
          move BPawn   D7 D6 >>>
          move WPawn   D2 D4 >>>
          move BPawn   C5 D4 >>>
          move WKnight F3 D4 >>>
          move BKnight G8 F6 >>>
          move WKnight B1 C3 >>>
          move BPawn   A7 A6



result :: ValidBoard 5 'Black
result = checkValid $ opening defaultBoard


main = putStrLn $ show result
