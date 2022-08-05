module Chess where

import Control.Arrow ((>>>))

import Chess.Base
import Chess.Board
import Chess.Move  (checkInvalid, checkValid, move)


-- Invalid states

incorrectPlayer = checkInvalid $
  (move WPawn A2 A3 >>>
   move WPawn B2 B3)
  defaultBoard


nonExistantPiece = checkInvalid $
  move WPawn A4 A5
  defaultBoard


invalidMove = checkInvalid $
  move WPawn A2 A5
  defaultBoard


blockedMove = checkInvalid $
  move WRook A1 A3
  defaultBoard


blockedEndSquare = checkInvalid $
  move WRook A1 A2
  defaultBoard


endSquareWithoutEnemyPiece = checkInvalid $
  move WPawn A2 B3
  defaultBoard



-- Valid state

validProgram = checkValid $
               (move WPawn   G2 G3 >>>
                move BKnight B8 C6 >>>
                move WBishop F1 H3 >>>
                move BPawn   D7 D5)
               defaultBoard
