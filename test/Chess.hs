module Chess where

import Control.Arrow ((>>>))

import Chess.Base
import Chess.Board
import Chess.Move (move')


-- Invalid states

incorrectPlayer :: Board 'Invalid _ _
incorrectPlayer =
  (move' WPawn A2 A3 >>>
   move' WPawn B2 B3)
  defaultBoard


nonExistantPiece :: Board 'Invalid _ _
nonExistantPiece =
  move' WPawn A4 A5
  defaultBoard


invalidMove :: Board 'Invalid _ _
invalidMove =
  move' WPawn A2 A5
  defaultBoard


blockedMove :: Board 'Invalid _ _
blockedMove =
  move' WRook A1 A3
  defaultBoard


blockedEndSquare :: Board 'Invalid _ _
blockedEndSquare =
  move' WRook A1 A2
  defaultBoard


endSquareWithoutEnemyPiece :: Board 'Invalid _ _
endSquareWithoutEnemyPiece =
  move' WPawn A2 B3
  defaultBoard


-- Valid state

validProgram :: Board 'Valid _ _
validProgram = (move' WPawn   G2 G3 >>>
                move' BKnight B8 C6 >>>
                move' WBishop F1 H3 >>>
                move' BPawn   D7 D5)
               defaultBoard

