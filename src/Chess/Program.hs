{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Chess.Program where

import Control.Arrow ((>>>))

import Chess.Base
import Chess.Board
import Chess.Move


moves = move WPawn   G2 G3 >>>
        move BKnight B8 C6 >>>
        move WBishop F1 H3 >>>
        move BPawn   D7 D5


result = moves defaultBoard


defaultBoard =
  (A1, WRook) ::: (B1, WKnight) ::: (C1, WBishop) ::: (D1, WQueen) :::
  (E1, WKing) ::: (F1, WBishop) ::: (G1, WKnight) ::: (H1, WRook)  :::
  (A2, WPawn) ::: (B2, WPawn)   ::: (C2, WPawn)   ::: (D2, WPawn)  :::
  (E2, WPawn) ::: (F2, WPawn)   ::: (G2, WPawn)   ::: (H2, WPawn)  :::

  (A7, BPawn) ::: (B7, BPawn)   ::: (C7, BPawn)   ::: (D7, BPawn)  :::
  (E7, BPawn) ::: (F7, BPawn)   ::: (G7, BPawn)   ::: (H7, BPawn)  :::
  (A8, BRook) ::: (B8, BKnight) ::: (C8, BBishop) ::: (D8, BQueen) :::
  (E8, BKing) ::: (F8, BBishop) ::: (G8, BKnight) ::: (H8, BRook)  :::
  Empty
