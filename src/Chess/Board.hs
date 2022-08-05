module Chess.Board where

import Chess.Base
import GHC.Natural (Natural)


data ValidState = Valid | Invalid


type family (x :: ValidState) &&| (y :: ValidState) where
  'Valid &&| y = y
  x &&| 'Valid = x


data Board  (v :: ValidState)
            (t :: Turn)
            (c :: Color)
            (xs :: [((File, Rank), (Color, Piece))])
  where
    Empty :: Board 'Valid 0 'Black '[]
    (:::) :: (Position a b, PieceVal o p)
            -> Board v t c xs
            -> Board v t c ( '( '(a, b), '(o, p)) ': xs)
infixr 6 :::

data ValidBoard (t :: Turn)
                (c :: Color)

data InValidBoard (t :: Turn)
                  (c :: Color)

type Turn = Natural


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
