module Chess.Board where

import Chess.Base
import GHC.TypeNats
import Chess.Move

import Utils.TypeLevel

import Data.Type.Bool
import Data.Type.Equality


move :: BoardMove o p a1 b1 a2 b2 v t c xs m
move = MkMove

checkValid :: v ~ 'Valid =>
              Board v t c xs -> ValidBoard t c
checkValid = MkValid

checkInvalid :: v ~ 'Invalid =>
                Board v t c xs -> InValidBoard t c
checkInvalid = MkInvalid



type BoardMove o p a1 b1 a2 b2 v t c xs m =
         PieceVal o p
       -> Position a1 b1
       -> Position a2 b2
       -> Board v t c xs
       -> Board
        (If (ValidMove c o p a1 b1 a2 b2 xs) 'Valid 'Invalid &&| v)
        (If (c == 'Black) (t + 1) t)
        o
        ((xs /-/ '(a1, b1)) /+/ '( '(a2, b2), '(o, p)))



data Move o p a1 b1 a2 b2
  = Move
      { piece  :: PieceVal o p
      , origin :: Position a1 b1
      , target :: Position a2 b2
      }

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
    MkMove :: PieceVal o p
            -> Position a1 b1
            -> Position a2 b2
            -> Board v t c xs
            -> Board
              (If (ValidMove c o p a1 b1 a2 b2 xs) 'Valid 'Invalid &&| v)
              (If (c == 'Black) (t + 1) t)
              o
              ((xs /-/ '(a1, b1)) /+/ '( '(a2, b2), '(o, p)))


instance Show (Board v t c xs) where
  show (MkMove p x1 x2 b) = show b <> "\n -> " <>
                            show p <> " " <> show x1 <> " " <> show x2
  show Empty = "\n"
  show ((x, y) ::: b) = show b <> show (x, y)

instance Show (ValidBoard x y) where
  show (MkValid b) = show b

instance Show (InValidBoard x y) where
  show (MkInvalid b) = show b


infixr 6 :::

data ValidBoard (t :: Turn)
                (c :: Color) where
  MkValid :: v ~ 'Valid => Board v t c xs -> ValidBoard t c


data InValidBoard (t :: Turn)
                  (c :: Color) where
  MkInvalid :: v ~ 'Invalid => Board v t c xs -> InValidBoard t c

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
