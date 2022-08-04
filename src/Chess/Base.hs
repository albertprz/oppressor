module Chess.Base where

import Data.Kind (Type)


data Piece
  = Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
  deriving (Eq, Show)

data Color
  = White
  | Black
  deriving (Eq, Show)

data File
  = A
  | B
  | C
  | D
  | E
  | F
  | G
  | H
  deriving (Enum, Eq, Ord, Show)

data Rank
  = R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  deriving (Enum, Eq, Ord, Show)



data PieceVal :: Color -> Piece -> Type where
  WPawn :: PieceVal 'White 'Pawn
  WKnight :: PieceVal 'White 'Knight
  WBishop :: PieceVal 'White 'Bishop
  WRook :: PieceVal 'White 'Rook
  WQueen :: PieceVal 'White 'Queen
  WKing :: PieceVal 'White 'King
  BPawn :: PieceVal 'Black 'Pawn
  BKnight :: PieceVal 'Black 'Knight
  BBishop :: PieceVal 'Black 'Bishop
  BRook :: PieceVal 'Black 'Rook
  BQueen :: PieceVal 'Black 'Queen
  BKing :: PieceVal 'Black 'King


data Position :: File -> Rank -> Type where
  A1 :: Position 'A 'R1
  A2 :: Position 'A 'R2
  A3 :: Position 'A 'R3
  A4 :: Position 'A 'R4
  A5 :: Position 'A 'R5
  A6 :: Position 'A 'R6
  A7 :: Position 'A 'R7
  A8 :: Position 'A 'R8
  B1 :: Position 'B 'R1
  B2 :: Position 'B 'R2
  B3 :: Position 'B 'R3
  B4 :: Position 'B 'R4
  B5 :: Position 'B 'R5
  B6 :: Position 'B 'R6
  B7 :: Position 'B 'R7
  B8 :: Position 'B 'R8
  C1 :: Position 'C 'R1
  C2 :: Position 'C 'R2
  C3 :: Position 'C 'R3
  C4 :: Position 'C 'R4
  C5 :: Position 'C 'R5
  C6 :: Position 'C 'R6
  C7 :: Position 'C 'R7
  C8 :: Position 'C 'R8
  D1 :: Position 'D 'R1
  D2 :: Position 'D 'R2
  D3 :: Position 'D 'R3
  D4 :: Position 'D 'R4
  D5 :: Position 'D 'R5
  D6 :: Position 'D 'R6
  D7 :: Position 'D 'R7
  D8 :: Position 'D 'R8
  E1 :: Position 'E 'R1
  E2 :: Position 'E 'R2
  E3 :: Position 'E 'R3
  E4 :: Position 'E 'R4
  E5 :: Position 'E 'R5
  E6 :: Position 'E 'R6
  E7 :: Position 'E 'R7
  E8 :: Position 'E 'R8
  F1 :: Position 'F 'R1
  F2 :: Position 'F 'R2
  F3 :: Position 'F 'R3
  F4 :: Position 'F 'R4
  F5 :: Position 'F 'R5
  F6 :: Position 'F 'R6
  F7 :: Position 'F 'R7
  F8 :: Position 'F 'R8
  G1 :: Position 'G 'R1
  G2 :: Position 'G 'R2
  G3 :: Position 'G 'R3
  G4 :: Position 'G 'R4
  G5 :: Position 'G 'R5
  G6 :: Position 'G 'R6
  G7 :: Position 'G 'R7
  G8 :: Position 'G 'R8
  H1 :: Position 'H 'R1
  H2 :: Position 'H 'R2
  H3 :: Position 'H 'R3
  H4 :: Position 'H 'R4
  H5 :: Position 'H 'R5
  H6 :: Position 'H 'R6
  H7 :: Position 'H 'R7
  H8 :: Position 'H 'R8
