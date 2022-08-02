module Chess.Move (Move(piece, origin, target), move) where


import Chess.Base
import Utils.TypeLevel

import Data.Type.Bool
import Data.Type.Equality


move :: ValidMove p a b c d
        => PieceVal p
        -> Position a b
        -> Position c d
        -> Move p a b c d
move p a b = Move p a b


data Move p a b c d
  = ValidMove p a b c d => Move
      { piece  :: PieceVal p
      , origin :: Position a b
      , target :: Position c d
      }


type family MoveGen
            (p :: Piece)
            (a :: File)
            (b :: Rank)
            (c :: File)
            (d :: Rank)
            :: Bool
  where

    MoveGen 'Pawn'   a 'R2 a 'R4 = 'True
    MoveGen 'Pawn'   a b   a c   = b :+: 1 == c

    MoveGen 'Knight' a b   c d   = AtDistance a c 1 && AtDistance b d 2 ||
                                   AtDistance a c 2 && AtDistance b d 1

    MoveGen 'Rook'   a b   a d   = 'True
    MoveGen 'Rook'   a b   c b   = 'True

    MoveGen 'Bishop' a b   c d   = Distance a c == Distance b d

    MoveGen 'King'   a b   a d   = AtDistance b d 1
    MoveGen 'King'   a b   c b   = AtDistance a c 1
    MoveGen 'King'   a b   c d   = Distance a c == 1 && Distance b d == 1

    MoveGen 'Queen'  a b   a d   = 'True
    MoveGen 'Queen'  a b   c b   = 'True
    MoveGen 'Queen'  a b   c d   = Distance a c == Distance b d


type ValidMove a b c d e =
  (Position b c /~ Position d e, MoveGen a b c d e ~ 'True)
