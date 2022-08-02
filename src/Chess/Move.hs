module Chess.Move (Move(piece, origin, target), move) where


import Chess.Base
import Utils.TypeLevel

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
    MoveGen 'Pawn' a 'R2 a 'R4 = 'True
    MoveGen 'Pawn' a b   a c   = b :+: 1 == c
    MoveGen _ _ _ _ _          = 'False


type ValidMove a b c d e =
  (Position b c /~ Position d e, MoveGen a b c d e ~ 'True)
