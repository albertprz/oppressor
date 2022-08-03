{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Chess.Move (Move(..), move) where


import Chess.Base
import Chess.Board

import Utils.TypeLevel

import Data.Type.Bool
import Data.Type.Equality


move :: ValidMove o p a b c d xs
        => PieceVal o p
        -> Position a b
        -> Position c d
        -> Board xs
        -> Board (xs /-/ '(a, b) /+/ '( '(c, d), '(o, p)))

move x y z board = board /-/ origin /+/ (target, piece)
  where
    Move { .. } = Move x y z


data Move o p a b c d
  = Move
      { piece  :: PieceVal o p
      , origin :: Position a b
      , target :: Position c d
      }


type ValidMove o p a b c d xs =
  (Position a b /~ Position c d,
   Contains xs '( '(a, b), '(o, p)) ~ 'True,
   MoveGen o p a b c d ~ 'True,
   MoveBlocked o a b c d xs ~ 'False)


type family MoveGen
            (o :: Color)
            (p :: Piece)
            (a :: File)
            (b :: Rank)
            (c :: File)
            (d :: Rank)
            :: Bool
  where

    MoveGen 'White 'Pawn   x 'R2 x 'R4   = 'True
    MoveGen 'Black 'Pawn   x 'R7 x 'R5   = 'True

    MoveGen _ 'Pawn   x a   x b   = a :+: 1 == b
    MoveGen _ 'Pawn   a b   c d   = a :+: 1 == c && AtDistance b d 1

    MoveGen _ 'Knight a b   c d   = AtDistance a c 1 && AtDistance b d 2 ||
                                    AtDistance a c 2 && AtDistance b d 1

    MoveGen _ 'Rook   x _   x _   = 'True
    MoveGen _ 'Rook   _ x   _ x   = 'True

    MoveGen _ 'Bishop a b   c d   = Distance a c == Distance b d

    MoveGen _ 'King   x a   x b   = AtDistance a b 1
    MoveGen _ 'King   a x   b x   = AtDistance a b 1
    MoveGen _ 'King   a b   c d   = Distance a c == 1 && Distance b d == 1

    MoveGen _ 'Queen  x _   x _   = 'True
    MoveGen _ 'Queen  _ x   _ x   = 'True
    MoveGen _ 'Queen  a b   c d   = Distance a c == Distance b d


type family MoveBlocked
            (o :: Color)
            (a :: File)
            (b :: Rank)
            (c :: File)
            (d :: Rank)
            (xs :: k)
            :: Bool
  where
       MoveBlocked _ _ _ _ _ _ = 'False
