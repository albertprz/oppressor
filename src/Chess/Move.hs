{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Chess.Move (Move(..), move) where


import Chess.Base
import Chess.Board

import Utils.TypeLevel

import Data.Type.Bool
import Data.Type.Equality


move :: ValidMove u o p a b c d xs
        => PieceVal o p
        -> Position a b
        -> Position c d
        -> Board xs u
        -> Board (xs /-/ '(a, b) /+/ '( '(c, d), '(o, p))) ('Just o)

move k y z board = board /-/ origin /+/ (target, piece)
  where
    Move { .. } = Move k y z


data Move o p a b c d
  = Move
      { piece  :: PieceVal o p
      , origin :: Position a b
      , target :: Position c d
      }


type ValidMove u o p a b c d xs =
  (CorrectPlayer u o                      ~ 'True,
   Contains xs '( '(a, b), '(o, p))       ~ 'True,
   MoveGen o p a b c d                    ~ 'True,
   EndSquareWithEnemyPiece o p a b c d xs ~ 'True,
   MoveBlocked p a b c d xs               ~ 'False,
   EndSquareBlocked o c d xs              ~ 'False)


type family CorrectPlayer
            (u :: Maybe Color)
            (o :: Color)
  where
    CorrectPlayer u o = Not (u == 'Just o)


type family MoveGen
            (o :: Color)
            (p :: Piece)
            (a :: File)
            (b :: Rank)
            (c :: File)
            (d :: Rank)
            :: Bool
  where

    MoveGen 'White 'Pawn   k 'R2 k 'R4   = 'True
    MoveGen 'Black 'Pawn   k 'R7 k 'R5   = 'True

    MoveGen 'White 'Pawn   k a   k b     = a :+: 1 == b
    MoveGen 'White 'Pawn   a b   c d     = b :+: 1 == d && Distance a c == 1
    MoveGen 'Black 'Pawn   k a   k b     = a :-: 1 == b
    MoveGen 'Black 'Pawn   a b   c d     = b :-: 1 == d && Distance a c == 1

    MoveGen _ 'Knight a b   c d   = Distance a c == 1 && Distance b d == 2 ||
                                    Distance a c == 2 && Distance b d == 1

    MoveGen _ 'Rook   k _   k _   = 'True
    MoveGen _ 'Rook   _ k   _ k   = 'True

    MoveGen _ 'Bishop a b   c d   = Distance a c == Distance b d

    MoveGen _ 'King   k a   k b   = Distance a b == 1
    MoveGen _ 'King   a k   b k   = Distance a b == 1
    MoveGen _ 'King   a b   c d   = Distance a c == 1 && Distance b d == 1

    MoveGen _ 'Queen  k _   k _   = 'True
    MoveGen _ 'Queen  _ k   _ k   = 'True
    MoveGen _ 'Queen  a b   c d   = Distance a c == Distance b d


type family MoveBlocked
            (p :: Piece)
            (a :: File)
            (b :: Rank)
            (c :: File)
            (d :: Rank)
            (xs :: [((File, Rank), (Color, Piece))])
            :: Bool
  where
    MoveBlocked 'Knight _ _ _ _ _ = 'False
    MoveBlocked _ a k b k xs = NonEmpty (Lookups xs (ZipConst  (Between a b) k))
    MoveBlocked _ k a k b xs = NonEmpty (Lookups xs (ZipConst' (Between a b) k))
    MoveBlocked _ a b c d xs = NonEmpty (Lookups xs (Zip       (Between a c)
                                                               (Between b d)))


type family EndSquareBlocked
            (o :: Color)
            (c :: File)
            (d :: Rank)
            (xs :: [((File, Rank), (Color, Piece))])
            :: Bool
  where
    EndSquareBlocked o c d xs = MaybeMapFst (Lookup xs '(c, d)) == 'Just o

type family EndSquareWithEnemyPiece
            (o :: Color)
            (p :: Piece)
            (a :: File)
            (b :: Rank)
            (c :: File)
            (d :: Rank)
            (xs :: [((File, Rank), (Color, Piece))])
            :: Bool
  where
    EndSquareWithEnemyPiece o 'Pawn a b c d xs =
      NonEmpty (Lookup xs '(c, d)) &&
      Not (MaybeMapFst (Lookup xs '(c, d)) == 'Just o) ||
      a == c
    EndSquareWithEnemyPiece _ _ _ _ _ _ _ = 'True
