module Chess.Move (Move(..), move, checkValid, checkInvalid) where


import Chess.Base
import Chess.Board

import Utils.TypeLevel

import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeNats


move :: BoardMove o p a1 b1 a2 b2 v t c xs
move = undefined

checkValid :: v ~ 'Valid =>
              Board v t c xs -> ValidBoard t c
checkValid = undefined

checkInvalid :: v ~ 'Invalid =>
                Board v t c xs -> InValidBoard t c
checkInvalid = undefined


type BoardMove o p a1 b1 a2 b2 v t c xs =
         PieceVal o p
       -> Position a1 b1
       -> Position a2 b2
       -> Board v t c xs
       -> Board
        (ValidMove c o p a1 b1 a2 b2 xs &&| v)
        (If (c == 'Black) (t + 1) t)
        o
        ((xs /-/ '(a1, b1)) /+/ '( '(a2, b2), '(o, p)))


data Move o p a1 b1 a2 b2
  = Move
      { piece  :: PieceVal o p
      , origin :: Position a1 b1
      , target :: Position a2 b2
      }


type family ValidMove u o p a1 b1 a2 b2 xs where
  ValidMove u o p a1 b1 a2 b2 xs = If (
                                   CorrectPlayer u o &&
                                   Contains xs '( '(a1, b1), '(o, p)) &&
                                   MoveGen o p a1 b1 a2 b2 &&
                                   EndSquareWithEnemyPiece o p a1 b1 a2 b2 xs &&
                                   Not (MoveBlocked p a1 b1 a2 b2 xs) &&
                                   Not (EndSquareBlocked o a2 b2 xs))
                                   'Valid 'Invalid


type family CorrectPlayer
            (c :: Color)
            (o :: Color)
  where
    CorrectPlayer u o = Not (u == o)


type family MoveGen
            (o :: Color)
            (p :: Piece)
            (a1 :: File)
            (b1 :: Rank)
            (a2 :: File)
            (b2 :: Rank)
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
            (a1 :: File)
            (b1 :: Rank)
            (a2 :: File)
            (b2 :: Rank)
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
            (a2 :: File)
            (b2 :: Rank)
            (xs :: [((File, Rank), (Color, Piece))])
            :: Bool
  where
    EndSquareBlocked o c d xs = MaybeMapFst (Lookup xs '(c, d)) == 'Just o

type family EndSquareWithEnemyPiece
            (o :: Color)
            (p :: Piece)
            (a1 :: File)
            (b1 :: Rank)
            (a2 :: File)
            (b2 :: Rank)
            (xs :: [((File, Rank), (Color, Piece))])
            :: Bool
  where
    EndSquareWithEnemyPiece o 'Pawn a1 b1 a2 b2 xs =
      NonEmpty (Lookup xs '(a2, b2)) &&
      Not (MaybeMapFst (Lookup xs '(a2, b2)) == 'Just o) ||
      a1 == a2
    EndSquareWithEnemyPiece _ _ _ _ _ _ _ = 'True
