module Chess.Board where

import Chess.Base
import Utils.TypeLevel



data Board (xs :: [((File, Rank), (Color, Piece))])
           (c :: Maybe Color)
  where
    Empty :: Board '[] 'Nothing
    (:::) :: (Position a b, PieceVal o p)
            -> Board ts c
            -> Board ( '( '(a, b), '(o, p)) ': ts) c

infixr 6 :::


(/+/) :: Board xs c -> (Position a b, PieceVal o p)
       -> Board (xs /+/ '( '(a, b), '(o, p))) ('Just o)
(/+/) = undefined

(/-/) :: Board xs c -> Position a b
       -> Board (xs /-/ '(a, b)) c
(/-/) = undefined
