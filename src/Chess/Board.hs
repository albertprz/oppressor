module Chess.Board where

import Chess.Base

import Utils.TypeLevel



data Board xs where
  Empty :: Board '[]
  (:::) :: (Position a b, PieceVal o p)
          -> Board ts
          -> Board ( '( '(a, b), '(o, p)) ': ts)

infixr 6 :::


(/+/) :: Board xs -> (Position a b, PieceVal o p)
       -> Board (xs /+/ '( '(a, b), '(o, p)))
(/+/) = undefined

(/-/) :: Board xs -> Position a b
       -> Board (xs /-/ '(a, b))
(/-/) = undefined
