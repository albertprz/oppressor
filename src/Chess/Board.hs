module Chess.Board where

import Chess.Base



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



type family  (xs :: [(k, v)]) /+/ (x :: (k, v)) where
  '[]               /+/ x       = '[x]
  ('(a, _) ': rest) /+/ '(a, b) = '(a, b) ': rest
  (x ': rest)       /+/ '(a, b) = x ': (rest /+/ '(a, b))

type family  (xs :: [(k, v)]) /-/ (x :: k) where
  '[]               /-/ _ = '[]
  ('(a, _) ': rest) /-/ a = rest
  (x ': rest)       /-/ a = x ': (rest /-/ a)



type family Contains (xs :: [t]) (x :: t) :: Bool where
  Contains '[] _         = 'False
  Contains (a ': _) a    = 'True
  Contains (_ ': rest) a = Contains rest a

type family Lookup (xs :: [(k, v)]) (x :: k) :: Maybe v where
  Lookup '[] _            = 'Nothing
  Lookup ('(k, v) ': _) k = 'Just v
  Lookup (_ ': rest) a    = Lookup rest a


type FileRank = (File, Rank)
type ColorPiece = (Color, Piece)


