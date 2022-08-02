module Utils.TypeLevel where

import Chess.Base

import Data.Type.Bool
import Data.Type.Equality
import Data.Type.Ord
import GHC.TypeNats


type a /~ b = (a == b) ~ 'False



class EnumOps (a :: k) where

  type family FromNat (n :: Natural) :: k
  type family ToNat a :: Natural
  type family a :+: (n :: Natural) :: k
  type family a :-: (n :: Natural) :: k
  type family Distance a (b :: k) :: Natural
  type family AtDistance a (b :: k) (n :: Natural) :: Bool

  type instance a :+: n  = FromNat ((ToNat a) + n)
  type instance a :-: n  = FromNat ((ToNat a) - n)
  type instance Distance a b = If ((ToNat a) >=? (ToNat b))
                                   ((ToNat a) - (ToNat b))
                                   ((ToNat b) - (ToNat a))
  type instance AtDistance a b n = a :+: n == b || a :-: n == b



instance EnumOps 'A where
  type instance FromNat 1 = 'A
  type instance ToNat 'A = 1

instance EnumOps 'B where
  type instance FromNat 2 = 'B
  type instance ToNat 'B = 2

instance EnumOps 'C where
  type instance FromNat 3 = 'C
  type instance ToNat 'C = 3

instance EnumOps 'D where
  type instance FromNat 4 = 'D
  type instance ToNat 'D = 4

instance EnumOps 'E where
  type instance FromNat 5 = 'E
  type instance ToNat 'E = 5

instance EnumOps 'F where
  type instance FromNat 6 = 'F
  type instance ToNat 'F = 6

instance EnumOps 'G where
  type instance FromNat 7 = 'G
  type instance ToNat 'G = 7

instance EnumOps 'H where
  type instance FromNat 8 = 'H
  type instance ToNat 'H = 8



instance EnumOps 'R1 where
  type instance FromNat 1 = 'R1
  type instance ToNat 'R1 = 1

instance EnumOps 'R2 where
  type instance FromNat 2 = 'R2
  type instance ToNat 'R2 = 2

instance EnumOps 'R3 where
  type instance FromNat 3 = 'R3
  type instance ToNat 'R3 = 3

instance EnumOps 'R4 where
  type instance FromNat 4 = 'R4
  type instance ToNat 'R4 = 4

instance EnumOps 'R5 where
  type instance FromNat 5 = 'R5
  type instance ToNat 'R5 = 5

instance EnumOps 'R6 where
  type instance FromNat 6 = 'R6
  type instance ToNat 'R6 = 6

instance EnumOps 'R7 where
  type instance FromNat 7 = 'R7
  type instance ToNat 'R7 = 7

instance EnumOps 'R8 where
  type instance FromNat 8 = 'R8
  type instance ToNat 'R8 = 8
