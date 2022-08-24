module Utils.TypeLevel where

import Chess.Base

import Data.Kind      (Constraint)
import Data.Type.Bool
import Data.Type.Ord
import GHC.TypeNats


type family (xs :: [a]) |+| (y :: a) :: [a] where
  '[] |+| y = '[y]
  (x ': xs) |+| y = x ': (xs |+| y)

type family  (xs :: [(k, v)]) /+/ (x :: (k, v)) :: [(k, v)] where
  '[]               /+/ x       = '[x]
  ('(a, _) ': rest) /+/ '(a, b) = '(a, b) ': rest
  (x ': rest)       /+/ '(a, b) = x ': (rest /+/ '(a, b))

type family  (xs :: [(k, v)]) /-/ (x :: k) :: [(k, v)] where
  '[]               /-/ _ = '[]
  ('(a, _) ': rest) /-/ a = rest
  (x ': rest)       /-/ a = x ': (rest /-/ a)


type family Contains (xs :: [t]) (x :: t) :: Bool where
  Contains '[] _         = 'False
  Contains (a ': _) a    = 'True
  Contains (_ ': rest) a = Contains rest a

type family ContainsMany (xs :: [t]) (ys :: [t]) :: Bool where
  ContainsMany _ '[]        = 'True
  ContainsMany xs (y ': ys) = Contains xs y && ContainsMany xs ys

type family Lookup (xs :: [(k, v)]) (y :: k) :: Maybe v where
  Lookup '[] _            = 'Nothing
  Lookup ('(k, v) ': _) k = 'Just v
  Lookup (_ ': rest) a    = Lookup rest a


type family Lookups (xs :: [(k, v)]) (ys :: [k]) :: [v] where
  Lookups _ '[]        = '[]
  Lookups xs (k ': ks) = MaybeToList (Lookup xs k) ++ Lookups xs ks


type family MaybeToList (x :: Maybe r) :: [r] where
  MaybeToList 'Nothing  = '[]
  MaybeToList ('Just x) = '[x]

type family Must (x :: Bool) :: Constraint where
  Must x = x ~ 'True

type family MustNot (x :: Bool) :: Constraint where
  MustNot x = x ~ 'False

type family  (x :: [r]) ++ (y :: [r]) :: [r] where
  '[]       ++ ys  = ys
  (x ': xs) ++ ys  = x ': (xs ++ ys)


type family   Empty (xs :: m a) :: Bool
type instance Empty 'Nothing  = 'True
type instance Empty ('Just x) = 'False
type instance Empty '[]       = 'True
type instance Empty (x ': _)  = 'False

type family NonEmpty (x :: m a) :: Bool where
  NonEmpty x = Not (Empty x)


type family MaybeMapFst (x :: Maybe (a, k)) :: Maybe a where
  MaybeMapFst 'Nothing        = 'Nothing
  MaybeMapFst ('Just '(x, y)) = 'Just x

type family MapFromNat (xs :: [Natural]) where
  MapFromNat '[] = '[]
  MapFromNat (x ': xs) = FromNat x ': MapFromNat xs



type family Zip (xs :: [k]) (ys :: [r]) :: [(k, r)] where
  Zip _ '[] = '[]
  Zip '[] _ = '[]
  Zip (a ': as) (b ': bs) = '(a, b) ': Zip as bs

type family ZipConst (xs :: [k]) (y :: r) :: [(k, r)] where
  ZipConst '[] _ = '[]
  ZipConst (a ': as) b = '(a, b) ': ZipConst as b

type family ZipConst' (xs :: [k]) (y :: r) :: [(r, k)] where
  ZipConst' '[] _ = '[]
  ZipConst' (a ': as) b = '(b, a) ': ZipConst' as b

type family Range (n1 :: Natural) (n2 :: Natural) :: [Natural] where
  Range 0 _ = '[]
  Range _ 0 = '[]
  Range n1 n2 = If ((n1 >? n2 + 1))
                   (n1 - 1 ': Range (n1 - 1) n2)
                   (If (n2 >? n1 + 1)
                       (n2 - 1 ': Range n1 (n2 - 1))
                     '[])




class EnumOps (a :: k) where

  type family FromNat (n :: Natural) :: k
  type family ToNat a :: Natural
  type family a :+: (n :: Natural) :: k
  type family a :-: (n :: Natural) :: k
  type family Distance a (b :: k) :: Natural
  type family Between a (b :: k) :: [k]

  type instance a :+: n  = FromNat ((ToNat a) + n)
  type instance a :-: n  = FromNat ((ToNat a) - n)

  type instance Distance a b = If ((ToNat a) >=? (ToNat b))
                                   ((ToNat a) - (ToNat b))
                                   ((ToNat b) - (ToNat a))

  type instance Between a b = MapFromNat (Range (ToNat a) (ToNat b))



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
