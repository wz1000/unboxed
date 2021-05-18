{-# Language MagicHash #-}
{-# Language UnboxedSums #-}
{-# Language UnboxedTuples #-}
{-# Language TypeFamilies #-}
{-# Language PolyKinds #-}
{-# Language BangPatterns #-}
{-# Language DataKinds #-}
{-# Language PatternSynonyms #-}
{-# Language RankNTypes #-}
{-# Language NoImplicitPrelude #-}
{-# Language TypeApplications #-}
{-# Language RebindableSyntax #-}
{-# Language ImportQualifiedPost #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language StandaloneKindSignatures #-}

module Def.Unlifted where

import Unboxed.Internal.Class
import Unboxed.Internal.List
import Unboxed.Internal.Maybe
import Unboxed.Internal.Rebind
import GHC.Types
import Prelude (otherwise, not, (++), ShowS, (.), showString, showParen,($), (&&), (||))
import Prelude qualified
import System.IO qualified as IO

instance EqRep UnliftedRep where
  eqDef x y = not (x /= y)
  neDef x y = not (x == y)

instance OrdRep UnliftedRep where
  compareDef x y
    | x == y = EQ
    | x <= y = LT
    | otherwise = GT

  ltDef x y = case compare x y of LT -> True;  _ -> False
  leDef x y = case compare x y of GT -> False; _ -> True
  gtDef x y = case compare x y of GT -> True;  _ -> False
  geDef x y = case compare x y of LT -> False; _ -> True

  maxDef x y
    | x <= y = y
    | otherwise = x

  minDef x y
    | x <= y = x
    | otherwise = y

instance NumRep UnliftedRep where 
  negateDef a = 0 - a
  minusDef a b = a + negate b

instance FractionalRep UnliftedRep where
  fractionalDef x y = x * recip y
  recipDef x = 1 / x

instance RealRep UnliftedRep where
  realToFracDef x = fromRational (toRational x)

instance EnumRep UnliftedRep where
{-
  enumFromDef x             = map toEnum [fromEnum x ..]
  enumFromThenDef x y       = map toEnum [fromEnum x, fromEnum y ..]
  enumFromToDef x y         = map toEnum [fromEnum x .. fromEnum y]
  enumFromThenToDef x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]
-}
  succDef x = toEnum (fromEnum x + 1)
  predDef x = toEnum (fromEnum x - 1)

instance IntegralRep UnliftedRep where
  n `quotDef` d =  q  where !(# q, _ #) = quotRem n d
  n `remDef` d  =  r  where !(# _, r #) = quotRem n d
  n `divDef` d  =  q  where !(# q, _ #) = divMod n d
  n `modDef` d  =  r  where !(# _, r #) = divMod n d
  divModDef n d
    | signum r == negate (signum d) = (# q - 1, r + d #)
    | otherwise = qr
    where !qr@(# q, r #) = quotRem n d

instance RealFracRep UnliftedRep where
  truncateDef x = fromInteger m where
    !(# m, _ #) = properFraction x
  {-# INLINE truncateDef #-}

  roundDef x =
    let !(# n, r #) = properFraction x
        m | r < 0     = n - 1
          | otherwise = n + 1
    in case signum (abs r - 0.5) of
      -1 -> fromInteger n
      0 | Prelude.even n -> fromInteger n
        | otherwise -> fromInteger m
      1 -> fromInteger m
      _ -> Prelude.error "round default defn: Bad value"

  ceilingDef x
    | r > 0 = fromInteger (n + 1)
    | otherwise = fromInteger n
    where !(# n, r #) = properFraction x

  floorDef x 
    | r < 0 = fromInteger (n - 1)
    | otherwise = fromInteger n
    where !(# n, r #) = properFraction x

instance FloatingRep UnliftedRep where
  {-# INLINE powDef #-}
  {-# INLINE logBaseDef #-}
  {-# INLINE sqrtDef #-}
  {-# INLINE tanDef #-}
  {-# INLINE tanhDef #-}
  powDef x y = exp (log x * y)
  logBaseDef x y = log y / log x
  sqrtDef x = x ** 0.5
  tanDef x = sin x / cos x
  tanhDef x = sinh x / cosh x

  {-# INLINE log1pDef #-}
  {-# INLINE expm1Def #-}
  {-# INLINE log1pexpDef #-}
  {-# INLINE log1mexpDef #-}
  log1pDef x = log (1 + x)
  expm1Def x = exp x - 1
  log1pexpDef x = log1p (exp x)
  log1mexpDef x = log1p (negate (exp x))

instance RealFloatRep UnliftedRep where
  exponentDef x
    | m == 0 = 0
    | otherwise = n + floatDigits x
    where !(# m, n #) = decodeFloat x

  significandDef x = encodeFloat m (negate (floatDigits x))
    where !(# m, _ #) = decodeFloat x

  scaleFloatDef 0 x =  x
  scaleFloatDef k x
    | isFix =  x
    | otherwise =  encodeFloat m (n + clamp b k)
    where
      !(# m, n #) = decodeFloat x
      !(# l, h #) = floatRange x
      d = floatDigits x
      b = h - l + 4 * d
      isFix = x == 0 || isNaN x || isInfinite x
      clamp :: Int -> Int -> Int
      clamp bd k' = max (-bd) (min bd k')

  atan2Def y x
    | x > 0            =  atan (y/x)
    | x == 0 && y > 0  =  pi/2
    | x <  0 && y > 0  =  pi + atan (y/x)
    |(x <= 0 && y < 0)            ||
     (x <  0 && isNegativeZero y) ||
     (isNegativeZero x && isNegativeZero y)
                       = -atan2 (-y) x
    | y == 0 && (x < 0 || isNegativeZero x)
                       =  pi    -- must be after the previous test on zero y
    | x==0 && y==0     =  y     -- must be after the other double zero tests
    | otherwise        =  x + y -- x or y is a NaN, return a NaN (via +)

data instance ListD (a :: TYPE UnliftedRep) = Nil | a :# List a
infixr 5 :#

instance ListRep UnliftedRep where
  cons = (:#) 
  cons' a as = a :# as
  nil = Nil
  uncons# (a :# as) = Maybe# (# | (# a, as #) #)
  uncons# Nil = Maybe# (# (##) | #)

instance Eq a => Prelude.Eq (ListD @UnliftedRep a) where
  Nil == Nil = True
  a :# as == b :# bs = a == b && as == bs
  _ == _ = False

  Nil /= Nil = True
  a :# as /= b :# bs = a /= b || as /= bs
  _ /= _ = False

instance Ord a => Prelude.Ord (ListD @UnliftedRep a) where
  compare Nil Nil = EQ
  compare Nil (:#){} = LT
  compare (:#){} Nil = GT
  compare (a:#as) (b:#bs) = compare a b <> compare as bs

instance ShowList a => Prelude.Show (ListD @UnliftedRep a) where
  showsPrec _ = showList

data instance MaybeD (a :: TYPE UnliftedRep) = Nothing | Just a

instance Eq a => Prelude.Eq (MaybeD @UnliftedRep a) where
  Nothing == Nothing = True
  Just a == Just b = a == b
  _ == _ = False
  
instance Ord a => Prelude.Ord (MaybeD @UnliftedRep a) where
  compare Nothing  Nothing = EQ
  compare Nothing  Just{} = LT
  compare Just{}   Nothing = GT
  compare (Just a) (Just b) = compare a b

instance Show a => Prelude.Show (MaybeD @UnliftedRep a) where
  showsPrec _ Nothing = showString "Nothing"
  showsPrec d (Just a) = showParen (d >= 11) $ showString "Just " . showsPrec 11 a

instance MaybeRep UnliftedRep where
  nothing = Nothing
  just = Just
  just' x = Just x
  maybe n _ Nothing = n
  maybe _ j (Just a) = j a
  mapMaybe _ Nothing = nothing
  mapMaybe f (Just a) = just' (f a)

instance MaybeRep# UnliftedRep where
  nothing# = Maybe# (# (##) | #)
  just# a = Maybe# (# | a #)
  just'# a = Maybe# (# | a #)
  maybe# n _ (Maybe# (# (##) | #)) = n
  maybe# _ j (Maybe# (# | a #)) = j a
  mapMaybe# _ (Maybe# (# (##) | #)) = nothing#
  mapMaybe# f (Maybe# (# | a #)) = just'# (f a)

pattern Nothing# :: forall (a :: TYPE UnliftedRep). Maybe# a
pattern Nothing# = Maybe# (# (##) | #)

pattern Just# :: forall (a :: TYPE UnliftedRep). a -> Maybe# a
pattern Just# a = Maybe# (# | a #)

-- Maybe# can be made a monomorphic functor where the result rep must match the input rep
-- not very satisfying, but the best we can do until richard allows type families inside
-- TYPE.

-- unfortunately ghc is not able to be talked into this, even with type family helpers
type RebindMaybe# :: forall (r :: RuntimeRep). TYPE r -> TYPE ('SumRep '[ 'TupleRep '[], UnliftedRep ])
type family RebindMaybe# where
  RebindMaybe# @UnliftedRep = Maybe# @UnliftedRep
  -- no otherwise

type instance Rebind (Maybe# @UnliftedRep) r = RebindMaybe# @r

instance Functor (Maybe# @UnliftedRep) where
  type FunctorRep (Maybe# @UnliftedRep) = (~) UnliftedRep
  fmap _ Nothing# = Nothing#
  fmap f (Just# a) = Just# (f a)

instance Show a => Show (Maybe# @UnliftedRep a) where
  showsPrec _ Nothing# = showString "Nothing#"
  showsPrec d (Just# a) = showParen (d >= 11) $ showString "Just# " . showsPrec 11 a
  show x = shows x ""

-- this instance will probably not fire without a lot of help, because that body condition is harsh
-- We split ShowList into a separate class, even if this breaks compat with base because of this
-- instance
instance (ListRep ('SumRep '[ 'TupleRep '[], UnliftedRep ]), Show a) => ShowList (Maybe# @UnliftedRep a) where
  showList = go shows where
    go :: forall (a :: TYPE UnliftedRep). (Maybe# a -> ShowS) -> List (Maybe# a) -> ShowS
    go showx l s = case uncons# l of
      Maybe# (# (##) | #) -> "[]" ++ s
      Maybe# (# | (# x, xs #) #) -> '[' : showx x (showl xs)
        where 
          showl l' = case uncons# l' of
            Maybe# (# (##) | #) -> ']' : s
            Maybe# (# | (# y, ys #) #) -> ',' : showx y (showl ys)

{-# complete Nothing#, Just# :: Maybe# #-}

instance ShowRep UnliftedRep where
  showsPrecDef _ x s = show x ++ s
  showDef x          = shows x ""

instance ShowListRep UnliftedRep where
  showListDef = showList__ shows

showList__ :: forall (a :: TYPE UnliftedRep). (a -> ShowS) -> List a -> ShowS
showList__ _     Nil       s = "[]" ++ s
showList__ showx (x :# xs) s = '[' : showx x (showl xs)
  where
    showl Nil       = ']' : s
    showl (y :# ys) = ',' : showx y (showl ys)

instance PrintRep UnliftedRep where
  hPrint h x = IO.hPutStrLn h (show x)
