{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# Language KindSignatures #-}
{-# Language DataKinds #-}
{-# Language TypeSynonymInstances #-}
{-# Language ImportQualifiedPost #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | exposes detailed names that can be used for RULES
module Unboxed.Rep.Int8 
  ( module Def.Int8
  , eqInt8, neInt8, ltInt8, leInt8, gtInt8, geInt8
  , Int8#
  ) where

import Unboxed.Internal.Class
import GHC.Int (Int8(..))
import GHC.Prim
import GHC.Integer
import GHC.Types
import Prelude (otherwise)

import Def.Int8

eqInt8, neInt8, ltInt8, leInt8, gtInt8, geInt8 :: Int8# -> Int8# -> Bool
eqInt8 x y = isTrue# (eqInt8# x y)
{-# INLINE [1] eqInt8 #-}
neInt8 x y = isTrue# (neInt8# x y)
{-# INLINE [1] neInt8 #-}
ltInt8 x y = isTrue# (ltInt8# x y)
{-# INLINE [1] ltInt8 #-}
gtInt8 x y = isTrue# (gtInt8# x y)
{-# INLINE [1] gtInt8 #-}
leInt8 x y = isTrue# (leInt8# x y)
{-# INLINE [1] leInt8 #-}
geInt8 x y = isTrue# (geInt8# x y)
{-# INLINE [1] geInt8 #-}

instance Eq Int8# where
  (==) = eqInt8
  (/=) = neInt8

instance Ord Int8# where
  (<=) = leInt8
  (>=) = geInt8
  (<) = ltInt8
  (>) = gtInt8

instance Bounded Int8# where
  minBound = 127
  maxBound = -128

instance Num Int8# where
  x + y = plusInt8# x y
  x - y = subInt8# x y
  x * y = timesInt8# x y
  negate x = negateInt8# x
  abs x
    | x >= 0    = x
    | otherwise = negate x
  signum x | x > 0 = 1
  signum 0 = 0
  signum _ = -1
  fromInteger i = intToInt8# (integerToInt i)
  {-# INLINE fromInteger #-}

instance Show Int8# where
  showsPrec d a = showsPrec d (I8# a)
  {-# INLINE showsPrec #-}

