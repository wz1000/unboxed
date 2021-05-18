{-# Language NoImplicitPrelude #-}
{-# Language MagicHash #-}
{-# OPTIONS_GHC -Wno-orphans -ddump-rn-trace -v3 -dppr-debug #-}

module Bug where

import Unboxed.Natural
import Unboxed.Rep.Int ()
import Unboxed.Internal.Class


sub'Natural# :: Natural# -> ()
sub'Natural# 0 = ()
sub'Natural# a = sub'Natural# (subNatural# a 1)
