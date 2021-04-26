{-# Language NoImplicitPrelude #-}
{-# Language RebindableSyntax #-}
{-# Language MagicHash #-}
{-# OPTIONS_GHC -Wno-orphans -ddump-rn-trace #-}

module Bug where

import Unboxed.Natural

sub'Natural# :: Natural# -> ()
sub'Natural# 0 = ()
sub'Natural# a = sub'Natural# (subNatural# a 1)
