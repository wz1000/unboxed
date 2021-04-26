{-# Language MagicHash #-}
{-# Language RankNTypes #-}
{-# Language DataKinds #-}
{-# Language PolyKinds #-}
module Whnf where

import Gauge.Benchmark
import GHC.Types
import Internal

whnfN#
  :: forall (a :: TYPE ('TupleRep '[ 'WordRep, 'BoxedRep 'Unlifted ])) b.
     (a -> b) -> a -> Benchmarkable
whnfN# f x = toBenchmarkable (whnf'N# f x)

whnfB#
  :: forall (a :: TYPE ('SumRep '[ 'IntRep, 'BoxedRep 'Lifted ])) b.
     (a -> b) -> a -> Benchmarkable
whnfB# f x = toBenchmarkable (whnf'B# f x)

whnfL
  :: forall a b.
     (a -> b) -> a -> Benchmarkable
whnfL f x = toBenchmarkable (whnf'L f x)
