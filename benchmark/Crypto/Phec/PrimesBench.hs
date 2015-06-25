{- |
Module      :  $Header$
Description :  Benchmark for Crypto.Phec.Primes.
Copyright   :  2015 Stian Ellingsen <stian@plaimi.net>
License     :  LGPL-3
Maintainer  :  Stian Ellingsen <stian@plaimi.net>
-}
module Crypto.Phec.PrimesBench (benchmarks) where

import Crypto.Phec.Primes (isGermainPrime')
import Math.NumberTheory.Primes.Testing (isPrime)

import Criterion (Benchmark, bench, bgroup, nf)

withGermainCandidates :: (Integer -> a) -> Int -> [a]
withGermainCandidates f b = fmap f (take 5000 [x, x + 6 ..])
  where x = 6 * (2^b `quot` 6) + 5

bmigp :: Int -> [Bool]
bmigp = withGermainCandidates isGermainPrime'

bmigpn :: Int -> [Bool]
bmigpn = withGermainCandidates (\n -> isPrime n && isPrime (2 * n + 1))

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup "isGermainPrime'-5k"
    [ bench "256"  $ nf bmigp 256
    , bench "512"  $ nf bmigp 512
    , bench "1024" $ nf bmigp 1024
    ]
  , bgroup "isPrime-isPrime-5k"
    [ bench "256" $ nf bmigpn 256
    , bench "512" $ nf bmigpn 512
    ]
  ]
