module Main (main) where

import qualified Crypto.Phec.PrimesBench
-- HASKELETON: import qualified New.ModuleBench

import Criterion.Main (bgroup, defaultMain)

main :: IO ()
main = defaultMain $
    [ bgroup "Primes" Crypto.Phec.PrimesBench.benchmarks
    -- HASKELETON: , bgroup "New.Module" New.ModuleBench.benchmarks
    ]
