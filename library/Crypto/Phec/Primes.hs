{- |
Module      :  $Header$
Description :  Testing and generation of Sophie Germain / safe primes.
Copyright   :  2015 Stian Ellingsen <stian@plaimi.net>
License     :  LGPL-3
Maintainer  :  Stian Ellingsen <stian@plaimi.net>
-}
module Crypto.Phec.Primes
  (isGermainPrime, isGermainPrime', isSafePrime) where

import Math.NumberTheory.Primes.Sieve (sieveFrom)
import Math.NumberTheory.Primes.Testing (bailliePSW, isStrongFermatPP)

-- |@'isGermainPrime' n@ checks whether @n@ is a Sophie Germain prime,
-- i.e., a (positive) prime number where @2 * n + 1@ is also prime.
isGermainPrime :: Integer -> Bool
isGermainPrime n | n < 0          = False
                 | n `rem` 6 == 5 = isGermainPrime' n
                 | otherwise      = n == 2 || n == 3

-- |Like 'isGermainPrime', but assumes that the number is positive and
-- congruent to 5 modulo 6. Optimised for numbers around 1024 bits.
isGermainPrime' :: Integer -> Bool
isGermainPrime' n | n < q * q = go' [n, m] smallPrimesFrom5
                  | otherwise = go smallPrimesFrom5
  where m = 2 * n + 1
        q = last smallPrimesFrom5
        go (p : ps) = n `rem` p /= 0 && m `rem` p /= 0 && go ps
        go []       = go' [n, m] []
        go' ns (p : ps)
          | null ns'  = True
          | otherwise = all (\x -> x `rem` p /= 0) ns' && go' ns' ps
          where ns' = dropWhile (p * p >) ns
        go' ns [] = all (\x -> isStrongFermatPP x 2) ns && all bailliePSW ns

-- |@'isSafePrime' n@ checks whether @n@ is a safe prime, i.e., a (positive)
-- prime number on the form @2 * p + 1@ where p is also prime.
isSafePrime :: Integer -> Bool
isSafePrime n = n `mod` 2 == 1 && isGermainPrime (n `div` 2)

smallPrimesFrom5 :: [Integer]
smallPrimesFrom5 = take 1536 (sieveFrom 5)
