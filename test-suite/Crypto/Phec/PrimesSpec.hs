{- |
Module      :  $Header$
Description :  Spec for Crypto.Phec.Primes.
Copyright   :  2015 Stian Ellingsen <stian@plaimi.net>
License     :  LGPL-3
Maintainer  :  Stian Ellingsen <stian@plaimi.net>
-}
module Crypto.Phec.PrimesSpec (spec) where

import Control.Arrow ((>>>))
import Control.Monad (forM_)
import Crypto.Phec.Primes (isGermainPrime, isGermainPrime', isSafePrime)
import Data.Functor ((<$>))
import Data.Monoid ((<>))
import Math.NumberTheory.Primes.Counting (nthPrime)
import Math.NumberTheory.Primes.Testing (isPrime)
import Test.Hspec (Spec, describe, it, parallel, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
  ( Gen, (===), (==>), arbitrary, choose, forAll, getNonNegative, getPositive
  , infiniteListOf, oneof, sized )

genPositiveLargeInteger :: Gen Integer
genPositiveLargeInteger = sized $ \n -> do
  b <- choose (1, 24 + n)
  choose (1, 2^b)

genLargePrime :: Gen Integer
genLargePrime = do
  n <- infiniteListOf $ (1 +) <$> (2 *) <$> genPositiveLargeInteger
  return . head $ filter isPrime n

genPrime :: Gen Integer
genPrime = nthPrime <$> getPositive <$> arbitrary

genGermainCandidate :: Gen Integer
genGermainCandidate = subtract 1 <$> (6 *) <$> genPositiveLargeInteger

genCompound :: Gen Integer
genCompound = do
  a <- (1 +) <$> oneof [getPositive <$> arbitrary, genPositiveLargeInteger]
  b <- (a +) <$> oneof [getNonNegative <$> arbitrary, genPositiveLargeInteger]
  return $ a * b

largeSafePrimes :: [Integer]
largeSafePrimes = [2^(2^i :: Int) - x | (i, x) <- zip [5 :: Int ..] xs]
  where xs = [209, 1469, 15449, 36113, 38117, 1093337, 1942289, 10895177]

safePrimes :: [Integer]
safePrimes = [5, 7, 11, 23, 2579, 2819, 2879, 2903] <> largeSafePrimes

germainPrimes :: [Integer]
germainPrimes = (`div` 2) <$> safePrimes

spec :: Spec
spec = parallel $ do
  describe "isGermainPrime'" $ do
    prop "returns False for compound numbers" .
      forAll genGermainCandidate $ \n ->
        not (isPrime n) ==> not (isGermainPrime' n)
    prop "returns False for compound 2 * n + 1" .
      forAll genGermainCandidate $ \n ->
        not (isPrime (2 * n + 1)) ==> not (isGermainPrime' n)
  describe "isGermainPrime" $ do
    prop "returns False for non-positive numbers" .
      forAll (negate <$> getNonNegative <$> arbitrary) $ not . isGermainPrime
    prop "returns False for numbers > 5 not congruent to 5 modulo 6" .
      forAll ((6 *) <$> genPositiveLargeInteger) $ \x ->
        not $ or [isGermainPrime $ x + i | i <- [0..4]]
    prop "for any prime p, returns True iff 2 * p + 1 is prime" .
      forAll (oneof [genLargePrime, genPrime]) $ \p ->
        isPrime (2 * p + 1) === isGermainPrime p
    prop "returns False for compound numbers" .
      forAll genCompound $ not . isGermainPrime
    it "returns True for certain known Germain primes" $
      forM_ germainPrimes (`shouldSatisfy` isGermainPrime)
  describe "isSafePrime" $ do
    prop "returns True iff (n - 1) / 2 is a Germain prime" $
      getPositive >>> \a ->
        isSafePrime a === (a `mod` 2 == 1 && isGermainPrime (a `div` 2))
    prop "for any prime p, returns True iff (p - 1) / 2 is prime" .
      forAll (oneof [genLargePrime, genPrime]) $ \p ->
        isSafePrime p === isPrime (p `div` 2)
    it "returns True for certain known safe primes" $
      forM_ safePrimes (`shouldSatisfy` isSafePrime)
