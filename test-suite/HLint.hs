-- Copyright (c) 2015 Taylor Fausak <taylor@fausak.me>
-- Copyright (c) 2015 Stian Ellingsen <stian@plaimi.net>
--
-- See COPYING.Haskeleton.md for the licence covering this file.

module Main (main) where

import Control.Monad (unless)
import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure)

arguments :: [String]
arguments = ["--cpp-simple", "benchmark", "library", "test-suite"]

main :: IO ()
main = hlint arguments >>= \h -> unless (null h) exitFailure
