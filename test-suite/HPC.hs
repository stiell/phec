-- Copyright (c) 2015 Taylor Fausak <taylor@fausak.me>
-- Copyright (c) 2015 Stian Ellingsen <stian@plaimi.net>
--
-- See COPYING.Haskeleton.md for the licence covering this file.

module Main (main) where

import Control.Monad (guard)
import Data.Functor ((<$>))
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import System.Directory (doesFileExist)
import System.Exit (exitFailure, exitSuccess)
import System.Process (readProcess)
import Text.XML.Light

ilu :: String -> [Attr] -> Maybe Integer
ilu k a = read <$> lookupAttr (blank_name {qName = k}) a

checkBoxes :: [Attr] -> Bool
checkBoxes a = ilu "boxes" a == ilu "count" a

checkBoolean :: [Attr] -> Bool
checkBoolean a = checkBoxes a && ilu "false" a == Just 0

checkGuards :: [Attr] -> Bool
checkGuards a = (checkBoolean a &&) . fromMaybe False $ do
  t <- ilu "true" a
  c <- ilu "count" a
  return $ t <= c `div` 2

check :: String -> [Attr] -> Bool
check "guards" = checkGuards
check t | t `elem` ["booleans", "conditionals", "qualifiers"] = checkBoolean
check _ = checkBoxes

main :: IO ()
main = do
    file <- tix
    let arguments n = ["report", "--per-module"] <> n <> [file]
    output <- readProcess "hpc" (arguments ["--xml-output"]) ""
    let res = fromMaybe False $ do
          e <- parseXMLDoc output
          s <- findChild (blank_name {qName = "summary"}) e
          let m = findChildren (blank_name {qName = "module"}) e
              c = concat $ elChildren <$> (s : m)
          guard . not $ null c
          return $ and [check (qName $ elName x) (elAttribs x) | x <- c]
    if res
        then exitSuccess
        else readProcess "hpc" (arguments []) "" >>= putStr >> exitFailure

-- The location of the TIX file changed between versions of cabal-install.
-- See <https://github.com/tfausak/haskeleton/issues/31> for details.
tix :: IO FilePath
tix = do
    let newFile = "tests.tix"
        oldFile = "dist/hpc/tix/tests/tests.tix"
    newFileExists <- doesFileExist newFile
    let file = if newFileExists then newFile else oldFile
    return file
