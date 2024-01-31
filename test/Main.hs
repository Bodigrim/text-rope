-- |
-- Copyright:   (c) 2021-2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module Main
  ( main
  ) where

import qualified CharLines
import qualified CharRope
import qualified MixedRope
import qualified Utf8Lines
import qualified Utf16Lines
import qualified Utf16Rope

import Prelude ()
import Data.Function (($))
import System.IO (IO)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "All"
  [ CharLines.testSuite
  , CharRope.testSuite
  , Utf8Lines.testSuite
  , Utf16Lines.testSuite
  , Utf16Rope.testSuite
  , MixedRope.testSuite
  ]
