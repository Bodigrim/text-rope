-- |
-- Copyright:   (c) 2021-2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module Utf16Rope
  ( testSuite
  ) where

import Prelude ((+), (-))
import Data.Bool (Bool(..), (&&))
import Data.Function (($))
import Data.Maybe (Maybe(..), isJust)
import Data.Semigroup ((<>))
import qualified Data.Text.Utf16.Lines as Lines 
import qualified Data.Text.Utf16.Rope as Rope
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty, (===), property, (.&&.), counterexample)

import Utils ()

testSuite :: TestTree
testSuite = testGroup "Utf16 Rope"
  [ testProperty "null" $
    \x -> Rope.null x === Lines.null (Rope.toTextLines x)

  , testProperty "length" $
    \x -> Rope.length x === Lines.length (Rope.toTextLines x)

  , testProperty "lengthInLines" $
    \x -> Rope.lengthInLines x === Lines.lengthInLines (Rope.toTextLines x)

  , testProperty "lines" $
    \x -> Rope.lines x === Lines.lines (Rope.toTextLines x)

  , testProperty "splitAtLine" $
    \i x -> let (y, z) = Rope.splitAtLine i x in
      (Rope.toTextLines y, Rope.toTextLines z) === Lines.splitAtLine i (Rope.toTextLines x)

  , testProperty "splitAt 1" $
    \i x -> case Rope.splitAt i x of
      Nothing -> property True
      Just (y, z) -> x === y <> z
  , testProperty "splitAt 2" $
    \i x -> case (Rope.splitAt i x, Lines.splitAt i (Lines.fromText $ Rope.toText x)) of
      (Nothing, Nothing) -> property True
      (Nothing, Just{}) -> counterexample "can split TextLines, but not Rope" False
      (Just{}, Nothing) -> counterexample "can split Rope, but not TextLines" False
      (Just (y, z), Just (y', z')) -> Lines.fromText (Rope.toText y) === y' .&&. Lines.fromText (Rope.toText z) === z'
  , testProperty "splitAt 3" $
    \i x -> case Rope.splitAt i x of
      Just{} -> True
      Nothing -> isJust (Rope.splitAt (i - 1) x) && isJust (Rope.splitAt (i + 1) x)

  , testProperty "splitAtPosition 1" $
    \i x -> case Rope.splitAtPosition i x of
      Nothing -> property True
      Just (y, z) -> x === y <> z
  , testProperty "splitAtPosition 2" $
    \i x -> case (Rope.splitAtPosition i x, Lines.splitAtPosition i (Lines.fromText $ Rope.toText x)) of
      (Nothing, Nothing) -> property True
      (Nothing, Just{}) -> counterexample "can split TextLines, but not Rope" False
      (Just{}, Nothing) -> counterexample "can split Rope, but not TextLines" False
      (Just (y, z), Just (y', z')) -> Lines.fromText (Rope.toText y) === y' .&&. Lines.fromText (Rope.toText z) === z'
  , testProperty "splitAtPosition 3" $
    \i x -> case Rope.splitAtPosition i x of
      Just{} -> True
      Nothing -> isJust (Rope.splitAtPosition (i <> Lines.Position 0 1) x)
  ]
