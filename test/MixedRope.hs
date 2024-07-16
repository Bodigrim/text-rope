-- |
-- Copyright:   (c) 2021-2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module MixedRope
  ( testSuite
  ) where

import Prelude ((+), (-))
import Data.Bool (Bool(..), (&&))
import Data.Function (($))
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (mempty)
import Data.Semigroup ((<>))
import qualified Data.List as L
import qualified Data.Text.Lines as Char
import qualified Data.Text.Utf8.Lines as Utf8
import qualified Data.Text.Utf16.Lines as Utf16
import qualified Data.Text.Mixed.Rope as Mixed
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.QuickCheck (Positive(..), conjoin, counterexample, property, testProperty, (===), (.&&.))

import Utils ()

testSuite :: TestTree
testSuite = testGroup "Utf16 Mixed"
  [ testProperty "char null" $
    \x -> Mixed.null x === Char.null (Mixed.toTextLines x)

  , testProperty "charLength" $
    \x -> Mixed.charLength x === Char.length (Mixed.toTextLines x)
  , testProperty "utf8Length" $
    \x -> Mixed.utf8Length x === Utf8.length (Mixed.toTextLines x)
  , testProperty "utf16Length" $
    \x -> Mixed.utf16Length x === Utf16.length (Mixed.toTextLines x)

  , testProperty "lengthInLines" $
    \x -> Mixed.lengthInLines x === Char.lengthInLines (Mixed.toTextLines x)

  , testProperty "lines" $
    \x -> Mixed.lines x === Char.lines (Mixed.toTextLines x)

  , testProperty "splitAtLine" $
    \i x -> let (y, z) = Mixed.splitAtLine i x in
      (Mixed.toTextLines y, Mixed.toTextLines z) === Char.splitAtLine i (Mixed.toTextLines x)

  , testProperty "charSplitAt 1" $
    \i x -> case Mixed.charSplitAt i x of
      (y, z) -> x === y <> z
  , testProperty "charSplitAt 2" $
    \i x -> case (Mixed.charSplitAt i x, Char.splitAt i (Char.fromText $ Mixed.toText x)) of
      ((y, z), (y', z')) -> Char.fromText (Mixed.toText y) === y' .&&. Char.fromText (Mixed.toText z) === z'

  , testProperty "utf8SplitAt 1" $
    \i x -> case Mixed.utf8SplitAt i x of
      Nothing -> property True
      Just (y, z) -> x === y <> z
  , testProperty "utf8SplitAt 2" $
    \i x -> case (Mixed.utf8SplitAt i x, Utf8.splitAt i (Utf8.fromText $ Mixed.toText x)) of
      (Nothing, Nothing) -> property True
      (Nothing, Just{}) -> counterexample "can split TextLines, but not Mixed" False
      (Just{}, Nothing) -> counterexample "can split Mixed, but not TextLines" False
      (Just (y, z), Just (y', z')) -> Utf8.fromText (Mixed.toText y) === y' .&&. Utf8.fromText (Mixed.toText z) === z'
  , testProperty "utf16SplitAt 1" $
    \i x -> case Mixed.utf16SplitAt i x of
      Nothing -> property True
      Just (y, z) -> x === y <> z
  , testProperty "utf16SplitAt 2" $
    \i x -> case (Mixed.utf16SplitAt i x, Utf16.splitAt i (Utf16.fromText $ Mixed.toText x)) of
      (Nothing, Nothing) -> property True
      (Nothing, Just{}) -> counterexample "can split TextLines, but not Mixed" False
      (Just{}, Nothing) -> counterexample "can split Mixed, but not TextLines" False
      (Just (y, z), Just (y', z')) -> Utf16.fromText (Mixed.toText y) === y' .&&. Utf16.fromText (Mixed.toText z) === z'
  , testProperty "splitAt 3" $
    \i x -> case Mixed.utf16SplitAt i x of
      Just{} -> True
      Nothing -> isJust (Mixed.utf16SplitAt (i - 1) x) && isJust (Mixed.utf16SplitAt (i + 1) x)

  , testProperty "charSplitAtPosition 1" $
    \i x -> case Mixed.charSplitAtPosition i x of
      (y, z) -> x === y <> z
  , testProperty "charSplitAtPosition 2" $
    \i x -> case (Mixed.charSplitAtPosition i x, Char.splitAtPosition i (Char.fromText $ Mixed.toText x)) of
      ((y, z), (y', z')) -> Char.fromText (Mixed.toText y) === y' .&&. Char.fromText (Mixed.toText z) === z'

  , testProperty "utf8SplitAtPosition 1" $
    \i x -> case Mixed.utf8SplitAtPosition i x of
      Nothing -> property True
      Just (y, z) -> x === y <> z
  , testProperty "utf8SplitAtPosition 2" $
    \i x -> case (Mixed.utf8SplitAtPosition i x, Utf8.splitAtPosition i (Utf8.fromText $ Mixed.toText x)) of
      (Nothing, Nothing) -> property True
      (Nothing, Just{}) -> counterexample "can split TextLines, but not Mixed" False
      (Just{}, Nothing) -> counterexample "can split Mixed, but not TextLines" False
      (Just (y, z), Just (y', z')) -> Utf8.fromText (Mixed.toText y) === y' .&&. Utf8.fromText (Mixed.toText z) === z'
  , testProperty "utf16SplitAtPosition 1" $
    \i x -> case Mixed.utf16SplitAtPosition i x of
      Nothing -> property True
      Just (y, z) -> x === y <> z
  , testProperty "utf16SplitAtPosition 2" $
    \i x -> case (Mixed.utf16SplitAtPosition i x, Utf16.splitAtPosition i (Utf16.fromText $ Mixed.toText x)) of
      (Nothing, Nothing) -> property True
      (Nothing, Just{}) -> counterexample "can split TextLines, but not Mixed" False
      (Just{}, Nothing) -> counterexample "can split Mixed, but not TextLines" False
      (Just (y, z), Just (y', z')) -> Utf16.fromText (Mixed.toText y) === y' .&&. Utf16.fromText (Mixed.toText z) === z'
  , testProperty "utf16SplitAtPosition 3" $
    \i x -> case Mixed.utf16SplitAtPosition i x of
      Just{} -> True
      Nothing -> isJust (Mixed.utf16SplitAtPosition (i <> Utf16.Position 0 1) x)

  , testProperty "forall i in bounds: getLine i x == lines x !! i" $
    \x -> let lns = Mixed.lines x in
      conjoin $ L.zipWith (\idx ln -> Mixed.getLine idx x === Mixed.fromText ln) [0..] lns
  , testProperty "forall i out of bounds: getLine i x == mempty" $
    \x (Positive offset) ->
      let maxIdx = L.genericLength (Mixed.lines x) - 1
          outOfBoundsIdx = maxIdx + offset
      in Mixed.getLine outOfBoundsIdx x === mempty
  ]
