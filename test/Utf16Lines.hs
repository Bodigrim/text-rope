-- |
-- Copyright:   (c) 2021-2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module Utf16Lines
  ( testSuite
  ) where

import Prelude ((+), (-), maxBound)
import Data.Bool (Bool(..), (&&))
import Data.Function (($))
import qualified Data.List as L
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (mempty)
import Data.Ord (min, (>=), (<))
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text.Utf16.Lines
import Data.Tuple (snd)
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty, (===), property, (.||.), (.&&.), (==>))

import Utils

testSuite :: TestTree
testSuite = testGroup "Utf16 Lines"
  [ testProperty "toText . fromText" $
    \x -> toText (fromText x) === x

  , testProperty "null" $
    \x -> null x === T.null (toText x)

  , testProperty "TextLines associativity" $
    \x y z -> (x <> y) <> z === x <> (y <> z :: TextLines)
  , testProperty "TextLines mempty <>" $
    \x -> mempty <> x === (x :: TextLines)
  , testProperty "TextLines <> mempty" $
    \x -> x <> mempty === (x :: TextLines)

  , testProperty "Position associativity" $
    \x y z -> posLine x < maxBound - posLine y && posLine y < maxBound - posLine z ==>
       (x <> y) <> z === x <> (y <> z :: Position)
  , testProperty "Position mempty <>" $
    \x -> mempty <> x === (x :: Position)
  , testProperty "Position <> mempty" $
    \x -> x <> mempty === (x :: Position)

  , testProperty "lines" $
    \x -> T.lines x === lines (fromText x)
  , testProperty "lengthInLines" $
    \x -> L.genericLength (T.lines x) === lengthInLines (fromText x)

  , testProperty "splitAtLine 1" $
    \i x -> let (y, z) = splitAtLine i x in x === y <> z
  , testProperty "splitAtLine 2" $
    \i x -> let (y, z) = splitAtLine i x in lines x === lines y <> lines z
  , testProperty "splitAtLine 3" $
    \i x -> let (y, _) = splitAtLine i x in
      L.genericLength (lines y) === min i (L.genericLength (lines x))

  , testProperty "length 1" $
    \x -> length x === utf16Length (toText x)
  , testProperty "length 2" $
    length (fromText (T.singleton '\xffff')) === 1
  , testProperty "length 3" $
    length (fromText (T.singleton '\x10000')) === 2

  , testProperty "splitAt 1" $
    \i x -> case splitAt i x of
      Nothing -> property True
      Just (y, z) -> x === y <> z
  , testProperty "splitAt 2" $
    \i x -> case splitAt i x of
      Nothing -> property True
      Just (y, _) -> utf16Length (toText y) === min i (utf16Length (toText x))
  , testProperty "splitAt 3" $
    \i x -> case splitAt i x of
      Just{} -> True
      Nothing -> isJust (splitAt (i - 1) x) && isJust (splitAt (i + 1) x)
  , testProperty "splitAt 4" $ let t = fromText (T.singleton '\xffff') in
    splitAt 1 t === Just (t, mempty)
  , testProperty "splitAt 5" $ let t = fromText (T.singleton '\x10000') in
    splitAt 1 t === Nothing

  , testProperty "lengthAsPosition 1" $
    \x -> splitAtPosition (lengthAsPosition x) x === Just (x, mempty)
  , testProperty "lengthAsPosition 2" $
    \x -> let Position l c = lengthAsPosition x in
      length (snd (splitAtLine l x)) === c

  , testProperty "splitAtPosition 1" $
    \i x -> case splitAtPosition i x of
      Nothing -> property True
      Just (y, z) -> x === y <> z
  , testProperty "splitAtPosition 2" $
    \i x -> case splitAtPosition i x of
      Nothing -> property True
      Just (y, z) -> lengthAsPosition x === lengthAsPosition y <> lengthAsPosition z
  , testProperty "splitAtPosition 3" $
    \i@(Position l _) x -> case splitAtPosition i x of
      Nothing -> True
      Just (y, _) ->
        let l' = min l (posLine (lengthAsPosition x)) in
        posLine (lengthAsPosition y) >= l'
  , testProperty "splitAtPosition 4" $
    \i@(Position l c) x -> case splitAtPosition i x of
      Nothing -> property True
      Just (y, z) -> null z .||. length (snd (splitAtLine l y)) === c
  , testProperty "splitAtPosition 5" $
    \i -> case splitAtPosition i mempty of
      Nothing -> property False
      Just (y, z) -> y === mempty .&&. z === mempty
  , testProperty "splitAtPosition 6" $
    \i x -> case splitAtPosition i x of
      Just{} -> True
      Nothing -> isJust (splitAtPosition (i <> Position 0 1) x)
  ]
