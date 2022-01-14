-- |
-- Copyright:   (c) 2021-2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

module CharLines
  ( testSuite
  ) where

import Prelude (fromIntegral, maxBound, (-))
import Data.Bool ((||), not, (&&))
import Data.Function (($))
import qualified Data.List as L
import Data.Monoid (mempty)
import Data.Ord (min, (>=), (<))
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text.Lines
import Data.Tuple (snd)
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.QuickCheck (testProperty, (===), applyFun, (.||.), (.&&.), (==>))

import Utils ()

testSuite :: TestTree
testSuite = testGroup "Char Lines"
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

  , testProperty "span 1" $
    \f x -> let (y, z) = span (applyFun f) x in x === y <> z
  , testProperty "span 2" $
    \f x -> let (y, _) = span (applyFun f) x in
      T.all (applyFun f) (toText y)
  , testProperty "span 3" $
    \f x -> let (_, z) = span (applyFun f) x in
      null z || not (applyFun f (T.head (toText z)))

  , testProperty "splitAtLine 1" $
    \i x -> let (y, z) = splitAtLine i x in x === y <> z
  , testProperty "splitAtLine 2" $
    \i x -> let (y, z) = splitAtLine i x in lines x === lines y <> lines z
  , testProperty "splitAtLine 3" $
    \i x -> let (y, _) = splitAtLine i x in
      L.genericLength (lines y) === min i (L.genericLength (lines x))

  , testProperty "length" $
    \x -> length x === fromIntegral (T.length (toText x))
  , testProperty "splitAt 1" $
    \i x -> let (y, z) = splitAt i x in x === y <> z
  , testProperty "splitAt 2" $
    \i x -> let (y, _) = splitAt i x in
      length y === min i (length x)

  , testProperty "lengthAsPosition 1" $
    \x -> splitAtPosition (lengthAsPosition x) x === (x, mempty)
  , testProperty "lengthAsPosition 2" $
    \x -> let Position l c = lengthAsPosition x in
      length (snd (splitAtLine l x)) === c

  , testProperty "splitAtPosition 1" $
    \i x -> let (y, z) = splitAtPosition i x in x === y <> z
  , testProperty "splitAtPosition 2" $
    \i x -> let (y, z) = splitAtPosition i x in
      lengthAsPosition x === lengthAsPosition y <> lengthAsPosition z
  , testProperty "splitAtPosition 3" $
    \i@(Position l _) x -> let (y, _) = splitAtPosition i x in
      let l' = min l (posLine (lengthAsPosition x)) in
      posLine (lengthAsPosition y) >= l'
  , testProperty "splitAtPosition 4" $
    \i@(Position l c) x -> let (y, z) = splitAtPosition i x in
      null z .||. length (snd (splitAtLine l y)) === c
  , testProperty "splitAtPosition 5" $
    \i -> let (y, z) = splitAtPosition i mempty in y === mempty .&&. z === mempty
  ]
