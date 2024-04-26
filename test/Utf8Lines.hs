module Utf8Lines
  ( testSuite
  ) where

import Prelude ((-), maxBound)
import Data.Bool (Bool(..), (&&))
import Data.Function (($))
import qualified Data.List as L
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty, mconcat)
import Data.Ord (min, (>=), (<))
import Data.Semigroup ((<>), stimes, stimesMonoid)
import qualified Data.Text as T
import Data.Text.Utf8.Lines
import Data.Tuple (snd)
import Data.Word (Word)
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.QuickCheck (Small(..), testProperty, (===), property, (.||.), (.&&.), (==>))

import Utils

testSuite :: TestTree
testSuite = testGroup "Utf8 Lines"
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

  , testProperty "mconcat" $
    \xs -> L.foldr (<>) mempty xs === mconcat (xs :: [TextLines])

  , testProperty "stimes" $
    \(Small n) xs -> stimesMonoid n xs === stimes (n :: Word) (xs :: TextLines)

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
    \x -> length x === utf8Length (toText x)
  , testProperty "length 2" $
    length (fromText (T.singleton '\x7f')) === 1
  , testProperty "length 3" $
    length (fromText (T.singleton '\x7ff')) === 2
  , testProperty "length 4" $
    length (fromText (T.singleton '\xffff')) === 3
  , testProperty "length 5" $
    length (fromText (T.singleton '\x10000')) === 4

  , testProperty "splitAt 1" $
    \i x -> case splitAt i x of
      Nothing -> property True
      Just (y, z) -> x === y <> z
  , testProperty "splitAt 2" $
    \i x -> case splitAt i x of
      Nothing -> property True
      Just (y, _) -> utf8Length (toText y) === min i (utf8Length (toText x))
  , testProperty "splitAt 3" $ let t = fromText (T.singleton '\x7f') in
    splitAt 1 t === Just (t, mempty)
  , testProperty "splitAt 4" $ let t = fromText (T.singleton '\x80') in
    splitAt 1 t === Nothing
  , testProperty "splitAt 5" $ let t = fromText (T.singleton '\x80') in
    splitAt 2 t === Just (t, mempty)

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
  ]
