module Utf8Rope
  ( testSuite
  ) where

import Prelude ((+), (-))
import Data.Bool (Bool(..))
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Semigroup ((<>))
import qualified Data.List as L
import qualified Data.Text.Utf8.Lines as Lines
import qualified Data.Text.Utf8.Rope as Rope
import Test.Tasty (testGroup, TestTree)
import Test.Tasty.QuickCheck (Positive(..), conjoin, counterexample, property, testProperty, (===), (.&&.))

import Utils ()

testSuite :: TestTree
testSuite = testGroup "Utf8 Rope"
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

  , testProperty "forall i in bounds: getLine i x == lines x !! i" $
    \x -> let lns = Rope.lines x in
      conjoin $ L.zipWith (\idx ln -> Rope.getLine idx x === ln) [0..] lns
  , testProperty "forall i out of bounds: getLine i x == mempty" $
    \x (Positive offset) ->
      let maxIdx = L.genericLength (Rope.lines x) - 1
          outOfBoundsIdx = maxIdx + offset
      in Rope.getLine outOfBoundsIdx x === mempty
  ]
