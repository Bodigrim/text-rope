-- |
-- Copyright:   (c) 2021-2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Utils
  ( utf16Length
  ) where

import Prelude (mod, (+), (-), maxBound)
import Control.Applicative (pure, (<$>), (<*>))
import Data.Char (Char)
import Data.Function (($), (.))
import qualified Data.List as L
import Data.Ord ((>))
import qualified Data.Text as T
import Data.Text.Internal (Text(..))
import qualified Data.Text.Lines as Char
import qualified Data.Text.Rope as CharRope
import qualified Data.Text.Utf16.Lines as Utf16
import qualified Data.Text.Utf16.Rope as Utf16Rope
import Data.Word (Word)
import Test.Tasty.QuickCheck (Gen, Arbitrary (arbitrary), arbitrary, shrink, frequency, arbitraryASCIIChar, arbitraryUnicodeChar, listOf, oneof)
import Data.Monoid (mconcat, mappend)
import Data.Bool (otherwise)
import Data.Maybe (maybe)

utf16Length :: Text -> Word
utf16Length t = L.genericLength xs + L.genericLength (L.filter (> '\xFFFF') xs)
  where
    xs = T.unpack t

charGen :: Gen Char
charGen = frequency
  [ (1, pure '\n')
  , (1, arbitraryASCIIChar)
  , (1, arbitraryUnicodeChar)
  ]

instance Arbitrary Text where
  arbitrary = do
    xs <- T.pack <$> listOf charGen
    d <- (`mod` (T.length xs + 1)) <$> arbitrary
    pure $ T.drop d xs
  shrink t@(Text arr off len)
    =    L.map (T.drop d . T.pack) (shrink ys)
    L.++ L.map (\d' -> T.drop d' $ T.pack $ L.drop (d - d') ys) (shrink d)
    where
      xs = T.unpack t
      ys = T.unpack (Text arr 0 (off + len))
      d  = L.length ys - L.length xs

instance Arbitrary Char.TextLines where
  arbitrary = Char.fromText <$> arbitrary
  shrink = L.map Char.fromText . shrink . Char.toText

instance Arbitrary Char.Position where
  arbitrary = oneof
    [ Char.Position <$> arbitrary <*> arbitrary
    , (\l -> Char.Position (maxBound - l)) <$> arbitrary <*> arbitrary
    , (\l c -> Char.Position l (maxBound - c)) <$> arbitrary <*> arbitrary
    , (\l c -> Char.Position (maxBound - l) (maxBound - c)) <$> arbitrary <*> arbitrary
    ]
  shrink (Char.Position x y) =
    [Char.Position x' y | x' <- shrink x] L.++ [Char.Position x y' | y' <- shrink y]

instance Arbitrary Utf16.Position where
  arbitrary = oneof
    [ Utf16.Position <$> arbitrary <*> arbitrary
    , (\l -> Utf16.Position (maxBound - l)) <$> arbitrary <*> arbitrary
    , (\l c -> Utf16.Position l (maxBound - c)) <$> arbitrary <*> arbitrary
    , (\l c -> Utf16.Position (maxBound - l) (maxBound - c)) <$> arbitrary <*> arbitrary
    ]
  shrink (Utf16.Position x y) =
    [Utf16.Position x' y | x' <- shrink x] L.++ [Utf16.Position x y' | y' <- shrink y]

instance Arbitrary CharRope.Rope where
  arbitrary = frequency
    [ (9, mconcat . L.map CharRope.fromText <$> arbitrary)
    , (1, mappend <$> arbitrary <*> arbitrary)
    ]
  shrink rp
    | CharRope.null rp = []
    | otherwise = L.concatMap (\i -> (\(x, y) -> [x, y]) (CharRope.splitAt i rp))
                  [1..CharRope.length rp - 1]

instance Arbitrary Utf16Rope.Rope where
  arbitrary = frequency
    [ (9, mconcat . L.map Utf16Rope.fromText <$> arbitrary)
    , (1, mappend <$> arbitrary <*> arbitrary)
    ]
  shrink rp
    | Utf16Rope.null rp = []
    | otherwise = L.concatMap (\i -> maybe [] (\(x, y) -> [x, y]) (Utf16Rope.splitAt i rp))
                  [1..Utf16Rope.length rp - 1]
