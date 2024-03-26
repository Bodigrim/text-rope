-- |
-- Copyright:   (c) 2021-2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import Prelude (fromIntegral, (<$>), (*))
import Paths_text_rope (getDataFileName)
import Data.Function (($), (.))
import Data.Int (Int)
import Data.List (foldl', take, zip, length)
import Data.Maybe (fromJust)
import Data.Monoid (Monoid, mappend)
import Data.Proxy (Proxy(..))
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Word (Word)
import System.IO (IO)
import System.IO.Unsafe (unsafePerformIO)
import System.Random (randomRs, mkStdGen)
import Test.Tasty.Bench (defaultMain, bgroup, bench, nf, bcompare)

import qualified Data.Text.Rope as CharRope
import qualified Data.Text.Utf8.Rope as Utf8Rope
import qualified Data.Text.Utf16.Rope as Utf16Rope
import qualified Data.Text.Utf16.Rope.Mixed as Mixed
#ifdef MIN_VERSION_core_text
import qualified Core.Text.Rope as CoreText
#endif
#ifdef MIN_VERSION_rope_utf16_splay
import qualified Data.Rope.UTF16 as RopeSplay
#endif
#ifdef MIN_VERSION_yi_rope
import qualified Yi.Rope as YiRope
#endif

data CodePoint
data Utf8
data Utf16

main :: IO ()
main = defaultMain
  [ bgroup "Split at position"
    [ bgroup "Unicode"
      [ bench "text-rope" $ nf (editByPosition (Proxy @CodePoint) (Proxy @CharRope.Rope)) txt
      , bench "text-rope-mixed" $ nf (editByPosition (Proxy @CodePoint) (Proxy @Mixed.Rope)) txt
#ifdef MIN_VERSION_yi_rope
      , bcompare "$NF == \"text-rope\" && $(NF-1) == \"Unicode\" && $(NF-2) == \"Split at position\""
      $ bench "yi-rope" $ nf (editByPosition (Proxy @CodePoint) (Proxy @YiRope.YiString)) txt
#endif
      ]
    , bgroup "UTF-16"
      [ bench "text-rope" $ nf (editByPosition (Proxy @Utf16) (Proxy @Utf16Rope.Rope)) txt
      , bench "text-rope-mixed" $ nf (editByPosition (Proxy @Utf16) (Proxy @Mixed.Rope)) txt
#ifdef MIN_VERSION_rope_utf16_splay
      , bcompare "$NF == \"text-rope\" && $(NF-1) == \"UTF-16\" && $(NF-2) == \"Split at position\""
      $ bench "rope-utf16-splay" $ nf (editByPosition (Proxy @Utf16) (Proxy @RopeSplay.Rope)) txt
#endif
      ]
    , bgroup "UTF-8"
      [ bench "text-rope" $ nf (editByPosition (Proxy @Utf8) (Proxy @Utf8Rope.Rope)) txtUtf8
      , bench "text-rope-mixed" $ nf (editByPosition (Proxy @Utf8) (Proxy @Mixed.Rope)) txtUtf8
      ]
    ]
  , bgroup "Split at offset"
    [ bgroup "Unicode"
      [ bench "text-rope" $ nf (editByOffset (Proxy @CodePoint) (Proxy @CharRope.Rope)) txt
      , bench "text-rope-mixed" $ nf (editByOffset (Proxy @CodePoint) (Proxy @Mixed.Rope)) txt
#ifdef MIN_VERSION_core_text
      , bcompare "$NF == \"text-rope\" && $(NF-1) == \"Unicode\" && $(NF-2) == \"Split at offset\""
      $ bench "core-text" $ nf (editByOffset (Proxy @CodePoint) (Proxy @CoreText.Rope)) txt
#endif
#ifdef MIN_VERSION_yi_rope
      , bcompare "$NF == \"text-rope\" && $(NF-1) == \"Unicode\" && $(NF-2) == \"Split at offset\""
      $ bench "yi-rope" $ nf (editByOffset (Proxy @Utf16) (Proxy @YiRope.YiString)) txt
#endif
      ]
    , bgroup "UTF-16"
      [ bench "text-rope" $ nf (editByOffset (Proxy @Utf16) (Proxy @Utf16Rope.Rope)) txt
      , bench "text-rope-mixed" $ nf (editByOffset (Proxy @Utf16) (Proxy @Mixed.Rope)) txt
#ifdef MIN_VERSION_rope_utf16_splay
      , bcompare "$NF == \"text-rope\" && $(NF-1) == \"UTF-16\" && $(NF-2) == \"Split at offset\""
      $ bench "rope-utf16-splay" $ nf (editByOffset (Proxy @Utf16) (Proxy @RopeSplay.Rope)) txt
#endif
      ]
    , bgroup "UTF-8"
      [ bench "text-rope" $ nf (editByOffset (Proxy @Utf8) (Proxy @Utf8Rope.Rope)) txtUtf8
      , bench "text-rope-mixed" $ nf (editByOffset (Proxy @Utf8) (Proxy @Mixed.Rope)) txtUtf8
      ]
    ]
  ]

scale :: Int
scale = 1

txt :: T.Text
txt = unsafePerformIO $ do
  fn <- getDataFileName "bench/bench.txt"
  T.replicate scale <$> T.readFile fn
{-# NOINLINE txt #-}

txtUtf8 :: T.Text
txtUtf8 = unsafePerformIO $ do
  fn <- getDataFileName "bench/bench-utf8.txt"
  T.replicate scale <$> T.readFile fn
{-# NOINLINE txtUtf8 #-}

randomOffsets :: [Word]
randomOffsets = take (1000 * scale) $
  randomRs (0, fromIntegral $ T.length txt) (mkStdGen 33)
{-# NOINLINE randomOffsets #-}

randomPositions :: [(Word, Word)]
randomPositions = take (1000 * scale) $ zip ls cs
  where
    ls = randomRs (0, fromIntegral $ length $ T.lines txt) (mkStdGen 42)
    -- assuming reasonable line length is < 80
    cs = randomRs (0, 80) (mkStdGen 24)
{-# NOINLINE randomPositions #-}

class Monoid a => Textable a where
  fromText :: T.Text -> a
  toText :: a -> T.Text

class Textable t => Splittable u t where
  splitAt :: Proxy u -> Word -> t -> (t, t)

class Splittable u t => SplittableAtPosition u t where
  splitAtPosition :: Proxy u -> Word -> Word -> t -> (t, t)

instance Textable CharRope.Rope where
  fromText = CharRope.fromText
  toText = CharRope.toText

instance Splittable CodePoint CharRope.Rope where
  splitAt _ = CharRope.splitAt

instance SplittableAtPosition CodePoint CharRope.Rope where
  splitAtPosition _ l c = CharRope.splitAtPosition (CharRope.Position l c)

instance Textable Utf8Rope.Rope where
  fromText = Utf8Rope.fromText
  toText = Utf8Rope.toText

instance Splittable Utf8 Utf8Rope.Rope where
  splitAt _ = (fromJust . ) . Utf8Rope.splitAt

instance SplittableAtPosition Utf8 Utf8Rope.Rope where
  splitAtPosition _ l c = fromJust . Utf8Rope.splitAtPosition (Utf8Rope.Position l c)

instance Textable Utf16Rope.Rope where
  fromText = Utf16Rope.fromText
  toText = Utf16Rope.toText

instance Splittable Utf16 Utf16Rope.Rope where
  splitAt _ = (fromJust . ) . Utf16Rope.splitAt

instance SplittableAtPosition Utf16 Utf16Rope.Rope where
  splitAtPosition _ l c = fromJust . Utf16Rope.splitAtPosition (Utf16Rope.Position l c)

instance Textable Mixed.Rope where
  fromText = Mixed.fromText
  toText = Mixed.toText

instance Splittable CodePoint Mixed.Rope where
  splitAt _ = Mixed.charSplitAt

instance SplittableAtPosition CodePoint Mixed.Rope where
  splitAtPosition _ l c = Mixed.charSplitAtPosition (CharRope.Position l c)

instance Splittable Utf8 Mixed.Rope where
  splitAt _ = (fromJust . ) . Mixed.utf8SplitAt

instance SplittableAtPosition Utf8 Mixed.Rope where
  splitAtPosition _ l c = fromJust . Mixed.utf8SplitAtPosition (Utf8Rope.Position l c)

instance Splittable Utf16 Mixed.Rope where
  splitAt _ = (fromJust . ) . Mixed.utf16SplitAt

instance SplittableAtPosition Utf16 Mixed.Rope where
  splitAtPosition _ l c = fromJust . Mixed.utf16SplitAtPosition (Utf16Rope.Position l c)

#ifdef MIN_VERSION_core_text
instance Textable CoreText.Rope where
  fromText = CoreText.intoRope
  toText = CoreText.fromRope

instance SplittableAtPosition CodePoint CoreText.Rope where
  splitAt _ = CoreText.splitRope . fromIntegral
#endif

#ifdef MIN_VERSION_yi_rope
instance Textable YiRope.YiString where
  fromText = YiRope.fromText
  toText = YiRope.toText

instance Splittable CodePoint YiRope.YiString where
  splitAt _ = YiRope.splitAt . fromIntegral

instance SplittableAtPosition CodePoint YiRope.YiString where
  splitAtPosition _ l c orig = (before `mappend` mid, after)
    where
      (before, after') = YiRope.splitAtLine (fromIntegral l) orig
      (mid, after) = YiRope.splitAt (fromIntegral c) after'
#endif

#ifdef MIN_VERSION_rope_utf16_splay
instance Textable RopeSplay.Rope where
  fromText = RopeSplay.fromText
  toText = RopeSplay.toText

instance Splittable Utf16 RopeSplay.Rope where
  splitAt _ = RopeSplay.splitAt . fromIntegral

instance SplittableAtPosition Utf16 RopeSplay.Rope where
  splitAtPosition _ l c orig = RopeSplay.splitAt k orig
    where
      k = RopeSplay.rowColumnCodeUnits (RopeSplay.RowColumn (fromIntegral l) (fromIntegral c)) orig
#endif

editByOffset :: forall u t. Splittable u t => Proxy u -> Proxy t -> T.Text -> T.Text
editByOffset _ _ txt = (toText @t) $ foldl' edit (fromText txt) randomOffsets
  where
    edit orig c = before `mappend` mid `mappend` after
      where
        (before, after') = splitAt (Proxy @u) c orig
        -- edit 10 characters
        (mid, after) = splitAt (Proxy @u) 10 after'

editByPosition :: forall u t. SplittableAtPosition u t => Proxy u -> Proxy t -> T.Text -> T.Text
editByPosition _ _ txt = (toText @t) $ foldl' edit (fromText txt) randomPositions
  where
    edit orig (l, c) = before `mappend` mid `mappend` after
      where
        (before, after') = splitAtPosition (Proxy @u) l c orig
        -- edit 10 characters
        (mid, after) = splitAt (Proxy @u) 10 after'
