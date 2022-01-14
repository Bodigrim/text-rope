-- |
-- Copyright:   (c) 2021-2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

{-# LANGUAGE CPP #-}
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
import qualified Data.Text.Utf16.Rope as Utf16Rope
#ifdef MIN_VERSION_core_text
import qualified Core.Text.Rope as CoreText
#endif
#ifdef MIN_VERSION_rope_utf16_splay
import qualified Data.Rope.UTF16 as RopeSplay
#endif
#ifdef MIN_VERSION_yi_rope
import qualified Yi.Rope as YiRope
#endif

main :: IO ()
main = defaultMain
  [ bgroup "Split at position"
    [ bgroup "Unicode"
      [ bench "text-rope" $ nf (editByPosition (Proxy @CharRope.Rope)) txt
#ifdef MIN_VERSION_yi_rope
      , bcompare "$NF == \"text-rope\" && $(NF-1) == \"Unicode\" && $(NF-2) == \"Split at position\""
      $ bench "yi-rope" $ nf (editByPosition (Proxy @YiRope.YiString)) txt
#endif
      ]
    , bgroup "UTF-16"
      [ bench "text-rope" $ nf (editByPosition (Proxy @Utf16Rope.Rope)) txt
#ifdef MIN_VERSION_rope_utf16_splay
      , bcompare "$NF == \"text-rope\" && $(NF-1) == \"UTF-16\" && $(NF-2) == \"Split at position\""
      $ bench "rope-utf16-splay" $ nf (editByPosition (Proxy @RopeSplay.Rope)) txt
#endif
      ]
    ]
  , bgroup "Split at offset"
    [ bgroup "Unicode"
      [ bench "text-rope" $ nf (editByOffset (Proxy @CharRope.Rope)) txt
#ifdef MIN_VERSION_core_text
      , bcompare "$NF == \"text-rope\" && $(NF-1) == \"Unicode\" && $(NF-2) == \"Split at offset\""
      $ bench "core-text" $ nf (editByOffset (Proxy @CoreText.Rope)) txt
#endif
#ifdef MIN_VERSION_yi_rope
      , bcompare "$NF == \"text-rope\" && $(NF-1) == \"Unicode\" && $(NF-2) == \"Split at offset\""
      $ bench "yi-rope" $ nf (editByOffset (Proxy @YiRope.YiString)) txt
#endif
      ]
    , bgroup "UTF-16"
      [ bench "text-rope" $ nf (editByOffset (Proxy @Utf16Rope.Rope)) txt
#ifdef MIN_VERSION_rope_utf16_splay
      , bcompare "$NF == \"text-rope\" && $(NF-1) == \"UTF-16\" && $(NF-2) == \"Split at offset\""
      $ bench "rope-utf16-splay" $ nf (editByOffset (Proxy @RopeSplay.Rope)) txt
#endif
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

class Monoid a => Splittable a where
  fromText :: T.Text -> a
  toText :: a -> T.Text
  splitAt :: Word -> a -> (a, a)

class Splittable a => SplittableAtPosition a where
  splitAtPosition :: Word -> Word -> a -> (a, a)

instance Splittable CharRope.Rope where
  fromText = CharRope.fromText
  toText = CharRope.toText
  splitAt = CharRope.splitAt

instance SplittableAtPosition CharRope.Rope where
  splitAtPosition l c = CharRope.splitAtPosition (CharRope.Position l c)

instance Splittable Utf16Rope.Rope where
  fromText = Utf16Rope.fromText
  toText = Utf16Rope.toText
  splitAt = (fromJust . ) . Utf16Rope.splitAt

instance SplittableAtPosition Utf16Rope.Rope where
  splitAtPosition l c = fromJust . Utf16Rope.splitAtPosition (Utf16Rope.Position l c)

#ifdef MIN_VERSION_core_text
instance Splittable CoreText.Rope where
  fromText = CoreText.intoRope
  toText = CoreText.fromRope
  splitAt = CoreText.splitRope . fromIntegral
#endif

#ifdef MIN_VERSION_yi_rope
instance Splittable YiRope.YiString where
  fromText = YiRope.fromText
  toText = YiRope.toText
  splitAt = YiRope.splitAt . fromIntegral

instance SplittableAtPosition YiRope.YiString where
  splitAtPosition l c orig = (before `mappend` mid, after)
    where
      (before, after') = YiRope.splitAtLine (fromIntegral l) orig
      (mid, after) = YiRope.splitAt (fromIntegral c) after'
#endif

#ifdef MIN_VERSION_rope_utf16_splay
instance Splittable RopeSplay.Rope where
  fromText = RopeSplay.fromText
  toText = RopeSplay.toText
  splitAt = RopeSplay.splitAt . fromIntegral

instance SplittableAtPosition RopeSplay.Rope where
  splitAtPosition l c orig = RopeSplay.splitAt k orig
    where
      k = RopeSplay.rowColumnCodeUnits (RopeSplay.RowColumn (fromIntegral l) (fromIntegral c)) orig
#endif

editByOffset :: forall a. Splittable a => Proxy a -> T.Text -> T.Text
editByOffset _ txt = (toText @a) $ foldl' edit (fromText txt) randomOffsets
  where
    edit orig c = before `mappend` mid `mappend` after
      where
        (before, after') = splitAt c orig
        -- edit 10 characters
        (mid, after) = splitAt 10 after'

editByPosition :: forall a. SplittableAtPosition a => Proxy a -> T.Text -> T.Text
editByPosition _ txt = (toText @a) $ foldl' edit (fromText txt) randomPositions
  where
    edit orig (l, c) = before `mappend` mid `mappend` after
      where
        (before, after') = splitAtPosition l c orig
        -- edit 10 characters
        (mid, after) = splitAt 10 after'
