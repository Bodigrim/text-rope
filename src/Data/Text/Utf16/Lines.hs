-- |
-- Copyright:   (c) 2021-2022 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Data.Text.Utf16.Lines
  ( I.TextLines
  , I.fromText
  , I.toText
  , I.null
  -- * Lines
  , I.lines
  , I.lengthInLines
  , I.splitAtLine
  -- * UTF-16 code units
  , length
  , splitAt
  , Position(..)
  , lengthAsPosition
  , splitAtPosition
  ) where

import Prelude ((+), (-), seq)
import Control.DeepSeq (NFData, rnf)
import Data.Bool (otherwise)
import Data.Eq (Eq, (==))
import Data.Function ((.), ($))
import Data.Maybe (Maybe(..))
import Data.Monoid (Monoid(..))
import Data.Ord (Ord, (<=), (>), (>=))
import Data.Semigroup (Semigroup(..))
import qualified Data.Text.Array as TA
import Data.Text.Internal (Text(..))
import qualified Data.Text.Lines.Internal as I
import qualified Data.Vector.Unboxed as U
import Data.Word (Word)
import Text.Show (Show)

#if MIN_VERSION_text(2,0,0)
import Prelude (fromIntegral)
import Foreign.C.Types (CSize(..))
import GHC.Exts (ByteArray#)
import System.IO (IO)
import System.IO.Unsafe (unsafeDupablePerformIO)
import System.Posix.Types (CSsize(..))
#else
import Data.Bool ((&&))
import Data.Ord ((<))
#endif

#ifdef DEBUG
import GHC.Stack (HasCallStack)
#else
#define HasCallStack ()
#endif

lengthTextUtf16 :: Text -> Word
#if MIN_VERSION_text(2,0,0)
lengthTextUtf16 (Text (TA.ByteArray arr) off len) = fromIntegral $ unsafeDupablePerformIO $
  lengthUtf8AsUtf16 arr (fromIntegral off) (fromIntegral len)

foreign import ccall unsafe "_hs_text_lines_length_utf8_as_utf16" lengthUtf8AsUtf16
  :: ByteArray# -> CSize -> CSize -> IO CSsize
#else
lengthTextUtf16 (Text _ _ len) = I.intToWord len
#endif

-- | Length in UTF-16 code units.
-- Takes linear time.
--
-- >>> :set -XOverloadedStrings
-- >>> length "fя𐀀"
-- 4
-- >>> Data.Text.Lines.length "fя𐀀"
-- 3
--
length :: I.TextLines -> Word
length = lengthTextUtf16 . I.toText

-- | Represent a position in a text.
data Position = Position
  { posLine   :: !Word -- ^ Line.
  , posColumn :: !Word -- ^ Column in UTF-16 code units.
  } deriving (Eq, Ord, Show)

instance NFData Position where
  rnf = (`seq` ())

-- | Associativity does not hold when 'posLine' overflows.
instance Semigroup Position where
  Position l1 c1 <> Position l2 c2 =
    Position (l1 + l2) (if l2 == 0 then c1 + c2 else c2)

instance Monoid Position where
  mempty = Position 0 0
  mappend = (<>)

-- | Measure text length as an amount of lines and columns.
-- Time is proportional to the length of the last line.
--
-- >>> :set -XOverloadedStrings
-- >>> lengthAsPosition "f𐀀"
-- Position {posLine = 0, posColumn = 3}
-- >>> lengthAsPosition "f\n𐀀"
-- Position {posLine = 1, posColumn = 2}
-- >>> lengthAsPosition "f\n𐀀\n"
-- Position {posLine = 2, posColumn = 0}
--
lengthAsPosition
  :: I.TextLines
  -> Position
lengthAsPosition (I.TextLines (Text arr off len) nls) = Position
  { posLine = I.intToWord $ U.length nls
  , posColumn = lengthTextUtf16 $ Text arr nl (off + len - nl)
  }
  where
    nl = if U.null nls then off else U.last nls + 1

splitTextAtUtf16Index :: Word -> Text -> Maybe (Text, Text)
splitTextAtUtf16Index k t@(Text arr off len)
  | k <= 0 = Just (Text arr off 0, t)
  | k >= I.intToWord len = Just (t, mempty)
#if MIN_VERSION_text(2,0,0)
  | o >= 0 = Just (Text arr off o, Text arr (off + o) (len - o))
  | otherwise = Nothing
    where
      !(TA.ByteArray arr#) = arr
      o = fromIntegral $ unsafeDupablePerformIO $
        takeUtf8AsUtf16 arr# (fromIntegral off) (fromIntegral len) (fromIntegral k)

foreign import ccall unsafe "_hs_text_lines_take_utf8_as_utf16" takeUtf8AsUtf16
  :: ByteArray# -> CSize -> CSize -> CSize -> IO CSsize
#else
  -- Something wrong is going here:
  | otherwise = if c >= 0xdc00 && c < 0xe000 then Nothing else Just
    (Text arr off k', Text arr (off + k') (len - k'))
    where
      k' = I.wordToInt k
      c = TA.unsafeIndex arr (off + k')
#endif

-- | Combination of 'I.splitAtLine' and subsequent 'splitAt'.
-- If requested number of code units splits a code point in half, return 'Nothing'.
-- Time is linear in 'posColumn', but does not depend on 'posLine'.
--
-- >>> :set -XOverloadedStrings
-- >>> splitAtPosition (Position 1 0) "f\n𐀀я"
-- Just ("f\n","𐀀я")
-- >>> splitAtPosition (Position 1 1) "f\n𐀀я"
-- Nothing
-- >>> splitAtPosition (Position 1 2) "f\n𐀀я"
-- Just ("f\n𐀀","я")
-- >>> splitAtPosition (Position 0 2) "f\n𐀀я"
-- Just ("f\n","𐀀я")
-- >>> splitAtPosition (Position 0 3) "f\n𐀀я"
-- Nothing
-- >>> splitAtPosition (Position 0 4) "f\n𐀀я"
-- Just ("f\n𐀀","я")
--
splitAtPosition
  :: HasCallStack
  => Position
  -> I.TextLines
  -> Maybe (I.TextLines, I.TextLines)
splitAtPosition (Position line column) (I.TextLines (Text arr off len) nls) =
  case splitTextAtUtf16Index column tx of
    Nothing -> Nothing
    Just (Text _ off' len', tz) -> let n = I.binarySearch nls (off' + len') in Just
      ( I.textLines (Text arr off (off' + len' - off)) (U.take n nls)
      , I.textLines tz (U.drop n nls))
  where
    arrLen = off + len
    nl
      | line <= 0 = off
      | line > I.intToWord (U.length nls) = arrLen
      | otherwise = nls U.! (I.wordToInt line - 1) + 1
    tx = Text arr nl (arrLen - nl)

-- | Split at given UTF-16 code unit.
-- If requested number of code units splits a code point in half, return 'Nothing'.
-- Takes linear time.
--
-- >>> :set -XOverloadedStrings
-- >>> map (\c -> splitAt c "fя𐀀") [0..4]
-- [Just ("","fя𐀀"),Just ("f","я𐀀"),Just ("fя","𐀀"),Nothing,Just ("fя𐀀","")]
--
splitAt :: HasCallStack => Word -> I.TextLines -> Maybe (I.TextLines, I.TextLines)
splitAt = splitAtPosition . Position 0
